[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [affine_ssa.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  (* === Atom interner === *)

  type ctx =
    { mutable atoms : (int * S.Instruction.t) list;
      mutable next : int
    }

  let new_ctx () = { atoms = []; next = 0 }

  let intern ctx (i : S.Instruction.t) : int =
    match List.find_opt (fun (_, j) -> IV.instr_same i j) ctx.atoms with
    | Some (id, _) -> id
    | None ->
      let id = ctx.next in
      ctx.next <- id + 1;
      ctx.atoms <- (id, i) :: ctx.atoms;
      id

  let atom_instr ctx id : S.Instruction.t = List.assoc id ctx.atoms

  let find_header_param_atom ctx (block : S.Block.t) index : int option =
    List.find_map
      (fun (id, i) ->
        if IV.is_header_param block index i then Some id else None)
      ctx.atoms

  (* === Linearization === *)

  let fits_int (n : nativeint) =
    Nativeint.equal (Nativeint.of_int (Nativeint.to_int n)) n

  (* Affine form of [instr]'s machine-integer value. Right shifts are atomized,
     pushing the sound bounds [2^k*t <= a] and [a <= 2^k*t + 2^k-1] onto [side].
     Target-specific scaled-add index ops are decoded via the [Arch] hook.
     Anything else becomes an atom. *)
  let rec linearize ctx side (instr : S.Instruction.t) : Affine.t =
    let lin = linearize ctx side in
    match instr with
    | Op { op = Const_int n; _ } when fits_int n ->
      Affine.const (Nativeint.to_int n)
    | Op { op = Intop Iadd; args = [| a; b |]; _ } -> Affine.add (lin a) (lin b)
    | Op { op = Intop Isub; args = [| a; b |]; _ } -> Affine.sub (lin a) (lin b)
    | Op { op = Intop_imm (Iadd, k); args = [| a |]; _ } ->
      Affine.add_const (lin a) k
    | Op { op = Intop_imm (Isub, k); args = [| a |]; _ } ->
      Affine.add_const (lin a) (-k)
    | Op { op = Intop_imm (Ilsl, k); args = [| a |]; _ } when k >= 0 && k < 16
      ->
      Affine.scale (1 lsl k) (lin a)
    | Op { op = Intop_imm (Iasr, k); args = [| a |]; _ } when k >= 0 && k < 16
      ->
      let t = Affine.var (intern ctx instr) in
      let av = lin a in
      let pow = 1 lsl k in
      side
        := Affine.sub av (Affine.scale pow t)
           :: Affine.sub (Affine.add_const (Affine.scale pow t) (pow - 1)) av
           :: !side;
      t
    | Op { op = Specific spec; args; _ } -> (
      match Arch.specific_operation_as_affine spec with
      | Some (coeff, disp) when Array.length coeff = Array.length args ->
        let acc = ref (Affine.const disp) in
        Array.iteri
          (fun i c -> acc := Affine.add !acc (Affine.scale c (lin args.(i))))
          coeff;
        !acc
      | Some _ | None -> (
        (* Fused multiply-add/sub: affine when one multiplicand linearizes to a
           constant [k], giving [±k * other + addend]. Otherwise atomize. *)
        match Arch.specific_operation_as_muladd spec with
        | Some (m0, m1, a, negate)
          when m0 < Array.length args
               && m1 < Array.length args
               && a < Array.length args -> (
          let a0 = lin args.(m0) and a1 = lin args.(m1) in
          let prod =
            if Affine.is_const a0
            then Some (Affine.scale a0.Affine.const a1)
            else if Affine.is_const a1
            then Some (Affine.scale a1.Affine.const a0)
            else None
          in
          match prod with
          | Some p ->
            Affine.add (if negate then Affine.neg p else p) (lin args.(a))
          | None -> Affine.var (intern ctx instr))
        | Some _ | None -> Affine.var (intern ctx instr)))
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      Affine.var (intern ctx instr)

  (* === Guard facts from dominating branches === *)

  (* Facts implied by the (possibly negated) signed comparison [la cmp lb].
     Unsigned comparisons and [Cne] cannot be expressed as a single affine
     inequality, so they contribute nothing. *)
  let cmp_facts ~negate (cmp : Cmm.integer_comparison) la lb : Affine.t list =
    let cmp = if negate then Cmm.negate_integer_comparison cmp else cmp in
    match cmp with
    | Cge -> [Affine.sub la lb]
    | Cgt -> [Affine.add_const (Affine.sub la lb) (-1)]
    | Cle -> [Affine.sub lb la]
    | Clt -> [Affine.add_const (Affine.sub lb la) (-1)]
    | Ceq -> [Affine.sub la lb; Affine.sub lb la]
    | Cne | Cult | Cugt | Cule | Cuge -> []

  let cond_facts ctx side ~negate (cond : S.Instruction.t) : Affine.t list =
    match cond with
    | Op { op = Intop (Icomp cmp); args = [| a; b |]; _ } ->
      cmp_facts ~negate cmp (linearize ctx side a) (linearize ctx side b)
    | Op { op = Intop_imm (Icomp cmp, k); args = [| a |]; _ } ->
      cmp_facts ~negate cmp (linearize ctx side a) (Affine.const k)
    | _ -> []

  (* Facts that hold at entry to [target], gathered from the branches on its
     immediate-dominator chain. *)
  let guards_at ctx side (target : S.Block.t) : Affine.t list =
    let acc = ref [] in
    let rec walk (block : S.Block.t) =
      let idom = block.dominator_info.dominator in
      if not (S.Block.equal idom block)
      then begin
        (match idom.terminator with
        | Branch { cond; ifso; ifnot } ->
          (* [cond] (or its negation) is a fact at [target] only if the taken
             edge [idom -> ifso] (resp. [idom -> ifnot]) *dominates* [target] --
             i.e. every path from entry to [target] traverses that specific
             edge. Block-dominance of [ifso] is not enough: control can reach
             [ifso] via the other edge when it reconverges, so the guard need
             not have held. We use the standard sufficient condition: the
             successor is dominated (so all paths to [target] pass through it)
             *and* its only predecessor is [idom] (so the only way into it is
             the taken edge). *)
          let edge_dominates (succ : S.Block.t) =
            S.Block.dominates succ target
            &&
            match S.Block.predecessors succ with
            | [p] -> S.Block.equal p idom
            | [] | _ :: _ :: _ -> false
          in
          if edge_dominates ifso
          then acc := cond_facts ctx side ~negate:false cond @ !acc
          else if edge_dominates ifnot
          then acc := cond_facts ctx side ~negate:true cond @ !acc
          else ()
        | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call _ | Invalid _ ->
          ());
        walk idom
      end
    in
    walk target;
    !acc
end
