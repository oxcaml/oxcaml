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

  (* === Recognition into {!Affine_expr} === *)

  let fits_int (n : nativeint) =
    Nativeint.equal (Nativeint.of_int (Nativeint.to_int n)) n

  type leaf_class =
    | Target
    | Invariant
    | Reject
    | Decompose

  (* Per-caller recognition policy; see the two instantiations below. Each hook
     returning an atom id may reject by returning [None]. *)
  type mode =
    { classify : S.Instruction.t -> leaf_class;
      target_atom : int;
      invariant_atom : unit -> int option;
      fallback_atom : S.Instruction.t -> int option;
      shr_atom : S.Instruction.t -> int option;
      max_shift_bits : int;
      decompose_mul : bool
    }

  (* The affine expression of [v]'s machine-integer value, per [mode]:
     decomposition through add/sub/shift (and constant-multiply when
     [decompose_mul]) shapes, target-specific scaled-add index ops and fused
     multiply-adds via the [Arch] hooks, with leaves and unrecognised values
     classified by the [mode]'s hooks. [None] when the policy rejects. *)
  let rec expr_of_value ~mode (v : S.Instruction.t) : Affine_expr.t option =
    let module E = Affine_expr in
    let ( let* ) = Option.bind in
    let recur = expr_of_value ~mode in
    let fallback v = Option.map (fun id -> E.Atom id) (mode.fallback_atom v) in
    match v with
    | Op { op = Const_int n; _ } when fits_int n ->
      Some (E.Const (Nativeint.to_int n))
    | _ -> (
      match mode.classify v with
      | Target -> Some (E.Atom mode.target_atom)
      | Invariant -> Option.map (fun id -> E.Atom id) (mode.invariant_atom ())
      | Reject -> None
      | Decompose -> (
        match v with
        | Op { op = Intop Iadd; args = [| a; b |]; _ } ->
          let* ea = recur a in
          let* eb = recur b in
          Some (E.Add (ea, eb))
        | Op { op = Intop Isub; args = [| a; b |]; _ } ->
          let* ea = recur a in
          let* eb = recur b in
          Some (E.Sub (ea, eb))
        | Op { op = Intop_imm (Iadd, k); args = [| a |]; _ } ->
          let* ea = recur a in
          Some (E.Add (ea, E.Const k))
        | Op { op = Intop_imm (Isub, k); args = [| a |]; _ } ->
          let* ea = recur a in
          Some (E.Sub (ea, E.Const k))
        | Op { op = Intop_imm (Ilsl, k); args = [| a |]; _ }
          when k >= 0 && k < mode.max_shift_bits ->
          let* ea = recur a in
          Some (E.Scale (1 lsl k, ea))
        | Op { op = Intop_imm (Iasr, k); args = [| a |]; _ }
          when k >= 0 && k < mode.max_shift_bits -> (
          (* Atomized right shift: the atom's relation to the shifted value is
             expressed by the side bounds {!Affine_expr.to_affine} emits. *)
          match mode.shr_atom v with
          | Some atom ->
            let* ea = recur a in
            Some (E.Shr_atom { atom; arg = ea; bits = k })
          | None -> fallback v)
        | Op { op = Intop_imm (Imul, k); args = [| a |]; _ }
          when mode.decompose_mul ->
          let* ea = recur a in
          Some (E.Scale (k, ea))
        | Op { op = Intop Imul; args = [| a; b |]; _ } when mode.decompose_mul
          -> (
          let* ea = recur a in
          let* eb = recur b in
          match E.as_const ea, E.as_const eb with
          | Some k, _ -> Some (E.Scale (k, eb))
          | None, Some k -> Some (E.Scale (k, ea))
          | None, None -> fallback v)
        | Op { op = Specific spec; args; _ } -> (
          match Arch.specific_operation_as_affine spec with
          | Some (coeff, disp) when Array.length coeff = Array.length args ->
            let rec build i acc =
              if i >= Array.length args
              then Some acc
              else
                let* ei = recur args.(i) in
                build (i + 1) (E.Add (acc, E.Scale (coeff.(i), ei)))
            in
            build 0 (E.Const disp)
          | Some _ | None -> (
            (* Fused multiply-add/sub: affine when one multiplicand's affine
               form is a constant [k], giving [±k * other + addend]. *)
            match Arch.specific_operation_as_muladd spec with
            | Some (m0, m1, a, negate)
              when m0 < Array.length args
                   && m1 < Array.length args
                   && a < Array.length args -> (
              let* e0 = recur args.(m0) in
              let* e1 = recur args.(m1) in
              let prod =
                match E.as_const e0, E.as_const e1 with
                | Some k, _ -> Some (E.Scale (k, e1))
                | None, Some k -> Some (E.Scale (k, e0))
                | None, None -> None
              in
              match prod with
              | Some p ->
                let* ea = recur args.(a) in
                Some (E.Add ((if negate then E.Scale (-1, p) else p), ea))
              | None -> fallback v)
            | Some _ | None -> fallback v))
        | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          fallback v))

  (* === Linearization === *)

  (* Affine form of [instr]'s machine-integer value. Right shifts are atomized,
     with the sound bounds [2^k*t <= a] and [a <= 2^k*t + 2^k-1] pushed onto
     [side]. Anything not decomposed becomes an atom, so this never rejects. *)
  let linearize ctx side (instr : S.Instruction.t) : Affine.t =
    let mode =
      { classify = (fun _ -> Decompose);
        target_atom = 0;
        invariant_atom = (fun () -> None);
        fallback_atom = (fun v -> Some (intern ctx v));
        shr_atom = (fun v -> Some (intern ctx v));
        max_shift_bits = 16;
        decompose_mul = false
      }
    in
    match expr_of_value ~mode instr with
    | Some e ->
      let form, sides = Affine_expr.to_affine e in
      side := sides @ !side;
      form
    | None ->
      (* Unreachable in this mode (no hook rejects); atomize for totality. *)
      Affine.var (intern ctx instr)

  (* === Coefficient extraction === *)

  let coeff_of_target ~(classify : S.Instruction.t -> leaf_class)
      (v : S.Instruction.t) : int option =
    let next = ref 0 in
    let mode =
      { classify;
        target_atom = 0;
        invariant_atom =
          (fun () ->
            incr next;
            Some !next);
        fallback_atom = (fun _ -> None);
        shr_atom = (fun _ -> None);
        max_shift_bits = 62;
        decompose_mul = true
      }
    in
    Option.bind (expr_of_value ~mode v) (Affine_expr.coeff_of_atom 0)

  (* === Guard facts from dominating branches === *)

  let cond_facts ctx side ~negate (cond : S.Instruction.t) : Affine.t list =
    match cond with
    | Op { op = Intop (Icomp cmp); args = [| a; b |]; _ } ->
      Loop_comparisons.facts ~negate cmp (linearize ctx side a)
        (linearize ctx side b)
    | Op { op = Intop_imm (Icomp cmp, k); args = [| a |]; _ } ->
      Loop_comparisons.facts ~negate cmp (linearize ctx side a) (Affine.const k)
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
             edge; see {!Natural_loop.Make.edge_dominates}. *)
          let edge_dominates (succ : S.Block.t) =
            IV.edge_dominates ~src:idom ~succ ~target
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
