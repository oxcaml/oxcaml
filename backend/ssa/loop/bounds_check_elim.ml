[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [bounds_check_elimination.md] for the design and soundness argument. *)

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  (* === Affine forms over interned atoms ===

     [terms] maps an atom (an SSA value we don't decompose) to a non-zero
     integer coefficient; [const] is the constant term. A value [v] means the
     inequality [v >= 0]. *)
  module Affine = struct
    type t =
      { const : int;
        terms : (int * int) list
      }

    let const c = { const = c; terms = [] }

    let var id = { const = 0; terms = [id, 1] }

    let is_const t = match t.terms with [] -> true | _ :: _ -> false

    let coeff id t =
      match List.assoc_opt id t.terms with Some c -> c | None -> 0

    let add_const t c = { t with const = t.const + c }

    let add a b =
      let ids =
        List.sort_uniq Int.compare (List.map fst a.terms @ List.map fst b.terms)
      in
      let terms =
        List.filter_map
          (fun id ->
            let c = coeff id a + coeff id b in
            if c = 0 then None else Some (id, c))
          ids
      in
      { const = a.const + b.const; terms }

    let scale k t =
      if k = 0
      then const 0
      else
        { const = t.const * k;
          terms = List.map (fun (id, c) -> id, c * k) t.terms
        }

    let neg t = scale (-1) t

    let sub a b = add a (neg b)
  end

  (* === Fourier-Motzkin === *)

  (* Is the conjunction [{ f >= 0 | f in ineqs }] satisfiable over the
     rationals? Eliminate atoms one at a time: for each, combine every
     lower-bound (positive coeff) with every upper-bound (negative coeff) into
     an atom-free resolvent; atoms occurring with only one sign are
     unconstrained and dropped. The system is infeasible iff some constant-only
     inequality becomes negative. *)
  let feasible (ineqs : Affine.t list) : bool =
    let atoms =
      List.sort_uniq Int.compare
        (List.concat_map (fun (f : Affine.t) -> List.map fst f.terms) ineqs)
    in
    let elim v ineqs =
      let cf f = Affine.coeff v f in
      let pos = List.filter (fun f -> cf f > 0) ineqs in
      let neg = List.filter (fun f -> cf f < 0) ineqs in
      let zero = List.filter (fun f -> cf f = 0) ineqs in
      let resolvents =
        List.concat_map
          (fun p ->
            List.map
              (fun n ->
                let cp = cf p and cn = -cf n in
                Affine.add (Affine.scale cn p) (Affine.scale cp n))
              neg)
          pos
      in
      List.rev_append zero resolvents
    in
    let reduced = List.fold_left (fun acc v -> elim v acc) ineqs atoms in
    List.for_all (fun (f : Affine.t) -> f.Affine.const >= 0) reduced

  (* Does [{ f >= 0 | f in facts }] entail [goal >= 0]? Add the integer negation
     [goal <= -1] and test for infeasibility. *)
  let entails (facts : Affine.t list) (goal : Affine.t) : bool =
    not (feasible (Affine.add_const (Affine.neg goal) (-1) :: facts))

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
      | _ -> Affine.var (intern ctx instr))
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
        | Branch { cond; ifso; ifnot } -> (
          let on_true = S.Block.dominates ifso target in
          let on_false = S.Block.dominates ifnot target in
          match on_true, on_false with
          | true, false -> acc := cond_facts ctx side ~negate:false cond @ !acc
          | false, true -> acc := cond_facts ctx side ~negate:true cond @ !acc
          | true, true | false, false -> ())
        | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call _ | Invalid _ ->
          ());
        walk idom
      end
    in
    walk target;
    !acc

  (* === Loop-invariance of atoms === *)

  let build_op_def_block () : S.Block.t S.Instruction.Id.Tbl.t =
    let tbl = S.Instruction.Id.Tbl.create 64 in
    List.iter
      (fun (bl : S.Block.t) ->
        Array.iter
          (fun (i : S.Instruction.t) ->
            match i with
            | Op { id; _ } -> S.Instruction.Id.Tbl.replace tbl id bl
            | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
            | Stack_check _ | Name_for_debugger _ ->
              ())
          bl.body)
      S.blocks;
    tbl

  let atom_invariant ctx ~op_def_block ~(body : S.Block.Set.t) id =
    match atom_instr ctx id with
    | Op
        { op =
            ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
          _
        } ->
      true
    | Op { id = oid; _ } -> (
      match S.Instruction.Id.Tbl.find_opt op_def_block oid with
      | Some bl -> not (S.Block.Set.mem bl body)
      | None -> false)
    | Block_param { block; _ } -> not (S.Block.Set.mem block body)
    | Proj _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
    | Name_for_debugger _ ->
      false

  let affine_invariant ctx ~op_def_block ~body (f : Affine.t) =
    List.for_all
      (fun (id, _) -> atom_invariant ctx ~op_def_block ~body id)
      f.Affine.terms

  (* === Induction-variable range facts === *)

  (* A constant lower bound on the initial values (only constants handled). *)
  let init_lower_const ctx side (biv : IV.biv) : int option =
    match biv.init with
    | [] -> None
    | inits ->
      let consts =
        List.map
          (fun v ->
            let a = linearize ctx side v in
            if Affine.is_const a then Some a.Affine.const else None)
          inits
      in
      if List.for_all Option.is_some consts
      then
        Some (List.fold_left (fun m o -> min m (Option.get o)) max_int consts)
      else None

  (* For an increasing IV: [param - min(init) >= 0]. *)
  let iv_lower_fact ctx side (loop : IV.loop) (biv : IV.biv) : Affine.t list =
    match biv.step, biv.sign with
    | Step_const c, `Add when c > 0 -> (
      match
        ( find_header_param_atom ctx loop.header biv.param_index,
          init_lower_const ctx side biv )
      with
      | Some pid, Some m -> [Affine.add_const (Affine.var pid) (-m)]
      | _ -> [])
    | _ -> []

  (* For an increasing IV in a bottom-tested loop: derive an invariant upper
     bound [U - param >= 0]. The value flowing on the (single) back edge becomes
     the next header value; a guard dominating that edge bounds it by some [U =
     f + backedge_arg] (when that is loop-invariant), provided every entry edge
     also satisfies [init <= U]. *)
  let iv_upper_fact ctx side ~op_def_block (loop : IV.loop) (biv : IV.biv) :
      Affine.t list =
    match biv.step, biv.sign with
    | Step_const c, `Add when c > 0 -> (
      match find_header_param_atom ctx loop.header biv.param_index with
      | None -> []
      | Some pid -> (
        let header = loop.header in
        let k = biv.param_index in
        let arg_to_header (p : S.Block.t) : S.Instruction.t option =
          match p.terminator with
          | Goto { goto; args }
            when S.Block.equal goto header && Array.length args > k ->
            args.(k)
          | Goto _ | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
          | Tailcall_func _ | Call _ | Invalid _ ->
            None
        in
        match loop.back_edges with
        | [pe] -> (
          match arg_to_header pe with
          | None -> []
          | Some arg -> (
            let a = linearize ctx side arg in
            let candidates =
              List.filter_map
                (fun f ->
                  let u = Affine.add f a in
                  if affine_invariant ctx ~op_def_block ~body:loop.body u
                  then Some u
                  else None)
                (guards_at ctx side pe)
            in
            let init_preds =
              List.filter
                (fun p -> not (List.exists (S.Block.equal p) loop.back_edges))
                (S.Block.predecessors header)
            in
            let verify u =
              (not (List.is_empty init_preds))
              && List.for_all
                   (fun ip ->
                     match arg_to_header ip with
                     | Some iarg ->
                       entails
                         (guards_at ctx side ip @ !side)
                         (Affine.sub u (linearize ctx side iarg))
                     | None -> false)
                   init_preds
            in
            match List.find_opt verify candidates with
            | Some u -> [Affine.sub u (Affine.var pid)]
            | None -> []))
        | [] | _ :: _ :: _ -> []))
    | _ -> []

  (* === Per-check proof and rewrite === *)

  let try_eliminate ctx ~op_def_block (loop : IV.loop) (bivs : IV.biv list)
      (block : S.Block.t) : bool =
    match block.terminator with
    | Branch
        { cond =
            Op
              { op = Intop (Icomp ((Cult | Cule) as cmp));
                args = [| idx; len |];
                _
              };
          ifso;
          ifnot = _
        } ->
      let side = ref [] in
      let gidx = linearize ctx side idx in
      let glen = linearize ctx side len in
      let guards = guards_at ctx side block in
      let ivf =
        List.concat_map
          (fun biv ->
            iv_lower_fact ctx side loop biv
            @ iv_upper_fact ctx side ~op_def_block loop biv)
          bivs
      in
      let facts = guards @ ivf @ !side in
      let goal_lo = gidx in
      let goal_hi =
        match cmp with
        | Cult -> Affine.add_const (Affine.sub glen gidx) (-1)
        | Cle | Ceq | Cne | Clt | Cgt | Cge | Cugt | Cule | Cuge ->
          Affine.sub glen gidx
      in
      if entails facts goal_lo && entails facts goal_hi
      then begin
        S.Block.set_terminator block (Goto { goto = ifso; args = [||] });
        true
      end
      else false
    | Branch _ | Goto _ | Switch _ | Return _ | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call _ | Invalid _ ->
      false

  let run () : int =
    match IV.analyze () with
    | [] -> 0
    | loops ->
      let ctx = new_ctx () in
      let op_def_block = build_op_def_block () in
      let count = ref 0 in
      List.iter
        (fun ((loop : IV.loop), bivs) ->
          S.Block.Set.iter
            (fun block ->
              if try_eliminate ctx ~op_def_block loop bivs block then incr count)
            loop.body)
        loops;
      !count
end

let run (m : (module Ssa.Finished_graph)) : int =
  let module S = (val m : Ssa.Finished_graph) in
  let module B = Make (S) in
  B.run ()
