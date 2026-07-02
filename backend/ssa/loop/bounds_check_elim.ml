[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [bounds_check_elimination.md] for the design and soundness argument. *)

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)

  (* Affine forms and Fourier-Motzkin feasibility/entailment live in the shared
     {!Fourier_motzkin} module. *)
  module Affine = Fourier_motzkin.Affine

  let entails = Fourier_motzkin.entails

  (* SSA linearization and dominating-guard facts live in the shared
     {!Affine_ssa} module; re-bind the pieces this pass uses. *)
  module A = Affine_ssa.Make (S)

  let new_ctx = A.new_ctx

  let atom_instr = A.atom_instr

  let find_header_param_atom = A.find_header_param_atom

  let linearize = A.linearize

  let guards_at = A.guards_at

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

  (* The overflow reasoning below (induction-variable monotonicity, and the [x
     lsl k = 2^k * x] decomposition) is only valid for operands that stay in the
     63-bit OCaml [int] range; a full-width [int64#]/[nativeint] index could in
     principle wrap. [Cmm.machtype_component] is [Int] for both, so we cannot
     tell them apart by type. What we *can* recognise is the shape the frontend
     only ever emits for a genuine array / string / bytes bounds check -- an
     unsigned comparison guarding a load whose out-of-bounds edge raises -- and
     whose operands are therefore tagged [int]s. We require that shape here, via
     the out-of-bounds ([ifnot]) edge reaching a raising terminator through a
     chain of gotos, so the pass stays confined to real bounds checks. *)
  let rec out_of_bounds_raises ~fuel (bl : S.Block.t) : bool =
    fuel > 0
    &&
    match bl.terminator with
    | Raise _ | Invalid _ -> true
    | Goto { goto; _ } -> out_of_bounds_raises ~fuel:(fuel - 1) goto
    | Branch _ | Switch _ | Return _ | Tailcall_self _ | Tailcall_func _
    | Call _ ->
      false

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
          ifnot
        }
      when out_of_bounds_raises ~fuel:5 ifnot ->
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
