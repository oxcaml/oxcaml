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

  let linearize_goal = A.linearize_goal

  let guards_at = A.guards_at

  (* === Loop-invariance of atoms ===

     Deliberately more conservative than [IV.is_loop_invariant]: values other
     than [Op]s and [Block_param]s (projections, trap markers, ...) are rejected
     rather than approved, since a spurious invariance fact here could license
     an unsound elimination. *)

  let atom_invariant ctx ~op_def_block ~(body : S.Block.Set.t) id =
    match atom_instr ctx id with
    | v when IV.is_const v -> true
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

  (* === Induction-variable range facts ===

     For an increasing basic IV [i] (constant step [c > 0]) with a single back
     edge, we derive:

     - An invariant upper bound [u - i >= 0], from a dominating guard on the
     back-edge path that tests the back-edge value [arg] -- or the header
     parameter [i] itself -- against a loop-invariant bound [B], oriented
     [tested <= B] / [tested < B]. Machine-value soundness: + guard on [arg]:
     the comparison is on the very machine value that becomes the next header
     value, so [next <= u] directly ([u = B], or [B - 1] for a strict test)
     whatever arithmetic produced [arg]; + guard on [i]: from [i <= B] and [B <=
     max_int] (the no-wrap obligation below), [i + c] is computed without
     wrapping, so [next <= B + c] (u = [B + c], or [B + c - 1]). In both shapes
     every loop entry must also satisfy [init <= u], checked by Fourier-Motzkin
     from the entry-edge guards.

     - A constant lower bound [i - min(inits) >= 0] (constant inits only). This
     is inductive only while the increment cannot wrap, so it is emitted only
     under the no-wrap obligation: [u <= max_int], discharged by Fourier-Motzkin
     from the invariant guards dominating the header together with the range
     side-facts of atomized shifts -- e.g. an array length ([lsr] of the header
     word) is at most [2^54 - 1], so a bound tested against one is comfortably
     below [max_int]. Since the OCaml-[int] threshold [max_int] is far from the
     64-bit limit, [u <= max_int] keeps [u + c] (any OCaml-[int] step [c]) clear
     of wrapping.

     No fact is asserted for free: without a provable [u], the lower fact is not
     emitted, and the upper facts are justified by the dominating tests on the
     actual machine values. *)

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

  let iv_facts ctx side ~op_def_block (loop : IV.loop) (biv : IV.biv) :
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
            (* Upper-bound candidates: [fst] is [u], [snd] says whether the
               guard was on the param (so [u] embeds an un-wrapped [+ c] and the
               no-wrap obligation is required for the upper fact too). *)
            let bound_form (other : [`Value of S.Instruction.t | `Const of int])
                =
              match other with
              | `Const b -> Some (Affine.const b)
              | `Value v ->
                let f = linearize ctx side v in
                if affine_invariant ctx ~op_def_block ~body:loop.body f
                then Some f
                else None
            in
            let candidate ~extra (cmp, other) =
              let mk delta =
                match bound_form other with
                | None -> None
                | Some b -> (
                  match Affine.add_const_checked b delta with
                  | u -> Some u
                  | exception Fourier_motzkin.Overflow -> None)
              in
              match (cmp : Cmm.integer_comparison) with
              | Cle -> mk extra
              | Clt -> mk (extra - 1)
              | Ceq | Cne | Cgt | Cge | Cult | Cugt | Cule | Cuge -> None
            in
            let arg_candidates =
              A.bounding_guards_at ~target:pe ~matches:(IV.instr_same arg)
              |> List.filter_map (fun g ->
                  Option.map (fun u -> u, false) (candidate ~extra:0 g))
            in
            let param_candidates =
              A.bounding_guards_at ~target:pe
                ~matches:(IV.is_header_param header k)
              |> List.filter_map (fun g ->
                  Option.map (fun u -> u, true) (candidate ~extra:c g))
            in
            let init_preds = IV.entry_predecessors loop in
            let verify u =
              (not (List.is_empty init_preds))
              && List.for_all
                   (fun ip ->
                     match arg_to_header ip with
                     | Some iarg ->
                       (* Bind the goal first: its linearization pushes side
                          facts that the fact list below must include. *)
                       let goal = Affine.sub u (linearize ctx side iarg) in
                       entails (guards_at ctx side ip @ !side) goal
                     | None -> false)
                   init_preds
            in
            (* The no-wrap obligation [u <= max_int], from the invariant guards
               dominating the header (plus accumulated shift-range side
               facts). *)
            let no_wrap u =
              match
                Affine.add_const_checked (Affine.scale_checked (-1) u) max_int
              with
              | goal -> entails (guards_at ctx side header @ !side) goal
              | exception Fourier_motzkin.Overflow -> false
            in
            let chosen =
              List.find_opt
                (fun (u, needs_no_wrap) ->
                  verify u && ((not needs_no_wrap) || no_wrap u))
                (arg_candidates @ param_candidates)
            in
            match chosen with
            | None -> []
            | Some (u, needs_no_wrap) ->
              let upper = Affine.sub u (Affine.var pid) in
              let lower_ok = needs_no_wrap || no_wrap u in
              let lower =
                if lower_ok
                then
                  match init_lower_const ctx side biv with
                  | Some m -> [Affine.add_const (Affine.var pid) (-m)]
                  | None -> []
                else []
              in
              upper :: lower))
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
      (* Goals use the uncertified linearization: the entailments below prove [0
         <= gidx] and [gidx <= glen - 1] together with [glen <= max_int] (the
         third goal), which pins both forms' integer values into machine range;
         since a machine value always equals its form modulo 2^64, the machine
         check is then implied. Facts (guards, IV facts and shift side
         conditions) all come from the certified [linearize]. *)
      let gidx = linearize_goal ctx side idx in
      let glen = linearize_goal ctx side len in
      let guards = guards_at ctx side block in
      let ivf =
        List.concat_map
          (fun biv -> iv_facts ctx side ~op_def_block loop biv)
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
      let goal_len_in_range =
        match
          Affine.add_const_checked (Affine.scale_checked (-1) glen) max_int
        with
        | goal -> Some goal
        | exception Fourier_motzkin.Overflow -> None
      in
      let proved =
        match goal_len_in_range with
        | None -> false
        | Some goal_len_in_range ->
          entails facts goal_lo && entails facts goal_hi
          && entails facts goal_len_in_range
      in
      if proved
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
      let op_def_block = IV.op_def () in
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
