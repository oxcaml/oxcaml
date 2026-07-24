[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [strength_reduction.mli] for the design. *)

module Make (S : Ssa.Finished_graph) = struct
  module IV = Induction_var.Make (S)
  module AS = Affine_ssa.Make (S)

  (* Shared loop utilities: constants and availability at the preheader (via the
     memoized op-definition table) live in {!Induction_var}. *)
  let is_const = IV.is_const

  let available_at = IV.available_at

  let is_int_value (v : S.Instruction.t) =
    match S.Instruction.arg_type v with Cmm.Int -> true | _ -> false

  (* The coefficient of the induction variable [is_biv] in [v], requiring every
     non-IV leaf to be loop-invariant ([available_at preheader]) and
     [Int]-typed. The [Int] requirement keeps a GC-managed base pointer (e.g.
     [block] in [block + idx + i << 3], which is [Val]-typed) out of the reduced
     expression, so we never fold a pointer into the carried IV — only its
     integer offset. [None] if [v] is not an affine function [coeff * iv +
     invariant] of that shape. The constant term does not affect the
     coefficient, so displacements are ignored. Shape recognition is shared with
     the range analyses via {!Affine_ssa.coeff_of_target}. *)
  let biv_coeff ~is_biv ~preheader (v : S.Instruction.t) : int option =
    AS.coeff_of_target
      ~classify:(fun v ->
        if is_biv v
        then AS.Target
        else if available_at preheader v
        then if is_int_value v then AS.Invariant else AS.Reject
        else AS.Decompose)
      v

  (* A strength-reduction opportunity: replace the integer derived IV [derived]
     (which equals [coeff * iv + invariant]) with a fresh header parameter
     [new_index] that starts at [derived(iv := iv_init)] and is incremented by
     [step_delta = coeff * iv_step] on each back edge. *)
  type reduction =
    { header : S.Block.t;
      preheader : S.Block.t;
      back_edges : S.Block.t list;
      param_index : int;
      derived : S.Instruction.t;
      derived_id : S.Instruction.Id.t;
      derived_dbg : Debuginfo.t;
      typ : Cmm.machtype;
      step_delta : int;
      new_index : int
    }

  let is_biv_of (r : reduction) (v : S.Instruction.t) =
    IV.is_header_param r.header r.param_index v

  (* We only reduce [Int]-typed values: turning a non-[Int] (e.g. a [Val] base
     pointer) into a carried IV would create an interior pointer live across a
     GC safepoint, which a moving collector cannot handle. *)
  let is_int_typ (typ : Cmm.machtype) =
    match typ with [| Cmm.Int |] -> true | _ -> false

  let args_of (v : S.Instruction.t) : S.Instruction.t array =
    match v with
    | Op { args; _ } -> args
    | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      [||]

  (* Per header, number the new params after the existing ones, ordering the
     reductions deterministically so the analysis run that supplies
     [extra_params] and the one inside the reducer agree. *)
  let assign_new_indices (raw : reduction list) : reduction list =
    let by_header = S.Block.Tbl.create 8 in
    List.iter
      (fun r ->
        let cur =
          match S.Block.Tbl.find_opt by_header r.header with
          | Some l -> l
          | None -> []
        in
        S.Block.Tbl.replace by_header r.header (r :: cur))
      raw;
    S.Block.Tbl.fold
      (fun header group acc ->
        let base = Array.length header.params in
        let numbered =
          group
          |> List.sort (fun a b ->
              Int.compare (a.derived_id :> int) (b.derived_id :> int))
          |> List.mapi (fun i r -> { r with new_index = base + i })
        in
        (* Order across headers is irrelevant. *)
        List.rev_append numbered acc)
      by_header []

  let reductions_in_loop ((loop : IV.loop), (bivs : IV.biv list)) :
      reduction list =
    let header = loop.header in
    match IV.preheader_of loop with
    | Some preheader ->
      List.concat_map
        (fun (biv : IV.biv) ->
          match biv.step with
          | IV.Step_var _ -> []
          | IV.Step_const s ->
            let delta_i = match biv.sign with `Add -> s | `Sub -> -s in
            let is_biv v = IV.is_header_param header biv.param_index v in
            (* Affine integer derived IVs with a non-trivial scale. *)
            let cands = ref [] in
            S.Block.Set.iter
              (fun (bl : S.Block.t) ->
                Array.iter
                  (fun (instr : S.Instruction.t) ->
                    match instr with
                    | Op { id; typ; dbg; _ }
                      when is_int_typ typ && not (is_biv instr) -> (
                      match biv_coeff ~is_biv ~preheader instr with
                      | Some c when abs c >= 2 ->
                        cands := (instr, id, typ, dbg, c) :: !cands
                      | Some _ | None -> ())
                    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _
                    | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
                      ())
                  bl.body)
              loop.body;
            let cand_list = List.rev !cands in
            (* Reduce only "maximal" expressions: an affine value consumed by
               another affine candidate is left alone, so that the larger one
               subsumes it (and the smaller becomes dead). *)
            let used_by_other (d : S.Instruction.t) =
              List.exists
                (fun (e, _, _, _, _) ->
                  (not (IV.instr_same e d))
                  && Array.exists (IV.instr_same d) (args_of e))
                cand_list
            in
            List.filter_map
              (fun (derived, id, typ, dbg, c) ->
                if used_by_other derived
                then None
                else
                  Some
                    { header;
                      preheader;
                      back_edges = loop.back_edges;
                      param_index = biv.param_index;
                      derived;
                      derived_id = id;
                      derived_dbg = dbg;
                      typ;
                      step_delta = c * delta_i;
                      new_index = -1
                    })
              cand_list)
        bivs
    | None -> []

  let find_reductions () : reduction list =
    IV.analyze ()
    |> List.filter (fun ((loop : IV.loop), _) ->
        not (S.Block.is_function_start loop.header))
    |> List.concat_map reductions_in_loop
    |> assign_new_indices
end

(* The transformation, driven by the [Ssa_reducer] framework. The new header
   params are added through [Ssa_reducer.run]'s [extra_params] hook; everything
   else (replacing the derived op, materialising the init in the preheader and
   the increment on each back edge, and extending the [Goto] args) is done in
   the per-instruction / per-terminator hooks. *)
module Reducer (C : Ssa_reducer.Context) = struct
  include Ssa_reducer.Default (C)
  module A = Make (C.In)

  type action =
    | Init of A.reduction
    | Incr of A.reduction

  let replace_tbl : A.reduction C.In.Instruction.Id.Tbl.t =
    C.In.Instruction.Id.Tbl.create 16

  let pred_tbl : action list C.In.Block.Tbl.t = C.In.Block.Tbl.create 16

  let add_pred (bl : C.In.Block.t) (a : action) =
    let cur =
      match C.In.Block.Tbl.find_opt pred_tbl bl with Some l -> l | None -> []
    in
    C.In.Block.Tbl.replace pred_tbl bl (a :: cur)

  let analyze () =
    List.iter
      (fun (r : A.reduction) ->
        C.In.Instruction.Id.Tbl.replace replace_tbl r.derived_id r;
        add_pred r.preheader (Init r);
        List.iter (fun latch -> add_pred latch (Incr r)) r.back_edges)
      (A.find_reductions ())

  let new_param (r : A.reduction) : C.Instruction.t =
    C.Instruction.make_block_param (C.map_block r.header) ~index:r.new_index

  (* Clone the derived op into [c] (the preheader), substituting the IV with its
     initial value [i_init]; loop-invariant operands are reused via [map_arg]
     and constants are rematerialised. *)
  let rec clone (r : A.reduction) ~(i_init : C.In.Instruction.t)
      (c : C.Cursor.t) (v : C.In.Instruction.t) : C.Instruction.t =
    if A.is_biv_of r v
    then C.map_arg i_init
    else if A.is_const v
    then
      match v with
      | Op { op; typ; dbg; _ } -> C.emit_op c ~op ~typ ~dbg ~args:[||]
      | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
      | Stack_check _ | Name_for_debugger _ ->
        C.map_arg v
    else if A.available_at r.preheader v
    then C.map_arg v
    else
      match v with
      | Op { op; typ; args; dbg; _ } ->
        C.emit_op c ~op ~typ ~dbg ~args:(Array.map (clone r ~i_init c) args)
      | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
      | Stack_check _ | Name_for_debugger _ ->
        C.map_arg v

  let visit_instruction (block : C.In.Block.t) ~instr_index (_ : C.Cursor.t) =
    match block.body.(instr_index) with
    | Op { id; _ } -> (
      match C.In.Instruction.Id.Tbl.find_opt replace_tbl id with
      | Some r -> Ssa_reducer.Replaced (new_param r)
      | None -> Ssa_reducer.Unchanged)
    | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      Ssa_reducer.Unchanged

  (* The new trailing [Goto] arg contributed by one action, paired with the
     param index it targets. Side-effect: emits the init/increment op into
     [c]. *)
  let extra_arg (c : C.Cursor.t) ~(goto_args : C.In.Instruction.t option array)
      (a : action) : int * C.Instruction.t =
    match a with
    | Init r ->
      let i_init =
        match goto_args.(r.param_index) with
        | Some v -> v
        | None ->
          Misc.fatal_error
            "Strength_reduction: induction variable has no initial value"
      in
      r.new_index, clone r ~i_init c r.derived
    | Incr r ->
      let arg = new_param r in
      (* The per-iteration increment is [step_delta]. Instruction selection only
         ever forms an [Intop_imm (Iadd, n)] when [n] is a valid add immediate
         for the target; [Cfg_of_ssa] copies our op through verbatim without
         re-checking, so a large [step_delta] would reach the emitter as an
         illegal immediate (arm64 asserts, amd64 truncates). When it does not
         fit, materialise the step in a register and add it. *)
      let incr =
        match Cfg_selection.is_immediate Operation.Iadd r.step_delta with
        | Cfg_selectgen_target_intf.Is_immediate true ->
          C.emit_op c
            ~op:Operation.(Intop_imm (Iadd, r.step_delta))
            ~typ:r.typ ~dbg:r.derived_dbg ~args:[| arg |]
        | Cfg_selectgen_target_intf.Is_immediate false
        | Cfg_selectgen_target_intf.Use_default ->
          let cst =
            C.emit_op c
              ~op:Operation.(Const_int (Nativeint.of_int r.step_delta))
              ~typ:r.typ ~dbg:r.derived_dbg ~args:[||]
          in
          C.emit_op c
            ~op:Operation.(Intop Iadd)
            ~typ:r.typ ~dbg:r.derived_dbg ~args:[| arg; cst |]
      in
      r.new_index, incr

  let visit_terminator (block : C.In.Block.t) (c : C.Cursor.t) =
    match C.In.Block.Tbl.find_opt pred_tbl block with
    | None -> Ssa_reducer.Unchanged
    | Some actions -> (
      match block.terminator with
      | Goto { goto; args } ->
        let mapped = Array.map (Option.map C.map_arg) args in
        let extras =
          actions
          |> List.map (extra_arg c ~goto_args:args)
          |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
          |> List.map (fun (_, v) -> Some v)
          |> Array.of_list
        in
        C.finish_block c ~dbg:block.terminator_dbg
          (Goto { goto = C.map_block goto; args = Array.append mapped extras });
        Ssa_reducer.Replaced ()
      | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call _ | Invalid _ ->
        Ssa_reducer.Unchanged)
end

let run (input : (module Ssa.Finished_graph)) : (module Ssa.Finished_graph) =
  let module S = (val input : Ssa.Finished_graph) in
  let module A = Make (S) in
  match A.find_reductions () with
  | [] -> input
  | reductions ->
    (* New params per header, keyed by raw block id and ordered by new index, to
       feed [Ssa_reducer.run]'s [extra_params]. *)
    let tbl :
        (int, (int * (Cmm.machtype_component * string option)) list) Hashtbl.t =
      Hashtbl.create 8
    in
    List.iter
      (fun (r : A.reduction) ->
        let id = (r.header.id :> int) in
        let cur =
          match Hashtbl.find_opt tbl id with Some l -> l | None -> []
        in
        Hashtbl.replace tbl id ((r.new_index, (r.typ.(0), Some "sr")) :: cur))
      reductions;
    let extra_params id =
      match Hashtbl.find_opt tbl id with
      | None -> [||]
      | Some l ->
        l
        |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
        |> List.map snd |> Array.of_list
    in
    Ssa_reducer.run ~extra_params (module Reducer : Ssa_reducer.Reducer) input
