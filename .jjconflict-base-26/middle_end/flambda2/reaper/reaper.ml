(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let get_code_metadata ~cmx_loader ~all_code =
  let load_code = Flambda_cmx.get_imported_code cmx_loader in
  fun code_id ->
    Code_or_metadata.code_metadata
      (match Exported_code.find all_code code_id with
      | Some code -> code
      | None -> Exported_code.find_exn (load_code ()) code_id)

(* Import the .cmx file defining [code_id] if its metadata is not already
   available. Only the LTO rebuild gets here: it does not run [Simplify], so the
   metadata of code from units that did not take part in the solve is only
   available from their .cmx files. The metadata of the units in the analysis
   scope comes from the solution instead, and their .cmx files must not be
   read. *)
let load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader ~all_code
    code_id =
  let comp_unit = Code_id.get_compilation_unit code_id in
  if Analysis_scope.contains_unit analysis_scope comp_unit
  then
    Misc.fatal_errorf
      "Code ID %a belongs to the analysis scope, so its metadata must come \
       from the solution"
      Code_id.print code_id;
  if
    (not (Exported_code.mem code_id all_code))
    && not
         (Exported_code.mem code_id
            (Flambda_cmx.get_imported_code cmx_loader ()))
  then
    ignore
      (Flambda_cmx.load_cmx_file_contents cmx_loader comp_unit
        : Typing_env.Serializable.t option)

let get_code_metadata_or_load ~analysis_scope ~cmx_loader ~all_code code_id =
  (match Exported_code.find all_code code_id with
  | Some _ -> ()
  | None ->
    load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader ~all_code
      code_id);
  get_code_metadata ~cmx_loader ~all_code code_id

module Staged = struct
  module Solve_inputs = struct
    type t =
      { deps : Global_flow_graph.graph;
        slot_offsets_inputs : Slot_offsets_analysis.Inputs.t;
        code_deps : Traverse_acc.code_dep Code_id.Map.t;
        code_references : Traverse_acc.code_reference list;
        le_monde_exterieur : Symbol.t;
        applications : Traverse_acc.Applications.t
      }

    let ids_for_export
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          le_monde_exterieur;
          applications
        } =
      let ids =
        Code_id.Map.fold
          (fun code_id code_dep ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Traverse_acc.ids_for_export_code_dep code_dep))
          code_deps Ids_for_export.empty
      in
      Ids_for_export.union_list
        [ Ids_for_export.add_symbol ids le_monde_exterieur;
          Global_flow_graph.ids_for_export deps;
          Slot_offsets_analysis.Inputs.ids_for_export slot_offsets_inputs;
          Traverse_acc.ids_for_export_code_references code_references;
          Traverse_acc.Applications.ids_for_export applications ]

    let prune_for_lto t =
      { t with
        code_deps =
          Code_id.Map.map
            (fun (code_dep : Traverse_acc.code_dep) ->
              { code_dep with
                code_metadata =
                  Code_metadata.with_result_types Unknown code_dep.code_metadata
              })
            t.code_deps
      }

    let fields_for_export t = Global_flow_graph.fields_for_export t.deps

    let referenced_compilation_units t =
      Traverse_acc.code_references_compilation_units t.code_references

    let apply_renaming
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          le_monde_exterieur;
          applications
        } renaming ~rename_field =
      let code_deps =
        Code_id.Map.fold
          (fun code_id code_dep map ->
            Code_id.Map.add
              (Renaming.apply_code_id renaming code_id)
              (Traverse_acc.apply_renaming_code_dep code_dep renaming)
              map)
          code_deps Code_id.Map.empty
      in
      { deps = Global_flow_graph.apply_renaming deps renaming ~rename_field;
        slot_offsets_inputs =
          Slot_offsets_analysis.Inputs.apply_renaming slot_offsets_inputs
            renaming;
        code_deps;
        code_references =
          Traverse_acc.apply_renaming_code_references code_references renaming;
        le_monde_exterieur = Renaming.apply_symbol renaming le_monde_exterieur;
        applications =
          Traverse_acc.Applications.apply_renaming applications renaming
      }
  end

  module Rebuild_inputs = struct
    type t =
      { toplevel_expr : Rev_expr.t;
        code : Rev_expr.rev_code Code_id.Map.t;
        ordered_code_ids : Code_id.t array;
        fixed_arity_continuations : Continuation.Set.t;
        continuation_info : Traverse_acc.continuation_info Continuation.Map.t
      }

    let ids_for_export
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info
        } =
      let ids = Rev_expr.ids_for_export toplevel_expr in
      let ids =
        Code_id.Map.fold
          (fun code_id rev_code ids ->
            Ids_for_export.union
              (Ids_for_export.add_code_id ids code_id)
              (Rev_expr.ids_for_export_code rev_code))
          code ids
      in
      let ids =
        Array.fold_left Ids_for_export.add_code_id ids ordered_code_ids
      in
      let ids =
        Continuation.Set.fold
          (fun cont ids -> Ids_for_export.add_continuation ids cont)
          fixed_arity_continuations ids
      in
      Continuation.Map.fold
        (fun cont info ids ->
          Ids_for_export.union
            (Ids_for_export.add_continuation ids cont)
            (Traverse_acc.ids_for_export_continuation_info info))
        continuation_info ids

    let apply_renaming
        { toplevel_expr;
          code;
          ordered_code_ids;
          fixed_arity_continuations;
          continuation_info
        } renaming =
      let toplevel_expr' = Rev_expr.apply_renaming toplevel_expr renaming in
      let code' =
        Code_id.Map.fold
          (fun code_id rev_code code ->
            Code_id.Map.add
              (Renaming.apply_code_id renaming code_id)
              (Rev_expr.apply_renaming_code rev_code renaming)
              code)
          code Code_id.Map.empty
      in
      let ordered_code_ids' =
        Array.map (Renaming.apply_code_id renaming) ordered_code_ids
      in
      let fixed_arity_continuations' =
        Continuation.Set.fold
          (fun cont conts ->
            Continuation.Set.add
              (Renaming.apply_continuation renaming cont)
              conts)
          fixed_arity_continuations Continuation.Set.empty
      in
      let continuation_info' =
        Continuation.Map.fold
          (fun cont info map ->
            Continuation.Map.add
              (Renaming.apply_continuation renaming cont)
              (Traverse_acc.apply_renaming_continuation_info info renaming)
              map)
          continuation_info Continuation.Map.empty
      in
      { toplevel_expr = toplevel_expr';
        code = code';
        ordered_code_ids = ordered_code_ids';
        fixed_arity_continuations = fixed_arity_continuations';
        continuation_info = continuation_info'
      }
  end

  module Rebuild_data = struct
    type t =
      { analysis : Analysis.data;
        code_changes : Unboxing_analysis.code_changes_data;
        slot_offsets : Exported_offsets.t
      }

    let empty =
      { analysis = Analysis.empty;
        code_changes = Unboxing_analysis.empty_code_changes_data;
        slot_offsets = Exported_offsets.empty
      }

    let ids_for_export { analysis; code_changes; slot_offsets = _ } =
      Unboxing_analysis.code_changes_ids_for_export code_changes
        (Analysis.ids_for_export analysis)

    let fields_for_export { analysis; code_changes; slot_offsets = _ } =
      Unboxing_analysis.code_changes_fields_for_export code_changes
        (Analysis.fields_for_export analysis)

    let apply_renaming { analysis; code_changes; slot_offsets } renaming
        ~rename_field =
      { analysis = Analysis.apply_renaming analysis renaming ~rename_field;
        code_changes =
          Unboxing_analysis.code_changes_apply_renaming code_changes renaming
            ~rename_field;
        slot_offsets
      }

    let partition_by_compilation_unit { analysis; code_changes; slot_offsets } =
      let analysis = Analysis.partition_by_compilation_unit analysis in
      let code_changes =
        Unboxing_analysis.partition_code_changes_by_compilation_unit
          code_changes
      in
      let slot_offsets =
        Exported_offsets.partition_by_compilation_unit slot_offsets
      in
      let add_units map units =
        Compilation_unit.Map.fold
          (fun compilation_unit _ units ->
            Compilation_unit.Set.add compilation_unit units)
          map units
      in
      let units =
        Compilation_unit.Set.empty |> add_units analysis
        |> add_units code_changes |> add_units slot_offsets
      in
      let find map compilation_unit ~default =
        Option.value
          (Compilation_unit.Map.find_opt compilation_unit map)
          ~default
      in
      Compilation_unit.Set.fold
        (fun compilation_unit parts ->
          let part =
            { analysis = find analysis compilation_unit ~default:Analysis.empty;
              code_changes =
                find code_changes compilation_unit
                  ~default:Unboxing_analysis.empty_code_changes_data;
              slot_offsets =
                find slot_offsets compilation_unit
                  ~default:Exported_offsets.empty
            }
          in
          Compilation_unit.Map.add compilation_unit part parts)
        units Compilation_unit.Map.empty
  end

  module Solution = struct
    type t =
      { analysis_scope : Analysis_scope.t;
        data : Rebuild_data.t
      }

    let rebuild_data t = t.data
  end

  module Rebuild_solution = struct
    type store =
      | Single of Rebuild_data.t
      | Sharded of (Compilation_unit.t -> Rebuild_data.t)

    type t =
      { analysis_scope : Analysis_scope.t;
        store : store
      }

    let of_solution ({ analysis_scope; data } : Solution.t) =
      { analysis_scope; store = Single data }

    let sharded ~analysis_scope get_unit =
      { analysis_scope; store = Sharded get_unit }

    let data_for_unit t compilation_unit =
      match t.store with
      | Single data -> data
      | Sharded get_unit -> get_unit compilation_unit

    let analysis t : Analysis.result =
      match t.store with
      | Single data -> Single data.analysis
      | Sharded get_unit ->
        Sharded
          (fun compilation_unit ->
            (get_unit compilation_unit).Rebuild_data.analysis)

    let code_changes t =
      match t.store with
      | Single data ->
        Unboxing_analysis.single_code_changes ~analysis_scope:t.analysis_scope
          data.code_changes
      | Sharded get_unit ->
        Unboxing_analysis.sharded_code_changes ~analysis_scope:t.analysis_scope
          (fun compilation_unit ->
            (get_unit compilation_unit).Rebuild_data.code_changes)

    let offsets_for_free_names t free_names =
      let offsets =
        Function_slot.Set.fold
          (fun function_slot offsets ->
            let solved =
              (data_for_unit t
                 (Function_slot.get_compilation_unit function_slot))
                .slot_offsets
            in
            match
              Exported_offsets.function_slot_offset solved function_slot
            with
            | Some info ->
              Exported_offsets.add_function_slot_offset offsets function_slot
                info
            | None ->
              Misc.fatal_errorf "Reaper: no solved offset for function slot %a"
                Function_slot.print function_slot)
          (Name_occurrences.all_function_slots_at_normal_mode free_names)
          Exported_offsets.empty
      in
      Value_slot.Set.fold
        (fun value_slot offsets ->
          let solved =
            (data_for_unit t (Value_slot.get_compilation_unit value_slot))
              .slot_offsets
          in
          match Exported_offsets.value_slot_offset solved value_slot with
          | Some info ->
            Exported_offsets.add_value_slot_offset offsets value_slot info
          | None ->
            Misc.fatal_errorf "Reaper: no solved offset for value slot %a"
              Value_slot.print value_slot)
        (Name_occurrences.all_value_slots_at_normal_mode free_names)
        offsets
  end

  let traverse ~free_names ~cmx_loader ~all_code unit =
    let Traverse.
          { toplevel_expr;
            code;
            ordered_code_ids;
            deps;
            fixed_arity_continuations;
            continuation_info;
            code_deps;
            code_references;
            le_monde_exterieur;
            applications;
            all_sets_of_closures = _;
            closure_function_decls
          } =
      Traverse.run ~top_level_return_escapes:false unit
    in
    let slot_offsets_inputs =
      Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
        ~code_deps
        ~get_code_metadata:(get_code_metadata ~cmx_loader ~all_code)
    in
    let solve_inputs =
      Solve_inputs.
        { deps;
          slot_offsets_inputs;
          code_deps;
          code_references;
          le_monde_exterieur;
          applications
        }
    in
    let rebuild_inputs =
      { Rebuild_inputs.toplevel_expr;
        code;
        ordered_code_ids;
        fixed_arity_continuations;
        continuation_info
      }
    in
    solve_inputs, rebuild_inputs

  let solve ~analysis_scope (solve_inputs : Solve_inputs.t list) =
    let deps =
      match solve_inputs with
      | [] -> Global_flow_graph.create ()
      | first :: rest ->
        List.fold_left
          (fun deps (inputs : Solve_inputs.t) ->
            Global_flow_graph.union deps inputs.deps)
          first.deps rest
    in
    let slot_offsets_inputs =
      List.fold_left
        (fun combined (inputs : Solve_inputs.t) ->
          Slot_offsets_analysis.Inputs.union combined inputs.slot_offsets_inputs)
        Slot_offsets_analysis.Inputs.empty solve_inputs
    in
    let code_deps =
      List.fold_left
        (fun code_deps (inputs : Solve_inputs.t) ->
          Code_id.Map.disjoint_union code_deps inputs.code_deps)
        Code_id.Map.empty solve_inputs
    in
    let applications =
      List.fold_left
        (fun applications (inputs : Solve_inputs.t) ->
          Traverse_acc.Applications.union applications inputs.applications)
        Traverse_acc.Applications.empty solve_inputs
    in
    List.iter
      (fun (inputs : Solve_inputs.t) ->
        Cross_unit_calls.link deps ~analysis_scope ~code_deps
          ~le_monde_exterieur:inputs.le_monde_exterieur inputs.code_references)
      solve_inputs;
    let solved_dep, analysis =
      Profile.record_call ~accumulate:true "solver" (fun () ->
          Analysis.fixpoint_data deps ~applications ~analysis_scope)
    in
    let () =
      if Flambda_features.debug_reaper "print-solved"
      then (
        Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
        Dot_printer.print_solved_dep solved_dep deps)
    in
    let code_changes_data =
      Unboxing_analysis.compute_code_changes_data solved_dep ~analysis_scope
        ~rewrite_kind_with_subkind:(fun _name kind ->
          Types_rewriter.erase_subkind kind)
        ~rewrite_result_types:(fun ~my_closure:_ ~params:_ ~results:_ _types ->
          Or_unknown_or_bottom.Unknown)
        ~code_deps
    in
    let code_changes =
      Unboxing_analysis.single_code_changes ~analysis_scope code_changes_data
    in
    let slot_offsets =
      Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~analysis_scope
        ~code_changes solved_dep
    in
    { Solution.analysis_scope;
      data =
        { Rebuild_data.analysis;
          code_changes = code_changes_data;
          slot_offsets = slot_offsets.Slot_offsets.exported_offsets
        }
    }

  let rebuild ~unit_metadata ~rebuild_inputs ~(solution : Rebuild_solution.t)
      ~machine_width ~cmx_loader ~all_code =
    let analysis_scope = solution.Rebuild_solution.analysis_scope in
    let get_code_metadata =
      get_code_metadata_or_load ~analysis_scope ~cmx_loader ~all_code
    in
    let Rebuild_inputs.
          { toplevel_expr;
            code;
            ordered_code_ids;
            fixed_arity_continuations;
            continuation_info
          } =
      rebuild_inputs
    in
    let code_changes = Rebuild_solution.code_changes solution in
    let Rebuild.
          { body; all_code = rebuilt_code; code_ids_to_remember; free_names } =
      Rebuild.rebuild ~machine_width ~ordered_code_ids
        ~fixed_arity_continuations ~continuation_info ~final_typing_env:None
        ~rewrite_kind_with_subkind:(fun _ kind ->
          Types_rewriter.erase_subkind kind)
        ~code_changes
        (Rebuild_solution.analysis solution)
        get_code_metadata toplevel_expr code
    in
    let is_foreign code_id =
      not (Current_unit.is_current (Code_id.get_compilation_unit code_id))
    in
    (* The metadata of foreign code comes from the solution for units that took
       part in the solve, and from their .cmx files otherwise. *)
    let solution_metadata =
      Name_occurrences.fold_code_ids free_names ~init:[]
        ~f:(fun solution_metadata code_id ->
          if not (is_foreign code_id)
          then solution_metadata
          else
            match Unboxing_analysis.find_code_metadata code_changes code_id with
            | Some code_metadata -> code_metadata :: solution_metadata
            | None ->
              load_cmx_for_non_participant_code_id ~analysis_scope ~cmx_loader
                ~all_code code_id;
              solution_metadata)
    in
    (* Local entries are replaced by rebuilt code below. *)
    let imported_code =
      Exported_code.merge
        (Exported_code.mark_as_imported all_code)
        (Exported_code.mark_as_imported
           (Flambda_cmx.get_imported_code cmx_loader ()))
      |> Exported_code.filter ~f:is_foreign
    in
    let imported_code =
      List.fold_left Exported_code.add_code_metadata imported_code
        solution_metadata
    in
    let all_code =
      Exported_code.add_code
        ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
        rebuilt_code imported_code
    in
    let exported_offsets =
      Rebuild_solution.offsets_for_free_names solution free_names
    in
    ( Flambda_unit.create_of_metadata_and_body unit_metadata body,
      all_code,
      exported_offsets )
end

let run ~machine_width ~cmx_loader ~all_code ~final_typing_env ~free_names
    (unit : Flambda_unit.t) =
  let get_code_metadata = get_code_metadata ~cmx_loader ~all_code in
  let analysis_scope = Analysis_scope.Current_unit in
  let Traverse.
        { toplevel_expr;
          code;
          ordered_code_ids;
          deps;
          fixed_arity_continuations;
          continuation_info;
          code_deps;
          code_references;
          le_monde_exterieur;
          applications;
          all_sets_of_closures;
          closure_function_decls
        } =
    Traverse.run ~top_level_return_escapes:true unit
  in
  Cross_unit_calls.link deps ~analysis_scope ~code_deps ~le_monde_exterieur
    code_references;
  let solved_dep, uses =
    Profile.record_call ~accumulate:true "solver" (fun () ->
        Analysis.fixpoint deps ~applications ~analysis_scope)
  in
  let () =
    if Flambda_features.debug_reaper "print-solved"
    then (
      Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
      Dot_printer.print_solved_dep solved_dep deps)
  in
  let types_rewrite_context =
    Types_rewriter.prepare_rewrite_context solved_dep all_sets_of_closures
  in
  let code_changes =
    Unboxing_analysis.compute_code_changes solved_dep ~analysis_scope
      ~rewrite_kind_with_subkind:
        (Types_rewriter.rewrite_kind_with_subkind types_rewrite_context)
      ~rewrite_result_types:(fun ~my_closure ~params ~results types ->
        match final_typing_env with
        | None -> Or_unknown_or_bottom.Unknown
        | Some old_typing_env ->
          Or_unknown_or_bottom.Ok
            (Types_rewriter.rewrite_result_types types_rewrite_context
               ~old_typing_env ~my_closure ~params ~results types))
      ~code_deps
  in
  let slot_offsets_inputs =
    Slot_offsets_analysis.Inputs.create ~free_names ~closure_function_decls
      ~code_deps ~get_code_metadata
  in
  let slot_offsets =
    Slot_offsets_analysis.compute ~inputs:slot_offsets_inputs ~analysis_scope
      ~code_changes solved_dep
  in
  let Rebuild.{ body; all_code; code_ids_to_remember; _ } =
    Rebuild.rebuild ~machine_width ~ordered_code_ids ~fixed_arity_continuations
      ~continuation_info ~final_typing_env
      ~rewrite_kind_with_subkind:
        (Types_rewriter.rewrite_kind_with_subkind types_rewrite_context)
      ~code_changes uses get_code_metadata toplevel_expr code
  in
  let all_code =
    Exported_code.add_code
      ~keep_code:(fun code_id -> Code_id.Set.mem code_id code_ids_to_remember)
      all_code
      (Exported_code.mark_as_imported
         (Flambda_cmx.get_imported_code cmx_loader ()))
  in
  let final_typing_env =
    Option.map
      (Types_rewriter.rewrite_typing_env types_rewrite_context
         ~unit_symbol:(Flambda_unit.module_symbol unit))
      final_typing_env
  in
  Flambda_unit.with_body unit body, all_code, slot_offsets, final_typing_env
