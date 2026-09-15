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

module PTA = Points_to_analysis
module UA = Unboxing_analysis
module Schemas = Datalog_helpers.Schemas

module Solution = struct
  (* We use unit maps instead of sets, because it allows reuse of the tables
     stored in the Datalog database without copying. *)
  type 'f t =
    { analysis_scope : Analysis_scope.t;
      has_usage : unit Code_id_or_name.Map.t;
      has_source : unit Code_id_or_name.Map.t;
      field_of_constructor_is_used : unit Field.Map.t Code_id_or_name.Map.t;
      directly_called : Code_id.Set.t Or_unknown.t Code_id_or_name.Map.t;
      known_masks : PTA.keep_or_delete list Code_id_or_name.Map.t;
      unknown_masks : PTA.keep_or_delete list list Code_id_or_name.Map.t;
      unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
      changed_representation :
        (UA.changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t;
      code_changes : Unboxing_analysis.code_changes;
      slot_offsets : Slot_offsets.result;
      types_rewrite_context :
        ('f, Types_rewriter.rewrite_context) Traverse.With_types.t;
      final_typing_env : ('f, typing_env) Traverse.With_types.t
    }

  let ids_for_export
      { analysis_scope = _;
        has_usage;
        has_source;
        field_of_constructor_is_used;
        directly_called;
        known_masks;
        unknown_masks;
        unboxed_fields;
        changed_representation;
        code_changes;
        slot_offsets = _;
        types_rewrite_context = Without_types;
        final_typing_env = Without_types
      } =
    let add_keys map ids =
      Code_id_or_name.Map.fold
        (fun id _ ids -> Ids_for_export.add_code_id_or_name ids id)
        map ids
    in
    (* Slots are not hashconsed, so the offsets need no renaming. *)
    let ids =
      Ids_for_export.union_list
        [ Schemas.N.ids_for_export has_usage;
          Schemas.N.ids_for_export has_source;
          Schemas.NF.ids_for_export field_of_constructor_is_used ]
    in
    let ids = add_keys known_masks ids in
    let ids = add_keys unknown_masks ids in
    let ids =
      Code_id_or_name.Map.fold
        (fun callee targets ids ->
          let ids = Ids_for_export.add_code_id_or_name ids callee in
          match (targets : _ Or_unknown.t) with
          | Unknown -> ids
          | Known targets ->
            Code_id.Set.fold
              (fun code_id ids -> Ids_for_export.add_code_id ids code_id)
              targets ids)
        directly_called ids
    in
    let ids = UA.unboxed_fields_ids_for_export unboxed_fields ids in
    let ids =
      UA.changed_representation_ids_for_export changed_representation ids
    in
    UA.code_changes_ids_for_export code_changes ids

  let apply_renaming
      { analysis_scope;
        has_usage;
        has_source;
        field_of_constructor_is_used;
        directly_called;
        known_masks;
        unknown_masks;
        unboxed_fields;
        changed_representation;
        code_changes;
        slot_offsets;
        types_rewrite_context = Without_types;
        final_typing_env = Without_types
      } renaming =
    let rename_map map renaming ~f =
      Code_id_or_name.Map.fold
        (fun id value map ->
          Code_id_or_name.Map.add
            (Renaming.apply_code_id_or_name renaming id)
            (f value) map)
        map Code_id_or_name.Map.empty
    in
    { analysis_scope;
      has_usage = Schemas.N.apply_renaming has_usage renaming;
      has_source = Schemas.N.apply_renaming has_source renaming;
      field_of_constructor_is_used =
        Schemas.NF.apply_renaming field_of_constructor_is_used renaming;
      directly_called =
        rename_map directly_called renaming ~f:(fun targets ->
            Or_unknown.map targets ~f:(fun targets ->
                Code_id.Set.fold
                  (fun code_id targets ->
                    Code_id.Set.add
                      (Renaming.apply_code_id renaming code_id)
                      targets)
                  targets Code_id.Set.empty));
      known_masks = rename_map known_masks renaming ~f:Fun.id;
      unknown_masks = rename_map unknown_masks renaming ~f:Fun.id;
      unboxed_fields = UA.unboxed_fields_apply_renaming unboxed_fields renaming;
      changed_representation =
        UA.changed_representation_apply_renaming changed_representation renaming;
      code_changes = UA.code_changes_apply_renaming code_changes renaming;
      slot_offsets;
      types_rewrite_context = Without_types;
      final_typing_env = Without_types
    }
end

let fixpoint0 (graph : Global_flow_graph.graph) ~analysis_scope =
  let datalog = Global_flow_graph.to_datalog graph in
  let with_provenance = Flambda_features.debug_reaper "prov" in
  let stats =
    if with_provenance || Flambda_features.debug_reaper "stats"
    then Some (Datalog.Schedule.create_stats ~with_provenance datalog)
    else None
  in
  let db = PTA.perform_analysis datalog ?stats ~analysis_scope in
  let (unboxing : UA.result) = UA.perform_analysis db ?stats ~analysis_scope in
  Option.iter (Format.eprintf "%a@." Datalog.Schedule.print_stats) stats;
  if Flambda_features.debug_reaper "db"
  then Format.eprintf "%a@." Datalog.print db;
  unboxing

let fixpoint graph ~analysis_scope =
  if Flambda_features.debug_reaper "print-raw" then Dot_printer.print_dep graph;
  let solved_dep =
    Profile.record_call ~accumulate:true "solver" (fun () ->
        fixpoint0 graph ~analysis_scope)
  in
  if Flambda_features.debug_reaper "print-solved"
  then (
    Format.printf "RESULT@ %a@." Unboxing_analysis.pp_result solved_dep;
    Dot_printer.print_solved_dep solved_dep graph);
  solved_dep

let answer_call_queries db (applications : Traverse_acc.Applications.t) =
  Code_id_or_name.Map.fold
    (fun callee ({ known; unknown } : Traverse_acc.Applications.bounds)
         (~directly_called, ~known_masks, ~unknown_masks) ->
      let directly_called, known_masks =
        match known with
        | None -> directly_called, known_masks
        | Some width ->
          let name =
            Code_id_or_name.pattern_match' callee
              ~name:(fun name -> name)
              ~code_id:(fun _ ->
                Misc.fatal_errorf "Analysis: expected a named callee, found %a"
                  Code_id_or_name.print callee)
          in
          ( Code_id_or_name.Map.add callee
              (PTA.code_id_actually_directly_called db name)
              directly_called,
            Code_id_or_name.Map.add callee
              (PTA.arguments_used_by_known_arity_call db callee width)
              known_masks )
      in
      let unknown_masks =
        match unknown with
        | None -> unknown_masks
        | Some widths ->
          Code_id_or_name.Map.add callee
            (PTA.arguments_used_by_unknown_arity_call db callee widths)
            unknown_masks
      in
      ~directly_called, ~known_masks, ~unknown_masks)
    applications
    ( ~directly_called:Code_id_or_name.Map.empty,
      ~known_masks:Code_id_or_name.Map.empty,
      ~unknown_masks:Code_id_or_name.Map.empty )

let rewrite_kind_with_subkind (type f)
    (types_rewrite_context : (f, _) Traverse.With_types.t) =
  match types_rewrite_context with
  | With_types types_rewrite_context ->
    Types_rewriter.rewrite_kind_with_subkind types_rewrite_context
  | Without_types -> fun _ -> Types_rewriter.erase_subkind

(* [deps] must have its delayed dependencies resolved. *)
let solve0 (type f) deps ~analysis_scope ~code_deps ~applications ~free_names
    ~(all_sets_of_closures : (f, _) Traverse.With_types.t)
    ~(final_typing_env : (f, typing_env) Traverse.With_types.t)
    ~(module_symbol : (f, Symbol.t) Traverse.With_types.t) : f Solution.t =
  let unboxing = fixpoint deps ~analysis_scope in
  let db = unboxing.db in
  let ~directly_called, ~known_masks, ~unknown_masks =
    answer_call_queries db applications
  in
  let types_rewrite_context =
    Traverse.With_types.map
      (Types_rewriter.prepare_rewrite_context unboxing)
      all_sets_of_closures
  in
  let code_changes =
    Unboxing_analysis.compute_code_changes unboxing ~analysis_scope
      ~rewrite_kind_with_subkind:
        (rewrite_kind_with_subkind types_rewrite_context)
      ~rewrite_result_types:(fun ~my_closure ~params ~results types ->
        match types_rewrite_context, final_typing_env with
        | Without_types, Without_types -> Or_unknown_or_bottom.Unknown
        | With_types types_rewrite_context, With_types old_typing_env ->
          Or_unknown_or_bottom.Ok
            (Types_rewriter.rewrite_result_types types_rewrite_context
               ~old_typing_env ~my_closure ~params ~results types))
      ~code_deps
  in
  let slot_offsets =
    Slot_offsets_analysis.compute ~free_names ~analysis_scope unboxing
  in
  let final_typing_env : (f, _) Traverse.With_types.t =
    match types_rewrite_context, final_typing_env, module_symbol with
    | Without_types, Without_types, Without_types -> Without_types
    | ( With_types types_rewrite_context,
        With_types final_typing_env,
        With_types unit_symbol ) ->
      With_types
        (Types_rewriter.rewrite_typing_env types_rewrite_context ~unit_symbol
           final_typing_env)
  in
  { analysis_scope;
    has_usage = Datalog.get_table PTA.Relations.has_usage_tbl db;
    has_source = Datalog.get_table PTA.Relations.has_source_tbl db;
    field_of_constructor_is_used =
      Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl db;
    directly_called;
    known_masks;
    unknown_masks;
    unboxed_fields = unboxing.unboxed_fields;
    changed_representation = unboxing.changed_representation;
    code_changes;
    slot_offsets;
    types_rewrite_context;
    final_typing_env
  }

let solve_single (problem : _ Traverse.Problem.t) =
  let { Traverse.Problem.deps;
        delayed_deps;
        code_deps;
        applications;
        all_sets_of_closures;
        final_typing_env;
        module_symbol;
        free_names;
        toplevel_return
      } =
    problem
  in
  Global_flow_graph.add_any_usage deps toplevel_return;
  Traverse_acc.resolve_delayed_deps deps ~analysis_scope:Current_unit ~code_deps
    delayed_deps;
  solve0 deps ~analysis_scope:Current_unit ~code_deps ~applications ~free_names
    ~all_sets_of_closures ~final_typing_env ~module_symbol

let solve_whole_program
    (problems : (Compilation_unit.t * [`Without_types] Traverse.Problem.t) list)
    =
  let deps =
    match problems with
    | [] -> Global_flow_graph.create ()
    | (_, first) :: rest ->
      List.fold_left
        (fun deps (_, (problem : _ Traverse.Problem.t)) ->
          Global_flow_graph.union deps problem.deps)
        first.deps rest
  in
  let code_deps =
    List.fold_left
      (fun code_deps (_, (problem : _ Traverse.Problem.t)) ->
        Code_id.Map.disjoint_union code_deps problem.code_deps)
      Code_id.Map.empty problems
  in
  let applications =
    List.fold_left
      (fun applications (_, (problem : _ Traverse.Problem.t)) ->
        Traverse_acc.Applications.union applications problem.applications)
      Traverse_acc.Applications.empty problems
  in
  let free_names =
    List.fold_left
      (fun free_names (_, (problem : _ Traverse.Problem.t)) ->
        Name_occurrences.union free_names problem.free_names)
      Name_occurrences.empty problems
  in
  let analysis_scope =
    Analysis_scope.Lto_participants
      (List.fold_left
         (fun scope (compilation_unit, _) ->
           Compilation_unit.Set.add compilation_unit scope)
         Compilation_unit.Set.empty problems)
  in
  List.iter
    (fun (_, (problem : _ Traverse.Problem.t)) ->
      Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps
        problem.delayed_deps)
    problems;
  solve0 deps ~analysis_scope ~code_deps ~applications ~free_names
    ~all_sets_of_closures:Without_types ~final_typing_env:Without_types
    ~module_symbol:Without_types

let get_unboxed_fields (solution : _ Solution.t) cn =
  Code_id_or_name.Map.find_opt cn solution.unboxed_fields

let get_changed_representation (solution : _ Solution.t) cn =
  Option.map fst
    (Code_id_or_name.Map.find_opt cn solution.changed_representation)

let has_use (solution : _ Solution.t) v =
  Code_id_or_name.Map.mem v solution.has_usage

let field_used (solution : _ Solution.t) v f =
  match
    Code_id_or_name.Map.find_opt v solution.field_of_constructor_is_used
  with
  | None -> false
  | Some fields -> Field.Map.mem f fields

let find_answer map callee query =
  match Code_id_or_name.Map.find_opt callee map with
  | Some answer -> answer
  | None ->
    Misc.fatal_errorf "Analysis: no %s request for callee %a" query
      Code_id_or_name.print callee

let code_id_actually_directly_called (solution : _ Solution.t) closure =
  find_answer solution.directly_called
    (Code_id_or_name.name closure)
    "direct-call targets"

let rec apply_mask callee query mask args =
  match args, mask with
  | [], _ -> []
  | arg :: args, keep :: mask ->
    (arg, keep) :: apply_mask callee query mask args
  | _ :: _, [] ->
    Misc.fatal_errorf "Analysis: insufficient %s argument width for callee %a"
      query Code_id_or_name.print callee

let arguments_used_by_known_arity_call (solution : _ Solution.t) callee args =
  let mask = find_answer solution.known_masks callee "known-arity" in
  apply_mask callee "known-arity" mask args

let arguments_used_by_unknown_arity_call (solution : _ Solution.t) callee args =
  let masks = find_answer solution.unknown_masks callee "unknown-arity" in
  let rec apply_groups masks args =
    match args, masks with
    | [], _ -> []
    | args :: rest, mask :: masks ->
      apply_mask callee "unknown-arity" mask args :: apply_groups masks rest
    | _ :: _, [] ->
      Misc.fatal_errorf
        "Analysis: insufficient unknown-arity argument groups for callee %a"
        Code_id_or_name.print callee
  in
  apply_groups masks args

let has_source (solution : _ Solution.t) v =
  Code_id_or_name.Map.mem v solution.has_source

let get_calling_convention_change (solution : _ Solution.t) code_id =
  Unboxing_analysis.get_calling_convention_change solution.code_changes
    ~analysis_scope:solution.analysis_scope code_id

let is_changing_calling_convention (solution : _ Solution.t) code_id =
  Unboxing_analysis.is_changing_calling_convention solution.code_changes
    ~analysis_scope:solution.analysis_scope code_id

let find_code_metadata (solution : _ Solution.t) code_id =
  Unboxing_analysis.find_code_metadata solution.code_changes
    ~analysis_scope:solution.analysis_scope code_id

let slot_offsets (solution : _ Solution.t) = solution.slot_offsets

let rewrite_kind_with_subkind (solution : _ Solution.t) =
  rewrite_kind_with_subkind solution.types_rewrite_context

let final_typing_env (solution : _ Solution.t) = solution.final_typing_env
