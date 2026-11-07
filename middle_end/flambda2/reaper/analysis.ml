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

(* The identifiers that the tables of a solution have data about. *)
type coverage =
  | All_identifiers
  | Identifiers_of_unit of
      { compilation_unit : Compilation_unit.t;
        foreign_symbols : Symbol.Set.t
      }

module Solution = struct
  (* We use unit maps instead of sets, because it allows reuse of the tables
     stored in the Datalog database without copying. *)
  type 'f t =
    { analysis_scope : Analysis_scope.t;
      coverage : coverage;
      exported_symbols : Symbol.Set.t;
      has_usage : Schemas.N.t;
      has_source : Schemas.N.t;
      field_of_constructor_is_used : Schemas.NF.t;
      directly_called : Code_id.Set.t Or_unknown.t Code_id_or_name.Map.t;
      known_masks : PTA.keep_or_delete list Code_id_or_name.Map.t;
      unknown_masks : PTA.keep_or_delete list list Code_id_or_name.Map.t;
      unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
      changed_representation :
        (UA.Changed_representation.t * Code_id_or_name.t) Code_id_or_name.Map.t;
      code_changes : UA.Code_changes.t;
      slot_offsets : Slot_offsets.result;
      types_rewrite_context :
        ('f, Types_rewriter.rewrite_context) Traverse.With_types.t;
      final_typing_env : ('f, typing_env) Traverse.With_types.t
    }

  let ids_for_export
      { analysis_scope = _;
        coverage;
        exported_symbols;
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
    let keys_of map =
      Code_id_or_name.Map.fold
        (fun id _ ids -> Ids_for_export.add_code_id_or_name ids id)
        map Ids_for_export.empty
    in
    (* Slots are not hashconsed, so the offsets need no renaming. *)
    let ids =
      Ids_for_export.union_list
        [ Schemas.N.ids_for_export has_usage;
          Schemas.N.ids_for_export has_source;
          Schemas.NF.ids_for_export field_of_constructor_is_used;
          keys_of known_masks;
          keys_of unknown_masks ]
    in
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
    let ids =
      Code_id_or_name.Map.fold
        (fun id fields ids ->
          Ids_for_export.union
            (Ids_for_export.add_code_id_or_name ids id)
            (UA.Unboxed_fields.ids_for_export Ids_for_export.singleton_variable
               fields))
        unboxed_fields ids
    in
    let ids =
      Code_id_or_name.Map.fold
        (fun id (repr, allocation_point) ids ->
          let ids = Ids_for_export.add_code_id_or_name ids id in
          let ids = Ids_for_export.add_code_id_or_name ids allocation_point in
          Ids_for_export.union ids
            (UA.Changed_representation.ids_for_export repr))
        changed_representation ids
    in
    let add_symbols symbols ids =
      Symbol.Set.fold
        (fun symbol ids -> Ids_for_export.add_symbol ids symbol)
        symbols ids
    in
    let ids =
      match coverage with
      | All_identifiers -> ids
      | Identifiers_of_unit { compilation_unit = _; foreign_symbols } ->
        add_symbols foreign_symbols ids
    in
    let ids = add_symbols exported_symbols ids in
    Ids_for_export.union ids (UA.Code_changes.ids_for_export code_changes)

  let apply_renaming
      { analysis_scope;
        coverage;
        exported_symbols;
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
    let coverage =
      match coverage with
      | All_identifiers -> All_identifiers
      | Identifiers_of_unit { compilation_unit; foreign_symbols } ->
        Identifiers_of_unit
          { compilation_unit;
            foreign_symbols = Renaming.apply_symbol_set renaming foreign_symbols
          }
    in
    { analysis_scope;
      coverage;
      exported_symbols = Renaming.apply_symbol_set renaming exported_symbols;
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
      unboxed_fields =
        rename_map unboxed_fields renaming ~f:(fun fields ->
            UA.Unboxed_fields.apply_renaming fields renaming
              (fun var renaming -> Renaming.apply_variable renaming var));
      changed_representation =
        rename_map changed_representation renaming
          ~f:(fun (repr, allocation_point) ->
            ( UA.Changed_representation.apply_renaming repr renaming,
              Renaming.apply_code_id_or_name renaming allocation_point ));
      code_changes = UA.Code_changes.apply_renaming code_changes renaming;
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

(* [deps] must have its delayed dependencies resolved. The database is returned
   to answer further queries. *)
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
    coverage = All_identifiers;
    exported_symbols = Symbol.Set.empty;
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

(* The offsets in [solved] of the given slots. The slots without offsets are
   skipped. *)
let offsets_of_slots solved ~function_slots ~value_slots =
  let offsets =
    Function_slot.Set.fold
      (fun function_slot offsets ->
        match Exported_offsets.function_slot_offset solved function_slot with
        | Some info ->
          Exported_offsets.add_function_slot_offset offsets function_slot info
        | None -> offsets)
      function_slots Exported_offsets.empty
  in
  Value_slot.Set.fold
    (fun value_slot offsets ->
      match Exported_offsets.value_slot_offset solved value_slot with
      | Some info ->
        Exported_offsets.add_value_slot_offset offsets value_slot info
      | None -> offsets)
    value_slots offsets

(* A whole-program solution is restricted to what the rebuild of each unit
   needs. The rebuild only queries the tables about the identifiers of its unit
   and about the symbols of other units occurring in its code, which the
   traversal records as imported symbols.

   [restrictor map] returns a function restricting [map] to the identifiers of a
   compilation unit and to the given symbols of other units. The table is split
   by compilation unit once, so that restricting it for every unit takes linear
   time. *)
let restrictor map =
  let parts =
    Code_id_or_name.Map.fold
      (fun id value parts ->
        Compilation_unit.Map.update
          (Code_id_or_name.compilation_unit id)
          (fun part ->
            Some
              (Code_id_or_name.Map.add id value
                 (Option.value part ~default:Code_id_or_name.Map.empty)))
          parts)
      map Compilation_unit.Map.empty
  in
  fun ~compilation_unit ~foreign_symbols ->
    Symbol.Set.fold
      (fun symbol restricted ->
        let id = Code_id_or_name.symbol symbol in
        match Code_id_or_name.Map.find_opt id map with
        | None -> restricted
        | Some value -> Code_id_or_name.Map.add id value restricted)
      foreign_symbols
      (Option.value
         (Compilation_unit.Map.find_opt compilation_unit parts)
         ~default:Code_id_or_name.Map.empty)

(* The code ids whose code changes the rebuild of a unit reads: those of its
   code, those it calls directly or puts in sets of closures, and those that its
   calls may be turned into direct calls to. *)
let code_ids_used_by_rebuild (problem : _ Traverse.Problem.t) ~directly_called =
  Code_id_or_name.Map.fold
    (fun _callee (targets : _ Or_unknown.t) code_ids ->
      match targets with
      | Unknown -> code_ids
      | Known targets -> Code_id.Set.union targets code_ids)
    directly_called
    (Code_id.Set.union
       (Code_id.Map.keys problem.code_deps)
       (Traverse_acc.code_ids_of_delayed_deps problem.delayed_deps))

(* The slots that a rebuilt unit may contain: those of its code before the
   rebuild, and those introduced by the changes of representation it reads. *)
let slots_used_by_rebuild (problem : _ Traverse.Problem.t)
    ~changed_representation =
  Code_id_or_name.Map.fold
    (fun _ ((repr : UA.Changed_representation.t), _)
         (function_slots, value_slots) ->
      match repr with
      | Block_representation _ -> function_slots, value_slots
      | Closure_representation (fields, function_slot_rewrites, _) ->
        ( Function_slot.Map.fold
            (fun _ function_slot function_slots ->
              Function_slot.Set.add function_slot function_slots)
            function_slot_rewrites function_slots,
          UA.Unboxed_fields.fold_with_kind
            (fun _kind value_slot value_slots ->
              Value_slot.Set.add value_slot value_slots)
            fields value_slots ))
    changed_representation
    ( Name_occurrences.all_function_slots_at_normal_mode problem.free_names,
      Name_occurrences.all_value_slots_at_normal_mode problem.free_names )

let restrict_to_units (solution : [`Without_types] Solution.t) problems
    ~exported_symbols =
  let restrict_has_usage = restrictor solution.has_usage in
  let restrict_has_source = restrictor solution.has_source in
  let restrict_field_of_constructor_is_used =
    restrictor solution.field_of_constructor_is_used
  in
  let restrict_directly_called = restrictor solution.directly_called in
  let restrict_known_masks = restrictor solution.known_masks in
  let restrict_unknown_masks = restrictor solution.unknown_masks in
  let restrict_unboxed_fields = restrictor solution.unboxed_fields in
  let restrict_changed_representation =
    restrictor solution.changed_representation
  in
  let { Slot_offsets.used_value_slots; exported_offsets } =
    solution.slot_offsets
  in
  List.map
    (fun (compilation_unit, (problem : _ Traverse.Problem.t)) ->
      let foreign_symbols =
        Traverse_acc.imported_symbols problem.delayed_deps
      in
      let directly_called =
        restrict_directly_called ~compilation_unit ~foreign_symbols
      in
      let changed_representation =
        restrict_changed_representation ~compilation_unit ~foreign_symbols
      in
      let function_slots, value_slots =
        slots_used_by_rebuild problem ~changed_representation
      in
      let restricted : [`Without_types] Solution.t =
        { analysis_scope = solution.analysis_scope;
          coverage = Identifiers_of_unit { compilation_unit; foreign_symbols };
          exported_symbols =
            Option.value
              (Compilation_unit.Map.find_opt compilation_unit exported_symbols)
              ~default:Symbol.Set.empty;
          has_usage = restrict_has_usage ~compilation_unit ~foreign_symbols;
          has_source = restrict_has_source ~compilation_unit ~foreign_symbols;
          field_of_constructor_is_used =
            restrict_field_of_constructor_is_used ~compilation_unit
              ~foreign_symbols;
          directly_called;
          known_masks = restrict_known_masks ~compilation_unit ~foreign_symbols;
          unknown_masks =
            restrict_unknown_masks ~compilation_unit ~foreign_symbols;
          unboxed_fields =
            restrict_unboxed_fields ~compilation_unit ~foreign_symbols;
          changed_representation;
          code_changes =
            UA.restrict_code_changes solution.code_changes
              (code_ids_used_by_rebuild problem ~directly_called);
          slot_offsets =
            { Slot_offsets.used_value_slots =
                Value_slot.Set.inter used_value_slots value_slots;
              exported_offsets =
                offsets_of_slots exported_offsets ~function_slots ~value_slots
            };
          types_rewrite_context = Without_types;
          final_typing_env = Without_types
        }
      in
      compilation_unit, restricted)
    problems

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
  (* The symbols of each participant that other participants refer to, which
     must remain global when it is rebuilt. *)
  let exported_symbols =
    List.fold_left
      (fun exported_symbols (_, (problem : _ Traverse.Problem.t)) ->
        Symbol.Set.fold
          (fun symbol exported_symbols ->
            let compilation_unit = Symbol.compilation_unit symbol in
            if Analysis_scope.contains_unit analysis_scope compilation_unit
            then
              Compilation_unit.Map.update compilation_unit
                (fun symbols ->
                  Some
                    (Symbol.Set.add symbol
                       (Option.value symbols ~default:Symbol.Set.empty)))
                exported_symbols
            else exported_symbols)
          (Traverse_acc.imported_symbols problem.delayed_deps)
          exported_symbols)
      Compilation_unit.Map.empty problems
  in
  List.iter
    (fun (_, (problem : _ Traverse.Problem.t)) ->
      Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps
        problem.delayed_deps)
    problems;
  let solution =
    solve0 deps ~analysis_scope ~code_deps ~applications ~free_names
      ~all_sets_of_closures:Without_types ~final_typing_env:Without_types
      ~module_symbol:Without_types
  in
  restrict_to_units solution problems ~exported_symbols

(* Look up [id] in a table of [solution]. In a solution restricted to a unit,
   the absence of an identifier outside the restriction is not an answer, so
   looking it up is a fatal error. *)
let find_in_table (solution : _ Solution.t) table ~table_name id =
  match Code_id_or_name.Map.find_opt id table with
  | Some _ as data -> data
  | None -> (
    match solution.coverage with
    | All_identifiers -> None
    | Identifiers_of_unit { compilation_unit; foreign_symbols } ->
      let covered =
        Compilation_unit.equal
          (Code_id_or_name.compilation_unit id)
          compilation_unit
        || Code_id_or_name.pattern_match id
             ~symbol:(fun symbol -> Symbol.Set.mem symbol foreign_symbols)
             ~var:(fun _ -> false)
             ~code_id:(fun _ -> false)
      in
      if not covered
      then
        Misc.fatal_errorf
          "Analysis: the solution for unit %a has no %s data about %a"
          (Format_doc.compat Compilation_unit.print)
          compilation_unit table_name Code_id_or_name.print id;
      None)

let get_unboxed_fields (solution : _ Solution.t) cn =
  find_in_table solution solution.unboxed_fields ~table_name:"unboxing" cn

let get_changed_representation (solution : _ Solution.t) cn =
  Option.map fst
    (find_in_table solution solution.changed_representation
       ~table_name:"representation" cn)

let has_use (solution : _ Solution.t) v =
  Option.is_some
    (find_in_table solution solution.has_usage ~table_name:"usage" v)

let field_used (solution : _ Solution.t) v f =
  match
    find_in_table solution solution.field_of_constructor_is_used
      ~table_name:"field usage" v
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
  Option.is_some
    (find_in_table solution solution.has_source ~table_name:"source" v)

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

let exported_symbols (solution : _ Solution.t) = solution.exported_symbols
