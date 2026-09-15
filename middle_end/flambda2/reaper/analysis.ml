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

(* We use unit maps instead of sets, because it allows reuse of the tables
   stored in the Datalog database without copying. *)
type answers =
  { has_usage : unit Code_id_or_name.Map.t;
    has_source : unit Code_id_or_name.Map.t;
    field_of_constructor_is_used : unit Field.Map.t Code_id_or_name.Map.t;
    directly_called : Code_id.Set.t Or_unknown.t Code_id_or_name.Map.t;
    known_masks : PTA.keep_or_delete list Code_id_or_name.Map.t;
    unknown_masks : PTA.keep_or_delete list list Code_id_or_name.Map.t;
    unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
    changed_representation :
      (UA.changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

type data =
  { answers : answers;
    code_changes : UA.code_changes;
    slot_offsets : Exported_offsets.t
  }

(* A whole-program solution read back from a file is sharded: the data about an
   identifier or slot comes from the data of its compilation unit. *)
type store =
  | Single of data
  | Sharded of (Compilation_unit.t -> data)

type 'f solution =
  { analysis_scope : Analysis_scope.t;
    store : store;
    slot_offsets : ('f, Slot_offsets.result) Traverse.With_types.t;
    types_rewrite_context :
      ('f, Types_rewriter.rewrite_context) Traverse.With_types.t;
    final_typing_env : ('f, typing_env) Traverse.With_types.t
  }

let fixpoint0 (graph : Global_flow_graph.graph) ~analysis_scope =
  let datalog = Global_flow_graph.to_datalog graph in
  let with_provenance = Flambda_features.debug_reaper "prov" in
  let stats = Datalog.Schedule.create_stats ~with_provenance datalog in
  let db = PTA.perform_analysis datalog ~stats ~analysis_scope in
  let (unboxing : UA.result) = UA.perform_analysis db ~stats ~analysis_scope in
  if with_provenance || Flambda_features.debug_reaper "stats"
  then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
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

(* [deps] must have its delayed dependencies resolved. Returns the solution and
   the data it stores. *)
let solve0 (type f) deps ~analysis_scope ~code_deps ~applications ~free_names
    ~(all_sets_of_closures : (f, _) Traverse.With_types.t)
    ~(final_typing_env : (f, typing_env) Traverse.With_types.t)
    ~(module_symbol : (f, Symbol.t) Traverse.With_types.t) : f solution * data =
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
  let data =
    { answers =
        { has_usage = Datalog.get_table PTA.Relations.has_usage_tbl db;
          has_source = Datalog.get_table PTA.Relations.has_source_tbl db;
          field_of_constructor_is_used =
            Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl db;
          directly_called;
          known_masks;
          unknown_masks;
          unboxed_fields = unboxing.unboxed_fields;
          changed_representation = unboxing.changed_representation
        };
      code_changes;
      slot_offsets = slot_offsets.Slot_offsets.exported_offsets
    }
  in
  ( { analysis_scope;
      store = Single data;
      slot_offsets =
        Traverse.With_types.map
          (fun (_ : typing_env) -> slot_offsets)
          final_typing_env;
      types_rewrite_context;
      final_typing_env
    },
    data )

let solve (problem : _ Traverse.Problem.t) ~(analysis_scope : Analysis_scope.t)
    =
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
  (match analysis_scope with
  | Current_unit -> Global_flow_graph.add_any_usage deps toplevel_return
  | Lto_participants _ -> ());
  Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps delayed_deps;
  let solution, (_ : data) =
    solve0 deps ~analysis_scope ~code_deps ~applications ~free_names
      ~all_sets_of_closures ~final_typing_env ~module_symbol
  in
  solution

let solve_whole_program (problems : [`Without_types] Traverse.Problem.t list)
    ~analysis_scope =
  let deps =
    match problems with
    | [] -> Global_flow_graph.create ()
    | first :: rest ->
      List.fold_left
        (fun deps (problem : _ Traverse.Problem.t) ->
          Global_flow_graph.union deps problem.deps)
        first.deps rest
  in
  let code_deps =
    List.fold_left
      (fun code_deps (problem : _ Traverse.Problem.t) ->
        Code_id.Map.disjoint_union code_deps problem.code_deps)
      Code_id.Map.empty problems
  in
  let applications =
    List.fold_left
      (fun applications (problem : _ Traverse.Problem.t) ->
        Traverse_acc.Applications.union applications problem.applications)
      Traverse_acc.Applications.empty problems
  in
  let free_names =
    List.fold_left
      (fun free_names (problem : _ Traverse.Problem.t) ->
        Name_occurrences.union free_names problem.free_names)
      Name_occurrences.empty problems
  in
  List.iter
    (fun (problem : _ Traverse.Problem.t) ->
      Traverse_acc.resolve_delayed_deps deps ~analysis_scope ~code_deps
        problem.delayed_deps)
    problems;
  let (_ : [`Without_types] solution), data =
    solve0 deps ~analysis_scope ~code_deps ~applications ~free_names
      ~all_sets_of_closures:Without_types ~final_typing_env:Without_types
      ~module_symbol:Without_types
  in
  data

let sharded ~analysis_scope data_for_unit : [`Without_types] solution =
  { analysis_scope;
    store = Sharded data_for_unit;
    slot_offsets = Without_types;
    types_rewrite_context = Without_types;
    final_typing_env = Without_types
  }

let data_for_unit solution compilation_unit =
  match solution.store with
  | Single data -> data
  | Sharded data_for_unit -> data_for_unit compilation_unit

let answers_for solution id =
  (data_for_unit solution (Code_id_or_name.compilation_unit id)).answers

let code_changes_for solution code_id =
  (data_for_unit solution (Code_id.get_compilation_unit code_id)).code_changes

let get_unboxed_fields solution cn =
  Code_id_or_name.Map.find_opt cn (answers_for solution cn).unboxed_fields

let get_changed_representation solution cn =
  Option.map fst
    (Code_id_or_name.Map.find_opt cn
       (answers_for solution cn).changed_representation)

let has_use solution v =
  Code_id_or_name.Map.mem v (answers_for solution v).has_usage

let field_used solution v f =
  match
    Code_id_or_name.Map.find_opt v
      (answers_for solution v).field_of_constructor_is_used
  with
  | None -> false
  | Some fields -> Field.Map.mem f fields

let find_answer map callee query =
  match Code_id_or_name.Map.find_opt callee map with
  | Some answer -> answer
  | None ->
    Misc.fatal_errorf "Analysis: no %s request for callee %a" query
      Code_id_or_name.print callee

let code_id_actually_directly_called solution closure =
  let callee = Code_id_or_name.name closure in
  find_answer (answers_for solution callee).directly_called callee
    "direct-call targets"

let rec apply_mask callee query mask args =
  match args, mask with
  | [], _ -> []
  | arg :: args, keep :: mask ->
    (arg, keep) :: apply_mask callee query mask args
  | _ :: _, [] ->
    Misc.fatal_errorf "Analysis: insufficient %s argument width for callee %a"
      query Code_id_or_name.print callee

let arguments_used_by_known_arity_call solution callee args =
  let mask =
    find_answer (answers_for solution callee).known_masks callee "known-arity"
  in
  apply_mask callee "known-arity" mask args

let arguments_used_by_unknown_arity_call solution callee args =
  let masks =
    find_answer (answers_for solution callee).unknown_masks callee
      "unknown-arity"
  in
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

let has_source solution v =
  Code_id_or_name.Map.mem v (answers_for solution v).has_source

let get_calling_convention_change solution code_id =
  Unboxing_analysis.get_calling_convention_change
    (code_changes_for solution code_id)
    ~analysis_scope:solution.analysis_scope code_id

let is_changing_calling_convention solution code_id =
  Unboxing_analysis.is_changing_calling_convention
    (code_changes_for solution code_id)
    ~analysis_scope:solution.analysis_scope code_id

let find_code_metadata solution code_id =
  Unboxing_analysis.find_code_metadata
    (code_changes_for solution code_id)
    ~analysis_scope:solution.analysis_scope code_id

let slot_offsets solution = solution.slot_offsets

let solved_offsets solution compilation_unit =
  (data_for_unit solution compilation_unit : data).slot_offsets

let offsets_for_free_names solution free_names =
  let offsets =
    Function_slot.Set.fold
      (fun function_slot offsets ->
        let solved =
          solved_offsets solution
            (Function_slot.get_compilation_unit function_slot)
        in
        match Exported_offsets.function_slot_offset solved function_slot with
        | Some info ->
          Exported_offsets.add_function_slot_offset offsets function_slot info
        | None ->
          Misc.fatal_errorf "Reaper: no solved offset for function slot %a"
            Function_slot.print function_slot)
      (Name_occurrences.all_function_slots_at_normal_mode free_names)
      Exported_offsets.empty
  in
  Value_slot.Set.fold
    (fun value_slot offsets ->
      let solved =
        solved_offsets solution (Value_slot.get_compilation_unit value_slot)
      in
      match Exported_offsets.value_slot_offset solved value_slot with
      | Some info ->
        Exported_offsets.add_value_slot_offset offsets value_slot info
      | None ->
        Misc.fatal_errorf "Reaper: no solved offset for value slot %a"
          Value_slot.print value_slot)
    (Name_occurrences.all_value_slots_at_normal_mode free_names)
    offsets

let rewrite_kind_with_subkind solution =
  rewrite_kind_with_subkind solution.types_rewrite_context

let final_typing_env solution = solution.final_typing_env

module Rebuild_data = struct
  type t = data =
    { answers : answers;
      code_changes : UA.code_changes;
      slot_offsets : Exported_offsets.t
    }

  let empty_answers =
    { has_usage = Code_id_or_name.Map.empty;
      has_source = Code_id_or_name.Map.empty;
      field_of_constructor_is_used = Code_id_or_name.Map.empty;
      directly_called = Code_id_or_name.Map.empty;
      known_masks = Code_id_or_name.Map.empty;
      unknown_masks = Code_id_or_name.Map.empty;
      unboxed_fields = Code_id_or_name.Map.empty;
      changed_representation = Code_id_or_name.Map.empty
    }

  let empty =
    { answers = empty_answers;
      code_changes = UA.empty_code_changes;
      slot_offsets = Exported_offsets.empty
    }

  let add_keys map ids =
    Code_id_or_name.Map.fold
      (fun id _ ids -> Ids_for_export.add_code_id_or_name ids id)
      map ids

  let ids_for_export { answers; code_changes; slot_offsets = _ } =
    (* Slots are not hashconsed, so the offsets need no renaming. *)
    let ids =
      Ids_for_export.union_list
        [ Schemas.N.ids_for_export answers.has_usage;
          Schemas.N.ids_for_export answers.has_source;
          Schemas.NF.ids_for_export answers.field_of_constructor_is_used ]
    in
    let ids = add_keys answers.known_masks ids in
    let ids = add_keys answers.unknown_masks ids in
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
        answers.directly_called ids
    in
    let ids = UA.unboxed_fields_ids_for_export answers.unboxed_fields ids in
    let ids =
      UA.changed_representation_ids_for_export answers.changed_representation
        ids
    in
    UA.code_changes_ids_for_export code_changes ids

  let rename_map map renaming ~f =
    Code_id_or_name.Map.fold
      (fun id value map ->
        Code_id_or_name.Map.add
          (Renaming.apply_code_id_or_name renaming id)
          (f value) map)
      map Code_id_or_name.Map.empty

  let apply_renaming { answers; code_changes; slot_offsets } renaming =
    let answers =
      { has_usage = Schemas.N.apply_renaming answers.has_usage renaming;
        has_source = Schemas.N.apply_renaming answers.has_source renaming;
        field_of_constructor_is_used =
          Schemas.NF.apply_renaming answers.field_of_constructor_is_used
            renaming;
        directly_called =
          rename_map answers.directly_called renaming ~f:(fun targets ->
              Or_unknown.map targets ~f:(fun targets ->
                  Code_id.Set.fold
                    (fun code_id targets ->
                      Code_id.Set.add
                        (Renaming.apply_code_id renaming code_id)
                        targets)
                    targets Code_id.Set.empty));
        known_masks = rename_map answers.known_masks renaming ~f:Fun.id;
        unknown_masks = rename_map answers.unknown_masks renaming ~f:Fun.id;
        unboxed_fields =
          UA.unboxed_fields_apply_renaming answers.unboxed_fields renaming;
        changed_representation =
          UA.changed_representation_apply_renaming
            answers.changed_representation renaming
      }
    in
    { answers;
      code_changes = UA.code_changes_apply_renaming code_changes renaming;
      slot_offsets
    }

  let partition_answers answers =
    let distribute map ~get ~set partitions =
      Code_id_or_name.Map.fold
        (fun id value partitions ->
          Compilation_unit.Map.update
            (Code_id_or_name.compilation_unit id)
            (fun part ->
              let part = Option.value part ~default:empty_answers in
              Some (set part (Code_id_or_name.Map.add id value (get part))))
            partitions)
        map partitions
    in
    Compilation_unit.Map.empty
    |> distribute answers.has_usage
         ~get:(fun part -> part.has_usage)
         ~set:(fun part has_usage -> { part with has_usage })
    |> distribute answers.has_source
         ~get:(fun part -> part.has_source)
         ~set:(fun part has_source -> { part with has_source })
    |> distribute answers.field_of_constructor_is_used
         ~get:(fun part -> part.field_of_constructor_is_used)
         ~set:(fun part field_of_constructor_is_used ->
           { part with field_of_constructor_is_used })
    |> distribute answers.directly_called
         ~get:(fun part -> part.directly_called)
         ~set:(fun part directly_called -> { part with directly_called })
    |> distribute answers.known_masks
         ~get:(fun part -> part.known_masks)
         ~set:(fun part known_masks -> { part with known_masks })
    |> distribute answers.unknown_masks
         ~get:(fun part -> part.unknown_masks)
         ~set:(fun part unknown_masks -> { part with unknown_masks })
    |> distribute answers.unboxed_fields
         ~get:(fun part -> part.unboxed_fields)
         ~set:(fun part unboxed_fields -> { part with unboxed_fields })
    |> distribute answers.changed_representation
         ~get:(fun part -> part.changed_representation)
         ~set:(fun part changed_representation ->
           { part with changed_representation })

  let partition_by_compilation_unit { answers; code_changes; slot_offsets } =
    let add_parts by_unit ~set parts =
      Compilation_unit.Map.fold
        (fun compilation_unit value parts ->
          Compilation_unit.Map.update compilation_unit
            (fun part -> Some (set (Option.value part ~default:empty) value))
            parts)
        by_unit parts
    in
    Compilation_unit.Map.empty
    |> add_parts (partition_answers answers) ~set:(fun part answers ->
        { part with answers })
    |> add_parts (UA.partition_code_changes_by_compilation_unit code_changes)
         ~set:(fun part code_changes -> { part with code_changes })
    |> add_parts (Exported_offsets.partition_by_compilation_unit slot_offsets)
         ~set:(fun part slot_offsets -> { part with slot_offsets })
end
