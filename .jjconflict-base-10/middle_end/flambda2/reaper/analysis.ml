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
module Serialisation = Datalog_helpers.Serialisation

(* We use unit maps instead of sets, because it allows reuse of the tables
   stored in the Datalog database without copying. *)
type result =
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

let answer_call_queries db (applications : Traverse_acc.Applications.t) result =
  Code_id_or_name.Map.fold
    (fun callee ({ known; unknown } : Traverse_acc.Applications.bounds) result
       ->
      let result =
        match known with
        | None -> result
        | Some width ->
          let name =
            Code_id_or_name.pattern_match' callee
              ~name:(fun name -> name)
              ~code_id:(fun _ ->
                Misc.fatal_errorf "Analysis: expected a named callee, found %a"
                  Code_id_or_name.print callee)
          in
          let directly_called = PTA.code_id_actually_directly_called db name in
          let mask = PTA.arguments_used_by_known_arity_call db callee width in
          { result with
            directly_called =
              Code_id_or_name.Map.add callee directly_called
                result.directly_called;
            known_masks = Code_id_or_name.Map.add callee mask result.known_masks
          }
      in
      match unknown with
      | None -> result
      | Some widths ->
        let masks = PTA.arguments_used_by_unknown_arity_call db callee widths in
        { result with
          unknown_masks =
            Code_id_or_name.Map.add callee masks result.unknown_masks
        })
    applications result

let fixpoint (graph : Global_flow_graph.graph) ~applications ~analysis_scope =
  let datalog = Global_flow_graph.to_datalog graph in
  let with_provenance = Flambda_features.debug_reaper "prov" in
  let stats = Datalog.Schedule.create_stats ~with_provenance datalog in
  let db = PTA.perform_analysis datalog ~stats ~analysis_scope in
  let (unboxing : UA.result) = UA.perform_analysis db ~stats ~analysis_scope in
  if with_provenance || Flambda_features.debug_reaper "stats"
  then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Flambda_features.debug_reaper "db"
  then Format.eprintf "%a@." Datalog.print db;
  let result =
    { has_usage = Datalog.get_table PTA.Relations.has_usage_tbl db;
      has_source = Datalog.get_table PTA.Relations.has_source_tbl db;
      field_of_constructor_is_used =
        Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl db;
      directly_called = Code_id_or_name.Map.empty;
      known_masks = Code_id_or_name.Map.empty;
      unknown_masks = Code_id_or_name.Map.empty;
      unboxed_fields = unboxing.unboxed_fields;
      changed_representation = unboxing.changed_representation
    }
  in
  unboxing, answer_call_queries db applications result

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Option.map fst (Code_id_or_name.Map.find_opt cn uses.changed_representation)

let has_use uses v = Code_id_or_name.Map.mem v uses.has_usage

let field_used uses v f =
  match Code_id_or_name.Map.find_opt v uses.field_of_constructor_is_used with
  | None -> false
  | Some fields -> Field.Map.mem f fields

let find_answer map callee query =
  match Code_id_or_name.Map.find_opt callee map with
  | Some answer -> answer
  | None ->
    Misc.fatal_errorf "Analysis: no %s request for callee %a" query
      Code_id_or_name.print callee

let code_id_actually_directly_called uses closure =
  find_answer uses.directly_called
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

let arguments_used_by_known_arity_call uses callee args =
  let mask = find_answer uses.known_masks callee "known-arity" in
  apply_mask callee "known-arity" mask args

let arguments_used_by_unknown_arity_call uses callee args =
  let masks = find_answer uses.unknown_masks callee "unknown-arity" in
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

let has_source uses v = Code_id_or_name.Map.mem v uses.has_source

let empty =
  { has_usage = Code_id_or_name.Map.empty;
    has_source = Code_id_or_name.Map.empty;
    field_of_constructor_is_used = Code_id_or_name.Map.empty;
    directly_called = Code_id_or_name.Map.empty;
    known_masks = Code_id_or_name.Map.empty;
    unknown_masks = Code_id_or_name.Map.empty;
    unboxed_fields = Code_id_or_name.Map.empty;
    changed_representation = Code_id_or_name.Map.empty
  }

let add_keys map ids =
  Code_id_or_name.Map.fold
    (fun id _ ids -> Ids_for_export.add_code_id_or_name ids id)
    map ids

let ids_for_export t =
  let ids = Serialisation.N.add_ids t.has_usage Ids_for_export.empty in
  let ids = Serialisation.N.add_ids t.has_source ids in
  let ids = Serialisation.Nf.add_ids t.field_of_constructor_is_used ids in
  let ids = add_keys t.known_masks ids in
  let ids = add_keys t.unknown_masks ids in
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
      t.directly_called ids
  in
  let ids = UA.unboxed_fields_ids_for_export t.unboxed_fields ids in
  UA.changed_representation_ids_for_export t.changed_representation ids

let fields_for_export t =
  let fields =
    Serialisation.Nf.add_fields t.field_of_constructor_is_used Field.Set.empty
  in
  let fields = UA.unboxed_fields_fields_for_export t.unboxed_fields fields in
  UA.changed_representation_fields_for_export t.changed_representation fields

let rename_map map renaming ~f =
  Code_id_or_name.Map.fold
    (fun id value map ->
      Code_id_or_name.Map.add
        (Renaming.apply_code_id_or_name renaming id)
        (f value) map)
    map Code_id_or_name.Map.empty

let apply_renaming t renaming ~rename_field =
  let rename_id = Renaming.apply_code_id_or_name renaming in
  { has_usage = Serialisation.N.rename t.has_usage ~rename_id;
    has_source = Serialisation.N.rename t.has_source ~rename_id;
    field_of_constructor_is_used =
      Serialisation.Nf.rename t.field_of_constructor_is_used ~rename_id
        ~rename_field;
    directly_called =
      rename_map t.directly_called renaming ~f:(fun targets ->
          Or_unknown.map targets ~f:(fun targets ->
              Code_id.Set.fold
                (fun code_id targets ->
                  Code_id.Set.add
                    (Renaming.apply_code_id renaming code_id)
                    targets)
                targets Code_id.Set.empty));
    known_masks = rename_map t.known_masks renaming ~f:(fun mask -> mask);
    unknown_masks = rename_map t.unknown_masks renaming ~f:(fun masks -> masks);
    unboxed_fields =
      UA.unboxed_fields_apply_renaming t.unboxed_fields renaming ~rename_field;
    changed_representation =
      UA.changed_representation_apply_renaming t.changed_representation renaming
        ~rename_field
  }

let partition_by_compilation_unit t =
  let distribute map ~get ~set partitions =
    Code_id_or_name.Map.fold
      (fun id value partitions ->
        Compilation_unit.Map.update
          (Code_id_or_name.compilation_unit id)
          (fun part ->
            let part = Option.value part ~default:empty in
            Some (set part (Code_id_or_name.Map.add id value (get part))))
          partitions)
      map partitions
  in
  Compilation_unit.Map.empty
  |> distribute t.has_usage
       ~get:(fun part -> part.has_usage)
       ~set:(fun part has_usage -> { part with has_usage })
  |> distribute t.has_source
       ~get:(fun part -> part.has_source)
       ~set:(fun part has_source -> { part with has_source })
  |> distribute t.field_of_constructor_is_used
       ~get:(fun part -> part.field_of_constructor_is_used)
       ~set:(fun part field_of_constructor_is_used ->
         { part with field_of_constructor_is_used })
  |> distribute t.directly_called
       ~get:(fun part -> part.directly_called)
       ~set:(fun part directly_called -> { part with directly_called })
  |> distribute t.known_masks
       ~get:(fun part -> part.known_masks)
       ~set:(fun part known_masks -> { part with known_masks })
  |> distribute t.unknown_masks
       ~get:(fun part -> part.unknown_masks)
       ~set:(fun part unknown_masks -> { part with unknown_masks })
  |> distribute t.unboxed_fields
       ~get:(fun part -> part.unboxed_fields)
       ~set:(fun part unboxed_fields -> { part with unboxed_fields })
  |> distribute t.changed_representation
       ~get:(fun part -> part.changed_representation)
       ~set:(fun part changed_representation ->
         { part with changed_representation })
