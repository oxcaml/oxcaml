(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Flambda.Import
module UA = Unboxing_analysis

type data =
  { queries : Rebuild_queries.t;
    unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
    changed_representation :
      (UA.changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t;
    code_changes : UA.code_changes;
    slot_offsets : Exported_offsets.t
  }

let create_data ~queries ~(unboxing : UA.result) ~code_changes ~slot_offsets =
  { queries;
    unboxed_fields = unboxing.unboxed_fields;
    changed_representation = unboxing.changed_representation;
    code_changes;
    slot_offsets
  }

let empty_data =
  { queries = Rebuild_queries.empty;
    unboxed_fields = Code_id_or_name.Map.empty;
    changed_representation = Code_id_or_name.Map.empty;
    code_changes = UA.empty_code_changes;
    slot_offsets = Exported_offsets.empty
  }

let ids_for_export data =
  let ids = Rebuild_queries.ids_for_export data.queries in
  let ids = UA.unboxed_fields_ids_for_export data.unboxed_fields ids in
  let ids =
    UA.changed_representation_ids_for_export data.changed_representation ids
  in
  UA.code_changes_ids_for_export data.code_changes ids

let fields_for_export data =
  let fields = Rebuild_queries.fields_for_export data.queries in
  let fields = UA.unboxed_fields_fields_for_export data.unboxed_fields fields in
  let fields =
    UA.changed_representation_fields_for_export data.changed_representation
      fields
  in
  UA.code_changes_fields_for_export data.code_changes fields

let apply_renaming data renaming ~rename_field =
  { data with
    queries = Rebuild_queries.apply_renaming data.queries renaming ~rename_field;
    unboxed_fields =
      UA.unboxed_fields_apply_renaming data.unboxed_fields renaming
        ~rename_field;
    changed_representation =
      UA.changed_representation_apply_renaming data.changed_representation
        renaming ~rename_field;
    code_changes =
      UA.code_changes_apply_renaming data.code_changes renaming ~rename_field
  }

let partition_by_compilation_unit data =
  let add_parts parts ~f partitions =
    Compilation_unit.Map.fold
      (fun compilation_unit value partitions ->
        Compilation_unit.Map.update compilation_unit
          (fun part -> Some (f (Option.value part ~default:empty_data) value))
          partitions)
      parts partitions
  in
  let partition_map map =
    Code_id_or_name.Map.fold
      (fun id value partitions ->
        Compilation_unit.Map.update
          (Code_id_or_name.compilation_unit id)
          (fun part ->
            let part = Option.value part ~default:Code_id_or_name.Map.empty in
            Some (Code_id_or_name.Map.add id value part))
          partitions)
      map Compilation_unit.Map.empty
  in
  let partitions =
    add_parts
      (Rebuild_queries.partition_by_compilation_unit data.queries)
      ~f:(fun part queries -> { part with queries })
      Compilation_unit.Map.empty
  in
  let partitions =
    add_parts
      (partition_map data.unboxed_fields)
      ~f:(fun part unboxed_fields -> { part with unboxed_fields })
      partitions
  in
  let partitions =
    add_parts
      (partition_map data.changed_representation)
      ~f:(fun part changed_representation ->
        { part with changed_representation })
      partitions
  in
  let partitions =
    add_parts
      (UA.partition_code_changes_by_compilation_unit data.code_changes)
      ~f:(fun part code_changes -> { part with code_changes })
      partitions
  in
  add_parts
    (Exported_offsets.partition_by_compilation_unit data.slot_offsets)
    ~f:(fun part slot_offsets -> { part with slot_offsets })
    partitions

type t =
  { analysis_scope : Analysis_scope.t;
    get_unit : Compilation_unit.t -> data
  }

let create ~analysis_scope ~get_unit = { analysis_scope; get_unit }

let of_data data ~analysis_scope =
  create ~analysis_scope ~get_unit:(fun _ -> data)

let data_for_id t id = t.get_unit (Code_id_or_name.compilation_unit id)

let has_use t id = Rebuild_queries.has_use (data_for_id t id).queries id

let has_source t id = Rebuild_queries.has_source (data_for_id t id).queries id

let field_used t id field =
  Rebuild_queries.field_used (data_for_id t id).queries id field

let get_unboxed_fields t id =
  Code_id_or_name.Map.find_opt id (data_for_id t id).unboxed_fields

let get_changed_representation t id =
  Option.map fst
    (Code_id_or_name.Map.find_opt id (data_for_id t id).changed_representation)

let code_id_actually_directly_called t name =
  let data = t.get_unit (Name.compilation_unit name) in
  Rebuild_queries.code_id_actually_directly_called data.queries name

let arguments_used_by_known_arity_call t callee args =
  Rebuild_queries.arguments_used_by_known_arity_call
    (data_for_id t callee).queries callee args

let arguments_used_by_unknown_arity_call t callee args =
  Rebuild_queries.arguments_used_by_unknown_arity_call
    (data_for_id t callee).queries callee args

let find_code_metadata_in_data t data code_id =
  match UA.find_code_metadata data.code_changes code_id with
  | Some _ as metadata -> metadata
  | None ->
    if
      Analysis_scope.contains_unit t.analysis_scope
        (Code_id.get_compilation_unit code_id)
    then
      Misc.fatal_errorf
        "Rebuild_solution: code_id %a is in the analysis scope but missing in \
         code changes"
        Code_id.print code_id;
    None

let find_code_metadata t code_id =
  let data = t.get_unit (Code_id.get_compilation_unit code_id) in
  find_code_metadata_in_data t data code_id

let get_code_metadata t code_id =
  match find_code_metadata t code_id with
  | Some metadata -> metadata
  | None ->
    Misc.fatal_errorf "Rebuild_solution: no metadata for code_id %a"
      Code_id.print code_id

let get_calling_convention_change t code_id =
  let data = t.get_unit (Code_id.get_compilation_unit code_id) in
  match find_code_metadata_in_data t data code_id with
  | Some _ -> UA.get_calling_convention_change data.code_changes code_id
  | None -> UA.Not_changing_calling_convention

let offsets_for_free_names t free_names =
  let offsets =
    Function_slot.Set.fold
      (fun function_slot offsets ->
        let data =
          t.get_unit (Function_slot.get_compilation_unit function_slot)
        in
        match
          Exported_offsets.function_slot_offset data.slot_offsets function_slot
        with
        | Some info ->
          Exported_offsets.add_function_slot_offset offsets function_slot info
        | None ->
          Misc.fatal_errorf "Rebuild_solution: no offset for function slot %a"
            Function_slot.print function_slot)
      (Name_occurrences.all_function_slots_at_normal_mode free_names)
      Exported_offsets.empty
  in
  Value_slot.Set.fold
    (fun value_slot offsets ->
      let data = t.get_unit (Value_slot.get_compilation_unit value_slot) in
      match Exported_offsets.value_slot_offset data.slot_offsets value_slot with
      | Some info ->
        Exported_offsets.add_value_slot_offset offsets value_slot info
      | None ->
        Misc.fatal_errorf "Rebuild_solution: no offset for value slot %a"
          Value_slot.print value_slot)
    (Name_occurrences.all_value_slots_at_normal_mode free_names)
    offsets
