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

type t =
  { analysis_scope : Analysis_scope.t;
    data : data
  }

let create ~analysis_scope ~queries ~(unboxing : UA.result) ~code_changes
    ~slot_offsets =
  { analysis_scope;
    data =
      { queries;
        unboxed_fields = unboxing.unboxed_fields;
        changed_representation = unboxing.changed_representation;
        code_changes;
        slot_offsets
      }
  }

let has_use t id = Rebuild_queries.has_use t.data.queries id

let has_source t id = Rebuild_queries.has_source t.data.queries id

let field_used t id field = Rebuild_queries.field_used t.data.queries id field

let get_unboxed_fields t id =
  Code_id_or_name.Map.find_opt id t.data.unboxed_fields

let get_changed_representation t id =
  Option.map fst (Code_id_or_name.Map.find_opt id t.data.changed_representation)

let code_id_actually_directly_called t name =
  Rebuild_queries.code_id_actually_directly_called t.data.queries name

let arguments_used_by_known_arity_call t callee args =
  Rebuild_queries.arguments_used_by_known_arity_call t.data.queries callee args

let arguments_used_by_unknown_arity_call t callee args =
  Rebuild_queries.arguments_used_by_unknown_arity_call t.data.queries callee
    args

let find_code_metadata t code_id =
  match UA.find_code_metadata t.data.code_changes code_id with
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

let get_code_metadata t code_id =
  match find_code_metadata t code_id with
  | Some metadata -> metadata
  | None ->
    Misc.fatal_errorf "Rebuild_solution: no metadata for code_id %a"
      Code_id.print code_id

let get_calling_convention_change t code_id =
  match find_code_metadata t code_id with
  | Some _ -> UA.get_calling_convention_change t.data.code_changes code_id
  | None -> UA.Not_changing_calling_convention

let is_changing_calling_convention t code_id =
  match get_calling_convention_change t code_id with
  | UA.Not_changing_calling_convention -> false
  | UA.Changing_calling_convention _ -> true

let offsets_for_free_names t free_names =
  let offsets =
    Function_slot.Set.fold
      (fun function_slot offsets ->
        match
          Exported_offsets.function_slot_offset t.data.slot_offsets
            function_slot
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
      match
        Exported_offsets.value_slot_offset t.data.slot_offsets value_slot
      with
      | Some info ->
        Exported_offsets.add_value_slot_offset offsets value_slot info
      | None ->
        Misc.fatal_errorf "Rebuild_solution: no offset for value slot %a"
          Value_slot.print value_slot)
    (Name_occurrences.all_value_slots_at_normal_mode free_names)
    offsets
