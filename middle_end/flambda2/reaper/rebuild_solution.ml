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

type t =
  { queries : Rebuild_queries.t;
    unboxed_fields : UA.unboxed Code_id_or_name.Map.t;
    changed_representation :
      (UA.changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t;
    code_changes : UA.code_changes
  }

let create ~queries ~(unboxing : UA.result) ~code_changes =
  { queries;
    unboxed_fields = unboxing.unboxed_fields;
    changed_representation = unboxing.changed_representation;
    code_changes
  }

let has_use t id = Rebuild_queries.has_use t.queries id

let has_source t id = Rebuild_queries.has_source t.queries id

let field_used t id field = Rebuild_queries.field_used t.queries id field

let get_unboxed_fields t id = Code_id_or_name.Map.find_opt id t.unboxed_fields

let get_changed_representation t id =
  Option.map fst (Code_id_or_name.Map.find_opt id t.changed_representation)

let code_id_actually_directly_called t name =
  Rebuild_queries.code_id_actually_directly_called t.queries name

let arguments_used_by_known_arity_call t callee args =
  Rebuild_queries.arguments_used_by_known_arity_call t.queries callee args

let arguments_used_by_unknown_arity_call t callee args =
  Rebuild_queries.arguments_used_by_unknown_arity_call t.queries callee args

let find_code_metadata t code_id =
  match UA.find_code_metadata t.code_changes code_id with
  | Some _ as metadata -> metadata
  | None ->
    if Current_unit.is_current (Code_id.get_compilation_unit code_id)
    then
      Misc.fatal_errorf
        "Rebuild_solution: code_id %a is in the current unit but missing in \
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
  | Some _ -> UA.get_calling_convention_change t.code_changes code_id
  | None -> UA.Not_changing_calling_convention

let is_changing_calling_convention t code_id =
  match get_calling_convention_change t code_id with
  | UA.Not_changing_calling_convention -> false
  | UA.Changing_calling_convention _ -> true
