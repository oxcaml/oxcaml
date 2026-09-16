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

(** Materialised answers to the queries the rebuild makes of the solved
    analysis, without a Datalog database. *)
type t

val create :
  queries:Rebuild_queries.t ->
  unboxing:Unboxing_analysis.result ->
  code_changes:Unboxing_analysis.code_changes ->
  t

val has_use : t -> Code_id_or_name.t -> bool

val has_source : t -> Code_id_or_name.t -> bool

val field_used : t -> Code_id_or_name.t -> Field.t -> bool

val get_unboxed_fields :
  t -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  t -> Code_id_or_name.t -> Unboxing_analysis.changed_representation option

val code_id_actually_directly_called : t -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  t ->
  Code_id_or_name.t ->
  'a list ->
  ('a * Points_to_analysis.keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  t ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * Points_to_analysis.keep_or_delete) list list

(** Missing metadata for code in the current unit is a fatal error. Missing
    metadata for other units' code returns [None]. *)
val find_code_metadata : t -> Code_id.t -> Code_metadata.t option

(** Require metadata, including for code from other units. *)
val get_code_metadata : t -> Code_id.t -> Code_metadata.t

(** Missing entries for other units' code have unchanged calling conventions;
    missing entries for the current unit are fatal errors. *)
val get_calling_convention_change :
  t -> Code_id.t -> Unboxing_analysis.calling_convention_change

val is_changing_calling_convention : t -> Code_id.t -> bool
