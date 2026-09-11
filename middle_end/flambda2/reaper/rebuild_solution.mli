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

(** Materialised answers and offsets, without a Datalog database. *)
type data

val create_data :
  queries:Rebuild_queries.t ->
  unboxing:Unboxing_analysis.result ->
  code_changes:Unboxing_analysis.code_changes ->
  slot_offsets:Exported_offsets.t ->
  data

val empty_data : data

val ids_for_export : data -> Ids_for_export.t

val fields_for_export : data -> Field.Set.t

val apply_renaming :
  data -> Renaming.t -> rename_field:(Field.t -> Field.t) -> data

(** Partition by the compilation unit of each outermost key or slot. *)
val partition_by_compilation_unit : data -> data Compilation_unit.Map.t

(** Rebuild queries load only the section owning their key. *)
type t

(** Any caching of loaded sections is the responsibility of [get_unit]. *)
val create :
  analysis_scope:Analysis_scope.t -> get_unit:(Compilation_unit.t -> data) -> t

(** Use one in-memory data record for every compilation unit. *)
val of_data : data -> analysis_scope:Analysis_scope.t -> t

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

(** Missing metadata for a unit in the analysis scope is a fatal error. Missing
    metadata outside the scope returns [None]. *)
val find_code_metadata : t -> Code_id.t -> Code_metadata.t option

(** Require metadata, including for code outside the analysis scope. *)
val get_code_metadata : t -> Code_id.t -> Code_metadata.t

(** Missing entries outside the analysis scope have unchanged calling
    conventions; missing entries inside the scope are fatal errors. *)
val get_calling_convention_change :
  t -> Code_id.t -> Unboxing_analysis.calling_convention_change

(** Copy exactly the offsets of slots occurring at normal mode, loading their
    owning sections. Missing offsets are fatal errors. *)
val offsets_for_free_names : t -> Name_occurrences.t -> Exported_offsets.t
