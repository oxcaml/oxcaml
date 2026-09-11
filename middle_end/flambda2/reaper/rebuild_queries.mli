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

module Requests : sig
  type t

  val empty : t

  (** Record the named callee and argument widths of a function call, including
      nullary calls. Ignore other call kinds and absent or constant callees. *)
  val add_apply : t -> Apply.t -> t

  (** Take the maximum known-arity width and pointwise maxima of unknown-arity
      group widths, retaining the longer tail. *)
  val union : t -> t -> t

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t
end

(** Materialised rebuild answers, without a Datalog database. *)
type t

val empty : t

val create : Datalog.database -> requests:Requests.t -> t

val has_use : t -> Code_id_or_name.t -> bool

val has_source : t -> Code_id_or_name.t -> bool

val field_used : t -> Code_id_or_name.t -> Field.t -> bool

(** Call queries require a corresponding request; missing requests and argument
    dimensions exceeding the recorded bounds are fatal errors. *)
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

val ids_for_export : t -> Ids_for_export.t

val fields_for_export : t -> Field.Set.t

val apply_renaming : t -> Renaming.t -> rename_field:(Field.t -> Field.t) -> t

(** Union answers whose outermost key sets are disjoint in each map. *)
val disjoint_union : t -> t -> t

(** Partition by the compilation unit of each map's outermost key. *)
val partition_by_compilation_unit : t -> t Compilation_unit.Map.t
