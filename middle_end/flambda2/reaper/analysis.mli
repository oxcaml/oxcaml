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

(** What the rebuild needs to know about the solved analysis. *)
type 'f solution

val solve :
  'f Traverse.Problem.t -> analysis_scope:Analysis_scope.t -> 'f solution

val get_unboxed_fields :
  'f solution -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  'f solution ->
  Code_id_or_name.t ->
  Unboxing_analysis.changed_representation option

val has_use : 'f solution -> Code_id_or_name.t -> bool

val has_source : 'f solution -> Code_id_or_name.t -> bool

val field_used : 'f solution -> Code_id_or_name.t -> Field.t -> bool

(* The call queries below expect the corresponding application to have been
   recorded during traversal. *)
val code_id_actually_directly_called :
  'f solution -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  'f solution ->
  Code_id_or_name.t ->
  'a list ->
  ('a * Points_to_analysis.keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  'f solution ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * Points_to_analysis.keep_or_delete) list list

val get_calling_convention_change :
  'f solution -> Code_id.t -> Unboxing_analysis.calling_convention_change

val is_changing_calling_convention : 'f solution -> Code_id.t -> bool

(* Returns [None] for code ids of units that did not participate in the
   solve. *)
val find_code_metadata : 'f solution -> Code_id.t -> Code_metadata.t option

val slot_offsets : 'f solution -> Slot_offsets.result

val rewrite_kind_with_subkind :
  'f solution ->
  Name.t ->
  Flambda_kind.With_subkind.t ->
  Flambda_kind.With_subkind.t

val final_typing_env : 'f solution -> ('f, typing_env) Traverse.With_types.t
