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
type result

(** Returns the result used for the rebuild, and the result of unboxing
    containing the full database for the type rewriting, code changes and slot
    offsets computations. *)
val fixpoint :
  Global_flow_graph.graph ->
  applications:Traverse_acc.Applications.t ->
  analysis_scope:Analysis_scope.t ->
  Unboxing_analysis.result * result

val get_unboxed_fields :
  result -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  result -> Code_id_or_name.t -> Unboxing_analysis.changed_representation option

val has_use : result -> Code_id_or_name.t -> bool

val has_source : result -> Code_id_or_name.t -> bool

val field_used : result -> Code_id_or_name.t -> Field.t -> bool

(* The call queries below expect the corresponding application to have been
   recorded during traversal. *)
val code_id_actually_directly_called :
  result -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  result ->
  Code_id_or_name.t ->
  'a list ->
  ('a * Points_to_analysis.keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  result ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * Points_to_analysis.keep_or_delete) list list
