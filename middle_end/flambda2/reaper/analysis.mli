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

type result = Unboxing_analysis.result

val fixpoint : Global_flow_graph.graph -> result

val get_unboxed_fields :
  result -> Code_id_or_name.t -> Unboxing_analysis.unboxed option

val get_changed_representation :
  result -> Code_id_or_name.t -> Unboxing_analysis.changed_representation option

val has_use : result -> Code_id_or_name.t -> bool

val any_usage : result -> Code_id_or_name.t -> bool

val has_source : result -> Code_id_or_name.t -> bool

val any_source : result -> Code_id_or_name.t -> bool

val field_used : result -> Code_id_or_name.t -> Field.t -> bool

val not_local_field_has_source : result -> Code_id_or_name.t -> Field.t -> bool

val cannot_change_calling_convention : result -> Code_id.t -> bool

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
