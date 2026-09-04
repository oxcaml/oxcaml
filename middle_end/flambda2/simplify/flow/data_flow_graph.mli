(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** An internal type for the data_flow graph *)
type t

(** Printing function *)
val print : Format.formatter -> t -> unit

(** Create the data flow graph *)
val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  code_age_relation:Code_age_relation.t ->
  used_value_slots:Name_occurrences.t Or_unknown.t ->
  code_ids_to_never_delete:Code_id.Set.t ->
  generate_phantom_lets:bool ->
  Flow_types.Continuation_info.t Continuation.Map.t ->
  t

(** Run the required names analysis *)
val required_names : t -> Flow_types.Data_flow_result.t

(** The names that the debugger may need to locate in order to display the
    values of variables described by phantom lets: the transitive closure of the
    name dependency graph (including the edges from continuation parameters to
    their arguments) from the user-visible variables. See
    [Flow_types.Data_flow_result.required_names_for_phantom_lets]. *)
val required_names_for_phantom_lets : t -> Name.Set.t
