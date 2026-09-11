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
  compute_specialisation_site_info:bool ->
  Flow_types.Continuation_info.t Continuation.Map.t ->
  t

(** Run the required names analysis. The site information is empty unless the
    graph was created with [compute_specialisation_site_info]. *)
val required_names :
  t -> Flow_types.Data_flow_result.t * Flow_types.Specialisation_site_info.t
