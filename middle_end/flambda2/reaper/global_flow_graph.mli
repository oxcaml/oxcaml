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

type graph

val to_datalog : graph -> Datalog.database

type 'a atom = [> `Atom of Datalog.atom] as 'a

type 'a term = 'a Datalog.Term.t

val alias_rel :
  to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

val add_alias : graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val use_rel :
  to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val accessor_rel :
  to_:Code_id_or_name.t term ->
  relation:Field.t term ->
  base:Code_id_or_name.t term ->
  _ atom

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val constructor_rel :
  base:Code_id_or_name.t term ->
  relation:Field.t term ->
  from:Code_id_or_name.t term ->
  _ atom

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val coaccessor_rel :
  to_:Code_id_or_name.t term ->
  relation:Cofield.t term ->
  base:Code_id_or_name.t term ->
  _ atom

val add_coaccessor_dep :
  graph -> to_:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

val coconstructor_rel :
  base:Code_id_or_name.t term ->
  relation:Cofield.t term ->
  from:Code_id_or_name.t term ->
  _ atom

val add_coconstructor_dep :
  graph -> base:Code_id_or_name.t -> Cofield.t -> from:Code_id_or_name.t -> unit

val propagate_rel :
  if_used:Code_id_or_name.t term ->
  to_:Code_id_or_name.t term ->
  from:Code_id_or_name.t term ->
  _ atom

val add_propagate_dep :
  graph ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val any_usage_pred : Code_id_or_name.t term -> _ atom

val add_use : graph -> Code_id_or_name.t -> unit

val any_source_pred : Code_id_or_name.t term -> _ atom

val add_any_source : graph -> Code_id_or_name.t -> unit

(* An entry (code_id, v) in this relation means that [v] is the "my_closure"
   variable of the code associated to [code_id]. *)
val code_id_my_closure_rel :
  code_id:Code_id_or_name.t term -> my_closure:Code_id_or_name.t term -> _ atom

val add_code_id_my_closure : graph -> Code_id.t -> Variable.t -> unit

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val print_iter_edges :
  print_edge:(Code_id_or_name.t * Code_id_or_name.t * string -> unit) ->
  graph ->
  unit
