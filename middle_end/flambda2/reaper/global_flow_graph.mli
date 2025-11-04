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

module Relations : sig
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  val alias :
    to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

  val use : to_:Code_id_or_name.t term -> from:Code_id_or_name.t term -> _ atom

  val accessor :
    to_:Code_id_or_name.t term ->
    Field.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  val constructor :
    base:Code_id_or_name.t term ->
    Field.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val coaccessor :
    to_:Code_id_or_name.t term ->
    Cofield.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  val coconstructor :
    base:Code_id_or_name.t term ->
    Cofield.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val propagate :
    if_used:Code_id_or_name.t term ->
    to_:Code_id_or_name.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val any_usage : Code_id_or_name.t term -> _ atom

  val any_source : Code_id_or_name.t term -> _ atom

  (* An entry (code_id, v) in this relation means that [v] is the "my_closure"
     variable of the code associated to [code_id]. *)
  val code_id_my_closure :
    code_id:Code_id_or_name.t term ->
    my_closure:Code_id_or_name.t term ->
    _ atom
end

val add_alias : graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_coaccessor_dep :
  graph -> to_:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

val add_coconstructor_dep :
  graph -> base:Code_id_or_name.t -> Cofield.t -> from:Code_id_or_name.t -> unit

val add_propagate_dep :
  graph ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_any_usage : graph -> Code_id_or_name.t -> unit

val add_any_source : graph -> Code_id_or_name.t -> unit

val add_code_id_my_closure : graph -> Code_id.t -> Variable.t -> unit

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val print_iter_edges :
  print_edge:(Code_id_or_name.t * Code_id_or_name.t * string -> unit) ->
  graph ->
  unit
