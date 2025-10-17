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

type 'a rel0 = [> `Atom of Datalog.atom] as 'a

type ('a, 'b) rel1 = 'a Datalog.Term.t -> 'b rel0

type ('a, 'b, 'c) rel2 = 'a Datalog.Term.t -> ('b, 'c) rel1

type ('a, 'b, 'c, 'd) rel3 = 'a Datalog.Term.t -> ('b, 'c, 'd) rel2

val alias_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val use_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val accessor_rel : (Code_id_or_name.t, Field.t, Code_id_or_name.t, _) rel3

val constructor_rel : (Code_id_or_name.t, Field.t, Code_id_or_name.t, _) rel3

val coaccessor_rel : (Code_id_or_name.t, Cofield.t, Code_id_or_name.t, _) rel3

val coconstructor_rel :
  (Code_id_or_name.t, Cofield.t, Code_id_or_name.t, _) rel3

val propagate_rel :
  (Code_id_or_name.t, Code_id_or_name.t, Code_id_or_name.t, _) rel3

val any_usage_pred : (Code_id_or_name.t, _) rel1

val any_source_pred : (Code_id_or_name.t, _) rel1

(* An entry (code_id, v) in this relation means that [v] is the "my_closure"
   variable of the code associated to [code_id]. *)
val code_id_my_closure_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val add_alias : graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use : graph -> Code_id_or_name.t -> unit

val add_any_source : graph -> Code_id_or_name.t -> unit

val add_propagate_dep :
  graph ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val add_coaccessor_dep :
  graph -> to_:Code_id_or_name.t -> Cofield.t -> base:Code_id_or_name.t -> unit

val add_coconstructor_dep :
  graph -> base:Code_id_or_name.t -> Cofield.t -> from:Code_id_or_name.t -> unit

val add_code_id_my_closure : graph -> Code_id.t -> Variable.t -> unit

val print_iter_edges :
  print_edge:(Code_id_or_name.t * Code_id_or_name.t * string -> unit) ->
  graph ->
  unit
