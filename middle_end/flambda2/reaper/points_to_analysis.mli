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

type keep_or_delete =
  | Keep
  | Delete

module Relations : sig
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  val usages : Code_id_or_name.t term -> Code_id_or_name.t term -> _ atom

  val sources : Code_id_or_name.t term -> Code_id_or_name.t term -> _ atom

  val rev_alias :
    from:Code_id_or_name.t term -> to_:Code_id_or_name.t term -> _ atom

  val rev_use :
    from:Code_id_or_name.t term -> to_:Code_id_or_name.t term -> _ atom

  val rev_constructor :
    from:Code_id_or_name.t term ->
    Field.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  val rev_accessor :
    base:Code_id_or_name.t term ->
    Field.t term ->
    to_:Code_id_or_name.t term ->
    _ atom

  val rev_parameter :
    to_:Code_id_or_name.t term ->
    Cofield.t term ->
    base:Code_id_or_name.t term ->
    _ atom

  val rev_argument :
    base:Code_id_or_name.t term ->
    Cofield.t term ->
    from:Code_id_or_name.t term ->
    _ atom

  val reading_field : Field.t term -> Code_id_or_name.t term -> _ atom

  val escaping_field : Field.t term -> Code_id_or_name.t term -> _ atom

  val has_usage : Code_id_or_name.t term -> _ atom

  val has_source : Code_id_or_name.t term -> _ atom

  val field_of_constructor_is_used_tbl :
    ( unit Field.Map.t Code_id_or_name.Map.t,
      Code_id_or_name.t -> Field.t -> Datalog.nil,
      unit )
    Datalog.table

  val field_of_constructor_is_used :
    Code_id_or_name.t term -> Field.t term -> _ atom

  val allocation_point_dominator :
    Code_id_or_name.t term -> Code_id_or_name.t term -> _ atom

  val dominated_by_allocation_point :
    Code_id_or_name.t term -> Code_id_or_name.t term -> _ atom
end

type usages = Usages of unit Code_id_or_name.Map.t [@@unboxed]

val get_all_usages :
  follow_known_arity_calls:bool ->
  Datalog.database ->
  unit Code_id_or_name.Map.t ->
  usages

val get_direct_usages :
  Datalog.database -> unit Code_id_or_name.Map.t -> unit Code_id_or_name.Map.t

type field_usage =
  | Used_as_top
  | Used_as_vars of unit Code_id_or_name.Map.t

val get_one_field : Datalog.database -> Field.t -> usages -> field_usage

val get_fields : Datalog.database -> usages -> field_usage Field.Map.t

val get_one_field_usage_of_constructors :
  Datalog.database -> unit Code_id_or_name.Map.t -> Field.t -> field_usage

val get_fields_usage_of_constructors :
  Datalog.database -> unit Code_id_or_name.Map.t -> field_usage Field.Map.t

type set_of_closures_def =
  | Not_a_set_of_closures
  | Set_of_closures of (Function_slot.t * Code_id_or_name.t) list

val get_set_of_closures_def :
  Datalog.database -> Code_id_or_name.t -> set_of_closures_def

val any_usage : Datalog.database -> Code_id_or_name.t -> bool

val any_source : Datalog.database -> Code_id_or_name.t -> bool

val has_use : Datalog.database -> Code_id_or_name.t -> bool

val field_used : Datalog.database -> Code_id_or_name.t -> Field.t -> bool

val not_local_field_has_source :
  Datalog.database -> Code_id_or_name.t -> Field.t -> bool

val has_source_query : Datalog.database -> Code_id_or_name.t -> bool

val code_id_actually_directly_called :
  Datalog.database -> Name.t -> Code_id.Set.t Or_unknown.t

val arguments_used_by_known_arity_call :
  Datalog.database -> Code_id_or_name.t -> 'a list -> ('a * keep_or_delete) list

val arguments_used_by_unknown_arity_call :
  Datalog.database ->
  Code_id_or_name.t ->
  'a list list ->
  ('a * keep_or_delete) list list

type single_field_source =
  | No_source
  | One of Code_id_or_name.t
  | Many

val get_single_field_source :
  Datalog.database -> Code_id_or_name.t -> Field.t -> single_field_source

val get_allocation_point :
  Datalog.database -> Code_id_or_name.t -> Code_id_or_name.t option

val perform_analysis :
  Datalog.database -> stats:Datalog.Schedule.stats -> Datalog.database
