(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

type action

val bind_iterator :
  'a option Channel.receiver with_name -> 'a Trie.Iterator.t with_name -> action

val unless :
  is_trie:('t, 'k, 'v) Trie.is_trie ->
  name:string ->
  't Channel.receiver ->
  'k Option_receiver.hlist with_names ->
  action

val unless_eq :
  'k Value.repr ->
  'k option Channel.receiver with_name ->
  'k option Channel.receiver with_name ->
  action

val filter :
  ('k Constant.hlist -> bool) -> 'k Option_receiver.hlist with_names -> action

type actions

val add_action : actions -> action -> unit

module Order : sig
  type t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val parameters : t
end

module Level : sig
  type 'a t

  val print : Format.formatter -> 'a t -> unit

  (** Returns a reference to the current value at this level.

      {b Note}: This reference is set to any new value found prior to executing
      the associated actions, if any, and can thus be used in actions for this
      level or levels of later orders. *)
  val use_output : 'a t -> 'a option Channel.receiver with_name

  (** Actions to execute immediately after a value is found at this level. *)
  val actions : 'a t -> actions

  val add_iterator : 'a t -> 'a Trie.Iterator.t with_name -> unit

  (** Order of this level. Levels will be iterated over in a nested loop of
      ascending order: if level [order b >= order a], then the loop for [b] is
      nested {b inside} the loop for [a]. *)
  val order : 'a t -> Order.t
end

type context

val create_context : unit -> context

val add_new_level : context -> string -> 'a Level.t

(** Initial actions are always executed when iterating over a cursor, before
    opening the first level. *)
val initial_actions : context -> actions

type 'v t

type 'a cursor = 'a t

val print : Format.formatter -> 'a t -> unit

type call

val create_call :
  ('a Constant.hlist -> unit) ->
  name:string ->
  'a Option_receiver.hlist with_names ->
  call

val create :
  ?calls:call list ->
  ?output:'v Option_receiver.hlist with_names ->
  context ->
  'v t

val iter : 'v t -> ('v Constant.hlist -> unit) -> unit
