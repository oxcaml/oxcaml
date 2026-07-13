(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Aspen Smith, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

@@ portable

(** A value of type ['a Dynamic.t] is a dynamically scoped variable of type
    ['a].

    A fiber can temporarily change the locally visible value of a dynamic
    variable by calling [with_temporarily]. Child fibers executing within
    [with_temporarily] will see the local value, but escaped fibers will
    read the value bound by their future parent. *)
type 'a t : value mod everything with 'a @@ contended portable

(** [make ()] creates a new dynamic variable. *)
val make : unit -> 'a t

(** [get t] retrieves the current value of the dynamic variable [t] in the
    current fiber.

    Within a call to [with_temporarily], this returns the {i local} value
    of the dynamic variable. Outside of a call to [with_temporarily], this
    returns [Null].

    Because the dynamic binding may be accessed concurrently, its contents
    are contended. *)
val get : 'a t -> 'a or_null @ contended portable

(** [with_temporarily t v ~f] invokes [f] in a context where [t] is bound to
    [v], then restores [t] to its previous state.

    Because the dynamic binding may be accessed concurrently, the new value
    must be portable. *)
val with_temporarily : ('b : value_or_null).
  'a t ->
  'a @ contended portable ->
  f:(unit -> 'b @ local unique once) @ local once ->
  'b @ local unique once

module Context : sig
  (** ['a Context.t] is like ['a Dynamic.t], but fibers permanently inherit the
      value bound at the time of their creation. *)
  type 'a t : value mod everything with 'a @@ contended portable

  (** [make ()] creates a new contextual variable. *)
  val make : unit -> 'a t

  (** [get t] retrieves the current value of the contextual variable [t] in the
      current fiber.

      Within a call to [with_temporarily], this returns the {i local} value
      of the contextual variable. Outside of a call to [with_temporarily], this
      returns [Null].

      Because the contextual binding may be accessed concurrently, its contents
      are contended. *)
  val get : 'a t -> 'a or_null @ contended portable

  (** [with_temporarily t v ~f] invokes [f] in a context where [t] is bound to
      [v], then restores [t] to its previous state.

      Because the contextual binding may be accessed concurrently, the new value
      must be portable. *)
  val with_temporarily : ('b : value_or_null).
    'a t ->
    'a @ contended portable ->
    f:(unit -> 'b @ local unique once) @ local once ->
    'b @ local unique once
end
