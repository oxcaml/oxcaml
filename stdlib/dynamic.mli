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

    [Dynamic] works like [Ref], except that changes to its value are only
    visible to the current fiber and its children.

    Every dynamic variable has a single "root" value, which is visible by
    default to all running fibers. A fiber can temporarily change the locally
    visible value of a dynamic variable within the scope of a function by
    calling [with_temporarily]. During the execution of [with_temporarily],
    any changes to the root value of the dynamic variable (eg via calls to
    [set_root] by the current or other fibers) are unobservable until after
    the outermost call to [with_temporarily] returns. *)
type ('a : value_or_null) t : value mod contended portable

(** [make v] creates a new dynamic variable with initial root value [v].

    Since any domain can access the [Dynamic.t], the value must be portable.
    Since any domain can access and modify the [Dynamic.t] without
    synchronization, the type of values in the [Dynamic.t] must cross the
    contention axis. *)
val make : ('a : value_or_null mod contended). 'a @ portable -> 'a t

(** [get dynamic] retrieves the current value of the dynamic variable [dynamic]
    in the current fiber.

    Within a call to [with_temporarily], this returns the {i local} value
    of the dynamic variable (the value that was passed to [with_temporarily].
    Outside of a call to [with_temporarily], this returns the {i root} value *)
val get : ('a : value_or_null mod contended). 'a t -> 'a @ portable

(** [with_temporarily dynamic v ~f] invokes [f] in a context where [dynamic] is
    set to [v], then restores [dynamic] to the parent value (either the root
    value, or the value set by a surrounding call to [with_temporarily]). *)
val with_temporarily :
  ('a : value mod contended) 'b.
  'a t ->
  'a @ portable ->
  f:(unit -> 'b @ unique once) @ once ->
  'b @ unique once
