(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The representation of the application of a continuation. In the zero-arity
    case this is just "goto". *)

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val create :
  ?trap_action:Trap_action.t ->
  fdo_counters:Fdo_counter.t list ->
  Continuation.t ->
  args:Simple.t list ->
  dbg:Debuginfo.t ->
  t

val goto : fdo_counters:Fdo_counter.t list -> Continuation.t -> t

val continuation : t -> Continuation.t

val args : t -> Simple.t list

val trap_action : t -> Trap_action.t option

val debuginfo : t -> Debuginfo.t

(** The pseudo-instrumentation counters of the edge this application of a
    continuation takes, when it is an arm of a switch (see [Fdo_counter]).
    Transformations that rearrange or redirect arms keep them attached to the
    edge they describe; they are dropped with the branch when a switch
    disappears. *)
val fdo_counters : t -> Fdo_counter.t list

val with_continuation : t -> Continuation.t -> t

val with_continuation_and_args : t -> Continuation.t -> args:Simple.t list -> t

val update_args : t -> args:Simple.t list -> t

val with_debuginfo : t -> dbg:Debuginfo.t -> t

val with_fdo_counters : t -> Fdo_counter.t list -> t

val is_raise : t -> bool

val is_goto : t -> bool

val clear_trap_action : t -> t

val to_one_arg_without_trap_action : t -> Simple.t option

(** Note that [compare] ignores the [Debuginfo.t]. *)
include Container_types.S with type t := t
