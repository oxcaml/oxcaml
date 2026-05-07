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

(** The representation of the application of an OCaml function, OCaml method or
    external call to a list of arguments. *)

type t

val free_names_except_callee : t -> Name_occurrences.t

include Expr_std.S with type t := t

val free_names_without_exn_continuation : t -> Name_occurrences.t

include Contains_ids.S with type t := t

module Result_continuation : sig
  type t =
    | Return of Continuation.t
    | Never_returns

  val print : Format.formatter -> t -> unit
end

(** Where the result of an application goes, together with the shape of that
    result.

    These two pieces of information are fused into a single type so that
    [Returns_to] is the only way to say "a concrete result of this arity is
    consumed at this continuation". An application whose result shape is not
    known therefore cannot claim to hand a result to a continuation of the
    current function, and an application that does not return cannot be mistaken
    for one that does. *)
module Return : sig
  type t =
    | Returns_to of
        { cont : Continuation.t;
          arity : [`Unarized] Flambda_arity.t
        }
        (** The application produces a result of the given arity, which is
            received by the continuation [cont]. *)
    | Tail_forwards_to_caller of Continuation.t
        (** The application produces a result whose shape is not known here,
            because the callee's return type has layout [any]. The result is not
            consumed in the current function: it is passed straight through to
            the caller.

            [cont] is the application's return continuation: the enclosing
            function's return continuation, or a continuation that eta-forwards
            to it. Unlike the other constructors this one really does constrain
            placement: [To_cmm] and [To_jsir] can only lower it in tail
            position. *)
    | Never_returns of { arity : Result_arity.t }
        (** Control never reaches a destination, and no destination is recorded
            at all: either the callee does not return normally, or the result
            has been shown to be unreachable. In particular an application whose
            result arity is [Bottom] never records a destination, even off tail
            position: whatever continuation the surrounding code was translated
            into is simply not referenced by the application, and dies as
            unreachable.

            No result is ever produced, but [arity] still records the shape the
            callee would have returned. When [arity] is concrete, lowering needs
            it: the Cmm result type of the call is fixed by the callee's calling
            convention, not by what the caller does with the result. [Unknown]
            and [Bottom] arities carry no such convention and are lowered as
            calls returning nothing; within this constructor the two are
            interchangeable, and consumers must not treat them differently. *)

  val equal : t -> t -> bool

  include Contains_names.S with type t := t

  (** Convert from a separately-computed destination and result arity. The
      conversion is total but collapsing: a [Bottom] arity drops the
      destination, because control never arrives there. It is lossless on
      everything [t] can represent, so the passes that carry the two around
      independently (the reaper, [Compare], the parser) round-trip exactly;
      prefer building the constructor directly wherever the shape of the result
      is statically known, since that is what makes the intent legible. *)
  val create : Result_continuation.t -> Result_arity.t -> t

  (** Where control goes upon normal return, forgetting both the shape of the
      result and whether control gets there at all. Prefer matching on [t]
      itself unless the destination really is all that matters (computing free
      names, say). *)
  val continuation : t -> Result_continuation.t

  (** The shape the callee returns, or would return: [Unknown] only when the
      result is forwarded to the caller. Prefer matching on [t] itself when the
      answer feeds a decision about where the result goes; this accessor is for
      the callers that only need the calling convention. *)
  val arity : t -> Result_arity.t

  (** Change the destination continuation, keeping everything else. Applications
      with no recorded destination are returned unchanged. *)
  val with_continuation : t -> Continuation.t -> t
end

module Position : sig
  type t =
    | Normal
    | Nontail

  val equal : t -> t -> bool
end

(** Create an application expression.

    External calls must have a concrete ([Ok]) result arity, so for that call
    kind the only permitted [return]s are [Returns_to] and
    [Never_returns { arity = Ok _ }]. *)
val create :
  callee:Simple.t option ->
  return:Return.t ->
  Exn_continuation.t ->
  args:Simple.t list ->
  args_arity:[`Complex] Flambda_arity.t ->
  call_kind:Call_kind.t ->
  return_mode:Alloc_mode.For_applications.t ->
  Debuginfo.t ->
  inlined:Inlined_attribute.t ->
  inlining_state:Inlining_state.t ->
  probe:Probe.t ->
  position:Position.t ->
  relative_history:Inlining_history.Relative.t ->
  t

(* CR mshinwell: This doesn't really make sense for C calls; we should have a
   separate type of symbols for those too, since [Symbol.t] is for data
   symbols. *)

(* CR mshinwell: Try to have a more robust way of tracking applications of
   probes *)

(** The function or method being applied. *)
val callee : t -> Simple.t option

(** The arguments of the function or method being applied. *)
val args : t -> Simple.t list

(** The arity of the arguments being applied. *)
val args_arity : t -> [`Complex] Flambda_arity.t

(** Where the result of the application goes, and what shape it has. *)
val return : t -> Return.t

(** [Return.arity (return t)]. *)
val return_arity : t -> Result_arity.t

(** Information about what kind of call is involved (direct function call,
    method call, etc). *)
val call_kind : t -> Call_kind.t

val return_mode : t -> Alloc_mode.For_applications.t

(** [Return.continuation (return t)]. *)
val continuation : t -> Result_continuation.t

(** Where to jump to upon the application raising an exception. *)
val exn_continuation : t -> Exn_continuation.t

(** Debugging information attached to the application. *)
val dbg : t -> Debuginfo.t

(** Instructions from the source code as to whether the callee should be
    inlined. *)
val inlined : t -> Inlined_attribute.t

(** Whether the call was marked [@nontail] *)
val position : t -> Position.t

val erase_callee : t -> t

(** Change where the result of an application goes. The [create] invariants are
    re-checked. *)
val with_return : t -> Return.t -> t

val with_return_and_exn_continuation : t -> Return.t -> Exn_continuation.t -> t

val with_exn_continuation : t -> Exn_continuation.t -> t

(** Change the arguments of an application *)
val with_args : t -> Simple.t list -> args_arity:[`Complex] Flambda_arity.t -> t

(** Change the call kind of an application. *)
val with_call_kind : t -> Call_kind.t -> t

val inlining_state : t -> Inlining_state.t

val inlining_arguments : t -> Inlining_arguments.t

val probe : t -> Probe.t

val relative_history : t -> Inlining_history.Relative.t

(** Whether control can return from the application: [true] for [Returns_to] and
    [Tail_forwards_to_caller], [false] for [Never_returns]. *)
val returns : t -> bool

val with_inlined_attribute : t -> Inlined_attribute.t -> t
