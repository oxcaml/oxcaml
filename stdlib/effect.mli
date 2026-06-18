(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                                                                        *)
(*   Copyright 2021 Indian Institute of Technology, Madras                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Effects.

    See 'Language extensions/Effect handlers' section in the manual.

    @since 5.0 *)

[@@@alert unstable
    "The Effect interface may change in incompatible ways in the future."
]

type 'a t = 'a eff = ..
(** The type of effects. *)

exception Unhandled : 'a t -> exn
(** [Unhandled e] is raised when effect [e] is performed and there is no
    handler for it. *)

exception Continuation_already_resumed
(** Exception raised when a continuation is continued or discontinued more
    than once. *)

exception Out_of_fibers
(** Exception raised by functions that allocate fibers. Does not
    necessarily imply that heap memory has been exhausted. *)

type _ t +=
  | Preemption : unit t
  (* CR aspsmith: Add more documentation here once preemption is closer to being
     finished *)

external perform : 'a t -> 'a = "%perform"
(** [perform e] performs an effect [e].

    @raise Unhandled if there is no handler for [e]. *)

type tick_outcome =
  | Preempt
  | Continue

module Deep : sig
  (** Deep handlers *)

  type nonrec ('a,'b) continuation = ('a,'b) continuation
  (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
      value and returns a ['b] value. *)

  val continue: ('a, 'b) continuation -> 'a -> 'b
  (** [continue k x] resumes the continuation [k] by passing [x] to [k].

      @raise Continuation_already_resumed if the continuation has already been
      resumed. *)

  val discontinue: ('a, 'b) continuation -> exn -> 'b
  (** [discontinue k e] resumes the continuation [k] by raising the
      exception [e] in [k].

      @raise Continuation_already_resumed if the continuation has already been
      resumed. *)

  val discontinue_with_backtrace:
    ('a, 'b) continuation -> exn -> Printexc.raw_backtrace -> 'b
  (** [discontinue_with_backtrace k e bt] resumes the continuation [k] by
      raising the exception [e] in [k] using [bt] as the origin for the
      exception.

      @raise Continuation_already_resumed if the continuation has already been
      resumed. *)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three fields -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val match_with: ('c -> 'a) -> 'c -> ('a,'b) handler -> 'b
  (** [match_with f x h] runs the computation [f x] in the handler [h].

      @raise Out_of_fibers if unable to allocate a fiber. *)

  type 'a effect_handler =
    { effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option }
  (** ['a effect_handler] is a deep handler with an identity value handler
      [fun x -> x] and an exception handler that raises any exception
      [fun e -> raise e]. *)

  val try_with: ('b -> 'a) -> 'b -> 'a effect_handler -> 'a
  (** [try_with f x h] runs the computation [f x] under the handler [h].

      @raise Out_of_fibers if unable to allocate a fiber. *)

  module Preemptible : sig
    (** Preemptible handlers

        Preemptible handlers are like normal handlers, except they also have
        the ability to receive "ticks" from the runtime, and can decide to
        preempt the current fiber on tick.

        To set the tick interval, call [Domain.Tick.acquire] before running a
        preemptible fiber. *)

    type ('a,'b) handler =
        { retc: 'a -> 'b;
          exnc: exn -> 'b;
          effc: 'c.'c t -> (('c,'b) continuation -> 'b) option;
          tickc: unit -> tick_outcome }
    (** [('a,'b) handler] is a handler record with four fields -- [retc]
        is the value handler, [exnc] handles exceptions, [effc] handles the
        effects performed by the computation enclosed by the handler, and
        [tickc] handles ticks, deciding whether to preempt. [tickc] should be
        signal-safe. If [tickc] returns [Preempt], a [Preemption] effect is
        performed. *)

    val match_with : ('c -> 'a) -> 'c -> ('a,'b) handler -> 'b
    (** [match_with f x h] runs the computation [f x] in the handler [h].

        @raise Out_of_fibers if unable to allocate a fiber. *)

    val try_with :
      on_tick:(unit -> tick_outcome) -> ('b -> 'a) -> 'b ->
      'a effect_handler -> 'a
      (** [try_with ~on_tick f x h] runs the computation [f x] under the handler
          [h], calling [on_tick] whenever a tick occurs. [on_tick] should
          be signal-safe. If it returns [Preempt], a [Preemption] effect is
          performed.

        @raise Out_of_fibers if unable to allocate a fiber. *)
  end

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)
end

module Shallow : sig
  (* Shallow handlers *)

  type ('a,'b) continuation
  (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
      value and returns a ['b] value. *)

  val fiber : ('a -> 'b) -> ('a, 'b) continuation
  (** [fiber f] constructs a continuation that runs the computation [f].

      @raise Out_of_fibers if unable to allocate a fiber. *)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three fields -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val continue_with : ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b
  (** [continue_with k v h] resumes the continuation [k] with value [v] with
      the handler [h].

      @raise Continuation_already_resumed if the continuation has already been
      resumed.
   *)

  val discontinue_with : ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b
  (** [discontinue_with k e h] resumes the continuation [k] by raising the
      exception [e] with the handler [h].

      @raise Continuation_already_resumed if the continuation has already been
      resumed.
   *)

  val discontinue_with_backtrace :
    ('a,'b) continuation -> exn -> Printexc.raw_backtrace ->
    ('b,'c) handler -> 'c
  (** [discontinue_with k e bt h] resumes the continuation [k] by raising the
      exception [e] with the handler [h] using the raw backtrace [bt] as the
      origin of the exception.

      @raise Continuation_already_resumed if the continuation has already been
      resumed.
   *)

  module Preemptible : sig
    (** Preemptible handlers

        Preemptible handlers are like normal handlers, except they also have
        the ability to receive "ticks" from the runtime, and can decide to
        preempt the current fiber on tick.

        To set the tick interval, call [Domain.Tick.acquire] before running a
        preemptible fiber. *)

    type ('a,'b) handler =
        { retc: 'a -> 'b;
          exnc: exn -> 'b;
          effc: 'c.'c t -> (('c,'a) continuation -> 'b) option;
          tickc: unit -> tick_outcome }
    (** [('a,'b) handler] is a handler record with four fields -- [retc]
        is the value handler, [exnc] handles exceptions, [effc] handles the
        effects performed by the computation enclosed by the handler, and
        [tickc] handles ticks, deciding whether to preempt. [tickc] should be
        signal-safe. If [tickc] returns [Preempt], a [Preemption] effect is
        performed. *)

    val continue_with : ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b
    (** [continue_with k v h] resumes the continuation [k] with value [v] within
        the handler [h].

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
    *)

    val discontinue_with : ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b
    (** [discontinue_with k e h] resumes the continuation [k] by raising the
        exception [e] within the handler [h].

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
    *)

    val discontinue_with_backtrace : ('a,'b) continuation -> exn ->
        Printexc.raw_backtrace -> ('b,'c) handler -> 'c
    (** [discontinue_with k e bt h] resumes the continuation [k] by raising the
        exception [e] with the handler [h] using the raw backtrace [bt] as the
        origin of the exception.

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
    *)
  end

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)
end
