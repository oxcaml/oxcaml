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
[@@alert unsafe_effects "Use [Effect.Safe.perform]. [Effect.perform] may not \
                         function correctly on Js_of_ocaml"]
(** [perform e] performs an effect [e].

    @raise Unhandled if there is no handler for [e]. *)

module Handler : sig
  (** A value of type [Effect.Handler.t] at mode [local] is proof that the
      current function is running within an effect handler, and hence may
      perform effects.

      It is also necessary to ensure that effect handlers are compiled correctly
      on the OxCaml branch of the Js_of_ocaml compiler *)
  type t : void mod external_ many stateless immutable
end

module Safe : sig
  (** OxCaml-compatible version of [perform]. Takes a [Handler.t @ local] to
      prove that the current function is running in an effect handler. *)
  val perform : Handler.t @ local -> 'a t -> 'a
end

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

  (** OxCaml versions of [Effect.Deep]. The computation passed to [match_with]
      and [try_with] receives a [Handler.t @ local], allowing it to perform
      effects. *)
  module Safe : sig
    val match_with
      :  (Handler.t @ local -> 'c -> 'a) @ unyielding
      -> 'c
      -> ('a,'b) handler
      -> 'b

    val try_with
      :  (Handler.t @ local -> 'b -> 'a) @ unyielding
      -> 'b
      -> 'a effect_handler
      -> 'a

    (** Like {!Deep.Safe}, but allow threading [Handler.t @ local] for the
        parent stack to the handler callbacks *)
    module With_handler : sig
      type ('a,'b) handler =
        { retc: Handler.t @ local -> 'a -> 'b;
          exnc: Handler.t @ local -> exn -> 'b;
          effc: 'c. Handler.t @ local -> 'c t
                -> (('c,'b) continuation -> 'b) option @ local }
      (** Like {!Deep.handler}, but each callback receives a {!Handler.t} token,
          allowing them to perform effects. *)

      type 'a effect_handler =
        { effc: 'b. Handler.t @ local -> 'b t
                -> (('b,'a) continuation -> 'a) option @ local }
      (** Like {!Deep.effect_handler}, but [effc] receives a {!Handler.t} token,
          allowing it to perform effects. *)

      val match_with
        :  Handler.t @ local
        -> (Handler.t @ local -> 'c -> 'a) @ unyielding
        -> 'c
        -> ('a,'b) handler
        -> 'b

      val try_with
        :  Handler.t @ local
        -> (Handler.t @ local -> 'b -> 'a) @ unyielding
        -> 'b
        -> 'a effect_handler
        -> 'a
    end
  end


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

    (** OxCaml versions of [Effect.Deep.Preemptible]. *)
    module Safe : sig
      val match_with
        :  (Handler.t @ local -> 'c -> 'a) @ unyielding
        -> 'c
        -> ('a,'b) handler
        -> 'b

      val try_with
        :  on_tick:(unit -> tick_outcome)
        -> (Handler.t @ local -> 'b -> 'a) @ unyielding
        -> 'b
        -> 'a effect_handler
        -> 'a

      (** Like {!Deep.Preemptible.Safe}, but allow threading [Handler.t @ local]
          for the parent stack to the handler callbacks *)
      module With_handler : sig
        type ('a,'b) handler =
            { retc: Handler.t @ local -> 'a -> 'b;
              exnc: Handler.t @ local -> exn -> 'b;
              effc: 'c. Handler.t @ local -> 'c t
                    -> (('c,'b) continuation -> 'b) option @ local;
              tickc: unit -> tick_outcome }
        (** Like {!Deep.Preemptible.handler}, but [retc], [exnc] and [effc] each
            receive a {!Handler.t} token, allowing them to perform effects.
            [tickc] does not, since it must be signal-safe. *)

        val match_with
          :  Handler.t @ local
          -> (Handler.t @ local -> 'c -> 'a) @ unyielding
          -> 'c
          -> ('a,'b) handler
          -> 'b

        val try_with
          :  Handler.t @ local
          -> on_tick:(unit -> tick_outcome)
          -> (Handler.t @ local -> 'b -> 'a) @ unyielding
          -> 'b
          -> 'a Safe.With_handler.effect_handler
          -> 'a
      end
    end
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

  module Safe : sig
    (** OxCaml version of [fiber], which provides a [Handler.t @ local
        unyielding] to the computation to run. *)
    val fiber
      : (Handler.t @ local -> 'a -> 'b) @ unyielding -> ('a, 'b) continuation

    (** Like {!Shallow}, but allow threading [Handler.t @ local]
        for the parent stack to the handler callbacks *)
    module With_handler : sig
      type ('a,'b) handler =
        { retc: Handler.t @ local -> 'a -> 'b;
          exnc: Handler.t @ local -> exn -> 'b;
          effc: 'c. Handler.t @ local -> 'c t
                -> (('c,'a) continuation -> 'b) option @ local }
      (** Like {!Shallow.handler}, but each callback receives a {!Handler.t}
          token, allowing them to perform effects. *)

      val continue_with
        :  Handler.t @ local
        -> ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b

      val discontinue_with
        :  Handler.t @ local
        -> ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b

      val discontinue_with_backtrace
        :  Handler.t @ local
        -> ('a,'b) continuation -> exn -> Printexc.raw_backtrace
        -> ('b,'c) handler -> 'c
    end
  end

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

    module Safe : sig

      (** Like {!Shallow.Preemptible}, but allow threading [Handler.t @ local]
          for the parent stack to the handler callbacks *)
      module With_handler : sig
        type ('a,'b) handler =
            { retc: Handler.t @ local -> 'a -> 'b;
              exnc: Handler.t @ local -> exn -> 'b;
              effc: 'c. Handler.t @ local -> 'c t
                    -> (('c,'a) continuation -> 'b) option @ local;
              tickc: unit -> tick_outcome }
        (** Like {!Shallow.Preemptible.handler}, but [retc], [exnc] and [effc]
            each receive a {!Handler.t} token, allowing them to perform effects.
            [tickc] does not, since it must be signal-safe. *)

        val continue_with
          :  Handler.t @ local
          -> ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b
        (** [continue_with h k v handler] resumes the continuation [k] with
            value [v] within the handler [handler].

            @raise Continuation_already_resumed if the continuation has already
            been resumed.
        *)

        val discontinue_with
          :  Handler.t @ local
          -> ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b
        (** [discontinue_with h k e handler] resumes the continuation [k] by
            raising the exception [e] within the handler [handler].

            @raise Continuation_already_resumed if the continuation has already
            been resumed.
        *)

        val discontinue_with_backtrace
          :  Handler.t @ local
          -> ('a,'b) continuation -> exn -> Printexc.raw_backtrace
          -> ('b,'c) handler -> 'c
        (** [discontinue_with_backtrace h k e bt handler] resumes the
            continuation [k] by raising the exception [e] within the handler
            [handler] using the raw backtrace [bt] as the origin of the
            exception.

            @raise Continuation_already_resumed if the continuation has already
            been resumed.
        *)
      end
    end
  end

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)
end
