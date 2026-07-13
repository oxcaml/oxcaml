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

type 'a t = 'a eff = ..
external perform : 'a t -> 'a = "%perform"
exception Out_of_fibers = Out_of_fibers
type exn += Unhandled: 'a t -> exn
exception Continuation_already_resumed

let () =
  let printer = function
    | Unhandled x ->
        let msg = Printf.sprintf "Stdlib.Effect.Unhandled(%s)"
            (Printexc.string_of_extension_constructor @@ Obj.repr x)
        in
        Some msg
    | _ -> None
  in
  (* need magic because jkind doesn't know [t] crosses portability and
    contention  *)
  Printexc.Safe.register_printer (Obj.magic_portable printer)

(* Register the exceptions so that the runtime can access it *)
type _ t += Should_not_see_this__ : unit t
let _ = Callback.Safe.register_exception "Effect.Unhandled"
          (Unhandled Should_not_see_this__)
let _ = Callback.Safe.register_exception "Effect.Continuation_already_resumed"
          Continuation_already_resumed

(* A paused fiber, awaiting an 'a, which terminates with an 'x,
   equipped with a handler that produces a 'b *)
type (-'a, 'x, +'b) cont : value mod non_float

(* A last_fiber is a tagged pointer, so does not keep the fiber alive.
   It must never be the sole reference to the fiber, and is only used to cache
   the final fiber in the linked list formed by [cont.fiber->parent]. *)
type last_fiber [@@immediate]

type ('a,'x,'b) effc = 'a t -> ('a, 'x, 'b) cont -> last_fiber -> 'b

type tick_outcome =
  | Preempt
  | Continue

module Prim = struct
  external register_named_value : string -> 'a -> unit
    = "caml_register_named_value"

  external cont_set_last_fiber :
    _ cont -> last_fiber -> unit = "%setfield1"

  external continue : ('a, _, 'b) cont -> 'a -> 'b = "%continue"
  external discontinue : ('a, _, 'b) cont -> exn -> 'b = "%discontinue"
  external discontinue_with_backtrace :
    ('a, _, 'b) cont -> exn -> Printexc.raw_backtrace -> 'b
    = "%discontinue_with_backtrace"

  external reperform :
    'a t -> ('a, 'x, 'b) cont -> last_fiber -> 'c = "%reperform"

  external with_stack :
    ('x -> 'b) ->
    (exn -> 'b) ->
    ('a . ('a,'x,'b) effc) ->
    ('d -> 'x) ->
    'd ->
    'b = "%with_stack"

  external with_stack_preemptible :
    ('x -> 'b) ->
    (exn -> 'b) ->
    ('a . ('a,'x,'b) effc) ->
    (unit -> tick_outcome) ->
    ('d -> 'x) ->
    'd ->
    'b = "%with_stack_preemptible"

  external update_cont_handler_noexc :
    ('a, 'x, _) cont ->
    ('x -> 'b) ->
    (exn -> 'b) ->
    ('a2 . ('a2, 'x, 'b) effc) ->
    (unit -> tick_outcome) or_null ->
    ('a, 'x, 'b) cont = "caml_continuation_update_handler_noexc"
end

type _ t += Preemption : unit t
let () = Prim.register_named_value "Effect.Preemption" Preemption

(* Retrieve the stack from a [cont]inuation, update its handlers, and resume it.

   FIXME: There's a race condition here - if multiple threads call one of these
   on the same continuation at once with handlers that return different types,
   they could be interleaved, causing a segfault rather than an exception. *)

let continue_with_handler cont valuec exnc (effc : 'a. ('a, _, _) effc) tickc
    v =
  Prim.continue (Prim.update_cont_handler_noexc cont valuec exnc effc tickc) v

let discontinue_with_handler cont valuec exnc (effc : 'a. ('a, _, _) effc) tickc
    e =
  Prim.discontinue
    (Prim.update_cont_handler_noexc cont valuec exnc effc tickc)
    e

let discontinue_with_handler_with_backtrace cont valuec exnc
    (effc : 'a. ('a, _, _) effc) tickc e bt =
  Prim.discontinue_with_backtrace
    (Prim.update_cont_handler_noexc cont valuec exnc effc tickc)
    e bt

module Deep = struct

  type nonrec ('a,'b) continuation = ('a,'b) continuation

  type ('a,'b) continuation_ =
    | Cont : ('a,'x,'b) cont -> ('a, 'b) continuation_ [@@unboxed]

  let[@inline] continue (Cont k) v = Prim.continue k v

  let[@inline] discontinue (Cont k) e = Prim.discontinue k e

  let[@inline] discontinue_with_backtrace (Cont k) e bt =
    Prim.discontinue_with_backtrace k e bt

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }

  (* FIXME Upstream the 3-parameter version of continuation and use it to
           maintain type safety here. *)
  let[@inline] to_continuation (f : _ continuation -> 'a) (k : _ continuation_)
      =
    f (Obj.magic k)

  let[@inline] of_continuation (f : _ continuation_ -> 'a) (k : _ continuation)
      =
    f (Obj.magic k)

  let match_with comp arg handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f -> to_continuation f (Cont k)
      | None -> Prim.reperform eff k last_fiber
    in
    Prim.with_stack handler.retc handler.exnc effc comp arg

  type 'a effect_handler =
    { effc: 'b. 'b t -> (('b,'a) continuation -> 'a) option }

  let try_with comp arg handler =
    let effc' eff k last_fiber =
      match handler.effc eff with
      | Some f -> to_continuation f (Cont k)
      | None -> Prim.reperform eff k last_fiber
    in
    Prim.with_stack (fun x -> x) (fun e -> raise e) effc' comp arg

  let[@inline] continue k = of_continuation continue k
  let[@inline] discontinue k = of_continuation discontinue k
  let[@inline] discontinue_with_backtrace k =
    of_continuation discontinue_with_backtrace k

  module Preemptible = struct
    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c t -> (('c,'b) continuation -> 'b) option;
        tickc: unit -> tick_outcome }

    let match_with comp arg handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f ->
          Prim.cont_set_last_fiber k last_fiber;
          to_continuation f (Cont k)
        | None -> Prim.reperform eff k last_fiber
      in
      Prim.with_stack_preemptible
        handler.retc handler.exnc effc handler.tickc
        comp arg

    let try_with ~on_tick comp arg (handler : _ effect_handler) =
      match_with comp arg
        { retc = Fun.id
        ; exnc = raise
        ; effc = handler.effc
        ; tickc = on_tick
        };
    ;;
  end

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end

module Shallow = struct

  type ('a,'b) continuation =
    | Cont : ('a,'b,'x) cont -> ('a,'b) continuation [@@unboxed]

  let fiber : type a b. (a -> b) -> (a, b) continuation = fun f ->
    let module M = struct type _ t += Initial_setup__ : a t end in
    let exception E of (a,b) continuation in
    let f' () = f (perform M.Initial_setup__) in
    let error _ = failwith "impossible" in
    let effc (type a2) (eff : a2 t) (k : (a2,b,_) cont) _last_fiber =
      match eff with
      | M.Initial_setup__ -> raise_notrace (E (Cont k))
      (* We need to handle [Preemption] here since it's triggered automatically
         on a timer, and might arrive while we're setting up the fiber *)
      | Preemption ->
          Prim.continue k ()
      | _ -> error ()
    in
    match Prim.with_stack error error effc f' () with
    | exception E k -> k
    | _ -> error ()

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }

  let continue_with (Cont k) v handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f -> f (Cont k)
      | None -> Prim.reperform eff k last_fiber
    in
    continue_with_handler k handler.retc handler.exnc effc Null v

  let discontinue_with (Cont k) e handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f -> f (Cont k)
      | None -> Prim.reperform eff k last_fiber
    in
    discontinue_with_handler k handler.retc handler.exnc effc Null e

  let discontinue_with_backtrace (Cont k) e bt handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f -> f (Cont k)
      | None -> Prim.reperform eff k last_fiber
    in
    discontinue_with_handler_with_backtrace k handler.retc handler.exnc effc
      Null e bt

  module Preemptible = struct
    type ('a,'b) handler =
        { retc: 'a -> 'b;
          exnc: exn -> 'b;
          effc: 'c.'c t -> (('c,'a) continuation -> 'b) option;
          tickc: unit -> tick_outcome }

    let continue_with (Cont k) v handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f ->
          Prim.cont_set_last_fiber k last_fiber;
          f (Cont k)
        | None -> Prim.reperform eff k last_fiber
      in
      continue_with_handler k handler.retc handler.exnc effc
        (This handler.tickc) v

    let discontinue_with (Cont k) e handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f ->
          Prim.cont_set_last_fiber k last_fiber;
          f (Cont k)
        | None -> Prim.reperform eff k last_fiber
      in
      discontinue_with_handler k handler.retc handler.exnc effc
        (This handler.tickc) e

    let discontinue_with_backtrace (Cont k) e bt handler =
      let effc eff k last_fiber =
        match handler.effc eff with
        | Some f ->
          Prim.cont_set_last_fiber k last_fiber;
          f (Cont k)
        | None -> Prim.reperform eff k last_fiber
      in
      discontinue_with_handler_with_backtrace k handler.retc handler.exnc effc
        (This handler.tickc) e bt
  end

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end
