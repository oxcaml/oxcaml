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

type 'a t = ..
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

external cont_set_last_fiber :
  _ cont -> last_fiber -> unit = "%setfield1"

external resume : ('a, _, 'b) cont -> ('c -> 'a) -> 'c -> 'b = "%resume"

external with_stack :
  ('x -> 'b) ->
  (exn -> 'b) ->
  ('a t -> ('a, 'x, 'b) cont -> last_fiber -> 'b) ->
  ('d -> 'x) ->
  'd ->
  'b = "%with_stack"

external update_cont_handler_noexc :
  ('a, 'x, _) cont ->
  ('x -> 'b) ->
  (exn -> 'b) ->
  ('a2 t -> ('a2, 'x, 'b) cont -> last_fiber -> 'b) ->
  ('a, 'x, 'b) cont = "caml_continuation_update_handler_noexc"

(* Retrieve the stack from a [cont]inuation, update its handlers, and run
   [f x] using it. *)
let with_handler cont valuec exnc effc f x =
  resume
    (* FIXME: There's a race condition here - if multiple threads call
       [with_handler] on the same continuation at once, they could be
       interleaved, causing a segfault rather
       than an exception. *)
    (update_cont_handler_noexc cont valuec exnc effc) f x

module Deep = struct

  type ('a,'b) continuation =
    | Cont : ('a,'x,'b) cont -> ('a, 'b) continuation [@@unboxed]

  let continue (Cont k) v = resume k (fun x-> x) v

  let discontinue (Cont k) e = resume k (fun e -> raise e) e

  let discontinue_with_backtrace (Cont k) e bt =
    resume k (fun e -> Printexc.raise_with_backtrace e bt) e

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }

  external reperform :
    'a t -> ('a, _, 'b) cont -> last_fiber -> 'b = "%reperform"

  let match_with comp arg handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f (Cont k)
      | None -> reperform eff k last_fiber
    in
    with_stack handler.retc handler.exnc effc comp arg

  type 'a effect_handler =
    { effc: 'b. 'b t -> (('b,'a) continuation -> 'a) option }

  let try_with comp arg handler =
    let effc' eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f (Cont k)
      | None -> reperform eff k last_fiber
    in
    with_stack (fun x -> x) (fun e -> raise e) effc' comp arg

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
    let effc eff k last_fiber =
      match eff with
      | M.Initial_setup__ ->
          cont_set_last_fiber k last_fiber;
          raise_notrace (E (Cont k))
      | _ -> error ()
    in
    match with_stack error error effc f' () with
    | exception E k -> k
    | _ -> error ()

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }

  external reperform :
    'a t -> ('a, 'b, _) cont -> last_fiber -> 'c = "%reperform"

  let continue_gen (Cont k) resume_fun v handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f (Cont k)
      | None -> reperform eff k last_fiber
    in
    with_handler k handler.retc handler.exnc effc resume_fun v

  let continue_with k v handler =
    continue_gen k (fun x -> x) v handler

  let discontinue_with k v handler =
    continue_gen k (fun e -> raise e) v handler

  let discontinue_with_backtrace k v bt handler =
    continue_gen k (fun e -> Printexc.raise_with_backtrace e bt) v handler

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end
