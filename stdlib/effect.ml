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

type (-'a, +'b) cont

(* A last_fiber is a tagged pointer, so does not keep the fiber alive.
   It must never be the sole reference to the fiber, and is only used to cache
   the final fiber in the linked list formed by [cont.fiber->parent]. *)
type last_fiber [@@immediate]

external cont_set_last_fiber :
  ('a, 'b) cont -> last_fiber -> unit = "%setfield1"

external resume : ('a, 'b) cont -> ('c -> 'a) -> 'c -> 'b = "%resume"

external with_stack :
  ('a -> 'b) ->
  (exn -> 'b) ->
  ('c t -> ('c, 'b) cont -> last_fiber -> 'b) ->
  ('d -> 'a) ->
  'd ->
  'b = "%with_stack"

external update_cont_handler_noexc :
  ('a,'b) cont ->
  ('b -> 'c) ->
  (exn -> 'c) ->
  ('d t -> ('d,'b) cont -> last_fiber -> 'c) ->
  ('a,'c) cont = "caml_continuation_update_handler_noexc"

(* Retrieve the stack from a [cont]inuation, update its handlers, and run
   [f x] using it. *)
let with_handler cont valuec exnc effc f x =
  resume (update_cont_handler_noexc cont valuec exnc effc) f x

module Deep = struct

  type ('a,'b) continuation = ('a,'b) cont

  let continue k v = resume k (fun x-> x) v

  let discontinue k e = resume k (fun e -> raise e) e

  let discontinue_with_backtrace k e bt =
    resume k (fun e -> Printexc.raise_with_backtrace e bt) e

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }

  external reperform :
    'a t -> ('a, 'b) continuation -> last_fiber -> 'b = "%reperform"

  let match_with comp arg handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f k
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
          f k
      | None -> reperform eff k last_fiber
    in
    with_stack (fun x -> x) (fun e -> raise e) effc' comp arg

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
end

module Shallow = struct

  type ('a,'b) continuation = ('a,'b) cont

  let fiber : type a b. (a -> b) -> (a, b) continuation = fun f ->
    let module M = struct type _ t += Initial_setup__ : a t end in
    let exception E of (a,b) continuation in
    let f' () = f (perform M.Initial_setup__) in
    let error _ = failwith "impossible" in
    let effc eff k last_fiber =
      match eff with
      | M.Initial_setup__ ->
          cont_set_last_fiber k last_fiber;
          raise_notrace (E k)
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
    'a t -> ('a, 'b) continuation -> last_fiber -> 'c = "%reperform"

  let continue_gen k resume_fun v handler =
    let effc eff k last_fiber =
      match handler.effc eff with
      | Some f ->
          cont_set_last_fiber k last_fiber;
          f k
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
