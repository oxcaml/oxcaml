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

type ('a : value_or_null) t : value mod contended portable

(* This is the same type as [Effect.t], but redefined here to avoid circular
   dependencies. *)
type 'a effect
type (-'a, +'b) cont
type last_fiber [@@immediate]

external make
  : ('a : value_or_null mod contended).
  'a @ portable -> 'a t
  @@ portable
  = "caml_dynamic_make"

external get
  : ('a : value_or_null mod contended). 'a t -> 'a @ portable
  @@ portable
  = "caml_dynamic_get"

external reperform :
  'a effect @ unique ->
  ('a, 'b) cont @ unique ->
  last_fiber ->
  'b @ unique once
  @@ portable
  = "%reperform"

external with_stack_bind :
  'a 'b 'c ('d : value_or_null mod contended) 'e.
  ('a @ unique once -> 'b @ unique once) ->
  (exn -> 'b) ->
  ('c effect @ unique ->
   ('c, 'b) cont @ unique ->
   last_fiber ->
   'b @ unique once) ->
  'd t ->
  'd @ portable ->
  ('e @ once unique -> 'a @ unique once) @ once ->
  'e ->
  'b @ once unique
  @@ portable
  = "%with_stack_bind" "%with_stack_bind"

let with_temporarily d v ~f =
  let effc eff k last_fiber = reperform eff k last_fiber in
  with_stack_bind (fun x -> x) (fun e -> raise e) effc d v (fun () -> f ()) ()
