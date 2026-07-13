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

type 'a t : value mod everything with 'a @@ contended portable

external make' : inherit_:bool -> 'a t @@ portable = "caml_dynamic_make"
external get : 'a t -> 'a or_null @ contended portable @@ portable =
  "caml_dynamic_get" [@@noalloc]
external push : 'a t -> 'a @ contended portable -> unit @@ portable =
  "caml_dynamic_push"
external pop : 'a t -> unit @@ portable = "caml_dynamic_pop" [@@noalloc]

let[@inline] make () = make' ~inherit_:false

let[@inline] with_temporarily t v ~f = exclave_
  push t v;
  match f () with
  | res -> pop t; res
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    pop t;
    Printexc.raise_with_backtrace exn bt

module Context = struct
  type nonrec 'a t = 'a t

  let[@inline] make () = make' ~inherit_:true

  let get = get
  let with_temporarily = with_temporarily
end
