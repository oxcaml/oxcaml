(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Gabriel Scherer, projet Parsifal, INRIA Saclay                 *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type ('a : any, 'b : any) t = Left of 'a | Right of 'b

let left v = Left v
let right v = Right v

(* CR-someday lmaurer: It's reasonable (though not trivial to implement) to
   allow matching a value of layout [any] with a blank (since it doesn't
   actually bind anything). This would allow quite a few of these to have more
   layout-polymorphic types. *)

let is_left = function
| Left _ -> true
| Right _ -> false

let is_right = function
| Left _ -> false
| Right _ -> true

let get_left = function
| Left v -> v
| _ -> invalid_arg "Either.t is Right _"

let get_right = function
| Right v -> v
| _ -> invalid_arg "Either.t is Left _"

let find_left = function
| Left v -> Some v
| _ -> None

let find_right = function
| Right v -> Some v
| _ -> None

let map_left f = function
| Left v -> Left (f v)
| Right _ as e -> e

let map_right f = function
| Left _ as e -> e
| Right v -> Right (f v)

let map ~left ~right = function
| Left v -> Left (left v)
| Right v -> Right (right v)

let fold ~left ~right = function
| Left v -> left v
| Right v -> right v

let retract = function
| Left v -> v
| Right v -> v

let iter = fold

let for_all = fold

let equal ~left ~right e1 e2 = match e1, e2 with
| Left v1, Left v2 -> left v1 v2
| Right v1, Right v2 -> right v1 v2
| Left _, Right _ | Right _, Left _ -> false

let compare ~left ~right e1 e2 = match e1, e2 with
| Left v1, Left v2 -> left v1 v2
| Right v1, Right v2 -> right v1 v2
| Left _, Right _ -> (-1)
| Right _, Left _ -> 1
