(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

open! Stdlib

type t = int

let size = Sys.int_size

external ( < ) : int -> int -> bool = "%int_lessthan"

let zero = 0
let one = 1
let minus_one = -1

external to_int : int -> int = "%identity"
external of_int : int -> int = "%identity"
external neg : int -> int = "%int_neg"
external add : int -> int -> int = "%int_add"
external sub : int -> int -> int = "%int_sub"
external mul : int -> int -> int = "%int_mul"
external div : int -> int -> int = "%int_div"
external rem : int -> int -> int = "%int_mod"
external succ : int -> int = "%int_succ"
external pred : int -> int = "%int_pred"
external logand : int -> int -> int = "%int_and"
external logor : int -> int -> int = "%int_or"
external logxor : int -> int -> int = "%int_xor"

let[@inline] lognot x = logxor x minus_one

external shift_left : int -> int -> int = "%int_lsl"
external shift_right : int -> int -> int = "%int_asr"
external shift_right_logical : int -> int -> int = "%int_lsr"

let[@inline] abs x = if x < zero then neg x else x

external equal : int -> int -> bool = "%int_equal"
external compare : int -> int -> int = "%int_compare"

let[@inline] min x y = if x < y then x else y
let[@inline] max x y = if x < y then y else x

external of_float : float -> int = "%int_of_float"
external to_float : int -> float = "%float_of_int"
external format_int : string -> int -> string = "caml_format_int"

let[@inline] to_string t = format_int "%d" t

external of_string : string -> int = "caml_int_of_string"

let max_int = shift_right_logical minus_one 1
let min_int = succ max_int
let[@inline] unsigned_to_int t = if t < 0 then None else Some t

external unsigned_compare : int -> int -> int = "%int_unsigned_compare"
external unsigned_lt : int -> int -> bool = "%int_unsigned_lessthan"

(* Unsigned division from signed division of the same bitness. See Warren Jr.,
   Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3. *)
let[@inline] unsigned_div n d =
  if d < zero then if unsigned_lt n d then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline] unsigned_rem n d = sub n (mul (unsigned_div n d) d)
let seeded_hash seed t = Stdlib.Hashtbl.seeded_hash seed t
let hash t = Stdlib.Hashtbl.hash t
