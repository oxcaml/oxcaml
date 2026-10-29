(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

open! Stdlib

type t = int16

let size = 16

external of_int : int -> int16 @@ portable = "%int16_of_int"
external to_int : int16 -> int @@ portable = "%int_of_int16"
external ( < ) : int16 -> int16 -> bool @@ portable = "%int16_lessthan"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)

external neg : int16 -> int16 @@ portable = "%int16_neg"
external add : int16 -> int16 -> int16 @@ portable = "%int16_add"
external sub : int16 -> int16 -> int16 @@ portable = "%int16_sub"
external mul : int16 -> int16 -> int16 @@ portable = "%int16_mul"
external div : int16 -> int16 -> int16 @@ portable = "%int16_div"
external rem : int16 -> int16 -> int16 @@ portable = "%int16_mod"
external succ : int16 -> int16 @@ portable = "%int16_succ"
external pred : int16 -> int16 @@ portable = "%int16_pred"
external logand : int16 -> int16 -> int16 @@ portable = "%int16_and"
external logor : int16 -> int16 -> int16 @@ portable = "%int16_or"
external logxor : int16 -> int16 -> int16 @@ portable = "%int16_xor"

let[@inline] lognot x = logxor x minus_one

external shift_left : int16 -> int -> int16 @@ portable = "%int16_lsl"
external shift_right : int16 -> int -> int16 @@ portable = "%int16_asr"
external shift_right_logical : int16 -> int -> int16 @@ portable = "%int16_lsr"

let[@inline] abs x = if x < zero then neg x else x

external equal : int16 -> int16 -> bool @@ portable = "%int16_equal"
external compare : int16 -> int16 -> int @@ portable = "%int16_compare"

let[@inline] min x y = if x < y then x else y
let[@inline] max x y = if x < y then y else x

external of_float : float -> int16 @@ portable = "%int16_of_float"
external to_float : int16 -> float @@ portable = "%float_of_int16"

let[@inline] to_string t = Int.to_string (to_int t)

external of_string : string -> int16 @@ portable = "caml_int16_of_string"

let max_int = shift_right_logical minus_one 1
let min_int = succ max_int
let[@inline] unsigned_to_int t = to_int t land ((1 lsl size) - 1)

external unsigned_compare : int16 -> int16 -> int @@ portable
  = "%int16_unsigned_compare"

external unsigned_div : int16 -> int16 -> int16 @@ portable
  = "%int16_unsigned_div"
external unsigned_rem : int16 -> int16 -> int16 @@ portable
  = "%int16_unsigned_mod"

let seeded_hash seed x = Stdlib.Hashtbl.seeded_hash seed (to_int x)
let hash x = Stdlib.Hashtbl.hash (to_int x)
