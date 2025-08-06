
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

type t = int#

let size = Sys.int_size

external of_int : int -> int# = "%int#_of_int"

external to_int : int# -> int = "%int_of_int#"

external ( < ) : int# -> int# -> bool = "%int#_lessthan"

let zero () = of_int 0

let one () = of_int 1

let minus_one () = of_int (-1)

external neg : int# -> int# = "%int#_neg"

external add : int# -> int# -> int# = "%int#_add"

external sub : int# -> int# -> int# = "%int#_sub"

external mul : int# -> int# -> int# = "%int#_mul"

external div : int# -> int# -> int# = "%int#_div"

external rem : int# -> int# -> int# = "%int#_mod"

external succ : int# -> int# = "%int#_succ"

external pred : int# -> int# = "%int#_pred"

external logand : int# -> int# -> int# = "%int#_and"

external logor : int# -> int# -> int# = "%int#_or"

external logxor : int# -> int# -> int# = "%int#_xor"

let[@inline] lognot x = logxor x (minus_one ())

external shift_left : int# -> int -> int# = "%int#_lsl"

external shift_right : int# -> int -> int# = "%int#_asr"

external shift_right_logical : int# -> int -> int# = "%int#_lsr"

let[@inline] abs x = if x < zero () then neg x else x

external equal : int# -> int# -> bool = "%int#_equal"

external compare : int# -> int# -> int = "%int#_compare"

let[@inline] min x y = if x < y then x else y

let[@inline] max x y = if x < y then y else x

external of_float : float -> int# = "%int#_of_float"

external to_float : int# -> float = "%float_of_int#"

let[@inline] to_string t = string_of_int (to_int t)

let[@inline] of_string s = of_int (int_of_string s)

let max_int () = shift_right_logical (minus_one ()) 1

let min_int () = succ (max_int ())

let[@inline] unsigned_to_int t =
  if t < zero () then None else Some (to_int t)

let[@inline] unsigned_compare n m =
  compare (sub n (min_int ())) (sub m (min_int ()))

let[@inline] unsigned_lt n m = sub n (min_int ()) < sub m (min_int ())

(* Unsigned division from signed division of the same bitness. See Warren Jr.,
   Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3. *)
let[@inline] unsigned_div n d =
  if d < zero ()
  then if unsigned_lt n d then zero () else one ()
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline] unsigned_rem n d = sub n (mul (unsigned_div n d) d)

let seeded_hash seed x = Stdlib.Hashtbl.seeded_hash seed (to_int x)

let hash x = Stdlib.Hashtbl.hash (to_int x)
