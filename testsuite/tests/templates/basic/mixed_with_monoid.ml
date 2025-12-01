external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"
external box_int64 : int64# -> int64 = "%box_int64"
external unbox_int64 : int64 -> int64# = "%unbox_int64"
external int_of_int64 : int64 -> int = "%int_of_int64"
external int64_of_int : int -> int64 = "%int64_of_int"

let of_int x = unbox_int64 (int64_of_int x)
let to_int x = int_of_int64 (box_int64 x)

type t = #(float# * Monoid.t * int64#)

let empty = #(#0.0, Monoid.empty, #0L)

let append #(a, b, c) #(d, e, f) =
  #( unbox_float (box_float a *. box_float d)
   , Monoid.append b e
   , of_int (to_int c * to_int f) )
;;
