type void : void

external void : unit -> void = "%unbox_unit"
external box_int64 : int64# -> int64 = "%box_int64"
external unbox_int64 : int64 -> int64# = "%unbox_int64"
external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"
external int_of_int64 : int64 -> int = "%int_of_int64"
external int64_of_int : int -> int64 = "%int64_of_int"

let of_int x = unbox_int64 (int64_of_int x)
let to_int x = int_of_int64 (box_int64 x)

type t = #(int64# * float# * string * void)

let empty = #(#1L, #1.0, "", void ())

let append #(a0, a1, a2, _v1) #(b0, b1, b2, _v2) =
  #( of_int (to_int a0 * to_int b0)
   , unbox_float (box_float a1 *. box_float b1)
   , a2 ^ b2
   , void () )
;;
