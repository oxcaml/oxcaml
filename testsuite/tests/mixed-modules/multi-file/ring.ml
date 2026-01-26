type free_ring_elt =
  | Zero
  | One
  | Negative of free_ring_elt
  | Add of free_ring_elt * free_ring_elt
  | Multiply of free_ring_elt * free_ring_elt

type void : void
type t = #(int64# * float# * void * free_ring_elt)

external void : unit -> void = "%unbox_unit"
external box_int64 : int64# -> int64 = "%box_int64"
external unbox_int64 : int64 -> int64# = "%unbox_int64"
external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"
external int_of_int64 : int64 -> int = "%int_of_int64"
external int64_of_int : int -> int64 = "%int64_of_int"

let of_int x = unbox_int64 (int64_of_int x)
let to_int x = int_of_int64 (box_int64 x)

let mul #(a0, a1, _, a2) #(b0, b1, _, b2) =
  #( of_int (to_int a0 * to_int b0)
   , unbox_float (box_float a1 *. box_float b1)
   , void ()
   , Multiply (a2, b2) )

let rec elt_to_string = function
  | Zero -> "0"
  | One -> "1"
  | Negative x -> "-" ^ elt_to_string x
  | Add (x, y) -> "(" ^ elt_to_string x ^ " + " ^ elt_to_string y ^ ")"
  | Multiply (x, y) -> elt_to_string x ^ " * " ^ elt_to_string y

let one = #(#1L, #1.0, void (), One)

let sub #(a0, a1, _, a2) #(b0, b1, _, b2) =
  #( of_int (to_int a0 - to_int b0)
   , unbox_float (box_float a1 -. box_float b1)
   , void ()
   , Add (a2, Negative b2) )

let add #(a0, a1, _, a2) #(b0, b1, _, b2) =
  #( of_int (to_int a0 + to_int b0)
   , unbox_float (box_float a1 +. box_float b1)
   , void ()
   , Add (a2, b2) )

let zero = #(#0L, #0.0, void (), Zero)

let rec elt_to_int = function
  | Zero -> 0
  | One -> 1
  | Negative x -> -elt_to_int x
  | Add (x, y) -> elt_to_int x + elt_to_int y
  | Multiply (x, y) -> elt_to_int x * elt_to_int y

let to_string #(a, b, _, c) =
  Format.sprintf "%d %.0f %d" (to_int a) (box_float b) (elt_to_int c)

let negative_one = #(-#1L, -#1.0, void (), Negative One)
let two = #(#2L, #2.0, void (), Add (One, One))
