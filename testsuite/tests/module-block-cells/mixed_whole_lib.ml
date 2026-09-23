(* A unit with a mixed-representation module block: unboxed fields ([x], the
   dynamic [w] and the product [p]) among value fields. *)

external unbox_float : float -> float# = "%unbox_float"
external box_float : float# -> float = "%box_float"

let x = #2.5
let y = Sys.opaque_identity 7
let w = unbox_float (Sys.opaque_identity 4.0)
let p = #(#1.5, Sys.opaque_identity 3)
let f n = box_float x *. float_of_int n
