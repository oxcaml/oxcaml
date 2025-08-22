type t = int64#

external box : int64# -> int64 = "%box_int64"
external unbox : int64 -> int64# = "%unbox_int64"
external int_of_int64 : int64 -> int = "%int_of_int64"
external int64_of_int : int -> int64 = "%int64_of_int"

let zero = #0L
let one = #1L

let of_int a = int64_of_int a |> unbox
let to_int a = int_of_int64 a |> box

let add a b = of_int (to_int a + to_int b)
let mul a b = of_int (to_int a * to_int b)
