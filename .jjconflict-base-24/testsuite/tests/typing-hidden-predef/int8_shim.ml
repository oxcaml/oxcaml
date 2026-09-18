type t = int8
type t_unboxed = int8#

let zero = 0s
let one = 1s
let zero_unboxed = #0s
let one_unboxed = #1s

external box : t_unboxed -> t = "%int8_of_int8#"
external unbox : t -> t_unboxed = "%int8#_of_int8"

external add : t -> t -> t = "%int8_add"
external add_unboxed : t_unboxed -> t_unboxed -> t_unboxed = "%int8#_add"

external to_int : t -> int = "%int_of_int8"

let to_string t = Int.to_string (to_int t)
