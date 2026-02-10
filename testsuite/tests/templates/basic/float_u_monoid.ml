type t = float#

external box : float# -> float = "%float_of_float#"
external unbox : float -> float# = "%float#_of_float"

let empty = #1.0
let append a b = unbox (box a *. box b)
