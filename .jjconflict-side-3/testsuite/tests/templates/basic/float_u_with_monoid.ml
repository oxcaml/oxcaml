external box : float# -> float = "%float_of_float#"
external unbox : float -> float# = "%float#_of_float"

type t = #(float# * Monoid.t)

let empty = #(#0.0, Monoid.empty)
let append #(a, b) #(c, d) = #(unbox (box a *. box c), Monoid.append b d)
