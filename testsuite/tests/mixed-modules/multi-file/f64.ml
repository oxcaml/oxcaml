external float_of_int64 : int64 -> float = "%float_of_int64"
external f64_of_float : float -> float# = "%unbox_float"

let of_int64 i = f64_of_float (float_of_int64 i)

let zero = #0.0
