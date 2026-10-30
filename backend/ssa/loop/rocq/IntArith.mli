val add_fits : int -> int -> bool

val mul_fits : int -> int -> bool

val add_ovf : int -> int -> int option

val mul_ovf : int -> int -> int option

val neg_ovf : int -> int option

val add_fits64 : int64 -> int64 -> bool

val mul_fits64 : int64 -> int64 -> bool

val add_ovf64 : int64 -> int64 -> int64 option

val mul_ovf64 : int64 -> int64 -> int64 option

val int64_is_int : int64 -> bool

val to_int_ovf : int64 -> int option

val gcd64 : int64 -> int64 -> int64

val floor_div64 : int64 -> int64 -> int64
