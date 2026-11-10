type t

val create : int -> t option
val get : t -> int

law? get_is_even (x : t) : get x mod 2 = 0
