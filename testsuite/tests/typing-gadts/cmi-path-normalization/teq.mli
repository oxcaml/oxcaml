type (_, _) t = Refl : ('a, 'a) t
type 'a id

val same_exn : 'a id -> 'b id -> ('a, 'b) t
