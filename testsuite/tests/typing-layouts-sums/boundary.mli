type t : bits32 + value

val sample : t
val raw : int32_u -> t
val count : int -> t
val read : t -> int

type wrapped : void + (bits32 + value)

val wrap : t -> wrapped
val read_wrapped : wrapped -> int
