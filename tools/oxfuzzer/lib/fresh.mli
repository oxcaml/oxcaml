type t

val create : prefix:string -> t

val next : t -> Ir.Name.t

val count : t -> int
