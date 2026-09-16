type t : bits64 & float64 & void & value

val zero : t
val one : t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val to_string : t -> string
