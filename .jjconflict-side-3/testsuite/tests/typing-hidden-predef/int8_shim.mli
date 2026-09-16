type t = int8
type t_unboxed = int8#

val zero : t
val one : t
val zero_unboxed : t_unboxed
val one_unboxed : t_unboxed

val box : t_unboxed -> t
val unbox : t -> t_unboxed

val add : t -> t -> t
val add_unboxed : t_unboxed -> t_unboxed -> t_unboxed

val to_string : t -> string
