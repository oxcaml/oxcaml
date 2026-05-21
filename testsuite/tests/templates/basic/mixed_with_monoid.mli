type t = #(float# * Monoid.t * int64_u)

val empty : t
val append : t -> t -> t
