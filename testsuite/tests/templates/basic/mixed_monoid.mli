type void : void
type t = #(int64_u * float# * string * void)

val empty : t
val append : t -> t -> t
val void : unit -> void
