type t

val create : unit -> t

include Container_types.S with type t := t
