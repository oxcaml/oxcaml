type t
type step = t -> unit

val spelled : t -> t -> unit
val aliased : t -> step
