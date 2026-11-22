type t : void + (bits32 & value & void) + void + value

val empty : unit -> t
val mixed : int32_u -> string ref -> t
val absent : unit -> t
val narrow : string ref -> t
val inspect : t -> int32 * string option
