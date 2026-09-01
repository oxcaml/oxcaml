type t

val create : file:string -> text:string -> t

val load : Location.t -> t

val holds : t -> Location.t -> bool

val length : t -> int

val sub : t -> pos:int -> len:int -> string
