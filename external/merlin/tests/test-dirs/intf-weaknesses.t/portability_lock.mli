type 'a t
type 'a u = (string * 'a) list

val create : unit -> 'a t
val register : 'a t -> name:string -> 'a -> (unit, string) result
val find : 'a t -> string -> 'a option
val iter : 'a t -> f:('a -> unit) -> unit
