type fn

val make_fn : unit -> fn

type t

val create : int -> string -> t
val id : t -> int
val label : t -> string
val relabel : t -> string -> t
val default : t

type metrics

val create_metrics : unit -> metrics
val record : metrics -> float -> unit
val mean : metrics -> float
val summarize : metrics -> f:(float -> string) -> string list

module Nested : sig
  val double : int -> int
end
