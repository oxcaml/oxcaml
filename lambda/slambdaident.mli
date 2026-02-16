type t

include Identifiable.S with type t := t

val print : Format.formatter -> t -> unit

(** Creates a new unique identifier. *)
val create_local : string -> t

(** "Lifts" an [Ident.t], equal [Ident.t]s will produce equal values. *)
val of_ident : Ident.t -> t
