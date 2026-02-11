type t

include Identifiable.S with type t := t

val print : Format.formatter -> t -> unit

val create_local : string -> t

val of_ident : Ident.t -> t
