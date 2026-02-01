type t

include Identifiable.S with type t := t

val print : Format.formatter -> t -> unit

val of_ident : Ident.t -> t

val of_string : string -> t
