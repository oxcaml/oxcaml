(* Type-directed program generator. *)

module Mode : sig
  type t =
    | Soundness
    | Completeness

  val to_string : t -> string

  val of_string : string -> t option
end

module Sample : sig
  type t =
    { fe_source : string;
      be_source : string
    }

  val to_string : t -> string
end

val generate : mode:Mode.t -> seed:int -> Sample.t
