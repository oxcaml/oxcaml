module Suspect : sig
  type t =
    { seed : int;
      sample : Gen.Sample.t;
      backend_error : string
    }

  val to_string : t -> string
end

module Gap : sig
  type t =
    { seed : int;
      cause : string;
      sample : Gen.Sample.t
    }
end

module Stats : sig
  type t

  val agree_noalloc : t -> int

  val gen_errors : t -> int

  (* Both lists are in generation order. *)
  val suspects : t -> Suspect.t list

  val gaps : t -> Gap.t list
end

val run :
  compiler:string ->
  count:int ->
  seed0:int ->
  mode:Gen.Mode.t ->
  max_decls:int ->
  Stats.t

val report : Stats.t -> unit

val save : Stats.t -> dir:string -> unit
