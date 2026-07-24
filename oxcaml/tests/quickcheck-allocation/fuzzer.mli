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

module Prelude_reject : sig
  type t =
    { seed : int;
      stage : Oracle.Outcome.gate_stage;
      cause : string;
      sample : Gen.Sample.t
    }

  val stage_to_string : Oracle.Outcome.gate_stage -> string
end

module Stats : sig
  type t

  val agree_noalloc : t -> int

  val gen_errors : t -> int

  (* All lists are in generation order. *)
  val agrees : t -> (int * Gen.Sample.t) list

  val suspects : t -> Suspect.t list

  val gaps : t -> Gap.t list

  val prelude_rejects : t -> Prelude_reject.t list
end

val run :
  compiler:string ->
  count:int ->
  seed0:int ->
  mode:Gen.Mode.t ->
  max_decls:int ->
  allow_assume:bool ->
  Stats.t

val report : Stats.t -> unit

val save : Stats.t -> dir:string -> unit
