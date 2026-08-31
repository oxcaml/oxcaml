module Safe : sig
  type t = Yojson.Safe.t

  val from_string : ?fname:string -> ?lnum:int -> string -> (t, string) result

  val from_channel :
    ?fname:string -> ?lnum:int -> in_channel -> (t, string) result

  val from_file : ?fname:string -> ?lnum:int -> string -> (t, string) result

  val to_string :
    ?buf:Buffer.t -> ?len:int -> ?suf:string -> ?std:bool -> t -> string

  val to_channel :
    ?buf:Stdlib.Buffer.t ->
    ?len:int ->
    ?suf:string ->
    ?std:bool ->
    Stdlib.out_channel ->
    t ->
    unit

  val to_output :
    ?buf:Stdlib.Buffer.t ->
    ?len:int ->
    ?suf:string ->
    ?std:bool ->
    < output : string -> int -> int -> int > ->
    t ->
    unit

  val to_file : ?len:int -> ?std:bool -> ?suf:string -> string -> t -> unit
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Basic : sig
  type t = Yojson.Basic.t

  val from_string : ?fname:string -> ?lnum:int -> string -> (t, string) result

  val from_channel :
    ?fname:string -> ?lnum:int -> in_channel -> (t, string) result

  val from_file : ?fname:string -> ?lnum:int -> string -> (t, string) result

  val to_string :
    ?buf:Buffer.t -> ?len:int -> ?suf:string -> ?std:bool -> t -> string

  val to_channel :
    ?buf:Stdlib.Buffer.t ->
    ?len:int ->
    ?suf:string ->
    ?std:bool ->
    Stdlib.out_channel ->
    t ->
    unit

  val to_output :
    ?buf:Stdlib.Buffer.t ->
    ?len:int ->
    ?suf:string ->
    ?std:bool ->
    < output : string -> int -> int -> int > ->
    t ->
    unit

  val to_file : ?len:int -> ?std:bool -> ?suf:string -> string -> t -> unit
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end
