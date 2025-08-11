type 'a t

val return : 'a -> 'a t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t
val join : 'a t t -> 'a t
val all : 'a t list -> 'a list t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

module Sys : sig
  val file_exists : string -> [ `Yes | `No | `Unknown ] t
  val realpath : string -> string
  val get_cwd : unit -> string
end

module Io : sig
  val read_file : path:string -> string t
  val create_file_write_content : path:string -> content:string -> unit t
end

module Process : sig
  val fold_lines
    :  prog:string
    -> args:string list
    -> init:'a
    -> f:('a -> string -> 'a)
    -> unit
    -> ('a, exn) Result.t t

  val run : prog:string -> args:string list -> unit -> (string, exn) Result.t t
end

val try_with : (unit -> 'a t) -> ('a, exn) Result.t t
val protect : finally:(unit -> unit t) -> (unit -> 'a t) -> 'a t
val block_on : (unit -> 'a t) -> 'a
val from_blocking : (unit -> 'a) -> 'a t
val with_temp_dir : ?template:string -> (string -> 'a t) -> 'a t
