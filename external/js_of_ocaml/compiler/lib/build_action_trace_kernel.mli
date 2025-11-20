(** A low-level and zero-dependency API for writing Dune trace events from build actions.
    Among other things, this API can be used to instrument ppxes. *)

val enabled : unit -> bool
(** Is action tracing enabled? You can use this to skip paying some initialization costs
    related to collecting and emitting action trace events. *)

module Event : sig
  module Json : sig
    type t =
      [ `Null
      | `False
      | `True
      | `String of string
      | `Number of string
      | `Object of (string * t) list
      | `Array of t list
      ]
  end

  type args := (string * Json.t) list

  type t

  val instant :
    ?args:args -> category:string -> name:string -> time_in_nanoseconds:int -> unit -> t

  val span :
       ?args:args
    -> category:string
    -> name:string
    -> start_in_nanoseconds:int
    -> finish_in_nanoseconds:int
    -> unit
    -> t
end

(** A tracing context, which corresponds to a trace file that is managed by this library
    and will be validated and collected by Dune after the action terminates.

    Functions in this module do anything only when tracing is [enabled]. *)
module Context : sig
  type t

  val create : name:string -> t
  (** Create a fresh trace file, including [name] in the file name to help with debugging.
      Pick informative names but don't worry about name collisions: this function uses
      [Filename.open_temp_file] under the hood, and so each name will be augmented with a
      random number with some retries. *)

  val emit : t -> Event.t -> unit
  (** Emit a trace event. *)

  val close : t -> unit
  (** Close the trace file. Raises if it has already been closed. *)
end

val with_fresh_context : name:string -> f:(Context.t -> 'a) -> 'a
(** Calls [Context.create] to create a fresh trace file, runs [f] to populate it, and then
    closes it when [f] returns. *)

module For_testing : sig
  module Json = Json

  val microseconds_string_preserve_precision : ns:int -> string
end

module Private__for_use_by_dune_only : sig
  val trace_dir : unit -> string option

  val trace_dir_env_var : string
end
