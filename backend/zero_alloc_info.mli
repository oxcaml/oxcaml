(** Function summaries computed by zero_alloc analysis for storing in .cmx
    files. *)

[@@@ocaml.warning "+a-40-41-42"]

module Call_outcomes : sig
  type t = private int

  val normal_return : t

  val exceptional_return : t

  val diverging : t

  val all : t
end

module Allocation_sources : sig
  type t = private
    | Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments of
        Call_outcomes.t Numbers.Int.Map.t

  val unknown : t

  val argument : lowered_index:int -> outcomes:Call_outcomes.t -> t

  val is_empty : t -> bool

  val compare : t -> t -> int

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end

type value

type component =
  | Top of Allocation_sources.t
  | Safe
  | Bot

type components =
  { nor : component;
    exn : component;
    div : component
  }

val encode : components -> value option

val decode : value -> components

type t

val create : unit -> t

val reset : t -> unit

(** [merge_checks t ~into] modifies [into] by adding information from [t]. *)
val merge : t -> into:t -> unit

(** [get_value t fun_name] returns None if [fun_name] is not associated with any
    value. *)
val get_value : t -> string -> value option

val set_value : t -> string -> value -> unit

val iter : f:(string -> value -> unit) -> t -> unit

module Raw : sig
  type t

  val print : t -> unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
