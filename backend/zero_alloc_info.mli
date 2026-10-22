(** Function summaries computed by zero_alloc analysis and encoded as integers
    for storing in .cmx files. *)

[@@@ocaml.warning "+a-40-41-42"]

module Encoded : sig
  module Component : sig
    type t

    val encode :
      Zero_alloc_utils.('args_only Allocation_sources.t Component.t) ->
      t

    val decode :
      t ->
      Zero_alloc_utils.(unit Allocation_sources.t Component.t)
  end

  module Components : sig
    type t

    val encode :
      encode_component:('a -> Component.t) ->
      'a Zero_alloc_utils.Components.t ->
      t

    val decode :
      decode_component:(Component.t -> 'a) ->
      t ->
      'a Zero_alloc_utils.Components.t

    val all_top : t

    val ( = ) : t -> t -> bool
  end
end

val encode :
  Zero_alloc_utils.('args_only Allocation_sources.t Component.t Components.t) ->
  Encoded.Components.t

val decode :
  Encoded.Components.t ->
  Zero_alloc_utils.(unit Allocation_sources.t Component.t Components.t)

type t

val create : unit -> t

val reset : t -> unit

(** [merge_checks t ~into] modifies [into] by adding information from [t]. *)
val merge : t -> into:t -> unit

(** [get_value t fun_name] returns None if [fun_name] is not associated with any
    value. *)
val get : t -> string -> Encoded.Components.t option

val set : t -> string -> Encoded.Components.t -> unit

val iter : f:(string -> Encoded.Components.t -> unit) -> t -> unit

module Raw : sig
  type t

  val print : t -> unit
end

val to_raw : t -> Raw.t

val of_raw : Raw.t -> t
