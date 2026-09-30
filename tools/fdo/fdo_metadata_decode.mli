(** Complete within-function stacks and dynamic cross-function context. *)

type t

(** Parse versioned fragments and expand metadata-order stack sharing. Call
    lengths determine their return PCs. Reject malformed data and conflicting
    names for a hash (when names are present). *)
val parse : string -> t

val names : t -> string Fdo_counter.Hash.Tbl.t

(** Print the annotations of each function in metadata order, and the body
    hashes, by name where names are present. Code addresses and offsets are
    omitted, so that the output does not depend on code layout. *)
val print : Format.formatter -> t -> unit

(** The body hashes of every compiled instrumented function, by function hash.
*)
val bodies : t -> Fdo_counter.Function_body_hash.t Fdo_counter.Hash.Tbl.t
