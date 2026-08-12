(** Basic functionality implemented over the [P] parameter. *)

(** A [P.t] with minor enhancements. *)
type t

(** Make a [t] from scratch. *)
val create : unit -> t

(** Make a [t] from a [P.t]. *)
val wrap : P.t -> t

val p : t -> P.t

(** Convert [t] to string. *)
val to_string : t -> string
