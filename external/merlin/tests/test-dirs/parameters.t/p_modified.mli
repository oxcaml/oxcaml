(** A parameter module. *)

(** A thing. *)
type t

(** Make a thing. *)
val create : int -> t

(** Frobnicate the thing. *)
val frob : t -> t

(** Show the thing. *)
val to_string : t -> string
