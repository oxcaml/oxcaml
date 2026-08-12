type t

(** Make something fancy out of something basic. *)
val create : Basic.t -> t
val wrap : P.t -> t
val to_string : t -> string
