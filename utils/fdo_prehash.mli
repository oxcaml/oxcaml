(** The stable hash function behind FDO counters. It is part of the profile
    specification, so changing it invalidates all existing profiles. *)

type t

(** Order-sensitive. *)
val combine : t -> t -> t

val int : int -> t

val string : string -> t

val to_int32 : t -> int32
