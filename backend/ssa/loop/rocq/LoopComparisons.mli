open FourierMotzkin

val oriented_continue_comparison :
  bool -> bool -> Cmm.integer_comparison -> Cmm.integer_comparison

type termination =
  | Terminates
  | Terminates_if_bound_in_range
  | Unknown

val continue_terminates : int -> Cmm.integer_comparison -> termination

val facts :
  bool -> Cmm.integer_comparison -> Affine.t -> Affine.t -> Affine.t list
