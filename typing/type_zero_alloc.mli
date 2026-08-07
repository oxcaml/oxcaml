(* Interaction between [zero_alloc] annotations and the allocation axis. *)

(** The [zero_alloc] guarantee attached to a value, as far as the mode system
    is concerned. *)
type t =
  | Zero_alloc of { strict : bool; arity : int }
  | Default

val val_zero_alloc : Zero_alloc.t -> t
