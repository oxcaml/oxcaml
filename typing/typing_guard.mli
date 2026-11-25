(** Guards of suspended constraints.

    A guard is held by suspended constraints that may update variables after
    they are generalized. It is {e live} until its last hold is released, then
    released forever. To pass a hold on, acquire before releasing.

    A variable is live while one of its guards is (see {!Set}). A live
    generalizable variable is {e partially generalized}. *)

type t

(** Acquires and releases are logged, see {!With_backtracking}. *)
include With_backtracking.S
(** @inline *)

(** Never live. [acquire] and [release] do nothing on it. *)
val none : t

(** [create ()] is live with no holds. Acquire it before generalizing what it
    guards. *)
val create : unit -> t

(** @raise Fatal_error if released. *)
val acquire : t -> unit

(** @raise Fatal_error if not held. *)
val release : t -> unit

val is_live : t -> bool

module Set : sig
  type guard := t

  type t

  val empty : t

  (** Whether no guard, even a released one, was added. *)
  val is_empty : t -> bool

  val mem : guard -> t -> bool

  (** Also forgets released guards. *)
  val add : guard -> t -> t

  val is_live : t -> bool

  val iter_live : (guard -> unit) -> t -> unit
end
