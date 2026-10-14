(** Union-find with union by rank, path compression, and backtracking.

    Each equivalence class carries a value, stored at its root. Every mutation
    (including path compression) is reported to the change log installed by
    [set_log], so that it can be undone with [Change.undo]. Each application of
    [Make] has its own log. Values should be immutable for undoing to fully
    restore a class. *)

module Make () : sig
  type 'a t

  (** [create v] is a new singleton class with value [v]. *)
  val create : 'a -> 'a t

  (** [get t] is the value of [t]'s class. *)
  val get : 'a t -> 'a

  (** [set t v] replaces the value of [t]'s class with [v]. *)
  val set : 'a t -> 'a -> unit

  (** [same t1 t2] is [true] iff [t1] and [t2] are in the same class. *)
  val same : 'a t -> 'a t -> bool

  (** [union t1 t2] merges the classes of [t1] and [t2]. The value of the
      combined class is the value of [t1] or [t2]; it is unspecified which.

      After [union t1 t2], [same t1 t2] always holds true. *)
  val union : 'a t -> 'a t -> unit

  include With_backtracking.S
end
