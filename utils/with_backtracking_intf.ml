module Change = struct
  module type S = sig
    (** An undoable mutation. *)
    type t

    (** [undo change] reverts [change]. Changes are undone in LIFO order. *)
    val undo : t -> unit
  end
end

(** Mutable state whose changes are reported to a client-installed log. *)
module type S = sig
  module Change : Change.S

  (** [set_log log] calls [log] on each subsequent change. *)
  val set_log : (Change.t -> unit) -> unit
end

module type With_backtracking = sig
  module Change = Change

  module type S = S

  (** [Make (C)] is a fresh log for changes of type [C.t]. *)
  module Make (C : Change.S) : sig
    include S with module Change := C

    (** [log change] reports [change] to the log installed by [set_log]. *)
    val log : C.t -> unit
  end

  (** Stamps allow us to skip logging changes to objects created since the
      latest snapshot. Restoring that snapshot makes those objects unreachable.

      [Stamp] tracks two counters:
      - [last], the last stamp created via [fresh]
      - [latest_snapshot], the value of [last] when the last snapshot was taken.

      [Snapshot.take] and [Snapshot.restore] only changes [latest_snapshot]. It
      doesn't undo any changes, the client must snapshot and backtrack its own
      changelog.

      {[
      let log_type ty =
        if With_backtracking.Stamp.needs_log ty.id
        then log_change (Ctype (ty, ty.desc))

      let snapshot () = !changelog, With_backtracking.Stamp.Snapshot.take ()
      ]}

      The client's [backtrack] must undo its [changelog] and then calls
      [Snapshot.restore]. *)

  module Stamp : sig
    val fresh : unit -> int

    (** [needs_log t] is whether [t] was stamped before the latest snapshot. *)
    val needs_log : int -> bool

    module Snapshot : sig
      type t

      val take : unit -> t

      val restore : t -> unit
    end
  end
end
