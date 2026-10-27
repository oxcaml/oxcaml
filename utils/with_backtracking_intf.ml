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

  (** Stamps order object creation against snapshots: changes to objects created
      after the latest snapshot need not be logged. *)
  module Stamp : sig
    val fresh : unit -> int

    (** [needs_log t] is whether [t] was stamped before the latest snapshot. *)
    val needs_log : int -> bool
  end

  module Snapshot : sig
    type t

    val take : unit -> t

    (** [restore t] undoes the [take] that returned [t]. *)
    val restore : t -> unit
  end

  (** [Make (C)] is a fresh log for changes of type [C.t]. *)
  module Make (C : Change.S) : sig
    include S with module Change := C

    (** [log change] reports [change] to the log installed by [set_log]. *)
    val log : C.t -> unit
  end
end
