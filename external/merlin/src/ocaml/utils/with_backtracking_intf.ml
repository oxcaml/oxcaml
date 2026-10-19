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
end
