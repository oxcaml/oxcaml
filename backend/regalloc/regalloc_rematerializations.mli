[@@@ocaml.warning "+a-30-40-41-42"]

(** Maps each inserted rematerialization's instruction ID to its pre-allocation
    source instruction ID and pseudo-register stamp. Only identifiers are
    retained, since allocation mutates [Reg.t] locations. *)

type rematerialization =
  { source : InstructionId.t;
        (** ID of the pre-allocation instruction being copied. *)
    stamp : Reg.Stamp.t
        (** Stamp of the pre-allocation pseudo-register whose value is
            recomputed: the result of [source], or a register reached from it
            through moves. *)
  }

type t

val create : unit -> t

(** It is a fatal error to record the same [id] twice. *)
val record : t -> id:InstructionId.t -> rematerialization -> unit

val find : t -> InstructionId.t -> rematerialization option
