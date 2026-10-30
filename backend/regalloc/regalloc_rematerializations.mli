[@@@ocaml.warning "+a-30-40-41-42"]

(** The rematerialized instructions inserted by the register allocator (cf.
    [Regalloc_split_state.RewriteAsRematerialize]): for the id of each inserted
    instruction, the pre-allocation instruction it is a copy of and the
    pre-allocation pseudo-register whose value it recomputes. Both are plain
    identifiers: no [Reg.t], whose location the allocator mutates, is retained.
    The allocator returns this structure alongside the allocated CFG, and the
    validator (cf. [Regalloc_validate]) consumes it: each entry is checked
    against the pre-allocation description, from which the validator then takes
    the registers its equation transfer uses for the inserted instruction (which
    are the ones the rest of the description refers to, rather than the fresh
    registers the split pass introduced). *)

type rematerialization =
  { source : InstructionId.t;
        (** id of the pre-allocation instruction the inserted instruction is a
            copy of *)
    stamp : Reg.Stamp.t
        (** stamp of the pre-allocation pseudo-register whose value the inserted
            instruction recomputes: the result of [source], or a register
            reached from it through moves (cf.
            [Regalloc_split_utils.try_rematerialize]) *)
  }

type t

val create : unit -> t

(** [record t ~id rematerialization] records that the instruction [id], inserted
    by the allocator, is the rematerialization described by [rematerialization].
    It is a fatal error to record the same [id] twice. *)
val record : t -> id:InstructionId.t -> rematerialization -> unit

val find : t -> InstructionId.t -> rematerialization option
