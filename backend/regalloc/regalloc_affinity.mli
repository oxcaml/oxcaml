[@@@ocaml.warning "+a-30-40-41-42"]

(** This module implements a basic variant of affinity: we compute the numbers
    of moves between temporaries and physical registers, and use this
    information in register allocators to try to assign a temporary to a
    physical register with high affinity. *)

type affinity =
  { priority : int;
    phys_reg : Regs.Phys_reg.t
  }

type t

(** Computes the affinities for the passed CFG, i.e. for each temporary the
    number of times it moves from/to a given physical register. *)
val compute : Cfg_with_infos.t -> Regalloc_split.phi_move list -> t

(** Returns [true] iff the two passed temporaries are in the same "phi" class,
    i.e. are linked by phi moves. *)
val same_phi_class : t -> Reg.t -> Reg.t -> bool

(** Returns the affinity between the passed temporary and the passed physical
    register (i.e. the number of moves linking them, weighted by loop depth),
    returning [0] if they have no affinity. *)
val priority : t -> temp:Reg.t -> phys_reg:Regs.Phys_reg.t -> int

type affinities

(** Returns the affinities for the passed temporary in descending order (i.e.
    from the highest to the lowest affinity), use `next` to get the elements in
    order. *)
val get : t -> Reg.t -> affinities

(* CR-someday mslater for xclerc: consider a stateless `iter` or `iter_until`
   function, that wouldn't need to store an index. *)

(** Returns the next affinity if there is one, `None` otherwise *)
val next : affinities -> affinity option
