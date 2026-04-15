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
val compute : Cfg_with_infos.t -> (Reg.t * Reg.t) list -> t

type affinities

(** Returns the affinities for the passed temporary in descending order (i.e.
    from the highest to the lowest affinity), use `next` to get the elements in
    order. *)
val get : t -> Reg.t -> affinities

(** Returns the next affinity if there is one, `None` otherwise *)
val next : affinities -> affinity option
