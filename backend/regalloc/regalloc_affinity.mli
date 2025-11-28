[@@@ocaml.warning "+a-30-40-41-42"]

(** This module implements a basic variant of affinity: we compute the numbers
    of moves between temporaries and physical registers, and use this
    information in register allocators to try to assign a temporary to a
    physical register with high affinity.

    This could be extended to the phi moves introduced during splitting, but
    this basic version already seems useful, in particular for the linscan
    and greedy allocators. *)

type phys_reg = int

type affinity =
  { priority : int;
    phys_reg : phys_reg
  }

type t

(** Computes the affinities for the passed CFG, i.e. for each temporary the
    number of times it moves from/to a given physical register. *)
val compute : Cfg_with_infos.t -> t

(** Returns the affinities for the passed temporary in descending order
    (i.e. from the highest to the lowest affinity), returning an empty list
    if the temporary has no affinity with any physical register. *)
val get : t -> Reg.t -> affinity list
