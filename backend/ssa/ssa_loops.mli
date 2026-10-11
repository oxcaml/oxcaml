[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(** Natural loops of a finished SSA graph.

    Loops are found from back edges, i.e. edges whose target dominates their
    source. Natural loops with the same header are merged, so that any two loops
    are either nested or disjoint. *)

open Ssa.Export

module Loop : sig
  type t

  (** The loop header, which dominates every block of the loop. *)
  val header : t -> finished Block.t

  val contains : t -> finished Block.t -> bool
end

type t

(** [None] if the graph is irreducible, i.e. if it has a cycle that can be
    entered other than through a block dominating the whole cycle: such cycles
    are not natural loops. *)
val compute : finished Ssa.graph -> t option

(** The innermost loop containing the block, if any. *)
val innermost_loop : t -> finished Block.t -> Loop.t option
