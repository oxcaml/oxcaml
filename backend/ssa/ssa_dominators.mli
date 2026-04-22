[@@@ocaml.warning "+a-40-41-42"]

(** Dominator analysis over the SSA CFG (Cooper-Harvey-Kennedy algorithm).

    Each [Ssa.t] is traversed once to build the immediate-dominator tree and
    DFS in/out times, giving O(1) [dominates] queries. The predecessor map
    computed as a by-product is also cached and exposed, since most loop and
    control-flow analyses need it. *)

type t

val compute : Ssa.t -> t

(** [dominates t a b] is [true] iff every path from the function entry to
    [b] must pass through [a]. *)
val dominates : t -> Ssa.block -> Ssa.block -> bool

(** CFG predecessors of [b], derived from walking every block's terminator
    successors in the input graph. *)
val predecessors : t -> Ssa.block -> Ssa.block list

(** CFG successors of [b], derived from its terminator. Stateless — exposed
    here because every consumer of dominator info also wants the successor
    function that was used to build it. *)
val successors : Ssa.block -> Ssa.block list
