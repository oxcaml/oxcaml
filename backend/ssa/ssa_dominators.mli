[@@@ocaml.warning "+a-40-41-42"]

(** Dominator / predecessor / successor queries over a finished SSA graph.

    The dominator tree is computed once by [Ssa.finish_graph] and exposed on
    [Block]; this module just gathers the queries the loop analyses need behind
    a single name, so they do not have to depend on the precise [Block] API. *)

module Make (S : Ssa.Finished_graph) : sig
  (** [dominates a b] is [true] iff every path from the function entry to [b]
      passes through [a]. *)
  val dominates : S.Block.t -> S.Block.t -> bool

  (** CFG predecessors of a block. *)
  val predecessors : S.Block.t -> S.Block.t list

  (** All successors of a block, including the implicit trap successor. *)
  val successors : S.Block.t -> S.Block.t list
end
