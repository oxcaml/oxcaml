[@@@ocaml.warning "+a-4-40-41-42-44"]

(* Dominator information is now built into [Ssa.Finished_graph] (computed by
   [finish_graph]); this module simply re-exposes the relevant queries under the
   names the loop analyses use. *)

module Make (S : Ssa.Finished_graph) = struct
  let dominates = S.Block.dominates

  let predecessors = S.Block.predecessors

  let successors = S.Block.successors
end
