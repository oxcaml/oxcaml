[@@@ocaml.warning "+a-40-41-42"]

(** Loop fusion / list deforestation over the SSA representation.

    Collapses a chain of adjacent "reversing list-map" loops (the shape a
    [List.rev_map]/[List.rev] pipeline produces after inlining) into a single
    loop applying the composition of the per-element functions, eliminating the
    intermediate lists. Only applied when the loop bodies have no observable
    side effects (allocation of the fresh cells is fine).

    Returns the (possibly rewritten) graph and the number of chains fused. *)
val run : (module Ssa.Finished_graph) -> (module Ssa.Finished_graph) * int
