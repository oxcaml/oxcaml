[@@@ocaml.warning "+a-40-41-42"]

(** Range-based array bounds-check elimination over the SSA representation.

    A checked array read lowers to an unsigned comparison guarding the load,
    with the out-of-bounds edge raising [Invalid_argument]:

    {[
     if idx <u len then <load> else <raise>
    ]}

    This pass rewrites such a [Branch] into an unconditional [Goto] to the
    in-bounds target whenever it can prove [0 <= idx < len] from a small affine
    analysis: induction-variable ranges (via {!Induction_var}), dominating
    signed guards, and the bounds implied by right shifts, combined by
    Fourier-Motzkin. See [bounds_check_elimination.md] for the design and the
    soundness argument. *)

(** Rewrite every provably-redundant bounds-check [Branch] into an unconditional
    [Goto] to its in-bounds target, mutating the graph in place; returns the
    number of checks eliminated. *)
val run : (module Ssa.Finished_graph) -> int
