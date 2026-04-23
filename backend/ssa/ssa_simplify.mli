(** Optimization pass that simplifies trivial comparisons.

    Currently: [(cmp) != 0] ⇒ [cmp] for any already-boolean comparison [cmp].
    This removes the [!= 0] wrap that [ssa_of_cmm] inserts around truth tests.
*)

val run : Ssa.t -> Ssa.t
