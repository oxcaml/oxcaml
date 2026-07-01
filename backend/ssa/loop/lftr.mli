[@@@ocaml.warning "+a-40-41-42"]

(** Linear-function test replacement over the SSA representation.

    After {!Strength_reduction}, a loop can carry two induction variables that
    run in lockstep: the original counter [i] (used only by the exit test and
    its own increment) and a derived IV [sr = ratio * i + b] that does the real
    work (e.g. the address offset). This pass re-expresses the exit test
    [i CMP bound] on [sr], after which [i] has no remaining uses and is deleted,
    leaving a single induction variable — matching the hand-written idiom that
    advances an offset and compares it against a precomputed bound.

    The test is rewritten to [(sr - limit) CMP 0], where [limit] is the value
    [sr] would take at [i = bound] (built once in the preheader). Because
    [sr - limit = ratio * (i - bound)] as a wrapping subtraction, the
    loop-invariant base [b] of the IV cancels {e exactly} — so, unlike comparing
    [sr] against [limit] directly, correctness never depends on that base (e.g.
    a heap offset) staying in range.

    Preconditions checked: a single preheader; the exit test is a signed order
    comparison between a basic IV and a loop-invariant bound; that IV is
    {!Dead_induction_var.is_dead} (its only uses are the test and its update);
    and a distinct, non-dead basic IV exists whose step is a positive integer
    multiple [ratio] of the counter's step.

    {b Soundness.} [(sr - limit) CMP 0] agrees with [i CMP bound] only while
    [ratio * (i - bound)] stays in signed 64-bit range. Given the loop's own
    [0 <= i <= bound], that follows from [0 <= bound] and [ratio * bound] not
    overflowing. This pass {e proves} both from the guards that dominate the
    loop (via {!Affine_ssa} and {!Fourier_motzkin}) and fires only when it can —
    e.g. a dominating [assert (0 <= n && n < 1_000_000)] licenses reducing a
    loop bounded by [n]. It never assumes non-overflow. *)

(** Run linear-function test replacement, returning a new graph (the retired
    counter is removed by a trailing cleanup pass). If there is nothing to do,
    the input is returned unchanged. *)
val run : (module Ssa.Finished_graph) -> (module Ssa.Finished_graph)
