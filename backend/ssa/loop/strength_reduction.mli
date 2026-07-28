[@@@ocaml.warning "+a-40-41-42"]

(** Induction-variable strength reduction over the SSA representation.

    For a basic induction variable [i] (a header parameter with a constant step,
    via {!Induction_var}), an integer value computed in the loop as an affine
    function [coeff * i + invariant] — e.g. the scaled offset [idx + i << 3] of
    an indexed read — is a derived induction variable. Rather than recomputing
    it each iteration, this pass introduces a fresh header parameter [t] that
    runs in lockstep with [i]:

    - it starts at the same affine expression evaluated at [i]'s initial value,
      materialised once in the preheader, and
    - it is incremented by the constant [coeff * step] on every back edge,

    and rewrites the in-loop occurrence to use [t]. The per-iteration multiply
    or scaled-index computation is thereby replaced by an addition.

    Only [Int]-typed derived values are reduced: making a [Val]/[Addr] base
    pointer into a carried IV would keep an interior pointer live across a GC
    safepoint, which a moving collector cannot handle (this is also why the
    hand-written idiom keeps the base and the integer offset separate). The new
    parameter is added through {!Ssa_reducer}'s [extra_params] hook.

    The transformation is unconditionally sound: it only introduces a redundant
    induction variable and replaces a pure computation with an equal one. It
    does not retire the original counter [i] — when the loop's exit test still
    uses it, that additionally requires linear-function test replacement, which
    is not done here. *)

(** Run strength reduction, returning a new graph. If there is nothing to do,
    the input is returned unchanged. *)
val run : (module Ssa.Finished_graph) -> (module Ssa.Finished_graph)
