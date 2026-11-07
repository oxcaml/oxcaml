(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(** Remove array bounds checks that always pass.

    A bounds check is a two-way [Switch] on an unsigned comparison
    [index <u limit] (or [<=u], or either with the operands exchanged), the
    single comparison by which the compiler tests [0 <= index < length]. It is
    removed when the facts known on entry to its block entail that the
    comparison is true: the facts are those of the branches dominating the block
    (rocq/AffineSsa.v), the side facts of the linearizations (for an array
    length read from the header word, the range of the logical shift that
    extracts it), and the certified bounds of loop counters
    (rocq/InductionRange.v: a header parameter that is a basic induction
    variable with a positive literal step, entered with literals, never
    wrapping, and, when some branch compares the counter or its update with a
    value defined before the loop that every entry respects, as the exit test of
    a [for] loop does, bounded above by that value). The index is linearized
    modulo 2^64 (rocq/AffineSsa.v, [linearize_goal]); the goals pin it into
    register range. The index of a [for] loop is the counter re-tagged by a
    target-specific scaled addition, which the model reads through the target's
    [Arch.linear_specific_operation] (trusted; see backend/rocq/Operation.v).

    The analysis first applies the gate of [Delete_empty_loops] to the whole
    graph (at most 2^32 blocks; every exactly modeled integer operation has one
    result). The transformation rewrites the check's [Switch] into an
    unconditional jump to its true target; the failure block becomes unreachable
    when it has no other predecessor and is pruned when the output graph is
    finished. The analysis is [BoundsCheckElim.find_eliminations], extracted
    from rocq/BoundsCheckElim.v; rocq/BoundsCheckElimProofs.v says, as
    [elimination_certificate], what is established of every check that is
    removed. rocq/BoundsCheckExecutionSpec.v states, and
    BoundsCheckExecutionProofs.v proves ([find_eliminations_sound]), for every
    reported (block, target) pair, that every execution reaching the block's
    terminator takes the branch to the target, under the graph contract
    [finished_graph] (SsaProgressSpec.v) and bounded machine-word inputs. The
    mechanical graph rewrite is outside the proof's scope. *)

open Ssa.Export

(** Remove the redundant bounds checks of the graph, returning the rewritten
    graph together with the number of checks removed. Returns the input
    unchanged when there is nothing to remove. *)
val run :
  ppf_dump:Format.formatter -> finished Ssa.graph -> finished Ssa.graph * int
