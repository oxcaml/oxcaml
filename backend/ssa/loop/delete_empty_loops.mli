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

(** Bypass every loop whose body does no observable work.

    The analysis first rejects graphs larger than 2^32 blocks and graphs in
    which an exactly modeled operation family (integer constants, additions,
    subtractions, comparisons, immediate shifts and or) has other than one
    result, so that the proof's observation semantics has a rule for every
    instruction of the graph. This conservatively checks the whole graph, not
    only code preceding a candidate loop. Trap handlers, opaque loads and calls
    outside loops are permitted.

    A loop is considered empty when:
    - the termination analysis (rocq/LoopTermination.v) reports [Terminates],
      and the body with its header removed is acyclic, so execution cannot
      diverge between visits to the header,
    - every block parameter of the header is a basic induction variable
      (rocq/InductionVar.v) whose only uses are its own update expression and
      the exit comparison, which in turn is used only by the header's branch
      (rocq/DeadInductionVar.v), and
    - every block of the loop body, including the header, contains only integer
      constants, additions, subtractions and comparisons, with one result and
      the expected operands (rocq/SsaSemantics.v), and ends in a [Goto] or
      [Switch]; memory operations are not supported, and
    - every edge leaving the loop body goes to the same exit target as the
      header's exit edge.

    The transformation rewrites the loop header's exit [Switch] into an
    unconditional jump to the exit target. The loop body, back edges and IV
    update computations become unreachable and are pruned when the output graph
    is finished; downstream CFG passes (e.g. merge-block cleanup) tidy up the
    remaining control flow. The analysis is [DeleteEmptyLoops.find_deletions],
    extracted from rocq/DeleteEmptyLoops.v, whose [deletion_certificate] states
    what is established of every loop that is deleted. The operational semantics
    is in backend/ssa/rocq/SsaSemantics.v; SsaContext.v overapproximates word
    observations of opaque computations in the unchanged prefix.
    LoopEligibility.find_deletions_eligible proves, for every reported
    header/exit pair, that the execution from the header first arrives at the
    exit after finitely many steps and agrees with the first header execution on
    live-out values, under the graph contract [finished_graph] (SsaProgress.v)
    and the input contract [reached_header] (SsaContext.v). The size and
    prefix-instruction checks are proved by [find_deletions_supported]. The
    mechanical graph rewrite is outside the proof's scope. *)

open Ssa.Export

(** Delete empty loops in the graph, returning the rewritten graph together with
    the number of loops deleted. Returns the input unchanged when there is
    nothing to delete. *)
val run :
  ppf_dump:Format.formatter -> finished Ssa.graph -> finished Ssa.graph * int
