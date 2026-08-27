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

    A loop is considered empty when:
    - {!Termination.analyze} reports [Terminates],
    - {!Dead_induction_var.analyze} reports [useless = true], i.e. every block
      parameter of the header is a basic induction variable whose only uses are
      its own update expression and the exit comparison, and
    - every block of the loop body contains only side-effect-free instructions
      and ends in a pure control-flow terminator.

    The transformation rewrites the loop header's exit [Switch] into an
    unconditional jump to the exit target. The loop body, back edges and IV
    update computations become unreachable and are pruned when the output graph
    is finished; downstream CFG passes (e.g. merge-block cleanup) tidy up the
    remaining control flow. *)

open Ssa.Export

(** Delete empty loops in the graph, returning the rewritten graph together with
    the number of loops deleted. Returns the input unchanged when there is
    nothing to delete. *)
val run : finished Ssa.graph -> finished Ssa.graph * int
