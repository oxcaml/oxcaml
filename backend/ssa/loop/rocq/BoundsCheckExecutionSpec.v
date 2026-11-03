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

(* The statement of the bounds-check elimination, on the original graph.

   This file is to be reviewed: it is [find_eliminations_sound_statement],
   what is claimed of every pair the extracted decision
   BoundsCheckElim.find_eliminations reports. The proof is
   BoundsCheckExecutionProofs.v, which needs no review.

   Take any pair [(b, target)] reported for a finished graph
   ([finished_graph], SsaProgressSpec.v). Whenever the function is entered
   with machine words in its registers ([bounded_registers]) and the
   observation semantics of SsaContextSpec.v ([context_steps], which
   represents every real execution) reaches the end of the instructions of
   [b], the next step of the original graph (SsaSemanticsSpec.step) enters
   [target] with the registers unchanged: the branch ending [b], a two-way
   [Switch] on the unsigned comparison the compiler emits for an array bounds
   check, takes its edge to [target]. Replacing the branch by a jump to
   [target] therefore changes no execution, and since nothing is skipped
   there is no liveness condition to meet (ROCQ-REVIEW-GUIDE.md, section 5.5).
   The guards, loop counters and affine facts the decision relied on do not
   appear in the statement. Nothing is extracted. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel SsaSemanticsSpec SsaProgressSpec
  SsaContextSpec.
From OxCamlLoop Require Import BoundsCheckElim.
Import ListNotations.

(* ======================================================================== *)
(* Statements                                                               *)
(* ======================================================================== *)

(* The statement below is bound as a proposition here;
   BoundsCheckExecutionProofs.v proves it under the same name without the
   suffix, and [make -C rocq claims] checks that it does. *)

(* Every context execution from the entry that reaches a removed check steps
   to the target returned for it. The premises are the graph contract, bound
   initial registers and the actual execution. *)
Definition find_eliminations_sound_statement : Prop :=
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (b target : block),
  finished_graph blocks entry defs ->
  (b, target) ∈ (find_eliminations defs entry blocks) ->
  ∀ (initial_regs : registers) (reached : state),
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  current_block reached = b -> remaining reached = [] ->
  SsaSemanticsSpec.step reached (enter target (current_registers reached)).
