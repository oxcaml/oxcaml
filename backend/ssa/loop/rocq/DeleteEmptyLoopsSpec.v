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

(* What is relied on about [find_deletions] (DeleteEmptyLoops.v) outside Rocq,
   besides eligibility.

   This file is to be reviewed. The result of the development,
   LoopEligibilitySpec.find_deletions_eligible_statement, says what holds of
   the execution of the original graph for every pair [find_deletions]
   reports. Two further facts about those pairs are used by arguments that
   are not mechanised, and are stated here in the vocabulary of the graph and
   the semantics only, so that nothing of the analyses has to be read:

   - [find_deletions_exit_edge_statement]: the exit is a target of the
     header's own [Switch]. The rewrite replaces that terminator by a [Goto]
     to the exit, so the rewritten graph has a subset of the original's
     edges, and a register live at the exit's entry is live at the header's
     terminator (ROCQ-REVIEW-GUIDE.md, section 4).
   - [find_deletions_context_covered_statement]: no instruction of the graph
     is an exactly modelled operation with other than one result, the one
     instruction shape SsaContextSpec.context_step has no rule for. So every
     real execution of the code before the loop is some [context_steps] run,
     and the [reached_header] premise of the eligibility statement can be met
     by every real execution reaching the header (section 5.3).

   What the analyses check of a loop they delete, read back from the decision,
   is DeleteEmptyLoopsProofs.deletion_certificate: the proof's internal
   interface, stated through the analyses themselves, which needs no review. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaSemanticsSpec.
From OxCamlLoop Require Import DeleteEmptyLoops.
Import ListNotations.

(* ======================================================================== *)
(* Statements                                                               *)
(* ======================================================================== *)

(* The statements below are bound as propositions here; DeleteEmptyLoopsProofs.v
   proves them under the same names without the suffix, and
   [make -C rocq claims] checks that it does. *)

(* The exit of a reported pair is a target of the header's [Switch]. With
   SsaGraphSpec.control_edges it is a successor of the header, so the header's
   terminator and the exit's entry are joined by a
   SsaDominanceSpec.program_point_edge. *)
Definition find_deletions_exit_edge_statement : Prop :=
  ∀ (defs : op_def) (blocks : list block) (header exit : block),
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∃ (index : value) (targets : list block),
    terminator header = Terminator.Switch index targets ∧ exit ∈ targets.

(* When a pair is reported, every [Op] of every block of the graph whose
   operation is exactly modelled ([modeled_operation]) has one result. *)
Definition find_deletions_context_covered_statement : Prop :=
  ∀ (defs : op_def) (blocks : list block) (header exit : block),
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∀ (b : block) (id : instr_id) (op : operation) (args : list value)
    (count : int),
    b ∈ blocks -> (Op id op args count) ∈ (block_body b) ->
    modeled_operation op args = true -> count = one.
