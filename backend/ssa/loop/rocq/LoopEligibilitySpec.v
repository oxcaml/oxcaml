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

(* Semantic eligibility for bypassing a loop, stated on the original graph.

   This file is to be reviewed: it is the statement of the development,
   [eligible_entry] and [find_deletions_eligible_statement]. The proof is
   LoopEligibilityProofs.v, which needs no review.

   [eligible_entry header exit header_regs] is the target property, not a
   record of analysis answers. Entering [header] with registers [header_regs],
   the original graph executes the header's instructions once, then its
   execution first arrives at [exit] after finitely many steps of
   SsaSemanticsSpec.step, at the exit's entry with no parameter to bind, holding
   the value computed by that first header execution in every register the
   exit's continuation reads before redefining it. The retained header is
   executed, not skipped. [steps_until] is a property of the one execution from
   the header, since [step] is deterministic
   (SsaSemanticsSpec.step_deterministic_statement); so the loop neither
   diverges nor gets stuck before the exit, and its steps are whitelisted
   integer operations, Gotos and Switches, because [step] has no other rule.

   [find_deletions_eligible_statement] claims this for every header/exit pair
   the extracted gate and analyses report, given [finished_graph] (what
   Ssa.finish_graph guarantees of the graph, SsaProgressSpec.v) and
   [reached_header] (bound inputs and an observed prefix to the header,
   SsaContextSpec.v). It assumes no recurrence, invariant bound, guard outcome
   or finite exit; those are conclusions. The loop and its body never appear
   in the statement: which blocks the execution visits before the exit is
   irrelevant to bypassing it.

   The loop executes in SsaSemanticsSpec.v. Its unchanged prefix uses
   SsaContextSpec.v's word-observation overapproximation, so loads, calls and
   other opaque computations need not be given full memory semantics. Review
   that abstraction and the graph contracts in SsaExecutionSpec.v,
   SsaGraphSpec.v and SsaProgressSpec.v alongside the extraction veneer.

   No post-deletion graph, renaming or pruning is modelled here, and
   eligibility is not assumed as a compiler contract. The step from
   [eligible_entry], per loop and per header entry, to the rewritten function
   behaving like the original is the usual liveness simulation; it is written
   out in ROCQ-REVIEW-GUIDE.md, section 4.7. Its one non-obvious point: a
   register live at the exit's entry is live at the header's terminator,
   because the exit is a target of the header's own [Switch]
   (DeleteEmptyLoopsSpec.find_deletions_exit_edge_statement) and has no
   parameters, so nothing is defined on entering it. Nothing is extracted. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlSsa Require Import SsaModel SsaDominanceSpec SsaSemanticsSpec
  SsaLivenessSpec SsaProgressSpec SsaContextSpec.
From OxCamlLoop Require Import DeleteEmptyLoops.
Import ListNotations.

(* ======================================================================== *)
(* Eligibility                                                              *)
(* ======================================================================== *)

(* A valid bypass retains the first header body, then replaces only its
   terminator by a jump to the parameterless exit. All execution relations here
   refer to the unchanged original graph. [steps_until exit] stops at the first
   state in [exit], so no instruction of the exit is included. *)
Definition eligible_entry (header exit : block) (header_regs : registers)
    : Prop :=
  params exit = []
  ∧ ∃ (first_header_regs exit_regs : registers),
       executes_insns header_regs (block_body header) first_header_regs
       ∧ steps_until exit
            {| current_block := header; remaining := [];
               current_registers := first_header_regs |}
            (enter exit exit_regs)
       ∧ agree_on_live (block_entry_point exit) first_header_regs exit_regs.

(* ======================================================================== *)
(* Statements                                                               *)
(* ======================================================================== *)

(* The statement below is bound as a proposition here; LoopEligibilityProofs.v
   proves it under the same name without the suffix, and [make -C rocq claims]
   checks that it does. *)

(* The result of the development, from the actual deletion decision. Its graph
   premise is builder metadata only; its input premise is ordinary bound
   arguments, an observed prefix and defined Switch indices. No loop recurrence
   or termination premise remains. *)
Definition find_deletions_eligible_statement : Prop :=
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (header exit : block),
  finished_graph blocks entry defs ->
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∀ (initial_regs header_regs : registers),
    reached_header entry header initial_regs header_regs ->
    eligible_entry header exit header_regs.
