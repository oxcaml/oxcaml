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

(* Every read, and every live register, has an available canonical
   definition; derived from the structural SSA contract, not assumed as an
   execution or loop-analysis invariant.

   Nothing here needs review. *)

From Stdlib Require Import ZArith Lia List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlStdlib Require Import OxCamlInt.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaDominanceSpec
  SsaDominanceProofs SsaSemanticsSpec SsaExecutionSpec SsaExecutionProofs
  SsaLivenessSpec.
Import ListNotations.

(* The point immediately before an actual instruction has its instruction edge
   to the definition site. *)
Lemma definition_instruction_edge : ∀ (b : block) (remaining : nat)
    (id : instr_id) (op : operation) (args : list value) (count : int),
  result_definition id op args count (b, remaining) ->
  program_point_edge (b, S remaining) (b, remaining).
Proof.
  intros b remaining id op args count [prefix [suffix [Hbody Hlength]]].
  cbn in Hbody, Hlength. constructor.
  rewrite Hbody, length_app. cbn. lia.
Qed.

(* Every actual read has an available canonical definition. For an instruction,
   strict definition/use dominance puts the operand before the instruction,
   rather than merely after its result is written. *)
Theorem operand_at_available : ∀ (blocks : list block) (entry : block)
    (position : program_point) (v : value),
  well_formed_ssa blocks entry -> (fst position) ∈ blocks ->
  operand_at position v -> available_value blocks entry position v.
Proof.
  intros blocks entry position v Hwf Hblock Huse.
  destruct Huse as [b remaining id op args count arg Hdefinition Harg |
    b args arg Hargs Harg]; cbn in Hblock.
  - destruct (operand_definitions _ _ Hwf id op args count (b, remaining)
      Hblock Hdefinition arg Harg)
      as [site [Harg_definition [Hsite [Hdom Hstrict]]]].
    exists site. split; [exact Harg_definition |]. split; [exact Hsite |].
    eapply program_point_dominates_predecessor; [exact Hdom | exact Hstrict |].
    eapply definition_instruction_edge. exact Hdefinition.
  - destruct (terminator_definitions _ _ Hwf b args Hblock Hargs arg Harg)
      as [site [Hdefinition [Hsite Hdom]]].
    exists site. auto.
Qed.

(* A point edge stays inside a successor-closed graph. *)
Lemma program_point_edge_in_graph : ∀ (blocks : list block)
  (entry : block)
    (before after : program_point),
  well_formed_ssa blocks entry -> (fst before) ∈ blocks ->
  program_point_edge before after -> (fst after) ∈ blocks.
Proof.
  intros blocks entry before after Hwf Hbefore Hedge. destruct Hedge;
    cbn in *; [exact Hbefore |].
  eapply graph_successors_closed; eassumption.
Qed.

(* A value needed before its next definition is already available. This is why
   live-out agreement can be proved using available-value equations; stale
   registers for definitions that will execute later are not live. *)
Theorem live_at_available : ∀ (blocks : list block) (entry : block)
    (position : program_point) (reg : register),
  well_formed_ssa blocks entry -> (fst position) ∈ blocks ->
  register_live_at position reg ->
  ∃ (v : value), register_of_value v = reg ∧
    available_value blocks entry position v.
Proof.
  intros blocks entry position reg Hwf Hblock Hlive.
  revert Hblock. induction Hlive as [position v Huse |
    before after reg Hedge Hnot Hlive IH]; intros Hblock.
  - exists v. split; [reflexivity |].
    eapply operand_at_available; eassumption.
  - destruct (IH (program_point_edge_in_graph _ _ _ _ Hwf Hblock Hedge))
      as [v [Hreg [site [Hdefinition [Hsite Hdom]]]]].
    exists v. split; [exact Hreg |]. exists site.
    split; [exact Hdefinition |]. split; [exact Hsite |].
    eapply program_point_dominates_predecessor; [exact Hdom | | exact Hedge].
    intros Heq. subst site. apply Hnot. exists v. auto.
Qed.
