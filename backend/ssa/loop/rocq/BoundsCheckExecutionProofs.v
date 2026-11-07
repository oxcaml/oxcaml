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

(* The proof of BoundsCheckExecutionSpec.find_eliminations_sound_statement.
   Nothing here needs review.

   BoundsCheckElim.v decides, from the facts of the branches dominating a
   block (AffineSsa.v) and the certified bounds of loop counters
   (InductionRange.v), that the two-way branch ending the block, on an
   unsigned comparison, always takes its true edge. This module proves that
   of actual executions in the observation semantics of SsaContextSpec.v:
   every context execution from the entry that reaches the branch steps to
   the target [find_eliminations] returns for it ([find_eliminations_sound]),
   so replacing the branch by a jump there changes no execution. The guard
   facts come from LoopGuardExecutionProofs.v, the counter facts from
   InductionRangeExecutionProofs.v and the valuation of the SSA values from
   LoopValueSnapshotProofs.v. The mechanical graph rewrite is outside this
   proof's scope. Nothing here is extracted. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlIntProofs OxCamlInt64
  OxCamlInt64Proofs OxCamlList OxCamlListProofs.
From OxCamlBackend Require Import IntegerComparison IntegerComparisonProofs
  Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaGraphProofs
  SsaDominanceSpec SsaDominance SsaDominanceProofs SsaSemanticsSpec
  SsaSemantics SsaSemanticsProofs SsaExecutionSpec SsaExecutionProofs
  SsaProgressSpec SsaProgressProofs SsaContextSpec SsaContextProofs.
From OxCamlLoop Require Import AffineSsa AffineSsaProofs InductionRange
  InductionRangeProofs BoundsCheckElim BoundsCheckElimProofs
  LoopValueSnapshotProofs LoopGuardExecutionProofs LoopRangeExecutionProofs
  InductionRangeExecutionProofs BoundsCheckExecutionSpec.
Import ListNotations.
Local Open Scope Z_scope.

(* The graph-size contract and the certificate count discharge the affine
   atom capacity bound of a check. *)
Lemma check_atoms_fit : ∀ (blocks : list block) (b : block)
    (certificates : list range_certificate),
  graph_size_supported blocks -> NoDup (strict_dominators b) ->
  (∀ (dominator : block), dominator ∈ (strict_dominators b) ->
     dominator ∈ blocks) ->
  Z.of_nat (length certificates) <= 2 ^ 32 ->
  check_atom_bound certificates b <= ocaml_max_int.
Proof.
  intros blocks b certificates Hsize Hnodup Hmembers Hcertificates.
  pose proof (NoDup_incl_length Hnodup Hmembers) as Hlength.
  pose proof (guard_list_atom_bound_by_length b (strict_dominators b))
    as Hguards.
  unfold graph_size_supported in Hsize.
  unfold check_atom_bound, guards_atom_bound, range_atom_bound.
  rewrite linearize_atom_bound_eq.
  rewrite comparison_atom_bound_eq in Hguards.
  unfold ocaml_max_int, two62. cbn in Hsize, Hcertificates. lia.
Qed.

(* The comparison a check branches on is a modeled instruction result. *)
Lemma unsigned_check_shape : ∀ (cond : value) (cmp : integer_comparison)
    (index : value) (l : limit),
  unsigned_check cond = Some (cmp, index, l) ->
  ∃ (id : instr_id) (op : operation) (args : list value)
    (result_index : int),
    cond = Res id op args result_index ∧ modeled_operation op args = true.
Proof.
  intros cond cmp index l H. unfold unsigned_check in H.
  destruct cond as [id op args result_index | b i]; [ | discriminate].
  exists id, op, args, result_index. split; [reflexivity |].
  destruct op; try discriminate; destruct op; try discriminate;
    destruct args as [ | a [ | b [ | extra rest]]]; try discriminate;
    reflexivity.
Qed.

(* Every context execution from the entry that reaches a removed check steps
   to the target returned for it. The premises are the graph contracts, a
   bound initial register file and the actual execution. *)
Theorem find_eliminations_sound : find_eliminations_sound_statement.
Proof.
  intros blocks entry defs b target Hfinished Hin initial_regs reached
    Hbounded Hrun Hblock Hdone.
  pose proof (graph_well_formed _ _ _ Hfinished) as Hwf.
  pose proof (graph_edges _ _ _ Hfinished) as Hedges.
  destruct (find_eliminations_supported _ _ _ _ _ Hin) as [Hsize _].
  destruct (find_eliminations_spec _ _ _ _ _ Hin)
    as [cond [ifnot [cmp [index [l [certificates
      [Hterm [Hcheck [Hcertificates [Hcount Hpasses]]]]]]]]]].
  destruct reached as [current remaining regs]. cbn in Hblock, Hdone.
  subst current remaining.
  pose proof (context_steps_in_graph Hedges _ _ _ _ Hwf Hrun
    (entry_in_graph _ _ Hwf)) as Hb. cbn in Hb.
  assert (Hargs : [cond] ∈ (terminator_args (terminator b))).
  { rewrite Hterm. cbn. auto. }
  pose proof (terminator_operand_available blocks entry
    {| current_block := b; remaining := []; current_registers := regs |}
    [cond] cond Hwf Hb eq_refl Hargs (or_introl eq_refl)) as Havailable.
  pose proof (context_execution_values_coherent Hedges blocks entry
    initial_regs _ Hwf Hrun) as Hcoherent.
  pose proof (proj2 (context_execution_invariants _ _ _ Hbounded Hrun))
    as Hregs_bounded. cbn in Hregs_bounded.
  destruct (unsigned_check_shape _ _ _ _ Hcheck)
    as [id [op [args [result_index [Hcond Hmodeled]]]]].
  pose proof (Hcoherent _ Havailable) as Hvalue. rewrite Hcond in Hvalue.
  cbn [value_coherent] in Hvalue. rewrite Hmodeled in Hvalue.
  destruct Hvalue as [values [result [_ [_ Hresult]]]].
  assert (Hread : regs (register_of_value cond) = Some result).
  { rewrite Hcond. exact Hresult. }
  pose proof (snapshot_read_available blocks entry _ regs cond result Hwf
    Hcoherent Hregs_bounded Havailable Hread) as Hsnapshot.
  assert (Heqb : ∀ (v w : value), value_eqb v w = true ->
    snapshot_value regs v = snapshot_value regs w).
  { intros v w Heq. apply value_eqb_spec in Heq. subst w. reflexivity. }
  destruct (graph_strict_dominators _ _ _ Hfinished b Hb)
    as [Hnodup Hmembers].
  pose proof (executed_guards_taken blocks entry Hedges Hwf
    (graph_dominance _ _ _ Hfinished)
    (graph_dominance_complete _ _ _ Hfinished)
    (graph_switch_edges _ _ _ Hfinished) b initial_regs
    {| current_block := b; remaining := []; current_registers := regs |}
    Hmembers Hbounded Hrun eq_refl) as Htaken.
  cbn [current_registers] in Htaken.
  pose proof (certificates_counters_in_range blocks entry defs certificates
    initial_regs _ Hfinished Hsize Hcertificates Hbounded Hrun) as Hcounters.
  cbn in Hcounters.
  pose proof (check_passes_sound (snapshot_value regs)
    (snapshot_value_bounded regs) Heqb (snapshot_operation_semantics regs)
    certificates b cond cmp index l
    (check_atoms_fit blocks b certificates Hsize Hnodup Hmembers Hcount)
    Hcheck Hpasses Htaken Hcounters) as Htrue.
  pose proof (unsigned_check_value (snapshot_value regs)
    (snapshot_operation_semantics regs) cond cmp index l Hcheck) as Hcompare.
  unfold comparison_value in Hcompare. rewrite Htrue in Hcompare.
  rewrite Hsnapshot in Hcompare. rewrite Hcompare in Hread.
  eapply Step_switch with (targets := [ifnot; target]) (index_word := 1);
    [exact Hterm | exact Hread | lia | reflexivity |].
  eapply switch_parameterless;
    [exact (graph_layout _ _ _ Hfinished) | exact Hb | exact Hterm |].
  right. left. reflexivity.
Qed.
