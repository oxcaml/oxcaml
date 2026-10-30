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

(* Entry guards justified by actual branch execution, not a run record.

   The extra graph contract [switch_edge_multiplicity] states that repeated
   Switch targets produce repeated predecessor entries. Membership alone is
   insufficient: a Switch to [next; next] establishes no boolean fact.
   Dominance completeness and graph closure are ordinary metadata contracts. No
   guard truth or per-iteration value equation is assumed here.

   This proof-only module connects AffineSsa's guards to the mathematical
   snapshot of LoopValueSnapshot.v. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64.
From OxCamlBackend Require Import Operation IntegerComparison.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaContext.
From OxCamlLoop Require Import AffineSsa LoopValueSnapshot.
Import ListNotations.
Local Open Scope Z_scope.

(* Read back the singleton predecessor test without discarding its multiplicity
   or the exclusion of the graph entry. *)
Lemma sole_predecessor_structure : ∀ (source target : block),
  sole_predecessor source target = true ->
  dominates target source = false ∧ predecessors target = [source].
Proof.
  intros source target Hsole. unfold sole_predecessor in Hsole.
  apply andb_true_iff in Hsole. destruct Hsole as [Hdom Hpreds].
  apply negb_true_iff in Hdom. split; [exact Hdom |].
  destruct (predecessors target) as [ | pred [ | extra rest]] eqn:Hpred;
    try discriminate.
  apply block_eqb_spec in Hpreds. subst pred. reflexivity.
Qed.

(* A singleton predecessor list rules out the ambiguous duplicate-target
   Switch, because the builder records both incoming edge occurrences. *)
Lemma sole_predecessor_switch_distinct : switch_edge_multiplicity ->
  ∀ (source ifnot ifso : block) (condition : value) (negate : bool),
  terminator source = Terminator.Switch condition [ifnot; ifso] ->
  sole_predecessor source (if negate then ifnot else ifso) = true ->
  ifnot <> ifso.
Proof.
  intros Hmultiplicity source ifnot ifso condition negate Hterm Hsole Heq.
  subst ifso. destruct negate; cbn in Hsole;
    destruct (sole_predecessor_structure _ _ Hsole) as [_ Hpreds];
    pose proof (Hmultiplicity _ _ _ ifnot Hterm) as Hcount;
    rewrite Hpreds in Hcount;
    cbn [filter] in Hcount;
    rewrite !block_eqb_refl in Hcount; cbn in Hcount; lia.
Qed.

(* The accepted guard identifies a specific successor, not merely a block
   occurring somewhere among the targets. *)
Lemma guard_test_structure : ∀ (target source : block)
    (condition : value) (negate : bool),
  guard_test target source = Some (condition, negate) ->
  ∃ (ifnot ifso : block),
    terminator source = Terminator.Switch condition [ifnot; ifso]
    ∧ dominates (if negate then ifnot else ifso) target = true
    ∧ sole_predecessor source (if negate then ifnot else ifso) = true.
Proof.
  intros target source condition negate Hguard. unfold guard_test in Hguard.
  destruct (terminator source) as [ | index targets | |] eqn:Hterm;
    try discriminate.
  destruct targets as [ | ifnot [ | ifso [ | extra rest]]];
    try discriminate.
  destruct (dominates ifso target && sole_predecessor source ifso)
    eqn:Hso.
  - injection Hguard as <- <-. exists ifnot, ifso.
    apply andb_true_iff in Hso. auto.
  - destruct (dominates ifnot target && sole_predecessor source ifnot)
      eqn:Hnot; [ | discriminate].
    injection Hguard as <- <-. exists ifnot, ifso.
    apply andb_true_iff in Hnot. auto.
Qed.

Section Execution.
  Variable blocks : list block.
  Variable entry : block.
  Hypothesis edges : control_edges.
  Hypothesis well_formed : well_formed_ssa blocks entry.
  Hypothesis dominance : dominance_semantics entry.
  Hypothesis dominance_complete : ∀ (dominator target : block),
    dominator ∈ blocks -> target ∈ blocks ->
    (∀ (visited_blocks : list block), SsaDominance.block_path_from entry
      visited_blocks target ->
       dominator ∈ visited_blocks) -> dominator ▷ target.

  (* The checked non-dominance is also non-dominance of program points.
     Completeness matters here: soundness alone says nothing about false. *)
  Lemma sole_predecessor_not_program_point_dominator : ∀ (source target :
    block),
    source ∈ blocks -> target ∈ blocks ->
    sole_predecessor source target = true ->
    ~ program_point_dominates entry (block_entry_point target) (source, 0%nat).
  Proof.
    intros source target Hsource Htarget Hsole Hdom.
    destruct (sole_predecessor_structure _ _ Hsole) as [Hfalse _].
    assert (dominates target source = true) as Htrue.
    { apply dominance_complete; [exact Htarget | exact Hsource |].
      eapply block_dominance_of_program_point; eassumption. }
    congruence.
  Qed.

  (* On an actual transfer into the singleton successor, the Switch index
     selects exactly that edge. Its two targets must be distinct. *)
  Lemma guard_entry_word : ∀ (source ifnot ifso : block)
      (condition : value) (negate : bool) (before after : state),
    terminator source = Terminator.Switch condition [ifnot; ifso] ->
    predecessors (if negate then ifnot else ifso) = [source] ->
    ifnot <> ifso -> valid_position before -> step before after ->
    state_program_point after = block_entry_point (if negate then ifnot else
      ifso) ->
    current_registers after (register_of_value condition)
      = Some (if negate then 0 else 1).
  Proof.
    intros source ifnot ifso condition negate before after Hterm Hpreds
      Hdistinct Hposition Hstep Hentry.
    assert (Hsource : current_block before = source).
    { destruct Hstep; cbn [state_program_point enter block_entry_point] in
      Hentry.
      - inversion Hentry; subst. destruct Hposition as [prefix Hbody].
        cbn in Hbody. rewrite Hbody, length_app in *. cbn in *. lia.
      - injection Hentry as Htarget Hlength.
        pose proof (goto_successor edges _ _ _ H) as Hsucc.
        apply (proj2 (predecessor_successor edges _ _)) in Hsucc.
        rewrite Htarget, Hpreds in Hsucc. destruct Hsucc as [<- | []].
        reflexivity.
      - injection Hentry as Htarget Hlength.
        pose proof (switch_successor edges _ _ _ _ H
          (nth_error_In _ _ H2)) as Hsucc.
        apply (proj2 (predecessor_successor edges _ _)) in Hsucc.
        rewrite Htarget, Hpreds in Hsucc. destruct Hsucc as [<- | []].
        reflexivity. }
    destruct Hstep as [b insn rest rs rs' H |
      b target args rs zs rs' H H0 H1 |
      b index targets rs n target H H0 H1 H2 H3];
      cbn in Hsource; subst b.
    - cbn [state_program_point block_entry_point] in Hentry. inversion Hentry;
      subst.
      destruct Hposition as [prefix Hbody]. cbn in Hbody.
      rewrite Hbody, length_app in *. cbn in *. lia.
    - rewrite Hterm in H. discriminate.
    - rewrite Hterm in H. injection H as <- <-.
      cbn [state_program_point enter block_entry_point] in Hentry.
      injection Hentry as Htarget Hlength. subst target.
      assert (Hindex : n = if negate then 0 else 1).
      { assert (Hlen : (Z.to_nat n < 2)%nat).
        { change (Z.to_nat n < length [ifnot; ifso])%nat.
          apply nth_error_Some. rewrite H2. discriminate. }
        assert (Hrange : 0 <= n < 2) by lia.
        assert (Hcases : n = 0 ∨ n = 1) by lia.
        destruct negate; destruct Hcases as [-> | ->];
          cbv in H2; cbn; congruence. }
      cbn. rewrite <- Hindex. exact H0.
  Qed.

  (* Opaque context effects cannot fabricate entry into a guard's sole
     successor. Its sole predecessor is the actual Switch source. *)
  Lemma context_guard_entry_word : ∀ (source ifnot ifso : block)
      (condition : value) (negate : bool) (before after : state),
    terminator source = Terminator.Switch condition [ifnot; ifso] ->
    predecessors (if negate then ifnot else ifso) = [source] ->
    ifnot <> ifso -> valid_position before -> context_step before after ->
    state_program_point after = block_entry_point (if negate then ifnot else
      ifso) ->
    current_registers after (register_of_value condition)
      = Some (if negate then 0 else 1).
  Proof.
    intros source ifnot ifso condition negate before after Hterm Hpreds
      Hdistinct Hposition Hstep Hentry.
    destruct Hstep.
    - eapply guard_entry_word; eassumption.
    - cbn [state_program_point block_entry_point] in Hentry.
      injection Hentry as Hblock Hlength.
      destruct Hposition as [prefix Hbody]. cbn in Hbody.
      rewrite Hblock in Hbody. rewrite Hbody, length_app in Hlength.
      cbn in Hlength. lia.
    - cbn [state_program_point block_entry_point enter] in Hentry.
      injection Hentry as Htarget Hlength.
      apply (proj2 (predecessor_successor edges _ _)) in H0.
      rewrite Htarget, Hpreds in H0. destruct H0 as [<- | []].
      rewrite Hterm in H. discriminate.
    - exfalso. eapply trap_marker_not_entry; eassumption.
  Qed.

  (* A guard's result cannot be redefined while its selected successor
     dominates execution: its definition already dominates the branch, whereas
     that successor does not. *)
  Lemma executed_guard_word : ∀ (source ifnot ifso : block)
      (condition : value) (negate : bool) (initial_regs : registers)
      (reached : state),
    source ∈ blocks ->
    terminator source = Terminator.Switch condition [ifnot; ifso] ->
    sole_predecessor source (if negate then ifnot else ifso) = true ->
    ifnot <> ifso -> context_steps (enter entry initial_regs) reached ->
    program_point_dominates entry (block_entry_point (if negate then ifnot else
      ifso))
      (state_program_point reached) ->
    current_registers reached (register_of_value condition)
      = Some (if negate then 0 else 1).
  Proof.
    intros source ifnot ifso condition negate initial_regs reached Hsource
      Hterm Hsole Hdistinct Hsteps Hdom.
    set (target := if negate then ifnot else ifso) in *.
    assert (Htarget : target ∈ blocks).
    { eapply graph_successors_closed; [exact well_formed | exact Hsource |].
      eapply switch_successor; [exact edges | exact Hterm |].
      unfold target. destruct negate; cbn; auto. }
    pose proof (sole_predecessor_not_program_point_dominator source target
      Hsource
      Htarget Hsole) as Hnotdom.
    destruct (sole_predecessor_structure _ _ Hsole) as [Hfalse Hpreds].
    destruct (terminator_definitions _ _ well_formed source [condition]
      Hsource ltac:(rewrite Hterm; cbn; auto) condition ltac:(cbn; auto))
      as [site [Hdefinition [Hsite Hsite_dom]]].
    assert (Hsite_not_dominated :
      ~ program_point_dominates entry (block_entry_point target) site).
    { intros Hbad. apply Hnotdom.
      eapply program_point_dominance_transitive; eassumption. }
    assert (Hprop : forall (before after : state),
      context_steps before after ->
      valid_position before -> (current_block before) ∈ blocks ->
      (program_point_dominates entry (block_entry_point target)
        (state_program_point before) ->
        current_registers before (register_of_value condition)
          = Some (if negate then 0 else 1)) ->
      program_point_dominates entry (block_entry_point target)
        (state_program_point after) ->
      current_registers after (register_of_value condition)
        = Some (if negate then 0 else 1)).
    { intros before after Hrun.
      induction Hrun as [s | s t u H Hrun IH];
        intros Hposition Hin Hbefore Hafter; [apply Hbefore; exact Hafter |].
      apply IH; [eapply context_step_valid_position; eassumption |
        eapply context_step_in_graph; eassumption | | exact Hafter].
      intros Hnext.
      destruct (program_point_eqb (state_program_point t)
        (block_entry_point target)) eqn:Heq.
      - apply program_point_eqb_spec in Heq.
        eapply context_guard_entry_word; eassumption.
      - assert (Hdifferent : block_entry_point target <> state_program_point
        t).
        { intros Hequal. rewrite Hequal in Heq.
          rewrite (proj2 (program_point_eqb_spec _ _) eq_refl) in Heq.
            discriminate. }
        rewrite (context_step_definition_frame blocks entry s t condition site
          well_formed Hposition Hin Hdefinition Hsite
          ltac:(intros ->; contradiction) H).
        apply Hbefore. eapply program_point_dominates_predecessor;
          [exact Hnext | exact Hdifferent |].
        eapply context_step_program_point_edge; eassumption. }
    eapply Hprop; [exact Hsteps | apply enter_valid_position |
      exact (entry_in_graph _ _ well_formed) | | exact Hdom].
    intros Hentry.
    specialize (Hentry [block_entry_point entry] (Program_point_path_one _)).
    destruct Hentry as [Heq | []].
    apply (f_equal fst) in Heq. cbn in Heq.
    rewrite <- Heq, (entry_dominates dominance source) in Hfalse.
    discriminate.
  Qed.

  (* At any point dominated by the selected successor, the guard's canonical
     condition is still available. *)
  Lemma guard_condition_available : ∀ (source target : block)
      (condition : value) (negate : bool) (position : program_point),
    source ∈ blocks -> guard_test target source = Some (condition, negate) ->
    fst position = target -> available_value blocks entry position condition.
  Proof.
    intros source target condition negate position Hsource Hguard Hposition.
    destruct (guard_test_structure _ _ _ _ Hguard)
      as [ifnot [ifso [Hterm [Hdom Hsole]]]].
    destruct (sole_predecessor_structure _ _ Hsole) as [Hfalse Hpreds].
    destruct (terminator_definitions _ _ well_formed source [condition]
      Hsource ltac:(rewrite Hterm; cbn; auto) condition ltac:(cbn; auto))
      as [site [Hdefinition [Hsite Hsite_dom]]].
    exists site. split; [exact Hdefinition |]. split; [exact Hsite |].
    eapply program_point_dominance_transitive; [exact Hsite_dom |].
    eapply program_point_dominance_transitive.
    - eapply singleton_predecessor_dominance;
        [exact edges | | exact Hpreds].
      intros Heq. rewrite Heq, (entry_dominates dominance source) in Hfalse.
      discriminate.
    - apply program_point_dominance_of_block; [exact edges |].
      rewrite Hposition. exact (dominates_spec dominance _ _ Hdom).
  Qed.
End Execution.

(* A comparison's executed boolean is its arithmetic test in the snapshot. The
   snapshot bridge is used only for available operands. *)
Lemma snapshot_comparison_result : ∀ (blocks : list block)
    (entry : block) (position : program_point) (regs : registers)
    (condition : value) (negate : bool),
  well_formed_ssa blocks entry -> coherent_at blocks entry regs position ->
  bounded_registers regs -> available_value blocks entry position condition ->
  regs (register_of_value condition) = Some (if negate then 0 else 1) ->
  comparison_matches_branch (snapshot_value regs) negate condition.
Proof.
  intros blocks entry position regs condition negate Hwf Hcoherent Hbounded
    Havailable Hword.
  pose proof (Hcoherent _ Havailable) as Hvalue.
  destruct condition as [id op args index | b index]; [ | exact I].
  destruct op; try exact I; try (destruct op; try exact I);
    destruct args as [ | lhs [ | rhs [ | extra rest]]]; try exact I.
  - destruct Hvalue as [words [result [Hread [Heval Hresult]]]].
    cbn [read_values] in Hread.
    destruct (regs (register_of_value lhs)) as [lhs_word |] eqn:Hlhs;
      [ | discriminate].
    destruct (regs (register_of_value rhs)) as [rhs_word |] eqn:Hrhs;
      [ | discriminate].
    injection Hread as <-. cbn [eval_operation] in Heval.
    injection Heval as <-.
    cbn [comparison_matches_branch].
    rewrite (snapshot_read_available blocks entry position regs lhs lhs_word
      Hwf Hcoherent Hbounded ltac:(eapply operand_available; eauto;
        cbn; auto) Hlhs).
    rewrite (snapshot_read_available blocks entry position regs rhs rhs_word
      Hwf Hcoherent Hbounded ltac:(eapply operand_available; eauto;
        cbn; auto) Hrhs).
    cbn [register_of_value] in Hword. rewrite Hword in Hresult.
    unfold comparison_value in Hresult.
    destruct (IntegerComparison.eval cmp lhs_word rhs_word), negate;
      cbn in *; congruence.
  - destruct Hvalue as [words [result [Hread [Heval Hresult]]]].
    cbn [read_values] in Hread.
    destruct (regs (register_of_value lhs)) as [word |] eqn:Hlhs;
      [ | discriminate].
    injection Hread as <-. cbn [eval_operation] in Heval.
    injection Heval as <-. cbn [comparison_matches_branch].
    rewrite (snapshot_read_available blocks entry position regs lhs word
      Hwf Hcoherent Hbounded ltac:(eapply operand_available; eauto;
        cbn; auto) Hlhs).
    cbn [register_of_value] in Hword. rewrite Hword in Hresult.
    unfold comparison_value in Hresult.
    destruct (IntegerComparison.eval cmp word (to_Z k)), negate;
      cbn in *; congruence.
Qed.

(* Every accepted entry guard holds in the snapshot reached by an actual
   execution. Unlike the old branch-run premise this handles duplicate Switch
   targets through the builder's edge-occurrence metadata. *)
Theorem executed_guards_taken : ∀ (blocks : list block) (entry : block),
  control_edges -> well_formed_ssa blocks entry -> dominance_semantics entry ->
  (∀ (dominator target : block),
    dominator ∈ blocks -> target ∈ blocks ->
    (∀ (visited_blocks : list block), SsaDominance.block_path_from entry
      visited_blocks target ->
       dominator ∈ visited_blocks) -> dominator ▷ target) ->
  switch_edge_multiplicity ->
  ∀ (target : block) (initial_regs : registers) (reached : state),
  (∀ (dominator : block), dominator ∈ (strict_dominators target) ->
     dominator ∈ blocks) ->
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  current_block reached = target ->
  guards_taken (snapshot_value (current_registers reached)) target.
Proof.
  intros blocks entry Hedges Hwf Hdominance Hcomplete Hmultiplicity target
    initial_regs reached Hdominators Hbounded Hsteps Hblock source Hsource
    condition negate Hguard.
  specialize (Hdominators source Hsource).
  destruct (guard_test_structure _ _ _ _ Hguard)
    as [ifnot [ifso [Hterm [Hdom Hsole]]]].
  eapply snapshot_comparison_result.
  - exact Hwf.
  - eapply context_execution_values_coherent; eassumption.
  - exact (proj2 (context_execution_invariants _ _ _ Hbounded Hsteps)).
  - eapply guard_condition_available; eassumption.
  - eapply executed_guard_word; try eassumption.
    + eapply sole_predecessor_switch_distinct; eassumption.
    + apply program_point_dominance_of_block; [exact Hedges |].
      change (forall block_path_from : list block,
        SsaDominance.block_path_from entry block_path_from (current_block
          reached) ->
        (if negate then ifnot else ifso) ∈ block_path_from).
      rewrite Hblock. exact (dominates_spec Hdominance _ _ Hdom).
Qed.
