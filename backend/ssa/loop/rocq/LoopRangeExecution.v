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

(* Operational meaning of the affine entry-range check.

   LoopValueSnapshot constructs a mathematical interpretation of syntax;
   LoopGuardExecution derives its guard facts from executed Switches. This
   module connects the entailment check to an actual bound register. Its
   graph-size premise is discharged by the extracted applicability check, not a
   field asserting arithmetic properties of a loop run. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlSsa Require Import SsaModel SsaGraph SsaDominance SsaSemantics
  SsaExecution SsaContext.
From OxCamlLoop Require Import FourierMotzkin AffineExpr AffineSsa
  LoopComparisons LoopTermination LoopValueSnapshot LoopGuardExecution.
Import ListNotations.
Local Open Scope Z_scope.

(* One guard allocates at most two linearizations' worth of atoms. *)
Lemma guard_list_atom_bound_by_length : ∀ (target : block)
  (dominators : list block),
  guard_list_atom_bound target dominators <= comparison_atom_bound * Z.of_nat
    (length dominators).
Proof.
  intros target dominators. induction dominators as [ | source rest IH].
  - rewrite guard_list_atom_bound_nil. cbn. lia.
  - rewrite guard_list_atom_bound_cons. cbn [length]. rewrite Nat2Z.inj_succ.
    unfold guard_atom_bound. destruct (guard_test target source);
      rewrite comparison_atom_bound_eq in *; lia.
Qed.

(* The graph-size contract discharges the affine counter capacity bound; it
   does not assume anything about the values of loop counters. *)
Lemma graph_atoms_fit : ∀ (blocks : list block) (target : block),
  graph_size_supported blocks -> NoDup (strict_dominators target) ->
  (∀ (dominator : block), dominator ∈ (strict_dominators target) ->
     dominator ∈ blocks) ->
  linearize_atom_bound + guards_atom_bound target <= ocaml_max_int.
Proof.
  intros blocks target Hsize Hnodup Hmembers.
  pose proof (NoDup_incl_length Hnodup Hmembers) as Hlength.
  pose proof (guard_list_atom_bound_by_length target (strict_dominators
    target)) as Hguards.
  unfold graph_size_supported in Hsize.
  unfold guards_atom_bound. rewrite linearize_atom_bound_eq.
  rewrite comparison_atom_bound_eq in Hguards.
  unfold ocaml_max_int, two62. cbn in Hsize. lia.
Qed.

(* The bound form and all entry facts share one mathematical valuation. This
   proof uses no assumed operation equations: they are theorems of the snapshot
   construction, even on fabricated syntax. *)
Lemma snapshot_bound_facts : ∀ (regs : registers) (target : block)
    (bound_ref : value) (form : Affine.t) (bound_side_facts : list Affine.t)
      (bound_ctx : ctx)
    (guards all_side_facts : list Affine.t) (guard_ctx : ctx),
  linearize_atom_bound + guards_atom_bound target <= ocaml_max_int ->
  guards_taken (snapshot_value regs) target ->
  linearize new_ctx [] bound_ref = ((form, bound_side_facts), bound_ctx) ->
  guards_at bound_ctx bound_side_facts target = ((guards, all_side_facts),
    guard_ctx) ->
  ∃ (rho : valuation),
    denote rho form = snapshot_value regs bound_ref
    ∧ Forall wf (guards ++ all_side_facts) ∧ wf form
    ∧ holds_all rho (guards ++ all_side_facts).
Proof.
  intros regs target v form side1 c1 guards side2 c2 Hsmall Htaken Hlin Hg.
  assert (Heqb : forall (v w : value), value_eqb v w = true ->
    snapshot_value regs v = snapshot_value regs w).
  { intros lhs rhs Heq. apply value_eqb_spec in Heq. subst rhs. reflexivity. }
  assert (Hn0 : to_Z new_ctx.(next) = 0) by exact to_Z_zero.
  assert (Hgs : 0 <= guards_atom_bound target) by apply
    guard_list_atom_bound_nonnegative.
  destruct (linearize_sound (snapshot_value regs) (snapshot_value_bounded regs)
    Heqb (snapshot_operation_semantics regs) v new_ctx [] form side1 c1
    new_ctx_wf ltac:(lia) Hlin)
    as [Hwf1 [_ [Hn1 [Hwff [Hwfs1 Hrho1]]]]].
  destruct (guards_at_sound (snapshot_value regs) (snapshot_value_bounded regs)
    Heqb (snapshot_operation_semantics regs) target c1 side1 guards side2 c2
    Hwf1 ltac:(lia) Hg) as [Hwf2 [He12 [_ [Hwfg [Hwfs2 Hrho2]]]]].
  destruct (agrees_exists (snapshot_value regs) c2 Hwf2) as [rho Hrho].
  destruct (Hrho2 rho Hrho) as [Hs2 Hg2].
  destruct (Hrho1 rho (agrees_extends _ _ _ _ He12 Hrho)) as [Hden Hs1].
  exists rho. split; [exact Hden |].
  split; [apply Forall_app; split; [exact Hwfg |] |].
  { apply Hwfs2, Hwfs1. exact (Forall_nil _). }
  split; [exact Hwff |]. unfold holds_all. apply Forall_app. split.
  - apply Hg2. exact Htaken.
  - apply Hs2, Hs1. exact (Forall_nil _).
Qed.

(* Fourier-Motzkin entailment gives the arithmetic termination rule's range
   condition for the snapshot's bound. *)
Lemma snapshot_bound_in_range : ∀ (regs : registers) (target : block)
    (step_size : int) (comparison_bound : bound),
  linearize_atom_bound + guards_atom_bound target <= ocaml_max_int ->
  guards_taken (snapshot_value regs) target ->
  LoopTermination.bound_in_range step_size target comparison_bound = true ->
  LoopComparisons.bound_in_range (to_Z step_size)
    (bound_value (snapshot_value regs) comparison_bound) = true.
Proof.
  intros regs target step_size [v | constant] Hsmall Htaken Hcheck;
    [ | eapply constant_bound_in_range; exact Hcheck].
  unfold LoopComparisons.bound_in_range. rewrite Z.gtb_ltb.
  unfold LoopTermination.bound_in_range in Hcheck. cbv zeta in Hcheck.
  rewrite zero_ltb_to_Z in Hcheck. cbn [bound_value].
  destruct (linearize new_ctx [] v) as [[form side1] c1] eqn:Hlin.
  destruct (guards_at c1 side1 target) as [[guards side2] c2] eqn:Hg.
  destruct (snapshot_bound_facts regs target v form side1 c1 guards side2 c2
    Hsmall Htaken Hlin Hg) as [rho [Hden [Hwffacts [Hwff Hfacts]]]].
  destruct (0 <? to_Z step_size) eqn:Hpositive; cbv iota in Hcheck.
  - destruct (Affine.scale_checked minus_one form) as [negated |] eqn:Hscale;
      [ | discriminate]. cbv iota in Hcheck.
    destruct (Affine.add_const_checked negated max_int) as [goal |] eqn:Hadd;
      [ | discriminate]. cbv iota in Hcheck.
    destruct (scale_checked_spec rho _ _ _ Hscale) as [Hd1 [_ Hwf1]].
    destruct (add_const_checked_spec rho _ _ _ Hadd) as [Hd2 [_ Hwf2]].
    pose proof (entails_sound _ _ Hwffacts (Hwf2 (Hwf1 Hwff)) Hcheck rho
      Hfacts) as Hgoal.
    rewrite Hd2, Hd1, to_Z_minus_one, to_Z_max_int, Hden in Hgoal.
    apply Z.leb_le. unfold ocaml_max_int. lia.
  - destruct (Affine.add_const_checked form max_int) as [goal |] eqn:Hadd;
      [ | discriminate]. cbv iota in Hcheck.
    destruct (add_const_checked_spec rho _ _ _ Hadd) as [Hdenote [_ Hwfgoal]].
    pose proof (entails_sound _ _ Hwffacts (Hwfgoal Hwff) Hcheck rho Hfacts)
      as Hgoal.
    rewrite Hdenote, to_Z_max_int, Hden in Hgoal.
    apply Z.leb_le. unfold ocaml_max_int. lia.
Qed.

(* The register-bound range check is sound on an actual execution prefix. All
   arithmetic and guard facts have been discharged; the premises are graph
   metadata, resource size, a reached state and an actual read. *)
Theorem executed_bound_in_range : ∀ (blocks : list block) (entry : block),
  control_edges -> well_formed_ssa blocks entry -> dominance_semantics entry ->
  (∀ (dominator target : block),
    dominator ∈ blocks -> target ∈ blocks ->
    (∀ (visited_blocks : list block), SsaDominance.block_path_from entry
      visited_blocks target ->
       dominator ∈ visited_blocks) -> dominator ▷ target) ->
  switch_edge_multiplicity -> graph_size_supported blocks ->
  ∀ (target : block) (initial_regs : registers) (reached : state)
    (step_size : int) (bound_ref : value) (bound_word : Z),
  NoDup (strict_dominators target) ->
  (∀ (dominator : block), dominator ∈ (strict_dominators target) ->
     dominator ∈ blocks) ->
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  current_block reached = target ->
  available_value blocks entry (state_program_point reached) bound_ref ->
  current_registers reached (register_of_value bound_ref) = Some bound_word ->
  LoopTermination.bound_in_range step_size target (Value_bound bound_ref)
    = true ->
  LoopComparisons.bound_in_range (to_Z step_size) bound_word = true.
Proof.
  intros blocks entry Hedges Hwf Hdominance Hcomplete Hmultiplicity Hsize
    target initial_regs reached step_size v word Hnodup Hmembers Hbounded
    Hsteps Hblock Havailable Hread Hcheck.
  assert (Hsnapshot : snapshot_value (current_registers reached) v = word).
  { eapply snapshot_read_available; [exact Hwf | | | exact Havailable |
      exact Hread].
    - eapply context_execution_values_coherent; eassumption.
    - exact (proj2 (context_execution_invariants _ _ _ Hbounded Hsteps)). }
  rewrite <- Hsnapshot.
  change (LoopComparisons.bound_in_range (to_Z step_size)
    (bound_value (snapshot_value (current_registers reached)) (Value_bound v))
    = true).
  eapply snapshot_bound_in_range; [ | | exact Hcheck].
  - eapply graph_atoms_fit; eassumption.
  - eapply executed_guards_taken; eassumption.
Qed.
