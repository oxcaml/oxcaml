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

(* What the analysis certifies of a check [find_eliminations]
   (BoundsCheckElim.v) removes, and the proofs. Nothing here needs review:
   the statement of the development (BoundsCheckExecutionSpec.v) does not
   mention the certificate, so if it were too weak the proof of the statement
   would not go through, and Rocq checks the proofs.

   The results are [find_eliminations_spec]: a check is removed only when
   [elimination_certificate] holds of it; [find_eliminations_supported],
   which reads back the applicability gate of DeleteEmptyLoops.v; and
   [check_passes_sound]: under any reading of the values on which the
   dominating branches went as taken and the certified counters are within
   their bounds, the comparison is true. The operational root,
   BoundsCheckExecutionProofs.find_eliminations_sound, turns these into the
   statement: in every execution reaching the branch, it is taken towards the
   target returned. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlInt64 OxCamlInt64Proofs OxCamlList OxCamlListProofs.
From OxCamlBackend Require Import IntegerComparison IntegerComparisonProofs
  Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaGraphProofs
  SsaSemanticsSpec SsaSemantics SsaSemanticsProofs.
From OxCamlLoop Require Import FourierMotzkin FourierMotzkinProofs AffineSsa
  AffineSsaProofs NaturalLoop NaturalLoopProofs InductionVar
  InductionVarProofs DeleteEmptyLoops DeleteEmptyLoopsProofs InductionRange
  InductionRangeProofs BoundsCheckElim.
Import ListNotations.

(* ======================================================================== *)
(* The intended meaning                                                     *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* The value a limit stands for, given a valuation of SSA values. *)
Definition limit_value (value_words : value -> Z) (l : limit) : Z :=
  match l with
  | Value_limit v => value_words v
  | Const_limit k => to_Z k
  end.

(* What holds of a block [b] whose branch is replaced by a jump to [target]:
   [b] ends in a two-way branch on a comparison [unsigned_check] reads as
   [index cmp limit], with [target] its true edge; the certificates used
   satisfy [certificate_spec] (InductionRange.v) and are few enough for the
   atom counter; and [check_passes] accepted the check with them. *)
Definition elimination_certificate (op_def : op_def) (entry : block)
    (blocks : list block) (b target : block) : Prop :=
  ∃ (cond : value) (ifnot : block) (cmp : integer_comparison) (index : value)
    (limit : limit) (certificates : list range_certificate),
    terminator b = Terminator.Switch cond [ifnot; target]
    ∧ unsigned_check cond = Some (cmp, index, limit)
    ∧ (∀ (cert : range_certificate), cert ∈ certificates ->
         certificate_spec op_def entry blocks cert)
    ∧ Z.of_nat (length certificates) <= 2 ^ 32
    ∧ check_passes certificates b cond = true.

(* The atoms [check_passes] may allocate: two linearizations, the guards of
   the block and the range facts. *)
Definition check_atom_bound (certificates : list range_certificate)
    (b : block) : Z :=
  2 * linearize_atom_bound + guards_atom_bound b
  + range_atom_bound certificates.

(* ======================================================================== *)
(* Lemmas                                                                   *)
(* ======================================================================== *)

(* What [check_goals] builds: the index form itself, and the room below the
   limit, less one for a strict comparison. *)
Lemma check_goals_spec : ∀ (cmp : integer_comparison)
    (index limit : Affine.t) (goals : list Affine.t),
  wf index -> wf limit ->
  check_goals cmp index limit = Some goals ->
  (cmp = Cult ∨ cmp = Cule) ∧ Forall wf goals
  ∧ ∀ (rho : valuation), Forall (holds rho) goals ->
      0 <= denote rho index
      ∧ denote rho index + (match cmp with Cult => 1 | _ => 0 end)
         <= denote rho limit.
Proof.
  intros cmp index limit goals Hwfi Hwfl H. unfold check_goals in H.
  destruct (Affine.sub_checked limit index) as [room | ] eqn:Hroom;
    [ | discriminate ].
  cbn in H.
  destruct (sub_checked_spec (fun _ => 0) _ _ _ Hwfl Hwfi Hroom)
    as [_ [_ Hwfr]].
  destruct cmp; try discriminate.
  - destruct (Affine.add_const_checked room minus_one) as [room' | ]
      eqn:Hroom'; [ | discriminate ].
    cbn in H. injection H as <-.
    destruct (add_const_checked_spec (fun _ => 0) _ _ _ Hroom')
      as [_ [_ Hwfr']].
    split; [ left; reflexivity | ].
    split; [ repeat constructor; auto | ].
    intros rho Hgoals. inversion Hgoals as [ | ? ? Hindex Hrest ]; subst.
    inversion Hrest as [ | ? ? Hroom_holds _ ]; subst.
    unfold holds in *.
    destruct (add_const_checked_spec rho _ _ _ Hroom') as [Hd' _].
    destruct (sub_checked_spec rho _ _ _ Hwfl Hwfi Hroom) as [Hd _].
    rewrite Hd', Hd, to_Z_minus_one in Hroom_holds. cbn. lia.
  - injection H as <-.
    split; [ right; reflexivity | ].
    split; [ repeat constructor; auto | ].
    intros rho Hgoals. inversion Hgoals as [ | ? ? Hindex Hrest ]; subst.
    inversion Hrest as [ | ? ? Hroom_holds _ ]; subst.
    unfold holds in *.
    destruct (sub_checked_spec rho _ _ _ Hwfl Hwfi Hroom) as [Hd _].
    rewrite Hd in Hroom_holds. cbn. lia.
Qed.

(* An unsigned comparison of two values in [[0, 2^63)] is the signed one. *)
Lemma unsigned64_nonnegative : ∀ (z : Z), 0 <= z -> unsigned64 z = z.
Proof.
  intros z Hz. unfold unsigned64.
  destruct (z <? 0) eqn:Hneg; [ | reflexivity ].
  apply Z.ltb_lt in Hneg. lia.
Qed.

Section Semantics.
  Variable value_words : value -> Z.
  Hypothesis value_words_bounded : ∀ (v : value), in_int64 (value_words v).
  Hypothesis value_words_respect_equality :
    ∀ (v w : value),
    value_eqb v w = true -> value_words v = value_words w.
  Hypothesis value_words_follow_operations :
    ∀ (v : value), op_semantics value_words v.

  (* What [unsigned_check] reads back: the comparison's machine value is the
     comparison of the index with the limit. *)
  Lemma unsigned_check_value : ∀ (cond : value) (cmp : integer_comparison)
      (index : value) (l : limit),
    unsigned_check cond = Some (cmp, index, l) ->
    value_words cond
      = comparison_value cmp (value_words index) (limit_value value_words l).
  Proof.
    intros cond cmp index l H. unfold unsigned_check in H.
    pose proof (value_words_follow_operations cond) as Hop.
    destruct cond as [id op args i | b i]; [ | discriminate ].
    destruct op; try discriminate; destruct op; try discriminate;
      destruct args as [ | a [ | b [ | extra rest ] ] ]; try discriminate;
      cbn [op_semantics modeled_operation map eval_operation] in Hop;
      specialize (Hop eq_refl); injection Hop as Hop.
    - destruct cmp0; try discriminate; injection H as <- <- <-;
        cbn [limit_value swap]; rewrite <- Hop; try reflexivity;
        unfold comparison_value; rewrite swap_correct; reflexivity.
    - destruct cmp0; try discriminate; injection H as <- <- <-;
        cbn [limit_value]; rewrite <- Hop; reflexivity.
  Qed.

  (* The limit's form is exact, within the usual counter bound. *)
  Lemma limit_form_sound : ∀ (l : limit) (initial_ctx : ctx)
      (initial_side_facts : list Affine.t) (form : Affine.t)
      (final_side_facts : list Affine.t) (final_ctx : ctx),
    ctx_wf initial_ctx ->
    to_Z initial_ctx.(next) + linearize_atom_bound <= ocaml_max_int ->
    limit_form initial_ctx initial_side_facts l
      = ((form, final_side_facts), final_ctx) ->
    ctx_wf final_ctx ∧ extends initial_ctx final_ctx
    ∧ to_Z final_ctx.(next) <= to_Z initial_ctx.(next) + linearize_atom_bound
    ∧ wf form
    ∧ (Forall wf initial_side_facts -> Forall wf final_side_facts)
    ∧ ∀ (rho : valuation), agrees value_words rho final_ctx ->
         denote rho form = limit_value value_words l
         ∧ (holds_all rho initial_side_facts ->
             holds_all rho final_side_facts).
  Proof.
    intros l c side form side' c' Hwf Hroom H. destruct l as [v | k].
    - exact (linearize_sound value_words value_words_bounded
        value_words_respect_equality value_words_follow_operations v c side
        form side' c' Hwf Hroom H).
    - cbn in H. injection H as <- <- <-.
      split; [ exact Hwf | split; [ apply extends_refl | ] ].
      split; [ unfold linearize_atom_bound; rewrite to_Z_max_nodes; lia | ].
      split; [ apply constant_wf | split; [ auto | ] ].
      intros rho _. split; [ apply constant_denote | auto ].
  Qed.

(* ======================================================================== *)
(* Theorems                                                                 *)
(* ======================================================================== *)

  (* When [check_passes] accepts a check and the facts it relies on are true of
     the values, the comparison is true: the dominating branches went as taken
     and the certified counters are within their bounds. *)
  Theorem check_passes_sound : ∀ (certificates : list range_certificate)
      (b : block) (cond : value) (cmp : integer_comparison) (index : value)
      (l : limit),
    check_atom_bound certificates b <= ocaml_max_int ->
    unsigned_check cond = Some (cmp, index, l) ->
    check_passes certificates b cond = true ->
    guards_taken value_words b ->
    counters_in_range value_words certificates b ->
    IntegerComparison.eval cmp (value_words index) (limit_value value_words l)
      = true.
  Proof.
    intros certificates b cond cmp index l Hroom Hcheck Hpasses Htaken
      Hcounters.
    unfold check_passes in Hpasses. rewrite Hcheck in Hpasses.
    destruct (linearize_goal new_ctx index) as [index_form c1] eqn:Hindex.
    destruct (limit_form c1 [] l) as [[limit_form side1] c2] eqn:Hlimit.
    destruct (guards_at c2 side1 b) as [[guards side2] c3] eqn:Hguards.
    destruct (range_facts certificates b c3) as [ranges c4] eqn:Hranges.
    destruct (check_goals cmp index_form limit_form) as [goals | ] eqn:Hgoals;
      [ | discriminate ].
    unfold check_atom_bound in Hroom.
    pose proof (guard_list_atom_bound_nonnegative b (strict_dominators b))
      as Hguards_nonnegative.
    assert (Hranges_nonnegative : 0 <= range_atom_bound certificates)
      by (unfold range_atom_bound; rewrite linearize_atom_bound_eq; lia).
    assert (Hn0 : to_Z new_ctx.(next) = 0) by exact to_Z_zero.
    pose proof linearize_atom_bound_eq as Hbound.
    assert (Hmax : ocaml_max_int = 4611686018427387903) by reflexivity.
    destruct (linearize_goal_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations index
      new_ctx index_form c1 new_ctx_wf ltac:(lia) Hindex)
      as [Hwf1 [_ [Hn1 [Hwfi Hrho1]]]].
    destruct (limit_form_sound l c1 [] limit_form side1 c2 Hwf1 ltac:(lia)
      Hlimit) as [Hwf2 [He12 [Hn2 [Hwfl [Hwfs1 Hrho2]]]]].
    destruct (guards_at_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations b c2 side1
      guards side2 c3 Hwf2 ltac:(lia) Hguards)
      as [Hwf3 [He23 [Hn3 [Hwfg [Hwfs2 Hrho3]]]]].
    destruct (range_facts_sound value_words value_words_bounded
      value_words_respect_equality value_words_follow_operations
      certificates b c3 ranges c4 Hwf3 ltac:(lia) Hranges)
      as [Hwf4 [He34 [_ [Hwfr Hrho4]]]].
    destruct (agrees_exists value_words c4 Hwf4) as [rho Hrho].
    pose proof (agrees_extends value_words rho _ _ He34 Hrho) as Hrho3'.
    pose proof (agrees_extends value_words rho _ _ He23 Hrho3') as Hrho2'.
    pose proof (agrees_extends value_words rho _ _ He12 Hrho2') as Hrho1'.
    destruct (Hrho3 rho Hrho3') as [Hside2 Hguards_hold].
    destruct (Hrho2 rho Hrho2') as [Hlimit_denote Hside1].
    assert (Hfacts : holds_all rho (ranges ++ guards ++ side2)).
    { unfold holds_all. apply Forall_app. split; [ apply Hrho4; assumption | ].
      apply Forall_app. split; [ apply Hguards_hold; exact Htaken | ].
      apply Hside2, Hside1. constructor. }
    assert (Hwffacts : Forall wf (ranges ++ guards ++ side2)).
    { apply Forall_app. split; [ exact Hwfr | ].
      apply Forall_app. split; [ exact Hwfg | ].
      apply Hwfs2, Hwfs1. constructor. }
    destruct (check_goals_spec _ _ _ _ Hwfi Hwfl Hgoals)
      as [Hcmp [Hwfgoals Hgoals_meaning]].
    assert (Hgoals_hold : Forall (holds rho) goals).
    { rewrite forallb_forall in Hpasses. apply Forall_forall.
      intros goal Hgoal.
      apply (entails_sound _ _ Hwffacts); [ | | exact Hfacts ].
      - rewrite Forall_forall in Hwfgoals. apply Hwfgoals. exact Hgoal.
      - apply Hpasses. exact Hgoal. }
    destruct (Hgoals_meaning rho Hgoals_hold) as [Hlow Hhigh].
    rewrite Hlimit_denote in Hhigh.
    assert (Hlimit_bounded : in_int64 (limit_value value_words l)).
    { destruct l as [v | k]; cbn [limit_value];
        [ apply value_words_bounded | ].
      pose proof (int_bounds k). unfold in_int64, two63, two62 in *. lia. }
    assert (Hindex_value : value_words index = denote rho index_form).
    { rewrite <- (Hrho1 rho Hrho1'). apply wrap64_id.
      unfold in_int64 in *. destruct cmp; cbn in Hhigh; lia. }
    rewrite Hindex_value.
    destruct Hcmp as [-> | ->]; cbn [IntegerComparison.eval] in *;
      rewrite (unsigned64_nonnegative _ Hlow);
      rewrite unsigned64_nonnegative by lia.
    - apply Z.ltb_lt. lia.
    - apply Z.leb_le. lia.
  Qed.
End Semantics.

(* Every reported elimination passed both applicability checks, as
   DeleteEmptyLoopsProofs.find_deletions_supported for deletions. *)
Theorem find_eliminations_supported :
  ∀ (defs : op_def) (entry : block) (blocks : list block) (b target : block),
  (b, target) ∈ (find_eliminations defs entry blocks) ->
  graph_size_supported blocks ∧
  ∀ (b : block) (id : instr_id) (op : operation) (args : list value)
    (count : int),
    b ∈ blocks -> (Op id op args count) ∈ (block_body b) ->
    modeled_operation op args = true -> count = one.
Proof.
  intros defs entry blocks b target Hin. unfold find_eliminations in Hin.
  destruct (graph_supported blocks) eqn:Hcheck; [ | contradiction ].
  apply graph_supported_spec. exact Hcheck.
Qed.

(* Every pair [(b, target)] that [find_eliminations] returns satisfies
   [elimination_certificate]. *)
Theorem find_eliminations_spec :
  ∀ (op_def : op_def) (entry : block) (blocks : list block)
    (b target : block),
  (b, target) ∈ (find_eliminations op_def entry blocks) ->
  elimination_certificate op_def entry blocks b target.
Proof.
  intros op_def entry blocks b target H. unfold find_eliminations in H.
  destruct (graph_supported blocks); [ | contradiction ]. cbv zeta in H.
  apply filter_map_In in H. destruct H as [b' [_ H]].
  unfold eliminate_check in H.
  destruct (terminator b') as [ | cond [ | ifnot [ | ifso [ | ] ] ] | | ]
    eqn:Hterm; try discriminate.
  destruct (check_passes _ b' cond) eqn:Hpasses; [ | discriminate ].
  injection H as <- <-.
  pose proof Hpasses as Hshape. unfold check_passes in Hshape.
  destruct (unsigned_check cond) as [[[cmp index] l] | ] eqn:Hcheck;
    [ | discriminate ].
  exists cond, ifnot, cmp, index, l,
    (if length_at_most graph_block_limit (find_certificates op_def entry
      blocks)
     then find_certificates op_def entry blocks else []).
  split; [ exact Hterm | split; [ exact Hcheck | ] ].
  destruct (length_at_most graph_block_limit _) eqn:Hlength.
  - split; [ | split; [ | exact Hpasses ] ].
    + intros cert Hcert. apply find_certificates_spec. exact Hcert.
    + apply length_at_most_spec in Hlength. exact Hlength.
  - split; [ | split; [ | exact Hpasses ] ].
    + intros cert Hcert. contradiction.
    + cbn. lia.
Qed.
