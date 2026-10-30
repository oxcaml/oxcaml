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

(* The decisions on signed integer comparisons shared by the loop analyses: the
   orientation of a loop's continue test, whether an induction variable makes
   it fail, and the affine facts a comparison implies. The extraction of this
   file, LoopComparisons.ml next to it, is the implementation.

   Reading guide (no Rocq knowledge assumed):

   - [Inductive] declares a variant type, like OxCaml [type t = A | B].
   - [Definition] declares a function or constant, like OxCaml [let].
   - [Fixpoint] declares a recursive function, like OxCaml [let rec].
   - [Lemma] / [Theorem] state a property; the text between [Proof.] and [Qed.]
     is a script that the checker runs to establish it. The statements are what
     matter; the scripts can be skipped.
   - [Z] is the type of unbounded mathematical integers. [int] is Rocq's
     primitive signed 63-bit integer, which is exactly OxCaml's [int]; [to_Z]
     reads an [int] as a mathematical integer.
   - [a ▷ b] (SsaModel.v) says that block [a] dominates block [b].

   Shared material lives elsewhere: rocq/ at the root of the repository models
   OxCaml's own types (OxCamlInt.v, OxCamlInt64.v, OxCamlList.v,
   OxCamlOption.v), backend/rocq/IntegerComparison.v has
   [Cmm.integer_comparison] with [negate], [swap] and their semantics, and
   FourierMotzkin.v next to this file has [Affine], the affine forms.

   The file has four parts:
   1. The definitions.
   2. The intended meaning: how an induction variable (IV) advances with
     wrap-around, and what "the bound is in range" means.
   3. Lemmas used by the proofs. Stepping stones; skip on a first reading.
   4. The theorems. These are the correctness claims, each with its proof.

   Review parts 1 and 2 and the theorem statements: the definitions, semantics,
   assumptions and conclusions must match the intended claim. Rocq checks the
   proofs (see rocq/Makefile at the root of the repository).

   This file proves arithmetic facts, not optimizer correctness. The
   independent instruction semantics is in SsaSemantics.v, and structural SSA
   well-formedness is in SsaExecution.v. DeleteEmptyLoops.v states the analysis
   certificate. The operational root is
   LoopEligibility.find_deletions_eligible, proving finite first exit and
   live-out agreement from that decision and explicit graph/input contracts.
   SsaContext.v permits opaque computations in the unchanged prefix. The
   post-deletion graph and its cleanup are deliberately not modelled. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 ZTactics.
From OxCamlBackend Require Import IntegerComparison.
From OxCamlLoop Require Import FourierMotzkin.
Import ListNotations.

(* ======================================================================== *)
(* Part 1: the definitions                                                  *)
(* ======================================================================== *)

(* The comparison under which the loop continues, with the induction variable
   on the left: [cmp] is the test of the exit branch, [iv_is_left] whether the
   variable is its left operand, [continue_when_true] whether the loop
   continues when the test holds. *)
Definition oriented_continue_comparison
    (iv_is_left continue_when_true : bool) (cmp : integer_comparison)
    : integer_comparison :=
  let cmp := if iv_is_left then cmp else swap cmp in
  if continue_when_true then cmp else negate cmp.

(* The verdict on a continue comparison and a step. *)
Inductive termination : Type :=
  | Terminates
  | Terminates_if_bound_in_range
  | Unknown.

(* A [step : int] is a 63-bit signed integer. *)
Local Open Scope sint63_scope.
Local Notation "x >? y" := (y <? x)
  (at level 70, no associativity) : sint63_scope.

(* Whether an induction variable advancing by [step] must eventually fail the
   continue comparison [cmp] against a fixed bound: outright, only if the bound
   is in range (part 2), or not that this can tell. *)
Definition continue_terminates (step : int) (cmp : integer_comparison)
    : termination :=
  if step =? zero
  then Unknown
  else
    match cmp with
    | Ceq => Terminates
    | Clt =>
      if step =? one
      then Terminates
      else if step >? zero
      then Terminates_if_bound_in_range
      else Unknown
    | Cle => if step >? zero then Terminates_if_bound_in_range else Unknown
    | Cgt =>
      if step =? minus_one
      then Terminates
      else if step <? zero
      then Terminates_if_bound_in_range
      else Unknown
    | Cge => if step <? zero then Terminates_if_bound_in_range else Unknown
    | Cne | Cult | Cugt | Cule | Cuge => Unknown
    end.

(* [Affine], from FourierMotzkin.v, is the affine forms: a form [f] stands for
   the assertion [f >= 0], [denote rho f] is its integer value when the atoms
   take the values given by [rho], and [wf f] says that no atom occurs twice in
   it. The checked operations return [None] when a coefficient would leave the
   [int] range. *)

(* The facts implied by [left_form cmp right_form] holding ([negate_comparison
   = false]) or failing ([negate_comparison = true]): none when a coefficient
   overflows (the final [match]). *)
Definition facts (negate_comparison : bool) (cmp : integer_comparison)
    (left_form right_form : Affine.t) : list Affine.t :=
  let cmp := if negate_comparison then negate cmp else cmp in
  let computed : option (list Affine.t) :=
    match cmp with
    | Cge => let* f := Affine.sub_checked left_form right_form in Some [f]
    | Cgt =>
      let* d := Affine.sub_checked left_form right_form in
      let* f := Affine.add_const_checked d minus_one in
      Some [f]
    | Cle => let* f := Affine.sub_checked right_form left_form in Some [f]
    | Clt =>
      let* d := Affine.sub_checked right_form left_form in
      let* f := Affine.add_const_checked d minus_one in
      Some [f]
    | Ceq =>
      let* f1 := Affine.sub_checked left_form right_form in
      let* f2 := Affine.sub_checked right_form left_form in
      Some [f1; f2]
    | Cne | Cult | Cugt | Cule | Cuge => Some []
    end
  in
  match computed with
  | Some facts => facts
  | None => []
  end.

Local Close Scope sint63_scope.

(* ======================================================================== *)
(* Part 2: semantics                                                        *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* The value of an induction variable after [iterations] iterations: it starts
   at [initial_counter] and each iteration adds [step] with 64-bit wrap-around.
   *)
Fixpoint counter_after_iterations (initial_counter step : Z)
  (iterations : nat) : Z :=
  match iterations with
  | O => initial_counter
  | S m => wrap64 (counter_after_iterations initial_counter step m + step)
  end.

(* Whether the loop continues, given the shape of its exit branch: the header
   tests [counter cmp bound] (or [bound cmp counter] when the IV is the right
   operand) and continues on the [true] edge or on the [false] edge. This is
   the situation [Termination.biv_implies_termination] is in. *)
Definition loop_continues (iv_is_left continue_when_true : bool)
    (cmp : integer_comparison) (counter bound : Z) : bool :=
  let test := if iv_is_left then eval cmp counter bound else eval cmp bound
    counter in
  if continue_when_true then test else negb test.

(* The side condition attached to [Terminates_if_bound_in_range], as checked by
   [Termination.bound_in_range] for a literal bound (the register case proves
   the same inequality by Fourier-Motzkin). *)
Definition bound_in_range (step limit : Z) : bool :=
  if step >? 0 then limit <=? ocaml_max_int else - ocaml_max_int <=? limit.

(* ======================================================================== *)
(* Part 3: lemmas used by the proofs                                        *)
(* ======================================================================== *)

(* --- The IV sequence. --- *)

(* Iterating from [initial_counter] for [iterations + 1] steps is iterating
   from the next value for [iterations] steps. *)
Lemma counter_after_iterations_shift : ∀ (initial_counter step : Z)
  (iterations : nat),
  counter_after_iterations initial_counter step (S iterations) =
    counter_after_iterations (wrap64 (initial_counter + step)) step iterations.
Proof.
  intros v0 step n. induction n as [ | n IH ].
  - reflexivity.
  - cbn [counter_after_iterations] in *. rewrite IH. reflexivity.
Qed.

(* The termination argument, once and for all. If, whenever the test holds on a
   64-bit value [counter], the next value [counter + step] is again a 64-bit
   value (no wrap-around) and some integer measure [decreasing_measure]
   decreases while staying non-negative, then the test eventually fails. *)
Lemma terminate_by_measure :
  ∀ (test : Z -> bool) (step : Z) (decreasing_measure : Z -> Z),
    (∀ (counter : Z), in_int64 counter -> test counter = true ->
       in_int64 (counter + step) ∧ 0 <= decreasing_measure counter ∧
         decreasing_measure (counter + step) < decreasing_measure counter) ->
    ∀ (initial_counter : Z), in_int64 initial_counter -> ∃
      iterations, test (counter_after_iterations initial_counter step
      iterations) = false.
Proof.
  intros test step mu Hstep.
  (* Induction on a bound [m] for the measure. *)
  assert (forall (m : nat) v, in_int64 v -> mu v < Z.of_nat m ->
            exists n, test (counter_after_iterations v step n) = false) as
              Hind.
  { induction m as [ | m IH ]; intros v Hv Hm; destruct (test v) eqn:Ht.
    - destruct (Hstep v Hv Ht) as [_ [Hmu _]]. lia.
    - exists O. exact Ht.
    - destruct (Hstep v Hv Ht) as [Hr [Hmu Hdec]].
      destruct (IH (v + step) Hr ltac:(lia)) as [n Hn].
      exists (S n). rewrite counter_after_iterations_shift. rewrite (wrap64_id
        _ Hr). exact Hn.
    - exists O. exact Ht. }
  intros v0 Hv0.
  apply (Hind (Z.to_nat (mu v0) + 1)%nat v0 Hv0).
  rewrite Nat2Z.inj_add. pose proof (Nat2Z.is_nonneg (Z.to_nat (mu v0))).
  destruct (Z_le_gt_dec 0 (mu v0)) as [ Hge | Hlt ].
  - rewrite (Z2Nat.id _ Hge). lia.
  - lia.
Qed.

(* --- One lemma per verdict of [continue_terminates]. --- *)

(* [Ceq], any non-zero step: the test [v = bound] cannot hold twice in a row,
   since adding a non-zero 63-bit step to a 64-bit value changes it even with
   wrap-around. *)
Lemma ceq_terminates : ∀ (step initial_counter bound : Z),
  - two62 <= step <= two62 - 1 -> step <> 0 ->
  ∃ iterations, eval Ceq (counter_after_iterations initial_counter step
    iterations) bound = false.
Proof.
  intros step v0 bound Hs Hnz.
  destruct (Z.eqb_spec v0 bound) as [ E | E ].
  - exists 1%nat. subst v0.
    change ((wrap64 (bound + step) =? bound) = false).
    destruct (wrap64_sub_multiple (bound + step)) as [k Hk]. rewrite Hk.
    destruct (Z.eqb_spec (bound + step - k * two64) bound) as [ E | E ];
      [ | reflexivity ].
    exfalso. unfold two62, two64 in *.
    assert (k = 0) by lia. subst k. lia.
  - exists O. change ((v0 =? bound) = false).
    destruct (Z.eqb_spec v0 bound); [ contradiction | reflexivity ].
Qed.

(* [Clt], step 1: continuing means [v < bound], so [v + 1 <= bound] is again a
   64-bit value, and [bound - v] decreases. *)
Lemma clt_one_terminates : ∀ (initial_counter bound : Z),
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Clt (counter_after_iterations initial_counter 1
    iterations) bound = false.
Proof.
  intros v0 bound Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Clt v bound) 1
           (fun v => bound - v)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, two63 in *. lia.
Qed.

(* [Cgt], step -1: symmetric. *)
Lemma cgt_minus_one_terminates : ∀ (initial_counter bound : Z),
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Cgt (counter_after_iterations initial_counter (-1)
    iterations) bound = false.
Proof.
  intros v0 bound Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Cgt v bound) (-1)
           (fun v => v - bound)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, two63 in *. lia.
Qed.

(* [Clt], positive step, [bound <= max_int]: continuing means [v < bound <=
   2^62 - 1] and [step <= 2^62 - 1], so [v + step < 2^63]. *)
Lemma clt_pos_terminates : ∀ (step initial_counter bound : Z),
  0 < step <= two62 - 1 -> bound <= ocaml_max_int ->
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Clt (counter_after_iterations initial_counter step
    iterations) bound = false.
Proof.
  intros step v0 bound Hs Hbr Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Clt v bound) step
           (fun v => bound - v)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, ocaml_max_int, two62, two63 in *. lia.
Qed.

(* [Cle], positive step, [bound <= max_int]: as above with [v <= bound]. *)
Lemma cle_pos_terminates : ∀ (step initial_counter bound : Z),
  0 < step <= two62 - 1 -> bound <= ocaml_max_int ->
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Cle (counter_after_iterations initial_counter step
    iterations) bound = false.
Proof.
  intros step v0 bound Hs Hbr Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Cle v bound) step
           (fun v => bound - v + 1)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, ocaml_max_int, two62, two63 in *. lia.
Qed.

(* [Cgt], negative step, [bound >= -max_int]: symmetric. *)
Lemma cgt_neg_terminates : ∀ (step initial_counter bound : Z),
  - two62 <= step < 0 -> - ocaml_max_int <= bound ->
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Cgt (counter_after_iterations initial_counter step
    iterations) bound = false.
Proof.
  intros step v0 bound Hs Hbr Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Cgt v bound) step
           (fun v => v - bound)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, ocaml_max_int, two62, two63 in *. lia.
Qed.

(* [Cge], negative step, [bound >= -max_int]: symmetric. *)
Lemma cge_neg_terminates : ∀ (step initial_counter bound : Z),
  - two62 <= step < 0 -> - ocaml_max_int <= bound ->
  in_int64 initial_counter -> in_int64 bound ->
  ∃ iterations, eval Cge (counter_after_iterations initial_counter step
    iterations) bound = false.
Proof.
  intros step v0 bound Hs Hbr Hv0 Hb.
  apply (terminate_by_measure (fun v => eval Cge v bound) step
           (fun v => v - bound + 1)); [ | exact Hv0 ].
  intros v Hv Ht. unfold eval in Ht. zcmp_hyp Ht.
  unfold in_int64, ocaml_max_int, two62, two63 in *. lia.
Qed.

(* ======================================================================== *)
(* Part 4: theorems                                                         *)
(* ======================================================================== *)

(* The oriented comparison, evaluated with the IV on the left, is exactly the
   loop's continue test. *)
Theorem oriented_continue_comparison_correct :
  ∀ (iv_is_left continue_when_true : bool) (cmp : integer_comparison)
    (counter bound : Z),
    eval (oriented_continue_comparison iv_is_left continue_when_true cmp)
         counter bound
    = loop_continues iv_is_left continue_when_true cmp counter bound.
Proof.
  intros [] [] cmp v bound;
    unfold oriented_continue_comparison, loop_continues; cbv beta iota zeta;
    rewrite ?negate_correct, ?swap_correct; reflexivity.
Qed.

(* A [Terminates] verdict is sound: whatever the initial value and the bound
   (any 64-bit values), the continue test [counter_after_iterations cmp bound]
   fails after finitely many iterations of adding [step] with wrap-around. *)
Theorem continue_terminates_Terminates :
  ∀ (step : int) (cmp : integer_comparison) (initial_counter bound : Z),
    in_int64 initial_counter -> in_int64 bound ->
    continue_terminates step cmp = Terminates ->
    ∃ iterations, eval cmp (counter_after_iterations initial_counter (to_Z
      step) iterations) bound = false.
Proof.
  intros step cmp v0 bound Hv0 Hb.
  pose proof (int_bounds step) as Hs.
  unfold continue_terminates. destruct cmp; case_int_tests;
    intro H; cbv iota in H; try discriminate H; clear H;
    eauto using ceq_terminates, clt_one_terminates,
                cgt_minus_one_terminates.
Qed.

(* A [Terminates_if_bound_in_range] verdict is sound once the side condition on
   the bound holds. *)
Theorem continue_terminates_Terminates_if_bound_in_range :
  ∀ (step : int) (cmp : integer_comparison) (initial_counter bound : Z),
    in_int64 initial_counter -> in_int64 bound ->
    continue_terminates step cmp = Terminates_if_bound_in_range ->
    bound_in_range (to_Z step) bound = true ->
    ∃ iterations, eval cmp (counter_after_iterations initial_counter (to_Z
      step) iterations) bound = false.
Proof.
  intros step cmp v0 bound Hv0 Hb.
  pose proof (int_bounds step) as Hs.
  unfold continue_terminates. destruct cmp; case_int_tests;
    intro H; cbv iota in H; try discriminate H; clear H;
    intro Hr; unfold bound_in_range in Hr; rewrite Z.gtb_ltb in Hr;
    revert Hr; destruct (Z.ltb_spec 0 (to_Z step)); intro Hr; zcmp_hyp Hr;
    first [ apply clt_pos_terminates | apply cle_pos_terminates
          | apply cgt_neg_terminates | apply cge_neg_terminates ];
    first [ assumption | lia ].
Qed.

(* Putting the pieces together the way [Termination.biv_implies_termination]
   does: orient the comparison, ask [continue_terminates], discharge the side
   condition if asked to. Then the loop's continue test eventually fails, i.e.
   the loop exits. *)
Corollary loop_exits :
  ∀ (iv_is_left continue_when_true : bool) (cmp : integer_comparison)
    (step : int) (initial_counter bound : Z),
    in_int64 initial_counter -> in_int64 bound ->
    let continue_comparison := oriented_continue_comparison iv_is_left
      continue_when_true cmp
    in
    (continue_terminates step continue_comparison = Terminates
     ∨ (continue_terminates step continue_comparison =
       Terminates_if_bound_in_range
         ∧ bound_in_range (to_Z step) bound = true)) ->
    ∃ iterations,
      loop_continues iv_is_left continue_when_true cmp
        (counter_after_iterations initial_counter (to_Z step) iterations) bound
          = false.
Proof.
  intros iv_is_left continue_when_true cmp step v0 bound Hv0 Hb ccmp Hverdict.
  assert (exists n, eval ccmp (counter_after_iterations v0 (to_Z step) n) bound
    = false) as [n Hn].
  { destruct Hverdict as [ H | [H Hrange] ].
    - eapply continue_terminates_Terminates; eauto.
    - eapply continue_terminates_Terminates_if_bound_in_range; eauto. }
  exists n. rewrite <- oriented_continue_comparison_correct. exact Hn.
Qed.

(* What a successful checked operation means, from FourierMotzkin.v: its value,
   and that it keeps the forms well-formed. *)
Lemma sub_checked_denote : ∀ (rho : valuation)
  (left_form right_form difference : Affine.t),
  Affine.sub_checked left_form right_form = Some difference -> wf left_form ->
    wf right_form ->
  denote rho difference = denote rho left_form - denote rho right_form.
Proof.
  intros rho a b f H Ha Hb. exact (proj1 (sub_checked_spec rho a b f Ha Hb H)).
Qed.

Lemma sub_checked_wf : ∀ (left_form right_form difference : Affine.t),
  Affine.sub_checked left_form right_form = Some difference -> wf left_form ->
    wf right_form -> wf difference.
Proof.
  intros a b f H Ha Hb.
  exact (proj2 (proj2 (sub_checked_spec (fun _ => 0) a b f Ha Hb H))).
Qed.

Lemma add_const_checked_denote :
  ∀ (rho : valuation) (form : Affine.t) (constant : int)
    (result_form : Affine.t),
  Affine.add_const_checked form constant = Some result_form ->
  denote rho result_form = denote rho form + to_Z constant.
Proof.
  intros rho a c f H. exact (proj1 (add_const_checked_spec rho a c f H)).
Qed.

Lemma add_const_checked_wf : ∀ (form : Affine.t) (constant : int)
  (result_form : Affine.t),
  Affine.add_const_checked form constant = Some result_form ->
  wf form -> wf result_form.
Proof.
  intros a c f H Ha.
  exact (proj2 (proj2 (add_const_checked_spec (fun _ => 0) a c f H)) Ha).
Qed.

(* Proof helper: case-split on each overflow-checked operation in the goal. A
   successful operation yields its value and its well-formedness as hypotheses;
   an overflow leaves the empty list of facts. *)
Ltac checked_cases rho :=
  repeat match goal with
         | |- context [Affine.sub_checked ?a ?b] =>
           let E := fresh "E" in
           let W := fresh "W" in
           destruct (Affine.sub_checked a b) eqn:E;
           [ pose proof (sub_checked_wf _ _ _ E ltac:(assumption)
                           ltac:(assumption)) as W;
             apply (sub_checked_denote rho) in E; [ | assumption | assumption ]
           | ];
           cbv beta iota
         | |- context [Affine.add_const_checked ?a ?c] =>
           let E := fresh "E" in
           let W := fresh "W" in
           destruct (Affine.add_const_checked a c) eqn:E;
           [ pose proof (add_const_checked_wf _ _ _ E ltac:(assumption)) as W;
             apply (add_const_checked_denote rho) in E
           | ];
           cbv beta iota
         end.

(* [facts] is sound: if the (possibly negated) comparison [left_form cmp
   right_form] held, every returned form is non-negative. Nothing is assumed
   about overflow: when the checked operations fail, no fact is returned. *)
Theorem facts_sound :
  ∀ (negate_comparison : bool) (cmp : integer_comparison)
    (left_form right_form : Affine.t)
    (rho : int -> Z),
    wf left_form -> wf right_form ->
    (let test := eval cmp (denote rho left_form) (denote rho right_form) in
     if negate_comparison then negb test else test) = true ->
    Forall (fun f => 0 <= denote rho f) (facts negate_comparison cmp left_form
      right_form).
Proof.
  intros neg cmp la lb rho Hla Hlb Hcmp. cbv zeta in Hcmp.
  (* The negated test is the test of the negated comparison. *)
  replace (if neg
           then negb (eval cmp (denote rho la) (denote rho lb))
           else eval cmp (denote rho la) (denote rho lb))
    with (eval (if neg then negate cmp else cmp)
               (denote rho la) (denote rho lb)) in Hcmp
    by (destruct neg; cbv iota; [ apply negate_correct | reflexivity ]).
  unfold facts. cbv zeta.
  set (c := if neg then negate cmp else cmp) in *. clearbody c.
  destruct c; cbv beta iota; unfold eval in Hcmp; try zcmp_hyp Hcmp;
    checked_cases rho;
    repeat (apply Forall_cons || apply Forall_nil);
    repeat match goal with
           | E : denote rho _ = _ |- _ => rewrite E; clear E
           end;
    rewrite ?to_Z_minus_one; lia.
Qed.

(* The facts are well-formed, as [entails] requires of its hypotheses (see
   [entails_sound] in FourierMotzkin.v). *)
Theorem facts_wf :
  ∀ (negate_comparison : bool) (cmp : integer_comparison)
    (left_form right_form : Affine.t),
  wf left_form -> wf right_form -> Forall wf (facts negate_comparison cmp
    left_form right_form).
Proof.
  intros neg cmp la lb Hla Hlb. unfold facts. cbv zeta.
  destruct (if neg then negate cmp else cmp); cbv beta iota;
    checked_cases (fun _ : int => 0);
    repeat (apply Forall_cons || apply Forall_nil); assumption.
Qed.
