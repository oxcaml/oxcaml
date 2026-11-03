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

(* What the checked arithmetic of IntArith.v computes: each operation, when it
   answers, gives the exact result; [to_int_ovf] preserves the value; [gcd64]
   divides both arguments and [floor_div64] is floor division. Nothing here
   needs review; the overflow tests the proofs rest on are the axioms of
   IntArith.v. *)

From Stdlib Require Import ZArith Lia Bool List Recdef.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlInt64 OxCamlInt64Proofs IntArith.

Local Open Scope sint63_scope.
Local Open Scope Z_scope.

(* Each checked operation, when it answers, computes the exact result. *)

Lemma add_ovf_spec : ∀ (a b sum : int),
  add_ovf a b = Some sum -> to_Z sum = to_Z a + to_Z b.
Proof.
  unfold add_ovf. intros a b s H. rewrite add_fits_spec in H.
  destruct (fits_int _) eqn:Hr; [ | discriminate ].
  injection H as <-. apply Sint63.to_Z_add.
  rewrite to_Z_min_int, to_Z_max_int. apply fits_int_true. exact Hr.
Qed.

(* The checked product is the exact product. *)
Lemma mul_ovf_spec : ∀ (a b product : int),
  mul_ovf a b = Some product -> to_Z product = to_Z a * to_Z b.
Proof.
  unfold mul_ovf. intros a b s H. rewrite mul_fits_spec in H.
  destruct (fits_int _) eqn:Hr; [ | discriminate ].
  injection H as <-. apply Sint63.to_Z_mul.
  rewrite to_Z_min_int, to_Z_max_int. apply fits_int_true. exact Hr.
Qed.

(* The checked negation is the exact negation. *)
Lemma neg_ovf_spec : ∀ (a negated : int),
  neg_ovf a = Some negated -> to_Z negated = - to_Z a.
Proof.
  unfold neg_ovf. intros a s H.
  destruct (a =? min_int)%sint63 eqn:E; [ discriminate | ].
  injection H as <-. apply Sint63.to_Z_opp. apply eqb_int_false. exact E.
Qed.

(* The checked 64-bit sum is the exact sum. *)
Lemma add_ovf64_spec : ∀ (a b sum : Int64.t),
  add_ovf64 a b = Some sum -> Int64.to_Z sum = Int64.to_Z a + Int64.to_Z b.
Proof.
  unfold add_ovf64. intros a b s H. rewrite add_fits64_spec in H.
  destruct (fits_int64 _) eqn:Hr; [ | discriminate ].
  injection H as <-. rewrite Int64.add_spec. apply wrap64_id.
  apply fits_int64_true. exact Hr.
Qed.

(* The checked 64-bit product is the exact product. *)
Lemma mul_ovf64_spec : ∀ (a b product : Int64.t),
  mul_ovf64 a b = Some product -> Int64.to_Z product = Int64.to_Z a *
    Int64.to_Z b.
Proof.
  unfold mul_ovf64. intros a b s H. rewrite mul_fits64_spec in H.
  destruct (fits_int64 _) eqn:Hr; [ | discriminate ].
  injection H as <-. rewrite Int64.mul_spec. apply wrap64_id.
  apply fits_int64_true. exact Hr.
Qed.

(* An [Int64] that [int64_is_int] accepts has the value of the [int] read from
   it. *)
Lemma int64_is_int_spec : ∀ (n : Int64.t),
  int64_is_int n = true -> to_Z (Int64.to_int n) = Int64.to_Z n.
Proof.
  intros n H. unfold int64_is_int in H. apply Int64.equal_spec in H.
  rewrite Int64.of_int_spec in H. exact H.
Qed.

(* The [int] [to_int_ovf] returns has the value of its argument. *)
Lemma to_int_ovf_spec : ∀ (x : Int64.t) (i : int),
  to_int_ovf x = Some i -> to_Z i = Int64.to_Z x.
Proof.
  unfold to_int_ovf. intros x i H.
  destruct (int64_is_int x) eqn:E; [ | discriminate ].
  injection H as <-. apply int64_is_int_spec. exact E.
Qed.

(* [gcd64 a b] divides both arguments (that it is their greatest common divisor
   is not needed). *)
Lemma gcd64_divides : ∀ (a b : Int64.t),
  (Int64.to_Z (gcd64 a b) | Int64.to_Z a)
  ∧ (Int64.to_Z (gcd64 a b) | Int64.to_Z b).
Proof.
  intros a b. functional induction (gcd64 a b).
  - apply Int64.equal_spec in e. rewrite Int64.to_Z_zero in e. rewrite e.
    split; [ apply Z.divide_refl | apply Z.divide_0_r ].
  - match goal with IH : _ ∧ _ |- _ => destruct IH as [IH1 IH2] end.
    apply int64_nonzero in e.
    rewrite Int64.rem_spec in IH2 by exact e.
    split; [ | exact IH1 ].
    rewrite (Z.quot_rem' (Int64.to_Z a) (Int64.to_Z b)).
    apply Z.divide_add_r; [ apply Z.divide_mul_l; exact IH1 | exact IH2 ].
Qed.

(* [Int64.abs] does not change divisibility, even on [min_int] (which it leaves
   unchanged). *)
Lemma abs64_divide : ∀ (d : Z) (c : Int64.t),
  (d | Int64.to_Z (Int64.abs c)) -> (d | Int64.to_Z c).
Proof.
  intros d c H. rewrite Int64.abs_spec in H.
  pose proof (Int64.to_Z_bounded c) as Hc. unfold in_int64 in Hc.
  destruct (Z.eq_dec (Int64.to_Z c) (- two63)) as [ E | NE ].
  - rewrite E in *.
    replace (wrap64 (Z.abs (- two63))) with (- two63) in H
      by (unfold wrap64, two63, two64; vm_compute; reflexivity).
    exact H.
  - rewrite wrap64_id in H by (unfold in_int64; lia).
    apply Z.divide_abs_r. exact H.
Qed.

(* [floor_div64] is floor division. *)
Lemma floor_div64_spec : ∀ (dividend divisor : Int64.t),
  0 < Int64.to_Z divisor -> Int64.to_Z (floor_div64 dividend divisor) =
    Int64.to_Z dividend / Int64.to_Z divisor.
Proof.
  intros c g Hg. unfold floor_div64.
  assert (Hg0 : Int64.to_Z g <> 0) by lia.
  pose proof (Int64.to_Z_bounded c) as Hc. unfold in_int64 in Hc.
  pose proof (Z.quot_rem' (Int64.to_Z c) (Int64.to_Z g)) as Hqr.
  pose proof (Z.rem_bound_abs (Int64.to_Z c) (Int64.to_Z g) Hg0) as Hrb.
  assert (Hq : Int64.to_Z (Int64.div c g)
               = Z.quot (Int64.to_Z c) (Int64.to_Z g)).
  { rewrite Int64.div_spec by exact Hg0. apply wrap64_id.
    apply quot_in_int64; [ exact Hg | exact (Int64.to_Z_bounded c) ]. }
  assert (Hlt' : (Int64.compare c Int64.zero <? zero)%sint63 = true
                 <-> Int64.to_Z c < 0)
    by (rewrite int64_compare_lt, Int64.to_Z_zero; reflexivity).
  assert (Heq' : Int64.equal (Int64.rem c g) Int64.zero = true
                 <-> Z.rem (Int64.to_Z c) (Int64.to_Z g) = 0)
    by (rewrite Int64.equal_spec, Int64.rem_spec, Int64.to_Z_zero by exact Hg0;
        reflexivity).
  destruct (Int64.compare c Int64.zero <? zero)%sint63 eqn:Hlt;
    destruct (Int64.equal (Int64.rem c g) Int64.zero) eqn:Heq; simpl.
  - (* c < 0 and g divides c: the quotient is exact. *)
    assert (Hr : Z.rem (Int64.to_Z c) (Int64.to_Z g) = 0)
      by (apply Heq'; reflexivity).
    rewrite Hq. apply Z.div_unique_pos with (r := 0); lia.
  - (* c < 0 and g does not divide c: round the quotient down. *)
    assert (Hneg : Int64.to_Z c < 0) by (apply Hlt'; reflexivity).
    assert (Hr : Z.rem (Int64.to_Z c) (Int64.to_Z g) <> 0)
      by (intro H; apply Heq' in H; discriminate).
    assert (Hrneg : Z.rem (Int64.to_Z c) (Int64.to_Z g) <= 0)
      by (apply Z.rem_nonpos; lia).
    assert (Hg2 : 2 <= Int64.to_Z g).
    { destruct (Z.eq_dec (Int64.to_Z g) 1) as [ E | E ]; [ | lia ].
      rewrite E, Z.rem_1_r in Hr. contradiction. }
    assert (Hqb : - two63 < Z.quot (Int64.to_Z c) (Int64.to_Z g)).
    { pose proof (quot_abs_le (Int64.to_Z c) (Int64.to_Z g) Hg) as Habs.
      pose proof (Int64.to_Z_bounded g) as Hgb. unfold in_int64 in Hgb.
      destruct (Z.eq_dec (Z.quot (Int64.to_Z c) (Int64.to_Z g)) (- two63))
        as [ E | NE ].
      - exfalso. rewrite E in Hqr. unfold two63 in *. lia.
      - unfold two63 in *. lia. }
    pose proof (quot_in_int64 (Int64.to_Z c) (Int64.to_Z g) Hg
                  (Int64.to_Z_bounded c)) as Hqrange.
    unfold in_int64 in Hqrange.
    rewrite Int64.sub_spec, Hq, Int64.to_Z_one.
    rewrite wrap64_id by (unfold in_int64; unfold two63 in *; lia).
    apply Z.div_unique_pos
      with (r := Z.rem (Int64.to_Z c) (Int64.to_Z g) + Int64.to_Z g).
    + lia.
    + rewrite Z.mul_sub_distr_l. lia.
  - (* c >= 0 and g divides c. *)
    assert (Hr : Z.rem (Int64.to_Z c) (Int64.to_Z g) = 0)
      by (apply Heq'; reflexivity).
    rewrite Hq. apply Z.div_unique_pos with (r := 0); lia.
  - (* c >= 0 and g does not divide c: the quotient is already the floor. *)
    assert (Hnn : 0 <= Int64.to_Z c).
    { destruct (Z_le_gt_dec 0 (Int64.to_Z c)) as [ | Hneg ]; [ assumption | ].
      exfalso. assert (E : false = true) by (apply Hlt'; lia).
      discriminate E. }
    assert (Hrnn : 0 <= Z.rem (Int64.to_Z c) (Int64.to_Z g))
      by (apply Z.rem_nonneg; lia).
    rewrite Hq.
    apply Z.div_unique_pos with (r := Z.rem (Int64.to_Z c) (Int64.to_Z g));
      lia.
Qed.

(* [Int64.div] by a positive divisor that divides exactly is the exact
   quotient. *)
Lemma div_exact64 : ∀ (dividend divisor : Int64.t),
  0 < Int64.to_Z divisor -> (Int64.to_Z divisor | Int64.to_Z dividend) ->
  Int64.to_Z (Int64.div dividend divisor)
  = Int64.to_Z dividend / Int64.to_Z divisor.
Proof.
  intros c g Hg Hd. rewrite Int64.div_spec by lia.
  rewrite wrap64_id
    by (apply quot_in_int64; [ lia | apply Int64.to_Z_bounded ]).
  apply Z.quot_div_exact; [ lia | exact Hd ].
Qed.
