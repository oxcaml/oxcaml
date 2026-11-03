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

(* Facts about 64-bit arithmetic and OxCaml's [Int64] (OxCamlInt64.v) that the
   proofs use: the ranges of [wrap64] and [unsigned64], the bounds of quotients,
   powers of two and bitwise or, and the comparison tests OxCaml code writes as
   [Int64.compare x y < 0]. Nothing here needs review: each follows from the
   definitions and axioms of OxCamlInt64.v, which Rocq checks. *)

From Stdlib Require Import ZArith Lia Bool Zbitwise.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlIntProofs OxCamlInt64.

Local Open Scope Z_scope.

(* ---- 64-bit arithmetic on Z ------------------------------------------- *)

(* The two constants as powers, for [rewrite]. *)
Lemma two63_pow : two63 = 2 ^ 63. Proof. reflexivity. Qed.
Lemma two64_pow : two64 = 2 ^ 64. Proof. reflexivity. Qed.

(* [fits_int64] answering [true] means [in_int64]. *)
Lemma fits_int64_true : ∀ (z : Z), fits_int64 z = true -> in_int64 z.
Proof.
  unfold fits_int64, in_int64. intros z H. apply andb_prop in H as [H1 H2].
  apply Z.leb_le in H1. apply Z.ltb_lt in H2. lia.
Qed.

(* The unsigned reading of a 64-bit value lies in [[0, 2^64)]. *)
Lemma unsigned64_bounds : ∀ (z : Z),
  in_int64 z -> 0 <= unsigned64 z < two64.
Proof.
  intros z Hz. unfold unsigned64, in_int64, two63, two64 in *.
  destruct (z <? 0) eqn:E; [ apply Z.ltb_lt in E | apply Z.ltb_ge in E ]; lia.
Qed.

(* [wrap64 z] differs from [z] by a multiple of 2^64 ... *)
Lemma wrap64_sub_multiple : ∀ (z : Z), ∃ multiple, wrap64 z = z -
  multiple * two64.
Proof.
  intro z. exists ((z + two63) / two64). unfold wrap64.
  pose proof (Z.div_mod (z + two63) two64 ltac:(unfold two64; lia)) as H.
  unfold two63, two64 in *. lia.
Qed.

(* ... and is a 64-bit value ... *)
Lemma wrap64_in_range : ∀ (z : Z), in_int64 (wrap64 z).
Proof.
  intro z. unfold in_int64, wrap64.
  pose proof (Z.mod_pos_bound (z + two63) two64 ltac:(unfold two64; lia)).
  unfold two63, two64 in *. lia.
Qed.

(* ... so it is the identity on 64-bit values. *)
Lemma wrap64_id : ∀ (z : Z), in_int64 z -> wrap64 z = z.
Proof.
  intros z Hz. destruct (wrap64_sub_multiple z) as [k Hk].
  pose proof (wrap64_in_range z) as Hr. rewrite Hk in Hr.
  unfold in_int64, two63, two64 in *.
  assert (k = 0) by lia. subst k. lia.
Qed.

(* The truncated quotient of a 64-bit value by a positive divisor is again a
   64-bit value (its magnitude does not grow). *)
Lemma quot_abs_le : ∀ (dividend divisor : Z), 0 < divisor -> Z.abs (Z.quot
  dividend divisor) <= Z.abs dividend.
Proof.
  intros a b Hb.
  assert (Hpos : forall x, 0 <= x -> Z.quot x b <= x).
  { intros x Hx. apply Z.quot_le_upper_bound; [ exact Hb | nia ]. }
  destruct (Z_le_gt_dec 0 a) as [ Ha | Ha ].
  - pose proof (Z.quot_pos a b Ha Hb). pose proof (Hpos a Ha). lia.
  - pose proof (Z.quot_pos (- a) b ltac:(lia) Hb) as H1.
    pose proof (Hpos (- a) ltac:(lia)) as H2.
    rewrite Z.quot_opp_l in H1, H2 by lia. lia.
Qed.

(* So the quotient of a 64-bit value by a positive divisor is a 64-bit value.
   *)
Lemma quot_in_int64 : ∀ (dividend divisor : Z),
  0 < divisor -> in_int64 dividend -> in_int64 (Z.quot dividend divisor).
Proof.
  intros a b Hb Ha. pose proof (quot_abs_le a b Hb).
  destruct (Z_le_gt_dec 0 a) as [ Ha0 | Ha0 ].
  - pose proof (Z.quot_pos a b Ha0 Hb). unfold in_int64 in *. lia.
  - pose proof (Z.quot_pos (- a) b ltac:(lia) Hb) as H1.
    rewrite Z.quot_opp_l in H1 by lia. unfold in_int64 in *. lia.
Qed.

(* ---- Powers of two and bitwise or, on Z ------------------------------- *)

Lemma pow2_pos : ∀ (n : Z), 0 <= n -> 0 < 2 ^ n.
Proof. intros n Hn. apply Z.pow_pos_nonneg; lia. Qed.

(* Lower bounds on powers of two, as numerals for [lia]. *)
Lemma pow2_ge_2 : ∀ (n : Z), 1 <= n -> 2 <= 2 ^ n.
Proof.
  intros n Hn. change 2 with (2 ^ 1) at 1. apply Z.pow_le_mono_r; lia.
Qed.

Lemma pow2_ge_4 : ∀ (n : Z), 2 <= n -> 4 <= 2 ^ n.
Proof.
  intros n Hn. change 4 with (2 ^ 2) at 1. apply Z.pow_le_mono_r; lia.
Qed.

(* [x lor m] for [m >= 0] lies in [[x, x + m]] (two's complement). *)
Lemma lor_bounds : ∀ (x m : Z), 0 <= m -> x <= Z.lor x m <= x + m.
Proof.
  intros x m Hm.
  pose proof (Z.add_lor_land x m) as Hsum.
  assert (H0 : 0 <= Z.land x m) by (apply Z.land_nonneg; right; exact Hm).
  assert (Hle : Z.land x m <= m).
  { pose proof (Z.sub_land_same_l m x) as Hs. rewrite (Z.land_comm m x) in Hs.
    assert (0 <= Z.land m (Z.lnot x))
      by (apply Z.land_nonneg; left; exact Hm).
    lia. }
  lia.
Qed.

(* Nonnegative signed-register values have no set bit at position 63. *)
Lemma nonnegative_word_log2 : ∀ (word : Z),
  0 <= word < two63 -> Z.log2 word < 63.
Proof.
  intros word Hword. destruct (Z.eq_dec word 0) as [-> | Hnonzero].
  - reflexivity.
  - apply (proj1 (Z.log2_lt_pow2 word 63 ltac:(lia))).
    rewrite <- two63_pow. lia.
Qed.

(* Bitwise or of two nonnegative signed words cannot set their sign bit. *)
Lemma nonnegative_lor_word : ∀ (left right : Z),
  0 <= left < two63 -> 0 <= right < two63 ->
  0 <= Z.lor left right < two63.
Proof.
  intros left right Hleft Hright.
  assert (Hnonnegative : 0 <= Z.lor left right).
  { apply Z.lor_nonneg. lia. }
  split; [exact Hnonnegative |].
  destruct (Z.eq_dec (Z.lor left right) 0) as [-> | Hnonzero];
    [unfold two63; lia |].
  rewrite two63_pow.
  apply (proj2 (Z.log2_lt_pow2 (Z.lor left right) 63 ltac:(lia))).
  rewrite Z.log2_lor by lia.
  pose proof (nonnegative_word_log2 left Hleft).
  pose proof (nonnegative_word_log2 right Hright).
  apply Z.max_lub_lt; assumption.
Qed.

(* Bitwise and also preserves the range of nonnegative signed words. *)
Lemma nonnegative_land_word : ∀ (left right : Z),
  0 <= left < two63 -> 0 <= right < two63 ->
  0 <= Z.land left right < two63.
Proof.
  intros left right Hleft Hright.
  assert (Hnonnegative : 0 <= Z.land left right).
  { apply Z.land_nonneg. lia. }
  split; [exact Hnonnegative |].
  destruct (Z.eq_dec (Z.land left right) 0) as [-> | Hnonzero];
    [unfold two63; lia |].
  rewrite two63_pow.
  apply (proj2 (Z.log2_lt_pow2 (Z.land left right) 63 ltac:(lia))).
  pose proof (Z.log2_land left right ltac:(lia) ltac:(lia)).
  pose proof (nonnegative_word_log2 left Hleft).
  pose proof (Z.le_min_l (Z.log2 left) (Z.log2 right)). lia.
Qed.

(* Two's-complement or preserves signed 64-bit bounds, including negative
   masks. Complementing the two-negative case reduces it to bitwise and. *)
Lemma lor_in_int64 : ∀ (left right : Z),
  in_int64 left -> in_int64 right -> in_int64 (Z.lor left right).
Proof.
  intros left right Hleft Hright. unfold in_int64 in *.
  destruct (Z_lt_ge_dec left 0) as [Hleft_neg | Hleft_pos];
    destruct (Z_lt_ge_dec right 0) as [Hright_neg | Hright_pos].
  - pose proof (nonnegative_land_word (Z.lnot left) (Z.lnot right)
      ltac:(rewrite Z.lnot_eq_pred_opp; lia)
      ltac:(rewrite Z.lnot_eq_pred_opp; lia)) as Hcomplement.
    rewrite <- Z.lnot_lor, Z.lnot_eq_pred_opp in Hcomplement. lia.
  - pose proof (lor_bounds left right ltac:(lia)).
    assert (Z.lor left right < 0) by (apply Z.lor_neg; auto).
    unfold two63 in *. lia.
  - rewrite Z.lor_comm. pose proof (lor_bounds right left ltac:(lia)).
    assert (Z.lor right left < 0) by (apply Z.lor_neg; auto).
    unfold two63 in *. lia.
  - pose proof (nonnegative_lor_word left right ltac:(lia) ltac:(lia)).
    unfold two63 in *. lia.
Qed.

(* ---- OxCaml's [Int64] --------------------------------------------------- *)

(* The tests OxCaml code writes as [Int64.compare x y < 0], [> 0], [>= 0] and
   [<= 0], which the models write the same way. *)
Lemma int64_compare_lt : ∀ (a b : Int64.t),
  (Int64.compare a b <? OxCamlInt.zero)%sint63 = true
  <-> Int64.to_Z a < Int64.to_Z b.
Proof.
  intros a b. rewrite ltb_int, to_Z_zero.
  exact (proj1 (Int64.compare_spec a b)).
Qed.

Lemma int64_compare_gt : ∀ (a b : Int64.t),
  (OxCamlInt.zero <? Int64.compare a b)%sint63 = true
  <-> Int64.to_Z b < Int64.to_Z a.
Proof.
  intros a b. rewrite ltb_int, to_Z_zero.
  destruct (Int64.compare_spec a b) as [Hlt Heq]. lia.
Qed.

Lemma int64_compare_ge : ∀ (a b : Int64.t),
  (OxCamlInt.zero <=? Int64.compare a b)%sint63 = true
  <-> Int64.to_Z b <= Int64.to_Z a.
Proof.
  intros a b. rewrite leb_int, to_Z_zero.
  destruct (Int64.compare_spec a b) as [Hlt Heq]. lia.
Qed.

Lemma int64_compare_le : ∀ (a b : Int64.t),
  (Int64.compare a b <=? OxCamlInt.zero)%sint63 = true
  <-> Int64.to_Z a <= Int64.to_Z b.
Proof.
  intros a b. rewrite leb_int, to_Z_zero.
  destruct (Int64.compare_spec a b) as [Hlt Heq]. lia.
Qed.

(* [Int64.equal] answering [false] means different values; in particular a
   value it distinguishes from zero is not zero. *)
Lemma int64_equal_false : ∀ (a b : Int64.t),
  Int64.equal a b = false -> Int64.to_Z a <> Int64.to_Z b.
Proof.
  intros a b H E. apply Int64.equal_spec in E. congruence.
Qed.

Lemma int64_nonzero : ∀ (b : Int64.t),
  Int64.equal b Int64.zero = false -> Int64.to_Z b <> 0.
Proof.
  intros b H. rewrite <- Int64.to_Z_zero. apply int64_equal_false. exact H.
Qed.
