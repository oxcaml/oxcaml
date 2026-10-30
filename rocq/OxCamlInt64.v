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

(* 64-bit machine integers: the arithmetic of a machine register, and OxCaml's
   [Int64].

   The first half is pure mathematics on [Z]: two's-complement wrap-around and
   the signed/unsigned readings of a 64-bit value. The second half models
   OxCaml's [Int64] as an abstract type whose operations are specified, as
   axioms, by that arithmetic. Rocq has no primitive 64-bit integer, so this is
   the trusted base for every model that computes in [Int64]; each axiom is the
   documented behaviour of the OxCaml operation. *)

From Stdlib Require Import ZArith Lia Bool Zbitwise.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt.

Local Open Scope Z_scope.

(* ---- 64-bit arithmetic on Z ------------------------------------------- *)

Definition two63 : Z := 9223372036854775808.
Definition two64 : Z := 18446744073709551616.

(* The two constants as powers, for [rewrite]. *)
Lemma two63_pow : two63 = 2 ^ 63. Proof. reflexivity. Qed.
Lemma two64_pow : two64 = 2 ^ 64. Proof. reflexivity. Qed.

(* A 64-bit machine register, read as a signed integer. *)
Definition in_int64 (z : Z) : Prop := - two63 <= z < two63.

(* The same, as a boolean test. *)
Definition fits_int64 (z : Z) : bool := (- two63 <=? z) && (z <? two63).

(* [fits_int64] answering [true] means [in_int64]. *)
Lemma fits_int64_true : ∀ (z : Z), fits_int64 z = true -> in_int64 z.
Proof.
  unfold fits_int64, in_int64. intros z H. apply andb_prop in H as [H1 H2].
  apply Z.leb_le in H1. apply Z.ltb_lt in H2. lia.
Qed.

(* 64-bit two's-complement wrap-around: the register holding [z]. *)
Definition wrap64 (z : Z) : Z := (z + two63) mod two64 - two63.

(* The unsigned reading of a signed 64-bit value. *)
Definition unsigned64 (z : Z) : Z := if z <? 0 then z + two64 else z.

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

(* The type and its operations are declared at top level and given their OxCaml
   names by the abbreviations in [Module Int64] below, so that the models write
   [Int64.add a b] as OxCaml does, while extraction (OxCamlExtract.v) maps each
   operation straight to OxCaml's without emitting a module that would shadow
   OxCaml's [Int64]. The axioms, which are not extracted, live in the module.
   *)

Parameter int64 : Type.
Parameter int64_to_Z : int64 -> Z.
Parameter int64_zero int64_one int64_minus_one int64_min_int int64_max_int
  : int64.
Parameter int64_of_int : int -> int64.
Parameter int64_to_int : int64 -> int.
Parameter int64_add int64_sub int64_mul int64_div int64_rem
  : int64 -> int64 -> int64.
Parameter int64_abs int64_neg : int64 -> int64.
Parameter int64_shift_right int64_shift_right_logical : int64 -> int -> int64.
Parameter int64_equal : int64 -> int64 -> bool.
Parameter int64_compare : int64 -> int64 -> int.

(* OxCaml's [nativeint] and [int32], the payloads of two constant operations,
   which the models do not compute on; [Int64.of_nativeint] reads a [nativeint]
   as the same 64-bit value. *)
Parameter nativeint int32 : Type.
Parameter nativeint_to_int64 : nativeint -> int64.

Module Int64.
  Abbreviation t := int64.
  Abbreviation to_Z := int64_to_Z.
  Abbreviation zero := int64_zero.
  Abbreviation one := int64_one.
  Abbreviation minus_one := int64_minus_one.
  Abbreviation min_int := int64_min_int.
  Abbreviation max_int := int64_max_int.
  Abbreviation of_int := int64_of_int.
  Abbreviation to_int := int64_to_int.
  Abbreviation add := int64_add.
  Abbreviation sub := int64_sub.
  Abbreviation mul := int64_mul.
  Abbreviation div := int64_div.
  Abbreviation rem := int64_rem.
  Abbreviation abs := int64_abs.
  Abbreviation neg := int64_neg.
  Abbreviation shift_right := int64_shift_right.
  Abbreviation shift_right_logical := int64_shift_right_logical.
  Abbreviation equal := int64_equal.
  Abbreviation compare := int64_compare.
  Abbreviation of_nativeint := nativeint_to_int64.

  (* Every [Int64] is a 64-bit value. *)
  Axiom to_Z_bounded : ∀ (x : int64), in_int64 (to_Z x).
  (* The values of the literals. *)
  Axiom to_Z_zero : to_Z zero = 0.
  Axiom to_Z_one : to_Z one = 1.
  Axiom to_Z_minus_one : to_Z minus_one = -1.
  Axiom to_Z_min_int : to_Z min_int = - two63.
  Axiom to_Z_max_int : to_Z max_int = two63 - 1.

  (* [Int64.of_int] is exact: every [int] is a 64-bit value. *)
  Axiom of_int_spec : ∀ (i : int), to_Z (of_int i) = Sint63Axioms.to_Z i.

  (* [Int64.to_int] drops the top bit, so it is exact on the values that fit an
     [int]. *)
  Axiom to_int_exact : ∀ (a : int64),
    - two62 <= to_Z a <= two62 - 1 -> Sint63Axioms.to_Z (to_int a) = to_Z a.

  (* Addition, subtraction and multiplication wrap at 64 bits. *)
  Axiom add_spec : ∀ (a b : int64),
    to_Z (add a b) = wrap64 (to_Z a + to_Z b).
  Axiom sub_spec : ∀ (a b : int64),
    to_Z (sub a b) = wrap64 (to_Z a - to_Z b).
  Axiom mul_spec : ∀ (a b : int64),
    to_Z (mul a b) = wrap64 (to_Z a * to_Z b).
  (* Division truncates toward zero; [min_int / -1] wraps to [min_int]. The
     OxCaml raises [Division_by_zero] on a zero divisor, which the models never
     pass. *)
  Axiom div_spec : ∀ (a b : int64),
    to_Z b <> 0 -> to_Z (div a b) = wrap64 (Z.quot (to_Z a) (to_Z b)).
  (* [Int64.rem] is the remainder of that division ([Z.rem], with the sign of
     the dividend). *)
  Axiom rem_spec : ∀ (a b : int64),
    to_Z b <> 0 -> to_Z (rem a b) = Z.rem (to_Z a) (to_Z b).
  (* [Int64.abs min_int = min_int], and likewise [Int64.neg]. *)
  Axiom abs_spec : ∀ (a : int64), to_Z (abs a) = wrap64 (Z.abs (to_Z a)).
  Axiom neg_spec : ∀ (a : int64), to_Z (neg a) = wrap64 (- to_Z a).
  (* The right shifts, for amounts within the width (OxCaml leaves the others
     unspecified): [shift_right] is the arithmetic shift, division by
     [2^shift_amount] rounding towards minus infinity, and
     [shift_right_logical] divides the unsigned reading of the value. *)
  Axiom shift_right_spec : ∀ (a : int64) (shift_amount : int),
    0 <= Sint63Axioms.to_Z shift_amount <= 63 ->
    to_Z (shift_right a shift_amount)
    = to_Z a / 2 ^ Sint63Axioms.to_Z shift_amount.
  Axiom shift_right_logical_spec : ∀ (a : int64) (shift_amount : int),
    0 <= Sint63Axioms.to_Z shift_amount <= 63 ->
    to_Z (shift_right_logical a shift_amount)
    = wrap64 (unsigned64 (to_Z a) / 2 ^ Sint63Axioms.to_Z shift_amount).
  (* [Int64.equal] is equality of values. *)
  Axiom equal_spec : ∀ (a b : int64),
    equal a b = true <-> to_Z a = to_Z b.
  (* OxCaml's [Int64.compare] returns an [int] that is negative, zero or
     positive according as the first argument is less than, equal to or greater
     than the second. *)
  Axiom compare_spec : ∀ (a b : int64),
    (Sint63Axioms.to_Z (compare a b) < 0 <-> to_Z a < to_Z b)
    ∧ (Sint63Axioms.to_Z (compare a b) = 0 <-> to_Z a = to_Z b).

  (* The tests OxCaml code writes as [Int64.compare x y < 0], [> 0] and [>= 0],
     which the models write the same way. *)
  Lemma compare_lt : ∀ (a b : int64),
    (compare a b <? OxCamlInt.zero)%sint63 = true <-> to_Z a < to_Z b.
  Proof.
    intros a b. rewrite ltb_int, OxCamlInt.to_Z_zero.
    exact (proj1 (compare_spec a b)).
  Qed.

  Lemma compare_gt : ∀ (a b : int64),
    (OxCamlInt.zero <? compare a b)%sint63 = true <-> to_Z b < to_Z a.
  Proof.
    intros a b. rewrite ltb_int, OxCamlInt.to_Z_zero.
    destruct (compare_spec a b) as [Hlt Heq]. lia.
  Qed.

  Lemma compare_ge : ∀ (a b : int64),
    (OxCamlInt.zero <=? compare a b)%sint63 = true <-> to_Z b <= to_Z a.
  Proof.
    intros a b. rewrite leb_int, OxCamlInt.to_Z_zero.
    destruct (compare_spec a b) as [Hlt Heq]. lia.
  Qed.

  Lemma compare_le : ∀ (a b : int64),
    (compare a b <=? OxCamlInt.zero)%sint63 = true <-> to_Z a <= to_Z b.
  Proof.
    intros a b. rewrite leb_int, OxCamlInt.to_Z_zero.
    destruct (compare_spec a b) as [Hlt Heq]. lia.
  Qed.

  (* [Int64.equal] answering [false] means different values; in particular a
     value it distinguishes from zero is not zero. *)
  Lemma equal_false : ∀ (a b : int64),
    equal a b = false -> to_Z a <> to_Z b.
  Proof.
    intros a b H E. apply equal_spec in E. congruence.
  Qed.

  Lemma nonzero : ∀ (b : int64), equal b zero = false -> to_Z b <> 0.
  Proof.
    intros b H. rewrite <- to_Z_zero. apply equal_false. exact H.
  Qed.
End Int64.
