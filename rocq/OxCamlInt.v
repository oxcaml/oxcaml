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

(* OxCaml's [int], modelled by Rocq's primitive signed 63-bit integer [int]
   (from the standard library's Sint63), which is exactly the same type. [to_Z]
   reads an [int] as a mathematical integer ([Z]).

   This file re-exports Sint63, names the literals the models use, and collects
   the basic facts about [int] that the proofs need. *)

From Stdlib Require Import ZArith Lia Bool.
From OxCamlStdlib Require Import OxCamlLogic.
From Stdlib Require Export Sint63.

(* The literals are named so that extraction can print them as OxCaml literals
   (a primitive-int literal would otherwise be printed as a call into Rocq's
   runtime). *)
Local Open Scope sint63_scope.
Definition zero : int := 0.
Definition one : int := 1.
Definition minus_one : int := -1.
Definition two : int := 2.
Local Close Scope sint63_scope.

Local Open Scope Z_scope.

Definition two62 : Z := 4611686018427387904.

(* OxCaml's [max_int], as a mathematical integer. *)
Definition ocaml_max_int : Z := two62 - 1.

(* The values of the named literals, of [min_int] and of [max_int]. *)
Lemma to_Z_zero : to_Z zero = 0.
Proof. reflexivity. Qed.

Lemma to_Z_one : to_Z one = 1.
Proof. reflexivity. Qed.

Lemma to_Z_minus_one : to_Z minus_one = -1.
Proof. reflexivity. Qed.

Lemma to_Z_two : to_Z two = 2.
Proof. reflexivity. Qed.

Lemma to_Z_min_int : to_Z min_int = - two62.
Proof. reflexivity. Qed.

Lemma to_Z_max_int : to_Z max_int = two62 - 1.
Proof. reflexivity. Qed.

(* An OxCaml [int] lies in [[-2^62, 2^62 - 1]]. *)
Lemma int_bounds : ∀ x : int, - two62 <= to_Z x <= two62 - 1.
Proof.
  intro x. pose proof (Sint63.to_Z_bounded x) as H.
  rewrite to_Z_min_int, to_Z_max_int in H. exact H.
Qed.

(* Whether a mathematical integer is representable as an [int]. *)
Definition fits_int (z : Z) : bool := (- two62 <=? z) && (z <=? two62 - 1).

(* [fits_int] answering [true] means that the bounds hold. *)
Lemma fits_int_true : ∀ (z : Z),
  fits_int z = true -> - two62 <= z <= two62 - 1.
Proof.
  unfold fits_int. intros z H. apply andb_prop in H as [H1 H2].
  apply Z.leb_le in H1. apply Z.leb_le in H2. lia.
Qed.

(* --- The boolean tests on [int], as propositions. --- *)

Lemma eqb_int : ∀ x y : int, (x =? y)%sint63 = true <-> x = y.
Proof. intros. apply Uint63.eqb_spec. Qed.

Lemma eqb_int_false : ∀ x y : int, (x =? y)%sint63 = false <-> x <> y.
Proof. intros. rewrite <- not_true_iff_false, eqb_int. tauto. Qed.

Lemma eqb_int_refl : ∀ x : int, (x =? x)%sint63 = true.
Proof. intros. apply eqb_int. reflexivity. Qed.

Lemma ltb_int : ∀ x y : int, (x <? y)%sint63 = true <-> to_Z x < to_Z y.
Proof. intros. apply Sint63.ltb_spec. Qed.

Lemma leb_int : ∀ x y : int, (x <=? y)%sint63 = true <-> to_Z x <= to_Z y.
Proof. intros. apply Sint63.leb_spec. Qed.

(* Testing a machine int's sign agrees with its mathematical value. *)
Lemma zero_ltb_to_Z : ∀ (value : int),
  (zero <? value)%sint63 = (0 <? to_Z value)%Z.
Proof.
  intros value. destruct (zero <? value)%sint63 eqn:E; symmetry.
  - apply Z.ltb_lt. rewrite <- to_Z_zero. apply ltb_int. exact E.
  - apply Z.ltb_ge.
    destruct (Z.lt_ge_cases 0 (to_Z value)) as [Hpositive | Hnonpositive];
      [ | exact Hnonpositive].
    exfalso. rewrite <- to_Z_zero in Hpositive.
    apply ltb_int in Hpositive. congruence.
Qed.

(* --- [lsl] --- *)

(* The unsigned reading of a non-negative [int] is its value. *)
Lemma uint_to_Z_nonneg : ∀ n : int, 0 <= to_Z n -> Uint63.to_Z n = to_Z n.
Proof.
  intros n Hn. rewrite <- (Sint63.to_Z_mod_Uint63to_Z n). apply Z.mod_small.
  assert (HwB : Uint63Axioms.wB = 2 * two62) by reflexivity.
  pose proof (int_bounds n). unfold two62 in *. lia.
Qed.

(* [1 lsl shift_amount] is [2^shift_amount] as long as that fits an [int], i.e.
   for [shift_amount <= 61]: [lsl] shifts the unsigned reading, and bit 62 is
   the sign bit. *)
Lemma to_Z_lsl_one : ∀ shift_amount : int,
  0 <= to_Z shift_amount <= 61 ->
  to_Z (one << shift_amount)%sint63 = 2 ^ to_Z shift_amount.
Proof.
  intros n Hn.
  assert (HwB : Uint63Axioms.wB = 9223372036854775808) by reflexivity.
  assert (HwB2 : Uint63Axioms.wB / 2 = 4611686018427387904) by reflexivity.
  pose proof (Z.pow_le_mono_r 2 (to_Z n) 61 ltac:(lia) ltac:(lia)) as Hup.
  pose proof (Z.pow_pos_nonneg 2 (to_Z n) ltac:(lia) ltac:(lia)) as Hpos.
  replace (2 ^ 61) with 2305843009213693952 in Hup by reflexivity.
  rewrite (Sint63.to_Z_cmodwB (one << n)%sint63), Uint63Axioms.lsl_spec.
  change (Uint63.to_Z one) with 1. rewrite (uint_to_Z_nonneg n) by lia.
  unfold Sint63.cmod. rewrite HwB2, HwB, Z.mul_1_l.
  rewrite (Z.mod_small (2 ^ to_Z n)) by lia.
  rewrite (Z.mod_small (2 ^ to_Z n + 4611686018427387904)) by lia. lia.
Qed.

(* [(1 lsl shift_amount) - 1] is [2^shift_amount - 1] for [1 <= shift_amount <=
   62]: at [shift_amount = 62] the shift gives [min_int] and the subtraction
   wraps to [max_int], which is the right value. *)
Lemma to_Z_lsl_one_pred : ∀ shift_amount : int,
  1 <= to_Z shift_amount <= 62 -> to_Z ((one << shift_amount) - one)%sint63 = 2
    ^ to_Z shift_amount - 1.
Proof.
  intros n Hn.
  assert (HwB : Uint63Axioms.wB = 9223372036854775808) by reflexivity.
  assert (HwB2 : Uint63Axioms.wB / 2 = 4611686018427387904) by reflexivity.
  pose proof (Z.pow_le_mono_r 2 (to_Z n) 62 ltac:(lia) ltac:(lia)) as Hup.
  pose proof (Z.pow_le_mono_r 2 1 (to_Z n) ltac:(lia) ltac:(lia)) as Hlow.
  replace (2 ^ 62) with 4611686018427387904 in Hup by reflexivity.
  change (2 ^ 1) with 2 in Hlow.
  rewrite (Sint63.to_Z_cmodwB ((one << n) - one)%sint63),
    Uint63Axioms.sub_spec, Uint63Axioms.lsl_spec.
  change (Uint63.to_Z one) with 1. rewrite (uint_to_Z_nonneg n) by lia.
  unfold Sint63.cmod. rewrite HwB2, HwB, Z.mul_1_l.
  rewrite (Z.mod_small (2 ^ to_Z n)) by lia.
  rewrite (Z.mod_small (2 ^ to_Z n - 1)) by lia.
  rewrite (Z.mod_small (2 ^ to_Z n - 1 + 4611686018427387904)) by lia. lia.
Qed.

(* Split on the [int] tests in the goal, turning [step =? one] into the cases
   [to_Z step = 1] / [to_Z step <> 1], etc., and then rewrite any equation on a
   [to_Z] everywhere. *)
Ltac case_int_tests :=
  repeat match goal with
         | |- context [PrimInt63.eqb ?x ?y] => destruct (Sint63.eqbP x y)
         | |- context [PrimInt63.ltsb ?x ?y] => destruct (Sint63.ltbP x y)
         end;
  rewrite ?to_Z_zero, ?to_Z_one, ?to_Z_minus_one in *;
  repeat match goal with
         | H : to_Z ?s = ?c |- _ => rewrite H in *
         end.

(* Rearrangements of sums of [int]s, which hold with wrap-around. *)

(* [cmod] may be applied to either summand first, or not at all. *)
Lemma cmod_add_l : ∀ (a b modulus : Z),
  Sint63.cmod (Sint63.cmod a modulus + b) modulus
  = Sint63.cmod (a + b) modulus.
Proof.
  intros a b d. unfold Sint63.cmod.
  replace ((a + d / 2) mod d - d / 2 + b + d / 2)
    with ((a + d / 2) mod d + b) by ring.
  rewrite Zplus_mod_idemp_l. f_equal. f_equal. ring.
Qed.

Lemma cmod_add_r : ∀ (a b modulus : Z),
  Sint63.cmod (a + Sint63.cmod b modulus) modulus
  = Sint63.cmod (a + b) modulus.
Proof.
  intros a b d. rewrite (Z.add_comm a), cmod_add_l, (Z.add_comm b).
  reflexivity.
Qed.

(* Zero is neutral for the wrap-around addition. *)
Lemma add_zero_l : ∀ x : int, (zero + x)%sint63 = x.
Proof.
  intros x. rewrite <- (Sint63.of_to_Z (zero + x)%sint63), Sint63.add_spec.
  rewrite Sint63.of_Z_cmod, to_Z_zero, Z.add_0_l. apply Sint63.of_to_Z.
Qed.

Lemma add_zero_r : ∀ x : int, (x + zero)%sint63 = x.
Proof.
  intros x. rewrite <- (Sint63.of_to_Z (x + zero)%sint63), Sint63.add_spec.
  rewrite Sint63.of_Z_cmod, to_Z_zero, Z.add_0_r. apply Sint63.of_to_Z.
Qed.

(* [(x + 1) + (k - 1) = x + k], with wrap-around. *)
Lemma add_one_sub_one : ∀ x k : int,
  ((x + one) + (k - one))%sint63 = (x + k)%sint63.
Proof.
  intros x k. apply Sint63.to_Z_inj.
  rewrite !Sint63.add_spec, Sint63.sub_spec, cmod_add_l, cmod_add_r, to_Z_one.
  f_equal. ring.
Qed.

(* Equality of [int]s is decidable: for [in_dec] and the like in proofs. *)
Definition int_eq_dec : ∀ x y : int, {x = y} + {x <> y}.
Proof.
  intros x y. destruct (x =? y)%sint63 eqn:E.
  - left. apply eqb_int. exact E.
  - right. apply eqb_int_false. exact E.
Defined.
