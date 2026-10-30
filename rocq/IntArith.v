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

(* Integer arithmetic beyond what OxCaml's [Int] and [Int64] provide, for the
   models that compute with machine integers: overflow-checked addition,
   multiplication and negation, which return [None] instead of wrapping (see
   OxCamlOption.v); the exact conversion of an [Int64] to an [int]; and, on
   [Int64], a greatest common divisor and floor division.

   The overflow tests [add_fits], [mul_fits], [add_fits64] and [mul_fits64] are
   taken as given: each is a [Parameter] with an axiom saying that it answers
   whether the exact result fits the type. Their OxCaml, the sign-bit trick for
   addition and a divide-back check for multiplication, is in IntArithExtract.v
   and is trusted by inspection. Everything else is defined here and proved
   below. The extraction of this file, IntArith.ml (in backend/ssa/loop/rocq/,
   with the other extracted files), is the implementation. *)

From Stdlib Require Import ZArith Lia Bool List Recdef.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64.

Local Open Scope sint63_scope.

(* ---- Overflow-checked [int] arithmetic -------------------------------- *)

(* Whether the exact sum or product of two [int]s is again an [int]. *)
Parameter add_fits mul_fits : int -> int -> bool.
(* What the tests answer: whether the exact sum, or product, is an [int]. *)
Axiom add_fits_spec : ∀ (a b : int),
  add_fits a b = fits_int (to_Z a + to_Z b).
Axiom mul_fits_spec : ∀ (a b : int),
  mul_fits a b = fits_int (to_Z a * to_Z b).

(* The sum, the product and the negation, when they are [int]s. *)
Definition add_ovf (a b : int) : option int :=
  if add_fits a b then Some (a + b) else None.

Definition mul_ovf (a b : int) : option int :=
  if mul_fits a b then Some (a * b) else None.

Definition neg_ovf (a : int) : option int :=
  if a =? min_int then None else Some (- a).

(* ---- Overflow-checked [Int64] arithmetic ------------------------------ *)

(* Whether the exact sum or product of two [Int64]s is again an [Int64]. *)
Parameter add_fits64 mul_fits64 : Int64.t -> Int64.t -> bool.
(* The same tests on [Int64]. *)
Axiom add_fits64_spec : ∀ (a b : Int64.t),
  add_fits64 a b = fits_int64 (Int64.to_Z a + Int64.to_Z b).
Axiom mul_fits64_spec : ∀ (a b : Int64.t),
  mul_fits64 a b = fits_int64 (Int64.to_Z a * Int64.to_Z b).

Definition add_ovf64 (a b : Int64.t) : option Int64.t :=
  if add_fits64 a b then Some (Int64.add a b) else None.

Definition mul_ovf64 (a b : Int64.t) : option Int64.t :=
  if mul_fits64 a b then Some (Int64.mul a b) else None.

(* ---- From [Int64] to [int] -------------------------------------------- *)

(* Whether an [Int64] is an [int]: reading it back from [Int64.to_int] gives it
   again. *)
Definition int64_is_int (n : Int64.t) : bool :=
  Int64.equal (Int64.of_int (Int64.to_int n)) n.

Definition to_int_ovf (x : Int64.t) : option int :=
  if int64_is_int x then Some (Int64.to_int x) else None.

(* ---- Greatest common divisor and floor division on [Int64] ------------ *)

(* [let rec gcd64 a b = if b = 0L then a else gcd64 b (Int64.rem a b)]. Rocq
   needs a termination argument: [|b|] strictly decreases. *)
Function gcd64 (a b : Int64.t)
    {measure (fun b => Z.to_nat (Z.abs (Int64.to_Z b))) b} : Int64.t :=
  if Int64.equal b Int64.zero then a else gcd64 b (Int64.rem a b).
Proof.
  intros a b Hb. apply Int64.nonzero in Hb.
  rewrite Int64.rem_spec by exact Hb.
  apply Z2Nat.inj_lt; try apply Z.abs_nonneg.
  apply Z.rem_bound_abs. exact Hb.
Defined.

(* Floor division for [divisor > 0]: [Int64.div] rounds towards zero, so a
   negative dividend with a remainder is rounded down by one. *)
Definition floor_div64 (dividend divisor : Int64.t) : Int64.t :=
  let q := Int64.div dividend divisor in
  if (Int64.compare dividend Int64.zero <? zero)
     && negb (Int64.equal (Int64.rem dividend divisor) Int64.zero)
  then Int64.sub q Int64.one
  else q.

Local Open Scope Z_scope.

(* ---- Lemmas ----------------------------------------------------------- *)

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
    apply Int64.nonzero in e.
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
    by (rewrite Int64.compare_lt, Int64.to_Z_zero; reflexivity).
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
