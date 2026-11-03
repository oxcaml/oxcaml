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
  intros a b Hb.
  assert (Hnonzero : (Int64.to_Z b <> 0)%Z).
  { intro E. rewrite <- Int64.to_Z_zero in E.
    apply Int64.equal_spec in E. congruence. }
  rewrite Int64.rem_spec by exact Hnonzero.
  apply Z2Nat.inj_lt; try apply Z.abs_nonneg.
  apply Z.rem_bound_abs. exact Hnonzero.
Defined.

(* Floor division for [divisor > 0]: [Int64.div] rounds towards zero, so a
   negative dividend with a remainder is rounded down by one. *)
Definition floor_div64 (dividend divisor : Int64.t) : Int64.t :=
  let q := Int64.div dividend divisor in
  if (Int64.compare dividend Int64.zero <? zero)
     && negb (Int64.equal (Int64.rem dividend divisor) Int64.zero)
  then Int64.sub q Int64.one
  else q.
