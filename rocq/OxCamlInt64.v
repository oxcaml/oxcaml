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

(* A 64-bit machine register, read as a signed integer. *)
Definition in_int64 (z : Z) : Prop := - two63 <= z < two63.

(* The same, as a boolean test. *)
Definition fits_int64 (z : Z) : bool := (- two63 <=? z) && (z <? two63).

(* 64-bit two's-complement wrap-around: the register holding [z]. *)
Definition wrap64 (z : Z) : Z := (z + two63) mod two64 - two63.

(* The unsigned reading of a signed 64-bit value. *)
Definition unsigned64 (z : Z) : Z := if z <? 0 then z + two64 else z.

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
End Int64.
