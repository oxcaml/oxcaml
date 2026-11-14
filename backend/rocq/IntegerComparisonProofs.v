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

(* [negate] and [swap] (IntegerComparison.v) compute the negated and the
   exchanged comparison under [eval]. Nothing here needs review; Rocq checks the
   proofs. *)

From Stdlib Require Import ZArith.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt64 ZTactics.
From OxCamlBackend Require Import IntegerComparison.

Local Open Scope Z_scope.

(* [negate] computes the negation of a comparison. *)
Theorem negate_correct : ∀ (cmp : integer_comparison) (a b : Z),
  eval (negate cmp) a b = negb (eval cmp a b).
Proof.
  intros cmp a b. destruct cmp; simpl; case_zcmp.
Qed.

(* [swap] computes the comparison with its operands exchanged. *)
Theorem swap_correct : ∀ (cmp : integer_comparison) (a b : Z),
  eval (swap cmp) a b = eval cmp b a.
Proof.
  intros cmp a b. destruct cmp; simpl; case_zcmp.
Qed.
