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

(* [Cmm.integer_comparison] (i.e. [Scalar.Integer_comparison.t]), its [negate]
   and [swap], and what a comparison computes on 64-bit machine values.

   The transcription is to be compared with typing/scalar.ml; the constructor
   order matters for extraction. *)

From Stdlib Require Import ZArith Lia.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt64 ZTactics.

Local Open Scope Z_scope.

Inductive integer_comparison : Type :=
  | Ceq | Cne | Clt | Cgt | Cle | Cge | Cult | Cugt | Cule | Cuge.

(* [Scalar.Integer_comparison.negate] = [Cmm.negate_integer_comparison] *)
Definition negate (c : integer_comparison) : integer_comparison :=
  match c with
  | Ceq => Cne
  | Cne => Ceq
  | Clt => Cge
  | Cle => Cgt
  | Cgt => Cle
  | Cge => Clt
  | Cult => Cuge
  | Cule => Cugt
  | Cugt => Cule
  | Cuge => Cult
  end.

(* [Scalar.Integer_comparison.swap] = [Cmm.swap_integer_comparison] *)
Definition swap (c : integer_comparison) : integer_comparison :=
  match c with
  | Ceq => Ceq
  | Cne => Cne
  | Clt => Cgt
  | Cle => Cge
  | Cgt => Clt
  | Cge => Cle
  | Cult => Cugt
  | Cule => Cuge
  | Cugt => Cult
  | Cuge => Cule
  end.

(* What the machine computes for [a cmp b] on 64-bit signed values [a] and [b]:
   the signed comparisons compare the values, the unsigned ones compare their
   unsigned readings. *)
Definition eval (cmp : integer_comparison) (a b : Z) : bool :=
  match cmp with
  | Ceq => a =? b
  | Cne => negb (a =? b)
  | Clt => a <? b
  | Cgt => b <? a
  | Cle => a <=? b
  | Cge => b <=? a
  | Cult => unsigned64 a <? unsigned64 b
  | Cugt => unsigned64 b <? unsigned64 a
  | Cule => unsigned64 a <=? unsigned64 b
  | Cuge => unsigned64 b <=? unsigned64 a
  end.

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
