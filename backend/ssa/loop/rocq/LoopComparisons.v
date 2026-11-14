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
   file, LoopComparisons.ml next to it, is the implementation; it needs no
   review. What the decisions mean, and the proofs, are in
   LoopComparisonsProofs.v.

   Shared material lives elsewhere: rocq/ at the root of the repository models
   OxCaml's own types (OxCamlInt.v, OxCamlInt64.v, OxCamlList.v,
   OxCamlOption.v), backend/rocq/IntegerComparison.v has
   [Cmm.integer_comparison] with [negate], [swap] and their semantics, and
   FourierMotzkin.v next to this file has [Affine], the affine forms.

   The type [termination] here has the constructors [Terminates] and [Unknown],
   as does [LoopTermination.t]; the latter writes these ones qualified. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 ZTactics.
From OxCamlBackend Require Import IntegerComparison.
From OxCamlLoop Require Import FourierMotzkin.
Import ListNotations.

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
   is in range (LoopComparisonsProofs.v), or not that this can tell. *)
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
