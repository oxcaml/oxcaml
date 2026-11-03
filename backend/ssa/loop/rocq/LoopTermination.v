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

(* Whether a loop terminates: its header's exit branch compares a basic
   induction variable with a loop-invariant bound in a way the variable's step
   must eventually make fail. The extraction of this file, LoopTermination.ml,
   is the implementation; it needs no review. What the answer means, and the
   proofs, are in LoopTerminationProofs.v. NaturalLoop.v and InductionVar.v
   find the loops, their exit branches and the basic induction variables this
   file is given; LoopComparisons.v has the arithmetic termination argument;
   and AffineSsa.v the facts a register bound is checked against.

   The type [t] here has the constructors [Terminates] and [Unknown], as does
   [LoopComparisons.termination]; the latter's are written qualified below. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaSemanticsSpec.
From OxCamlLoop Require Import FourierMotzkin LoopComparisons AffineExpr
  NaturalLoop AffineSsa InductionVar.
Import ListNotations.

Local Open Scope sint63_scope.

(* The verdict. *)
Inductive t : Type :=
  | Terminates
  | Unknown.

(* The other operand of the exit comparison. *)
Inductive bound : Type :=
  | Value_bound (v : value)
  | Const_bound (k : int).

(* Whether the bound is in the range LoopComparisonsProofs.v asks for: for
   a constant, by comparison; for a value, by asking whether the facts holding
   at the header (AffineSsa.v) entail it; [false] when an affine form
   overflows.

   A literal bound is an [int], so [k <= max_int] always holds and [- max_int
   <= k] fails only for [min_int]: for literal bounds the whole argument is in
   [continue_terminates]. A register bound needs the facts to pin it within [[-
   max_int, max_int]]; the affine constants are [int]s too, so a fact such as
   [b + 2^62 >= 0] cannot be written, and the lower bound of an arithmetic
   shift by one bit ([>>s 1], the shape of an untagged bound) is not available
   (AffineExpr.shr_range_facts). This is why a [downto] loop with such a bound
   is not deleted while the corresponding [to] loop is. *)
Definition bound_in_range (step : int) (header : block) (bound : bound)
    : bool :=
  match bound with
  | Const_bound k => if zero <? step then k <=? max_int else - max_int <=? k
  | Value_bound v =>
    let '((form, side), c) := linearize new_ctx [] v in
    let '((guards, side), c) := guards_at c side header in
    let facts := guards ++ side in
    let goal :=
      if zero <? step
      then
        let* f := Affine.scale_checked minus_one form in
        Affine.add_const_checked f max_int
      else Affine.add_const_checked form max_int
    in
    match goal with
    | Some goal => entails facts goal
    | None => false
    end
  end.

(* The comparison the exit branch tests, whether the induction variable is its
   left operand, and the other operand. *)
Definition extract_comparison (is_counter_operand is_bound : value -> bool)
    (comparison_result : value) : option (integer_comparison * bool * bound) :=
  match comparison_result with
  | Res _ (Intop_imm (Icomp cmp) k) [x] _ =>
    if is_counter_operand x then Some (cmp, true, Const_bound k) else None
  | Res _ (Intop (Icomp cmp)) [x; y] _ =>
    if is_counter_operand x && is_bound y
    then Some (cmp, true, Value_bound y)
    else if is_counter_operand y && is_bound x
    then Some (cmp, false, Value_bound x)
    else None
  | _ => None
  end.

(* Whether [biv]'s step must eventually make the exit comparison fail
   (LoopComparisonsProofs.v): then the loop terminates. Sound for a [biv]
   satisfying [biv_spec] (InductionVarProofs.v);
   LoopTerminationProofs.biv_implies_termination_spec reads it back. *)
Definition biv_implies_termination (op_def : op_def) (biv : biv) : bool :=
  match find_exit_branch biv.(InductionVar.loop), signed_step biv with
  | Some exit_info, Some step =>
    let header := biv.(InductionVar.loop).(header) in
    let is_self := is_param header biv.(param_index) in
    let is_iv_val v := is_self v || existsb (value_eqb v) biv.(update) in
    let is_bound v := is_loop_invariant op_def header v in
    match extract_comparison is_iv_val is_bound exit_info.(condition) with
    | None => false
    | Some (cmp, iv_is_left, bound) =>
      let continue_cmp :=
        oriented_continue_comparison iv_is_left
          exit_info.(continue_when_true) cmp
      in
      match continue_terminates step continue_cmp with
      | LoopComparisons.Terminates => true
      | LoopComparisons.Terminates_if_bound_in_range =>
        bound_in_range step header bound
      | LoopComparisons.Unknown => false
      end
    end
  | None, _ | Some _, None => false
  end.

(* Accept if one of the supplied induction variables proves termination.
   DeleteEmptyLoops.v connects these variables to loop discovery. *)
Definition analyze (op_def : op_def) (bivs : list biv) : t :=
  if existsb (biv_implies_termination op_def) bivs
  then Terminates
  else Unknown.

Local Close Scope sint63_scope.
