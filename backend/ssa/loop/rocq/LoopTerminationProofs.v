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

(* What a [Terminates] answer of LoopTermination.v means. Nothing here needs
   review; Rocq checks the proofs.

   The read-back lemmas expose the accepted comparison and counter rule:
   which comparison the exit branch tests, which induction variable it tests
   and against which invariant bound, and that the bound passes the range
   check. LoopCounterExecutionProofs.v connects these to actual finite
   execution, including register bounds justified by affine entry guards. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlInt64 OxCamlInt64Proofs OxCamlList OxCamlListProofs.
From OxCamlBackend Require Import IntegerComparison IntegerComparisonProofs
  Operation.
From OxCamlSsa Require Import SsaModel SsaGraph SsaSemanticsSpec.
From OxCamlLoop Require Import FourierMotzkin FourierMotzkinProofs
  LoopComparisons LoopComparisonsProofs AffineExpr AffineExprProofs
  NaturalLoop NaturalLoopProofs AffineSsa AffineSsaProofs InductionVar
  InductionVarProofs LoopTermination.
Import ListNotations.

(* ======================================================================== *)
(* Lemmas                                                                   *)
(* ======================================================================== *)

Local Open Scope Z_scope.

(* The machine value of a comparison: 1 for true, 0 for false. *)
Definition bool_to_Z (b : bool) : Z := if b then 1 else 0.

(* The value a bound stands for, given a valuation of SSA values. *)
Definition bound_value (valuation : value -> Z)
  (comparison_bound : bound) : Z :=
  match comparison_bound with
  | Value_bound v => valuation v
  | Const_bound k => to_Z k
  end.

(* A literal-bound range check depends only on machine-int comparisons, not on
   a run, entry guards or a valuation of SSA expressions. *)
Lemma constant_bound_in_range : ∀ (step_size : int) (header : block)
    (limit : int),
  bound_in_range step_size header (Const_bound limit) = true ->
  LoopComparisonsProofs.bound_in_range (to_Z step_size) (to_Z limit) = true.
Proof.
  intros step_size header limit Hcheck.
  unfold bound_in_range in Hcheck. cbv zeta in Hcheck.
  rewrite zero_ltb_to_Z in Hcheck.
  unfold LoopComparisonsProofs.bound_in_range. rewrite Z.gtb_ltb.
  destruct (0 <? to_Z step_size)%Z.
  - apply leb_int in Hcheck. rewrite to_Z_max_int in Hcheck.
    apply Z.leb_le. exact Hcheck.
  - apply leb_int in Hcheck. rewrite Sint63.to_Z_opp in Hcheck.
    + rewrite to_Z_max_int in Hcheck. apply Z.leb_le.
      unfold ocaml_max_int. lia.
    + intros Heq. apply (f_equal to_Z) in Heq.
      rewrite to_Z_max_int, to_Z_min_int in Heq. unfold two62 in Heq. lia.
Qed.

Section Recognition.
  Variable op_def : op_def.
  Variable loop : natural_loop.

  (* --- The comparison the exit branch tests --- *)

  (* What [extract_comparison] found: [comparison_result] compares a value
     [is_counter_operand] accepts with a literal, or with a value [is_bound]
     accepts on either side, [iv_is_left] saying which. *)
  Lemma extract_comparison_spec :
    ∀ (is_counter_operand is_bound : value -> bool)
      (comparison_result : value)
      (cmp : integer_comparison) (iv_is_left : bool)
        (comparison_bound : bound),
    extract_comparison is_counter_operand is_bound comparison_result = Some
      (cmp, iv_is_left, comparison_bound) ->
    ∃ id result_index,
      (∃ left_operand constant_bound,
         comparison_result = Res id (Intop_imm (Icomp cmp) constant_bound)
           [left_operand] result_index
         ∧ is_counter_operand left_operand = true ∧ iv_is_left = true ∧
           comparison_bound = Const_bound constant_bound)
      ∨ (∃ left_operand right_operand,
         comparison_result = Res id (Intop (Icomp cmp)) [left_operand;
           right_operand] result_index
         ∧ is_counter_operand left_operand = true
         ∧ is_bound right_operand = true
         ∧ iv_is_left = true ∧ comparison_bound = Value_bound right_operand)
      ∨ (∃ left_operand right_operand,
         comparison_result = Res id (Intop (Icomp cmp)) [left_operand;
           right_operand] result_index
         ∧ is_counter_operand right_operand = true
         ∧ is_bound left_operand = true
         ∧ iv_is_left = false ∧ comparison_bound = Value_bound left_operand).
  Proof.
    intros is_iv_val is_bound cond cmp iv_is_left bnd H.
    unfold extract_comparison in H.
    destruct cond as [id op args i | bl i]; [ | discriminate ].
    destruct op as [ | | | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? ? ? | iop
                   | ? | iop k | ? | ? ? | ? | ? | ? | ? | | | | ? | ? | | | |
                   | | ? ];
      try discriminate; destruct iop; try discriminate;
      destruct args as [ | x [ | y [ | ? ? ] ] ]; try discriminate.
    - (* Intop (Icomp cmp'), [x; y] *)
      destruct (is_iv_val x && is_bound y) eqn:H1.
      + injection H as <- <- <-. apply andb_true_iff in H1.
        destruct H1 as [Hx Hy].
        exists id, i. right. left. exists x, y.
        repeat split; try reflexivity; assumption.
      + destruct (is_iv_val y && is_bound x) eqn:H2; [ | discriminate ].
        injection H as <- <- <-. apply andb_true_iff in H2.
        destruct H2 as [Hy Hx].
        exists id, i. right. right. exists x, y.
        repeat split; try reflexivity; assumption.
    - (* Intop_imm (Icomp cmp') k, [x] *)
      destruct (is_iv_val x) eqn:Hx; [ | discriminate ].
      injection H as <- <- <-.
      exists id, i. left. exists x, k.
      repeat split; try reflexivity; assumption.
  Qed.

  (* --- The induction variable's update --- *)

  (* A constant step comes from [self + step_constant] or [self -
     step_constant]. *)
  Lemma classify_update_const :
    ∀ (is_self : value -> bool) (step_constant : int) (sign : biv_sign)
      (updated_value : value),
    classify_update is_self op_def loop.(header) updated_value
      = Some (Step_const step_constant, sign) ->
    ∃ id result_index self,
      is_self self = true
      ∧ ((sign = Add ∧ updated_value = Res id (Intop_imm Iadd step_constant)
        [self] result_index)
          ∨ (sign = Sub ∧ updated_value = Res id (Intop_imm Isub
            step_constant) [self] result_index)).
  Proof.
    intros is_self c sign u H. unfold classify_update in H.
    destruct u as [id op args i | bl i]; [ | discriminate ].
    destruct op as [ | | | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? | ? ? ? | iop
                   | ? | iop c0 | ? | ? ? | ? | ? | ? | ? | | | | ? | ? | | | |
                   | | ? ];
      try discriminate; destruct iop; try discriminate;
      destruct args as [ | x [ | y [ | ? ? ] ] ]; try discriminate.
    - destruct (is_self x && _); [ discriminate | ].
      destruct (is_self y && _); discriminate.
    - destruct (is_self x && _); discriminate.
    - destruct (is_self x) eqn:Hx; [ | discriminate ].
      injection H as <- <-. exists id, i, x. split; [ exact Hx | ].
      left. split; reflexivity.
    - destruct (is_self x) eqn:Hx; [ | discriminate ].
      injection H as <- <-. exists id, i, x. split; [ exact Hx | ].
      right. split; reflexivity.
  Qed.

(* ======================================================================== *)
(* Theorems                                                                 *)
(* ======================================================================== *)

  (* What [biv_implies_termination] accepting [biv], a BIV of [loop], says: the
     header has an exit branch [exit_info], [biv] a literal step, the exit
     comparison the shape [extract_comparison] reads, and the verdict of
     LoopComparisons.continue_terminates on them is [Terminates], or
     [Terminates_if_bound_in_range] with the bound checked in range. *)
  Lemma biv_implies_termination_spec : ∀ (biv : biv),
    biv.(InductionVar.loop) = loop ->
    biv_implies_termination op_def biv = true ->
    ∃ exit_info step_size cmp iv_is_left comparison_bound,
      find_exit_branch loop = Some exit_info
      ∧ signed_step biv = Some step_size
      ∧ extract_comparison
           (fun v => is_param loop.(header) biv.(param_index) v
                     || existsb (value_eqb v) biv.(update))
           (is_loop_invariant op_def loop.(header)) exit_info.(condition)
         = Some (cmp, iv_is_left, comparison_bound)
      ∧ (continue_terminates step_size
            (oriented_continue_comparison iv_is_left
               exit_info.(continue_when_true) cmp)
          = LoopComparisons.Terminates
          ∨ (continue_terminates step_size
                (oriented_continue_comparison iv_is_left
                   exit_info.(continue_when_true) cmp)
              = LoopComparisons.Terminates_if_bound_in_range
              ∧ bound_in_range step_size loop.(header) comparison_bound =
                true)).
  Proof.
    intros b Hl Hb. unfold biv_implies_termination in Hb. rewrite Hl in Hb.
    destruct (find_exit_branch loop) as [eb | ] eqn:Hexit; [ | discriminate ].
    destruct (signed_step b) as [step | ] eqn:Hstep; [ | discriminate ].
    cbv iota zeta beta in Hb.
    destruct (extract_comparison _ _ eb.(condition))
      as [[[cmp iv_is_left] bnd] | ] eqn:Hext; [ | discriminate ].
    cbv zeta in Hb.
    exists eb, step, cmp, iv_is_left, bnd.
    split; [ reflexivity | ]. split; [ reflexivity | ].
    split; [ first [ reflexivity | exact Hext ] | ].
    destruct (continue_terminates step
                (oriented_continue_comparison iv_is_left
                   eb.(continue_when_true) cmp)) eqn:Hct; cbv iota in Hb.
    - left. reflexivity.
    - right. split; [ reflexivity | exact Hb ].
    - discriminate.
  Qed.

End Recognition.
