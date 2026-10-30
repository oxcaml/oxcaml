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

(* A mathematical valuation for the affine analysis, constructed rather than
   assumed of a program execution.

   AffineSsa's arithmetic theorems use a total valuation of syntax. Here
   [snapshot_value] extends a register snapshot to all syntax by arithmetic
   interpretation, including fabricated expressions. It is not execution and is
   not extracted. [snapshot_agrees_available] proves agreement with actual
   register reads only for canonical available values. No invariance or
   execution meaning is assigned to fabricated expressions. *)

From Stdlib Require Import ZArith Lia List Bool.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaSemantics SsaExecution.
From OxCamlLoop Require Import AffineExpr AffineSsa.
Import ListNotations.
Local Open Scope Z_scope.

Definition snapshot_atom (regs : registers) (v : value) : Z :=
  match regs (register_of_value v) with
  | Some word => wrap64 word
  | None => 0
  end.

(* Syntax is interpreted by [eval_operation], the one arithmetic semantics of
   the development, wherever it applies; a value it does not evaluate (an
   opaque operation, a shift out of range, a parameter) is read from its
   register. *)
Fixpoint snapshot_value (regs : registers) (v : value) : Z :=
  match v with
  | Res _ op args _ =>
    match eval_operation op (map (snapshot_value regs) args) with
    | Some word => word
    | None => snapshot_atom regs v
    end
  | Block_param _ _ => snapshot_atom regs v
  end.

(* Unavailable atoms may be assigned defaults in the mathematical extension,
   but those defaults are not used by the execution semantics. *)
Lemma snapshot_atom_bounded : ∀ (regs : registers) (v : value),
  in_int64 (snapshot_atom regs v).
Proof.
  intros regs v. unfold snapshot_atom. destruct (regs (register_of_value v)).
  - apply wrap64_in_range.
  - unfold in_int64, two63. lia.
Qed.

(* The total mathematical extension is bounded even for unavailable or
   fabricated syntax: evaluated results fit a register by
   [eval_operation_bounded], and the rest are atoms. *)
Lemma snapshot_value_bounded : ∀ (regs : registers) (v : value),
  in_int64 (snapshot_value regs v).
Proof.
  intros regs [id op args index | b index]; [ | apply snapshot_atom_bounded].
  cbn [snapshot_value].
  destruct (eval_operation op (map (snapshot_value regs) args)) eqn:Heval;
    [ | apply snapshot_atom_bounded].
  eapply eval_operation_bounded. exact Heval.
Qed.

(* The extension satisfies the affine library's operation equations on all
   syntax. This universal fact is proved, not a machine/run premise. *)
Theorem snapshot_operation_semantics : ∀ (regs : registers) (v : value),
  op_semantics (snapshot_value regs) v.
Proof.
  intros regs [id op args index | b index]; [ | exact I].
  cbn [op_semantics]. intros Hmodeled.
  destruct (modeled_operation_total _ _ _ (snapshot_value regs) Hmodeled)
    as [result Heval].
  cbn [snapshot_value]. rewrite Heval. reflexivity.
Qed.

(* A bound register's mathematical atom is exactly its stored word. *)
Lemma snapshot_atom_read : ∀ (regs : registers) (v : value) (word : Z),
  bounded_registers regs -> regs (register_of_value v) = Some word ->
  snapshot_atom regs v = word.
Proof.
  intros regs v word Hbounded Hread. unfold snapshot_atom. rewrite Hread.
  apply wrap64_id. eapply Hbounded. exact Hread.
Qed.

(* Pointwise agreement on actual operands gives the same argument list in the
   affine valuation as the instruction read from its registers. *)
Lemma read_values_snapshot : ∀ (regs : registers) (args : list value)
    (words : list Z),
  bounded_registers regs -> read_values regs args = Some words ->
  (∀ (arg : value), arg ∈ args ->
     snapshot_value regs arg = snapshot_atom regs arg) ->
  Forall2 (fun arg word => snapshot_value regs arg = word) args words.
Proof.
  intros regs args. induction args as [ | arg args IH];
    intros words Hbounded Hread Hagree; cbn [read_values] in Hread.
  - injection Hread as <-. constructor.
  - destruct (regs (register_of_value arg)) as [word |] eqn:Harg;
      [ | discriminate].
    destruct (read_values regs args) as [rest |] eqn:Hrest; [ | discriminate].
    injection Hread as <-. constructor.
    + rewrite (Hagree arg (or_introl eq_refl)).
      eapply snapshot_atom_read; eassumption.
    + eapply IH; [exact Hbounded | reflexivity |].
      intros value Hin. apply Hagree. right. exact Hin.
Qed.

(* Interpreting operands pointwise gives the argument list the instruction
   read, so an actually evaluated operation computes its stored result. *)
Lemma forall2_snapshot_map : ∀ (regs : registers) (args : list value)
    (words : list Z),
  Forall2 (fun arg word => snapshot_value regs arg = word) args words ->
  map (snapshot_value regs) args = words.
Proof.
  intros regs args words Hall. induction Hall; cbn [map]; congruence.
Qed.

(* For an actually evaluated operation, interpreting its operands in an
   agreeing snapshot computes the stored result. Comparisons and opaque syntax
   use their result register directly in the affine valuation. *)
Lemma snapshot_evaluated_result : ∀ (regs : registers) (id : instr_id)
    (op : operation) (args : list value) (index : int)
    (words : list Z) (result : Z),
  eval_operation op words = Some result ->
  Forall2 (fun arg word => snapshot_value regs arg = word) args words ->
  regs (Result id index) = Some result ->
  snapshot_value regs (Res id op args index)
    = snapshot_atom regs (Res id op args index).
Proof.
  intros regs id op args index words result Heval Hargs Hvalue.
  pose proof (eval_operation_bounded _ _ _ Heval) as Hbounded.
  cbn [snapshot_value]. rewrite (forall2_snapshot_map _ _ _ Hargs), Heval.
  unfold snapshot_atom. cbn [register_of_value]. rewrite Hvalue.
  symmetry. apply wrap64_id. exact Hbounded.
Qed.

(* Syntax outside the exact operation model reads its register directly. In
   particular, no equation is assumed for a load or other opaque op. *)
Lemma snapshot_unmodeled : ∀ (regs : registers) (id : instr_id)
    (op : operation) (args : list value) (index : int),
  modeled_operation op args = false ->
  snapshot_value regs (Res id op args index)
    = snapshot_atom regs (Res id op args index).
Proof.
  intros regs id op args index Hmodeled. cbn [snapshot_value].
  rewrite (unmodeled_operation_undefined _ _ _ (snapshot_value regs) Hmodeled).
  reflexivity.
Qed.

(* Available actual values have their stored register value in the mathematical
   extension. The induction descends only through available canonical operands,
   whose equations follow from execution. *)
Theorem snapshot_agrees_available : ∀ (blocks : list block)
    (entry : block) (position : SsaDominance.program_point) (regs : registers),
  well_formed_ssa blocks entry -> coherent_at blocks entry regs position ->
  bounded_registers regs -> ∀ (v : value),
  available_value blocks entry position v ->
  snapshot_value regs v = snapshot_atom regs v.
Proof.
  intros blocks entry position regs Hwf Hcoherent Hbounded v.
  induction v as [id op args index IHargs | b index] using value_ind';
    intros Havailable; [ | reflexivity].
  destruct (modeled_operation op args) eqn:Hmodeled.
  2: { apply snapshot_unmodeled. exact Hmodeled. }
  specialize (Hcoherent _ Havailable).
  cbn [value_coherent] in Hcoherent. rewrite Hmodeled in Hcoherent.
  destruct Hcoherent as [words [result [Hread [Heval Hvalue]]]].
  eapply snapshot_evaluated_result; [exact Heval | | exact Hvalue].
  eapply read_values_snapshot; [exact Hbounded | exact Hread |].
  intros arg Hin. apply Forall_forall with (x := arg) in IHargs;
    [ | exact Hin].
  apply IHargs. eapply operand_available; eassumption.
Qed.

(* This is the bridge used by range proofs: no default or recomputation remains
   when the available value has an actual register reading. *)
Theorem snapshot_read_available : ∀ (blocks : list block) (entry : block)
    (position : SsaDominance.program_point) (regs : registers) (v : value)
      (word : Z),
  well_formed_ssa blocks entry -> coherent_at blocks entry regs position ->
  bounded_registers regs -> available_value blocks entry position v ->
  regs (register_of_value v) = Some word -> snapshot_value regs v = word.
Proof.
  intros blocks entry position regs v word Hwf Hcoherent Hbounded
    Havailable Hread.
  rewrite (snapshot_agrees_available _ _ _ _ Hwf Hcoherent Hbounded _
    Havailable).
  eapply snapshot_atom_read; eassumption.
Qed.
