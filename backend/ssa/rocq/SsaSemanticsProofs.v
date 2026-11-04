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

(* Lemmas about execution, and the proofs about the whitelist of
   SsaSemantics.v.

   Nothing here needs review. [at_entry], [valid_position] and
   [register_defined_in_region] are invariants the proofs carry; the lemmas
   and theorems are stepping stones for SsaExecutionProofs.v,
   SsaContextProofs.v and the loop proofs. [step_deterministic] proves the
   statement SsaSemanticsSpec.v makes. *)

From Stdlib Require Import ZArith Lia Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlIntProofs
  OxCamlInt64 OxCamlInt64Proofs OxCamlList OxCamlListProofs.
From OxCamlBackend Require Import IntegerComparison IntegerComparisonProofs
  Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaGraph SsaSemanticsSpec
  SsaSemantics.
Import ListNotations.

Local Open Scope Z_scope.

(* ======================================================================== *)
(* Invariants of execution                                                  *)
(* ======================================================================== *)

(* The state is at the entry of [target]. *)
Definition at_entry (target : block) (current : state) : Prop :=
  current_block current = target ∧ remaining current = block_body target.

(* The remaining instructions are a suffix of the current block's body. *)
Definition valid_position (current : state) : Prop :=
  ∃ (prefix : list instr),
    block_body (current_block current) = prefix ++ remaining current.

(* Registers defined by instructions or parameters of a region. This is a
   structural property of the graph, not a claim about a run. *)
Definition register_defined_in_region (region : blockset)
  (reg : register) : Prop :=
  match reg with
  | Result id _ =>
    ∃ (b : block) (op : operation) (args : list value) (results : int),
      set_mem b region = true ∧ (Op id op args results) ∈ (block_body b)
  | Param b _ => set_mem b region = true
  end.

(* ======================================================================== *)
(* Lemmas                                                                   *)
(* ======================================================================== *)

(* Executing a body list is an actual finite execution ending immediately
   before its terminator. No instruction is evaluated merely by reading it. *)
Lemma executes_insns_steps : ∀ (b : block) (insns : list instr)
    (before after : registers),
  executes_insns before insns after ->
  steps {| current_block := b; remaining := insns;
           current_registers := before |}
        {| current_block := b; remaining := [];
           current_registers := after |}.
Proof.
  intros b insns before after Hbody. induction Hbody.
  - constructor.
  - eapply Steps_cons; [constructor; exact H | exact IHHbody].
Qed.

(* Register equality depends only on identity, as the compiler's does. *)
Lemma register_eqb_spec : ∀ (lhs rhs : register),
  register_eqb lhs rhs = true <-> lhs = rhs.
Proof.
  intros [id i | b i] [id' i' | b' i']; cbn [register_eqb];
    try (split; discriminate).
  - rewrite andb_true_iff, instr_id_eqb_spec, eqb_int.
    split; [intros [-> ->]; reflexivity | intros H; inversion H; auto].
  - rewrite andb_true_iff, block_eqb_spec, eqb_int.
    split; [intros [-> ->]; reflexivity | intros H; inversion H; auto].
Qed.

(* Writing a result does not modify any other register. *)
Lemma write_register_other :
  ∀ (regs : registers) (written other : register) (result : option Z),
  other <> written -> write_register regs written result other = regs other.
Proof.
  intros rs r s z Hneq. unfold write_register.
  destruct (register_eqb s r) eqn:E; [ | reflexivity ].
  apply register_eqb_spec in E. contradiction.
Qed.

(* The written register receives exactly the supplied value. *)
Lemma write_register_same :
  ∀ (regs : registers) (reg : register) (result : option Z),
  write_register regs reg result reg = result.
Proof.
  intros rs r z. unfold write_register.
  assert (register_eqb r r = true) by (apply register_eqb_spec; reflexivity).
  rewrite H. reflexivity.
Qed.

(* Reading operands preserves their number. *)
Lemma read_values_length :
  ∀ (regs : registers) (args : list value) (arg_values : list Z),
  read_values regs args = Some arg_values ->
  length arg_values = length args.
Proof.
  intros rs args. induction args as [ | v args IH ]; intros zs H.
  - injection H as <-. reflexivity.
  - cbn [read_values] in H.
    destruct (rs (register_of_value v)); [ | discriminate ].
    destruct (read_values rs args) eqn:E; [ | discriminate ].
    injection H as <-. cbn. f_equal. apply IH. reflexivity.
Qed.

(* Equal operand-register values give equal argument lists, without making any
   claim about other registers or unexecuted instructions. *)
Lemma read_values_ext : ∀ (args : list value)
    (regs_before regs_after : registers),
  (∀ (arg : value), arg ∈ args ->
    regs_after (register_of_value arg)
      = regs_before (register_of_value arg)) ->
  read_values regs_after args = read_values regs_before args.
Proof.
  intros args. induction args as [ | arg args IH ];
    intros regs_before regs_after Hagree; cbn [read_values]; [reflexivity |].
  rewrite (Hagree arg (or_introl eq_refl)).
  rewrite (IH regs_before regs_after
    (fun v Hin => Hagree v (or_intror Hin))).
  reflexivity.
Qed.

(* Every whitelisted operation is defined for every list of operands of its
   accepted arity. There are no divisions, shifts or memory accesses requiring
   further dynamic preconditions. *)
Lemma supported_operation_total : ∀ (A : Type) (op : operation)
    (args : list A) (arg_values : list Z),
  supported_operation op args = true -> length arg_values = length args ->
  ∃ (result : Z), eval_operation op arg_values = Some result.
Proof.
  intros A op args zs Hsup Hlen.
  destruct op; try discriminate;
    try (destruct op; try discriminate);
    destruct args as [ | a [ | b [ | c args ] ]]; try discriminate;
    destruct zs as [ | x [ | y [ | z zs ] ]]; try discriminate;
    cbn [eval_operation]; eexists; reflexivity.
Qed.

(* The loop whitelist is a subset of the exact observation equations. *)
Lemma supported_operation_modeled : ∀ (A : Type) (op : operation)
    (args : list A),
  supported_operation op args = true -> modeled_operation op args = true.
Proof.
  intros A op args Hsupported.
  destruct op; try discriminate; try (destruct op; try discriminate);
    destruct args as [ | lhs [ | rhs [ | extra rest]]];
    try discriminate; reflexivity.
Qed.

(* The specific operations with a linear reading are exactly those the
   evaluator accepts, on any operand values. *)
Lemma specific_modeled_total : ∀ (A : Type) (s : specific_operation)
    (args : list A) (value_of : A -> Z),
  specific_modeled s args = true ->
  ∃ (result : Z),
    eval_operation (Specific s) (map value_of args) = Some result.
Proof.
  intros A s args value_of Hmodeled.
  unfold specific_modeled in Hmodeled. cbn [eval_operation].
  unfold eval_specific.
  destruct (specific_linear s) as [[[ | c [ | c' [ | c'' cs]]] k] | ];
    destruct args as [ | lhs [ | rhs [ | extra rest]]]; try discriminate;
    cbn [map]; eexists; reflexivity.
Qed.

Lemma specific_unmodeled_undefined : ∀ (A : Type) (s : specific_operation)
    (args : list A) (value_of : A -> Z),
  specific_modeled s args = false ->
  eval_operation (Specific s) (map value_of args) = None.
Proof.
  intros A s args value_of Hmodeled.
  unfold specific_modeled in Hmodeled. cbn [eval_operation].
  unfold eval_specific.
  destruct (specific_linear s) as [[[ | c [ | c' [ | c'' cs]]] k] | ];
    destruct args as [ | lhs [ | rhs [ | extra rest]]]; try discriminate;
    reflexivity.
Qed.

(* A linear reading fits a machine register, being wrapped. *)
Lemma eval_specific_bounded : ∀ (s : specific_operation)
    (arg_values : list Z) (result : Z),
  eval_operation (Specific s) arg_values = Some result -> in_int64 result.
Proof.
  intros s zs z H. cbn [eval_operation] in H. unfold eval_specific in H.
  destruct (specific_linear s) as [[[ | c [ | c' [ | c'' cs]]] k] | ];
    destruct zs as [ | x [ | y [ | a zs]]]; try discriminate;
    injection H as <-; apply wrap64_in_range.
Qed.

(* The modeled operations are exactly those the evaluator accepts, on any
   operand values: [eval_operation] is total on them and undefined on the
   others. The affine analysis interprets syntax through these two facts. *)
Lemma modeled_operation_total : ∀ (A : Type) (op : operation)
    (args : list A) (value_of : A -> Z),
  modeled_operation op args = true ->
  ∃ (result : Z), eval_operation op (map value_of args) = Some result.
Proof.
  intros A op args value_of Hmodeled.
  destruct op; try discriminate; try (destruct op; try discriminate);
    destruct args as [ | lhs [ | rhs [ | extra rest]]]; try discriminate;
    cbn [modeled_operation] in Hmodeled;
    try (apply specific_modeled_total; exact Hmodeled);
    cbn [map eval_operation]; try rewrite Hmodeled; eexists; reflexivity.
Qed.

Lemma unmodeled_operation_undefined : ∀ (A : Type) (op : operation)
    (args : list A) (value_of : A -> Z),
  modeled_operation op args = false ->
  eval_operation op (map value_of args) = None.
Proof.
  intros A op args value_of Hmodeled.
  destruct op; try reflexivity; try (destruct op; try reflexivity);
    destruct args as [ | lhs [ | rhs [ | extra rest]]];
    cbn [modeled_operation] in Hmodeled; try discriminate;
    try (apply specific_unmodeled_undefined; exact Hmodeled);
    cbn [map eval_operation]; try rewrite Hmodeled; reflexivity.
Qed.

(* All supported results fit a machine register, including wraparound. *)
Lemma eval_operation_bounded :
  ∀ (op : operation) (arg_values : list Z) (result : Z),
  eval_operation op arg_values = Some result -> in_int64 result.
Proof.
  intros op zs z H. destruct op; try discriminate;
    try (destruct op; try discriminate);
    try (eapply eval_specific_bounded; exact H);
    destruct zs as [ | x [ | y [ | a zs ] ]]; try discriminate;
    cbn [eval_operation] in H;
    try (destruct (Z.leb 0 (to_Z k) && Z.leb (to_Z k) 63);
      [ | discriminate]);
    injection H as <-;
    try apply Int64.to_Z_bounded; try apply wrap64_in_range;
    unfold comparison_value; destruct (IntegerComparison.eval _ _ _);
    unfold in_int64, two63; lia.
Qed.

(* Binding a bounded value, or unbinding an omitted parameter, preserves the
   register-width invariant. *)
Lemma write_register_bounded :
  ∀ (regs : registers) (reg : register) (result : option Z),
  bounded_registers regs ->
  (∀ (word : Z), result = Some word -> in_int64 word) ->
  bounded_registers (write_register regs reg result).
Proof.
  intros rs r z Hrs Hz s n H. unfold write_register in H.
  destruct (register_eqb s r).
  - apply Hz. exact H.
  - eapply Hrs. exact H.
Qed.

(* Reading Goto arguments preserves the width of each supplied value. *)
Lemma read_arguments_bounded : ∀ (regs : registers)
    (args : list Terminator.arg) (arg_values : list (option Z)),
  bounded_registers regs -> read_arguments regs args = Some arg_values ->
  Forall (fun (result : option Z) =>
    ∀ (word : Z), result = Some word -> in_int64 word) arg_values.
Proof.
  intros rs args. induction args as [ | [v |] args IH ];
    intros zs Hrs Hread; cbn [read_arguments] in Hread.
  - injection Hread as <-. constructor.
  - destruct (rs (register_of_value v)) eqn:Ev; [ | discriminate ].
    destruct (read_arguments rs args) eqn:E; [ | discriminate ].
    injection Hread as <-. constructor.
    + intros n Hn. injection Hn as <-. eapply Hrs. exact Ev.
    + apply IH; [exact Hrs | reflexivity].
  - destruct (read_arguments rs args) eqn:E; [ | discriminate ].
    injection Hread as <-. constructor.
    + intros n Hn. discriminate.
    + apply IH; [exact Hrs | reflexivity].
Qed.

(* Binding parameters does not change a register outside the parameter list.
   This also applies when some parameters are omitted. *)
Lemma bind_parameters_frame : ∀ (parameters : list value)
    (arg_values : list (option Z)) (regs_before regs_after : registers)
    (reg : register),
  bind_parameters parameters arg_values regs_before = Some regs_after ->
  reg ∉ (map register_of_value parameters) ->
  regs_after reg = regs_before reg.
Proof.
  intros ps. induction ps as [ | p ps IH ];
    intros zs rs rs' r H Hnot; destruct zs; try discriminate.
  - injection H as <-. reflexivity.
  - cbn [bind_parameters] in H. rewrite (IH _ _ _ _ H).
    + apply write_register_other. cbn in Hnot. intuition congruence.
    + cbn in Hnot. tauto.
Qed.

(* Distinct parameters receive the precomputed argument values. The old
   environment cannot affect the assignments, even when arguments and
   parameters overlap (for example, swapping two parameters). *)
Lemma bind_parameters_values : ∀ (parameters : list value)
    (arg_values : list (option Z)) (regs_before regs_after : registers),
  NoDup (map register_of_value parameters) ->
  bind_parameters parameters arg_values regs_before = Some regs_after ->
  Forall2 (fun (parameter : value) (result : option Z) =>
    regs_after (register_of_value parameter) = result) parameters arg_values.
Proof.
  intros ps. induction ps as [ | p ps IH ];
    intros zs rs rs' Hdup H; destruct zs; try discriminate.
  - constructor.
  - cbn in Hdup. inversion Hdup as [ | ? ? Hnot Htail ]; subst.
    cbn [bind_parameters] in H. constructor.
    + rewrite (bind_parameters_frame _ _ _ _ _ H Hnot).
      apply write_register_same.
    + eapply IH; eassumption.
Qed.

(* Reading and then binding argument lists associates each target parameter
   with the original argument register, not an already-updated parameter
   environment. *)
Lemma bound_parameters_match_arguments : ∀ (args : list Terminator.arg)
    (regs_before : registers) (arg_values : list (option Z))
    (parameters : list value) (regs_after : registers),
  read_arguments regs_before args = Some arg_values ->
  Forall2 (fun (parameter : value) (result : option Z) =>
    regs_after (register_of_value parameter) = result) parameters arg_values ->
  Forall2
    (fun (arg : Terminator.arg) (parameter : value) =>
      match arg with
      | Terminator.Arg v =>
        regs_after (register_of_value parameter)
        = regs_before (register_of_value v)
      | Terminator.Omitted_since_unused =>
        regs_after (register_of_value parameter) = None
      end)
    args parameters.
Proof.
  intros args. induction args as [ | [v |] args IH ];
    intros rs zs ps rs' Hread Hbind; cbn [read_arguments] in Hread.
  - injection Hread as <-. inversion Hbind. constructor.
  - destruct (rs (register_of_value v)) eqn:Ev; [ | discriminate].
    destruct (read_arguments rs args) eqn:E; [ | discriminate].
    injection Hread as <-. inversion Hbind; subst.
    constructor; [rewrite Ev; assumption | eapply IH; eassumption].
  - destruct (read_arguments rs args) eqn:E; [ | discriminate].
    injection Hread as <-. inversion Hbind; subst.
    constructor; [assumption | eapply IH; eassumption].
Qed.

(* Parameter binding preserves register widths. *)
Lemma bind_parameters_bounded : ∀ (parameters : list value)
    (arg_values : list (option Z)) (regs_before regs_after : registers),
  bounded_registers regs_before ->
  Forall (fun (result : option Z) =>
    ∀ (word : Z), result = Some word -> in_int64 word) arg_values ->
  bind_parameters parameters arg_values regs_before = Some regs_after ->
  bounded_registers regs_after.
Proof.
  intros ps. induction ps as [ | p ps IH ];
    intros zs rs rs' Hrs Hzs H; destruct zs; try discriminate.
  - injection H as <-. exact Hrs.
  - inversion Hzs; subst. eapply IH; [ | eassumption | exact H].
    apply write_register_bounded; assumption.
Qed.

(* ======================================================================== *)
(* Theorems                                                                 *)
(* ======================================================================== *)

(* Supported instructions always execute when their operands are bound. Their
   only modification is their sole result register. *)
Theorem supported_instruction_executes :
  ∀ (insn : instr) (regs : registers) (arg_values : list Z),
  supported_instruction insn = true ->
  read_values regs (args_of_instr insn) = Some arg_values ->
  ∃ (id : instr_id) (op : operation) (args : list value) (result : Z),
    insn = Op id op args one
    ∧ eval_operation op arg_values = Some result
    ∧ execute_instruction regs insn
       = Some (write_register regs (Result id zero) (Some result)).
Proof.
  intros [id op args results | |] rs zs Hsup Hread; try discriminate.
  apply andb_true_iff in Hsup. destruct Hsup as [Hresults Hsup].
  apply eqb_int in Hresults. subst results.
  destruct (supported_operation_total _ _ _ _ Hsup
              (read_values_length _ _ _ Hread)) as [z Hz].
  exists id, op, args, z. split; [reflexivity |]. split; [exact Hz |].
  unfold execute_instruction. rewrite eqb_refl. cbn [args_of_instr] in Hread.
  rewrite Hread, Hz. reflexivity.
Qed.

(* Every instruction step preserves the machine-width invariant; this is
   derived from execution, not assumed of an arbitrary valuation. *)
Theorem execute_instruction_bounded :
  ∀ (insn : instr) (regs_before regs_after : registers),
  bounded_registers regs_before ->
  execute_instruction regs_before insn = Some regs_after ->
  bounded_registers regs_after.
Proof.
  intros [id op args results | |] rs rs' Hrs H; try discriminate.
  unfold execute_instruction in H.
  destruct (results =? one)%sint63; [ | discriminate ].
  destruct (read_values rs args); [ | discriminate ].
  destruct (eval_operation op l) eqn:E; [ | discriminate ].
  injection H as <-. apply write_register_bounded; [exact Hrs |].
  intros n Hn. injection Hn as <-. eapply eval_operation_bounded. exact E.
Qed.

(* Successful execution reads the operands, evaluates the operation and writes
   its unique result; each part follows by inverting execution. *)
Theorem execute_instruction_result : ∀ (id : instr_id) (op : operation)
    (args : list value) (result_count : int)
    (regs_before regs_after : registers),
  execute_instruction regs_before (Op id op args result_count)
    = Some regs_after ->
  result_count = one ∧
  ∃ (arg_values : list Z) (result : Z),
    read_values regs_before args = Some arg_values
    ∧ eval_operation op arg_values = Some result
    ∧ regs_after = write_register regs_before (Result id zero) (Some result).
Proof.
  intros id op args result_count regs_before regs_after Hexec.
  unfold execute_instruction in Hexec.
  destruct (result_count =? one)%sint63 eqn:E; [ | discriminate].
  apply eqb_int in E. split; [exact E |].
  destruct (read_values regs_before args) as [arg_values |] eqn:Hread;
    [ | discriminate].
  destruct (eval_operation op arg_values) as [result |] eqn:Heval;
    [ | discriminate].
  injection Hexec as <-. exists arg_values, result. auto.
Qed.

(* A successful instruction writes no other register, regardless of the
   operation or environment. *)
Theorem execute_instruction_frame :
  ∀ (id : instr_id) (op : operation) (args : list value)
    (result_count : int) (regs_before regs_after : registers) (reg : register),
  execute_instruction regs_before (Op id op args result_count)
    = Some regs_after ->
  reg <> Result id zero -> regs_after reg = regs_before reg.
Proof.
  intros id op args results rs rs' r H Hneq.
  unfold execute_instruction in H.
  destruct (results =? one)%sint63; [ | discriminate ].
  destruct (read_values rs args); [ | discriminate ].
  destruct (eval_operation op l); [ | discriminate ].
  injection H as <-. apply write_register_other. exact Hneq.
Qed.

(* A concrete Goto step passes the source values to the corresponding
   parameters. No recurrence or run-model hypothesis is needed. *)
Theorem goto_step_parameter : parameter_layout ->
  ∀ (before after : state) (target : block) (args : list Terminator.arg),
  remaining before = [] ->
  terminator (current_block before)
    = Terminator.Continue (Goto target) args ->
  step before after ->
  current_block after = target ∧ remaining after = block_body target ∧
  ∀ (index : int) (arg : value),
    nth_int args index = Some (Terminator.Arg arg) ->
    current_registers after (register_of_value (param target index))
    = current_registers before (register_of_value arg).
Proof.
  intros Hlayout s s' target args Hremaining Hterm Hstep.
  destruct Hstep as [b insn rest rs rs' Hop |
    b dest arguments rs zs rs' Hgoto Hread Hbind |
    b index targets rs n dest Hswitch Hindex Hnonneg Hnth Hparams];
    cbn in *; try discriminate.
  - rewrite Hgoto in Hterm. injection Hterm as -> ->.
    split; [reflexivity |]. split; [reflexivity |]. intros i v Harg.
    pose proof (bind_parameters_values _ _ _ _
      (parameters_distinct Hlayout target) Hbind) as Hvalues.
    pose proof (bound_parameters_match_arguments _ _ _ _ _ Hread Hvalues) as
      Hargs.
    destruct (Forall2_nth_int _ _ _ Hargs _ _ Harg) as [p [Hp Hvalue]].
    rewrite (parameter_lookup Hlayout target i p Hp). exact Hvalue.
  - rewrite Hswitch in Hterm. discriminate.
Qed.

(* Every operational step, including simultaneous parameter binding, preserves
   machine-word bounds. *)
Theorem step_bounded : ∀ (before after : state),
  bounded_registers (current_registers before) -> step before after ->
  bounded_registers (current_registers after).
Proof.
  intros s s' Hrs Hstep. destruct Hstep; cbn in *.
  - eapply execute_instruction_bounded; eassumption.
  - eapply bind_parameters_bounded; [exact Hrs | | eassumption].
    eapply read_arguments_bounded; eassumption.
  - exact Hrs.
Qed.

(* An entry state starts with the actual instructions of its block. *)
Lemma enter_valid_position : ∀ (b : block) (regs : registers),
  valid_position (enter b regs).
Proof. intros b rs. exists []. reflexivity. Qed.

(* Stepping never introduces instructions not in the current block. *)
Theorem step_valid_position : ∀ (before after : state),
  valid_position before -> step before after -> valid_position after.
Proof.
  intros s s' Hpos Hstep. destruct Hstep.
  - destruct Hpos as [prefix Hprefix].
    exists (prefix ++ [insn]). cbn in *.
    rewrite Hprefix, <- app_assoc. reflexivity.
  - apply enter_valid_position.
  - apply enter_valid_position.
Qed.

(* Instruction execution never changes block parameters. *)
Lemma executes_insns_parameter_frame : ∀ (insns : list instr)
    (before after : registers),
  executes_insns before insns after -> ∀ (b : block) (index : int),
  after (Param b index) = before (Param b index).
Proof.
  intros insns before after Hbody. induction Hbody;
    intros b index; [reflexivity |].
  rewrite IHHbody. destruct insn; try discriminate.
  eapply execute_instruction_frame; [eassumption | discriminate].
Qed.

(* The true/false targets of a two-way Switch are selected by 1 and 0,
   respectively, as computed by an integer comparison. *)
Lemma boolean_switch_step : ∀ (before after : state) (index : value)
    (ifnot ifso : block) (test : bool),
  remaining before = [] ->
  terminator (current_block before) = Terminator.Switch index [ifnot; ifso] ->
  current_registers before (register_of_value index)
    = Some (if test then 1 else 0) ->
  step before after -> current_block after = if test then ifso else ifnot.
Proof.
  intros before after index ifnot ifso test Hremaining Hterm Hvalue Hstep.
  destruct Hstep as [source insn rest regs regs' Hexec |
    source target args regs values regs' Hgoto Hread Hbind |
    source condition targets regs word target Hswitch Hindex Hpos Hnth
      Hparams];
    cbn in *; try discriminate.
  - rewrite Hgoto in Hterm. discriminate.
  - rewrite Hswitch in Hterm. injection Hterm as -> ->.
    rewrite Hvalue in Hindex. injection Hindex as <-.
    destruct test; cbn in Hnth; injection Hnth as <-; reflexivity.
Qed.

(* Parameters of a block are unchanged by a step that does not enter that
   block. Instruction results and block parameters are distinct register
   identities, including when their numeric indices coincide. *)
Theorem step_parameter_frame : parameter_layout ->
  ∀ (b : block) (index : int) (before after : state),
  step before after -> current_block after <> b ->
  current_registers after (Param b index)
    = current_registers before (Param b index).
Proof.
  intros Hlayout b index before after Hstep Hnot.
  destruct Hstep as [source insn rest regs regs' Hexec |
    source target args regs values regs' Hterm Hread Hbind |
    source condition targets regs value target Hterm Hvalue Hpos Hnth Hparams];
    cbn in *.
  - destruct insn as [id op args results | |]; try discriminate.
    eapply execute_instruction_frame; [exact Hexec | discriminate].
  - eapply bind_parameters_frame; [exact Hbind |].
    intros Hin. apply in_map_iff in Hin.
    destruct Hin as [parameter [Hreg Hin]].
    destruct (parameters_local Hlayout target parameter Hin)
      as [param_index ->]. cbn in Hreg. inversion Hreg. contradiction.
  - reflexivity.
Qed.

(* A control transfer into a parameterless block changes no register. This
   applies to the final exiting edge, not just to internal steps. *)
Theorem step_to_parameterless_frame : ∀ (before after : state),
  remaining before = [] -> params (current_block after) = [] ->
  step before after -> current_registers after = current_registers before.
Proof.
  intros before after Hfinished Hparams Hstep. destruct Hstep;
    cbn in *; try discriminate; [ | reflexivity].
  rewrite Hparams in H1. destruct argument_words; cbn in H1;
    [ | discriminate].
  injection H1 as <-. reflexivity.
Qed.

(* A step within a region cannot change a register defined outside it. Validity
   of the instruction position is proved for executions from block entries,
   rather than assumed for arbitrary instruction lists. *)
Theorem step_region_frame : parameter_layout ->
  ∀ (region : blockset) (reg : register) (before after : state),
  valid_position before ->
  set_mem (current_block before) region = true ->
  set_mem (current_block after) region = true ->
  ~ register_defined_in_region region reg -> step before after ->
  current_registers after reg = current_registers before reg.
Proof.
  intros Hlayout region reg before after Hposition Hbefore Hafter Houtside
    Hstep.
  destruct reg as [id index | b index].
  - destruct Hstep as [source insn rest regs regs' Hexec |
      source target args regs values regs' Hterm Hread Hbind |
      source condition targets regs value target Hterm Hvalue Hpos Hnth
        Hparams];
      cbn in *.
    + destruct insn as [defined_id op args results | |]; try discriminate.
      eapply execute_instruction_frame; [exact Hexec |].
      intros Heq. inversion Heq; subst.
      apply Houtside. exists source, op, args, results.
      split; [exact Hbefore |].
      destruct Hposition as [prefix Hprefix]. cbn in Hprefix.
      rewrite Hprefix.
      apply in_or_app. right. left. reflexivity.
    + eapply bind_parameters_frame; [exact Hbind |].
      intros Hin. apply in_map_iff in Hin.
      destruct Hin as [parameter [Hreg Hin]].
      destruct (parameters_local Hlayout target parameter Hin)
        as [param_index ->]. discriminate.
    + reflexivity.
  - eapply step_parameter_frame; [exact Hlayout | exact Hstep |].
    intros Heq. subst b. contradiction.
Qed.

(* Each executable state has at most one successor. *)
Theorem step_deterministic : step_deterministic_statement.
Proof.
  intros s s' s'' H H'. inversion H; inversion H'; subst; congruence.
Qed.

(* The extracted block check establishes the instruction whitelist and permits
   only internal control transfers. *)
Theorem supported_block_spec : ∀ (b : block),
  supported_block b = true ->
  (∀ (insn : instr), insn ∈ (block_body b) ->
     supported_instruction insn = true)
  ∧ terminator_pure_control (terminator b) = true.
Proof.
  intros b H. apply andb_true_iff in H. destruct H as [Hi Ht].
  rewrite forallb_forall in Hi. auto.
Qed.
