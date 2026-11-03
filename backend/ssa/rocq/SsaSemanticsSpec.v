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

(* Execution of SSA code: registers, the evaluation of the integer operations
   the models give an exact meaning to, and the steps of instructions, Gotos
   and Switches.

   This file is to be reviewed, against the backend (section 5.1 of
   ROCQ-REVIEW-GUIDE.md): [eval_operation], [execute_instruction],
   [read_arguments], [bind_parameters] and the rules of [step]. Registers are
   identified by instruction/result or block/parameter, not by a freely
   constructed expression; an operand reads its register, and only executing
   an instruction evaluates its operation. [step] executes one instruction or
   terminator and is deterministic ([step_deterministic_statement], proved in
   SsaSemanticsProofs.v); it has no rule for returns, calls or other opaque
   code, which SsaContextSpec.v observes without interpreting. The exact
   evaluator covers the loop whitelist and, for bounds computed before a
   loop, immediate shifts and or ([modeled_operation]); this does not widen
   the whitelist. [steps_until] and [executes_insns] are the forms of
   execution the eligibility statement (LoopEligibilitySpec.v) speaks of;
   [bounded_registers] and [parameter_layout] are premises of it, through
   [reached_header] and [well_formed_ssa].

   The whitelist the loop analyses accept is SsaSemantics.v, extracted; the
   lemmas and the proofs are in SsaSemanticsProofs.v. Neither needs
   review. *)

From Stdlib Require Import ZArith Bool List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64
  OxCamlList.
From OxCamlBackend Require Import IntegerComparison Operation.
From OxCamlSsa Require Import SsaModel.
Import ListNotations.

Local Open Scope Z_scope.

(* ======================================================================== *)
(* Registers and operations                                                 *)
(* ======================================================================== *)

Inductive register : Type :=
  | Result (id : instr_id) (index : int)
  | Param (b : block) (index : int).

Definition register_of_value (v : value) : register :=
  match v with
  | Res id _ _ i => Result id i
  | Block_param b i => Param b i
  end.

Definition register_eqb (left right : register) : bool :=
  match left, right with
  | Result id i, Result id' i' =>
    instr_id_eqb id id' && (i =? i')%sint63
  | Param b i, Param b' i' => block_eqb b b' && (i =? i')%sint63
  | _, _ => false
  end.

(* Unbound registers have no value. There are no arbitrary defaults for
   instructions that have not executed. *)
Definition registers : Type := register -> option Z.

Definition write_register (regs : registers) (written : register)
    (word : option Z) : registers :=
  fun queried => if register_eqb queried written then word else regs queried.

Fixpoint read_values (regs : registers) (args : list value)
    : option (list Z) :=
  match args with
  | [] => Some []
  | v :: args =>
    let* word := regs (register_of_value v) in
    let* argument_words := read_values regs args in
    Some (word :: argument_words)
  end.

Definition comparison_value (cmp : integer_comparison) (x y : Z) : Z :=
  if IntegerComparison.eval cmp x y then 1 else 0.

(* Operations for which the observation semantics has an exact integer
   equation. This is broader than the loop whitelist: shifts and bitwise or may
   compute bounds before entering an otherwise empty loop. *)
Definition modeled_operation {A : Type} (op : operation) (args : list A)
    : bool :=
  match op, args with
  | Const_int _, _
  | Intop (Iadd | Isub | Icomp _), [_; _]
  | Intop_imm (Iadd | Isub | Icomp _ | Ior) _, [_] => true
  | Intop_imm (Ilsl | Iasr | Ilsr) bits, [_] =>
    Z.leb 0 (to_Z bits) && Z.leb (to_Z bits) 63
  | _, _ => false
  end.

(* The trap-stack markers, which SsaContextSpec.v steps over without touching
   any register. *)
Definition is_trap_marker (insn : instr) : bool :=
  match insn with
  | Push_trap | Pop_trap => true
  | Op _ _ _ _ => false
  end.

(* Operations compute on values read from the current environment. These
   clauses, rather than equations imposed on every SSA expression, define their
   semantics. Arithmetic wraps at 64 bits. *)
Definition eval_operation (op : operation) (args : list Z) : option Z :=
  match op, args with
  | Const_int n, _ => Some (Int64.to_Z (Int64.of_nativeint n))
  | Intop Iadd, [x; y] => Some (wrap64 (x + y))
  | Intop Isub, [x; y] => Some (wrap64 (x - y))
  | Intop (Icomp cmp), [x; y] => Some (comparison_value cmp x y)
  | Intop_imm Iadd k, [x] => Some (wrap64 (x + to_Z k))
  | Intop_imm Isub k, [x] => Some (wrap64 (x - to_Z k))
  | Intop_imm (Icomp cmp) k, [x] =>
    Some (comparison_value cmp x (to_Z k))
  | Intop_imm Ilsl bits, [x] =>
    if Z.leb 0 (to_Z bits) && Z.leb (to_Z bits) 63
    then Some (wrap64 (x * 2 ^ to_Z bits)) else None
  | Intop_imm Iasr bits, [x] =>
    if Z.leb 0 (to_Z bits) && Z.leb (to_Z bits) 63
    then Some (wrap64 (x / 2 ^ to_Z bits)) else None
  | Intop_imm Ilsr bits, [x] =>
    if Z.leb 0 (to_Z bits) && Z.leb (to_Z bits) 63
    then Some (wrap64 (unsigned64 x / 2 ^ to_Z bits)) else None
  | Intop_imm Ior mask, [x] => Some (wrap64 (Z.lor x (to_Z mask)))
  | _, _ => None
  end.

Definition execute_instruction (regs : registers) (insn : instr)
    : option registers :=
  match insn with
  | Op id op args results =>
    if (results =? one)%sint63 then
      let* argument_words := read_values regs args in
      let* word := eval_operation op argument_words in
      Some (write_register regs (Result id zero) (Some word))
    else None
  | Push_trap | Pop_trap => None
  end.

(* Read every Goto argument before binding any parameters. An omitted argument
   leaves its parameter unbound, rather than preserving a value from an earlier
   visit. *)
Fixpoint read_arguments (regs : registers) (args : list Terminator.arg)
    : option (list (option Z)) :=
  match args with
  | [] => Some []
  | Terminator.Arg v :: args =>
    let* word := regs (register_of_value v) in
    let* argument_words := read_arguments regs args in
    Some (Some word :: argument_words)
  | Terminator.Omitted_since_unused :: args =>
    let* argument_words := read_arguments regs args in
    Some (None :: argument_words)
  end.

Fixpoint bind_parameters (parameters : list value)
    (argument_words : list (option Z)) (regs : registers) : option registers :=
  match parameters, argument_words with
  | [], [] => Some regs
  | parameter :: parameters, word :: argument_words =>
    bind_parameters parameters argument_words (write_register regs
      (register_of_value parameter) word)
  | _, _ => None
  end.

(* ======================================================================== *)
(* States and steps                                                         *)
(* ======================================================================== *)

Record state : Type :=
  { current_block : block;
    remaining : list instr;
    current_registers : registers }.

Definition enter (target : block) (regs : registers) : state :=
  {| current_block := target;
     remaining := block_body target;
     current_registers := regs |}.

(* The remaining instruction list is a suffix of the current block's body in a
   reachable state. Successor selection checks nonnegativity before conversion
   to nat, so negative indices do not select target 0. Switch edges have no
   arguments and may only enter parameterless blocks. *)
Inductive step : state -> state -> Prop :=
  | Step_instruction : ∀ (source : block) (insn : instr)
      (rest : list instr) (before_regs after_regs : registers),
      execute_instruction before_regs insn = Some after_regs ->
      step {| current_block := source; remaining := insn :: rest;
              current_registers := before_regs |}
           {| current_block := source; remaining := rest;
              current_registers := after_regs |}
  | Step_goto : ∀ (source target : block) (args : list Terminator.arg)
      (before_regs : registers) (argument_words : list (option Z))
      (after_regs : registers),
      terminator source = Terminator.Continue (Goto target) args ->
      read_arguments before_regs args = Some argument_words ->
      bind_parameters (params target) argument_words before_regs
        = Some after_regs ->
      step {| current_block := source; remaining := [];
              current_registers := before_regs |} (enter target after_regs)
  | Step_switch : ∀ (source : block) (index_value : value)
      (targets : list block) (regs : registers) (index_word : Z)
      (target : block),
      terminator source = Terminator.Switch index_value targets ->
      regs (register_of_value index_value) = Some index_word ->
      0 <= index_word ->
      nth_error targets (Z.to_nat index_word) = Some target ->
      params target = [] ->
      step {| current_block := source; remaining := [];
              current_registers := regs |} (enter target regs).

(* Finite execution, including zero steps. A finite stuck execution is not
   identified with a successful return. *)
Inductive steps : state -> state -> Prop :=
  | Steps_refl : ∀ (before : state), steps before before
  | Steps_cons : ∀ (before middle after : state),
      step before middle -> steps middle after -> steps before after.

(* [steps_until target before middle]: the execution from [before] first enters
   a state of block [target] at [middle], after finitely many steps from states
   of other blocks. With [step] deterministic ([step_deterministic]) this is a
   statement about the execution from [before], not one path among many. *)
Inductive steps_until (target : block) : state -> state -> Prop :=
  | Until_here : ∀ (before : state),
      current_block before = target -> steps_until target before before
  | Until_step : ∀ (before middle after : state),
      current_block before <> target -> step before middle -> steps_until
        target middle after ->
      steps_until target before after.

(* Execute an instruction list without taking the block's terminator. This is
   the retained header computation in the eligibility theorem. *)
Inductive executes_insns : registers -> list instr -> registers -> Prop :=
  | Insns_nil : ∀ (regs : registers), executes_insns regs [] regs
  | Insns_cons : ∀ (before middle after : registers) (insn : instr)
      (rest : list instr),
      execute_instruction before insn = Some middle ->
      executes_insns middle rest after ->
      executes_insns before (insn :: rest) after.

Definition bounded_registers (regs : registers) : Prop :=
  ∀ (reg : register) (word : Z), regs reg = Some word -> in_int64 word.

(* The parameter observers describe distinct parameters of their own block, and
   the indexed accessor agrees with the list. These are local compiler-data
   contracts, independent of execution and loop analysis. *)
Record parameter_layout : Prop :=
  { parameters_local : ∀ (b : block) (v : value), v ∈ (params b) ->
      ∃ (index : int), v = Block_param b index;
    parameters_distinct : ∀ (b : block),
      NoDup (map register_of_value (params b));
    parameter_lookup : ∀ (b : block) (index : int) (v : value),
      nth_int (params b) index = Some v -> param b index = v;
    parameter_identity : ∀ (b : block) (index : int) (v : value),
      nth_int (params b) index = Some v -> v = Block_param b index }.

(* ======================================================================== *)
(* Statements                                                               *)
(* ======================================================================== *)

(* The statements below are bound as propositions here, next to the
   definitions they are about; SsaSemanticsProofs.v proves them under the same
   names without the suffix, and [make -C rocq claims] checks that it does. *)

(* Each executable state has at most one successor. [steps_until] therefore
   speaks of the one execution from a state, not of one path among many. *)
Definition step_deterministic_statement : Prop :=
  ∀ (before after other_after : state),
  step before after -> step before other_after -> after = other_after.
