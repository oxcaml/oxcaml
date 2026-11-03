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

(* Word observations of surrounding SSA code, and the contract on the
   function's inputs, [reached_header].

   This file is to be reviewed (section 3.3 of ROCQ-REVIEW-GUIDE.md). Integer
   operations and Goto/Switch use [SsaSemanticsSpec.step]. An opaque
   instruction may produce arbitrary signed-word observations in exactly its
   declared result registers. An opaque terminator may take any CFG successor
   and supply arbitrary word observations for its parameters. Memory and
   other effects are deliberately unconstrained, not treated as no-ops: this
   relation is used only for the unchanged prefix before a loop. The loop's
   silent execution still uses [step].

   This is an overapproximation, including nondeterministic opaque outputs,
   not a concrete semantics of calls, memory or vector values. [Push_trap]
   and [Pop_trap] change the trap stack, which no register observes, so they
   step without changing any register; exceptional transfers happen at Call
   and Raise terminators, whose handler is among [all_successors], and are
   therefore opaque transfers like any other. Mid-block operations never
   transfer control in this SSA. DeleteEmptyLoops.graph_supported rejects
   graphs where an exactly modeled operation family has other than one
   result, so that every prefix instruction has a rule here.

   Registers hold a word observation, [option Z] within [in_int64]. A
   register of vector type has no faithful such observation; read it as an
   arbitrary in-range word. No modeled operation reads such a register and no
   accepted loop writes one, so nothing concluded depends on it. Nothing here
   is extracted.

   The invariants and available-value equations the observation semantics
   preserves are proved in SsaContextProofs.v, which needs no review. *)

From Stdlib Require Import ZArith List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlInt OxCamlInt64 OxCamlList.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaDominanceSpec
  SsaSemanticsSpec SsaProgressSpec.
Import ListNotations.
Local Open Scope Z_scope.

(* The result registers of an instruction with [result_count] results. *)
Definition result_register (id : instr_id) (result_count : int)
  (reg : register)
    : Prop :=
  ∃ (index : int),
    reg = Result id index ∧ 0 <= to_Z index < to_Z result_count.

(* An opaque effect writes only its declared outputs. Each output is an
   arbitrary word observation; all other register observations persist. *)
Definition opaque_outputs (written : register -> Prop)
    (before after : registers) : Prop :=
  (∀ (reg : register), ~ written reg -> after reg = before reg)
  ∧ (∀ (reg : register), written reg ->
        ∃ (word : Z), in_int64 word ∧ after reg = Some word).

Inductive context_step : state -> state -> Prop :=
  | Context_integer : ∀ (before after : state),
      step before after -> context_step before after
  | Context_opaque_instruction : ∀ (b : block) (id : instr_id)
      (op : operation) (args : list value) (count : int) (rest : list instr)
      (before after : registers) (words : list Z),
      modeled_operation op args = false ->
      read_values before args = Some words ->
      opaque_outputs (result_register id count) before after ->
      context_step
        {| current_block := b; remaining := Op id op args count :: rest;
           current_registers := before |}
        {| current_block := b; remaining := rest; current_registers := after |}
  | Context_opaque_transfer : ∀ (source target : block)
      (before after : registers),
      terminator_pure_control (terminator source) = false ->
      target ∈ (all_successors source) ->
      opaque_outputs (fun reg => reg ∈ (map register_of_value (params
        target)))
        before after ->
      context_step
        {| current_block := source; remaining := [];
           current_registers := before |}
        (enter target after)
  | Context_trap_marker : ∀ (b : block) (marker : instr)
      (rest : list instr) (regs : registers),
      is_trap_marker marker = true ->
      context_step
        {| current_block := b; remaining := marker :: rest;
           current_registers := regs |}
        {| current_block := b; remaining := rest; current_registers := regs |}.

Inductive context_steps : state -> state -> Prop :=
  | Context_refl : ∀ (s : state), context_steps s s
  | Context_cons : ∀ (before middle after : state),
      context_step before middle -> context_steps middle after ->
      context_steps before after.

(* The inputs the eligibility theorem speaks of: the function is entered with
   machine words in its registers and its live parameters bound, the
   observation semantics takes it to [header] with registers [header_regs], and
   from there the exact execution takes only defined Switch indices
   (out-of-range indices are undefined behaviour in SSA). *)
Record reached_header (entry header : block)
    (initial_regs header_regs : registers) : Prop :=
  { inputs_bounded : bounded_registers initial_regs;
    live_inputs_defined : live_registers_defined (block_entry_point entry)
      initial_regs;
    header_reached :
      context_steps (enter entry initial_regs) (enter header header_regs);
    switches_defined : switches_in_range (enter header header_regs) }.
