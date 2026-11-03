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

(* Values needed at a program point, independently of any optimization.

   This file is to be reviewed. A register is live when some structural
   continuation reads it before redefining it: reads in later blocks and on
   later loop entries count, not just the operands of the next instruction,
   and so do infeasible paths; the definition is deliberately an
   overapproximation. [agree_on_live] is the condition the eligibility
   theorem (LoopEligibility.v) puts on the registers at the exit, and
   [register_live_at] says which registers [reached_header] requires to be
   bound at entry. Nothing here is extracted.

   That every live register has an available definition is proved in
   SsaLivenessProofs.v, which needs no review. *)

From Stdlib Require Import List.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlList.
From OxCamlStdlib Require Import OxCamlInt.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec SsaDominanceSpec
  SsaSemanticsSpec SsaExecutionSpec.
Import ListNotations.

(* Instruction operands are read before the instruction's result site;
   terminator operands are read at point zero. *)
Inductive operand_at : program_point -> value -> Prop :=
  | Instruction_operand : ∀ (b : block) (remaining : nat)
      (id : instr_id) (op : operation) (args : list value) (count : int)
      (arg : value),
      result_definition id op args count (b, remaining) -> arg ∈ args ->
      operand_at (b, S remaining) arg
  | Terminator_operand : ∀ (b : block) (args : list value) (arg : value),
      args ∈ (terminator_args (terminator b)) -> arg ∈ args ->
      operand_at (b, 0%nat) arg.

Definition register_defined_at (position : program_point)
  (reg : register) : Prop :=
  ∃ (v : value), value_definition v position ∧ register_of_value v = reg.

(* Definitions kill the old register value on the incoming edge. In particular,
   a later entry to a parameter's block does not read its old value merely
   because that parameter is used in the block. *)
Inductive register_live_at : program_point -> register -> Prop :=
  | Live_read : ∀ (position : program_point) (v : value),
      operand_at position v -> register_live_at position (register_of_value v)
  | Live_predecessor : ∀ (before after : program_point) (reg : register),
      program_point_edge before after -> ~ register_defined_at after reg ->
      register_live_at after reg -> register_live_at before reg.

Definition agree_on_live (position : program_point)
  (left right : registers) : Prop :=
  ∀ (reg : register),
    register_live_at position reg -> left reg = right reg.
