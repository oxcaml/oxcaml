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

(* The fragment of SSA code the loop analyses accept: constants, counter
   increments and decrements and integer comparisons, with Goto and Switch
   control flow; and the one shape of instruction the observation semantics
   has no rule for. The extraction of this file, SsaSemantics.ml, is the
   implementation.

   This file needs no review: SsaSemanticsProofs.v proves that every
   instruction [supported_instruction] accepts executes when its operands are
   bound ([supported_instruction_executes]), and DeleteEmptyLoops.v that a
   graph [context_instruction_supported] accepts everywhere has a rule of
   SsaContextSpec.v for each of its instructions. The arity checks of the
   whitelist are part of those proofs: a malformed instruction must not
   receive a totality guarantee. *)

From Stdlib Require Import Bool List.
From OxCamlStdlib Require Import OxCamlInt.
From OxCamlBackend Require Import Operation.
From OxCamlSsa Require Import SsaModel SsaGraphSpec.
Import ListNotations.

(* Constants, counter increments/decrements and integer comparisons only. Match
   the operand list as well as the opcode: a malformed instruction must not
   receive a totality guarantee. *)
Definition supported_operation {A : Type} (op : operation) (args : list A)
    : bool :=
  match op, args with
  | Const_int _, []
  | Intop (Iadd | Isub | Icomp _), [_; _]
  | Intop_imm (Iadd | Isub | Icomp _) _, [_] => true
  | _, _ => false
  end.

(* Every supported instruction defines exactly one machine-word result. *)
Definition supported_instruction (insn : instr) : bool :=
  match insn with
  | Op _ op args results =>
    (results =? one)%sint63 && supported_operation op args
  | Push_trap | Pop_trap => false
  end.

(* The supported control flow is Goto and Switch, as for the original effect
   check, but the instruction whitelist is deliberately narrower. *)
Definition supported_block (b : block) : bool :=
  forallb supported_instruction (block_body b)
  && terminator_pure_control (terminator b).

(* SsaContextSpec.v has a rule for every instruction except an exactly modeled
   operation family with other than its evaluator's single result. Checking the
   whole graph covers every prefix, not only dominators. *)
Definition context_instruction_supported (insn : instr) : bool :=
  match insn with
  | Op _ (Const_int _ | Intop (Iadd | Isub | Icomp _)
          | Intop_imm (Iadd | Isub | Icomp _ | Ior | Ilsl | Iasr | Ilsr) _)
      _ count => (count =? one)%sint63
  | Op _ _ _ _ | Push_trap | Pop_trap => true
  end.
