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

[@@@ocaml.warning "+a-40-41-42"]

(** Simple loop-termination analysis driven by basic induction variables.

    For each loop, we examine its exit branch (restricted to a two-target
    [Switch] terminator at the loop header where exactly one target is in the
    loop and the other is outside). If a basic induction variable is the
    comparison's IV-related operand, and the IV's monotonic direction makes the
    continue-condition eventually fail (accounting for 64-bit wrap-around, see
    {!Loop_comparisons}), the loop terminates. Otherwise the result is
    [Unknown]. *)

open Ssa.Export

type t =
  | Terminates
  | Unknown

val analyze :
  op_def:Induction_var.op_def ->
  Induction_var.loop ->
  Induction_var.biv list ->
  t

type exit_branch =
  { condition : finished Value.t;
    continue_when_true : bool;
    exit_target : finished Block.t  (** the out-of-loop target of the branch *)
  }

(** Identify the loop's exit branch when it has the simple shape the termination
    analysis recognises: a two-target [Switch] terminator on the loop header
    with exactly one of its targets inside the loop body. Returns [None]
    otherwise. *)
val find_exit_branch : Induction_var.loop -> exit_branch option
