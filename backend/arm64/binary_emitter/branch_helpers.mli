(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open Arm64_ast.Ast

val compute_branch_imm26 :
  Section_state.t ->
  instr_name:string ->
  reloc_kind:(Relocation.Kind.target_with_addend -> Relocation.Kind.t) ->
  'a Symbol.t ->
  int

val compute_branch_imm19 :
  Section_state.t -> instr_name:string -> 'a Symbol.t -> int

val compute_branch_imm14 :
  Section_state.t -> instr_name:string -> 'a Symbol.t -> int

val encode_conditional_branch : imm19:int -> cond:int -> int32

val encode_branch_register : opc:int -> rn:[`GP of 'a] Reg.t -> int32

val encode_branch_immediate : op:int -> imm26:int -> int32

val encode_compare_branch : sf:int -> op:int -> imm19:int -> rt:int -> int32

val encode_test_branch :
  b5:int -> op:int -> b40:int -> imm14:int -> rt:int -> int32
