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

val decode_shift_kind_int : 'a Operand.Shift.Kind.t -> int

(** Extract shift amount from an Imm.t. Used for shifted register operations
    where the instruction types guarantee a Six immediate, but OCaml can't
    prove this statically due to [<] constraints. *)
val decode_shift_amount_six : 'a Operand.Imm.t -> int

val is_sp_reg : 'a Reg.t -> bool

val encode_add_sub_shifted_register :
  sf:int ->
  op:int ->
  s:int ->
  shift:int ->
  rm:int ->
  imm6:int ->
  rn:int ->
  rd:int ->
  int32

val encode_add_sub_extended_register :
  sf:int ->
  op:int ->
  s:int ->
  rm:int ->
  option:int ->
  imm3:int ->
  rn:int ->
  rd:int ->
  int32

val encode_add_sub_immediate :
  sf:int ->
  op:int ->
  s:int ->
  sh:int ->
  imm12:int ->
  rn:[`GP of 'a] Reg.t ->
  rd:[`GP of 'b] Reg.t ->
  int32

val encode_add_sub_imm_auto_shift :
  op:int ->
  s:int ->
  imm12:int ->
  shift_opt:'a option ->
  rn:[`GP of 'b] Reg.t ->
  rd:[`GP of 'c] Reg.t ->
  int32

val encode_add_sub_shifted_reg :
  op:int ->
  s:int ->
  shift:int ->
  imm6:int ->
  rd:[`GP of 'a] Reg.t ->
  rn:[`GP of 'b] Reg.t ->
  rm:[`GP of 'c] Reg.t ->
  int32
