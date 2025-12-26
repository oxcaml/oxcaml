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

val vector_q_size : ('v, 's) Neon_reg_name.Vector.t -> int * int

(** Returns Q and the FP precision bit (sz) for FP vector operations.
    Only valid for V2S, V4S, V2D vector types. *)
val vector_q_fp_sz : (_, _) Neon_reg_name.Vector.t -> int * int

val simd_copy_imm5 : ('v, 's) Neon_reg_name.Vector.t -> int -> int

val simd_ins_element_imm4 : ('v, 's) Neon_reg_name.Vector.t -> int -> int

val encode_simd_copy :
  q:int -> op:int -> imm5:int -> imm4:int -> rn:int -> rd:int -> int32

val encode_simd_extract :
  q:int -> rm:int -> imm4:int -> rn:int -> rd:int -> int32

val encode_simd_modified_imm :
  q:int -> op:int -> abc:int -> cmode:int -> defgh:int -> rd:int -> int32

val encode_simd_across_lanes :
  q:int -> u:int -> size:int -> opcode:int -> rn:int -> rd:int -> int32

val encode_simd_permute :
  q:int -> size:int -> rm:int -> opcode:int -> rn:int -> rd:int -> int32

val vector_widening_size : ('v, 's) Neon_reg_name.Vector.t -> int

val encode_simd_two_reg_misc :
  q:int -> u:int -> size:int -> opcode:int -> rn:int -> rd:int -> int32

val encode_simd_three_same :
  q:int ->
  u:int ->
  size:int ->
  rm:int ->
  opcode:int ->
  rn:int ->
  rd:int ->
  int32

val encode_simd_three_different :
  q:int ->
  u:int ->
  size:int ->
  rm:int ->
  opcode:int ->
  rn:int ->
  rd:int ->
  int32

val encode_simd_shift_imm :
  q:int ->
  u:int ->
  immh:int ->
  immb:int ->
  opcode:int ->
  rn:int ->
  rd:int ->
  int32
