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

val encode_load_literal : opc:int -> v:int -> imm19:int -> rt:int -> int32

val encode_load_store_unscaled :
  size:int -> vr:int -> opc:int -> imm9:int -> rn:int -> rt:int -> int32

val encode_load_store_post_indexed :
  size:int -> vr:int -> opc:int -> imm9:int -> rn:int -> rt:int -> int32

val encode_load_store_pre_indexed :
  size:int -> vr:int -> opc:int -> imm9:int -> rn:int -> rt:int -> int32

val encode_load_store_unsigned_offset :
  size:int -> vr:int -> opc:int -> imm12:int -> rn:int -> rt:int -> int32

val encode_load_store_pair_post_indexed :
  opc:int -> v:int -> l:int -> imm7:int -> rt2:int -> rn:int -> rt:int -> int32

val encode_load_store_pair_pre_indexed :
  opc:int -> v:int -> l:int -> imm7:int -> rt2:int -> rn:int -> rt:int -> int32

val encode_load_store_pair_signed_offset :
  opc:int -> v:int -> l:int -> imm7:int -> rt2:int -> rn:int -> rt:int -> int32

val encode_load_store_gp_sized :
  all_sections:All_section_states.t ->
  Section_state.t ->
  instr_name:string ->
  size:int ->
  opc:int ->
  rd:[`GP of 'a] Reg.t ->
  [ `Base_reg
  | `Offset_imm
  | `Offset_unscaled
  | `Offset_sym
  | `Literal
  | `Pre
  | `Post ]
  Operand.Addressing_mode.t ->
  int32

val encode_load_store_gp :
  all_sections:All_section_states.t ->
  Section_state.t ->
  instr_name:string ->
  opc:int ->
  rd:[`GP of 'a] Reg.t ->
  [ `Base_reg
  | `Offset_imm
  | `Offset_unscaled
  | `Offset_sym
  | `Literal
  | `Pre
  | `Post ]
  Operand.Addressing_mode.t ->
  int32

val encode_load_store_byte :
  all_sections:All_section_states.t ->
  Section_state.t ->
  instr_name:string ->
  opc:int ->
  rd:[`GP of 'a] Reg.t ->
  [ `Base_reg
  | `Offset_imm
  | `Offset_unscaled
  | `Offset_sym
  | `Literal
  | `Pre
  | `Post ]
  Operand.Addressing_mode.t ->
  int32

val encode_load_store_halfword :
  all_sections:All_section_states.t ->
  Section_state.t ->
  instr_name:string ->
  opc:int ->
  rd:[`GP of 'a] Reg.t ->
  [ `Base_reg
  | `Offset_imm
  | `Offset_unscaled
  | `Offset_sym
  | `Literal
  | `Pre
  | `Post ]
  Operand.Addressing_mode.t ->
  int32

val encode_load_acquire : rd:[`GP of 'a] Reg.t -> rn:[`GP of 'b] Reg.t -> int32

val encode_memory_barrier : op2:int -> Memory_barrier.t -> int32

val encode_load_store_pair_gp :
  instr_name:string ->
  l:int ->
  rt1:[`GP of 'a] Reg.t ->
  rt2:[`GP of 'b] Reg.t ->
  [`Offset_pair | `Pre_pair | `Post_pair] Operand.Addressing_mode.t ->
  int32

val encode_load_store_simd_fp :
  all_sections:All_section_states.t ->
  Section_state.t ->
  instr_name:string ->
  is_load:bool ->
  rd:[`Neon of [`Scalar of 's]] Reg.t ->
  [ `Base_reg
  | `Offset_imm
  | `Offset_unscaled
  | `Offset_sym
  | `Literal
  | `Pre
  | `Post ]
  Operand.Addressing_mode.t ->
  int32
