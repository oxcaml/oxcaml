(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 Jane Street Group LLC                              *
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

(** Helper functions that convert compiler [Reg.t] values to ARM64 operands.

    This module provides the bridge between the compiler's register allocator
    (which uses [Reg.t]) and the ARM64 AST (which uses integer register
    indices). *)

module Ast = Arm64_ast.Ast
module Element_to_GP = Ast.Element_to_GP
module Smov_element_to_GP = Ast.Smov_element_to_GP

(** {1 Type aliases for register operands} *)

type gp_w = [`Reg of [`GP of [`W]]] Ast.Operand.t

type gp_x = [`Reg of [`GP of [`X]]] Ast.Operand.t

type scalar_s = [`Reg of [`Neon of [`Scalar of [`S]]]] Ast.Operand.t

type scalar_d = [`Reg of [`Neon of [`Scalar of [`D]]]] Ast.Operand.t

type scalar_q = [`Reg of [`Neon of [`Scalar of [`Q]]]] Ast.Operand.t

type v2s = [`Reg of [`Neon of [`Vector of [`V2S] * [`S]]]] Ast.Operand.t

type v4s = [`Reg of [`Neon of [`Vector of [`V4S] * [`S]]]] Ast.Operand.t

type v2d = [`Reg of [`Neon of [`Vector of [`V2D] * [`D]]]] Ast.Operand.t

type v8b = [`Reg of [`Neon of [`Vector of [`V8B] * [`B]]]] Ast.Operand.t

type v16b = [`Reg of [`Neon of [`Vector of [`V16B] * [`B]]]] Ast.Operand.t

type v4h = [`Reg of [`Neon of [`Vector of [`V4H] * [`H]]]] Ast.Operand.t

type v8h = [`Reg of [`Neon of [`Vector of [`V8H] * [`H]]]] Ast.Operand.t

(** {1 GP register operands} *)

val reg_w : Reg.t -> gp_w

val reg_x : Reg.t -> gp_x

(** {1 Scalar FP/SIMD register operands} *)

val reg_d : Reg.t -> scalar_d

val reg_s : Reg.t -> scalar_s

(** Address a Float register as S to access its lower 32 bits. Used for
    reinterpret casts between Float and Float32. *)
val reg_s_of_float : Reg.t -> scalar_s

(** Address a Vec128 register as D to access its lower 64 bits. Used for scalar
    extraction/insertion operations. *)
val reg_d_of_vec128 : Reg.t -> scalar_d

(** Address a Vec128 register as S to access its lower 32 bits. Used for scalar
    extraction/insertion operations. *)
val reg_s_of_vec128 : Reg.t -> scalar_s

val reg_q : Reg.t -> scalar_q

(** {1 Vector register operands} *)

val reg_v2s : Reg.t -> v2s

(** Like [reg_v2s] but expects Float machtype. Used for 64-bit vector operations
    like Zip1_f32. *)
val reg_v2s_of_float : Reg.t -> v2s

val reg_v4s : Reg.t -> v4s

val reg_v2d : Reg.t -> v2d

val reg_v16b : Reg.t -> v16b

val reg_v8h : Reg.t -> v8h

val reg_v8b : Reg.t -> v8b

val reg_v4h : Reg.t -> v4h

val reg_v2d_operand : Reg.t -> v2d

val reg_v16b_operand : Reg.t -> v16b

(** {1 Operand tuple helpers for SIMD instructions} *)

val v4s_v4s_v4s : Linear.instruction -> v4s * v4s * v4s

val v2d_v2d_v2d : Linear.instruction -> v2d * v2d * v2d

val v8h_v8h_v8h : Linear.instruction -> v8h * v8h * v8h

val v16b_v16b_v16b : Linear.instruction -> v16b * v16b * v16b

val v4s_v4s : Linear.instruction -> v4s * v4s

val v2d_v2d : Linear.instruction -> v2d * v2d

val v8h_v8h : Linear.instruction -> v8h * v8h

val v16b_v16b : Linear.instruction -> v16b * v16b

(** {1 Polymorphic scalar FP register helpers} *)

type scalar_fp_regs_3 = private
  | S_regs : (scalar_s as 'op) * 'op * 'op -> scalar_fp_regs_3
  | D_regs : (scalar_d as 'op) * 'op * 'op -> scalar_fp_regs_3

type scalar_fp_regs_4 = private
  | S_regs : (scalar_s as 'op) * 'op * 'op * 'op -> scalar_fp_regs_4
  | D_regs : (scalar_d as 'op) * 'op * 'op * 'op -> scalar_fp_regs_4

val reg_fp_operand_3 : Reg.t -> Reg.t -> Reg.t -> scalar_fp_regs_3

val reg_fp_operand_4 : Reg.t -> Reg.t -> Reg.t -> Reg.t -> scalar_fp_regs_4

(** {1 Memory operands} *)

(** Convert a compiler [Reg.t] to an AST GP register for use as a memory base.

    The compiler's [Reg.t] never represents SP (x31) - SP is always handled
    specially via [Ast.Reg.sp] or [Ast.DSL.sp]. The [int_reg_name_to_arch_index]
    array maps compiler register indices to architecture indices 0-28, never 31.
    Therefore this function always returns an X register, never SP. *)
val gp_reg_of_reg : Reg.t -> [`GP of [`X]] Ast.Reg.t

val mem :
  [`GP of [< `X | `SP]] Ast.Reg.t -> [`Mem of [> `Base_reg]] Ast.Operand.t

val addressing :
  scale:int ->
  Arch.addressing_mode ->
  Reg.t ->
  [ `Mem of
    [> `Offset_twelve_unsigned_scaled
    | `Offset_nine_signed_unscaled
    | `Offset_sym ] ]
  Ast.Operand.t

(** Result type for [stack]. Either [Stack_operand operand] if the offset can be
    encoded directly, or a large offset variant if the offset exceeds the
    immediate range and requires a multi-instruction sequence. *)
type 'a stack_result = private
  | Stack_operand of 'a
  | Stack_large_offset_sp of { bytes : int }
  | Stack_large_offset_domainstate of { bytes : int }

(** [stack] returns a memory operand for accessing a stack slot.

    For small offsets that fit in the immediate encoding, returns
    [Stack_operand operand]. For large offsets (with large stack frames),
    returns [Stack_large_offset_sp offset] (for SP-relative slots) or
    [Stack_large_offset_domainstate offset] (for domain state slots). The caller
    must then emit instructions to compute the address (typically: mov base to
    tmp register, add offset, access via tmp). *)
val stack :
  stack_offset:int ->
  contains_calls:bool ->
  num_stack_slots:int Stack_class.Tbl.t ->
  Reg.t ->
  [`Mem of [> `Offset_twelve_unsigned_scaled | `Offset_nine_signed_unscaled]]
  Ast.Operand.t
  stack_result

val mem_symbol_reg :
  [`GP of [< `X | `SP]] Ast.Reg.t ->
  reloc:[`Twelve] Ast.Symbol.same_unit_or_reloc ->
  ?offset:int ->
  Asm_targets.Asm_symbol.t ->
  [`Mem of [> `Offset_sym]] Ast.Operand.t

val mem_label :
  [`GP of [< `X | `SP]] Ast.Reg.t ->
  reloc:[`Twelve] Ast.Symbol.same_unit_or_reloc ->
  ?offset:int ->
  Asm_targets.Asm_label.t ->
  [`Mem of [> `Offset_sym]] Ast.Operand.t
