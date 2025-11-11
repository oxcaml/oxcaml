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

(** Extract ftype from scalar precision: S=0, D=1.
    Only valid for S and D scalar types. *)
val scalar_ftype : [`Scalar of 'a] Neon_reg_name.t -> int

val encode_fp_1_source : ftype:int -> opcode:int -> rn:int -> rd:int -> int32

val encode_fp_2_source :
  ftype:int -> rm:int -> opcode:int -> rn:int -> rd:int -> int32

val encode_fp_cond_select :
  ftype:int -> rm:int -> cond:int -> rn:int -> rd:int -> int32

val encode_fp_3_source :
  ftype:int -> o1:int -> rm:int -> o0:int -> ra:int -> rn:int -> rd:int -> int32

val encode_fp_compare : ftype:int -> rm:int -> opc2:int -> rn:int -> int32

val encode_fp_int_conv :
  sf:int -> ftype:int -> rmode:int -> opcode:int -> rn:int -> rd:int -> int32

val encode_fp_immediate : ftype:int -> imm8:int -> rd:int -> int32
