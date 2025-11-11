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

let split_21bit_immediate (imm21 : int) : int * int =
  if imm21 < -0x100000 || imm21 > 0xfffff
  then
    Misc.fatal_errorf
      "Immediate value %d (0x%x) out of range for 21-bit signed immediate (max \
       ±0x100000)"
      imm21 imm21;
  let immlo = imm21 land 0b11 in
  let immhi = (imm21 lsr 2) land 0x7ffff in
  immlo, immhi

(* PC-relative addressing - C4.1.92.2 *)
let encode_adr ~op ~immlo ~immhi ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int immlo) 29) in
  let result = logor result (shift_left (of_int 0b10000) 24) in
  let result = logor result (shift_left (of_int immhi) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
