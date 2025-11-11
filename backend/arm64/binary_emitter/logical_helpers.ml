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

(* Logical (immediate) - C4.1.92.6 *)
let encode_logical_immediate ~sf ~opc ~n ~immr ~imms ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100100) 23) in
  let result = logor result (shift_left (of_int n) 22) in
  let result = logor result (shift_left (of_int immr) 16) in
  let result = logor result (shift_left (of_int imms) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Logical (shifted register) - C4.1.94.3 *)
let encode_logical_shifted_register ~sf ~opc ~shift ~n ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b01010) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int n) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Helper to encode logical shifted register instructions.

   - opc: 00=AND, 01=ORR, 10=EOR, 11=ANDS *)
let encode_logical_shifted_reg ~opc ~shift ~imm6 ~rd ~rn ~rm =
  let sf = Reg.gp_sf rd in
  let rd_enc = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let rm_enc = Reg.gp_encoding rm in
  encode_logical_shifted_register ~sf ~opc ~shift ~n:0 ~rm:rm_enc ~imm6
    ~rn:rn_enc ~rd:rd_enc
