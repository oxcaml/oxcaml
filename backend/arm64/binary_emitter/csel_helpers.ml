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

(* Conditional select - C4.1.94.12

   Encoding: sf | op | S | 11010100 | Rm | cond | op2 | Rn | Rd *)
let encode_conditional_select ~sf ~op ~op2 ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int 0b0) 29) in
  (* S = 0 *)
  let result = logor result (shift_left (of_int 0b11010100) 21) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rm)) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int op2) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
