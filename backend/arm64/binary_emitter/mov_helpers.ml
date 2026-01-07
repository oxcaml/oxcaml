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

let encode_six_bit_shift (shift_opt : [`Shift of _ * [`Six]] Operand.t option) =
  match shift_opt with
  | Some (Shift shift) -> (match shift.amount with Six n -> n) / 16
  | None -> 0

(* Move wide (immediate) encoding - C4.1.92.7 Used for MOVN, MOVZ, MOVK *)
let encode_move_wide ~sf ~opc ~hw ~imm16 ~rd =
  let open Int32 in
  if imm16 < 0 || imm16 > 0xFFFF
  then Misc.fatal_errorf "MOVZ/MOVN/MOVK immediate out of range: %d" imm16 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int opc) 29) in
  let result = logor result (shift_left (of_int 0b100101) 23) in
  let result = logor result (shift_left (of_int hw) 21) in
  let result = logor result (shift_left (of_int imm16) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result
