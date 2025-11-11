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

(* Helper to extract ftype from scalar precision: S=0, D=1 *)
let scalar_ftype (type a) (s : [`Scalar of a] Neon_reg_name.t) : int =
  match s with
  | Scalar S -> 0
  | Scalar D -> 1
  | Scalar B | Scalar H | Scalar Q ->
    Misc.fatal_error "scalar_ftype: unsupported scalar precision"

(* Floating-point data-processing (1 source) - C4.1.95.34

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | opcode | 10000 | Rn | Rd

   opcode: 000001=FABS, 000010=FNEG, 000011=FSQRT *)
let encode_fp_1_source ~ftype ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int opcode) 15) in
  let result = logor result (shift_left (of_int 0b10000) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (2 source) - C4.1.95.38 *)
let encode_fp_2_source ~ftype ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point conditional select - C4.1.95.39 *)
let encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int cond) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point data-processing (3 source) - C4.1.95.40 *)
let encode_fp_3_source ~ftype ~o1 ~rm ~o0 ~ra ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11111) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int o1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int ra) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point compare - C4.1.95.35

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | Rm | op=00 | 1000 | Rn | opcode2

   opcode2: 00000=FCMP(reg), 01000=FCMP(zero), 10000=FCMPE(reg),
   11000=FCMPE(zero) *)
let encode_fp_compare ~ftype ~rm ~opc2 ~rn =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int 0b00) 14) in
  let result = logor result (shift_left (of_int 0b1000) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int opc2) in
  result

(* Floating-point <-> integer conversion - C4.1.95.33

   Format: sf | 0 | S=0 | 11110 | ftype | 1 | rmode | opcode | 000000 | Rn | Rd

   Common opcodes:

   - SCVTF (int->FP): rmode=00, opcode=010

   - UCVTF (int->FP): rmode=00, opcode=011

   - FCVTNS (FP->int, nearest): rmode=00, opcode=000

   - FCVTPS (FP->int, +inf): rmode=01, opcode=000

   - FCVTMS (FP->int, -inf): rmode=10, opcode=000

   - FCVTZS (FP->int, zero): rmode=11, opcode=000

   - FCVTNU (FP->int, nearest unsigned): rmode=00, opcode=001

   - FCVTZU (FP->int, zero unsigned): rmode=11, opcode=001

   - FMOV (FP->GP or GP->FP): rmode=00, opcode=110/111 *)
let encode_fp_int_conv ~sf ~ftype ~rmode ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rmode) 19) in
  let result = logor result (shift_left (of_int opcode) 16) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Floating-point immediate - C4.1.95.36

   Format: M=0 | 0 | S=0 | 11110 | ftype | 1 | imm8 | 100 | imm5=00000 | Rd *)
let encode_fp_immediate ~ftype ~imm8 ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b11110) 24) in
  let result = logor result (shift_left (of_int ftype) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int imm8) 13) in
  let result = logor result (shift_left (of_int 0b100) 10) in
  let result = logor result (of_int rd) in
  result
