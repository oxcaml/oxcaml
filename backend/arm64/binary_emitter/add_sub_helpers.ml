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

(* Decode shift type and amount for add/sub shifted register instructions.
   Returns (shift_type, amount) where shift_type is 00=LSL, 01=LSR, 10=ASR. *)
let decode_shift_kind_int : type a. a Operand.Shift.Kind.t -> int =
 fun kind ->
  match kind with Operand.Shift.Kind.LSL -> 0b00 | LSR -> 0b01 | ASR -> 0b10

let decode_shift_amount_six : type a. a Operand.Imm.t -> int =
 fun amount ->
  match amount with
  | Six n -> n
  | Twelve _ | Seven_signed_scaled _ | Nine_signed_unscaled _
  | Twelve_unsigned_scaled _ | Sixteen_unsigned _ | Sym _ | Float _
  | Nativeint _ ->
    Misc.fatal_error "decode_shift_amount_six: expected Six immediate"

(* Check if a register is SP (stack pointer) by examining its name *)
let is_sp_reg : type a. a Reg.t -> bool =
 fun r ->
  match r.reg_name with
  | GP GP_reg_name.SP -> true
  | GP GP_reg_name.WSP -> true
  | GP (W | X | WZR | XZR | LR | FP) -> false
  | Neon _ -> false

let encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm ~imm6 ~rn ~rd =
  let open Int32 in
  let max_shift = if sf = 1 then 63 else 31 in
  if imm6 < 0 || imm6 > max_shift
  then Misc.fatal_errorf "ADD/SUB shift amount out of range: %d" imm6 ();
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b01011) 24) in
  let result = logor result (shift_left (of_int shift) 22) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int imm6) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Add/subtract (extended register) encoding - C4.1.92.2 Encoding: sf op S 01011
   00 1 Rm option imm3 Rn Rd Used when Rn is SP or when an extend operation is
   needed *)
let encode_add_sub_extended_register ~sf ~op ~s ~rm ~option ~imm3 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b01011) 24) in
  let result = logor result (shift_left (of_int 0b00) 22) in
  let result = logor result (shift_left (of_int 1) 21) in
  (* bit 21 = 1 for extended *)
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int option) 13) in
  let result = logor result (shift_left (of_int imm3) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Add/subtract (immediate) - C4.1.92.3 *)
let encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int op) 30) in
  let result = logor result (shift_left (of_int s) 29) in
  let result = logor result (shift_left (of_int 0b100010) 23) in
  let result = logor result (shift_left (of_int sh) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int (Reg.gp_encoding rd)) in
  result

(* Helper to encode add/sub immediate with automatic shift detection. When imm12
   > 0xFFF but is a multiple of 4096 and the quotient fits in 12 bits, use sh=1
   and encode imm12 / 4096. This matches the assembler's behavior when given
   large immediate values like #0x6000 which become #0x6, lsl #12. *)
let encode_add_sub_imm_auto_shift ~op ~s ~imm12 ~shift_opt ~rn ~rd =
  let sf = Reg.gp_sf rd in
  let sh_from_opt = match shift_opt with Some _ -> 1 | None -> 0 in
  let sh, actual_imm12 =
    if imm12 <= 0xFFF
    then sh_from_opt, imm12
    else if sh_from_opt = 1
    then
      Misc.fatal_errorf
        "Cannot encode add/sub immediate with explicit shift and value > 0xFFF"
    else if imm12 land 0xFFF = 0 && imm12 lsr 12 <= 0xFFF
    then 1, imm12 lsr 12
    else
      Misc.fatal_errorf "Cannot encode add/sub immediate %d (0x%x)" imm12 imm12
  in
  encode_add_sub_immediate ~sf ~op ~s ~sh ~imm12:actual_imm12 ~rn ~rd

(* Helper to encode add/sub shifted register instructions.

   - op: 0=ADD, 1=SUB - s: 0=no flags, 1=set flags

   When Rn is SP, we must use extended register encoding because the shifted
   register form interprets register 31 as XZR, not SP. *)
let encode_add_sub_shifted_reg ~op ~s ~shift ~imm6 ~rd ~rn ~rm =
  let sf = Reg.gp_sf rd in
  let rd_enc = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let rm_enc = Reg.gp_encoding rm in
  (* Check if Rn is SP - if so, use extended register encoding *)
  if is_sp_reg rn && shift = 0 && imm6 = 0
  then
    (* Use extended register form with option=011 (UXTX/LSL for 64-bit) *)
    encode_add_sub_extended_register ~sf ~op ~s ~rm:rm_enc ~option:0b011 ~imm3:0
      ~rn:rn_enc ~rd:rd_enc
  else
    encode_add_sub_shifted_register ~sf ~op ~s ~shift ~rm:rm_enc ~imm6
      ~rn:rn_enc ~rd:rd_enc
