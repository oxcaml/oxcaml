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
module Symbol = Arm64_ast.Ast.Symbol

(* Helper to compute a 26-bit PC-relative offset for B/BL instructions. If the
   symbol is undefined, creates a relocation and returns 0.

   On Linux ELF (not macOS), we also emit relocations for global symbols even
   when they're defined in the same file. This matches the system assembler's
   behavior which uses RELA relocations with zero in the instruction for all
   global symbol references, allowing for symbol interposition at link time. *)
let compute_branch_imm26 state ~instr_name ~reloc_kind (sym : _ Symbol.t) =
  let is_global_symbol =
    match sym.target with
    | Symbol s ->
      Option.is_some (Section_state.find_symbol_offset_in_bytes state s)
    | Label _ -> false
  in
  (* Create relocation with addend 0 (branching to symbol itself) *)
  let make_reloc () =
    let r = { Relocation.Kind.target = sym.target; addend = 0 } in
    Section_state.add_relocation_at_current_offset state
      ~reloc_kind:(reloc_kind r)
  in
  (* On Linux ELF, emit relocations for global symbols to match assembler *)
  let macosx = String.equal Config.system "macosx" in
  if (not macosx) && is_global_symbol
  then (
    make_reloc ();
    0)
  else
    match Section_state.find_target_offset_in_bytes state sym.target with
    | None ->
      (* Symbol is undefined - create a relocation and use 0 as placeholder *)
      make_reloc ();
      0
    | Some target_offset ->
      let pc_relative_offset =
        target_offset - Section_state.offset_in_bytes state
      in
      let target_key () = Relocation.target_to_string sym.target in
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
          instr_name pc_relative_offset (target_key ());
      let imm26 = pc_relative_offset / 4 in
      if imm26 < -0x2000000 || imm26 > 0x1FFFFFF
      then
        Misc.fatal_errorf
          "%s offset %d to symbol '%s' out of range (max ±128MB)" instr_name
          pc_relative_offset (target_key ());
      imm26

(* Helper to compute a 19-bit PC-relative offset for CBZ/CBNZ instructions *)
let compute_branch_imm19 state ~instr_name (sym : _ Symbol.t) =
  match Section_state.find_target_offset_in_bytes state sym.target with
  | None ->
    let target_key = Relocation.target_to_string sym.target in
    Misc.fatal_errorf "%s references undefined symbol '%s'" instr_name
      target_key
  | Some target_offset ->
    let pc_relative_offset =
      target_offset - Section_state.offset_in_bytes state
    in
    let target_key () = Relocation.target_to_string sym.target in
    if pc_relative_offset mod 4 <> 0
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
        instr_name pc_relative_offset (target_key ());
    let imm19 = pc_relative_offset / 4 in
    if imm19 < -0x40000 || imm19 > 0x3FFFF
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' out of range (max ±1MB)"
        instr_name pc_relative_offset (target_key ());
    imm19

(* Helper to compute a 14-bit PC-relative offset for TBZ/TBNZ instructions *)
let compute_branch_imm14 state ~instr_name (sym : _ Symbol.t) =
  match Section_state.find_target_offset_in_bytes state sym.target with
  | None ->
    let target_key = Relocation.target_to_string sym.target in
    Misc.fatal_errorf "%s references undefined symbol '%s'" instr_name
      target_key
  | Some target_offset ->
    let pc_relative_offset =
      target_offset - Section_state.offset_in_bytes state
    in
    let target_key () = Relocation.target_to_string sym.target in
    if pc_relative_offset mod 4 <> 0
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' must be 4-byte aligned"
        instr_name pc_relative_offset (target_key ());
    let imm14 = pc_relative_offset / 4 in
    if imm14 < -0x2000 || imm14 > 0x1FFF
    then
      Misc.fatal_errorf "%s offset %d to symbol '%s' out of range (max ±32KB)"
        instr_name pc_relative_offset (target_key ());
    imm14

(* Conditional branch (immediate) - C4.1.93.1

   Encoding: 0101010 | 0 | imm19 | o0 | cond

   o0=0 for B.cond *)
let encode_conditional_branch ~imm19 ~cond =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b01010100) 24) in
  let result = logor result (shift_left (of_int (imm19 land 0x7FFFF)) 5) in
  let result = logor result (shift_left (of_int 0) 4) in
  (* o0 = 0 for B.cond *)
  let result = logor result (of_int cond) in
  result

(* Unconditional branch (register) - C4.1.93.13

   Encoding: 1101011 | opc[3:0] | op2[4:0] | op3[5:0] | Rn | op4[4:0] *)
let encode_branch_register ~opc ~rn =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int 0b1101011) 25) in
  let result = logor result (shift_left (of_int opc) 21) in
  let result = logor result (shift_left (of_int 0b11111) 16) in
  (* op2 = 11111 *)
  let result = logor result (shift_left (of_int 0b000000) 10) in
  (* op3 = 000000 *)
  let result = logor result (shift_left (of_int (Reg.gp_encoding rn)) 5) in
  let result = logor result (of_int 0b00000) in
  (* op4 = 00000 *)
  result

(* Unconditional branch (immediate) - C4.1.93.14 Encoding: op | 00101 | imm26 *)
let encode_branch_immediate ~op ~imm26 =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int op) 31) in
  let result = logor result (shift_left (of_int 0b00101) 26) in
  let result = logor result (of_int (imm26 land 0x3FFFFFF)) in
  result

(* Compare and branch (immediate) - C4.1.93.15

   Encoding: sf | 011010 | op | imm19 | Rt *)
let encode_compare_branch ~sf ~op ~imm19 ~rt =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int sf) 31) in
  let result = logor result (shift_left (of_int 0b011010) 25) in
  let result = logor result (shift_left (of_int op) 24) in
  let result = logor result (shift_left (of_int (imm19 land 0x7FFFF)) 5) in
  let result = logor result (of_int rt) in
  result

(* Test and branch (immediate) - C4.1.93.16

   Encoding: b5 | 011011 | op | b40 | imm14 | Rt *)
let encode_test_branch ~b5 ~op ~b40 ~imm14 ~rt =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int b5) 31) in
  let result = logor result (shift_left (of_int 0b011011) 25) in
  let result = logor result (shift_left (of_int op) 24) in
  let result = logor result (shift_left (of_int b40) 19) in
  let result = logor result (shift_left (of_int (imm14 land 0x3FFF)) 5) in
  let result = logor result (of_int rt) in
  result
