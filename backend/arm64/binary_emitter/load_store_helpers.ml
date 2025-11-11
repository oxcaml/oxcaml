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

(* Load register (literal) - C4.1.96.19 *)
let encode_load_literal ~opc ~v ~imm19 ~rt =
  assert (imm19 >= 0 && imm19 <= 0x7ffff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b011) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int imm19) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (unscaled immediate) - C4.1.96.25 *)
let encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate post-indexed) - C4.1.96.26 *)
let encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b01) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (immediate pre-indexed) - C4.1.96.28 *)
let encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt =
  assert (imm9 >= 0 && imm9 <= 0x1ff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm9) 12) in
  let result = logor result (shift_left (of_int 0b11) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store register (unsigned immediate) - C4.1.96.27 *)
let encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt =
  assert (imm12 >= 0 && imm12 <= 0xfff);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int size) 30) in
  let result = logor result (shift_left (of_int 0b111) 27) in
  let result = logor result (shift_left (of_int vr) 26) in
  let result = logor result (shift_left (of_int 0b01) 24) in
  let result = logor result (shift_left (of_int opc) 22) in
  let result = logor result (shift_left (of_int imm12) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (post-indexed) - C4.1.96.15

   Encoding: opc[1:0] | 101 | V | 001 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_post_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b001) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (pre-indexed) - C4.1.96.17

   Encoding: opc[1:0] | 101 | V | 011 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_pre_indexed ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b011) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Load/store pair (signed offset) - C4.1.96.16

   Encoding: opc[1:0] | 101 | V | 010 | L | imm7 | Rt2 | Rn | Rt *)
let encode_load_store_pair_signed_offset ~opc ~v ~l ~imm7 ~rt2 ~rn ~rt =
  assert (imm7 >= -0x40 && imm7 <= 0x3f);
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int opc) 30) in
  let result = logor result (shift_left (of_int 0b101) 27) in
  let result = logor result (shift_left (of_int v) 26) in
  let result = logor result (shift_left (of_int 0b010) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int (imm7 land 0x7F)) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rt) in
  result

(* Generalized load/store encoding for byte/halfword/word/doubleword operations.
   size: 00=byte, 01=halfword, 10=word, 11=doubleword opc encodes the operation
   (load/store and signed extension) *)
let encode_load_store_gp_sized :
    type a.
    all_sections:All_section_states.t ->
    Section_state.t ->
    instr_name:string ->
    size:int ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun ~all_sections:_ state ~instr_name ~size ~opc ~rd addressing ->
  let vr = 0 in
  let rt = Reg.gp_encoding rd in
  match addressing with
  | Reg rn ->
    (* Use unsigned offset encoding with imm12=0 to match assembler behavior *)
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:0 ~rn ~rt
  | Offset_unscaled (rn, Nine_signed_unscaled imm) ->
    (* Prefer unsigned offset encoding when the offset is non-negative and
       properly aligned for the access size. This matches assembler behavior and
       provides a larger range for positive offsets. scale = 1 << size (1 for
       byte, 2 for half, 4 for word, 8 for double) *)
    let scale = 1 lsl size in
    let rn = Reg.gp_encoding rn in
    if imm >= 0 && imm mod scale = 0 && imm / scale <= 0xFFF
    then
      let imm12 = imm / scale in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt
    else
      let imm9 = imm land 0x1FF in
      encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt
  | Literal (_rn, sym) -> (
    (* This is encoded as "LDR (literal)" - only valid for word/doubleword *)
    if size < 0b10
    then Misc.fatal_errorf "%s does not support literal addressing" instr_name;
    match Section_state.find_target_offset_in_bytes state sym.target with
    | None ->
      Misc.fatal_errorf "%s (literal) references undefined symbol '%a' (rd=%s)"
        instr_name Symbol.print_target sym.target (Reg.name rd)
    | Some target_offset ->
      let pc_relative_offset =
        target_offset - Section_state.offset_in_bytes state
      in
      assert (pc_relative_offset > 0);
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%a' must be 4-byte aligned"
          instr_name pc_relative_offset Symbol.print_target sym.target;
      let imm19_unmasked = pc_relative_offset / 4 in
      if imm19_unmasked < -0x40000 || imm19_unmasked > 0x3ffff
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%a' out of range (max ±1MB)"
          instr_name pc_relative_offset Symbol.print_target sym.target;
      let imm19 = imm19_unmasked land 0x7FFFF in
      let v = 0 in
      encode_load_literal ~opc ~v ~imm19 ~rt)
  | Offset_imm (rn, Twelve_unsigned_scaled imm12) ->
    let max_imm12 = 0xfff in
    (* Scale depends on size: byte=1, half=2, word=4, double=8 *)
    let scale = 1 lsl size in
    if imm12 mod scale <> 0
    then
      Misc.fatal_errorf
        "%s offset %d must be aligned to %d-byte access size (rd=%s, rn=%s)"
        instr_name imm12 scale (Reg.name rd) (Reg.name rn);
    let rn = Reg.gp_encoding rn in
    let imm12_scaled = imm12 / scale in
    if imm12_scaled < 0 || imm12_scaled > max_imm12
    then
      Misc.fatal_errorf
        "%s offset %d (scaled: %d) out of range (max 0x%x * %d = 0x%x bytes)"
        instr_name imm12 imm12_scaled max_imm12 scale (max_imm12 * scale);
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:imm12_scaled ~rn ~rt
  | Offset_sym (rn, sym) -> (
    match sym.reloc with
    | Needs_reloc reloc ->
      let max_imm12 = 0xfff in
      let reloc_kind : Relocation.Kind.t =
        let r = { Relocation.Kind.target = sym.target; addend = sym.offset } in
        match reloc with
        | GOT_PAGE_OFF | GOT_LOWER_TWELVE -> R_AARCH64_LD64_GOT_LO12_NC r
        | PAGE_OFF | LOWER_TWELVE ->
          (* Use LDST64 relocation for 64-bit load/store instructions. The
             immediate encoding differs from ADD: it's scaled by 8. *)
          if size = 0b11 (* XXX should this ever be an "ADD" one? *)
          then R_AARCH64_LDST64_ABS_LO12_NC r
          else R_AARCH64_ADD_ABS_LO12_NC r
      in
      Section_state.add_relocation_at_current_offset state ~reloc_kind;
      let rn = Reg.gp_encoding rn in
      (* On RELA platforms (Linux), encode 0 in instruction - addend is in
         relocation. On REL platforms (macOS), encode addend in instruction. *)
      let offset =
        if Encode_directive.is_rela_platform () then 0 else sym.offset
      in
      let imm12_unmasked = offset lsr size in
      if imm12_unmasked < 0 || imm12_unmasked > max_imm12
      then
        Misc.fatal_errorf
          "%s symbol offset %d (shifted by %d) out of range (max 0x%x)"
          instr_name offset size max_imm12;
      let imm12 = imm12_unmasked land 0xFFF in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt)
  | Pre (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Post (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt

let encode_load_store_gp :
    type a.
    all_sections:All_section_states.t ->
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun ~all_sections state ~instr_name ~opc ~rd addressing ->
  let size =
    match rd.reg_name with
    | GP W | GP WZR | GP WSP -> 0b10
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b11
  in
  encode_load_store_gp_sized ~all_sections state ~instr_name ~size ~opc ~rd
    addressing

(* Byte load/store - size=00 *)
let encode_load_store_byte :
    type a.
    all_sections:All_section_states.t ->
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun ~all_sections state ~instr_name ~opc ~rd addressing ->
  encode_load_store_gp_sized ~all_sections state ~instr_name ~size:0b00 ~opc ~rd
    addressing

(* Halfword load/store - size=01 *)
let encode_load_store_halfword :
    type a.
    all_sections:All_section_states.t ->
    Section_state.t ->
    instr_name:string ->
    opc:int ->
    rd:[`GP of a] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun ~all_sections state ~instr_name ~opc ~rd addressing ->
  encode_load_store_gp_sized ~all_sections state ~instr_name ~size:0b01 ~opc ~rd
    addressing

(* Load-Acquire (LDAR) encoding. Format: size[31:30] | 001000 | o2[23] | L[22] |
   o1[21] | Rs[20:16] | o0[15] | Rt2[14:10] | Rn[9:5] | Rt[4:0] For LDAR: o2=1,
   L=1, o1=0, Rs=11111, o0=1, Rt2=11111 size: 10 for 32-bit, 11 for 64-bit *)
let encode_load_acquire :
    type a. rd:[`GP of a] Reg.t -> rn:[`GP of _] Reg.t -> int32 =
 fun ~rd ~rn ->
  let size =
    match rd.reg_name with
    | GP W | GP WZR | GP WSP -> 0b10
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b11
  in
  let rt = Reg.gp_encoding rd in
  let rn_enc = Reg.gp_encoding rn in
  let o2 = 1 in
  let l = 1 in
  let o1 = 0 in
  let o0 = 1 in
  let rs = 0b11111 in
  let rt2 = 0b11111 in
  let open Int32 in
  let result = shift_left (of_int size) 30 in
  let result = logor result (shift_left (of_int 0b001000) 24) in
  let result = logor result (shift_left (of_int o2) 23) in
  let result = logor result (shift_left (of_int l) 22) in
  let result = logor result (shift_left (of_int o1) 21) in
  let result = logor result (shift_left (of_int rs) 16) in
  let result = logor result (shift_left (of_int o0) 15) in
  let result = logor result (shift_left (of_int rt2) 10) in
  let result = logor result (shift_left (of_int rn_enc) 5) in
  let result = logor result (of_int rt) in
  result

(* Memory barrier encoding. Format: 1101 0101 0000 0011 0011 | CRm[11:8] |
   op2[7:5] | 11111

   - DMB: op2=101

   - DSB: op2=100 *)
let encode_memory_barrier ~op2 (barrier : Memory_barrier.t) =
  let crm =
    match barrier with
    | SY -> 0b1111
    | ST -> 0b1110
    | LD -> 0b1101
    | ISH -> 0b1011
    | ISHST -> 0b1010
    | ISHLD -> 0b1001
    | NSH -> 0b0111
    | NSHST -> 0b0110
    | NSHLD -> 0b0101
    | OSH -> 0b0011
    | OSHST -> 0b0010
    | OSHLD -> 0b0001
  in
  let open Int32 in
  (* 1101 0101 0000 0011 0011 = 0xD503_30 shifted appropriately *)
  let result = of_int 0b11010101000000110011 in
  let result = shift_left result 12 in
  let result = logor result (shift_left (of_int crm) 8) in
  let result = logor result (shift_left (of_int op2) 5) in
  let result = logor result (of_int 0b11111) in
  result

(* Encode LDP/STP instructions for GP registers. l=1 for load (LDP), l=0 for
   store (STP). opc: 00 for 32-bit (W), 10 for 64-bit (X) *)
let encode_load_store_pair_gp :
    type a b.
    instr_name:string ->
    l:int ->
    rt1:[`GP of a] Reg.t ->
    rt2:[`GP of b] Reg.t ->
    [< `Offset_pair | `Pre_pair | `Post_pair] Operand.Addressing_mode.t ->
    int32 =
 fun ~instr_name ~l ~rt1 ~rt2 addressing ->
  let opc =
    match rt1.reg_name with
    | GP W | GP WZR | GP WSP -> 0b00
    | GP X | GP XZR | GP SP | GP LR | GP FP -> 0b10
  in
  let scale = if opc = 0b10 then 8 else 4 in
  let v = 0 in
  let rt1_enc = Reg.gp_encoding rt1 in
  let rt2_enc = Reg.gp_encoding rt2 in
  let encode_with_alignment_check encode_fn rn imm =
    if imm mod scale <> 0
    then
      Misc.fatal_errorf "%s offset %d must be aligned to %d bytes" instr_name
        imm scale;
    let imm7 = imm / scale in
    let rn = Reg.gp_encoding rn in
    encode_fn ~opc ~v ~l ~imm7 ~rt2:rt2_enc ~rn ~rt:rt1_enc
  in
  match addressing with
  | Offset_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_signed_offset rn imm
  | Pre_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_pre_indexed rn imm
  | Post_pair (rn, Imm (Seven_signed_scaled imm)) ->
    encode_with_alignment_check encode_load_store_pair_post_indexed rn imm

(* Encode load/store for SIMD&FP registers.

   For LDR: S->opc=01, D->opc=01, Q->opc=11

   For STR: S->opc=00, D->opc=00, Q->opc=10

   size: S->10, D->11, Q->00 *)
let encode_load_store_simd_fp :
    type s.
    all_sections:All_section_states.t ->
    Section_state.t ->
    instr_name:string ->
    is_load:bool ->
    rd:[`Neon of [`Scalar of s]] Reg.t ->
    [ `Base_reg
    | `Offset_imm
    | `Offset_unscaled
    | `Offset_sym
    | `Literal
    | `Pre
    | `Post ]
    Operand.Addressing_mode.t ->
    int32 =
 fun ~all_sections:_ state ~instr_name ~is_load ~rd addressing ->
  let vr = 1 in
  let size, opc, scale =
    match rd.reg_name with
    | Neon (Scalar S) -> 0b10, (if is_load then 0b01 else 0b00), 4
    | Neon (Scalar D) -> 0b11, (if is_load then 0b01 else 0b00), 8
    | Neon (Scalar Q) -> 0b00, (if is_load then 0b11 else 0b10), 16
    | Neon (Scalar B) -> 0b00, (if is_load then 0b01 else 0b00), 1
    | Neon (Scalar H) -> 0b01, (if is_load then 0b01 else 0b00), 2
  in
  let rt = rd.index in
  match addressing with
  | Reg rn ->
    (* Use unsigned offset encoding with imm12=0 to match assembler behavior *)
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:0 ~rn ~rt
  | Offset_unscaled (rn, Nine_signed_unscaled imm) ->
    (* Prefer unsigned offset encoding when the offset is non-negative and
       properly aligned for the access size. This matches assembler behavior. *)
    let rn = Reg.gp_encoding rn in
    if imm >= 0 && imm mod scale = 0 && imm / scale <= 0xFFF
    then
      let imm12 = imm / scale in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt
    else
      let imm9 = imm land 0x1FF in
      encode_load_store_unscaled ~size ~vr ~opc ~imm9 ~rn ~rt
  | Literal (_rn, sym) -> (
    match Section_state.find_target_offset_in_bytes state sym.target with
    | None ->
      Misc.fatal_errorf "%s (literal) references undefined symbol '%a'"
        instr_name Symbol.print_target sym.target
    | Some target_offset ->
      let pc_relative_offset =
        target_offset - Section_state.offset_in_bytes state
      in
      if pc_relative_offset mod 4 <> 0
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%a' must be 4-byte aligned"
          instr_name pc_relative_offset Symbol.print_target sym.target;
      let imm19_unmasked = pc_relative_offset / 4 in
      if imm19_unmasked < -0x40000 || imm19_unmasked > 0x3ffff
      then
        Misc.fatal_errorf
          "%s (literal) offset %d to symbol '%a' out of range (max ±1MB)"
          instr_name pc_relative_offset Symbol.print_target sym.target;
      let imm19 = imm19_unmasked land 0x7FFFF in
      let opc_lit =
        match rd.reg_name with
        | Neon (Scalar S) -> 0b00
        | Neon (Scalar D) -> 0b01
        | Neon (Scalar Q) -> 0b10
        | Neon (Scalar B) | Neon (Scalar H) ->
          Misc.fatal_errorf "%s (literal) not supported for B/H registers"
            instr_name
      in
      encode_load_literal ~opc:opc_lit ~v:1 ~imm19 ~rt)
  | Offset_imm (rn, Twelve_unsigned_scaled imm12) ->
    if imm12 mod scale <> 0
    then
      Misc.fatal_errorf "%s offset %d must be aligned to %d-byte access size"
        instr_name imm12 scale;
    let imm12_scaled = imm12 / scale in
    if imm12_scaled < 0 || imm12_scaled > 0xfff
    then
      Misc.fatal_errorf "%s offset %d (scaled: %d) out of range" instr_name
        imm12 imm12_scaled;
    let rn = Reg.gp_encoding rn in
    encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12:imm12_scaled ~rn ~rt
  | Offset_sym (rn, sym) -> (
    match sym.reloc with
    | Needs_reloc reloc ->
      let max_imm12 = 0xfff in
      let reloc_kind : Relocation.Kind.t =
        let r = { Relocation.Kind.target = sym.target; addend = sym.offset } in
        match reloc with
        | GOT_PAGE_OFF | GOT_LOWER_TWELVE -> R_AARCH64_LD64_GOT_LO12_NC r
        | PAGE_OFF | LOWER_TWELVE ->
          (* Use LDST64 relocation for 64-bit load/store instructions. The
             immediate encoding differs from ADD: it's scaled by 8. *)
          if size = 0b11
          then R_AARCH64_LDST64_ABS_LO12_NC r
          else R_AARCH64_ADD_ABS_LO12_NC r
      in
      Section_state.add_relocation_at_current_offset state ~reloc_kind;
      let rn = Reg.gp_encoding rn in
      (* On RELA platforms (Linux), encode 0 in instruction - addend is in
         relocation. On REL platforms (macOS), encode addend in instruction. *)
      let offset =
        if Encode_directive.is_rela_platform () then 0 else sym.offset
      in
      let imm12_unmasked = offset / scale in
      if imm12_unmasked < 0 || imm12_unmasked > max_imm12
      then
        Misc.fatal_errorf
          "%s symbol offset %d (scaled by %d) out of range (max 0x%x)"
          instr_name offset scale max_imm12;
      let imm12 = imm12_unmasked land 0xFFF in
      encode_load_store_unsigned_offset ~size ~vr ~opc ~imm12 ~rn ~rt)
  | Pre (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_pre_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
  | Post (rn, Imm (Nine_signed_unscaled imm)) ->
    let imm9 = imm land 0x1FF in
    let rn = Reg.gp_encoding rn in
    encode_load_store_post_indexed ~size ~vr ~opc ~imm9 ~rn ~rt
