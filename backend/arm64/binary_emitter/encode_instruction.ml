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
module Asm_label = Asm_targets.Asm_label

let encode_instruction :
    type num operands.
    all_sections:All_section_states.t ->
    Section_state.t ->
    (num, operands) Instruction_name.t ->
    (num, operands) many ->
    int32 =
 fun ~all_sections state instr operands ->
  match operands, instr with
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      ABS_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* ABS: U=0, opcode=01011 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size ~opcode:0b01011 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADD_immediate ->
    Add_sub_helpers.encode_add_sub_imm_auto_shift ~op:0 ~s:0 ~imm12
      ~shift_opt:shift ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Sym sym), Optional shift), ADD_immediate -> (
    let sh = match shift with Some _ -> 1 | None -> 0 in
    match sym.reloc with
    | Needs_reloc reloc ->
      (* Keep original target in relocation for JIT use. The conversion to
         section+offset for verification is done in emit.ml *)
      let reloc_kind : Relocation.Kind.t =
        let r = { Relocation.Kind.target = sym.target; addend = sym.offset } in
        match reloc with
        | LOWER_TWELVE | PAGE_OFF -> R_AARCH64_ADD_ABS_LO12_NC r
        | GOT_LOWER_TWELVE | GOT_PAGE_OFF -> R_AARCH64_LD64_GOT_LO12_NC r
      in
      Section_state.add_relocation_at_current_offset state ~reloc_kind;
      (* On RELA platforms (Linux), encode 0 in instruction - addend is in
         relocation. On REL platforms (macOS), encode addend in instruction. *)
      let imm12 =
        if Encode_directive.is_rela_platform () then 0 else sym.offset
      in
      Add_sub_helpers.encode_add_sub_immediate ~sf:1 ~op:0 ~s:0 ~sh ~imm12 ~rn
        ~rd)
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      ADD_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Add_sub_helpers.encode_add_sub_shifted_reg ~op:0 ~s:0 ~shift ~imm6 ~rd ~rn
      ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ADDP_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* ADDP: U=0, opcode=10111 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10111 ~rn
      ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), ADDS ->
    Add_sub_helpers.encode_add_sub_imm_auto_shift ~op:0 ~s:1 ~imm12
      ~shift_opt:shift ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ADD_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10000 ~rn
      ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      ADDV ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* ADDV: U=0, opcode=11011 *)
    Simd_helpers.encode_simd_across_lanes ~q ~u:0 ~size ~opcode:0b11011 ~rn ~rd
  | Pair (Reg rd, Imm (Sym sym)), ADR ->
    (* ADR only accepts Same_section_and_unit symbols (local labels) *)
    let Same_section_and_unit = sym.reloc in
    let lbl =
      match sym.target with
      | Label lbl -> lbl
      | Symbol _ -> Misc.fatal_error "ADR: expected label, got symbol"
    in
    (* Compute PC-relative offset at assembly time *)
    let target_offset =
      match Section_state.find_label_offset_in_bytes state lbl with
      | Some off -> off
      | None ->
        Misc.fatal_errorf "ADR: label %a not found in current section"
          Asm_label.print lbl
    in
    let current_offset = Section_state.offset_in_bytes state in
    let pc_rel = target_offset - current_offset + sym.offset in
    let immlo, immhi = Adr_helpers.split_21bit_immediate pc_rel in
    Adr_helpers.encode_adr ~op:0 ~immlo ~immhi ~rd
  | Pair (Reg rd, Imm (Sym sym)), ADRP ->
    (* Keep original target in relocation for JIT use. The conversion to
       section+offset for verification is done in emit.ml *)
    let reloc_kind : Relocation.Kind.t =
      let r = { Relocation.Kind.target = sym.target; addend = sym.offset } in
      match sym.reloc with
      | Needs_reloc GOT_PAGE -> R_AARCH64_ADR_GOT_PAGE r
      | Needs_reloc PAGE -> R_AARCH64_ADR_PREL_PG_HI21 r
    in
    Section_state.add_relocation_at_current_offset state ~reloc_kind;
    (* On RELA platforms (Linux), encode 0 in instruction - addend is in
       relocation. On REL platforms (macOS), encode addend in instruction. *)
    let offset =
      if Encode_directive.is_rela_platform () then 0 else sym.offset
    in
    let immlo, immhi = Adr_helpers.split_21bit_immediate offset in
    Adr_helpers.encode_adr ~op:1 ~immlo ~immhi ~rd
  | Triple (Reg rd, Reg rn, Bitmask bitmask), AND_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    Logical_helpers.encode_logical_immediate ~sf:1 ~opc:0b00 ~n ~immr ~imms ~rn
      ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      AND_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Logical_helpers.encode_logical_shifted_reg ~opc:0b00 ~shift ~imm6 ~rd ~rn
      ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      AND_vector ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    (* AND: U=0, size=00, opcode=00011 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:0b00 ~rm ~opcode:0b00011
      ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), ASRV ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001010 ~rm
      ~rn ~rd
  | Singleton (Imm (Sym sym)), B ->
    let imm26 =
      Branch_helpers.compute_branch_imm26 state ~instr_name:"B"
        ~reloc_kind:(fun r -> R_AARCH64_JUMP26 r)
        sym
    in
    Branch_helpers.encode_branch_immediate ~op:0 ~imm26
  | Singleton (Imm (Sym sym)), B_cond cond ->
    let imm19 =
      Branch_helpers.compute_branch_imm19 state ~instr_name:"B.cond" sym
    in
    let cond = Condition_helpers.encode_condition cond in
    Branch_helpers.encode_conditional_branch ~imm19 ~cond
  | Singleton (Imm (Sym sym)), B_cond_float cond ->
    let imm19 =
      Branch_helpers.compute_branch_imm19 state ~instr_name:"B.cond" sym
    in
    let cond = Condition_helpers.encode_float_condition cond in
    Branch_helpers.encode_conditional_branch ~imm19 ~cond
  | Singleton (Imm (Sym sym)), BL ->
    let imm26 =
      Branch_helpers.compute_branch_imm26 state ~instr_name:"BL"
        ~reloc_kind:(fun r -> R_AARCH64_CALL26 r)
        sym
    in
    Branch_helpers.encode_branch_immediate ~op:1 ~imm26
  | Singleton (Reg rn), BLR ->
    Branch_helpers.encode_branch_register ~opc:0b0001 ~rn
  | Singleton (Reg rn), BR ->
    Branch_helpers.encode_branch_register ~opc:0b0000 ~rn
  | Pair (Reg rt, Imm (Sym sym)), CBNZ ->
    let imm19 =
      Branch_helpers.compute_branch_imm19 state ~instr_name:"CBNZ" sym
    in
    let sf = Reg.gp_sf rt in
    let rt = Reg.gp_encoding rt in
    Branch_helpers.encode_compare_branch ~sf ~op:1 ~imm19 ~rt
  | Pair (Reg rt, Imm (Sym sym)), CBZ ->
    let imm19 =
      Branch_helpers.compute_branch_imm19 state ~instr_name:"CBZ" sym
    in
    let sf = Reg.gp_sf rt in
    let rt = Reg.gp_encoding rt in
    Branch_helpers.encode_compare_branch ~sf ~op:0 ~imm19 ~rt
  | Pair (Reg rd, Reg rn), CLZ ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode:0b000100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      CM_register cond ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* Integer vector compares (register):

       - CMGT: U=0, opcode=00110

       - CMGE: U=0, opcode=00111

       - CMEQ: U=1, opcode=10001

       - CMHI: U=1, opcode=00110

       - CMHS: U=1, opcode=00111 *)
    let u, opcode =
      match cond with
      | Cond.GT -> 0, 0b00110
      | Cond.GE -> 0, 0b00111
      | Cond.EQ -> 1, 0b10001
      | Cond.HI -> 1, 0b00110
      | Cond.CS -> 1, 0b00111 (* HS/CS: unsigned greater or equal *)
      | Cond.NE | Cond.CC | Cond.MI | Cond.PL | Cond.VS | Cond.VC | Cond.LS
      | Cond.LT | Cond.LE ->
        Misc.fatal_error "Unsupported CM_register condition"
    in
    Simd_helpers.encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      CM_zero cond ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* Integer vector compares (zero):

       - CMGT (zero): U=0, opcode=01000

       - CMEQ (zero): U=0, opcode=01001

       - CMLT (zero): U=0, opcode=01010

       - CMGE (zero): U=1, opcode=01000

       - CMLE (zero): U=1, opcode=01001 *)
    let u, opcode =
      match cond with
      | Cond.GT -> 0, 0b01000
      | Cond.EQ -> 0, 0b01001
      | Cond.LT -> 0, 0b01010
      | Cond.GE -> 1, 0b01000
      | Cond.LE -> 1, 0b01001
      | Cond.NE | Cond.CS | Cond.CC | Cond.MI | Cond.PL | Cond.VS | Cond.VC
      | Cond.HI | Cond.LS ->
        Misc.fatal_error "Unsupported CM_zero condition"
    in
    Simd_helpers.encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | Pair (Reg rd, Reg rn), CNT ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode:0b000111 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      CNT_vector ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    (* CNT: U=0, size=00, opcode=00101 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size:0b00 ~opcode:0b00101 ~rn
      ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Cond cond), CSEL ->
    let sf = Reg.gp_sf rd in
    let cond = Condition_helpers.encode_condition cond in
    Csel_helpers.encode_conditional_select ~sf ~op:0 ~op2:0b00 ~rm ~cond ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Cond cond), CSINC ->
    let sf = Reg.gp_sf rd in
    let cond = Condition_helpers.encode_condition cond in
    Csel_helpers.encode_conditional_select ~sf ~op:0 ~op2:0b01 ~rm ~cond ~rn ~rd
  | Pair (Reg rd, Reg rn), CTZ ->
    (* FEAT_CSSC required *)
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode:0b000110 ~rn ~rd
  | _, DMB barrier ->
    Load_store_helpers.encode_memory_barrier ~op2:0b101 barrier
  | _, DSB barrier ->
    Load_store_helpers.encode_memory_barrier ~op2:0b100 barrier
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      DUP lane_idx ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    let imm5 =
      Simd_helpers.simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx)
    in
    (* DUP (element): op=0, imm4=0000 *)
    Simd_helpers.encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0000 ~rn ~rd
  | Triple (Reg rd, Reg rn, Bitmask bitmask), EOR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    Logical_helpers.encode_logical_immediate ~sf:1 ~opc:0b10 ~n ~immr ~imms ~rn
      ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      EOR_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Logical_helpers.encode_logical_shifted_reg ~opc:0b10 ~shift ~imm6 ~rd ~rn
      ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      EOR_vector ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    (* EOR: U=1, size=00, opcode=00011 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size:0b00 ~rm ~opcode:0b00011
      ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Imm (Six imm4)), EXT ->
    (* EXT is 128-bit only (V16B), so Q=1 *)
    Simd_helpers.encode_simd_extract ~q:1 ~rm:rm.index ~imm4 ~rn:rn.index
      ~rd:rd.index
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FABS ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode:0b000001 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADD ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADDP_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FADDP: U=1, size=0x, opcode=11010 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11010 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FADD_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FADD: U=0, size=0x, opcode=11010 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:sz ~rm ~opcode:0b11010 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FCM_register cond ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FP vector compares (register):

       - FCMEQ: U=0, size=0x, opcode=11100

       - FCMGE: U=1, size=0x, opcode=11100

       - FCMGT: U=1, size=1x, opcode=11100 *)
    let u, size_hi =
      match cond with
      | Float_cond.EQ -> 0, 0
      | Float_cond.GE -> 1, 0
      | Float_cond.GT -> 1, 1
      | Float_cond.LE | Float_cond.LT | Float_cond.NE | Float_cond.CC
      | Float_cond.CS | Float_cond.LS | Float_cond.HI ->
        Misc.fatal_error "Unsupported FCM_register condition"
    in
    let size = (size_hi lsl 1) lor sz in
    Simd_helpers.encode_simd_three_same ~q ~u ~size ~rm ~opcode:0b11100 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCM_zero cond ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FP vector compares (zero):

       - FCMGT (zero): U=0, size=1x, opcode=01100

       - FCMEQ (zero): U=0, size=1x, opcode=01101

       - FCMLT (zero): U=0, size=1x, opcode=01110

       - FCMGE (zero): U=1, size=1x, opcode=01100

       - FCMLE (zero): U=1, size=1x, opcode=01101 *)
    let u, opcode =
      match cond with
      | Float_cond.GT -> 0, 0b01100
      | Float_cond.EQ -> 0, 0b01101
      | Float_cond.LT -> 0, 0b01110
      | Float_cond.GE -> 1, 0b01100
      | Float_cond.LE -> 1, 0b01101
      | Float_cond.NE | Float_cond.CC | Float_cond.CS | Float_cond.LS
      | Float_cond.HI ->
        Misc.fatal_error "Unsupported FCM_zero condition"
    in
    let size = (1 lsl 1) lor sz in
    (* size=1x *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rn },
          Reg { index = rm; _ } ),
      FCMP ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_compare ~ftype ~rm ~opc2:0b00000 ~rn
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Cond cond ),
      FCSEL ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    let cond = Condition_helpers.encode_condition cond in
    Fp_helpers.encode_fp_cond_select ~ftype ~rm ~cond ~rn ~rd
  (* FCVT: convert between single and double precision *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = Neon (Scalar S); index = rn } ),
      FCVT ) ->
    (* FCVT Dd, Sn: ftype=00 (source=single), opcode=000101 (to double) *)
    Fp_helpers.encode_fp_1_source ~ftype:0 ~opcode:0b000101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = Neon (Scalar D); index = rn } ),
      FCVT ) ->
    (* FCVT Sd, Dn: ftype=01 (source=double), opcode=000100 (to single) *)
    Fp_helpers.encode_fp_1_source ~ftype:1 ~opcode:0b000100 ~rn ~rd
  (* FCVT same-precision conversions: use FMOV instead *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FCVT ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode:0b000000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTL_vector ) ->
    (* FCVTL converts from narrower to wider FP (e.g., V2S->V2D) U=0,
       opcode=10111, sz bit encodes SOURCE element size: sz=0: half -> single,
       sz=1: single -> double *)
    let size =
      match vec with
      | V2D -> 1 (* sz=1: 32-bit source (single) -> 64-bit dest (double) *)
      | V8B | V16B | V4H | V8H | V2S | V4S | V1D ->
        Misc.fatal_error "FCVTL_vector: unsupported destination type"
    in
    (* Q=0 for lower half (FCVTL), Q=1 for upper half (FCVTL2) *)
    Simd_helpers.encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10111 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTN_vector ) ->
    (* FCVTN converts from wider to narrower FP (e.g., V2D->V2S) U=0,
       opcode=10110, sz bit encodes SOURCE element size: sz=0: single -> half,
       sz=1: double -> single *)
    let size =
      match vec with
      | V2S -> 1 (* sz=1: 64-bit source (double) -> 32-bit dest (single) *)
      | V8B | V16B | V4H | V8H | V4S | V1D | V2D ->
        Misc.fatal_error "FCVTN_vector: unsupported destination type"
    in
    (* Q=0 for lower half (FCVTN), Q=1 for upper half (FCVTN2) *)
    Simd_helpers.encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10110 ~rn
      ~rd
  (* FCVTNS: FP to signed int, round to nearest with ties to even *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar _ as scalar); index = rn } ),
      FCVTNS ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b00 ~opcode:0b000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTNS_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FCVTNS (vector): U=0, size=0x, opcode=11010 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size:sz ~opcode:0b11010 ~rn
      ~rd
  (* FCVTZS: FP to signed int, round toward zero *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar _ as scalar); index = rn } ),
      FCVTZS ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b11 ~opcode:0b000 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FCVTZS_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FCVTZS (vector, integer): U=0, size=1x, opcode=11011 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size:(0b10 lor sz)
      ~opcode:0b11011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FDIV ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0001 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FDIV_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FDIV: U=1, size=0x, opcode=11111 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11111 ~rn
      ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FMADD ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_3_source ~ftype ~o1:0 ~rm ~o0:0 ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMAX ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0100 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMAX_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FMAX: U=0, size=0x, opcode=11110 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:sz ~rm ~opcode:0b11110 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMIN ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0101 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMIN_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FMIN: U=0, size=1x, opcode=11110 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:(0b10 lor sz) ~rm
      ~opcode:0b11110 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FMSUB ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_3_source ~ftype ~o1:0 ~rm ~o0:1 ~ra ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FMOV_fp ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode:0b000000 ~rn ~rd
  (* FMOV_gp_to_fp_32: GP-to-FP (W to S) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = GP W; index = rn } ),
      FMOV_gp_to_fp_32 ) ->
    (* sf=0, ftype=00, rmode=00, opcode=111 for GP->FP single *)
    Fp_helpers.encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b111 ~rn
      ~rd
  (* FMOV_gp_to_fp_32: GP-to-FP (WZR to S) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar S); index = rd },
          Reg { reg_name = GP WZR; index = rn } ),
      FMOV_gp_to_fp_32 ) ->
    Fp_helpers.encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b111 ~rn
      ~rd
  (* FMOV_gp_to_fp_64: GP-to-FP (X to D) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = GP X; index = rn } ),
      FMOV_gp_to_fp_64 ) ->
    (* sf=1, ftype=01, rmode=00, opcode=111 for GP->FP double *)
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b111 ~rn
      ~rd
  (* FMOV_gp_to_fp_64: GP-to-FP (XZR to D) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar D); index = rd },
          Reg { reg_name = GP XZR; index = rn } ),
      FMOV_gp_to_fp_64 ) ->
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b111 ~rn
      ~rd
  (* FMOV_fp_to_gp_32: FP-to-GP (S to W) *)
  | ( Pair
        ( Reg { reg_name = GP W; index = rd },
          Reg { reg_name = Neon (Scalar S); index = rn } ),
      FMOV_fp_to_gp_32 ) ->
    (* sf=0, ftype=00, rmode=00, opcode=110 for FP->GP single *)
    Fp_helpers.encode_fp_int_conv ~sf:0 ~ftype:0 ~rmode:0b00 ~opcode:0b110 ~rn
      ~rd
  (* FMOV_fp_to_gp_64: FP-to-GP (D to X) *)
  | ( Pair
        ( Reg { reg_name = GP X; index = rd },
          Reg { reg_name = Neon (Scalar D); index = rn } ),
      FMOV_fp_to_gp_64 ) ->
    (* sf=1, ftype=01, rmode=00, opcode=110 for FP->GP double *)
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype:1 ~rmode:0b00 ~opcode:0b110 ~rn
      ~rd
  (* FMOV scalar immediate - Float case *)
  | ( Pair
        (Reg { reg_name = Neon (Scalar _ as scalar); index = rd }, Imm (Float f)),
      FMOV_scalar_immediate ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    let bits = Int64.bits_of_float f in
    (* Extract imm8 from double-precision IEEE bits using VFPExpandImm inverse:
       VFPExpandImm expands imm8 to:
       sign:NOT(imm8[6]):Replicate(imm8[6],8):imm8[5:0]:Zeros(48) So to encode,
       we extract: imm8[7] = sign (bit 63) imm8[6] = NOT(bit 62) - inverted MSB
       of exponent imm8[5:4] = bits 53:52 - lower exponent bits after the
       replicated part imm8[3:0] = bits 51:48 - top 4 mantissa bits For single,
       we convert from double representation. *)
    let sign = Int64.(to_int (logand (shift_right_logical bits 63) 1L)) in
    let exp10 = Int64.(to_int (logand (shift_right_logical bits 62) 1L)) in
    let imm8_5_4 = Int64.(to_int (logand (shift_right_logical bits 52) 3L)) in
    let frac = Int64.(to_int (logand (shift_right_logical bits 48) 0xFL)) in
    let imm8 =
      (sign lsl 7) lor ((1 - exp10) lsl 6) lor (imm8_5_4 lsl 4) lor frac
    in
    Fp_helpers.encode_fp_immediate ~ftype ~imm8 ~rd
  (* FMOV scalar immediate - Nativeint case (raw bits) *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Imm (Nativeint n) ),
      FMOV_scalar_immediate ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    let imm8 = Nativeint.to_int n land 0xFF in
    Fp_helpers.encode_fp_immediate ~ftype ~imm8 ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMUL ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0000 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FMUL_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FMUL: U=1, size=0x, opcode=11011 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size:sz ~rm ~opcode:0b11011 ~rn
      ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FNEG ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode:0b000010 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FNEG_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FNEG: U=1, size=1x, opcode=01111 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz)
      ~opcode:0b01111 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FNMADD ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_3_source ~ftype ~o1:1 ~rm ~o0:0 ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FNMUL ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b1000 ~rn ~rd
  | ( Quad
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ },
          Reg { index = ra; _ } ),
      FNMSUB ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_3_source ~ftype ~o1:1 ~rm ~o0:1 ~ra ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRECPE_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FRECPE: U=0, size=1x, opcode=11101 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size:(0b10 lor sz)
      ~opcode:0b11101 ~rn ~rd
  (* FRINT: round FP to integer in FP format.

     Opcodes: N=001000, P=001001, M=001010, Z=001011, A=001100, X=001110 *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FRINT rmode ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    let opcode =
      match rmode with
      | Rounding_mode.N -> 0b001000
      | Rounding_mode.P -> 0b001001
      | Rounding_mode.M -> 0b001010
      | Rounding_mode.Z -> 0b001011
      | Rounding_mode.X -> 0b001110
    in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRINT_vector rm ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FRINT(mode) vector - encoding depends on rounding mode:

       - N: U=0, size=0x, opcode=11000

       - M: U=0, size=0x, opcode=11001

       - P: U=0, size=1x, opcode=11000

       - Z: U=0, size=1x, opcode=11001

       - X: U=1, size=0x, opcode=11001 *)
    let u, size_hi, opcode =
      match rm with
      | Rounding_mode.N -> 0, 0, 0b11000
      | Rounding_mode.M -> 0, 0, 0b11001
      | Rounding_mode.P -> 0, 1, 0b11000
      | Rounding_mode.Z -> 0, 1, 0b11001
      | Rounding_mode.X -> 1, 0, 0b11001
    in
    let size = (size_hi lsl 1) lor sz in
    Simd_helpers.encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FRSQRTE_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FRSQRTE: U=1, size=1x, opcode=11101 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz)
      ~opcode:0b11101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ } ),
      FSQRT ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_1_source ~ftype ~opcode:0b000011 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      FSQRT_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FSQRT: U=1, size=1x, opcode=11111 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size:(0b10 lor sz)
      ~opcode:0b11111 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FSUB ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    Fp_helpers.encode_fp_2_source ~ftype ~rm ~opcode:0b0011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      FSUB_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* FSUB: U=0, size=1x, opcode=11010 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:(0b10 lor sz) ~rm
      ~opcode:0b11010 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { reg_name = GP _; index = rn } ),
      INS lane_idx ) ->
    let imm5 =
      Simd_helpers.simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx)
    in
    (* INS (general): Q=1, op=0, imm4=0011 *)
    Simd_helpers.encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0011 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { reg_name = Neon (Scalar _); index = rn } ),
      INS lane_idx ) ->
    (* INS from scalar D register - same as from GP X register *)
    let imm5 =
      Simd_helpers.simd_copy_imm5 vec (Neon_reg_name.Lane_index.to_int lane_idx)
    in
    (* INS (general): Q=1, op=0, imm4=0011 *)
    Simd_helpers.encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0011 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      INS_V lanes ) ->
    let dest_idx =
      Neon_reg_name.Lane_index.to_int
        (Neon_reg_name.Lane_index.Src_and_dest.dest_index lanes)
    in
    let src_idx =
      Neon_reg_name.Lane_index.to_int
        (Neon_reg_name.Lane_index.Src_and_dest.src_index lanes)
    in
    let imm5 = Simd_helpers.simd_copy_imm5 vec dest_idx in
    (* INS (element): Q=1, op=1, imm4 encodes source index in size-dependent
       way *)
    let imm4 = Simd_helpers.simd_ins_element_imm4 vec src_idx in
    Simd_helpers.encode_simd_copy ~q:1 ~op:1 ~imm5 ~imm4 ~rn ~rd
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem (Reg rn)), LDAR ->
    Load_store_helpers.encode_load_acquire ~rd ~rn
  | Triple (Reg rt1, Reg rt2, Mem addressing), LDP ->
    Load_store_helpers.encode_load_store_pair_gp ~instr_name:"LDP" ~l:1 ~rt1
      ~rt2 addressing
  | Pair (Reg rd, Mem addressing), LDR ->
    Load_store_helpers.encode_load_store_gp ~all_sections state
      ~instr_name:"LDR" ~opc:0b01 ~rd addressing
  | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as rd), Mem addressing),
      LDR_simd_and_fp ) ->
    Load_store_helpers.encode_load_store_simd_fp ~all_sections state
      ~instr_name:"LDR" ~is_load:true ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRB ->
    (* LDRB: size=00, opc=01 *)
    Load_store_helpers.encode_load_store_byte ~all_sections state
      ~instr_name:"LDRB" ~opc:0b01 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRH ->
    (* LDRH: size=01, opc=01 *)
    Load_store_helpers.encode_load_store_halfword ~all_sections state
      ~instr_name:"LDRH" ~opc:0b01 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRSB ->
    (* LDRSB (sign-extend byte to 64-bit): size=00, opc=10 *)
    Load_store_helpers.encode_load_store_byte ~all_sections state
      ~instr_name:"LDRSB" ~opc:0b10 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRSH ->
    (* LDRSH (sign-extend halfword to 64-bit): size=01, opc=10 *)
    Load_store_helpers.encode_load_store_halfword ~all_sections state
      ~instr_name:"LDRSH" ~opc:0b10 ~rd addressing
  | Pair (Reg rd, Mem addressing), LDRSW ->
    (* LDRSW (sign-extend word to 64-bit): size=10, opc=10 *)
    Load_store_helpers.encode_load_store_gp_sized ~all_sections state
      ~instr_name:"LDRSW" ~size:0b10 ~opc:0b10 ~rd addressing
  | Triple (Reg rd, Reg rn, Reg rm), LSLV ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001000 ~rm
      ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), LSRV ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_2_source ~sf ~s:0 ~opcode:0b001001 ~rm
      ~rn ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MADD ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:0
      ~rm ~ra ~rn ~rd
  | ( Pair (Reg { reg_name = Neon (Vector vec); index = rd }, Imm (Twelve imm)),
      MOVI ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* MOVI encoding depends on element size: - For byte elements (size=00):
       op=0, cmode=1110, byte replication - For 64-bit elements (size=11): op=1,
       cmode=1110, 64-bit immediate For zeroing, imm=0, abc=000, defgh=00000 *)
    let imm8 = imm land 0xFF in
    let abc = (imm8 lsr 5) land 0b111 in
    let defgh = imm8 land 0b11111 in
    let op = if size = 0b11 then 1 else 0 in
    Simd_helpers.encode_simd_modified_imm ~q ~op ~abc ~cmode:0b1110 ~defgh ~rd
  | ( Pair (Reg { reg_name = Neon (Scalar _); index = rd }, Imm (Twelve imm)),
      MOVI ) ->
    (* MOVI to scalar (D register): Q=0, op=1, cmode=1110 for 64-bit
       immediate *)
    let imm8 = imm land 0xFF in
    let abc = (imm8 lsr 5) land 0b111 in
    let defgh = imm8 land 0b11111 in
    Simd_helpers.encode_simd_modified_imm ~q:0 ~op:1 ~abc ~cmode:0b1110 ~defgh
      ~rd
  | Triple (Reg rd, Imm imm, Shift shift), MOVK ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = (match shift.amount with Six n -> n) / 16 in
    Mov_helpers.encode_move_wide ~sf:1 ~opc:0b11 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVN ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = Mov_helpers.encode_six_bit_shift shift_opt in
    Mov_helpers.encode_move_wide ~sf:1 ~opc:0b00 ~hw ~imm16 ~rd
  | Triple (Reg rd, Imm imm, Optional shift_opt), MOVZ ->
    let imm16 = match imm with Sixteen_unsigned n -> n in
    let hw = Mov_helpers.encode_six_bit_shift shift_opt in
    Mov_helpers.encode_move_wide ~sf:1 ~opc:0b10 ~hw ~imm16 ~rd
  | Quad (Reg rd, Reg rn, Reg rm, Reg ra), MSUB ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_3_source ~sf ~op54:0b00 ~op31:0b000 ~o0:1
      ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      MUL_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* MUL: U=0, opcode=10011 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b10011 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      MVN_vector ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    (* NOT/MVN: U=1, size=00, opcode=00101 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size:0b00 ~opcode:0b00101 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      NEG_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* NEG: U=1, opcode=01011 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size ~opcode:0b01011 ~rn ~rd
  | _, NOP -> Nop_helpers.encode_nop ()
  | Triple (Reg rd, Reg rn, Bitmask bitmask), ORR_immediate ->
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    Logical_helpers.encode_logical_immediate ~sf:1 ~opc:0b01 ~n ~immr ~imms ~rn
      ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      ORR_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Logical_helpers.encode_logical_shifted_reg ~opc:0b01 ~shift ~imm6 ~rd ~rn
      ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ORR_vector ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    (* ORR: U=0, size=10, opcode=00011 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size:0b10 ~rm ~opcode:0b00011
      ~rn ~rd
  | Pair (Reg rd, Reg rn), RBIT ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode:0b000000 ~rn ~rd
  | _, RET ->
    (* RET defaults to X30 (LR). Encoding is same as BR/BLR but with opc=0010
       and Rn=11111 (X30) encoded in bits 9:5 *)
    let open Int32 in
    let result = zero in
    let result = logor result (shift_left (of_int 0b1101011) 25) in
    let result = logor result (shift_left (of_int 0b0010) 21) in
    (* opc = 0010 *)
    let result = logor result (shift_left (of_int 0b11111) 16) in
    (* op2 = 11111 *)
    let result = logor result (shift_left (of_int 0b000000) 10) in
    (* op3 = 000000 *)
    let result = logor result (shift_left (of_int 30) 5) in
    (* Rn = X30 = 11110 *)
    let result = logor result (of_int 0b00000) in
    (* op4 = 00000 *)
    result
  | Pair (Reg rd, Reg rn), REV ->
    let sf = Reg.gp_sf rd in
    let opcode = if sf = 1 then 0b000011 else 0b000010 in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode ~rn ~rd
  | Pair (Reg rd, Reg rn), REV16 ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_1_source ~sf ~s:0 ~opcode2:0b00000
      ~opcode:0b000001 ~rn ~rd
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), SBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    Bitfield_helpers.encode_bitfield ~sf ~opc:0b00 ~n ~immr ~imms ~rn ~rd
  (* SCVTF: signed integer to FP conversion *)
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _ as scalar); index = rd },
          Reg { reg_name = GP X; index = rn } ),
      SCVTF ) ->
    let ftype = Fp_helpers.scalar_ftype scalar in
    (* sf=1 (64-bit int), rmode=00, opcode=010 *)
    Fp_helpers.encode_fp_int_conv ~sf:1 ~ftype ~rmode:0b00 ~opcode:0b010 ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SCVTF_vector ) ->
    let q, sz = Simd_helpers.vector_q_fp_sz vec in
    (* SCVTF (vector, integer): U=0, size=0x, opcode=11101 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:0 ~size:sz ~opcode:0b11101 ~rn
      ~rd
  | Triple (Reg rd, Reg rn, Reg rm), SDIV ->
    let sf = Reg.gp_sf rd in
    Data_proc_helpers.encode_data_proc_2_source ~sf ~s:0 ~opcode:0b000011 ~rm
      ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      SHL ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    let immh, immb = Shift_immh_immb_helpers.shl_immh_immb vec shift in
    (* SHL: U=0, opcode=01010 *)
    Simd_helpers.encode_simd_shift_imm ~q ~u:0 ~immh ~immb ~opcode:0b01010 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMAX_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SMAX: U=0, opcode=01100 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01100 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMIN_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SMIN: U=0, opcode=01101 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01101 ~rn
      ~rd
  | ( Pair
        ( Reg { reg_name = GP gp; index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      SMOV lane_idx ) ->
    let q = match gp with W -> 0 | X -> 1 in
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = Simd_helpers.simd_copy_imm5 vec lane_int in
    (* SMOV: op=0, imm4=0101 *)
    Simd_helpers.encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0101 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      SMOV lane_idx ) ->
    (* SMOV to scalar D - 64-bit, so Q=1 *)
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = Simd_helpers.simd_copy_imm5 vec lane_int in
    Simd_helpers.encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0101 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), SMULH ->
    (* SMULH is 64-bit only. Ra is encoded as 11111 (ignored for multiply-high) *)
    (* Encoding: sf=1 op54=00 11011 op31=010 Rm o0=0 Ra=11111 Rn Rd *)
    let ra = Reg.xzr () in
    Data_proc_helpers.encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b010
      ~o0:0 ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMULL2_vector ) ->
    let size = Simd_helpers.vector_widening_size vec in
    (* SMULL2: U=0, opcode=1100, Q=1 for "2" variant *)
    Simd_helpers.encode_simd_three_different ~q:1 ~u:0 ~size ~rm ~opcode:0b1100
      ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SMULL_vector ) ->
    let size = Simd_helpers.vector_widening_size vec in
    (* SMULL: U=0, opcode=1100, Q=0 for basic variant *)
    Simd_helpers.encode_simd_three_different ~q:0 ~u:0 ~size ~rm ~opcode:0b1100
      ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SQADD_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SQADD: U=0, opcode=00001 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b00001 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SQXTN ) ->
    let _, size = Simd_helpers.vector_q_size vec in
    (* SQXTN: U=0, opcode=10100, Q=0 for SQXTN *)
    Simd_helpers.encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10100 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SQXTN2 ) ->
    let _, size = Simd_helpers.vector_q_size vec in
    (* SQXTN2: U=0, opcode=10100, Q=1 for SQXTN2 *)
    Simd_helpers.encode_simd_two_reg_misc ~q:1 ~u:0 ~size ~opcode:0b10100 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SQSUB_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SQSUB: U=0, opcode=00101 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b00101 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SSHL_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SSHL: U=0, opcode=01000 *)
    Simd_helpers.encode_simd_three_same ~q ~u:0 ~size ~rm ~opcode:0b01000 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      SSHR ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    let immh, immb = Shift_immh_immb_helpers.shr_immh_immb vec shift in
    (* SSHR: U=0, opcode=00000 *)
    Simd_helpers.encode_simd_shift_imm ~q ~u:0 ~immh ~immb ~opcode:0b00000 ~rn
      ~rd
  | Triple (Reg rt1, Reg rt2, Mem addressing), STP ->
    Load_store_helpers.encode_load_store_pair_gp ~instr_name:"STP" ~l:0 ~rt1
      ~rt2 addressing
  | Pair (Reg rd, Mem addressing), STR ->
    Load_store_helpers.encode_load_store_gp ~all_sections state
      ~instr_name:"STR" ~opc:0b00 ~rd addressing
  | ( Pair (Reg ({ reg_name = Neon (Scalar _); _ } as rd), Mem addressing),
      STR_simd_and_fp ) ->
    Load_store_helpers.encode_load_store_simd_fp ~all_sections state
      ~instr_name:"STR" ~is_load:false ~rd addressing
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem addressing), STRB ->
    (* STRB: size=00, opc=00 *)
    Load_store_helpers.encode_load_store_byte ~all_sections state
      ~instr_name:"STRB" ~opc:0b00 ~rd addressing
  | Pair (Reg ({ reg_name = GP _; _ } as rd), Mem addressing), STRH ->
    (* STRH: size=01, opc=00 *)
    Load_store_helpers.encode_load_store_halfword ~all_sections state
      ~instr_name:"STRH" ~opc:0b00 ~rd addressing
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUB_immediate ->
    Add_sub_helpers.encode_add_sub_imm_auto_shift ~op:1 ~s:0 ~imm12
      ~shift_opt:shift ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      SUB_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Add_sub_helpers.encode_add_sub_shifted_reg ~op:1 ~s:0 ~shift ~imm6 ~rd ~rn
      ~rm
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      SUB_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* SUB: U=1, opcode=10000 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b10000 ~rn
      ~rd
  | Quad (Reg rd, Reg rn, Imm (Twelve imm12), Optional shift), SUBS_immediate ->
    Add_sub_helpers.encode_add_sub_imm_auto_shift ~op:1 ~s:1 ~imm12
      ~shift_opt:shift ~rn ~rd
  | ( Quad
        ( Reg ({ reg_name = GP _; _ } as rd),
          Reg ({ reg_name = GP _; _ } as rn),
          Reg ({ reg_name = GP _; _ } as rm),
          Optional shift_opt ),
      SUBS_shifted_register ) ->
    let shift, imm6 =
      match shift_opt with
      | None -> 0, 0
      | Some (Shift { kind; amount }) ->
        ( Add_sub_helpers.decode_shift_kind_int kind,
          Add_sub_helpers.decode_shift_amount_six amount )
    in
    Add_sub_helpers.encode_add_sub_shifted_reg ~op:1 ~s:1 ~shift ~imm6 ~rd ~rn
      ~rm
  (* TODO: SXTL is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      SXTL ) ->
    let immh = Shift_immh_immb_helpers.sxtl_immh vec in
    (* SXTL is alias for SSHLL with shift=0: U=0, opcode=10100, Q=0 *)
    Simd_helpers.encode_simd_shift_imm ~q:0 ~u:0 ~immh ~immb:0 ~opcode:0b10100
      ~rn ~rd
  | ( Triple (Reg ({ reg_name = GP _; _ } as rt), Imm (Six bit), Imm (Sym sym)),
      TBNZ ) ->
    let imm14 =
      Branch_helpers.compute_branch_imm14 state ~instr_name:"TBNZ" sym
    in
    let b5 = (bit lsr 5) land 1 in
    let b40 = bit land 0b11111 in
    let rt_enc = Reg.gp_encoding rt in
    Branch_helpers.encode_test_branch ~b5 ~op:1 ~b40 ~imm14 ~rt:rt_enc
  | ( Triple (Reg ({ reg_name = GP _; _ } as rt), Imm (Six bit), Imm (Sym sym)),
      TBZ ) ->
    let imm14 =
      Branch_helpers.compute_branch_imm14 state ~instr_name:"TBZ" sym
    in
    let b5 = (bit lsr 5) land 1 in
    let b40 = bit land 0b11111 in
    let rt_enc = Reg.gp_encoding rt in
    Branch_helpers.encode_test_branch ~b5 ~op:0 ~b40 ~imm14 ~rt:rt_enc
  (* TODO: TST is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | Pair (Reg ({ reg_name = GP _; _ } as rn), Bitmask bitmask), TST ->
    (* TST is an alias for ANDS with XZR/WZR as destination (rd=31) *)
    let n, immr, imms = Operand.Bitmask.decode_n_immr_imms bitmask in
    let rn_enc = Reg.gp_encoding rn in
    let open Int32 in
    let result = zero in
    (* sf=1 for 64-bit, opc=11 for ANDS *)
    let result = logor result (shift_left (of_int 1) 31) in
    let result = logor result (shift_left (of_int 0b11) 29) in
    let result = logor result (shift_left (of_int 0b100100) 23) in
    let result = logor result (shift_left (of_int n) 22) in
    let result = logor result (shift_left (of_int immr) 16) in
    let result = logor result (shift_left (of_int imms) 10) in
    let result = logor result (shift_left (of_int rn_enc) 5) in
    let result = logor result (of_int 31) in
    result
  | Quad (Reg rd, Reg rn, Imm (Six immr), Imm (Six imms)), UBFM ->
    let sf = Reg.gp_sf rd in
    let n = sf in
    Bitfield_helpers.encode_bitfield ~sf ~opc:0b10 ~n ~immr ~imms ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UADDLP_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* UADDLP: U=1, opcode=00010 *)
    Simd_helpers.encode_simd_two_reg_misc ~q ~u:1 ~size ~opcode:0b00010 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMAX_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* UMAX: U=1, opcode=01100 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01100 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMIN_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* UMIN: U=1, opcode=01101 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01101 ~rn
      ~rd
  | ( Pair
        ( Reg { reg_name = GP gp; index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      UMOV lane_idx ) ->
    let q = match gp with W -> 0 | X -> 1 in
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = Simd_helpers.simd_copy_imm5 vec lane_int in
    (* UMOV: op=0, imm4=0111 *)
    Simd_helpers.encode_simd_copy ~q ~op:0 ~imm5 ~imm4:0b0111 ~rn ~rd
  | ( Pair
        ( Reg { reg_name = Neon (Scalar _); index = rd },
          Reg { reg_name = Neon (Vector vec); index = rn } ),
      UMOV lane_idx ) ->
    (* UMOV to scalar D - 64-bit, so Q=1 *)
    let lane_int = Neon_reg_name.Lane_index.to_int lane_idx in
    let imm5 = Simd_helpers.simd_copy_imm5 vec lane_int in
    Simd_helpers.encode_simd_copy ~q:1 ~op:0 ~imm5 ~imm4:0b0111 ~rn ~rd
  | Triple (Reg rd, Reg rn, Reg rm), UMULH ->
    (* UMULH is 64-bit only. Ra is encoded as 11111 (ignored for multiply-high) *)
    (* Encoding: sf=1 op54=00 11011 op31=110 Rm o0=0 Ra=11111 Rn Rd *)
    let ra = Reg.xzr () in
    Data_proc_helpers.encode_data_proc_3_source ~sf:1 ~op54:0b00 ~op31:0b110
      ~o0:0 ~rm ~ra ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMULL2_vector ) ->
    let size = Simd_helpers.vector_widening_size vec in
    (* UMULL2: U=1, opcode=1100, Q=1 for "2" variant *)
    Simd_helpers.encode_simd_three_different ~q:1 ~u:1 ~size ~rm ~opcode:0b1100
      ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UMULL_vector ) ->
    let size = Simd_helpers.vector_widening_size vec in
    (* UMULL: U=1, opcode=1100, Q=0 for basic variant *)
    Simd_helpers.encode_simd_three_different ~q:0 ~u:1 ~size ~rm ~opcode:0b1100
      ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UQADD_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* UQADD: U=1, opcode=00001 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b00001 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UQXTN ) ->
    let _, size = Simd_helpers.vector_q_size vec in
    (* UQXTN: U=1, opcode=10100, Q=0 for UQXTN *)
    Simd_helpers.encode_simd_two_reg_misc ~q:0 ~u:1 ~size ~opcode:0b10100 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UQXTN2 ) ->
    let _, size = Simd_helpers.vector_q_size vec in
    (* UQXTN2: U=1, opcode=10100, Q=1 for UQXTN2 *)
    Simd_helpers.encode_simd_two_reg_misc ~q:1 ~u:1 ~size ~opcode:0b10100 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      UQSUB_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* UQSUB: U=1, opcode=00101 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b00101 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      USHL_vector ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* USHL: U=1, opcode=01000 *)
    Simd_helpers.encode_simd_three_same ~q ~u:1 ~size ~rm ~opcode:0b01000 ~rn
      ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Imm (Six shift) ),
      USHR ) ->
    let q, _ = Simd_helpers.vector_q_size vec in
    let immh, immb = Shift_immh_immb_helpers.shr_immh_immb vec shift in
    (* USHR: U=1, opcode=00000 *)
    Simd_helpers.encode_simd_shift_imm ~q ~u:1 ~immh ~immb ~opcode:0b00000 ~rn
      ~rd
  (* TODO: UXTL is an alias; this should be removed from Instruction_name.t and
     handled via a rewrite rule. *)
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      UXTL ) ->
    let immh = Shift_immh_immb_helpers.sxtl_immh vec in
    (* UXTL is alias for USHLL with shift=0: U=1, opcode=10100, Q=0 *)
    Simd_helpers.encode_simd_shift_imm ~q:0 ~u:1 ~immh ~immb:0 ~opcode:0b10100
      ~rn ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      XTN ) ->
    let _q, size = Simd_helpers.vector_q_size vec in
    (* XTN: U=0, opcode=10010, Q=0 for XTN *)
    Simd_helpers.encode_simd_two_reg_misc ~q:0 ~u:0 ~size ~opcode:0b10010 ~rn
      ~rd
  | ( Pair
        (Reg { reg_name = Neon (Vector vec); index = rd }, Reg { index = rn; _ }),
      XTN2 ) ->
    let _, size = Simd_helpers.vector_q_size vec in
    (* XTN2: U=0, opcode=10010, Q=1 for XTN2 *)
    Simd_helpers.encode_simd_two_reg_misc ~q:1 ~u:0 ~size ~opcode:0b10010 ~rn
      ~rd
  | _, YIELD -> Yield_helpers.encode_yield ()
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ZIP1 ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* ZIP1: opcode=011 *)
    Simd_helpers.encode_simd_permute ~q ~size ~rm ~opcode:0b011 ~rn ~rd
  | ( Triple
        ( Reg { reg_name = Neon (Vector vec); index = rd },
          Reg { index = rn; _ },
          Reg { index = rm; _ } ),
      ZIP2 ) ->
    let q, size = Simd_helpers.vector_q_size vec in
    (* ZIP2: opcode=111 *)
    Simd_helpers.encode_simd_permute ~q ~size ~rm ~opcode:0b111 ~rn ~rd
