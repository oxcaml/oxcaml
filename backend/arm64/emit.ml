(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                        The OxCaml developers                           *)
(*                                                                        *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* Emission of ARM assembly code, 64-bit mode *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Oxcaml_flags] and shared variables. For details, see [asmgen.mli]. *)

open Arch
open Proc
open Reg
open! Operation
open Linear
open Emitaux
module Ast = Arm64_ast.Ast
module I = Ast.Instruction_name
module O = Ast.DSL
module A = Ast.DSL.Acc
module R = Ast.Reg
module Cond = Ast.Cond
module Float_cond = Ast.Float_cond
module Simd_int_cmp = Ast.Simd_int_cmp
module Branch_cond = Ast.Branch_cond
module D = Asm_targets.Asm_directives
module S = Asm_targets.Asm_symbol

(* CR mshinwell: maybe the following two functions should move to a new
   Cmm.Symbol module, which would include Cmm.symbol as the type "t"? *)

(* Convert Cmm.is_global to Asm_symbol.visibility *)
let visibility_of_cmm_global : Cmm.is_global -> S.visibility = function
  | Cmm.Global -> S.Global
  | Cmm.Local -> S.Local

(* Create symbol from Cmm.symbol, preserving visibility *)
let symbol_of_cmm_symbol (s : Cmm.symbol) : S.t =
  S.create ~visibility:(visibility_of_cmm_global s.sym_global) s.sym_name

module L = Asm_targets.Asm_label
open! Int_replace_polymorphic_compare
module H = Dsl_helpers

(* Scratch FP/SIMD register 7 in various widths. - S7: used for float32
   load/store conversions - D7, V8B_7, B7: used for popcnt emulation when CSSC
   is unavailable *)
let reg_s7 = O.reg_op (R.reg_s 7)

let reg_d7 = O.reg_op (R.reg_d 7)

let reg_v8b_7 = O.reg_op (R.reg_v8b 7)

let reg_b7 = O.reg_op (R.reg_b 7)

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Names for special regs *)

let reg_domain_state_ptr = phys_reg Int 25 (* x28 *)

let reg_trap_ptr = phys_reg Int 23 (* x26 *)

let reg_x_trap_ptr = H.reg_x reg_trap_ptr

let reg_alloc_ptr = phys_reg Int 24 (* x27 *)

let reg_tmp1 = phys_reg Int 26 (* x16 *)

(* AST register for reg_tmp1, used as memory base *)
let reg_tmp1_base = R.reg_x 16

let reg_x_tmp1 = H.reg_x reg_tmp1

let reg_x_alloc_ptr = H.reg_x reg_alloc_ptr

let reg_x8 = phys_reg Int 8 (* x8 *)

let reg_x_x8 = H.reg_x reg_x8

let reg_stack_arg_begin = H.reg_x (phys_reg Int 17)

let reg_stack_arg_end = H.reg_x (phys_reg Int 18)

(** Turn a Linear label into an assembly label. The section is checked against
    the section tracked by [D] when emitting label definitions. *)
let label_to_asm_label (l : label) ~(section : Asm_targets.Asm_section.t) : L.t
    =
  L.create_int section (Label.to_int l)

(* Mark a symbol as global, with protected visibility on ELF if enabled.
   Protected visibility prevents symbol interposition, which allows direct
   addressing to be used safely. This matches amd64 behavior. *)
let global_maybe_protected sym =
  D.global sym;
  if (not macosx) && !Oxcaml_flags.symbol_visibility_protected
  then D.protected sym

(* Layout of the stack frame *)

let stack_offset = ref 0

let num_stack_slots = Stack_class.Tbl.make 0

let prologue_required = ref false

let contains_calls = ref false

let initial_stack_offset () =
  Proc.initial_stack_offset ~contains_calls:!contains_calls ~num_stack_slots

let frame_size () =
  Proc.frame_size ~stack_offset:!stack_offset ~contains_calls:!contains_calls
    ~num_stack_slots

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

(* Local wrapper for stack that provides the emit.ml-local values. Returns a
   stack_result indicating whether the offset fits in immediate encoding or
   requires a multi-instruction sequence. *)
let stack r =
  H.stack ~stack_offset:!stack_offset ~contains_calls:!contains_calls
    ~num_stack_slots r

(* Convenience functions for symbols and labels *)
let symbol ?offset reloc s = O.symbol (Ast.Symbol.create_symbol reloc ?offset s)

let label ?offset reloc lbl =
  O.symbol (Ast.Symbol.create_label reloc ?offset lbl)

let runtime_function sym = symbol (Needs_reloc CALL26) sym

let local_label lbl = label Same_section_and_unit lbl

module Validated_mem_offset = Ast.DSL.Validated_mem_offset

(* Create a symbol or label reference depending on whether the symbol is local.
   Local symbols are defined as labels (not linker symbols) to avoid ELF
   visibility issues, so references to them must also use labels. *)
let symbol_or_label_for_data ?offset reloc s =
  if S.is_local s
  then label ?offset reloc (L.create_label_for_local_symbol Data s)
  else symbol ?offset reloc s

(* labelled instruction helpers *)
let labelled_ins1 lbl instr op =
  D.define_label lbl;
  A.ins1 instr op

let labelled_ins4 lbl instr ops =
  D.define_label lbl;
  A.ins4 instr ops

(* Simd condition/rounding mode conversion *)
module Simd_cond = struct
  let create (c : Simd.Cond.t) : Simd_int_cmp.t =
    match c with
    | EQ -> Simd_int_cmp.EQ
    | GE -> Simd_int_cmp.GE
    | GT -> Simd_int_cmp.GT
    | LE -> Simd_int_cmp.LE
    | LT -> Simd_int_cmp.LT
end

module Simd_rounding_mode = struct
  let create (rm : Simd.Rounding_mode.t) : Ast.Rounding_mode.t =
    match rm with
    | Neg_inf -> M
    | Pos_inf -> P
    | Zero -> Z
    | Current -> X
    | Nearest -> N
end

(* SIMD instruction handling *)

(* CR mshinwell for TheNumbat: consider making simd_selection generate the AST
   types directly, as was done for amd64. *)
let simd_instr (op : Simd.operation) (i : Linear.instruction) =
  let module Lane_index = Ast.Neon_reg_name.Lane_index in
  (* Check register constraints for instructions that require res = arg0 *)
  (match[@ocaml.warning "-4"] op with
  | Copyq_laneq_s64 _ | Setq_lane_s8 _ | Setq_lane_s16 _ | Setq_lane_s32 _
  | Setq_lane_s64 _ | Movn_high_s64 | Qmovn_high_s64 | Qmovn_high_s32
  | Qmovn_high_u32 | Movn_high_s32 | Qmovn_high_s16 | Qmovn_high_u16
  | Movn_high_s16 ->
    if not (Reg.same_loc i.res.(0) i.arg.(0))
    then
      Misc.fatal_errorf
        "simd_instr: Instruction %s requires res and arg0 to be the same \
         register"
        (Simd.print_name op)
  | _ -> ());
  let module RM = Simd_rounding_mode in
  let open A.Tupled in
  (* CR mshinwell: let open A.Ins in *)
  let open H in
  let arg = i.arg in
  let res = i.res in
  match op with
  (* Vector floating-point arithmetic - V4S *)
  | Addq_f32 -> ins3 FADD_vector (v4s_v4s_v4s i)
  | Subq_f32 -> ins3 FSUB_vector (v4s_v4s_v4s i)
  | Mulq_f32 -> ins3 FMUL_vector (v4s_v4s_v4s i)
  | Divq_f32 -> ins3 FDIV_vector (v4s_v4s_v4s i)
  (* Vector floating-point arithmetic - V2D *)
  | Addq_f64 -> ins3 FADD_vector (v2d_v2d_v2d i)
  | Subq_f64 -> ins3 FSUB_vector (v2d_v2d_v2d i)
  | Mulq_f64 -> ins3 FMUL_vector (v2d_v2d_v2d i)
  | Divq_f64 -> ins3 FDIV_vector (v2d_v2d_v2d i)
  (* Vector integer arithmetic - ADD *)
  | Addq_s64 -> ins3 ADD_vector (v2d_v2d_v2d i)
  | Addq_s32 -> ins3 ADD_vector (v4s_v4s_v4s i)
  | Addq_s16 -> ins3 ADD_vector (v8h_v8h_v8h i)
  | Addq_s8 -> ins3 ADD_vector (v16b_v16b_v16b i)
  (* Vector integer arithmetic - SUB *)
  | Subq_s64 -> ins3 SUB_vector (v2d_v2d_v2d i)
  | Subq_s32 -> ins3 SUB_vector (v4s_v4s_v4s i)
  | Subq_s16 -> ins3 SUB_vector (v8h_v8h_v8h i)
  | Subq_s8 -> ins3 SUB_vector (v16b_v16b_v16b i)
  (* Vector integer arithmetic - MUL *)
  | Mulq_s32 -> ins3 MUL_vector (v4s_v4s_v4s i)
  | Mulq_s16 -> ins3 MUL_vector (v8h_v8h_v8h i)
  (* Vector integer arithmetic - NEG *)
  | Negq_s64 -> ins2 NEG_vector (v2d_v2d i)
  | Negq_s32 -> ins2 NEG_vector (v4s_v4s i)
  | Negq_s16 -> ins2 NEG_vector (v8h_v8h i)
  | Negq_s8 -> ins2 NEG_vector (v16b_v16b i)
  (* Vector min/max - floating-point *)
  | Minq_f32 -> ins3 FMIN_vector (v4s_v4s_v4s i)
  | Minq_f64 -> ins3 FMIN_vector (v2d_v2d_v2d i)
  | Maxq_f32 -> ins3 FMAX_vector (v4s_v4s_v4s i)
  | Maxq_f64 -> ins3 FMAX_vector (v2d_v2d_v2d i)
  (* Vector min/max - signed integer *)
  | Minq_s32 -> ins3 SMIN_vector (v4s_v4s_v4s i)
  | Minq_s16 -> ins3 SMIN_vector (v8h_v8h_v8h i)
  | Minq_s8 -> ins3 SMIN_vector (v16b_v16b_v16b i)
  | Maxq_s32 -> ins3 SMAX_vector (v4s_v4s_v4s i)
  | Maxq_s16 -> ins3 SMAX_vector (v8h_v8h_v8h i)
  | Maxq_s8 -> ins3 SMAX_vector (v16b_v16b_v16b i)
  (* Vector min/max - unsigned integer *)
  | Minq_u32 -> ins3 UMIN_vector (v4s_v4s_v4s i)
  | Minq_u16 -> ins3 UMIN_vector (v8h_v8h_v8h i)
  | Minq_u8 -> ins3 UMIN_vector (v16b_v16b_v16b i)
  | Maxq_u32 -> ins3 UMAX_vector (v4s_v4s_v4s i)
  | Maxq_u16 -> ins3 UMAX_vector (v8h_v8h_v8h i)
  | Maxq_u8 -> ins3 UMAX_vector (v16b_v16b_v16b i)
  (* Vector logical operations - all use V16B *)
  | Orrq_s32 | Orrq_s64 | Orrq_s16 | Orrq_s8 ->
    ins3 ORR_vector (v16b_v16b_v16b i)
  | Andq_s32 | Andq_s64 | Andq_s16 | Andq_s8 ->
    ins3 AND_vector (v16b_v16b_v16b i)
  | Eorq_s32 | Eorq_s64 | Eorq_s16 | Eorq_s8 ->
    ins3 EOR_vector (v16b_v16b_v16b i)
  | Mvnq_s32 | Mvnq_s64 | Mvnq_s16 | Mvnq_s8 -> ins2 MVN_vector (v16b_v16b i)
  (* Vector absolute value *)
  | Absq_s64 -> ins2 ABS_vector (v2d_v2d i)
  | Absq_s32 -> ins2 ABS_vector (v4s_v4s i)
  | Absq_s16 -> ins2 ABS_vector (v8h_v8h i)
  | Absq_s8 -> ins2 ABS_vector (v16b_v16b i)
  (* Vector sqrt and reciprocal estimates *)
  | Sqrtq_f32 -> ins2 FSQRT_vector (v4s_v4s i)
  | Sqrtq_f64 -> ins2 FSQRT_vector (v2d_v2d i)
  | Rsqrteq_f32 -> ins2 FRSQRTE_vector (v4s_v4s i)
  | Rsqrteq_f64 -> ins2 FRSQRTE_vector (v2d_v2d i)
  | Recpeq_f32 -> ins2 FRECPE_vector (v4s_v4s i)
  (* Vector conversions *)
  (* CR mshinwell for TheNumbat: convert manual indexing (e.g. res.(0),
     arg.(0)) to helper functions for consistency with other operations. *)
  | Cvtq_s32_f32 -> ins2 FCVTZS_vector (v4s_v4s i)
  | Cvtq_s64_f64 -> ins2 FCVTZS_vector (v2d_v2d i)
  | Cvtnq_s32_f32 -> ins2 FCVTNS_vector (v4s_v4s i)
  | Cvtnq_s64_f64 -> ins2 FCVTNS_vector (v2d_v2d i)
  | Cvtq_f32_s32 -> ins2 SCVTF_vector (v4s_v4s i)
  | Cvtq_f64_s64 -> ins2 SCVTF_vector (v2d_v2d i)
  | Cvt_f64_f32 -> ins2 FCVTL_vector (reg_v2d res.(0), reg_v2s arg.(0))
  | Cvt_f32_f64 -> ins2 FCVTN_vector (reg_v2s res.(0), reg_v2d arg.(0))
  (* Vector extend/narrow operations - signed extend *)
  | Movl_s32 -> ins2 (SXTL V2D_V2S) (reg_v2d res.(0), reg_v2s arg.(0))
  | Movl_s16 -> ins2 (SXTL V4S_V4H) (reg_v4s res.(0), reg_v4h arg.(0))
  | Movl_s8 -> ins2 (SXTL V8H_V8B) (reg_v8h res.(0), reg_v8b arg.(0))
  (* Vector extend/narrow operations - unsigned extend *)
  | Movl_u32 -> ins2 (UXTL V2D_V2S) (reg_v2d res.(0), reg_v2s arg.(0))
  | Movl_u16 -> ins2 (UXTL V4S_V4H) (reg_v4s res.(0), reg_v4h arg.(0))
  | Movl_u8 -> ins2 (UXTL V8H_V8B) (reg_v8h res.(0), reg_v8b arg.(0))
  (* Vector narrow operations *)
  | Movn_s64 -> ins2 (XTN V2S_V2D) (reg_v2s res.(0), reg_v2d arg.(0))
  | Movn_s32 -> ins2 (XTN V4H_V4S) (reg_v4h res.(0), reg_v4s arg.(0))
  | Movn_s16 -> ins2 (XTN V8B_V8H) (reg_v8b res.(0), reg_v8h arg.(0))
  (* Vector narrow high operations - uses arg.(1) for source *)
  | Movn_high_s64 -> ins2 (XTN2 V4S_V2D) (reg_v4s res.(0), reg_v2d arg.(1))
  | Movn_high_s32 -> ins2 (XTN2 V8H_V4S) (reg_v8h res.(0), reg_v4s arg.(1))
  | Movn_high_s16 -> ins2 (XTN2 V16B_V8H) (reg_v16b res.(0), reg_v8h arg.(1))
  (* Vector saturating narrow operations *)
  | Qmovn_s64 -> ins2 (SQXTN V2S_V2D) (reg_v2s res.(0), reg_v2d arg.(0))
  | Qmovn_s32 -> ins2 (SQXTN V4H_V4S) (reg_v4h res.(0), reg_v4s arg.(0))
  | Qmovn_s16 -> ins2 (SQXTN V8B_V8H) (reg_v8b res.(0), reg_v8h arg.(0))
  | Qmovn_high_s64 -> ins2 (SQXTN2 V4S_V2D) (reg_v4s res.(0), reg_v2d arg.(1))
  | Qmovn_high_s32 -> ins2 (SQXTN2 V8H_V4S) (reg_v8h res.(0), reg_v4s arg.(1))
  | Qmovn_high_s16 -> ins2 (SQXTN2 V16B_V8H) (reg_v16b res.(0), reg_v8h arg.(1))
  | Qmovn_u32 -> ins2 (UQXTN V4H_V4S) (reg_v4h res.(0), reg_v4s arg.(0))
  | Qmovn_u16 -> ins2 (UQXTN V8B_V8H) (reg_v8b res.(0), reg_v8h arg.(0))
  | Qmovn_high_u32 -> ins2 (UQXTN2 V8H_V4S) (reg_v8h res.(0), reg_v4s arg.(1))
  | Qmovn_high_u16 -> ins2 (UQXTN2 V16B_V8H) (reg_v16b res.(0), reg_v8h arg.(1))
  (* Vector pairwise operations *)
  | Paddq_f32 -> ins3 FADDP_vector (v4s_v4s_v4s i)
  | Paddq_f64 -> ins3 FADDP_vector (v2d_v2d_v2d i)
  | Paddq_s64 -> ins3 ADDP_vector (v2d_v2d_v2d i)
  | Paddq_s32 -> ins3 ADDP_vector (v4s_v4s_v4s i)
  | Paddq_s16 -> ins3 ADDP_vector (v8h_v8h_v8h i)
  | Paddq_s8 -> ins3 ADDP_vector (v16b_v16b_v16b i)
  (* Vector zip operations *)
  | Zip1_f32 ->
    ins3 ZIP1
      ( reg_v2s_of_float res.(0),
        reg_v2s_of_float arg.(0),
        reg_v2s_of_float arg.(1) )
  | Zip1q_s8 -> ins3 ZIP1 (v16b_v16b_v16b i)
  | Zip1q_s16 -> ins3 ZIP1 (v8h_v8h_v8h i)
  | Zip1q_f32 -> ins3 ZIP1 (v4s_v4s_v4s i)
  | Zip1q_f64 -> ins3 ZIP1 (v2d_v2d_v2d i)
  | Zip2q_s8 -> ins3 ZIP2 (v16b_v16b_v16b i)
  | Zip2q_s16 -> ins3 ZIP2 (v8h_v8h_v8h i)
  | Zip2q_f32 -> ins3 ZIP2 (v4s_v4s_v4s i)
  | Zip2q_f64 -> ins3 ZIP2 (v2d_v2d_v2d i)
  (* Vector shift operations - unsigned *)
  | Shlq_u64 -> ins3 USHL_vector (v2d_v2d_v2d i)
  | Shlq_u32 -> ins3 USHL_vector (v4s_v4s_v4s i)
  | Shlq_u16 -> ins3 USHL_vector (v8h_v8h_v8h i)
  | Shlq_u8 -> ins3 USHL_vector (v16b_v16b_v16b i)
  (* Vector shift operations - signed *)
  | Shlq_s64 -> ins3 SSHL_vector (v2d_v2d_v2d i)
  | Shlq_s32 -> ins3 SSHL_vector (v4s_v4s_v4s i)
  | Shlq_s16 -> ins3 SSHL_vector (v8h_v8h_v8h i)
  | Shlq_s8 -> ins3 SSHL_vector (v16b_v16b_v16b i)
  (* Vector multiply long *)
  | Mullq_s16 ->
    ins3 (SMULL_vector V4S_V4H)
      (reg_v4s res.(0), reg_v4h arg.(0), reg_v4h arg.(1))
  | Mullq_u16 ->
    ins3 (UMULL_vector V4S_V4H)
      (reg_v4s res.(0), reg_v4h arg.(0), reg_v4h arg.(1))
  | Mullq_high_s16 ->
    ins3 (SMULL2_vector V4S_V8H)
      (reg_v4s res.(0), reg_v8h arg.(0), reg_v8h arg.(1))
  | Mullq_high_u16 ->
    ins3 (UMULL2_vector V4S_V8H)
      (reg_v4s res.(0), reg_v8h arg.(0), reg_v8h arg.(1))
  (* Vector saturating arithmetic *)
  | Qaddq_s16 -> ins3 SQADD_vector (v8h_v8h_v8h i)
  | Qaddq_s8 -> ins3 SQADD_vector (v16b_v16b_v16b i)
  | Qaddq_u16 -> ins3 UQADD_vector (v8h_v8h_v8h i)
  | Qaddq_u8 -> ins3 UQADD_vector (v16b_v16b_v16b i)
  | Qsubq_s16 -> ins3 SQSUB_vector (v8h_v8h_v8h i)
  | Qsubq_s8 -> ins3 SQSUB_vector (v16b_v16b_v16b i)
  | Qsubq_u16 -> ins3 UQSUB_vector (v8h_v8h_v8h i)
  | Qsubq_u8 -> ins3 UQSUB_vector (v16b_v16b_v16b i)
  (* Vector shift by immediate *)
  | Shlq_n_u64 n ->
    let rd, rn = v2d_v2d i in
    ins3 SHL (rd, rn, O.shift_by_element_width_d n)
  | Shlq_n_u32 n ->
    let rd, rn = v4s_v4s i in
    ins3 SHL (rd, rn, O.shift_by_element_width_s n)
  | Shlq_n_u16 n ->
    let rd, rn = v8h_v8h i in
    ins3 SHL (rd, rn, O.shift_by_element_width_h n)
  | Shlq_n_u8 n ->
    let rd, rn = v16b_v16b i in
    ins3 SHL (rd, rn, O.shift_by_element_width_b n)
  | Shrq_n_u64 n ->
    let rd, rn = v2d_v2d i in
    ins3 USHR (rd, rn, O.shift_by_element_width_d n)
  | Shrq_n_u32 n ->
    let rd, rn = v4s_v4s i in
    ins3 USHR (rd, rn, O.shift_by_element_width_s n)
  | Shrq_n_u16 n ->
    let rd, rn = v8h_v8h i in
    ins3 USHR (rd, rn, O.shift_by_element_width_h n)
  | Shrq_n_u8 n ->
    let rd, rn = v16b_v16b i in
    ins3 USHR (rd, rn, O.shift_by_element_width_b n)
  | Shrq_n_s64 n ->
    let rd, rn = v2d_v2d i in
    ins3 SSHR (rd, rn, O.shift_by_element_width_d n)
  | Shrq_n_s32 n ->
    let rd, rn = v4s_v4s i in
    ins3 SSHR (rd, rn, O.shift_by_element_width_s n)
  | Shrq_n_s16 n ->
    let rd, rn = v8h_v8h i in
    ins3 SSHR (rd, rn, O.shift_by_element_width_h n)
  | Shrq_n_s8 n ->
    let rd, rn = v16b_v16b i in
    ins3 SSHR (rd, rn, O.shift_by_element_width_b n)
  (* Sign-extend lane to X register *)
  | Getq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (SMOV (S_to_X, lane_idx)) (reg_x res.(0), reg_v4s arg.(0))
  | Getq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (SMOV (H_to_X, lane_idx)) (reg_x res.(0), reg_v8h arg.(0))
  | Getq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (SMOV (B_to_X, lane_idx)) (reg_x res.(0), reg_v16b arg.(0))
  (* Extract 64-bit lane *)
  (* CR mshinwell: investigate whether MOV could be used here instead of UMOV.
     This was present before the typed DSL refactoring. *)
  | Getq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (UMOV (D, lane_idx)) (reg_x res.(0), reg_v2d arg.(0))
  (* Extract bytes from two vectors *)
  | Extq_u8 n ->
    ins4 EXT (reg_v16b res.(0), reg_v16b arg.(0), reg_v16b arg.(1), O.imm_six n)
  (* Compare against zero - floating-point *)
  | Cmpz_f32 c -> ins2 (FCM_zero c) (v4s_v4s i)
  | Cmpz_f64 c -> ins2 (FCM_zero c) (v2d_v2d i)
  (* Compare against zero - integer *)
  | Cmpz_s32 c -> ins2 (CM_zero (Simd_cond.create c)) (v4s_v4s i)
  | Cmpz_s64 c -> ins2 (CM_zero (Simd_cond.create c)) (v2d_v2d i)
  | Cmpz_s16 c -> ins2 (CM_zero (Simd_cond.create c)) (v8h_v8h i)
  | Cmpz_s8 c -> ins2 (CM_zero (Simd_cond.create c)) (v16b_v16b i)
  (* Float vector compare *)
  | Cmp_f32 ((EQ | GE | GT) as c) -> ins3 (FCM_register c) (v4s_v4s_v4s i)
  | Cmp_f64 ((EQ | GE | GT) as c) -> ins3 (FCM_register c) (v2d_v2d_v2d i)
  | Cmp_f32 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f32 LT/LE should be transformed in Simd_selection"
  | Cmp_f64 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f64 LT/LE should be transformed in Simd_selection"
  (* Scalar float min/max *)
  | Fmin_f32 -> ins3 FMIN (reg_s res.(0), reg_s arg.(0), reg_s arg.(1))
  | Fmax_f32 -> ins3 FMAX (reg_s res.(0), reg_s arg.(0), reg_s arg.(1))
  | Fmin_f64 -> ins3 FMIN (reg_d res.(0), reg_d arg.(0), reg_d arg.(1))
  | Fmax_f64 -> ins3 FMAX (reg_d res.(0), reg_d arg.(0), reg_d arg.(1))
  (* Scalar rounding *)
  | Round_f32 rm -> ins2 (FRINT (RM.create rm)) (reg_s res.(0), reg_s arg.(0))
  | Round_f64 rm -> ins2 (FRINT (RM.create rm)) (reg_d res.(0), reg_d arg.(0))
  (* Vector rounding *)
  | Roundq_f32 rm -> ins2 (FRINT_vector (RM.create rm)) (v4s_v4s i)
  | Roundq_f64 rm -> ins2 (FRINT_vector (RM.create rm)) (v2d_v2d i)
  (* Float to signed integer with rounding *)
  | Round_f32_s64 -> ins2 FCVTNS (reg_x res.(0), reg_s arg.(0))
  | Round_f64_s64 -> ins2 FCVTNS (reg_x res.(0), reg_d arg.(0))
  (* Integer vector compares *)
  | Cmp_s32 ((EQ | GE | GT) as c) ->
    ins3 (CM_register (Simd_cond.create c)) (v4s_v4s_v4s i)
  | Cmp_s64 ((EQ | GE | GT) as c) ->
    ins3 (CM_register (Simd_cond.create c)) (v2d_v2d_v2d i)
  | Cmp_s16 ((EQ | GE | GT) as c) ->
    ins3 (CM_register (Simd_cond.create c)) (v8h_v8h_v8h i)
  | Cmp_s8 ((EQ | GE | GT) as c) ->
    ins3 (CM_register (Simd_cond.create c)) (v16b_v16b_v16b i)
  | Cmp_s32 (LT | LE) | Cmp_s64 (LT | LE) | Cmp_s16 (LT | LE) | Cmp_s8 (LT | LE)
    ->
    Misc.fatal_error "Cmp_s LT/LE should be transformed in Simd_selection"
  (* Lane operations - insert from GP register *)
  | Setq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (INS (B, lane_idx)) (reg_v16b res.(0), reg_w arg.(1))
  | Setq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (INS (H, lane_idx)) (reg_v8h res.(0), reg_w arg.(1))
  | Setq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (INS (S, lane_idx)) (reg_v4s res.(0), reg_w arg.(1))
  | Setq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (INS (D, lane_idx)) (reg_v2d res.(0), reg_x arg.(1))
  (* Copy lane to lane *)
  | Copyq_laneq_s64 { src_lane; dst_lane } ->
    let lanes =
      Lane_index.Src_and_dest.create
        ~src:(Lane_index.create src_lane)
        ~dest:(Lane_index.create dst_lane)
    in
    ins2 (INS_V lanes) (reg_v2d res.(0), reg_v2d arg.(1))
  (* Duplicate lane *)
  | Dupq_lane_s8 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (DUP lane_idx) (v16b_v16b i)
  | Dupq_lane_s16 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (DUP lane_idx) (v8h_v8h i)
  | Dupq_lane_s32 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (DUP lane_idx) (v4s_v4s i)
  | Dupq_lane_s64 { lane } ->
    let lane_idx = Lane_index.create lane in
    ins2 (DUP lane_idx) (v2d_v2d i)
  (* Population count - CNT only works on 8-bit elements *)
  | Cntq_u8 -> ins2 CNT_vector (v16b_v16b i)
  (* Population count for 16-bit elements: CNT on bytes then UADDLP to sum
     pairs *)
  | Cntq_u16 ->
    let rd = reg_v8h res.(0) in
    let rs = reg_v16b arg.(0) in
    ins2 CNT_vector (rs, rs);
    ins2 UADDLP_vector (rd, rs)
  (* Two special cases for min/max scalar: generate a sequence that matches the
     weird semantics of amd64 instruction "minss", even when the flag [FPCR.AH]
     is not set. *)
  (* CR mshinwell for TheNumbat: the Min_scalar_f32/f64 and Max_scalar_f32/f64
     constructors should have more descriptive names to distinguish them from
     Fmin_f32/f64 and Fmax_f32/f64. *)
  | Min_scalar_f32 ->
    let rd = reg_s res.(0) in
    let rn = reg_s arg.(0) in
    let rm = reg_s arg.(1) in
    ins2 FCMP (rn, rm);
    ins4 FCSEL (rd, rn, rm, O.cond Cond.MI)
  | Min_scalar_f64 ->
    let rd = reg_d res.(0) in
    let rn = reg_d arg.(0) in
    let rm = reg_d arg.(1) in
    ins2 FCMP (rn, rm);
    ins4 FCSEL (rd, rn, rm, O.cond Cond.MI)
  | Max_scalar_f32 ->
    let rd = reg_s res.(0) in
    let rn = reg_s arg.(0) in
    let rm = reg_s arg.(1) in
    ins2 FCMP (rn, rm);
    ins4 FCSEL (rd, rn, rm, O.cond Cond.GT)
  | Max_scalar_f64 ->
    let rd = reg_d res.(0) in
    let rn = reg_d arg.(0) in
    let rm = reg_d arg.(1) in
    ins2 FCMP (rn, rm);
    ins4 FCSEL (rd, rn, rm, O.cond Cond.GT)

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = Cmm.new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | { typ = Val; loc = Reg r; _ } ->
        live_offset := ((r lsl 1) + 1) :: !live_offset
      | { typ = Val; loc = Stack s; _ } as reg ->
        live_offset
          := slot_offset s (Stack_class.of_machtype reg.typ) :: !live_offset
      | { typ = Addr; _ } as r ->
        Misc.fatal_errorf "bad GC root %a" Printreg.reg r
      | { typ = Valx2; _ } as r ->
        (* CR mslater: (SIMD) arm64 *)
        Misc.fatal_errorf "Unexpected Valx2 type of reg %a" Printreg.reg r
      | { typ = Val; loc = Unknown; _ } as r ->
        Misc.fatal_errorf "Unknown location %a" Printreg.reg r
      | { typ = Int | Float | Float32 | Vec128; _ } -> ()
      | { typ = Vec256 | Vec512; _ } ->
        Misc.fatal_error "arm64: got 256/512 bit vector")
    live;
  (* CR sspies: Consider changing [record_frame_descr] to [Asm_label.t] instead
     of linear labels. *)
  record_frame_descr ~label:lbl ~frame_size:(frame_size ())
    ~live_offset:!live_offset dbg;
  label_to_asm_label ~section:Text lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in
  D.define_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: L.t;                      (* Entry label *)
    gc_return_lbl: L.t;               (* Where to branch after GC *)
    gc_frame_lbl: L.t }               (* Label of frame descriptor *)
[@@ocamlformat "disable"]

let call_gc_sites = ref ([] : gc_call list)

let emit_call_gc gc =
  labelled_ins1 gc.gc_lbl BL (runtime_function S.Predef.caml_call_gc);
  labelled_ins1 gc.gc_frame_lbl B (local_label gc.gc_return_lbl)

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl : L.t;
    lr_return_lbl : L.t;
    lr_dbg : Debuginfo.t
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let file_emitter ~file_num ~file_name =
  D.file ~file_num:(Some file_num) ~file_name

let emit_debug_info ?discriminator dbg =
  Emitaux.emit_debug_info_gen ?discriminator dbg file_emitter D.loc

let emit_local_realloc lr =
  D.define_label lr.lr_lbl;
  emit_debug_info lr.lr_dbg;
  A.ins1 BL (runtime_function S.Predef.caml_call_local_realloc);
  A.ins1 B (local_label lr.lr_return_lbl)

(* Local stack reallocation *)

type stack_realloc =
  { sc_label : L.t; (* Label of the reallocation code. *)
    sc_return : L.t; (* Label to return to after reallocation. *)
    sc_max_frame_size_in_bytes : int (* Size for reallocation. *)
  }

let stack_realloc = ref (None : stack_realloc option)

let clear_stack_realloc () = stack_realloc := None

let emit_stack_realloc () =
  match !stack_realloc with
  | None -> ()
  | Some { sc_label; sc_return; sc_max_frame_size_in_bytes } ->
    D.define_label sc_label;
    (* Pass the desired frame size on the stack, since all of the
       argument-passing registers may be in use. *)
    A.ins_mov_imm reg_x_tmp1 (O.imm_sixteen sc_max_frame_size_in_bytes);
    A.ins3 (STP X) reg_x_tmp1 O.lr (O.mem_pre_pair ~base:R.sp ~offset:(-16));
    A.ins1 BL (runtime_function S.Predef.caml_call_realloc_stack);
    A.ins3 (LDP X) reg_x_tmp1 O.lr (O.mem_post_pair ~base:R.sp ~offset:16);
    A.ins1 B (local_label sc_return)

(* Names of various instructions *)

let cond_for_comparison : integer_comparison -> Cond.t = function
  | Ceq -> EQ
  | Cne -> NE
  | Cle -> LE
  | Cge -> GE
  | Clt -> LT
  | Cgt -> GT
  | Cule -> LS
  | Cuge -> CS
  | Cult -> CC
  | Cugt -> HI

(* Decompose an integer constant into four 16-bit shifted fragments. Omit the
   fragments that are equal to "default" (16 zeros or 16 ones). *)

let decompose_int default n =
  let rec decomp n pos =
    if pos >= 64
    then []
    else
      let frag = Nativeint.logand n 0xFFFFn
      and rem = Nativeint.shift_right_logical n 16 in
      if Nativeint.equal frag default
      then decomp rem (pos + 16)
      else (frag, pos) :: decomp rem (pos + 16)
  in
  decomp n 0

(* Load an integer constant into a register *)

let emit_movk dst (f, p) =
  A.ins3 MOVK dst
    (O.imm_sixteen_of_nativeint f)
    (O.lsl_by_multiple_of_16_bits p)

let emit_intconst dst n =
  if Arm64_ast.Logical_immediates.is_logical_immediate n
  then A.ins3 ORR_immediate dst O.xzr (O.bitmask n)
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    if List.length dz <= List.length dn
    then (
      match dz with
      | [] -> A.ins_mov_reg dst O.xzr
      | (f, p) :: l ->
        A.ins3 MOVZ dst
          (O.imm_sixteen_of_nativeint f)
          (O.optional_lsl_by_multiple_of_16_bits p);
        List.iter (emit_movk dst) l)
    else
      match dn with
      | [] -> A.ins3 MOVN dst (O.imm_sixteen 0) O.optional_none
      | (f, p) :: l ->
        let nf = Nativeint.logxor f 0xFFFFn in
        A.ins3 MOVN dst
          (O.imm_sixteen_of_nativeint nf)
          (O.optional_lsl_by_multiple_of_16_bits p);
        List.iter (emit_movk dst) l

let num_instructions_for_intconst n =
  if Arm64_ast.Logical_immediates.is_logical_immediate n
  then 1
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    max 1 (min (List.length dz) (List.length dn))

(* Recognize float constants appropriate for FMOV dst, #fpimm instruction: "a
   normalized binary floating point encoding with 1 sign bit, 4 bits of fraction
   and a 3-bit exponent" *)

let is_immediate_float bits =
  let exp = (Int64.(to_int (shift_right_logical bits 52)) land 0x7FF) - 1023 in
  let mant = Int64.logand bits 0xF_FFFF_FFFF_FFFFL in
  exp >= -3 && exp <= 4
  && Int64.equal (Int64.logand mant 0xF_0000_0000_0000L) mant

let is_immediate_float32 bits =
  let exp = (Int32.(to_int (shift_right_logical bits 23)) land 0x7F) - 63 in
  let mant = Int32.logand bits 0x7F_FFFFl in
  exp >= -3 && exp <= 4 && Int32.equal (Int32.logand mant 0x78_0000l) mant

(* Adjust sp (up or down) by the given byte amount *)

let emit_stack_adjustment n =
  let m = abs n in
  assert (m < 0x1_000_000);
  let ml = m land 0xFFF and mh = m land 0xFFF_000 in
  if n < 0
  then (
    if mh <> 0 then A.ins4 SUB_immediate O.sp O.sp (O.imm mh) O.optional_none;
    if ml <> 0 then A.ins4 SUB_immediate O.sp O.sp (O.imm ml) O.optional_none)
  else (
    if mh <> 0 then A.ins4 ADD_immediate O.sp O.sp (O.imm mh) O.optional_none;
    if ml <> 0 then A.ins4 ADD_immediate O.sp O.sp (O.imm ml) O.optional_none);
  if n <> 0 then D.cfi_adjust_cfa_offset ~bytes:(-n)

(* Output add-immediate / sub-immediate / cmp-immediate instructions *)

let rec emit_addimm rd rs n =
  if n < 0
  then emit_subimm rd rs (-n)
  else if n <= 0xFFF
  then A.ins4 ADD_immediate rd rs (O.imm n) O.optional_none
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    A.ins4 ADD_immediate rd rs (O.imm nh) O.optional_none;
    if nl <> 0 then A.ins4 ADD_immediate rd rd (O.imm nl) O.optional_none)

and emit_subimm rd rs n =
  if n < 0
  then emit_addimm rd rs (-n)
  else if n <= 0xFFF
  then A.ins4 SUB_immediate rd rs (O.imm n) O.optional_none
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    A.ins4 SUB_immediate rd rs (O.imm nh) O.optional_none;
    if nl <> 0 then A.ins4 SUB_immediate rd rd (O.imm nl) O.optional_none)

let emit_cmpimm rs n =
  if n >= 0
  then A.ins_cmp rs (O.imm n) O.optional_none
  else A.ins_cmn rs (O.imm (-n)) O.optional_none

(* Helper functions for load/store with large stack offsets. When stack frames
   grow large, offsets can exceed the ARM64 immediate limits. For out-of-range
   offsets, we compute the address in x16 (reg_tmp1) and then perform the
   load/store. x16 (IP0) is safe as it's reserved for linker veneers and not
   allocated by the register allocator. *)

(* XXX need to revalidate that reg_x_tmp1 is safe *)

let emit_load_store_sp_offset instr reg offset =
  match O.mem_offset ~base:R.sp ~scale:8 ~offset with
  | Ok operand -> A.ins2 instr reg operand
  | Offset_out_of_range ->
    A.ins_mov_from_sp ~dst:reg_x_tmp1;
    emit_addimm reg_x_tmp1 reg_x_tmp1 offset;
    A.ins2 instr reg (H.mem reg_tmp1_base)

(* Eta-expand to preserve polymorphism over register width (X, W, LR) *)
let emit_ldr_sp_offset reg offset = emit_load_store_sp_offset LDR reg offset

let emit_str_sp_offset reg offset = emit_load_store_sp_offset STR reg offset

(* Generic load/store with stack register that handles large offsets. Works with
   any instruction (LDR, STR, LDR_simd_and_fp, STR_simd_and_fp). *)
let emit_stack_load_store instr reg (r : Reg.t) =
  match stack r with
  | Stack_operand operand -> A.ins2 instr reg operand
  | Stack_large_offset_sp { bytes } ->
    A.ins_mov_from_sp ~dst:reg_x_tmp1;
    emit_addimm reg_x_tmp1 reg_x_tmp1 bytes;
    A.ins2 instr reg (H.mem reg_tmp1_base)
  | Stack_large_offset_domainstate { bytes } ->
    A.ins_mov_reg reg_x_tmp1 (H.reg_x reg_domain_state_ptr);
    emit_addimm reg_x_tmp1 reg_x_tmp1 bytes;
    A.ins2 instr reg (H.mem reg_tmp1_base)

let emit_stack_ldr reg r = emit_stack_load_store LDR reg r

let emit_stack_str reg r = emit_stack_load_store STR reg r

let emit_stack_ldr_simd_and_fp reg r =
  emit_stack_load_store LDR_simd_and_fp reg r

let emit_stack_str_simd_and_fp reg r =
  emit_stack_load_store STR_simd_and_fp reg r

(* Name of current function *)
let function_name = ref ""

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None

(* Pending floating-point literals *)
let float32_literals = ref ([] : (int32 * L.t) list)

let float_literals = ref ([] : (int64 * L.t) list)

let vec128_literals = ref ([] : (Cmm.vec128_bits * L.t) list)

(* Label a floating-point literal *)
let add_literal p f =
  try List.assoc f !p
  with Not_found ->
    (* CR sspies: The [Text] section here is incorrect. We should be in the
       respective section of the literal type (i.e., 16 or 8 bytes). The code
       below uses the [Text] section, because that is the section that we are in
       when we emit literals in the function body. Only macOS currently switches
       to a dedicated section. *)
    let lbl = L.create Text in
    p := (f, lbl) :: !p;
    lbl

let float32_literal f = add_literal float32_literals f

let float_literal f = add_literal float_literals f

let vec128_literal f = add_literal vec128_literals f

(* Emit all pending literals *)
let emit_literals p align emit_literal =
  if not (Misc.Stdlib.List.is_empty !p)
  then (
    if macosx
    then
      D.switch_to_section_raw
        ~names:["__TEXT"; "__literal" ^ Int.to_string align]
        ~flags:None
        ~args:[Int.to_string align ^ "byte_literals"]
        ~is_delayed:false
    else
      D.switch_to_section_raw
        ~names:[".rodata.cst" ^ Int.to_string align]
        ~flags:(Some "aM")
        ~args:["@progbits"; Int.to_string align]
        ~is_delayed:false;
    (* CR sspies: We set the internal section ref to Text here, because section
       ref does not support named text sections yet. Fix this when cleaning up
       the section mechanism. *)
    D.unsafe_set_internal_section_ref Text;
    D.align ~fill_x86_bin_emitter:Nop ~bytes:align;
    List.iter emit_literal !p;
    p := [])

let emit_float32_literal (f, lbl) =
  D.define_label lbl;
  D.float32_from_bits f;
  (* padding to 8 bytes *)
  D.int32 0xDEAD_BEEFl

let emit_float_literal (f, lbl) =
  D.define_label lbl;
  D.float64_from_bits f

let emit_vec128_literal (({ word0; word1 } : Cmm.vec128_bits), lbl) =
  D.define_label lbl;
  D.float64_from_bits word0;
  D.float64_from_bits word1

let emit_literals () =
  (* Align float32 literals to [size_float]=8 bytes, not 4. *)
  emit_literals float32_literals size_float emit_float32_literal;
  emit_literals float_literals size_float emit_float_literal;
  emit_literals vec128_literals size_vec128 emit_vec128_literal

(* Emit code to load the address of a symbol *)

let emit_load_symbol_addr dst s =
  (* Local symbols can use direct PC-relative addressing since they are defined
     in the same compilation unit and are emitted as labels (not linker
     symbols). Global symbols need GOT on macOS (always) or on Linux when
     building shared objects (dlcode). This matches x86-64 behavior in
     amd64/emit.ml. *)
  let dst_x = H.reg_x dst in
  if S.is_local s
  then (
    (* Local symbols are defined as labels, so reference them as labels. Use
       Text section since this is referencing code symbols. *)
    let lbl = L.create_label_for_local_symbol Text s in
    A.ins2 ADRP dst_x (label (Needs_reloc PAGE) lbl);
    A.ins4 ADD_immediate dst_x dst_x
      (label (Needs_reloc PAGE_OFF) lbl)
      O.optional_none)
  else if macosx || !Clflags.dlcode
  then (
    (* Global symbols need GOT on macOS or when building shared objects *)
    A.ins2 ADRP dst_x (symbol (Needs_reloc GOT_PAGE) s);
    A.ins2 LDR dst_x
      (H.mem_symbol_reg (H.gp_reg_of_reg dst)
         ~reloc:
           (Needs_reloc (if macosx then GOT_PAGE_OFF else GOT_LOWER_TWELVE))
         s))
  else (
    (* Global symbols without dlcode can use direct addressing *)
    A.ins2 ADRP dst_x (symbol (Needs_reloc PAGE) s);
    A.ins4 ADD_immediate dst_x dst_x
      (symbol (Needs_reloc PAGE_OFF) s)
      O.optional_none)

(* The following functions are used for calculating the sizes of the call GC and
   bounds check points emitted out-of-line from the function body. See
   branch_relaxation.mli. *)

let num_call_gc_points instr =
  let rec loop instr call_gc =
    match instr.desc with
    | Lend -> call_gc
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag ->
      loop instr.next (call_gc + 1)
    | Lop Poll -> loop instr.next (call_gc + 1)
    (* The following four should never be seen, since this function is run
       before branch relaxation. *)
    | Lop (Specific (Ifar_alloc _)) | Lop (Specific Ifar_poll) -> assert false
    | Lop (Alloc { mode = Local | Heap; _ })
    | Lop
        (Specific
           ( Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
           | Inegmulsubf | Isqrtf | Imove32
           | Ishiftarith (_, _)
           | Ibswap _ | Isignext _ | Isimd _ ))
    | Lop
        ( Move | Spill | Reload | Opaque | Pause | Begin_region | End_region
        | Dls_get | Tls_get | Domain_index | Const_int _ | Const_float32 _
        | Const_float _ | Const_symbol _ | Const_vec128 _ | Stackoffset _
        | Load _
        | Store (_, _, _)
        | Intop _ | Int128op _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Name_for_debugger _ )
    | Lprologue | Lepilogue_open | Lepilogue_close | Lreloadretaddr | Lreturn
    | Lentertrap | Lpoptrap _ | Lcall_op _ | Llabel _ | Lbranch _
    | Lcondbranch (_, _)
    | Lcondbranch3 (_, _, _)
    | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
    | Lstackcheck _ ->
      loop instr.next call_gc
    | Lop (Const_vec256 _ | Const_vec512 _) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Specific (Illvm_intrinsic intr)) ->
      Misc.fatal_errorf
        "Emit: Unexpected llvm_intrinsic %s: not using LLVM backend" intr
  in
  loop instr 0

let max_out_of_line_code_offset ~num_call_gc =
  if num_call_gc < 1
  then 0
  else
    let size_of_call_gc = 2 in
    let size_of_last_thing = size_of_call_gc in
    let total_size = size_of_call_gc * num_call_gc in
    let max_offset = total_size - size_of_last_thing in
    assert (max_offset >= 0);
    max_offset

module BR = Branch_relaxation.Make (struct
  (* CR-someday mshinwell: B and BL have +/- 128Mb ranges; for the moment we
     assume we will never exceed this. It would seem to be most likely to occur
     for branches between functions; in this case, the linker should be able to
     insert veneers anyway. (See section 4.6.7 of the document "ELF for the ARM
     64-bit architecture (AArch64)".) *)

  type distance = int

  module Cond_branch = struct
    type t =
      | TB
      | CB
      | Bcc

    let all = [TB; CB; Bcc]

    (* AArch64 instructions are 32 bits wide, so [distance] in this module means
       units of 32-bit words. *)
    let max_displacement = function
      | TB -> 32 * 1024 / 4 (* +/- 32Kb *)
      | CB | Bcc -> 1 * 1024 * 1024 / 4 (* +/- 1Mb *)

    let classify_instr = function
      | Lop (Alloc _) | Lop Poll -> Some Bcc
      (* The various "far" variants in [specific_operation] don't need to return
         [Some] here, since their code sequences never contain any conditional
         branches that might need relaxing. *)
      | Lcondbranch (Itruetest, _) | Lcondbranch (Ifalsetest, _) -> Some CB
      | Lcondbranch (Iinttest _, _)
      | Lcondbranch (Iinttest_imm _, _)
      | Lcondbranch (Ifloattest _, _) ->
        Some Bcc
      | Lcondbranch (Ioddtest, _) | Lcondbranch (Ieventest, _) -> Some TB
      | Lcondbranch3 _ -> Some Bcc
      | Lop
          ( Specific _ | Move | Spill | Reload | Opaque | Begin_region | Pause
          | End_region | Dls_get | Tls_get | Domain_index | Const_int _
          | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
          | Stackoffset _ | Load _
          | Store (_, _, _)
          | Intop _ | Int128op _
          | Intop_imm (_, _)
          | Intop_atomic _
          | Floatop (_, _)
          | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
          | Name_for_debugger _ )
      | Lprologue | Lepilogue_open | Lepilogue_close | Lend | Lreloadretaddr
      | Lreturn | Lentertrap | Lpoptrap _ | Lcall_op _ | Llabel _ | Lbranch _
      | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
      | Lstackcheck _ ->
        None
      | Lop (Const_vec256 _ | Const_vec512 _) ->
        Misc.fatal_error "arm64: got 256/512 bit vector"
  end

  let offset_pc_at_branch = 0

  let prologue_size () =
    (if initial_stack_offset () > 0 then 2 else 0)
    + if !contains_calls then 1 else 0

  let epilogue_size () = if !contains_calls then 3 else 2

  let memory_access_size (memory_chunk : Cmm.memory_chunk) =
    match memory_chunk with
    | Single { reg = Float64 } -> 2
    | Single { reg = Float32 } -> 1
    | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
    | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Double
    | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
      1
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector"

  let instr_size instr =
    match instr.Linear.desc with
    | Lend -> 0
    | Lprologue -> prologue_size ()
    | Lepilogue_open -> epilogue_size ()
    | Lepilogue_close -> 0
    | Lop (Move | Spill | Reload) -> 1
    | Lop (Const_int n) -> num_instructions_for_intconst n
    | Lop (Const_float32 _) -> 2
    | Lop (Const_float _) -> 2
    | Lop (Const_vec128 _) -> 2
    | Lop (Const_vec256 _ | Const_vec512 _) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Const_symbol _) -> 2
    | Lop (Intop_atomic _) ->
      Misc.fatal_errorf
        "emit_instr: builtins are not yet translated to atomics: %a"
        Printlinear.instr instr
    | Lcall_op Lcall_ind -> 1
    | Lcall_op (Lcall_imm _) -> 1
    | Lcall_op Ltailcall_ind -> epilogue_size ()
    | Lcall_op (Ltailcall_imm { func; _ }) ->
      if String.equal func.sym_name !function_name then 1 else epilogue_size ()
    | Lcall_op
        (Lextcall
           { alloc;
             stack_ofs;
             stack_align = _;
             func = _;
             ty_res = _;
             ty_args = _;
             returns = _
           }) ->
      if Config.runtime5 && stack_ofs > 0 then 5 else if alloc then 3 else 5
    | Lop (Stackoffset _) -> 2
    | Lop (Load { memory_chunk; addressing_mode; is_atomic; mutability = _ }) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier = if is_atomic then 1 else 0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Store (memory_chunk, addressing_mode, assignment)) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier =
        match memory_chunk, assignment with
        | (Word_int | Word_val), true -> 1
        | (Word_int | Word_val), false -> 0
        | ( ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
            | Onetwentyeight_unaligned | Onetwentyeight_aligned ),
            _ ) ->
          0
        | ( ( Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
            | Fivetwelve_unaligned ),
            _ ) ->
          Misc.fatal_error "arm64: got 256/512 bit vector"
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Alloc { mode = Local; _ }) -> 9
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag -> 5
    | Lop (Specific (Ifar_alloc _)) when !fastcode_flag -> 6
    | Lop Poll -> 3
    | Lop Pause -> 1
    | Lop (Specific Ifar_poll) -> 4
    | Lop (Alloc { mode = Heap; bytes = num_bytes; _ })
    | Lop (Specific (Ifar_alloc { bytes = num_bytes; _ })) -> (
      match num_bytes with
      | 16 | 24 | 32 -> 1
      | _ -> 1 + num_instructions_for_intconst (Nativeint.of_int num_bytes))
    | Lop (Csel _) -> 4
    | Lop (Begin_region | End_region) -> 1
    | Lop (Intop (Icomp _)) -> 2
    | Lop (Floatop (Float64, Icompf _)) -> 2
    | Lop (Floatop (Float32, Icompf _)) -> 2
    | Lop (Intop_imm (Icomp _, _)) -> 2
    | Lop (Int128op (Iadd128 | Isub128 | Imul64 _)) -> 2
    | Lop (Intop Imod) -> 2
    | Lop (Intop (Imulh _)) -> 1
    | Lop (Intop (Iclz _)) -> 1
    | Lop (Intop (Ictz _)) -> if !Arch.feat_cssc then 1 else 2
    | Lop (Intop Ipopcnt) -> if !Arch.feat_cssc then 1 else 4
    | Lop
        (Intop
           (Iadd | Isub | Imul | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr))
      ->
      1
    | Lop
        (Intop_imm
           ( ( Iadd | Isub | Imul | Idiv | Imod | Imulh _ | Iand | Ior | Ixor
             | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt ),
             _ )) ->
      1
    | Lop (Floatop (Float64, (Iabsf | Inegf))) -> 1
    | Lop (Floatop (Float32, (Iabsf | Inegf))) -> 1
    | Lop (Specific Isqrtf) -> 1
    | Lop
        (Reinterpret_cast
           (Value_of_int | Int_of_value | Float_of_int64 | Int64_of_float)) ->
      1
    | Lop
        (Reinterpret_cast
           ( Float32_of_float | Float_of_float32 | Float32_of_int32
           | Int32_of_float32 )) ->
      1
    | Lop (Reinterpret_cast (V128_of_vec Vec128)) -> 1
    | Lop
        (Reinterpret_cast
           (V128_of_vec (Vec256 | Vec512) | V256_of_vec _ | V512_of_vec _)) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Static_cast (Float_of_int Float64 | Int_of_float Float64)) -> 1
    | Lop
        (Static_cast
           ( Float_of_int Float32
           | Int_of_float Float32
           | Float_of_float32 | Float32_of_float )) ->
      1
    | Lop (Static_cast (Scalar_of_v128 Float16x8 | V128_of_scalar Float16x8)) ->
      Misc.fatal_error "float16 scalar type not supported"
    | Lop (Static_cast (Scalar_of_v128 (Int8x16 | Int16x8))) -> 2
    | Lop
        (Static_cast
           (Scalar_of_v128 (Int32x4 | Int64x2 | Float32x4 | Float64x2))) ->
      1
    | Lop (Static_cast (V128_of_scalar _)) -> 1
    | Lop
        (Static_cast
           ( V256_of_scalar _ | Scalar_of_v256 _ | V512_of_scalar _
           | Scalar_of_v512 _ )) ->
      Misc.fatal_error "arm64: got 256/512 bit vector"
    | Lop (Floatop (Float64, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Floatop (Float32, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Specific Inegmulf) -> 1
    | Lop Opaque -> 0
    | Lop (Specific (Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf)) -> 1
    | Lop (Specific (Ishiftarith _)) -> 1
    | Lop (Specific (Imuladd | Imulsub)) -> 1
    | Lop (Specific (Ibswap { bitwidth = Sixteen })) -> 2
    | Lop (Specific (Ibswap { bitwidth = Thirtytwo | Sixtyfour })) -> 1
    | Lop (Specific Imove32) -> 1
    | Lop (Specific (Isignext _)) -> 1
    | Lop (Name_for_debugger _) -> 0
    | Lcall_op (Lprobe _) ->
      Misc.fatal_error "Optimized probes not supported on arm64."
    | Lop (Probe_is_enabled _) -> 3
    | Lop Dls_get -> 1
    | Lop Tls_get -> 1
    | Lop Domain_index -> 1
    | Lreloadretaddr -> 0
    | Lreturn -> epilogue_size ()
    | Llabel _ -> 0
    | Lbranch _ -> 1
    | Lcondbranch (tst, _) -> (
      match tst with
      | Itruetest -> 1
      | Ifalsetest -> 1
      | Iinttest _ -> 2
      | Iinttest_imm _ -> 2
      | Ifloattest _ -> 2
      | Ioddtest -> 1
      | Ieventest -> 1)
    | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
      1
      + (match lbl0 with None -> 0 | Some _ -> 1)
      + (match lbl1 with None -> 0 | Some _ -> 1)
      + match lbl2 with None -> 0 | Some _ -> 1)
    | Lswitch jumptbl -> 3 + Array.length jumptbl
    | Lentertrap -> 0
    | Ladjust_stack_offset _ -> 0
    | Lpushtrap _ -> 4
    | Lpoptrap _ -> 1
    | Lraise k -> (
      match k with
      | Lambda.Raise_regular -> 1
      | Lambda.Raise_reraise -> 1
      | Lambda.Raise_notrace -> 4)
    | Lstackcheck _ -> 5
    | Lop (Specific (Isimd simd)) ->
      A.with_measuring ~f:(fun () -> simd_instr simd instr)
    | Lop (Specific (Illvm_intrinsic intr)) ->
      Misc.fatal_errorf
        "Emit.size:Unexpected llvm_intrinsic %s: not using LLVM backend" intr

  let relax_poll () = Lop (Specific Ifar_poll)

  let relax_allocation ~num_bytes ~dbginfo =
    Lop (Specific (Ifar_alloc { bytes = num_bytes; dbginfo }))
end)

let cond_for_float_comparison : Cmm.float_comparison -> Float_cond.t = function
  | CFeq -> EQ
  | CFneq -> NE
  | CFlt -> CC
  | CFnlt -> CS
  | CFle -> LS
  | CFnle -> HI
  | CFgt -> GT
  | CFngt -> LE
  | CFge -> GE
  | CFnge -> LT

let cond_for_cset_for_float_comparison : Cmm.float_comparison -> Cond.t =
  function
  | CFeq -> EQ
  | CFneq -> NE
  | CFlt -> CC
  | CFnlt -> CS
  | CFle -> LS
  | CFnle -> HI
  | CFgt -> GT
  | CFngt -> LE
  | CFge -> GE
  | CFnge -> LT

(* Output the assembly code for an allocation. *)

let assembly_code_for_local_allocation i ~n =
  let r = H.reg_x i.res.(0) in
  A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_local_limit);
  A.ins2 LDR r (H.domainstate_field Domain_local_sp);
  emit_subimm r r n;
  A.ins2 STR r (H.domainstate_field Domain_local_sp);
  A.ins_cmp_reg r reg_x_tmp1 O.optional_none;
  let lr_lbl = L.create Text in
  A.ins1 (B_cond (Branch_cond.Int LT)) (local_label lr_lbl);
  let lr_return_lbl = L.create Text in
  D.define_label lr_return_lbl;
  A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_local_top);
  A.ins4 ADD_shifted_register r r reg_x_tmp1 O.optional_none;
  A.ins4 ADD_immediate r r (O.imm 8) O.optional_none;
  local_realloc_sites
    := { lr_lbl; lr_dbg = i.dbg; lr_return_lbl } :: !local_realloc_sites

let assembly_code_for_fast_heap_allocation i ~n ~far ~dbginfo =
  let gc_frame_lbl = record_frame_label i.live (Dbg_alloc dbginfo) in
  let gc_return_lbl = L.create Text in
  let gc_lbl = L.create Text in
  (* n is at most Max_young_whsize * 8, i.e. currently 0x808, so it is
     reasonable to assume n < 0x1_000. This makes the generated code simpler. *)
  assert (16 <= n && n < 0x1_000 && n land 0x7 = 0);
  A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_young_limit);
  emit_subimm reg_x_alloc_ptr reg_x_alloc_ptr n;
  A.ins_cmp_reg reg_x_alloc_ptr reg_x_tmp1 O.optional_none;
  (if not far
   then A.ins1 (B_cond (Branch_cond.Int CC)) (local_label gc_lbl)
   else
     let lbl = L.create Text in
     A.ins1 (B_cond (Branch_cond.Int CS)) (local_label lbl);
     A.ins1 B (local_label gc_lbl);
     D.define_label lbl);
  labelled_ins4 gc_return_lbl ADD_immediate
    (H.reg_x i.res.(0))
    reg_x_alloc_ptr (O.imm 8) O.optional_none;
  call_gc_sites := { gc_lbl; gc_return_lbl; gc_frame_lbl } :: !call_gc_sites

let assembly_code_for_slow_heap_allocation i ~n ~dbginfo =
  let lbl_frame = record_frame_label i.live (Dbg_alloc dbginfo) in
  (match n with
  | 16 -> A.ins1 BL (runtime_function S.Predef.caml_alloc1)
  | 24 -> A.ins1 BL (runtime_function S.Predef.caml_alloc2)
  | 32 -> A.ins1 BL (runtime_function S.Predef.caml_alloc3)
  | _ ->
    emit_intconst reg_x_x8 (Nativeint.of_int n);
    A.ins1 BL (runtime_function S.Predef.caml_allocN));
  labelled_ins4 lbl_frame ADD_immediate
    (H.reg_x i.res.(0))
    reg_x_alloc_ptr (O.imm 8) O.optional_none

let assembly_code_for_allocation i ~local ~n ~far ~dbginfo =
  if local
  then assembly_code_for_local_allocation i ~n
  else if !fastcode_flag
  then assembly_code_for_fast_heap_allocation i ~n ~far ~dbginfo
  else assembly_code_for_slow_heap_allocation i ~n ~dbginfo

(* Output the assembly code for a poll. *)

let assembly_code_for_poll i ~far ~return_label =
  let gc_frame_lbl = record_frame_label i.live (Dbg_alloc []) in
  let gc_lbl = L.create Text in
  let gc_return_lbl =
    match return_label with None -> L.create Text | Some lbl -> lbl
  in
  A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_young_limit);
  A.ins_cmp_reg reg_x_alloc_ptr reg_x_tmp1 O.optional_none;
  (if not far
   then (
     match return_label with
     | None ->
       A.ins1 (B_cond (Branch_cond.Int LS)) (local_label gc_lbl);
       D.define_label gc_return_lbl
     | Some return_label ->
       A.ins1 (B_cond (Branch_cond.Int HI)) (local_label return_label);
       A.ins1 B (local_label gc_lbl))
   else
     match return_label with
     | None ->
       A.ins1 (B_cond (Branch_cond.Int HI)) (local_label gc_return_lbl);
       A.ins1 B (local_label gc_lbl);
       D.define_label gc_return_lbl
     | Some return_label ->
       let lbl = L.create Text in
       A.ins1 (B_cond (Branch_cond.Int LS)) (local_label lbl);
       A.ins1 B (local_label return_label);
       labelled_ins1 lbl B (local_label gc_lbl));
  call_gc_sites := { gc_lbl; gc_return_lbl; gc_frame_lbl } :: !call_gc_sites

(* Output .text section directive, or named .text.caml.<name> if enabled. *)

let emit_named_text_section func_name =
  if !Clflags.function_sections
  then (
    (* CR sspies: Clean this up and add proper support for function sections in
       the new asm directives. *)
    D.switch_to_section_raw
      ~names:[".text.caml." ^ S.encode (S.create_global func_name)]
      ~flags:(Some "ax") ~args:["%progbits"] ~is_delayed:false;
    (* Warning: We set the internal section ref to Text here, because it
       currently does not supported named text sections. In the rest of this
       file, we pretend the section is called Text rather than the function
       specific text section. *)
    D.unsafe_set_internal_section_ref Text)
  else D.text ()

(* Emit code to load an emitted literal *)

let emit_load_literal dst lbl =
  let reloc : [`Twelve] Ast.Symbol.same_unit_or_reloc =
    if macosx then Needs_reloc PAGE_OFF else Needs_reloc LOWER_TWELVE
  in
  let addr = H.mem_label reg_tmp1_base ~reloc lbl in
  (* ADRP always needs a PAGE relocation on both platforms - the assembler
     cannot resolve page-aligned addresses until link time. On macOS this prints
     "label@PAGE"; on ELF it prints just "label" since the GNU assembler infers
     the relocation from the ADRP instruction. *)
  A.ins2 ADRP reg_x_tmp1 (label (Needs_reloc PAGE) lbl);
  match dst.typ with
  | Float -> A.ins2 LDR_simd_and_fp (H.reg_d dst) addr
  | Float32 -> A.ins2 LDR_simd_and_fp (H.reg_s dst) addr
  | Val | Int | Addr -> A.ins2 LDR (H.reg_x dst) addr
  | Vec128 | Valx2 -> A.ins2 LDR_simd_and_fp (H.reg_q dst) addr
  | Vec256 | Vec512 ->
    Misc.fatal_errorf "emit_load_literal: unexpected vector register %a"
      Printreg.reg dst

let move_between_distinct_locs (src : Reg.t) (dst : Reg.t) =
  match src.typ, src.loc, dst.typ, dst.loc with
  | Float, Reg _, Float, Reg _ -> A.ins2 FMOV_fp (H.reg_d dst) (H.reg_d src)
  | Float32, Reg _, Float32, Reg _ -> A.ins2 FMOV_fp (H.reg_s dst) (H.reg_s src)
  | (Vec128 | Valx2), Reg _, (Vec128 | Valx2), Reg _ ->
    A.ins_mov_vector (H.reg_v16b_operand dst) (H.reg_v16b_operand src)
  | (Vec256 | Vec512), _, _, _ | _, _, (Vec256 | Vec512), _ ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | (Int | Val | Addr), Reg _, (Int | Val | Addr), Reg _ ->
    A.ins_mov_reg (H.reg_x dst) (H.reg_x src)
  | Float, Reg _, Float, Stack _ -> emit_stack_str_simd_and_fp (H.reg_d src) dst
  | Float32, Reg _, Float32, Stack _ ->
    emit_stack_str_simd_and_fp (H.reg_s src) dst
  | (Vec128 | Valx2), Reg _, (Vec128 | Valx2), Stack _ ->
    emit_stack_str_simd_and_fp (H.reg_q src) dst
  | (Int | Val | Addr), Reg _, (Int | Val | Addr), Stack _ ->
    emit_stack_str (H.reg_x src) dst
  | Float, Stack _, Float, Reg _ -> emit_stack_ldr_simd_and_fp (H.reg_d dst) src
  | Float32, Stack _, Float32, Reg _ ->
    emit_stack_ldr_simd_and_fp (H.reg_s dst) src
  | (Vec128 | Valx2), Stack _, (Vec128 | Valx2), Reg _ ->
    emit_stack_ldr_simd_and_fp (H.reg_q dst) src
  | (Int | Val | Addr), Stack _, (Int | Val | Addr), Reg _ ->
    emit_stack_ldr (H.reg_x dst) src
  | _, Stack _, _, Stack _ ->
    Misc.fatal_errorf "Illegal move between stack slots (%a to %a)\n"
      Printreg.reg src Printreg.reg dst
  | _, Unknown, _, (Reg _ | Stack _ | Unknown)
  | _, (Reg _ | Stack _), _, Unknown ->
    Misc.fatal_errorf
      "Illegal move with an unknown register location (%a to %a)\n" Printreg.reg
      src Printreg.reg dst
  | ( (Float | Float32 | Vec128 | Int | Val | Addr | Valx2),
      (Reg _ | Stack _),
      _,
      _ ) ->
    Misc.fatal_errorf
      "Illegal move between registers of differing types (%a to %a)\n"
      Printreg.reg src Printreg.reg dst

let move src dst =
  let distinct = not (Reg.same_loc src dst) in
  if distinct then move_between_distinct_locs src dst

let emit_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
  let src = i.arg.(0) in
  let dst = i.res.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int64_of_float -> A.ins2 FMOV_fp_to_gp_64 (H.reg_x dst) (H.reg_d src)
  | Float_of_int64 -> A.ins2 FMOV_gp_to_fp_64 (H.reg_d dst) (H.reg_x src)
  | Float32_of_int32 -> A.ins2 FMOV_gp_to_fp_32 (H.reg_s dst) (H.reg_w src)
  | Int32_of_float32 -> A.ins2 FMOV_fp_to_gp_32 (H.reg_w dst) (H.reg_s src)
  | Float32_of_float ->
    (* Reinterpret cast: copy lower 32 bits of Float to Float32 using 32-bit
       FMOV. We address the Float source as S to get its lower 32 bits. *)
    if distinct
    then (
      assert (Cmm.equal_machtype_component src.typ Float);
      assert (Cmm.equal_machtype_component dst.typ Float32);
      A.ins2 FMOV_fp (H.reg_s dst) (H.reg_s_of_float src))
  | Float_of_float32 ->
    (* Reinterpret cast: copy 32 bits of Float32 to lower 32 bits of Float using
       32-bit FMOV. We address the Float destination as S; ARM64 zeros the upper
       32 bits when writing to S. *)
    if distinct
    then (
      assert (Cmm.equal_machtype_component src.typ Float32);
      assert (Cmm.equal_machtype_component dst.typ Float);
      A.ins2 FMOV_fp (H.reg_s_of_float dst) (H.reg_s src))
  | V128_of_vec Vec128 ->
    if distinct
    then A.ins_mov_vector (H.reg_v16b_operand dst) (H.reg_v16b_operand src)
  | V128_of_vec (Vec256 | Vec512) | V256_of_vec _ | V512_of_vec _ ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | Int_of_value | Value_of_int -> move src dst

let emit_static_cast (cast : Cmm.static_cast) i =
  let dst = i.res.(0) in
  let src = i.arg.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int_of_float Float64 -> A.ins2 FCVTZS (H.reg_x dst) (H.reg_d src)
  | Int_of_float Float32 -> A.ins2 FCVTZS (H.reg_x dst) (H.reg_s src)
  | Float_of_int Float64 -> A.ins2 SCVTF (H.reg_d dst) (H.reg_x src)
  | Float_of_int Float32 -> A.ins2 SCVTF (H.reg_s dst) (H.reg_x src)
  | Float_of_float32 -> A.ins2 FCVT (H.reg_d dst) (H.reg_s src)
  | Float32_of_float -> A.ins2 FCVT (H.reg_s dst) (H.reg_d src)
  | Scalar_of_v128 v -> (
    (* src is Vec128, dst depends on vector element type *)
    match v with
    | Int8x16 ->
      A.ins2 FMOV_fp_to_gp_32 (H.reg_w dst) (H.reg_s_of_vec128 src);
      A.ins_uxtb (H.reg_w dst) (H.reg_w dst)
    | Int16x8 ->
      A.ins2 FMOV_fp_to_gp_32 (H.reg_w dst) (H.reg_s_of_vec128 src);
      A.ins_uxth (H.reg_w dst) (H.reg_w dst)
    | Int32x4 -> A.ins2 FMOV_fp_to_gp_32 (H.reg_w dst) (H.reg_s_of_vec128 src)
    | Int64x2 -> A.ins2 FMOV_fp_to_gp_64 (H.reg_x dst) (H.reg_d_of_vec128 src)
    | Float16x8 -> Misc.fatal_error "float16 scalar type not supported"
    | Float32x4 ->
      if distinct then A.ins2 FMOV_fp (H.reg_s dst) (H.reg_s_of_vec128 src)
    | Float64x2 ->
      if distinct then A.ins2 FMOV_fp (H.reg_d dst) (H.reg_d_of_vec128 src))
  | V128_of_scalar v -> (
    (* dst is Vec128, src depends on vector element type *)
    match v with
    | Int8x16 -> A.ins2 FMOV_gp_to_fp_32 (H.reg_s_of_vec128 dst) (H.reg_w src)
    | Int16x8 -> A.ins2 FMOV_gp_to_fp_32 (H.reg_s_of_vec128 dst) (H.reg_w src)
    | Int32x4 -> A.ins2 FMOV_gp_to_fp_32 (H.reg_s_of_vec128 dst) (H.reg_w src)
    | Int64x2 -> A.ins2 FMOV_gp_to_fp_64 (H.reg_d_of_vec128 dst) (H.reg_x src)
    | Float16x8 -> Misc.fatal_error "float16 scalar type not supported"
    | Float32x4 ->
      if distinct then A.ins2 FMOV_fp (H.reg_s_of_vec128 dst) (H.reg_s src)
    | Float64x2 ->
      if distinct then A.ins2 FMOV_fp (H.reg_d_of_vec128 dst) (H.reg_d src))
  | V256_of_scalar _ | Scalar_of_v256 _ | V512_of_scalar _ | Scalar_of_v512 _ ->
    Misc.fatal_error "arm64: got 256/512 bit vector"

(* Output the assembly code for an instruction *)

let emit_instr i =
  emit_debug_info i.dbg;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    assert !prologue_required;
    let n = frame_size () in
    if n > 0 then emit_stack_adjustment (-n);
    if !contains_calls
    then (
      D.cfi_offset ~reg:(R.gp_encoding R.lr) ~offset:(-8);
      emit_str_sp_offset O.lr (n - 8))
  | Lepilogue_open ->
    let n = frame_size () in
    if !contains_calls then emit_ldr_sp_offset O.lr (n - 8);
    if n > 0 then emit_stack_adjustment n
  | Lepilogue_close ->
    let n = frame_size () in
    if n > 0 then D.cfi_adjust_cfa_offset ~bytes:n
  | Lop (Intop_atomic _) ->
    Misc.fatal_errorf
      "emit_instr: builtins are not yet translated to atomics: %a"
      Printlinear.instr i
  | Lop (Reinterpret_cast cast) -> emit_reinterpret_cast cast i
  | Lop (Static_cast cast) -> emit_static_cast cast i
  | Lop (Move | Spill | Reload) -> move i.arg.(0) i.res.(0)
  | Lop (Specific Imove32) -> (
    let src = i.arg.(0) and dst = i.res.(0) in
    if not (Reg.same_loc src dst)
    then
      match src.loc, dst.loc with
      | Reg _, Reg _ -> A.ins_mov_reg_w (H.reg_w dst) (H.reg_w src)
      | Reg _, Stack _ -> emit_stack_str (H.reg_w src) dst
      | Stack _, Reg _ -> emit_stack_ldr (H.reg_w dst) src
      | Stack _, Stack _ | _, Unknown | Unknown, _ -> assert false)
  | Lop (Const_int n) -> emit_intconst (H.reg_x i.res.(0)) n
  | Lop (Const_float32 f) ->
    if Int32.equal f 0l
    then A.ins2 FMOV_gp_to_fp_32 (H.reg_s i.res.(0)) O.wzr
    else if is_immediate_float32 f
    then
      A.ins2 FMOV_scalar_immediate
        (H.reg_s i.res.(0))
        (O.imm_float (Int32.float_of_bits f))
    else
      (* float32 constants take up 8 bytes when we emit them with
         [float_literal] (see the conversion from int32 to int64 below). Thus,
         we load the lower half. Note that this is different from Cmm 32-bit
         floats ([Csingle]), which are emitted as 4-byte constants. *)
      let lbl = float32_literal f in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_float f) ->
    if Int64.equal f 0L
    then A.ins2 FMOV_gp_to_fp_64 (H.reg_d i.res.(0)) O.xzr
    else if is_immediate_float f
    then
      A.ins2 FMOV_scalar_immediate
        (H.reg_d i.res.(0))
        (O.imm_float (Int64.float_of_bits f))
    else
      let lbl = float_literal f in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_vec256 _ | Const_vec512 _) ->
    Misc.fatal_error "arm64: got 256/512 bit vector"
  | Lop (Const_vec128 ({ word0; word1 } as l)) -> (
    match word0, word1 with
    | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
      let dst = H.reg_v2d_operand i.res.(0) in
      A.ins2 MOVI dst (O.imm 0)
    | _ ->
      let lbl = vec128_literal l in
      emit_load_literal i.res.(0) lbl)
  | Lop (Const_symbol s) ->
    emit_load_symbol_addr i.res.(0) (symbol_of_cmm_symbol s)
  | Lcall_op Lcall_ind ->
    A.ins1 BLR (H.reg_x i.arg.(0));
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op (Lcall_imm { func }) ->
    A.ins1 BL (symbol (Needs_reloc CALL26) (symbol_of_cmm_symbol func));
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op Ltailcall_ind -> A.ins1 BR (H.reg_x i.arg.(0))
  | Lcall_op (Ltailcall_imm { func }) ->
    if String.equal func.sym_name !function_name
    then
      match !tailrec_entry_point with
      | None -> Misc.fatal_error "jump to missing tailrec entry point"
      | Some tailrec_entry_point -> A.ins1 B (local_label tailrec_entry_point)
    else A.ins1 B (symbol (Needs_reloc JUMP26) (symbol_of_cmm_symbol func))
  | Lcall_op (Lextcall { func; alloc; stack_ofs; _ }) ->
    if Config.runtime5 && stack_ofs > 0
    then (
      A.ins_mov_from_sp ~dst:reg_stack_arg_begin;
      A.ins4 ADD_immediate reg_stack_arg_end O.sp
        (O.imm (Misc.align stack_ofs 16))
        O.optional_none;
      emit_load_symbol_addr reg_x8 (S.create_global func);
      A.ins1 BL (runtime_function S.Predef.caml_c_call_stack_args);
      record_frame i.live (Dbg_other i.dbg))
    else if alloc
    then (
      emit_load_symbol_addr reg_x8 (S.create_global func);
      A.ins1 BL (runtime_function S.Predef.caml_c_call);
      record_frame i.live (Dbg_other i.dbg))
    else (
      (* Store OCaml stack pointer in the frame pointer register. No need to
         store previous x29 because OCaml doesn't maintain frame pointers. *)
      if Config.runtime5
      then (
        A.ins_mov_from_sp ~dst:O.fp;
        D.cfi_remember_state ();
        D.cfi_def_cfa_register ~reg:(Int.to_string (R.gp_encoding R.fp));
        A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_c_stack);
        A.ins_mov_to_sp ~src:reg_x_tmp1)
      else D.cfi_remember_state ();
      A.ins1 BL (symbol (Needs_reloc CALL26) (S.create_global func));
      if Config.runtime5 then A.ins_mov_to_sp ~src:O.fp;
      D.cfi_restore_state ())
  | Lop (Stackoffset n) ->
    assert (n mod 16 = 0);
    emit_stack_adjustment (-n);
    stack_offset := !stack_offset + n
  | Lop (Load { memory_chunk; addressing_mode; is_atomic; _ }) -> (
    assert (
      Cmm.equal_memory_chunk memory_chunk Word_int
      || Cmm.equal_memory_chunk memory_chunk Word_val
      || not is_atomic);
    let dst = i.res.(0) in
    let base =
      match addressing_mode with
      | Iindexed _ -> i.arg.(0)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP reg_x_tmp1
          (symbol_or_label_for_data ~offset:ofs (Needs_reloc PAGE) s);
        reg_tmp1
    in
    let addressing = H.addressing addressing_mode base in
    match memory_chunk with
    | Byte_unsigned -> A.ins2 LDRB (H.reg_w dst) addressing
    | Byte_signed -> A.ins2 LDRSB (H.reg_x dst) addressing
    | Sixteen_unsigned -> A.ins2 LDRH (H.reg_w dst) addressing
    | Sixteen_signed -> A.ins2 LDRSH (H.reg_x dst) addressing
    | Thirtytwo_unsigned -> A.ins2 LDR (H.reg_w dst) addressing
    | Thirtytwo_signed -> A.ins2 LDRSW (H.reg_x dst) addressing
    | Single { reg = Float64 } ->
      A.ins2 LDR_simd_and_fp reg_s7 addressing;
      A.ins2 FCVT (H.reg_d dst) reg_s7
    | Word_int | Word_val ->
      if is_atomic
      then (
        assert (
          match addressing_mode with
          | Iindexed v -> Validated_mem_offset.offset v = 0
          | Ibased _ -> false);
        A.ins0 (DMB ISHLD);
        A.ins2 LDAR (H.reg_x dst) (H.mem (H.gp_reg_of_reg i.arg.(0))))
      else A.ins2 LDR (H.reg_x dst) addressing
    | Double -> A.ins2 LDR_simd_and_fp (H.reg_d dst) addressing
    | Single { reg = Float32 } ->
      A.ins2 LDR_simd_and_fp (H.reg_s dst) addressing
    | Onetwentyeight_aligned -> A.ins2 LDR_simd_and_fp (H.reg_q dst) addressing
    | Onetwentyeight_unaligned ->
      (match addressing_mode with
      | Iindexed v ->
        let n = Validated_mem_offset.offset v in
        A.ins4 ADD_immediate reg_x_tmp1
          (H.reg_x i.arg.(0))
          (O.imm n) O.optional_none
      | Ibased (s, offset) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP reg_x_tmp1
          (symbol_or_label_for_data ~offset (Needs_reloc PAGE) s);
        A.ins4 ADD_immediate reg_x_tmp1 reg_x_tmp1
          (symbol_or_label_for_data ~offset (Needs_reloc LOWER_TWELVE) s)
          O.optional_none);
      A.ins2 LDR_simd_and_fp (H.reg_q dst) (H.mem reg_tmp1_base)
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector")
  | Lop (Store (size, addr, assignment)) -> (
    (* NB: assignments other than Word_int and Word_val do not follow the
       Multicore OCaml memory model and so do not emit a barrier *)
    let src = i.arg.(0) in
    let base =
      match addr with
      | Iindexed _ -> i.arg.(1)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        A.ins2 ADRP reg_x_tmp1
          (symbol_or_label_for_data ~offset:ofs (Needs_reloc PAGE) s);
        reg_tmp1
    in
    let addressing = H.addressing addr base in
    match size with
    | Byte_unsigned | Byte_signed -> A.ins2 STRB (H.reg_w src) addressing
    | Sixteen_unsigned | Sixteen_signed -> A.ins2 STRH (H.reg_w src) addressing
    | Thirtytwo_unsigned | Thirtytwo_signed ->
      A.ins2 STR (H.reg_w src) addressing
    | Single { reg = Float64 } ->
      A.ins2 FCVT reg_s7 (H.reg_d src);
      A.ins2 STR_simd_and_fp reg_s7 addressing
    | Word_int | Word_val ->
      (* memory model barrier for non-initializing store *)
      if assignment then A.ins0 (DMB ISHLD);
      A.ins2 STR (H.reg_x src) addressing
    | Double -> A.ins2 STR_simd_and_fp (H.reg_d src) addressing
    | Single { reg = Float32 } ->
      A.ins2 STR_simd_and_fp (H.reg_s src) addressing
    | Onetwentyeight_aligned -> A.ins2 STR_simd_and_fp (H.reg_q src) addressing
    | Onetwentyeight_unaligned -> (
      match addr with
      | Iindexed v ->
        let n = Validated_mem_offset.offset v in
        A.ins4 ADD_immediate reg_x_tmp1
          (H.reg_x i.arg.(1))
          (O.imm n) O.optional_none;
        A.ins2 STR_simd_and_fp (H.reg_q src) (H.mem reg_tmp1_base)
      | Ibased (s, offset) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        A.ins2 ADRP reg_x_tmp1
          (symbol_or_label_for_data ~offset (Needs_reloc PAGE) s);
        A.ins4 ADD_immediate reg_x_tmp1 reg_x_tmp1
          (symbol_or_label_for_data ~offset (Needs_reloc LOWER_TWELVE) s)
          O.optional_none;
        A.ins2 STR_simd_and_fp (H.reg_q src) (H.mem reg_tmp1_base))
    | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
    | Fivetwelve_unaligned ->
      Misc.fatal_error "arm64: got 256/512 bit vector")
  | Lop (Alloc { bytes = n; dbginfo; mode = Heap }) ->
    assembly_code_for_allocation i ~n ~local:false ~far:false ~dbginfo
  | Lop (Specific (Ifar_alloc { bytes = n; dbginfo })) ->
    assembly_code_for_allocation i ~n ~local:false ~far:true ~dbginfo
  | Lop (Alloc { bytes = n; dbginfo; mode = Local }) ->
    assembly_code_for_allocation i ~n ~local:true ~far:false ~dbginfo
  | Lop Begin_region ->
    A.ins2 LDR (H.reg_x i.res.(0)) (H.domainstate_field Domain_local_sp)
  | Lop End_region ->
    A.ins2 STR (H.reg_x i.arg.(0)) (H.domainstate_field Domain_local_sp)
  | Lop Poll -> assembly_code_for_poll i ~far:false ~return_label:None
  | Lop Pause -> A.ins0 YIELD
  | Lop (Specific Ifar_poll) ->
    assembly_code_for_poll i ~far:true ~return_label:None
  | Lop (Intop_imm (Iadd, n)) ->
    emit_addimm (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) n
  | Lop (Intop_imm (Isub, n)) ->
    emit_subimm (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) n
  | Lop (Intop (Icomp cmp)) ->
    A.ins_cmp_reg (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1)) O.optional_none;
    A.ins_cset (H.reg_x i.res.(0)) (cond_for_comparison cmp)
  | Lop (Floatop (Float64, Icompf cmp)) ->
    let cond = cond_for_cset_for_float_comparison cmp in
    A.ins2 FCMP (H.reg_d i.arg.(0)) (H.reg_d i.arg.(1));
    A.ins_cset (H.reg_x i.res.(0)) cond
  | Lop (Floatop (Float32, Icompf cmp)) ->
    let cond = cond_for_cset_for_float_comparison cmp in
    A.ins2 FCMP (H.reg_s i.arg.(0)) (H.reg_s i.arg.(1));
    A.ins_cset (H.reg_x i.res.(0)) cond
  | Lop (Intop_imm (Icomp cmp, n)) ->
    emit_cmpimm (H.reg_x i.arg.(0)) n;
    A.ins_cset (H.reg_x i.res.(0)) (cond_for_comparison cmp)
  | Lop (Intop Imod) ->
    A.ins3 SDIV reg_x_tmp1 (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1));
    A.ins4 MSUB
      (H.reg_x i.res.(0))
      reg_x_tmp1
      (H.reg_x i.arg.(1))
      (H.reg_x i.arg.(0))
  | Lop (Intop (Imulh { signed = true })) ->
    A.ins3 SMULH (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop (Imulh { signed = false })) ->
    A.ins3 UMULH (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Int128op _) ->
    (* CR mslater: restore after the arm DSL is merged *)
    Misc.fatal_error "arm64: got int128 op"
  | Lop (Intop Ipopcnt) ->
    if !Arch.feat_cssc
    then A.ins2 CNT (H.reg_x i.res.(0)) (H.reg_x i.arg.(0))
    else (
      A.ins2 FMOV_gp_to_fp_64 reg_d7 (H.reg_x i.arg.(0));
      A.ins2 CNT_vector reg_v8b_7 reg_v8b_7;
      A.ins2 ADDV reg_b7 reg_v8b_7;
      A.ins2 FMOV_fp_to_gp_64 (H.reg_x i.res.(0)) reg_d7)
  | Lop (Intop (Ictz _)) ->
    (* [ctz Rd, Rn] is optionally supported from Armv8.7, but rbit and clz are
       supported in all ARMv8 CPUs. *)
    if !Arch.feat_cssc
    then A.ins2 CTZ (H.reg_x i.res.(0)) (H.reg_x i.arg.(0))
    else (
      A.ins2 RBIT (H.reg_x i.res.(0)) (H.reg_x i.arg.(0));
      A.ins2 CLZ (H.reg_x i.res.(0)) (H.reg_x i.res.(0)))
  | Lop (Intop (Iclz _)) -> A.ins2 CLZ (H.reg_x i.res.(0)) (H.reg_x i.arg.(0))
  | Lop (Intop Iand) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    A.ins4 AND_shifted_register rd rn rm O.optional_none
  | Lop (Intop Ior) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    A.ins4 ORR_shifted_register rd rn rm O.optional_none
  | Lop (Intop Ixor) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    A.ins4 EOR_shifted_register rd rn rm O.optional_none
  | Lop (Intop Ilsl) ->
    A.ins3 LSLV (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop Ilsr) ->
    A.ins3 LSRV (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop Iasr) ->
    A.ins3 ASRV (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop Iadd) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    A.ins4 ADD_shifted_register rd rn rm O.optional_none
  | Lop (Intop Isub) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    A.ins4 SUB_shifted_register rd rn rm O.optional_none
  | Lop (Intop Imul) ->
    A.ins_mul (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop Idiv) ->
    A.ins3 SDIV (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1))
  | Lop (Intop_imm (Iand, n)) ->
    let rd, rn = H.reg_x i.res.(0), H.reg_x i.arg.(0) in
    A.ins3 AND_immediate rd rn (O.bitmask (Nativeint.of_int n))
  | Lop (Intop_imm (Ior, n)) ->
    let rd, rn = H.reg_x i.res.(0), H.reg_x i.arg.(0) in
    A.ins3 ORR_immediate rd rn (O.bitmask (Nativeint.of_int n))
  | Lop (Intop_imm (Ixor, n)) ->
    let rd, rn = H.reg_x i.res.(0), H.reg_x i.arg.(0) in
    A.ins3 EOR_immediate rd rn (O.bitmask (Nativeint.of_int n))
  | Lop (Intop_imm (Ilsl, shift_in_bits)) ->
    A.ins_lsl_immediate (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) ~shift_in_bits
  | Lop (Intop_imm (Ilsr, shift_in_bits)) ->
    A.ins_lsr_immediate (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) ~shift_in_bits
  | Lop (Intop_imm (Iasr, shift_in_bits)) ->
    A.ins_asr_immediate (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)) ~shift_in_bits
  | Lop
      (Intop_imm ((Imul | Idiv | Iclz _ | Ictz _ | Ipopcnt | Imod | Imulh _), _))
    ->
    Misc.fatal_errorf "emit_instr: immediate operand not supported for %a"
      Printlinear.instr i
  | Lop (Specific Isqrtf) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FSQRT rd rn
    | D_regs (rd, rn, _) -> A.ins2 FSQRT rd rn)
  | Lop (Floatop ((Float32 | Float64), Iabsf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FABS rd rn
    | D_regs (rd, rn, _) -> A.ins2 FABS rd rn)
  | Lop (Floatop ((Float32 | Float64), Inegf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(0) with
    | S_regs (rd, rn, _) -> A.ins2 FNEG rd rn
    | D_regs (rd, rn, _) -> A.ins2 FNEG rd rn)
  | Lop (Floatop ((Float32 | Float64), Iaddf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FADD rd rn rm
    | D_regs (rd, rn, rm) -> A.ins3 FADD rd rn rm)
  | Lop (Floatop ((Float32 | Float64), Isubf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FSUB rd rn rm
    | D_regs (rd, rn, rm) -> A.ins3 FSUB rd rn rm)
  | Lop (Floatop ((Float32 | Float64), Imulf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FMUL rd rn rm
    | D_regs (rd, rn, rm) -> A.ins3 FMUL rd rn rm)
  | Lop (Floatop ((Float32 | Float64), Idivf)) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FDIV rd rn rm
    | D_regs (rd, rn, rm) -> A.ins3 FDIV rd rn rm)
  | Lop (Specific Inegmulf) -> (
    match H.reg_fp_operand_3 i.res.(0) i.arg.(0) i.arg.(1) with
    | S_regs (rd, rn, rm) -> A.ins3 FNMUL rd rn rm
    | D_regs (rd, rn, rm) -> A.ins3 FNMUL rd rn rm)
  | Lop (Specific Imuladdf) -> (
    match H.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FMADD rd rn rm ra
    | D_regs (rd, rn, rm, ra) -> A.ins4 FMADD rd rn rm ra)
  | Lop (Specific Inegmuladdf) -> (
    match H.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FNMADD rd rn rm ra
    | D_regs (rd, rn, rm, ra) -> A.ins4 FNMADD rd rn rm ra)
  | Lop (Specific Imulsubf) -> (
    match H.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FMSUB rd rn rm ra
    | D_regs (rd, rn, rm, ra) -> A.ins4 FMSUB rd rn rm ra)
  | Lop (Specific Inegmulsubf) -> (
    match H.reg_fp_operand_4 i.res.(0) i.arg.(1) i.arg.(2) i.arg.(0) with
    | S_regs (rd, rn, rm, ra) -> A.ins4 FNMSUB rd rn rm ra
    | D_regs (rd, rn, rm, ra) -> A.ins4 FNMSUB rd rn rm ra)
  | Lop Opaque -> assert (Reg.equal_location i.arg.(0).loc i.res.(0).loc)
  | Lop (Specific (Ishiftarith (op, shift))) ->
    let rd, rn, rm = H.reg_x i.res.(0), H.reg_x i.arg.(0), H.reg_x i.arg.(1) in
    let emit_shift_arith instr kind amount =
      A.ins4 instr rd rn rm (O.optional_shift ~kind ~amount)
    in
    let instr : _ I.t =
      match op with
      | Ishiftadd -> ADD_shifted_register
      | Ishiftsub -> SUB_shifted_register
    in
    if shift >= 0
    then emit_shift_arith instr LSL shift
    else emit_shift_arith instr ASR (-shift)
  | Lop (Specific Imuladd) ->
    let rd, rn, rm, ra = i.res.(0), i.arg.(0), i.arg.(1), i.arg.(2) in
    A.ins4 MADD (H.reg_x rd) (H.reg_x rn) (H.reg_x rm) (H.reg_x ra)
  | Lop (Specific Imulsub) ->
    let rd, rn, rm, ra = i.res.(0), i.arg.(0), i.arg.(1), i.arg.(2) in
    A.ins4 MSUB (H.reg_x rd) (H.reg_x rn) (H.reg_x rm) (H.reg_x ra)
  | Lop (Specific (Ibswap { bitwidth })) -> (
    match bitwidth with
    | Sixteen ->
      let res_w = H.reg_w i.res.(0) in
      A.ins2 REV16 res_w (H.reg_w i.arg.(0));
      A.ins4 UBFM res_w res_w (O.imm_six 0) (O.imm_six 15)
    | Thirtytwo -> A.ins2 REV (H.reg_w i.res.(0)) (H.reg_w i.arg.(0))
    | Sixtyfour -> A.ins2 REV (H.reg_x i.res.(0)) (H.reg_x i.arg.(0)))
  | Lop (Specific (Isignext size)) ->
    let rd, rn = H.reg_x i.res.(0), H.reg_x i.arg.(0) in
    A.ins4 SBFM rd rn (O.imm_six 0) (O.imm_six (size - 1))
  | Lop (Specific (Isimd simd)) -> simd_instr simd i
  | Lop (Name_for_debugger _) -> ()
  | Lcall_op (Lprobe _) ->
    Misc.fatal_error "Optimized probes not supported on arm64."
  | Lop (Probe_is_enabled { name; enabled_at_init }) ->
    let semaphore_sym =
      Probe_emission.find_or_add_semaphore name enabled_at_init i.dbg
    in
    (* Load address of the semaphore symbol *)
    emit_load_symbol_addr reg_tmp1 (S.create_global semaphore_sym);
    (* Load unsigned 2-byte integer value from offset 2 *)
    A.ins2 LDRH
      (H.reg_w i.res.(0))
      (Validated_mem_offset.to_operand ~base:reg_tmp1_base
         Validated_mem_offset.probe_semaphore_offset);
    (* Compare with 0 and set result to 1 if non-zero, 0 if zero *)
    A.ins_cmp (H.reg_w i.res.(0)) (O.imm 0) O.optional_none;
    A.ins_cset (H.reg_x i.res.(0)) Cond.NE
  | Lop Dls_get when not Config.runtime5 ->
    Misc.fatal_error "Dls is not supported in runtime4."
  | Lop Dls_get ->
    A.ins2 LDR (H.reg_x i.res.(0)) (H.domainstate_field Domain_dls_state)
  | Lop Tls_get ->
    A.ins2 LDR (H.reg_x i.res.(0)) (H.domainstate_field Domain_tls_state)
  | Lop Domain_index ->
    if Config.runtime5
    then A.ins2 LDR (H.reg_x i.res.(0)) (H.domainstate_field Domain_id)
    else A.ins3 MOVZ (H.reg_x i.res.(0)) (O.imm_sixteen 0) O.optional_none
  | Lop (Csel tst) -> (
    let len = Array.length i.arg in
    let ifso = i.arg.(len - 2) in
    let ifnot = i.arg.(len - 1) in
    if Reg.same_loc ifso ifnot
    then move ifso i.res.(0)
    else
      let res_x = H.reg_x i.res.(0) in
      match tst with
      | Itruetest ->
        A.ins_cmp (H.reg_x i.arg.(0)) (O.imm 0) O.optional_none;
        A.ins4 CSEL res_x (H.reg_x i.arg.(1)) (H.reg_x i.arg.(2)) (O.cond NE)
      | Ifalsetest ->
        A.ins_cmp (H.reg_x i.arg.(0)) (O.imm 0) O.optional_none;
        A.ins4 CSEL res_x (H.reg_x i.arg.(1)) (H.reg_x i.arg.(2)) (O.cond EQ)
      | Iinttest cmp ->
        let cond = cond_for_comparison cmp in
        A.ins_cmp_reg (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1)) O.optional_none;
        A.ins4 CSEL res_x (H.reg_x i.arg.(2)) (H.reg_x i.arg.(3)) (O.cond cond)
      | Iinttest_imm (cmp, n) ->
        let cond = cond_for_comparison cmp in
        emit_cmpimm (H.reg_x i.arg.(0)) n;
        A.ins4 CSEL res_x (H.reg_x i.arg.(1)) (H.reg_x i.arg.(2)) (O.cond cond)
      | Ifloattest ((Float32 | Float64), cmp) ->
        let cond = cond_for_float_comparison cmp |> Cond.of_float_cond in
        (match H.reg_fp_operand_3 i.arg.(0) i.arg.(1) i.arg.(1) with
        | S_regs (rn, rm, _) -> A.ins2 FCMP rn rm
        | D_regs (rn, rm, _) -> A.ins2 FCMP rn rm);
        A.ins4 CSEL res_x (H.reg_x i.arg.(2)) (H.reg_x i.arg.(3)) (O.cond cond)
      | Ioddtest ->
        A.ins2 TST (H.reg_x i.arg.(0)) (O.bitmask 1n);
        A.ins4 CSEL res_x (H.reg_x i.arg.(1)) (H.reg_x i.arg.(2)) (O.cond NE)
      | Ieventest ->
        A.ins2 TST (H.reg_x i.arg.(0)) (O.bitmask 1n);
        A.ins4 CSEL res_x (H.reg_x i.arg.(1)) (H.reg_x i.arg.(2)) (O.cond EQ))
  | Lreloadretaddr -> ()
  | Lreturn -> A.ins0 RET
  | Llabel { label = lbl; _ } ->
    let lbl = label_to_asm_label ~section:Text lbl in
    D.define_label lbl
  | Lbranch lbl ->
    let lbl = label_to_asm_label ~section:Text lbl in
    A.ins1 B (local_label lbl)
  | Lcondbranch (tst, lbl) -> (
    let lbl = label_to_asm_label ~section:Text lbl |> local_label in
    match tst with
    | Itruetest -> A.ins2 CBNZ (H.reg_x i.arg.(0)) lbl
    | Ifalsetest -> A.ins2 CBZ (H.reg_x i.arg.(0)) lbl
    | Iinttest cmp ->
      A.ins_cmp_reg (H.reg_x i.arg.(0)) (H.reg_x i.arg.(1)) O.optional_none;
      A.ins1 (B_cond (Branch_cond.Int (cond_for_comparison cmp))) lbl
    | Iinttest_imm (cmp, n) ->
      emit_cmpimm (H.reg_x i.arg.(0)) n;
      A.ins1 (B_cond (Branch_cond.Int (cond_for_comparison cmp))) lbl
    | Ifloattest (Float64, cmp) ->
      A.ins2 FCMP (H.reg_d i.arg.(0)) (H.reg_d i.arg.(1));
      A.ins1 (B_cond (Branch_cond.Float (cond_for_float_comparison cmp))) lbl
    | Ifloattest (Float32, cmp) ->
      let cond = cond_for_float_comparison cmp in
      A.ins2 FCMP (H.reg_s i.arg.(0)) (H.reg_s i.arg.(1));
      A.ins1 (B_cond (Branch_cond.Float cond)) lbl
    | Ioddtest -> A.ins3 TBNZ (H.reg_x i.arg.(0)) (O.imm_six 0) lbl
    | Ieventest -> A.ins3 TBZ (H.reg_x i.arg.(0)) (O.imm_six 0) lbl)
  | Lcondbranch3 (lbl0, lbl1, lbl2) ->
    let section = Asm_targets.Asm_section.Text in
    let ins_cond cond lbl =
      Option.iter
        (fun lbl ->
          A.ins1 (B_cond (Branch_cond.Int cond))
            (local_label (label_to_asm_label ~section lbl)))
        lbl
    in
    A.ins_cmp (H.reg_x i.arg.(0)) (O.imm 1) O.optional_none;
    ins_cond LT lbl0;
    ins_cond EQ lbl1;
    ins_cond GT lbl2
  | Lswitch jumptbl ->
    let lbltbl = L.create Text in
    A.ins2 ADR reg_x_tmp1 (label Same_section_and_unit lbltbl);
    A.ins4 ADD_shifted_register reg_x_tmp1 reg_x_tmp1
      (H.reg_x i.arg.(0))
      (O.optional_shift ~kind:Ast.Operand.Shift.Kind.LSL ~amount:2);
    A.ins1 BR reg_x_tmp1;
    D.define_label lbltbl;
    for j = 0 to Array.length jumptbl - 1 do
      let jumplbl = label_to_asm_label ~section:Text jumptbl.(j) in
      A.ins1 B (local_label jumplbl)
    done
  (*= Alternative:
        let lbltbl = Cmm.new_label() in
        emit_printf "	adr	%a, %a\n" femit_reg reg_tmp1 femit_label lbltbl;
        emit_printf "	ldr	%a, [%a, %a, lsl #2]\n" femit_wreg reg_tmp2
          femit_reg reg_tmp1 femit_reg i.arg.(0);
        emit_printf "	add	%a, %a, sxtb\n" femit_reg reg_tmp1 femit_wreg
          reg_tmp2;
        emit_printf "	br	%a\n" femit_reg reg_tmp1;
        emit_printf "%a:\n" femit_label lbltbl;
        for j = 0 to Array.length jumptbl - 1 do
            emit_printf "	.4byte	%a - %a\n" femit_label jumptbl.(j)
              femit_label lbltbl
        done *)
  | Lentertrap -> ()
  | Ladjust_stack_offset { delta_bytes } ->
    D.cfi_adjust_cfa_offset ~bytes:delta_bytes;
    stack_offset := !stack_offset + delta_bytes
  | Lpushtrap { lbl_handler } ->
    let lbl_handler = label_to_asm_label ~section:Text lbl_handler in
    A.ins2 ADR reg_x_tmp1 (label Same_section_and_unit lbl_handler);
    stack_offset := !stack_offset + 16;
    A.ins3 (STP X) reg_x_trap_ptr reg_x_tmp1
      (O.mem_pre_pair ~base:R.sp ~offset:(-16));
    D.cfi_adjust_cfa_offset ~bytes:16;
    A.ins_mov_from_sp ~dst:reg_x_trap_ptr
  | Lpoptrap _ ->
    A.ins2 LDR reg_x_trap_ptr (O.mem_post ~base:R.sp ~offset:16);
    D.cfi_adjust_cfa_offset ~bytes:(-16);
    stack_offset := !stack_offset - 16
  | Lraise k -> (
    match k with
    | Raise_regular ->
      A.ins1 BL (runtime_function S.Predef.caml_raise_exn);
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Raise_reraise ->
      if Config.runtime5
      then A.ins1 BL (runtime_function S.Predef.caml_reraise_exn)
      else A.ins1 BL (runtime_function S.Predef.caml_raise_exn);
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Raise_notrace ->
      A.ins_mov_to_sp ~src:reg_x_trap_ptr;
      A.ins3 (LDP X) reg_x_trap_ptr reg_x_tmp1
        (O.mem_post_pair ~base:R.sp ~offset:16);
      A.ins1 BR reg_x_tmp1)
  | Lstackcheck { max_frame_size_bytes = sc_max_frame_size_in_bytes } ->
    let sc_label = L.create Text and sc_return = L.create Text in
    let threshold_offset =
      (Domainstate.stack_ctx_words * 8) + Stack_check.stack_threshold_size
    in
    let f = sc_max_frame_size_in_bytes + threshold_offset in
    A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_current_stack);
    emit_addimm reg_x_tmp1 reg_x_tmp1 f;
    A.ins_cmp_reg O.sp reg_x_tmp1 O.optional_none;
    A.ins1 (B_cond (Branch_cond.Int CC)) (local_label sc_label);
    D.define_label sc_return;
    stack_realloc := Some { sc_label; sc_return; sc_max_frame_size_in_bytes }
  | Lop (Specific (Illvm_intrinsic intr)) ->
    Misc.fatal_errorf
      "Emit: Unexpected llvm_intrinsic %s: not using LLVM backend" intr

let emit_instr i =
  try emit_instr i
  with exn ->
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn

(* Emission of an instruction sequence *)

let rec emit_all i =
  (* CR-soon xclerc for xclerc: get rid of polymorphic compare. *)
  if Stdlib.compare i.desc Lend = 0
  then ()
  else (
    emit_instr i;
    emit_all i.next)

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point
    := Option.map
         (label_to_asm_label ~section:Text)
         fundecl.fun_tailrec_entry_point_label;
  float_literals := [];
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_stack_realloc ();
  Stack_class.Tbl.copy_values ~from:fundecl.fun_num_stack_slots
    ~to_:num_stack_slots;
  prologue_required := fundecl.fun_prologue_required;
  contains_calls := fundecl.fun_contains_calls;
  emit_named_text_section !function_name;
  let fun_sym = S.create_global fundecl.fun_name in
  D.align ~fill_x86_bin_emitter:Nop ~bytes:8;
  global_maybe_protected fun_sym;
  D.type_symbol ~ty:Function fun_sym;
  (* Define both a symbol and a label so the function can be referenced either
     way. Local references use the label form to avoid ELF visibility issues. *)
  D.define_joint_label_and_symbol ~section:Text fun_sym;
  emit_debug_info fundecl.fun_dbg;
  D.cfi_startproc ();
  let num_call_gc = num_call_gc_points fundecl.fun_body in
  let max_out_of_line_code_offset = max_out_of_line_code_offset ~num_call_gc in
  BR.relax fundecl.fun_body ~max_out_of_line_code_offset;
  emit_all fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  List.iter emit_local_realloc !local_realloc_sites;
  emit_stack_realloc ();
  assert (List.length !call_gc_sites = num_call_gc);
  (match fun_end_label with
  | None -> ()
  | Some fun_end_label ->
    let fun_end_label = label_to_asm_label ~section:Text fun_end_label in
    D.define_label fun_end_label);
  D.cfi_endproc ();
  (* The type symbol and the size are system specific. They are not output on
     macOS. The asm directives take care of correctly handling this distinction.
     For the size, they automatically emit the size [. - symbol], meaning "this
     minus symbol definition". *)
  D.type_symbol ~ty:Function fun_sym;
  D.size fun_sym;
  emit_literals ()

(* Emission of data *)

let nativeint_to_int32 n =
  if
    Nativeint.compare n (Nativeint.of_int32 Int32.min_int) < 0
    || Nativeint.compare n (Nativeint.of_int32 Int32.max_int) > 0
  then Misc.fatal_errorf "nativeint_to_int32: value %nd out of int32 range" n;
  Nativeint.to_int32 n

(* CR sspies: Share the [emit_item] code with the x86 backend in emitaux. *)
let emit_item (d : Cmm.data_item) =
  match d with
  | Cdefine_symbol s -> (
    let sym = symbol_of_cmm_symbol s in
    match s.sym_global with
    | Local ->
      (* Use a label rather than a linker symbol for local definitions. This
         avoids visibility issues on ELF (symbols that "may bind externally"
         can't use PC-relative relocations in shared objects). *)
      D.define_label (L.create_string_unchecked Data (S.encode sym))
    | Global ->
      global_maybe_protected sym;
      (* Define both a label and a linker symbol, so the symbol can be
         referenced either way. This matches amd64 behaviour. *)
      D.define_joint_label_and_symbol ~section:Data sym)
  | Cint8 n -> D.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> D.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> D.int32 (nativeint_to_int32 n)
  (* CR mshinwell: Add [Targetint.of_nativeint] *)
  | Cint n -> D.targetint (Targetint.of_int64 (Int64.of_nativeint n))
  | Csingle f -> D.float32 f
  | Cdouble f -> D.float64 f
  | Cvec128 { word0; word1 } ->
    D.float64_from_bits word1;
    D.float64_from_bits word0
  | Cvec256 _ | Cvec512 _ -> Misc.fatal_error "arm64: got 256/512 bit vector"
  | Csymbol_address s -> (
    let sym = symbol_of_cmm_symbol s in
    match s.sym_global with
    | Global -> D.symbol sym
    | Local -> D.label (L.create_string_unchecked Data (S.encode sym)))
  | Csymbol_offset (s, o) -> (
    let sym = symbol_of_cmm_symbol s in
    match s.sym_global with
    | Global -> D.symbol_plus_offset ~offset_in_bytes:(Targetint.of_int o) sym
    | Local ->
      D.label_plus_offset ~offset_in_bytes:(Targetint.of_int o)
        (L.create_string_unchecked Data (S.encode sym)))
  | Cstring s -> D.string s
  | Cskip n -> D.space ~bytes:n
  | Calign n -> D.align ~fill_x86_bin_emitter:Zero ~bytes:n

let data l =
  D.data ();
  D.align ~fill_x86_bin_emitter:Zero ~bytes:8;
  List.iter emit_item l

let file_emitter ~file_num ~file_name =
  D.file ~file_num:(Some file_num) ~file_name

(* Beginning / end of an assembly file *)

let begin_assembly _unix =
  reset_debug_info ();
  Probe_emission.reset ();
  A.set_emit_string ~emit_string:Emitaux.emit_string;
  Asm_targets.Asm_label.initialize ~new_label:(fun () ->
      Cmm.new_label () |> Label.to_int);
  let asm_line_buffer = Buffer.create 200 in
  D.initialize ~big_endian:Arch.big_endian
    ~emit_assembly_comments:!Oxcaml_flags.dasm_comments ~emit:(fun d ->
      Buffer.clear asm_line_buffer;
      D.Directive.print asm_line_buffer d;
      Buffer.add_string asm_line_buffer "\n";
      Emitaux.emit_buffer asm_line_buffer);
  D.file ~file_num:None ~file_name:"";
  (* PR#7037 *)
  let data_begin = Cmm_helpers.make_symbol "data_begin" in
  let data_begin_sym = S.create_global data_begin in
  D.data ();
  global_maybe_protected data_begin_sym;
  D.define_symbol_label ~section:Data data_begin_sym;
  let code_begin = Cmm_helpers.make_symbol "code_begin" in
  let code_begin_sym = S.create_global code_begin in
  emit_named_text_section code_begin;
  global_maybe_protected code_begin_sym;
  D.define_symbol_label ~section:Text code_begin_sym;
  (* we need to pad here to avoid collision for the unwind test between the
     code_begin symbol and the first function. (See also #4690) Alignment is
     needed to avoid linker warnings for shared_startup__code_{begin,end} (e.g.
     tests/lib-dynlink-pr4839). *)
  if macosx
  then (
    A.ins0 NOP;
    D.align ~fill_x86_bin_emitter:Nop ~bytes:8);
  let code_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~code_begin ~code_end ~file_emitter

let end_assembly () =
  let code_end = Cmm_helpers.make_symbol "code_end" in
  let code_end_sym = S.create_global code_end in
  emit_named_text_section code_end;
  global_maybe_protected code_end_sym;
  D.define_symbol_label ~section:Text code_end_sym;
  let data_end = Cmm_helpers.make_symbol "data_end" in
  let data_end_sym = S.create_global data_end in
  D.data ();
  D.int64 0L;
  (* PR#6329 *)
  global_maybe_protected data_end_sym;
  D.define_symbol_label ~section:Data data_end_sym;
  D.int64 0L;
  D.align ~fill_x86_bin_emitter:Zero ~bytes:8;
  (* #7887 *)
  let frametable = Cmm_helpers.make_symbol "frametable" in
  let frametable_sym = S.create_global frametable in
  global_maybe_protected frametable_sym;
  D.define_symbol_label ~section:Data frametable_sym;
  (* CR sspies: Share the [emit_frames] code with the x86 backend. *)
  emit_frames
    { efa_code_label =
        (fun lbl ->
          let lbl = label_to_asm_label ~section:Text lbl in
          D.type_label ~ty:Function lbl;
          D.label lbl);
      efa_data_label =
        (fun lbl ->
          let lbl = label_to_asm_label ~section:Data lbl in
          D.type_label ~ty:Object lbl;
          D.label lbl);
      efa_i8 = (fun n -> D.int8 n);
      efa_i16 = (fun n -> D.int16 n);
      efa_i32 = (fun n -> D.int32 n);
      efa_u8 = (fun n -> D.uint8 n);
      efa_u16 = (fun n -> D.uint16 n);
      efa_u32 = (fun n -> D.uint32 n);
      efa_word = (fun n -> D.targetint (Targetint.of_int_exn n));
      efa_align = (fun n -> D.align ~fill_x86_bin_emitter:Zero ~bytes:n);
      efa_label_rel =
        (fun lbl ofs ->
          let lbl = label_to_asm_label ~section:Data lbl in
          D.between_this_and_label_offset_32bit_expr ~upper:lbl
            ~offset_upper:(Targetint.of_int32 ofs));
      efa_def_label =
        (fun lbl ->
          (* CR sspies: The frametable lives in the [.data] section on Arm, but
             in the [.text] section on x86. The frametable should move to the
             text section on Arm as well. *)
          let lbl = label_to_asm_label ~section:Data lbl in
          D.define_label lbl);
      efa_string = (fun s -> D.string (s ^ "\000"))
    };
  D.type_symbol ~ty:Object frametable_sym;
  D.size frametable_sym;
  if not !Oxcaml_flags.internal_assembler
  then Emitaux.Dwarf_helpers.emit_dwarf ();
  Probe_emission.emit_probe_notes ~slot_offset ~add_def_symbol:(fun _ -> ());
  D.mark_stack_non_executable ()
