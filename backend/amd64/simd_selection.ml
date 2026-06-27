(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

open! Int_replace_polymorphic_compare

(* SIMD instruction selection for AMD64.

   Some SIMD instructions have two encodings that exhibit identical behavior,
   but are intended for use on integer vs. float data (e.g. pxor vs xorpd). On
   some CPUs, it can be more efficient to forward the result of an integer
   instruction to further integer instructions, and likewise for floats.
   However, we ignore this distinction and only expose one variant. *)

open Arch
open Amd64_simd_instrs

type error = Bad_immediate of string

exception Error of error

module Seq = Simd.Seq

let cfg_operation simd args = Some (Operation.Specific (Isimd simd), args)

let instr instr ?i args = cfg_operation (Simd.instruction instr i) args

let seq seq ?i args = cfg_operation (Simd.sequence seq i) args

let sse_or_avx sse vex ?i args =
  let sse_or_avx = if Arch.Extension.enabled AVX then vex else sse in
  cfg_operation (Simd.instruction sse_or_avx i) args

let seq_or_avx sse vex ?i args =
  let seq = if Arch.Extension.enabled AVX then vex else sse in
  cfg_operation (Simd.sequence seq i) args

let seq_or_avx_zeroed ~dbg seq instr ?i args =
  if Arch.Extension.enabled AVX
  then
    cfg_operation (Simd.instruction instr i)
      (Cmm_helpers.vec128 ~dbg { word0 = 0L; word1 = 0L } :: args)
  else cfg_operation (Simd.sequence seq i) args

let simd_load ~mode instr args =
  Some
    ( Operation.Specific (Isimd_mem (Load (Simd.instruction instr None), mode)),
      args )

let simd_store ~mode instr args =
  Some
    ( Operation.Specific (Isimd_mem (Store (Simd.instruction instr None), mode)),
      args )

let simd_load_sse_or_avx ~mode sse vex args =
  let instr = if Arch.Extension.enabled AVX then vex else sse in
  simd_load ~mode instr args

let simd_store_sse_or_avx ~mode sse vex args =
  let instr = if Arch.Extension.enabled AVX then vex else sse in
  simd_store ~mode instr args

let bad_immediate fmt =
  Format.kasprintf (fun msg -> raise (Error (Bad_immediate msg))) fmt

(* Assumes untagged int *)
let extract_constant args name ~max =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < 0 || i > max
    then
      bad_immediate "Immediate for %s must be in range [0,%d] (got %d)" name max
        i;
    i, args
  | []
  | Cmm.(
      ( Cconst_float _
      | Cconst_natint (_, _)
      | Cconst_float32 (_, _)
      | Cconst_vec128 (_, _)
      | Cconst_vec256 (_, _)
      | Cconst_vec512 (_, _)
      | Cconst_mask (_, _)
      | Cconst_symbol (_, _)
      | Cvar _
      | Clet (_, _, _)
      | Cphantom_let (_, _, _)
      | Ctuple _
      | Cop (_, _, _)
      | Csequence (_, _)
      | Cifthenelse (_, _, _, _, _, _)
      | Cswitch (_, _, _, _)
      | Ccatch (_, _, _)
      | Cexit (_, _, _)
      | Cinvalid _ ))
    :: _ ->
    bad_immediate "Did not get integer immediate for %s" name

let extract_scale args name =
  let i, args = extract_constant args name ~max:8 in
  match i with
  | 1 | 2 | 4 | 8 -> i, args
  | _ -> bad_immediate "Did not get 1, 2, 4, or 8 as scale for %s" name

let int_of_float_rounding : X86_ast.rounding -> int = function
  | RoundNearest -> 0x8
  | RoundDown -> 0x9
  | RoundUp -> 0xA
  | RoundTruncate -> 0xB
  | RoundCurrent -> 0xC

let check_float_rounding = function
  (* Starts at 8, as these rounding modes also imply _MM_FROUND_NO_EXC (0x8) *)
  | 0x8 | 0x9 | 0xA | 0xB | 0xC -> ()
  | i -> bad_immediate "Invalid float rounding immediate: %d" i

let select_operation_clmul ~dbg:_ op args =
  if not (Arch.Extension.enabled CLMUL)
  then None
  else
    match op with
    | "caml_clmul_int64x2" ->
      let i, args = extract_constant args ~max:31 op in
      sse_or_avx pclmulqdq vpclmulqdq ~i args
    | _ -> None

let select_operation_popcnt ~dbg:_ op args =
  if not (Arch.Extension.enabled POPCNT)
  then None
  else
    match op with
    | "caml_popcnt_int16" -> instr popcnt_r16_r16m16 args
    | "caml_popcnt_int32" -> instr popcnt_r32_r32m32 args
    | "caml_popcnt_int64" -> instr popcnt_r64_r64m64 args
    | _ -> None

let select_operation_lzcnt ~dbg:_ op args =
  if not (Arch.Extension.enabled LZCNT)
  then None
  else
    match op with
    | "caml_lzcnt_int16" -> instr lzcnt_r16_r16m16 args
    | "caml_lzcnt_int32" -> instr lzcnt_r32_r32m32 args
    | "caml_lzcnt_int64" -> instr lzcnt_r64_r64m64 args
    | _ -> None

let select_operation_bmi ~dbg:_ op args =
  if not (Arch.Extension.enabled BMI)
  then None
  else
    match op with
    | "caml_bmi_andn_int32" -> instr andn_r32_r32_r32m32 args
    | "caml_bmi_andn_int64" -> instr andn_r64_r64_r64m64 args
    | "caml_bmi_bextr_int32" -> instr bextr_r32_r32m32_r32 args
    | "caml_bmi_bextr_int64" -> instr bextr_r64_r64m64_r64 args
    | "caml_bmi_blsi_int32" -> instr blsi_r32_r32m32 args
    | "caml_bmi_blsi_int64" -> instr blsi_r64_r64m64 args
    | "caml_bmi_blsmsk_int32" -> instr blsmsk_r32_r32m32 args
    | "caml_bmi_blsmsk_int64" -> instr blsmsk_r64_r64m64 args
    | "caml_bmi_blsr_int32" -> instr blsr_r32_r32m32 args
    | "caml_bmi_blsr_int64" -> instr blsr_r64_r64m64 args
    | "caml_bmi_tzcnt_int16" -> instr tzcnt_r16_r16m16 args
    | "caml_bmi_tzcnt_int32" -> instr tzcnt_r32_r32m32 args
    | "caml_bmi_tzcnt_int64" -> instr tzcnt_r64_r64m64 args
    | _ -> None

let select_operation_bmi2 ~dbg:_ op args =
  if not (Arch.Extension.enabled BMI2)
  then None
  else
    match op with
    | "caml_bmi2_bzhi_int32" -> instr bzhi_r32_r32m32_r32 args
    | "caml_bmi2_bzhi_int64" -> instr bzhi_r64_r64m64_r64 args
    | "caml_bmi2_mulx_int32" -> instr mulx_r32_r32_r32m32_rdx args
    | "caml_bmi2_mulx_int64" -> instr mulx_r64_r64_r64m64_rdx args
    | "caml_bmi2_pext_int32" -> instr pext_r32_r32_r32m32 args
    | "caml_bmi2_pext_int64" -> instr pext_r64_r64_r64m64 args
    | "caml_bmi2_pdep_int32" -> instr pdep_r32_r32_r32m32 args
    | "caml_bmi2_pdep_int64" -> instr pdep_r64_r64_r64m64 args
    | "caml_bmi2_rorx_int32" ->
      let i, args = extract_constant args ~max:31 op in
      instr rorx_r32_r32m32 ~i args
    | "caml_bmi2_rorx_int64" ->
      let i, args = extract_constant args ~max:63 op in
      instr rorx_r64_r64m64 ~i args
    | "caml_bmi2_sarx_int32" -> instr sarx_r32_r32m32_r32 args
    | "caml_bmi2_sarx_int64" -> instr sarx_r64_r64m64_r64 args
    | "caml_bmi2_shrx_int32" -> instr shrx_r32_r32m32_r32 args
    | "caml_bmi2_shrx_int64" -> instr shrx_r64_r64m64_r64 args
    | "caml_bmi2_shlx_int32" -> instr shlx_r32_r32m32_r32 args
    | "caml_bmi2_shlx_int64" -> instr shlx_r64_r64m64_r64 args
    | _ -> None

let select_operation_sse ~dbg op args =
  match op with
  | "caml_sse_vec128_load_aligned" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movapd_X_Xm128
      vmovapd_X_Xm128 args
  | "caml_sse_vec128_load_unaligned" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movupd_X_Xm128
      vmovupd_X_Xm128 args
  | "caml_sse_vec128_store_aligned" ->
    simd_store_sse_or_avx ~mode:Arch.identity_addressing movapd_m128_X
      vmovapd_m128_X args
  | "caml_sse_vec128_store_unaligned" ->
    simd_store_sse_or_avx ~mode:Arch.identity_addressing movupd_m128_X
      vmovupd_m128_X args
  | "caml_sse_vec128_store_aligned_uncached" ->
    simd_store_sse_or_avx ~mode:Arch.identity_addressing movntps vmovntps_m128_X
      args
  | "caml_sse_float32_sqrt" | "sqrtf" ->
    seq_or_avx_zeroed ~dbg Seq.sqrtss vsqrtss_X_X_Xm32 args
  | "caml_simd_float32_max" | "caml_sse_float32_max" ->
    sse_or_avx maxss vmaxss_X_X_Xm32 args
  | "caml_simd_float32_min" | "caml_sse_float32_min" ->
    sse_or_avx minss vminss_X_X_Xm32 args
  | "caml_simd_cast_float32_int64" | "caml_sse_cast_float32_int64" ->
    sse_or_avx cvtss2si_r64_Xm32 vcvtss2si_r64_Xm32 args
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args ~max:7 op in
    sse_or_avx cmpps vcmpps_X_X_Xm128 ~i args
  | "caml_sse_vec128_and" -> sse_or_avx andps vandps_X_X_Xm128 args
  | "caml_sse_vec128_andnot" -> sse_or_avx andnps vandnps_X_X_Xm128 args
  | "caml_sse_vec128_or" -> sse_or_avx orps vorps_X_X_Xm128 args
  | "caml_sse_vec128_xor" -> sse_or_avx xorps vxorps_X_X_Xm128 args
  | "caml_sse_float32x4_add" -> sse_or_avx addps vaddps_X_X_Xm128 args
  | "caml_sse_float32x4_sub" -> sse_or_avx subps vsubps_X_X_Xm128 args
  | "caml_sse_float32x4_mul" -> sse_or_avx mulps vmulps_X_X_Xm128 args
  | "caml_sse_float32x4_div" -> sse_or_avx divps vdivps_X_X_Xm128 args
  | "caml_sse_float32x4_max" -> sse_or_avx maxps vmaxps_X_X_Xm128 args
  | "caml_sse_float32x4_min" -> sse_or_avx minps vminps_X_X_Xm128 args
  | "caml_sse_float32x4_rcp" -> sse_or_avx rcpps vrcpps_X_Xm128 args
  | "caml_sse_float32x4_rsqrt" -> sse_or_avx rsqrtps vrsqrtps_X_Xm128 args
  | "caml_sse_float32x4_sqrt" -> sse_or_avx sqrtps vsqrtps_X_Xm128 args
  | "caml_sse_vec128_high_64_to_low_64" ->
    sse_or_avx movhlps vmovhlps_X_X_X args
  | "caml_sse_vec128_low_64_to_high_64" ->
    sse_or_avx movlhps vmovlhps_X_X_X args
  | "caml_sse_vec128_interleave_high_32" ->
    sse_or_avx unpckhps vunpckhps_X_X_Xm128 args
  | "caml_simd_vec128_interleave_low_32" | "caml_sse_vec128_interleave_low_32"
    ->
    sse_or_avx unpcklps vunpcklps_X_X_Xm128 args
  | "caml_sse_vec128_movemask_32" -> sse_or_avx movmskps vmovmskps_r64_X args
  | "caml_sse_vec128_shuffle_32" ->
    let i, args = extract_constant args ~max:255 op in
    sse_or_avx shufps vshufps_X_X_Xm128 ~i args
  | _ -> None

let select_operation_sse2 ~dbg op args =
  match op with
  | "caml_sse2_vec128_load_low64" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movq_X_r64m64
      vmovq_X_r64m64 args
  | "caml_sse2_vec128_load_low32" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movd_X_r32m32
      vmovd_X_r32m32 args
  | "caml_sse2_int32_store_uncached" ->
    simd_store ~mode:Arch.identity_addressing movnti_m32_r32 args
  | "caml_sse2_int64_store_uncached" ->
    simd_store ~mode:Arch.identity_addressing movnti_m64_r64 args
  | "caml_sse2_vec128_load_low64_copy_high64" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movlpd_X_m64
      vmovlpd_X_X_m64 args
  | "caml_sse2_vec128_load_high64_copy_low64" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movhpd_X_m64
      vmovhpd_X_X_m64 args
  | "caml_sse2_vec128_load_zero_low32" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movss_X_m32 vmovss_X_m32
      args
  | "caml_sse2_vec128_load_zero_low64" ->
    simd_load_sse_or_avx ~mode:Arch.identity_addressing movsd_X_m64 vmovsd_X_m64
      args
  | "caml_sse2_vec128_store_low32" ->
    simd_store_sse_or_avx ~mode:Arch.identity_addressing movss_m32_X
      vmovss_m32_X args
  | "caml_sse2_vec128_store_low64" ->
    simd_store_sse_or_avx ~mode:Arch.identity_addressing movsd_m64_X
      vmovsd_m64_X args
  | "caml_sse2_vec128_store_mask8" ->
    (* Does not have a mode; base address is always in rdi. *)
    sse_or_avx maskmovdqu vmaskmovdqu args
  | "caml_sse2_float64_sqrt" | "sqrt" ->
    seq_or_avx_zeroed ~dbg Seq.sqrtsd vsqrtsd_X_X_Xm64 args
  | "caml_simd_float64_max" | "caml_sse2_float64_max" ->
    sse_or_avx maxsd vmaxsd_X_X_Xm64 args
  | "caml_simd_float64_min" | "caml_sse2_float64_min" ->
    sse_or_avx minsd vminsd_X_X_Xm64 args
  | "caml_simd_cast_float64_int64" | "caml_sse2_cast_float64_int64" ->
    sse_or_avx cvtsd2si_r64_Xm64 vcvtsd2si_r64_Xm64 args
  | "caml_sse2_float64x2_sqrt" -> sse_or_avx sqrtpd vsqrtpd_X_Xm128 args
  | "caml_sse2_int8x16_add" -> sse_or_avx paddb vpaddb_X_X_Xm128 args
  | "caml_sse2_int16x8_add" -> sse_or_avx paddw vpaddw_X_X_Xm128 args
  | "caml_sse2_int32x4_add" -> sse_or_avx paddd vpaddd_X_X_Xm128 args
  | "caml_simd_int64x2_add" | "caml_sse2_int64x2_add" ->
    sse_or_avx paddq vpaddq_X_X_Xm128 args
  | "caml_sse2_float64x2_add" -> sse_or_avx addpd vaddpd_X_X_Xm128 args
  | "caml_sse2_int8x16_add_saturating" ->
    sse_or_avx paddsb vpaddsb_X_X_Xm128 args
  | "caml_sse2_int16x8_add_saturating" ->
    sse_or_avx paddsw vpaddsw_X_X_Xm128 args
  | "caml_sse2_int8x16_add_saturating_unsigned" ->
    sse_or_avx paddusb vpaddusb_X_X_Xm128 args
  | "caml_sse2_int16x8_add_saturating_unsigned" ->
    sse_or_avx paddusw vpaddusw_X_X_Xm128 args
  | "caml_sse2_int8x16_sub" -> sse_or_avx psubb vpsubb_X_X_Xm128 args
  | "caml_sse2_int16x8_sub" -> sse_or_avx psubw vpsubw_X_X_Xm128 args
  | "caml_sse2_int32x4_sub" -> sse_or_avx psubd vpsubd_X_X_Xm128 args
  | "caml_simd_int64x2_sub" | "caml_sse2_int64x2_sub" ->
    sse_or_avx psubq_X_Xm128 vpsubq_X_X_Xm128 args
  | "caml_sse2_float64x2_sub" -> sse_or_avx subpd vsubpd_X_X_Xm128 args
  | "caml_sse2_int8x16_sub_saturating" ->
    sse_or_avx psubsb vpsubsb_X_X_Xm128 args
  | "caml_sse2_int16x8_sub_saturating" ->
    sse_or_avx psubsw vpsubsw_X_X_Xm128 args
  | "caml_sse2_int8x16_sub_saturating_unsigned" ->
    sse_or_avx psubusb vpsubusb_X_X_Xm128 args
  | "caml_sse2_int16x8_sub_saturating_unsigned" ->
    sse_or_avx psubusw vpsubusw_X_X_Xm128 args
  | "caml_sse2_int8x16_max_unsigned" ->
    sse_or_avx pmaxub_X_Xm128 vpmaxub_X_X_Xm128 args
  | "caml_sse2_int16x8_max" -> sse_or_avx pmaxsw_X_Xm128 vpmaxsw_X_X_Xm128 args
  | "caml_sse2_float64x2_max" -> sse_or_avx maxpd vmaxpd_X_X_Xm128 args
  | "caml_sse2_int8x16_min_unsigned" ->
    sse_or_avx pminub_X_Xm128 vpminub_X_X_Xm128 args
  | "caml_sse2_int16x8_min" -> sse_or_avx pminsw_X_Xm128 vpminsw_X_X_Xm128 args
  | "caml_sse2_float64x2_min" -> sse_or_avx minpd vminpd_X_X_Xm128 args
  | "caml_sse2_float64x2_mul" -> sse_or_avx mulpd vmulpd_X_X_Xm128 args
  | "caml_sse2_float64x2_div" -> sse_or_avx divpd vdivpd_X_X_Xm128 args
  | "caml_sse2_vec128_movemask_8" ->
    sse_or_avx pmovmskb_r64_X vpmovmskb_r64_X args
  | "caml_sse2_vec128_movemask_64" -> sse_or_avx movmskpd vmovmskpd_r64_X args
  | "caml_sse2_vec128_shift_left_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    sse_or_avx pslldq vpslldq_X_X ~i args
  | "caml_sse2_vec128_shift_right_bytes" ->
    let i, args = extract_constant args ~max:15 op in
    sse_or_avx psrldq vpsrldq_X_X ~i args
  | "caml_sse2_int8x16_cmpeq" -> sse_or_avx pcmpeqb vpcmpeqb_X_X_Xm128 args
  | "caml_sse2_int16x8_cmpeq" -> sse_or_avx pcmpeqw vpcmpeqw_X_X_Xm128 args
  | "caml_sse2_int32x4_cmpeq" -> sse_or_avx pcmpeqd vpcmpeqd_X_X_Xm128 args
  | "caml_sse2_int8x16_cmpgt" -> sse_or_avx pcmpgtb vpcmpgtb_X_X_Xm128 args
  | "caml_sse2_int16x8_cmpgt" -> sse_or_avx pcmpgtw vpcmpgtw_X_X_Xm128 args
  | "caml_sse2_int32x4_cmpgt" -> sse_or_avx pcmpgtd vpcmpgtd_X_X_Xm128 args
  | "caml_sse2_float64x2_cmp" ->
    let i, args = extract_constant args ~max:31 op in
    sse_or_avx cmppd vcmppd_X_X_Xm128 ~i args
  | "caml_sse2_cvt_int32x4_float64x2" ->
    sse_or_avx cvtdq2pd vcvtdq2pd_X_Xm64 args
  | "caml_sse2_cvt_int32x4_float32x4" ->
    sse_or_avx cvtdq2ps vcvtdq2ps_X_Xm128 args
  | "caml_sse2_cvt_float64x2_int32x2" ->
    sse_or_avx cvtpd2dq vcvtpd2dq_X_Xm128 args
  | "caml_sse2_cvtt_float64x2_int32x2" ->
    sse_or_avx cvttpd2dq vcvttpd2dq_X_Xm128 args
  | "caml_sse2_cvt_float64x2_float32x2" ->
    sse_or_avx cvtpd2ps vcvtpd2ps_X_Xm128 args
  | "caml_sse2_cvt_float32x4_int32x4" ->
    sse_or_avx cvtps2dq vcvtps2dq_X_Xm128 args
  | "caml_sse2_cvtt_float32x4_int32x4" ->
    sse_or_avx cvttps2dq vcvttps2dq_X_Xm128 args
  | "caml_sse2_cvt_float32x4_float64x2" ->
    sse_or_avx cvtps2pd vcvtps2pd_X_Xm64 args
  | "caml_sse2_cvt_int16x8_int8x16_saturating" ->
    sse_or_avx packsswb vpacksswb_X_X_Xm128 args
  | "caml_sse2_cvt_int32x4_int16x8_saturating" ->
    sse_or_avx packssdw vpackssdw_X_X_Xm128 args
  | "caml_sse2_cvt_int16x8_int8x16_saturating_unsigned" ->
    sse_or_avx packuswb vpackuswb_X_X_Xm128 args
  | "caml_sse2_cvt_int32x4_int16x8_saturating_unsigned" ->
    sse_or_avx packusdw vpackusdw_X_X_Xm128 args
  | "caml_sse2_int8x16_avg_unsigned" ->
    sse_or_avx pavgb_X_Xm128 vpavgb_X_X_Xm128 args
  | "caml_sse2_int16x8_avg_unsigned" ->
    sse_or_avx pavgw_X_Xm128 vpavgw_X_X_Xm128 args
  | "caml_sse2_int8x16_sad_unsigned" ->
    sse_or_avx psadbw_X_Xm128 vpsadbw_X_X_Xm128 args
  | "caml_sse2_int16x8_sll" -> sse_or_avx psllw_X_Xm128 vpsllw_X_X_Xm128 args
  | "caml_sse2_int32x4_sll" -> sse_or_avx pslld_X_Xm128 vpslld_X_X_Xm128 args
  | "caml_sse2_int64x2_sll" -> sse_or_avx psllq_X_Xm128 vpsllq_X_X_Xm128 args
  | "caml_sse2_int16x8_srl" -> sse_or_avx psrlw_X_Xm128 vpsrlw_X_X_Xm128 args
  | "caml_sse2_int32x4_srl" -> sse_or_avx psrld_X_Xm128 vpsrld_X_X_Xm128 args
  | "caml_sse2_int64x2_srl" -> sse_or_avx psrlq_X_Xm128 vpsrlq_X_X_Xm128 args
  | "caml_sse2_int16x8_sra" -> sse_or_avx psraw_X_Xm128 vpsraw_X_X_Xm128 args
  | "caml_sse2_int32x4_sra" -> sse_or_avx psrad_X_Xm128 vpsrad_X_X_Xm128 args
  | "caml_sse2_int16x8_slli" ->
    let i, args = extract_constant args ~max:15 op in
    sse_or_avx psllw_X vpsllw_X_X ~i args
  | "caml_sse2_int32x4_slli" ->
    let i, args = extract_constant args ~max:31 op in
    sse_or_avx pslld_X vpslld_X_X ~i args
  | "caml_sse2_int64x2_slli" ->
    let i, args = extract_constant args ~max:63 op in
    sse_or_avx psllq_X vpsllq_X_X ~i args
  | "caml_sse2_int16x8_srli" ->
    let i, args = extract_constant args ~max:15 op in
    sse_or_avx psrlw_X vpsrlw_X_X ~i args
  | "caml_sse2_int32x4_srli" ->
    let i, args = extract_constant args ~max:31 op in
    sse_or_avx psrld_X vpsrld_X_X ~i args
  | "caml_sse2_int64x2_srli" ->
    let i, args = extract_constant args ~max:63 op in
    sse_or_avx psrlq_X vpsrlq_X_X ~i args
  | "caml_sse2_int16x8_srai" ->
    let i, args = extract_constant args ~max:15 op in
    sse_or_avx psraw_X vpsraw_X_X ~i args
  | "caml_sse2_int32x4_srai" ->
    let i, args = extract_constant args ~max:31 op in
    sse_or_avx psrad_X vpsrad_X_X ~i args
  | "caml_sse2_vec128_shuffle_64" ->
    let i, args = extract_constant args ~max:3 op in
    sse_or_avx shufpd vshufpd_X_X_Xm128 ~i args
  | "caml_sse2_vec128_shuffle_high_16" ->
    let i, args = extract_constant args ~max:255 op in
    sse_or_avx pshufhw vpshufhw_X_Xm128 ~i args
  | "caml_sse2_vec128_shuffle_low_16" ->
    let i, args = extract_constant args ~max:255 op in
    sse_or_avx pshuflw vpshuflw_X_Xm128 ~i args
  | "caml_sse2_vec128_interleave_high_8" ->
    sse_or_avx punpckhbw vpunpckhbw_X_X_Xm128 args
  | "caml_sse2_vec128_interleave_low_8" ->
    sse_or_avx punpcklbw vpunpcklbw_X_X_Xm128 args
  | "caml_sse2_vec128_interleave_high_16" ->
    sse_or_avx punpckhwd vpunpckhwd_X_X_Xm128 args
  | "caml_sse2_vec128_interleave_low_16" ->
    sse_or_avx punpcklwd vpunpcklwd_X_X_Xm128 args
  | "caml_simd_vec128_interleave_high_64"
  | "caml_sse2_vec128_interleave_high_64" ->
    sse_or_avx punpckhqdq vpunpckhqdq_X_X_Xm128 args
  | "caml_simd_vec128_interleave_low_64" | "caml_sse2_vec128_interleave_low_64"
    ->
    sse_or_avx punpcklqdq vpunpcklqdq_X_X_Xm128 args
  | "caml_sse2_int16x8_mul_high" -> sse_or_avx pmulhw vpmulhw_X_X_Xm128 args
  | "caml_sse2_int16x8_mul_high_unsigned" ->
    sse_or_avx pmulhuw_X_Xm128 vpmulhuw_X_X_Xm128 args
  | "caml_sse2_int16x8_mul_low" -> sse_or_avx pmullw vpmullw_X_X_Xm128 args
  | "caml_sse2_int16x8_mul_hadd_int32x4" ->
    sse_or_avx pmaddwd vpmaddwd_X_X_Xm128 args
  | "caml_sse2_int32x4_mul_even_unsigned" ->
    sse_or_avx pmuludq_X_Xm128 vpmuludq_X_X_Xm128 args
  | _ -> None

let select_operation_sse3 ~dbg:_ op args =
  if not (Arch.Extension.enabled SSE3)
  then None
  else
    match op with
    | "caml_sse3_vec128_load_known_unaligned" ->
      simd_load_sse_or_avx ~mode:Arch.identity_addressing lddqu vlddqu_X_m128
        args
    | "caml_sse3_vec128_load_broadcast64" ->
      simd_load_sse_or_avx ~mode:Arch.identity_addressing movddup
        vmovddup_X_Xm64 args
    | "caml_sse3_float32x4_addsub" ->
      sse_or_avx addsubps vaddsubps_X_X_Xm128 args
    | "caml_sse3_float64x2_addsub" ->
      sse_or_avx addsubpd vaddsubpd_X_X_Xm128 args
    | "caml_sse3_float32x4_hadd" -> sse_or_avx haddps vhaddps_X_X_Xm128 args
    | "caml_sse3_float64x2_hadd" -> sse_or_avx haddpd vhaddpd_X_X_Xm128 args
    | "caml_sse3_float32x4_hsub" -> sse_or_avx hsubps vhsubps_X_X_Xm128 args
    | "caml_sse3_float64x2_hsub" -> sse_or_avx hsubpd vhsubpd_X_X_Xm128 args
    | "caml_sse3_vec128_dup_low_64" -> sse_or_avx movddup vmovddup_X_Xm64 args
    | "caml_sse3_vec128_dup_odd_32" ->
      sse_or_avx movshdup vmovshdup_X_Xm128 args
    | "caml_sse3_vec128_dup_even_32" ->
      sse_or_avx movsldup vmovsldup_X_Xm128 args
    | _ -> None

let select_operation_ssse3 ~dbg:_ op args =
  if not (Arch.Extension.enabled SSSE3)
  then None
  else
    match op with
    | "caml_ssse3_int8x16_abs" -> sse_or_avx pabsb_X_Xm128 vpabsb_X_Xm128 args
    | "caml_ssse3_int16x8_abs" -> sse_or_avx pabsw_X_Xm128 vpabsw_X_Xm128 args
    | "caml_ssse3_int32x4_abs" -> sse_or_avx pabsd_X_Xm128 vpabsd_X_Xm128 args
    | "caml_ssse3_int16x8_hadd" ->
      sse_or_avx phaddw_X_Xm128 vphaddw_X_X_Xm128 args
    | "caml_ssse3_int32x4_hadd" ->
      sse_or_avx phaddd_X_Xm128 vphaddd_X_X_Xm128 args
    | "caml_ssse3_int16x8_hadd_saturating" ->
      sse_or_avx phaddsw_X_Xm128 vphaddsw_X_X_Xm128 args
    | "caml_ssse3_int16x8_hsub" ->
      sse_or_avx phsubw_X_Xm128 vphsubw_X_X_Xm128 args
    | "caml_ssse3_int32x4_hsub" ->
      sse_or_avx phsubd_X_Xm128 vphsubd_X_X_Xm128 args
    | "caml_ssse3_int16x8_hsub_saturating" ->
      sse_or_avx phsubsw_X_Xm128 vphsubsw_X_X_Xm128 args
    | "caml_ssse3_int8x16_mulsign" ->
      sse_or_avx psignb_X_Xm128 vpsignb_X_X_Xm128 args
    | "caml_ssse3_int16x8_mulsign" ->
      sse_or_avx psignw_X_Xm128 vpsignw_X_X_Xm128 args
    | "caml_ssse3_int32x4_mulsign" ->
      sse_or_avx psignd_X_Xm128 vpsignd_X_X_Xm128 args
    | "caml_ssse3_vec128_shuffle_8" ->
      sse_or_avx pshufb_X_Xm128 vpshufb_X_X_Xm128 args
    | "caml_ssse3_vec128_align_right_bytes" ->
      let i, args = extract_constant args ~max:31 op in
      sse_or_avx palignr_X_Xm128 vpalignr_X_X_Xm128 ~i args
    | "caml_ssse3_int8x16_mul_unsigned_hadd_saturating_int16x8" ->
      sse_or_avx pmaddubsw_X_Xm128 vpmaddubsw_X_X_Xm128 args
    | "caml_ssse3_int16x8_mul_round" ->
      sse_or_avx pmulhrsw_X_Xm128 vpmulhrsw_X_X_Xm128 args
    | _ -> None

let select_operation_sse41 ~dbg op args =
  if not (Arch.Extension.enabled SSE4_1)
  then None
  else
    match op with
    | "caml_sse41_vec128_load_aligned_uncached" ->
      simd_load_sse_or_avx ~mode:Arch.identity_addressing movntdqa
        vmovntdqa_X_m128 args
    | "caml_sse41_vec128_blend_16" ->
      let i, args = extract_constant args ~max:255 op in
      sse_or_avx pblendw vpblendw_X_X_Xm128 ~i args
    | "caml_sse41_vec128_blend_32" ->
      let i, args = extract_constant args ~max:15 op in
      sse_or_avx blendps vblendps_X_X_Xm128 ~i args
    | "caml_sse41_vec128_blend_64" ->
      let i, args = extract_constant args ~max:3 op in
      sse_or_avx blendpd vblendpd_X_X_Xm128 ~i args
    | "caml_sse41_vec128_blendv_8" ->
      sse_or_avx pblendvb vpblendvb_X_X_Xm128_X args
    | "caml_sse41_vec128_blendv_32" ->
      sse_or_avx blendvps vblendvps_X_X_Xm128_X args
    | "caml_sse41_vec128_blendv_64" ->
      sse_or_avx blendvpd vblendvpd_X_X_Xm128_X args
    | "caml_sse41_int64x2_cmpeq" -> sse_or_avx pcmpeqq vpcmpeqq_X_X_Xm128 args
    | "caml_sse41_cvtsx_int8x16_int16x8" ->
      sse_or_avx pmovsxbw vpmovsxbw_X_Xm64 args
    | "caml_sse41_cvtsx_int8x16_int32x4" ->
      sse_or_avx pmovsxbd vpmovsxbd_X_Xm32 args
    | "caml_sse41_cvtsx_int8x16_int64x2" ->
      sse_or_avx pmovsxbq vpmovsxbq_X_Xm16 args
    | "caml_sse41_cvtsx_int16x8_int32x4" ->
      sse_or_avx pmovsxwd vpmovsxwd_X_Xm64 args
    | "caml_sse41_cvtsx_int16x8_int64x2" ->
      sse_or_avx pmovsxwq vpmovsxwq_X_Xm32 args
    | "caml_sse41_cvtsx_int32x4_int64x2" ->
      sse_or_avx pmovsxdq vpmovsxdq_X_Xm64 args
    | "caml_sse41_cvtzx_int8x16_int16x8" ->
      sse_or_avx pmovzxbw vpmovzxbw_X_Xm64 args
    | "caml_sse41_cvtzx_int8x16_int32x4" ->
      sse_or_avx pmovzxbd vpmovzxbd_X_Xm32 args
    | "caml_sse41_cvtzx_int8x16_int64x2" ->
      sse_or_avx pmovzxbq vpmovzxbq_X_Xm16 args
    | "caml_sse41_cvtzx_int16x8_int32x4" ->
      sse_or_avx pmovzxwd vpmovzxwd_X_Xm64 args
    | "caml_sse41_cvtzx_int16x8_int64x2" ->
      sse_or_avx pmovzxwq vpmovzxwq_X_Xm32 args
    | "caml_sse41_cvtzx_int32x4_int64x2" ->
      sse_or_avx pmovzxdq vpmovzxdq_X_Xm64 args
    | "caml_sse41_float32x4_dp" ->
      let i, args = extract_constant args ~max:255 op in
      sse_or_avx dpps vdpps_X_X_Xm128 ~i args
    | "caml_sse41_float64x2_dp" ->
      let i, args = extract_constant args ~max:255 op in
      sse_or_avx dppd vdppd ~i args
    | "caml_sse41_int8x16_extract" ->
      let i, args = extract_constant args ~max:15 op in
      sse_or_avx pextrb vpextrb_r64m8_X ~i args
    | "caml_sse41_int16x8_extract" ->
      let i, args = extract_constant args ~max:7 op in
      sse_or_avx pextrw_r64m16_X vpextrw_r64m16_X ~i args
    | "caml_sse41_int32x4_extract" ->
      let i, args = extract_constant args ~max:3 op in
      sse_or_avx pextrd vpextrd_r32m32_X ~i args
    | "caml_sse41_int64x2_extract" ->
      let i, args = extract_constant args ~max:1 op in
      sse_or_avx pextrq vpextrq_r64m64_X ~i args
    | "caml_sse41_int8x16_insert" ->
      let i, args = extract_constant args ~max:15 op in
      sse_or_avx pinsrb vpinsrb_X_X_r32m8 ~i args
    | "caml_sse41_int16x8_insert" ->
      let i, args = extract_constant args ~max:7 op in
      sse_or_avx pinsrw_X_r32m16 vpinsrw_X_X_r32m16 ~i args
    | "caml_sse41_int32x4_insert" ->
      let i, args = extract_constant args ~max:3 op in
      sse_or_avx pinsrd vpinsrd_X_X_r32m32 ~i args
    | "caml_sse41_int64x2_insert" ->
      let i, args = extract_constant args ~max:1 op in
      sse_or_avx pinsrq vpinsrq_X_X_r64m64 ~i args
    | "caml_sse41_float32x4_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      sse_or_avx roundps vroundps_X_Xm128 ~i args
    | "caml_sse41_float64x2_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      sse_or_avx roundpd vroundpd_X_Xm128 ~i args
    | "caml_sse41_float64_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd ~i args
    | "caml_simd_float64_round_current" | "caml_sse41_float64_round_current" ->
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd
        ~i:(int_of_float_rounding RoundCurrent)
        args
    | "caml_simd_float64_round_neg_inf" | "caml_sse41_float64_round_neg_inf" ->
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd
        ~i:(int_of_float_rounding RoundDown)
        args
    | "caml_simd_float64_round_pos_inf" | "caml_sse41_float64_round_pos_inf" ->
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd
        ~i:(int_of_float_rounding RoundUp)
        args
    | "caml_simd_float64_round_towards_zero"
    | "caml_sse41_float64_round_towards_zero" ->
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd
        ~i:(int_of_float_rounding RoundTruncate)
        args
    | "caml_sse41_float32_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss ~i args
    | "caml_simd_float32_round_current" | "caml_sse41_float32_round_current" ->
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss
        ~i:(int_of_float_rounding RoundCurrent)
        args
    | "caml_simd_float32_round_neg_inf" | "caml_sse41_float32_round_neg_inf" ->
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss
        ~i:(int_of_float_rounding RoundDown)
        args
    | "caml_simd_float32_round_pos_inf" | "caml_sse41_float32_round_pos_inf" ->
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss
        ~i:(int_of_float_rounding RoundUp)
        args
    | "caml_simd_float32_round_towards_zero"
    | "caml_sse41_float32_round_towards_zero" ->
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss
        ~i:(int_of_float_rounding RoundTruncate)
        args
    | "caml_sse41_int8x16_max" -> sse_or_avx pmaxsb vpmaxsb_X_X_Xm128 args
    | "caml_sse41_int32x4_max" -> sse_or_avx pmaxsd vpmaxsd_X_X_Xm128 args
    | "caml_sse41_int16x8_max_unsigned" ->
      sse_or_avx pmaxuw vpmaxuw_X_X_Xm128 args
    | "caml_sse41_int32x4_max_unsigned" ->
      sse_or_avx pmaxud vpmaxud_X_X_Xm128 args
    | "caml_sse41_int8x16_min" -> sse_or_avx pminsb vpminsb_X_X_Xm128 args
    | "caml_sse41_int32x4_min" -> sse_or_avx pminsd vpminsd_X_X_Xm128 args
    | "caml_sse41_int16x8_min_unsigned" ->
      sse_or_avx pminuw vpminuw_X_X_Xm128 args
    | "caml_sse41_int32x4_min_unsigned" ->
      sse_or_avx pminud vpminud_X_X_Xm128 args
    | "caml_sse41_int8x16_multi_sad_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      sse_or_avx mpsadbw vmpsadbw_X_X_Xm128 ~i args
    | "caml_sse41_int16x8_minpos_unsigned" ->
      sse_or_avx phminposuw vphminposuw args
    | "caml_sse41_int32x4_mul_even" -> sse_or_avx pmuldq vpmuldq_X_X_Xm128 args
    | "caml_sse41_int32x4_mul_low" -> sse_or_avx pmulld vpmulld_X_X_Xm128 args
    | "caml_sse41_vec128_testz" -> seq_or_avx Seq.ptestz Seq.vptestz_X args
    | "caml_sse41_vec128_testc" -> seq_or_avx Seq.ptestc Seq.vptestc_X args
    | "caml_sse41_vec128_testnzc" ->
      seq_or_avx Seq.ptestnzc Seq.vptestnzc_X args
    | _ -> None

let select_operation_sse42 ~dbg:_ op args =
  if not (Arch.Extension.enabled SSE4_2)
  then None
  else
    match op with
    | "caml_sse42_int64x2_cmpgt" -> sse_or_avx pcmpgtq vpcmpgtq_X_X_Xm128 args
    | "caml_sse42_int64_crc" | "caml_sse42_int_untagged_crc" ->
      sse_or_avx crc32_r64_r64m64 crc32_r64_r64m64 args
    | "caml_sse42_vec128_cmpestrm" ->
      let i, args = extract_constant args ~max:127 op in
      sse_or_avx pcmpestrm vpcmpestrm ~i args
    | "caml_sse42_vec128_cmpestra" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpestra Seq.vpcmpestra ~i args
    | "caml_sse42_vec128_cmpestrc" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpestrc Seq.vpcmpestrc ~i args
    | "caml_sse42_vec128_cmpestri" ->
      let i, args = extract_constant args ~max:127 op in
      sse_or_avx pcmpestri vpcmpestri ~i args
    | "caml_sse42_vec128_cmpestro" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpestro Seq.vpcmpestro ~i args
    | "caml_sse42_vec128_cmpestrs" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpestrs Seq.vpcmpestrs ~i args
    | "caml_sse42_vec128_cmpestrz" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpestrz Seq.vpcmpestrz ~i args
    | "caml_sse42_vec128_cmpistrm" ->
      let i, args = extract_constant args ~max:127 op in
      sse_or_avx pcmpistrm vpcmpistrm ~i args
    | "caml_sse42_vec128_cmpistra" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpistra Seq.vpcmpistra ~i args
    | "caml_sse42_vec128_cmpistrc" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpistrc Seq.vpcmpistrc ~i args
    | "caml_sse42_vec128_cmpistri" ->
      let i, args = extract_constant args ~max:127 op in
      sse_or_avx pcmpistri vpcmpistri ~i args
    | "caml_sse42_vec128_cmpistro" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpistro Seq.vpcmpistro ~i args
    | "caml_sse42_vec128_cmpistrs" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpistrs Seq.vpcmpistrs ~i args
    | "caml_sse42_vec128_cmpistrz" ->
      let i, args = extract_constant args ~max:127 op in
      seq_or_avx Seq.pcmpistrz Seq.vpcmpistrz ~i args
    | _ -> None

let select_operation_avx ~dbg:_ op args =
  if not (Arch.Extension.enabled AVX)
  then None
  else
    match op with
    | "caml_avx_vec256_load_aligned" ->
      simd_load ~mode:Arch.identity_addressing vmovapd_Y_Ym256 args
    | "caml_avx_vec256_load_unaligned" ->
      simd_load ~mode:Arch.identity_addressing vmovupd_Y_Ym256 args
    | "caml_avx_vec256_load_known_unaligned" ->
      simd_load ~mode:Arch.identity_addressing vlddqu_Y_m256 args
    | "caml_avx_vec256_store_aligned" ->
      simd_store ~mode:Arch.identity_addressing vmovapd_m256_Y args
    | "caml_avx_vec256_store_unaligned" ->
      simd_store ~mode:Arch.identity_addressing vmovupd_m256_Y args
    | "caml_avx_vec256_load_aligned_uncached" ->
      simd_load ~mode:Arch.identity_addressing vmovntdqa_Y_m256 args
    | "caml_avx_vec256_store_aligned_uncached" ->
      simd_store ~mode:Arch.identity_addressing vmovntps_m256_Y args
    | "caml_avx_vec256_load_broadcast128" ->
      simd_load ~mode:Arch.identity_addressing vbroadcastf128 args
    | "caml_avx_vec256_load_broadcast64" ->
      simd_load ~mode:Arch.identity_addressing vbroadcastsd_Y_m64 args
    | "caml_avx_vec256_load_broadcast32" ->
      simd_load ~mode:Arch.identity_addressing vbroadcastss_Y_m32 args
    | "caml_avx_vec128_load_broadcast32" ->
      simd_load ~mode:Arch.identity_addressing vbroadcastss_X_m32 args
    | "caml_avx_vec128_load_mask64" ->
      simd_load ~mode:Arch.identity_addressing vmaskmovpd_X_X_m128 args
    | "caml_avx_vec256_load_mask64" ->
      simd_load ~mode:Arch.identity_addressing vmaskmovpd_Y_Y_m256 args
    | "caml_avx_vec128_load_mask32" ->
      simd_load ~mode:Arch.identity_addressing vmaskmovps_X_X_m128 args
    | "caml_avx_vec256_load_mask32" ->
      simd_load ~mode:Arch.identity_addressing vmaskmovps_Y_Y_m256 args
    | "caml_avx_vec128_store_mask64" ->
      simd_store ~mode:Arch.identity_addressing vmaskmovpd_m128_X_X args
    | "caml_avx_vec256_store_mask64" ->
      simd_store ~mode:Arch.identity_addressing vmaskmovpd_m256_Y_Y args
    | "caml_avx_vec128_store_mask32" ->
      simd_store ~mode:Arch.identity_addressing vmaskmovps_m128_X_X args
    | "caml_avx_vec256_store_mask32" ->
      simd_store ~mode:Arch.identity_addressing vmaskmovps_m256_Y_Y args
    | "caml_avx_float64x4_add" -> instr vaddpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_add" -> instr vaddps_Y_Y_Ym256 args
    | "caml_avx_float32x8_addsub" -> instr vaddsubps_Y_Y_Ym256 args
    | "caml_avx_float64x4_addsub" -> instr vaddsubpd_Y_Y_Ym256 args
    | "caml_avx_vec256_and" -> instr vandps_Y_Y_Ym256 args
    | "caml_avx_vec256_andnot" -> instr vandnps_Y_Y_Ym256 args
    | "caml_avx_vec256_blend_64" ->
      let i, args = extract_constant args ~max:15 op in
      instr vblendpd_Y_Y_Ym256 ~i args
    | "caml_avx_vec256_blend_32" ->
      let i, args = extract_constant args ~max:255 op in
      instr vblendps_Y_Y_Ym256 ~i args
    | "caml_avx_vec256_blendv_64" -> instr vblendvpd_Y_Y_Ym256_Y args
    | "caml_avx_vec256_blendv_32" -> instr vblendvps_Y_Y_Ym256_Y args
    | "caml_avx_vec256_broadcast_64" -> instr vbroadcastsd_Y_X args
    | "caml_avx_vec256_broadcast_32" -> instr vbroadcastss_Y_X args
    | "caml_avx_vec128_broadcast_32" -> instr vbroadcastss_X_X args
    | "caml_avx_float64x4_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_Y_Y_Ym256 ~i args
    | "caml_avx_float32x8_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_Y_Y_Ym256 ~i args
    | "caml_avx_cvt_int32x4_float64x4" -> instr vcvtdq2pd_Y_Xm128 args
    | "caml_avx_cvt_int32x8_float32x8" -> instr vcvtdq2ps_Y_Ym256 args
    | "caml_avx_cvt_float64x4_int32x4" -> instr vcvtpd2dq_X_Ym256 args
    | "caml_avx_cvt_float64x4_float32x4" -> instr vcvtpd2ps_X_Ym256 args
    | "caml_avx_cvt_float32x8_int32x8" -> instr vcvtps2dq_Y_Ym256 args
    | "caml_avx_cvt_float32x4_float64x4" -> instr vcvtps2pd_Y_Xm128 args
    | "caml_avx_cvtt_float64x4_int32x4" -> instr vcvttpd2dq_X_Ym256 args
    | "caml_avx_cvtt_float32x8_int32x8" -> instr vcvttps2dq_Y_Ym256 args
    | "caml_avx_float64x4_div" -> instr vdivpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_div" -> instr vdivps_Y_Y_Ym256 args
    | "caml_avx_float32x4x2_dp" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdpps_Y_Y_Ym256 ~i args
    | "caml_avx_vec256_extract_128" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf128 ~i args
    | "caml_avx_float64x2x2_hadd" -> instr vhaddpd_Y_Y_Ym256 args
    | "caml_avx_float32x4x2_hadd" -> instr vhaddps_Y_Y_Ym256 args
    | "caml_avx_float64x2x2_hsub" -> instr vhsubpd_Y_Y_Ym256 args
    | "caml_avx_float32x4x2_hsub" -> instr vhsubps_Y_Y_Ym256 args
    | "caml_avx_vec256_insert_128" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf128 ~i args
    | "caml_avx_float64x4_max" -> instr vmaxpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_max" -> instr vmaxps_Y_Y_Ym256 args
    | "caml_avx_float64x4_min" -> instr vminpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_min" -> instr vminps_Y_Y_Ym256 args
    | "caml_avx_vec256_dup_even_64" -> instr vmovddup_Y_Ym256 args
    | "caml_avx_vec256_dup_odd_32" -> instr vmovshdup_Y_Ym256 args
    | "caml_avx_vec256_dup_even_32" -> instr vmovsldup_Y_Ym256 args
    | "caml_avx_vec256_movemask_64" -> instr vmovmskpd_r64_Y args
    | "caml_avx_vec256_movemask_32" -> instr vmovmskps_r64_Y args
    | "caml_avx_float64x4_mul" -> instr vmulpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_mul" -> instr vmulps_Y_Y_Ym256 args
    | "caml_avx_vec256_or" -> instr vorps_Y_Y_Ym256 args
    | "caml_avx_vec128_permute_64" ->
      let i, args = extract_constant args ~max:3 op in
      instr vpermilpd_X_Xm128 ~i args
    | "caml_avx_vec128x2_permute_64" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpermilpd_Y_Ym256 ~i args
    | "caml_avx_vec128_permute_32" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_X_Xm128 ~i args
    | "caml_avx_vec128x2_permute_32" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_Y_Ym256 ~i args
    | "caml_avx_vec256_permute2_128" ->
      let i, args = extract_constant args ~max:255 op in
      instr vperm2f128 ~i args
    | "caml_avx_vec128_permutev_64" -> instr vpermilpd_X_X_Xm128 args
    | "caml_avx_vec128x2_permutev_64" -> instr vpermilpd_Y_Y_Ym256 args
    | "caml_avx_vec128_permutev_32" -> instr vpermilps_X_X_Xm128 args
    | "caml_avx_vec128x2_permutev_32" -> instr vpermilps_Y_Y_Ym256 args
    | "caml_avx_float32x8_rcp" -> instr vrcpps_Y_Ym256 args
    | "caml_avx_float64x4_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr vroundpd_Y_Ym256 ~i args
    | "caml_avx_float32x8_round" ->
      let i, args = extract_constant args ~max:15 op in
      check_float_rounding i;
      instr vroundps_Y_Ym256 ~i args
    | "caml_avx_float32x8_rsqrt" -> instr vrsqrtps_Y_Ym256 args
    | "caml_avx_vec128x2_shuffle_64" ->
      let i, args = extract_constant args ~max:15 op in
      instr vshufpd_Y_Y_Ym256 ~i args
    | "caml_avx_vec128x2_shuffle_32" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufps_Y_Y_Ym256 ~i args
    | "caml_avx_float64x4_sqrt" -> instr vsqrtpd_Y_Ym256 args
    | "caml_avx_float32x8_sqrt" -> instr vsqrtps_Y_Ym256 args
    | "caml_avx_float64x4_sub" -> instr vsubpd_Y_Y_Ym256 args
    | "caml_avx_float32x8_sub" -> instr vsubps_Y_Y_Ym256 args
    | "caml_avx_vec256_testz" -> seq Seq.vptestz_Y args
    | "caml_avx_vec256_testc" -> seq Seq.vptestc_Y args
    | "caml_avx_vec256_testnzc" -> seq Seq.vptestnzc_Y args
    | "caml_avx_vec128x2_interleave_high_64" -> instr vunpckhpd_Y_Y_Ym256 args
    | "caml_avx_vec128x2_interleave_high_32" -> instr vunpckhps_Y_Y_Ym256 args
    | "caml_avx_vec128x2_interleave_low_64" -> instr vunpcklpd_Y_Y_Ym256 args
    | "caml_avx_vec128x2_interleave_low_32" -> instr vunpcklps_Y_Y_Ym256 args
    | "caml_avx_vec256_xor" -> instr vxorps_Y_Y_Ym256 args
    | "caml_avx_zeroall" -> instr vzeroall args
    | "caml_avx_zeroupper" -> instr vzeroupper args
    | _ -> None

let select_operation_avx2 ~dbg:_ op args =
  if not (Arch.Extension.enabled AVX2)
  then None
  else
    match op with
    | "caml_avx2_vec128_gather32_index32" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherdd_X_M32X_X args
    | "caml_avx2_vec256_gather32_index32" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherdd_Y_M32Y_Y args
    | "caml_avx2_vec128_gather64_index32" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherdq_X_M32X_X args
    | "caml_avx2_vec256_gather64_index32" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherdq_Y_M32X_Y args
    | "caml_avx2_vec128_gather32_index64" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherqd_X_M64X_X args
    | "caml_avx2_vec256_gather32_index64" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherqd_X_M64Y_X args
    | "caml_avx2_vec128_gather64_index64" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherqq_X_M64X_X args
    | "caml_avx2_vec256_gather64_index64" ->
      let i, args = extract_scale args op in
      simd_load ~mode:(Iindexed2scaled (i, 0)) vpgatherqq_Y_M64Y_Y args
    | "caml_avx2_int8x32_abs" -> instr vpabsb_Y_Ym256 args
    | "caml_avx2_int16x16_abs" -> instr vpabsw_Y_Ym256 args
    | "caml_avx2_int32x8_abs" -> instr vpabsd_Y_Ym256 args
    | "caml_avx2_int8x32_add" -> instr vpaddb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_add" -> instr vpaddw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_add" -> instr vpaddd_Y_Y_Ym256 args
    | "caml_avx2_int64x4_add" -> instr vpaddq_Y_Y_Ym256 args
    | "caml_avx2_int8x32_add_saturating" -> instr vpaddsb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_add_saturating" -> instr vpaddsw_Y_Y_Ym256 args
    | "caml_avx2_int8x32_add_saturating_unsigned" ->
      instr vpaddusb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_add_saturating_unsigned" ->
      instr vpaddusw_Y_Y_Ym256 args
    | "caml_avx_vec128x2_align_right_bytes" ->
      let i, args = extract_constant args ~max:31 op in
      instr vpalignr_Y_Y_Ym256 ~i args
    | "caml_avx2_int8x32_avg_unsigned" -> instr vpavgb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_avg_unsigned" -> instr vpavgw_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_blend_16" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpblendw_Y_Y_Ym256 ~i args
    | "caml_avx2_vec256_blendv_8" -> instr vpblendvb_Y_Y_Ym256_Y args
    | "caml_avx2_vec128_broadcast_8" -> instr vpbroadcastb_X_Xm8 args
    | "caml_avx2_vec256_broadcast_8" -> instr vpbroadcastb_Y_Xm8 args
    | "caml_avx2_vec128_broadcast_16" -> instr vpbroadcastw_X_Xm16 args
    | "caml_avx2_vec256_broadcast_16" -> instr vpbroadcastw_Y_Xm16 args
    | "caml_avx2_vec128x2_shift_left_bytes" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpslldq_Y_Y ~i args
    | "caml_avx2_vec128x2_shift_right_bytes" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpsrldq_Y_Y ~i args
    | "caml_avx2_int8x32_cmpeq" -> instr vpcmpeqb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_cmpeq" -> instr vpcmpeqw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_cmpeq" -> instr vpcmpeqd_Y_Y_Ym256 args
    | "caml_avx2_int64x4_cmpeq" -> instr vpcmpeqq_Y_Y_Ym256 args
    | "caml_avx2_int8x32_cmpgt" -> instr vpcmpgtb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_cmpgt" -> instr vpcmpgtw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_cmpgt" -> instr vpcmpgtd_Y_Y_Ym256 args
    | "caml_avx2_int64x4_cmpgt" -> instr vpcmpgtq_Y_Y_Ym256 args
    | "caml_avx2_cvtsx_int16x8_int32x8" -> instr vpmovsxwd_Y_Xm128 args
    | "caml_avx2_cvtsx_int16x8_int64x4" -> instr vpmovsxwq_Y_Xm64 args
    | "caml_avx2_cvtsx_int32x4_int64x4" -> instr vpmovsxdq_Y_Xm128 args
    | "caml_avx2_cvtsx_int8x16_int16x16" -> instr vpmovsxbw_Y_Xm128 args
    | "caml_avx2_cvtsx_int8x16_int32x8" -> instr vpmovsxbd_Y_Xm64 args
    | "caml_avx2_cvtsx_int8x16_int64x4" -> instr vpmovsxbq_Y_Xm32 args
    | "caml_avx2_cvtzx_int16x8_int32x8" -> instr vpmovzxwd_Y_Xm128 args
    | "caml_avx2_cvtzx_int16x8_int64x4" -> instr vpmovzxwq_Y_Xm64 args
    | "caml_avx2_cvtzx_int32x4_int64x4" -> instr vpmovzxdq_Y_Xm128 args
    | "caml_avx2_cvtzx_int8x16_int16x16" -> instr vpmovzxbw_Y_Xm128 args
    | "caml_avx2_cvtzx_int8x16_int32x8" -> instr vpmovzxbd_Y_Xm64 args
    | "caml_avx2_cvtzx_int8x16_int64x4" -> instr vpmovzxbq_Y_Xm32 args
    | "caml_avx2_int16x8x2_hadd" -> instr vphaddw_Y_Y_Ym256 args
    | "caml_avx2_int32x4x2_hadd" -> instr vphaddd_Y_Y_Ym256 args
    | "caml_avx2_int16x8x2_hadd_saturating" -> instr vphaddsw_Y_Y_Ym256 args
    | "caml_avx2_int16x8x2_hsub" -> instr vphsubw_Y_Y_Ym256 args
    | "caml_avx2_int32x4x2_hsub" -> instr vphsubd_Y_Y_Ym256 args
    | "caml_avx2_int16x8x2_hsub_saturating" -> instr vphsubsw_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mul_hadd_int32x8" -> instr vpmaddwd_Y_Y_Ym256 args
    | "caml_avx2_int8x32_mul_unsigned_hadd_saturating_int16x16" ->
      instr vpmaddubsw_Y_Y_Ym256 args
    | "caml_avx2_int8x32_max" -> instr vpmaxsb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_max" -> instr vpmaxsw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_max" -> instr vpmaxsd_Y_Y_Ym256 args
    | "caml_avx2_int8x32_max_unsigned" -> instr vpmaxub_Y_Y_Ym256 args
    | "caml_avx2_int16x16_max_unsigned" -> instr vpmaxuw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_max_unsigned" -> instr vpmaxud_Y_Y_Ym256 args
    | "caml_avx2_int8x32_min" -> instr vpminsb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_min" -> instr vpminsw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_min" -> instr vpminsd_Y_Y_Ym256 args
    | "caml_avx2_int8x32_min_unsigned" -> instr vpminub_Y_Y_Ym256 args
    | "caml_avx2_int16x16_min_unsigned" -> instr vpminuw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_min_unsigned" -> instr vpminud_Y_Y_Ym256 args
    | "caml_avx2_vec256_movemask_8" -> instr vpmovmskb_r64_Y args
    | "caml_avx2_int8x16x2_multi_sad_unsigned" ->
      let i, args = extract_constant args ~max:63 op in
      instr vmpsadbw_Y_Y_Ym256 ~i args
    | "caml_avx2_int32x8_mul_even" -> instr vpmuldq_Y_Y_Ym256 args
    | "caml_avx2_int32x8_mul_even_unsigned" -> instr vpmuludq_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mul_high" -> instr vpmulhw_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mul_high_unsigned" -> instr vpmulhuw_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mul_round" -> instr vpmulhrsw_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mul_low" -> instr vpmullw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_mul_low" -> instr vpmulld_Y_Y_Ym256 args
    | "caml_avx2_cvt_int16x16_int8x32_saturating" ->
      instr vpacksswb_Y_Y_Ym256 args
    | "caml_avx2_cvt_int32x8_int16x16_saturating" ->
      instr vpackssdw_Y_Y_Ym256 args
    | "caml_avx2_cvt_int16x16_int8x32_saturating_unsigned" ->
      instr vpackuswb_Y_Y_Ym256 args
    | "caml_avx2_cvt_int32x8_int16x16_saturating_unsigned" ->
      instr vpackusdw_Y_Y_Ym256 args
    | "caml_avx2_vec256_permute_64" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermpd_Y_Ym256 ~i args
    | "caml_avx2_vec256_permutev_32" -> instr vpermps_Y_Y_Ym256 args
    | "caml_avx2_int8x32_sad_unsigned" -> instr vpsadbw_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_shuffle_8" -> instr vpshufb_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_shuffle_high_16" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufhw_Y_Ym256 ~i args
    | "caml_avx2_vec128x2_shuffle_low_16" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshuflw_Y_Ym256 ~i args
    | "caml_avx2_int8x32_mulsign" -> instr vpsignb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_mulsign" -> instr vpsignw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_mulsign" -> instr vpsignd_Y_Y_Ym256 args
    | "caml_avx2_int16x16_sll" -> instr vpsllw_Y_Y_Xm128 args
    | "caml_avx2_int32x8_sll" -> instr vpslld_Y_Y_Xm128 args
    | "caml_avx2_int64x4_sll" -> instr vpsllq_Y_Y_Xm128 args
    | "caml_avx2_int16x16_slli" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpsllw_Y_Y ~i args
    | "caml_avx2_int32x8_slli" ->
      let i, args = extract_constant args ~max:31 op in
      instr vpslld_Y_Y ~i args
    | "caml_avx2_int64x4_slli" ->
      let i, args = extract_constant args ~max:63 op in
      instr vpsllq_Y_Y ~i args
    | "caml_avx2_int32x4_sllv" -> instr vpsllvd_X_X_Xm128 args
    | "caml_avx2_int32x8_sllv" -> instr vpsllvd_Y_Y_Ym256 args
    | "caml_avx2_int64x2_sllv" -> instr vpsllvq_X_X_Xm128 args
    | "caml_avx2_int64x4_sllv" -> instr vpsllvq_Y_Y_Ym256 args
    | "caml_avx2_int16x16_sra" -> instr vpsraw_Y_Y_Xm128 args
    | "caml_avx2_int32x8_sra" -> instr vpsrad_Y_Y_Xm128 args
    | "caml_avx2_int16x16_srai" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpsraw_Y_Y ~i args
    | "caml_avx2_int32x8_srai" ->
      let i, args = extract_constant args ~max:31 op in
      instr vpsrad_Y_Y ~i args
    | "caml_avx2_int32x4_srav" -> instr vpsravd_X_X_Xm128 args
    | "caml_avx2_int32x8_srav" -> instr vpsravd_Y_Y_Ym256 args
    | "caml_avx2_int16x16_srl" -> instr vpsrlw_Y_Y_Xm128 args
    | "caml_avx2_int32x8_srl" -> instr vpsrld_Y_Y_Xm128 args
    | "caml_avx2_int64x4_srl" -> instr vpsrlq_Y_Y_Xm128 args
    | "caml_avx2_int16x16_srli" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpsrlw_Y_Y ~i args
    | "caml_avx2_int32x8_srli" ->
      let i, args = extract_constant args ~max:31 op in
      instr vpsrld_Y_Y ~i args
    | "caml_avx2_int64x4_srli" ->
      let i, args = extract_constant args ~max:63 op in
      instr vpsrlq_Y_Y ~i args
    | "caml_avx2_int32x4_srlv" -> instr vpsrlvd_X_X_Xm128 args
    | "caml_avx2_int32x8_srlv" -> instr vpsrlvd_Y_Y_Ym256 args
    | "caml_avx2_int64x2_srlv" -> instr vpsrlvq_X_X_Xm128 args
    | "caml_avx2_int64x4_srlv" -> instr vpsrlvq_Y_Y_Ym256 args
    | "caml_avx2_int8x32_sub" -> instr vpsubb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_sub" -> instr vpsubw_Y_Y_Ym256 args
    | "caml_avx2_int32x8_sub" -> instr vpsubd_Y_Y_Ym256 args
    | "caml_avx2_int64x4_sub" -> instr vpsubq_Y_Y_Ym256 args
    | "caml_avx2_int8x32_sub_saturating" -> instr vpsubsb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_sub_saturating" -> instr vpsubsw_Y_Y_Ym256 args
    | "caml_avx2_int8x32_sub_saturating_unsigned" ->
      instr vpsubusb_Y_Y_Ym256 args
    | "caml_avx2_int16x16_sub_saturating_unsigned" ->
      instr vpsubusw_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_interleave_high_8" -> instr vpunpckhbw_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_interleave_high_16" -> instr vpunpckhwd_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_interleave_low_8" -> instr vpunpcklbw_Y_Y_Ym256 args
    | "caml_avx2_vec128x2_interleave_low_16" -> instr vpunpcklwd_Y_Y_Ym256 args
    | _ -> None

let select_operation_f16c ~dbg:_ op args =
  if not (Arch.Extension.enabled F16C)
  then None
  else
    match op with
    | "caml_f16c_cvt_float16x8_float32x4" -> instr vcvtph2ps_X_Xm64 args
    | "caml_f16c_cvt_float16x8_float32x8" -> instr vcvtph2ps_Y_Xm128 args
    | "caml_f16c_cvt_float32x4_float16x8" ->
      let i, args = extract_constant args ~max:7 op in
      instr vcvtps2ph_Xm64_X ~i args
    | "caml_f16c_cvt_float32x8_float16x8" ->
      let i, args = extract_constant args ~max:7 op in
      instr vcvtps2ph_Xm128_Y ~i args
    | _ -> None

let select_operation_fma ~dbg:_ op args =
  if not (Arch.Extension.enabled FMA)
  then None
  else
    match op with
    | "caml_fma_float64x2_mul_add" -> instr vfmadd213pd_X_X_Xm128 args
    | "caml_fma_float64x4_mul_add" -> instr vfmadd213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_mul_add" -> instr vfmadd213ps_X_X_Xm128 args
    | "caml_fma_float32x8_mul_add" -> instr vfmadd213ps_Y_Y_Ym256 args
    | "caml_fma_float64_mul_add" -> instr vfmadd213sd_X_X_Xm64 args
    | "caml_fma_float32_mul_add" -> instr vfmadd213ss_X_X_Xm32 args
    | "caml_fma_float64x2_mul_addsub" -> instr vfmaddsub213pd_X_X_Xm128 args
    | "caml_fma_float64x4_mul_addsub" -> instr vfmaddsub213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_mul_addsub" -> instr vfmaddsub213ps_X_X_Xm128 args
    | "caml_fma_float32x8_mul_addsub" -> instr vfmaddsub213ps_Y_Y_Ym256 args
    | "caml_fma_float64x2_mul_sub" -> instr vfmsub213pd_X_X_Xm128 args
    | "caml_fma_float64x4_mul_sub" -> instr vfmsub213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_mul_sub" -> instr vfmsub213ps_X_X_Xm128 args
    | "caml_fma_float32x8_mul_sub" -> instr vfmsub213ps_Y_Y_Ym256 args
    | "caml_fma_float64_mul_sub" -> instr vfmsub213sd_X_X_Xm64 args
    | "caml_fma_float32_mul_sub" -> instr vfmsub213ss_X_X_Xm32 args
    | "caml_fma_float64x2_mul_subadd" -> instr vfmsubadd213pd_X_X_Xm128 args
    | "caml_fma_float64x4_mul_subadd" -> instr vfmsubadd213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_mul_subadd" -> instr vfmsubadd213ps_X_X_Xm128 args
    | "caml_fma_float32x8_mul_subadd" -> instr vfmsubadd213ps_Y_Y_Ym256 args
    | "caml_fma_float64x2_neg_mul_add" -> instr vfnmadd213pd_X_X_Xm128 args
    | "caml_fma_float64x4_neg_mul_add" -> instr vfnmadd213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_neg_mul_add" -> instr vfnmadd213ps_X_X_Xm128 args
    | "caml_fma_float32x8_neg_mul_add" -> instr vfnmadd213ps_Y_Y_Ym256 args
    | "caml_fma_float64_neg_mul_add" -> instr vfnmadd213sd_X_X_Xm64 args
    | "caml_fma_float32_neg_mul_add" -> instr vfnmadd213ss_X_X_Xm32 args
    | "caml_fma_float64x2_neg_mul_sub" -> instr vfnmsub213pd_X_X_Xm128 args
    | "caml_fma_float64x4_neg_mul_sub" -> instr vfnmsub213pd_Y_Y_Ym256 args
    | "caml_fma_float32x4_neg_mul_sub" -> instr vfnmsub213ps_X_X_Xm128 args
    | "caml_fma_float32x8_neg_mul_sub" -> instr vfnmsub213ps_Y_Y_Ym256 args
    | "caml_fma_float64_neg_mul_sub" -> instr vfnmsub213sd_X_X_Xm64 args
    | "caml_fma_float32_neg_mul_sub" -> instr vfnmsub213ss_X_X_Xm32 args
    | _ -> None

(* AVX512 intrinsics. Unlike the SSE/AVX tiers these are EVEX-only (no legacy
   fallback), so we always use the plain [instr] combinator with the generated
   zmm/ymm/xmm bindings. The descriptive builtin names follow the same
   [caml_avx512_<type>_<op>] convention as the other tiers; the OCaml external's
   argument order mirrors the chosen binding's [args] array, so masked variants
   take an extra [mask] operand last ([_maskz] selects the zeroing [~z:true]
   form) and merge-masked variants ([_mask]) take a leading merge-source operand
   (the [_K_merge] binding with [res = Arg [|0|]]).

   The match arms between the GENERATED markers below are produced by the AVX512
   intrinsic generator (tools/simdgen/avx512gen) from the Intel intrinsics XML;
   do not edit them by hand -- regenerate instead. *)
let select_operation_avx512 ~dbg:_ op args =
  if not (Arch.Extension.enabled AVX512F)
  then None
  else
    match op with
    (* BEGIN GENERATED AVX512 *)
    | "caml_avx512bw_int8x32_dbsad_unsigned" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_Y_Y_Ym256 ~i args
    | "caml_avx512bw_int8x32_dbsad_unsigned_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int8x32_dbsad_unsigned_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vdbpsadbw_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int8x16_dbsad_unsigned" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_X_X_Xm128 ~i args
    | "caml_avx512bw_int8x16_dbsad_unsigned_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_X_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int8x16_dbsad_unsigned_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vdbpsadbw_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int8x32_align_right_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpalignr_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int8x32_align_right_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpalignr_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int8x16_align_right_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpalignr_X_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int8x16_align_right_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpalignr_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int8x32_blend" ->
      instr (vpblendmb_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512bw_int8x16_blend" ->
      instr (vpblendmb_X_X_Xm128_K ~z:false) args
    | "caml_avx512bw_int16x16_blend" ->
      instr (vpblendmw_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512bw_int16x8_blend" ->
      instr (vpblendmw_X_X_Xm128_K ~z:false) args
    | "caml_avx512bw_int8x32_broadcast_mask" ->
      instr vpbroadcastb_Y_Xm8_K_merge args
    | "caml_avx512bw_int8x32_broadcast_maskz" ->
      instr (vpbroadcastb_Y_Xm8_K ~z:true) args
    | "caml_avx512bw_int8x16_broadcast_mask" ->
      instr vpbroadcastb_X_Xm8_K_merge args
    | "caml_avx512bw_int8x16_broadcast_maskz" ->
      instr (vpbroadcastb_X_Xm8_K ~z:true) args
    | "caml_avx512bw_int16x16_broadcast_mask" ->
      instr vpbroadcastw_Y_Xm16_K_merge args
    | "caml_avx512bw_int16x16_broadcast_maskz" ->
      instr (vpbroadcastw_Y_Xm16_K ~z:true) args
    | "caml_avx512bw_int16x8_broadcast_mask" ->
      instr vpbroadcastw_X_Xm16_K_merge args
    | "caml_avx512bw_int16x8_broadcast_maskz" ->
      instr (vpbroadcastw_X_Xm16_K ~z:true) args
    | "caml_avx512bw_int16x16_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2w_Y_Y_Ym256_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512bw_int16x16_permutex2var_mask" ->
      instr (vpermt2w_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512bw_int16x16_permutex2var_maskz" ->
      instr (vpermt2w_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x16_permutex2var" -> instr vpermt2w_Y_Y_Ym256 args
    | "caml_avx512bw_int16x8_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2w_X_X_Xm128_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512bw_int16x8_permutex2var_mask" ->
      instr (vpermt2w_X_X_Xm128_K ~z:false) args
    | "caml_avx512bw_int16x8_permutex2var_maskz" ->
      instr (vpermt2w_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_permutex2var" -> instr vpermt2w_X_X_Xm128 args
    | "caml_avx512bw_int16x16_permutexvar_mask" ->
      instr vpermw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_permutexvar_maskz" ->
      instr (vpermw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x16_permutexvar" -> instr vpermw_Y_Y_Ym256 args
    | "caml_avx512bw_int16x8_permutexvar_mask" ->
      instr vpermw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_permutexvar_maskz" ->
      instr (vpermw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_permutexvar" -> instr vpermw_X_X_Xm128 args
    | "caml_avx512bw_int8x32_movepi_mask" -> instr vpmovb2m_K_Y args
    | "caml_avx512bw_int8x16_movepi_mask" -> instr vpmovb2m_K_X args
    | "caml_avx512bw_int8x32_movm" -> instr vpmovm2b_Y_K args
    | "caml_avx512bw_int8x16_movm" -> instr vpmovm2b_X_K args
    | "caml_avx512bw_int16x16_movm" -> instr vpmovm2w_Y_K args
    | "caml_avx512bw_int16x8_movm" -> instr vpmovm2w_X_K args
    | "caml_avx512bw_int16x16_movepi_mask" -> instr vpmovw2m_K_Y args
    | "caml_avx512bw_int16x8_movepi_mask" -> instr vpmovw2m_K_X args
    | "caml_avx512bw_int8x32_shuffle_mask" ->
      instr vpshufb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_shuffle_maskz" ->
      instr (vpshufb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_shuffle_mask" ->
      instr vpshufb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_shuffle_maskz" ->
      instr (vpshufb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_shuffle_high_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufhw_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int16x16_shuffle_high_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufhw_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int16x8_shuffle_high_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufhw_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int16x8_shuffle_high_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufhw_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int16x16_shuffle_low_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshuflw_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int16x16_shuffle_low_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshuflw_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int16x8_shuffle_low_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshuflw_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int16x8_shuffle_low_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshuflw_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int8x32_interleave_high_mask" ->
      instr vpunpckhbw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_interleave_high_maskz" ->
      instr (vpunpckhbw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_interleave_high_mask" ->
      instr vpunpckhbw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_interleave_high_maskz" ->
      instr (vpunpckhbw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_interleave_high_mask" ->
      instr vpunpckhwd_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_interleave_high_maskz" ->
      instr (vpunpckhwd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_interleave_high_mask" ->
      instr vpunpckhwd_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_interleave_high_maskz" ->
      instr (vpunpckhwd_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_interleave_low_mask" ->
      instr vpunpcklbw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_interleave_low_maskz" ->
      instr (vpunpcklbw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_interleave_low_mask" ->
      instr vpunpcklbw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_interleave_low_maskz" ->
      instr (vpunpcklbw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_interleave_low_mask" ->
      instr vpunpcklwd_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_interleave_low_maskz" ->
      instr (vpunpcklwd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_interleave_low_mask" ->
      instr vpunpcklwd_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_interleave_low_maskz" ->
      instr (vpunpcklwd_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_mov_mask" -> instr vmovdqu16_Y_Y_K_merge args
    | "caml_avx512bw_int16x16_mov_maskz" ->
      instr (vmovdqu16_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_mov_mask" -> instr vmovdqu16_X_X_K_merge args
    | "caml_avx512bw_int16x8_mov_maskz" ->
      instr (vmovdqu16_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_mov_mask" -> instr vmovdqu8_Y_Y_K_merge args
    | "caml_avx512bw_int8x32_mov_maskz" ->
      instr (vmovdqu8_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_mov_mask" -> instr vmovdqu8_X_X_K_merge args
    | "caml_avx512bw_int8x16_mov_maskz" ->
      instr (vmovdqu8_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_abs_mask" -> instr vpabsb_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_abs_maskz" -> instr (vpabsb_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_abs_mask" -> instr vpabsb_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_abs_maskz" -> instr (vpabsb_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_abs_mask" -> instr vpabsw_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_abs_maskz" ->
      instr (vpabsw_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_abs_mask" -> instr vpabsw_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_abs_maskz" -> instr (vpabsw_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_add_mask" -> instr vpaddb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_add_maskz" ->
      instr (vpaddb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_add_mask" -> instr vpaddb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_add_maskz" ->
      instr (vpaddb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_add_saturating_mask" ->
      instr vpaddsb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_add_saturating_maskz" ->
      instr (vpaddsb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_add_saturating_mask" ->
      instr vpaddsb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_add_saturating_maskz" ->
      instr (vpaddsb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_add_saturating_mask" ->
      instr vpaddsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_add_saturating_maskz" ->
      instr (vpaddsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_add_saturating_mask" ->
      instr vpaddsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_add_saturating_maskz" ->
      instr (vpaddsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_add_saturating_unsigned_mask" ->
      instr vpaddusb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_add_saturating_unsigned_maskz" ->
      instr (vpaddusb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_add_saturating_unsigned_mask" ->
      instr vpaddusb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_add_saturating_unsigned_maskz" ->
      instr (vpaddusb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_add_saturating_unsigned_mask" ->
      instr vpaddusw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_add_saturating_unsigned_maskz" ->
      instr (vpaddusw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_add_saturating_unsigned_mask" ->
      instr vpaddusw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_add_saturating_unsigned_maskz" ->
      instr (vpaddusw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_add_mask" -> instr vpaddw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_add_maskz" ->
      instr (vpaddw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_add_mask" -> instr vpaddw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_add_maskz" ->
      instr (vpaddw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_avg_unsigned_mask" ->
      instr vpavgb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_avg_unsigned_maskz" ->
      instr (vpavgb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_avg_unsigned_mask" ->
      instr vpavgb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_avg_unsigned_maskz" ->
      instr (vpavgb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_avg_unsigned_mask" ->
      instr vpavgw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_avg_unsigned_maskz" ->
      instr (vpavgw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_avg_unsigned_mask" ->
      instr vpavgw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_avg_unsigned_maskz" ->
      instr (vpavgw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_maddubs_mask" ->
      instr vpmaddubsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_maddubs_maskz" ->
      instr (vpmaddubsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_maddubs_mask" ->
      instr vpmaddubsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_maddubs_maskz" ->
      instr (vpmaddubsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_madd_mask" ->
      instr vpmaddwd_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_madd_maskz" ->
      instr (vpmaddwd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_madd_mask" -> instr vpmaddwd_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_madd_maskz" ->
      instr (vpmaddwd_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_max_mask" -> instr vpmaxsb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_max_maskz" ->
      instr (vpmaxsb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_max_mask" -> instr vpmaxsb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_max_maskz" ->
      instr (vpmaxsb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_max_mask" -> instr vpmaxsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_max_maskz" ->
      instr (vpmaxsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_max_mask" -> instr vpmaxsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_max_maskz" ->
      instr (vpmaxsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_max_unsigned_mask" ->
      instr vpmaxub_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_max_unsigned_maskz" ->
      instr (vpmaxub_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_max_unsigned_mask" ->
      instr vpmaxub_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_max_unsigned_maskz" ->
      instr (vpmaxub_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_max_unsigned_mask" ->
      instr vpmaxuw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_max_unsigned_maskz" ->
      instr (vpmaxuw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_max_unsigned_mask" ->
      instr vpmaxuw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_max_unsigned_maskz" ->
      instr (vpmaxuw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_min_mask" -> instr vpminsb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_min_maskz" ->
      instr (vpminsb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_min_mask" -> instr vpminsb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_min_maskz" ->
      instr (vpminsb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_min_mask" -> instr vpminsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_min_maskz" ->
      instr (vpminsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_min_mask" -> instr vpminsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_min_maskz" ->
      instr (vpminsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_min_unsigned_mask" ->
      instr vpminub_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_min_unsigned_maskz" ->
      instr (vpminub_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_min_unsigned_mask" ->
      instr vpminub_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_min_unsigned_maskz" ->
      instr (vpminub_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_min_unsigned_mask" ->
      instr vpminuw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_min_unsigned_maskz" ->
      instr (vpminuw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_min_unsigned_mask" ->
      instr vpminuw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_min_unsigned_maskz" ->
      instr (vpminuw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_mul_round_mask" ->
      instr vpmulhrsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_mul_round_maskz" ->
      instr (vpmulhrsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_mul_round_mask" ->
      instr vpmulhrsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_mul_round_maskz" ->
      instr (vpmulhrsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_mul_high_unsigned_mask" ->
      instr vpmulhuw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_mul_high_unsigned_maskz" ->
      instr (vpmulhuw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_mul_high_unsigned_mask" ->
      instr vpmulhuw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_mul_high_unsigned_maskz" ->
      instr (vpmulhuw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_mul_high_mask" ->
      instr vpmulhw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_mul_high_maskz" ->
      instr (vpmulhw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_mul_high_mask" ->
      instr vpmulhw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_mul_high_maskz" ->
      instr (vpmulhw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_mul_low_mask" ->
      instr vpmullw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_mul_low_maskz" ->
      instr (vpmullw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_mul_low_mask" ->
      instr vpmullw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_mul_low_maskz" ->
      instr (vpmullw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_sub_mask" -> instr vpsubb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_sub_maskz" ->
      instr (vpsubb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_sub_mask" -> instr vpsubb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_sub_maskz" ->
      instr (vpsubb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_sub_saturating_mask" ->
      instr vpsubsb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_sub_saturating_maskz" ->
      instr (vpsubsb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_sub_saturating_mask" ->
      instr vpsubsb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_sub_saturating_maskz" ->
      instr (vpsubsb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_sub_saturating_mask" ->
      instr vpsubsw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_sub_saturating_maskz" ->
      instr (vpsubsw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_sub_saturating_mask" ->
      instr vpsubsw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_sub_saturating_maskz" ->
      instr (vpsubsw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int8x32_sub_saturating_unsigned_mask" ->
      instr vpsubusb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int8x32_sub_saturating_unsigned_maskz" ->
      instr (vpsubusb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x16_sub_saturating_unsigned_mask" ->
      instr vpsubusb_X_X_Xm128_K_merge args
    | "caml_avx512bw_int8x16_sub_saturating_unsigned_maskz" ->
      instr (vpsubusb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_sub_saturating_unsigned_mask" ->
      instr vpsubusw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_sub_saturating_unsigned_maskz" ->
      instr (vpsubusw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_sub_saturating_unsigned_mask" ->
      instr vpsubusw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_sub_saturating_unsigned_maskz" ->
      instr (vpsubusw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_sub_mask" -> instr vpsubw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_sub_maskz" ->
      instr (vpsubw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x8_sub_mask" -> instr vpsubw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_sub_maskz" ->
      instr (vpsubw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_cvt_int32x8_int16x16_saturating_mask" ->
      instr vpackssdw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_cvt_int32x8_int16x16_saturating_maskz" ->
      instr (vpackssdw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_cvt_int32x4_int16x8_saturating_mask" ->
      instr vpackssdw_X_X_Xm128_K_merge args
    | "caml_avx512bw_cvt_int32x4_int16x8_saturating_maskz" ->
      instr (vpackssdw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_cvt_int16x16_int8x32_saturating_mask" ->
      instr vpacksswb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_cvt_int16x16_int8x32_saturating_maskz" ->
      instr (vpacksswb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_cvt_int16x8_int8x16_saturating_mask" ->
      instr vpacksswb_X_X_Xm128_K_merge args
    | "caml_avx512bw_cvt_int16x8_int8x16_saturating_maskz" ->
      instr (vpacksswb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_cvt_int32x8_int16x16_saturating_unsigned_mask" ->
      instr vpackusdw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_cvt_int32x8_int16x16_saturating_unsigned_maskz" ->
      instr (vpackusdw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_cvt_int32x4_int16x8_saturating_unsigned_mask" ->
      instr vpackusdw_X_X_Xm128_K_merge args
    | "caml_avx512bw_cvt_int32x4_int16x8_saturating_unsigned_maskz" ->
      instr (vpackusdw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_cvt_int16x16_int8x32_saturating_unsigned_mask" ->
      instr vpackuswb_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_cvt_int16x16_int8x32_saturating_unsigned_maskz" ->
      instr (vpackuswb_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_cvt_int16x8_int8x16_saturating_unsigned_mask" ->
      instr vpackuswb_X_X_Xm128_K_merge args
    | "caml_avx512bw_cvt_int16x8_int8x16_saturating_unsigned_maskz" ->
      instr (vpackuswb_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating" ->
      instr vpmovswb_Xm128_Y args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_mask" ->
      instr vpmovswb_X_Y_K_merge args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_maskz" ->
      instr (vpmovswb_Xm128_Y_K ~z:true) args
    | "caml_avx512bw_cvtsx_int8x16_int16x16_mask" ->
      instr vpmovsxbw_Y_Xm128_K_merge args
    | "caml_avx512bw_cvtsx_int8x16_int16x16_maskz" ->
      instr (vpmovsxbw_Y_Xm128_K ~z:true) args
    | "caml_avx512bw_cvtsx_int8x16_int16x8_mask" ->
      instr vpmovsxbw_X_Xm64_K_merge args
    | "caml_avx512bw_cvtsx_int8x16_int16x8_maskz" ->
      instr (vpmovsxbw_X_Xm64_K ~z:true) args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_unsigned" ->
      instr vpmovuswb_Xm128_Y args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_unsigned_mask" ->
      instr vpmovuswb_X_Y_K_merge args
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_unsigned_maskz" ->
      instr (vpmovuswb_Xm128_Y_K ~z:true) args
    | "caml_avx512bw_cvt_int16x16_int8x16" -> instr vpmovwb_Xm128_Y args
    | "caml_avx512bw_cvt_int16x16_int8x16_mask" ->
      instr vpmovwb_X_Y_K_merge args
    | "caml_avx512bw_cvt_int16x16_int8x16_maskz" ->
      instr (vpmovwb_Xm128_Y_K ~z:true) args
    | "caml_avx512bw_cvtzx_int8x16_int16x16_mask" ->
      instr vpmovzxbw_Y_Xm128_K_merge args
    | "caml_avx512bw_cvtzx_int8x16_int16x16_maskz" ->
      instr (vpmovzxbw_Y_Xm128_K ~z:true) args
    | "caml_avx512bw_cvtzx_int8x16_int16x8_mask" ->
      instr vpmovzxbw_X_Xm64_K_merge args
    | "caml_avx512bw_cvtzx_int8x16_int16x8_maskz" ->
      instr (vpmovzxbw_X_Xm64_K ~z:true) args
    | "caml_avx512bw_int8x32_set1_mask" -> instr vpbroadcastb_Y_r32_K_merge args
    | "caml_avx512bw_int8x32_set1_maskz" ->
      instr (vpbroadcastb_Y_r32_K ~z:true) args
    | "caml_avx512bw_int8x16_set1_mask" -> instr vpbroadcastb_X_r32_K_merge args
    | "caml_avx512bw_int8x16_set1_maskz" ->
      instr (vpbroadcastb_X_r32_K ~z:true) args
    | "caml_avx512bw_int16x16_set1_mask" ->
      instr vpbroadcastw_Y_r32_K_merge args
    | "caml_avx512bw_int16x16_set1_maskz" ->
      instr (vpbroadcastw_Y_r32_K ~z:true) args
    | "caml_avx512bw_int16x8_set1_mask" -> instr vpbroadcastw_X_r32_K_merge args
    | "caml_avx512bw_int16x8_set1_maskz" ->
      instr (vpbroadcastw_X_r32_K ~z:true) args
    | "caml_avx512bw_int8x32_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_Y_Ym256 ~i args
    | "caml_avx512bw_int8x32_cmpeq" -> instr vpcmpb_K_Y_Ym256 ~i:0 args
    | "caml_avx512bw_int8x32_cmpge" -> instr vpcmpb_K_Y_Ym256 ~i:5 args
    | "caml_avx512bw_int8x32_cmpgt" -> instr vpcmpb_K_Y_Ym256 ~i:6 args
    | "caml_avx512bw_int8x32_cmple" -> instr vpcmpb_K_Y_Ym256 ~i:2 args
    | "caml_avx512bw_int8x32_cmplt" -> instr vpcmpb_K_Y_Ym256 ~i:1 args
    | "caml_avx512bw_int8x32_cmpneq" -> instr vpcmpb_K_Y_Ym256 ~i:4 args
    | "caml_avx512bw_int8x32_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_Y_Ym256_K ~i args
    | "caml_avx512bw_int8x32_cmpeq_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:0 args
    | "caml_avx512bw_int8x32_cmpge_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:5 args
    | "caml_avx512bw_int8x32_cmpgt_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:6 args
    | "caml_avx512bw_int8x32_cmple_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:2 args
    | "caml_avx512bw_int8x32_cmplt_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:1 args
    | "caml_avx512bw_int8x32_cmpneq_mask" -> instr vpcmpb_K_Y_Ym256_K ~i:4 args
    | "caml_avx512bw_int8x16_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_X_Xm128 ~i args
    | "caml_avx512bw_int8x16_cmpeq" -> instr vpcmpb_K_X_Xm128 ~i:0 args
    | "caml_avx512bw_int8x16_cmpge" -> instr vpcmpb_K_X_Xm128 ~i:5 args
    | "caml_avx512bw_int8x16_cmpgt" -> instr vpcmpb_K_X_Xm128 ~i:6 args
    | "caml_avx512bw_int8x16_cmple" -> instr vpcmpb_K_X_Xm128 ~i:2 args
    | "caml_avx512bw_int8x16_cmplt" -> instr vpcmpb_K_X_Xm128 ~i:1 args
    | "caml_avx512bw_int8x16_cmpneq" -> instr vpcmpb_K_X_Xm128 ~i:4 args
    | "caml_avx512bw_int8x16_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_X_Xm128_K ~i args
    | "caml_avx512bw_int8x16_cmpeq_mask" -> instr vpcmpb_K_X_Xm128_K ~i:0 args
    | "caml_avx512bw_int8x16_cmpge_mask" -> instr vpcmpb_K_X_Xm128_K ~i:5 args
    | "caml_avx512bw_int8x16_cmpgt_mask" -> instr vpcmpb_K_X_Xm128_K ~i:6 args
    | "caml_avx512bw_int8x16_cmple_mask" -> instr vpcmpb_K_X_Xm128_K ~i:2 args
    | "caml_avx512bw_int8x16_cmplt_mask" -> instr vpcmpb_K_X_Xm128_K ~i:1 args
    | "caml_avx512bw_int8x16_cmpneq_mask" -> instr vpcmpb_K_X_Xm128_K ~i:4 args
    | "caml_avx512bw_int8x32_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_Y_Ym256 ~i args
    | "caml_avx512bw_int8x32_cmpeq_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:0 args
    | "caml_avx512bw_int8x32_cmpge_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:5 args
    | "caml_avx512bw_int8x32_cmpgt_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:6 args
    | "caml_avx512bw_int8x32_cmple_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:2 args
    | "caml_avx512bw_int8x32_cmplt_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:1 args
    | "caml_avx512bw_int8x32_cmpneq_unsigned" ->
      instr vpcmpub_K_Y_Ym256 ~i:4 args
    | "caml_avx512bw_int8x32_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_Y_Ym256_K ~i args
    | "caml_avx512bw_int8x32_cmpeq_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:0 args
    | "caml_avx512bw_int8x32_cmpge_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:5 args
    | "caml_avx512bw_int8x32_cmpgt_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:6 args
    | "caml_avx512bw_int8x32_cmple_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:2 args
    | "caml_avx512bw_int8x32_cmplt_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:1 args
    | "caml_avx512bw_int8x32_cmpneq_unsigned_mask" ->
      instr vpcmpub_K_Y_Ym256_K ~i:4 args
    | "caml_avx512bw_int8x16_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_X_Xm128 ~i args
    | "caml_avx512bw_int8x16_cmpeq_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:0 args
    | "caml_avx512bw_int8x16_cmpge_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:5 args
    | "caml_avx512bw_int8x16_cmpgt_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:6 args
    | "caml_avx512bw_int8x16_cmple_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:2 args
    | "caml_avx512bw_int8x16_cmplt_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:1 args
    | "caml_avx512bw_int8x16_cmpneq_unsigned" ->
      instr vpcmpub_K_X_Xm128 ~i:4 args
    | "caml_avx512bw_int8x16_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_X_Xm128_K ~i args
    | "caml_avx512bw_int8x16_cmpeq_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:0 args
    | "caml_avx512bw_int8x16_cmpge_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:5 args
    | "caml_avx512bw_int8x16_cmpgt_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:6 args
    | "caml_avx512bw_int8x16_cmple_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:2 args
    | "caml_avx512bw_int8x16_cmplt_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:1 args
    | "caml_avx512bw_int8x16_cmpneq_unsigned_mask" ->
      instr vpcmpub_K_X_Xm128_K ~i:4 args
    | "caml_avx512bw_int16x16_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_Y_Ym256 ~i args
    | "caml_avx512bw_int16x16_cmpeq_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:0 args
    | "caml_avx512bw_int16x16_cmpge_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:5 args
    | "caml_avx512bw_int16x16_cmpgt_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:6 args
    | "caml_avx512bw_int16x16_cmple_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:2 args
    | "caml_avx512bw_int16x16_cmplt_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:1 args
    | "caml_avx512bw_int16x16_cmpneq_unsigned" ->
      instr vpcmpuw_K_Y_Ym256 ~i:4 args
    | "caml_avx512bw_int16x16_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_Y_Ym256_K ~i args
    | "caml_avx512bw_int16x16_cmpeq_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:0 args
    | "caml_avx512bw_int16x16_cmpge_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:5 args
    | "caml_avx512bw_int16x16_cmpgt_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:6 args
    | "caml_avx512bw_int16x16_cmple_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:2 args
    | "caml_avx512bw_int16x16_cmplt_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:1 args
    | "caml_avx512bw_int16x16_cmpneq_unsigned_mask" ->
      instr vpcmpuw_K_Y_Ym256_K ~i:4 args
    | "caml_avx512bw_int16x8_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_X_Xm128 ~i args
    | "caml_avx512bw_int16x8_cmpeq_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:0 args
    | "caml_avx512bw_int16x8_cmpge_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:5 args
    | "caml_avx512bw_int16x8_cmpgt_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:6 args
    | "caml_avx512bw_int16x8_cmple_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:2 args
    | "caml_avx512bw_int16x8_cmplt_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:1 args
    | "caml_avx512bw_int16x8_cmpneq_unsigned" ->
      instr vpcmpuw_K_X_Xm128 ~i:4 args
    | "caml_avx512bw_int16x8_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_X_Xm128_K ~i args
    | "caml_avx512bw_int16x8_cmpeq_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:0 args
    | "caml_avx512bw_int16x8_cmpge_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:5 args
    | "caml_avx512bw_int16x8_cmpgt_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:6 args
    | "caml_avx512bw_int16x8_cmple_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:2 args
    | "caml_avx512bw_int16x8_cmplt_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:1 args
    | "caml_avx512bw_int16x8_cmpneq_unsigned_mask" ->
      instr vpcmpuw_K_X_Xm128_K ~i:4 args
    | "caml_avx512bw_int16x16_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_Y_Ym256 ~i args
    | "caml_avx512bw_int16x16_cmpeq" -> instr vpcmpw_K_Y_Ym256 ~i:0 args
    | "caml_avx512bw_int16x16_cmpge" -> instr vpcmpw_K_Y_Ym256 ~i:5 args
    | "caml_avx512bw_int16x16_cmpgt" -> instr vpcmpw_K_Y_Ym256 ~i:6 args
    | "caml_avx512bw_int16x16_cmple" -> instr vpcmpw_K_Y_Ym256 ~i:2 args
    | "caml_avx512bw_int16x16_cmplt" -> instr vpcmpw_K_Y_Ym256 ~i:1 args
    | "caml_avx512bw_int16x16_cmpneq" -> instr vpcmpw_K_Y_Ym256 ~i:4 args
    | "caml_avx512bw_int16x16_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_Y_Ym256_K ~i args
    | "caml_avx512bw_int16x16_cmpeq_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:0 args
    | "caml_avx512bw_int16x16_cmpge_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:5 args
    | "caml_avx512bw_int16x16_cmpgt_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:6 args
    | "caml_avx512bw_int16x16_cmple_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:2 args
    | "caml_avx512bw_int16x16_cmplt_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:1 args
    | "caml_avx512bw_int16x16_cmpneq_mask" -> instr vpcmpw_K_Y_Ym256_K ~i:4 args
    | "caml_avx512bw_int16x8_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_X_Xm128 ~i args
    | "caml_avx512bw_int16x8_cmpeq" -> instr vpcmpw_K_X_Xm128 ~i:0 args
    | "caml_avx512bw_int16x8_cmpge" -> instr vpcmpw_K_X_Xm128 ~i:5 args
    | "caml_avx512bw_int16x8_cmpgt" -> instr vpcmpw_K_X_Xm128 ~i:6 args
    | "caml_avx512bw_int16x8_cmple" -> instr vpcmpw_K_X_Xm128 ~i:2 args
    | "caml_avx512bw_int16x8_cmplt" -> instr vpcmpw_K_X_Xm128 ~i:1 args
    | "caml_avx512bw_int16x8_cmpneq" -> instr vpcmpw_K_X_Xm128 ~i:4 args
    | "caml_avx512bw_int16x8_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_X_Xm128_K ~i args
    | "caml_avx512bw_int16x8_cmpeq_mask" -> instr vpcmpw_K_X_Xm128_K ~i:0 args
    | "caml_avx512bw_int16x8_cmpge_mask" -> instr vpcmpw_K_X_Xm128_K ~i:5 args
    | "caml_avx512bw_int16x8_cmpgt_mask" -> instr vpcmpw_K_X_Xm128_K ~i:6 args
    | "caml_avx512bw_int16x8_cmple_mask" -> instr vpcmpw_K_X_Xm128_K ~i:2 args
    | "caml_avx512bw_int16x8_cmplt_mask" -> instr vpcmpw_K_X_Xm128_K ~i:1 args
    | "caml_avx512bw_int16x8_cmpneq_mask" -> instr vpcmpw_K_X_Xm128_K ~i:4 args
    | "caml_avx512bw_int8x32_test_mask" -> instr vptestmb_K_Y_Ym256_K args
    | "caml_avx512bw_int8x32_test" -> instr vptestmb_K_Y_Ym256 args
    | "caml_avx512bw_int8x16_test_mask" -> instr vptestmb_K_X_Xm128_K args
    | "caml_avx512bw_int8x16_test" -> instr vptestmb_K_X_Xm128 args
    | "caml_avx512bw_int16x16_test_mask" -> instr vptestmw_K_Y_Ym256_K args
    | "caml_avx512bw_int16x16_test" -> instr vptestmw_K_Y_Ym256 args
    | "caml_avx512bw_int16x8_test_mask" -> instr vptestmw_K_X_Xm128_K args
    | "caml_avx512bw_int16x8_test" -> instr vptestmw_K_X_Xm128 args
    | "caml_avx512bw_int8x32_testn_mask" -> instr vptestnmb_K_Y_Ym256_K args
    | "caml_avx512bw_int8x32_testn" -> instr vptestnmb_K_Y_Ym256 args
    | "caml_avx512bw_int8x16_testn_mask" -> instr vptestnmb_K_X_Xm128_K args
    | "caml_avx512bw_int8x16_testn" -> instr vptestnmb_K_X_Xm128 args
    | "caml_avx512bw_int16x16_testn_mask" -> instr vptestnmw_K_Y_Ym256_K args
    | "caml_avx512bw_int16x16_testn" -> instr vptestnmw_K_Y_Ym256 args
    | "caml_avx512bw_int16x8_testn_mask" -> instr vptestnmw_K_X_Xm128_K args
    | "caml_avx512bw_int16x8_testn" -> instr vptestnmw_K_X_Xm128 args
    | "caml_avx512bw_int16x16_sllv_mask" -> instr vpsllvw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_sllv_maskz" ->
      instr (vpsllvw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x16_sllv" -> instr vpsllvw_Y_Y_Ym256 args
    | "caml_avx512bw_int16x8_sllv_mask" -> instr vpsllvw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_sllv_maskz" ->
      instr (vpsllvw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_sllv" -> instr vpsllvw_X_X_Xm128 args
    | "caml_avx512bw_int16x16_sll_mask" -> instr vpsllw_Y_Y_Xm128_K_merge args
    | "caml_avx512bw_int16x16_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllw_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int16x16_sll_maskz" ->
      instr (vpsllw_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllw_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int16x8_sll_mask" -> instr vpsllw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllw_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int16x8_sll_maskz" ->
      instr (vpsllw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllw_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int16x16_srav_mask" -> instr vpsravw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_srav_maskz" ->
      instr (vpsravw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x16_srav" -> instr vpsravw_Y_Y_Ym256 args
    | "caml_avx512bw_int16x8_srav_mask" -> instr vpsravw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_srav_maskz" ->
      instr (vpsravw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_srav" -> instr vpsravw_X_X_Xm128 args
    | "caml_avx512bw_int16x16_sra_mask" -> instr vpsraw_Y_Y_Xm128_K_merge args
    | "caml_avx512bw_int16x16_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraw_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int16x16_sra_maskz" ->
      instr (vpsraw_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsraw_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int16x8_sra_mask" -> instr vpsraw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraw_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int16x8_sra_maskz" ->
      instr (vpsraw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsraw_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_int16x16_srlv_mask" -> instr vpsrlvw_Y_Y_Ym256_K_merge args
    | "caml_avx512bw_int16x16_srlv_maskz" ->
      instr (vpsrlvw_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512bw_int16x16_srlv" -> instr vpsrlvw_Y_Y_Ym256 args
    | "caml_avx512bw_int16x8_srlv_mask" -> instr vpsrlvw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_srlv_maskz" ->
      instr (vpsrlvw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_srlv" -> instr vpsrlvw_X_X_Xm128 args
    | "caml_avx512bw_int16x16_srl_mask" -> instr vpsrlw_Y_Y_Xm128_K_merge args
    | "caml_avx512bw_int16x16_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlw_Y_Ym256_K_merge ~i args
    | "caml_avx512bw_int16x16_srl_maskz" ->
      instr (vpsrlw_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x16_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlw_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512bw_int16x8_srl_mask" -> instr vpsrlw_X_X_Xm128_K_merge args
    | "caml_avx512bw_int16x8_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlw_X_Xm128_K_merge ~i args
    | "caml_avx512bw_int16x8_srl_maskz" ->
      instr (vpsrlw_X_X_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x8_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlw_X_Xm128_K ~z:true) ~i args
    | "caml_avx512bw_mask64_unpack" -> instr kunpckdq args
    | "caml_avx512bw_mask32_unpack" -> instr kunpckwd args
    | "caml_avx512bw_int8x64_dbsad_unsigned" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_Z_Z_Zm512 ~i args
    | "caml_avx512bw_int8x64_dbsad_unsigned_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vdbpsadbw_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int8x64_dbsad_unsigned_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vdbpsadbw_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int8x64_align_right" ->
      let i, args = extract_constant args ~max:31 op in
      instr vpalignr_Z_Z_Zm512 ~i args
    | "caml_avx512bw_int8x64_align_right_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpalignr_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int8x64_align_right_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpalignr_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int8x64_blend" ->
      instr (vpblendmb_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512bw_int16x32_blend" ->
      instr (vpblendmw_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512bw_int8x64_broadcast" -> instr vpbroadcastb_Z_Xm8 args
    | "caml_avx512bw_int8x64_broadcast_mask" ->
      instr vpbroadcastb_Z_Xm8_K_merge args
    | "caml_avx512bw_int8x64_broadcast_maskz" ->
      instr (vpbroadcastb_Z_Xm8_K ~z:true) args
    | "caml_avx512bw_int16x32_broadcast" -> instr vpbroadcastw_Z_Xm16 args
    | "caml_avx512bw_int16x32_broadcast_mask" ->
      instr vpbroadcastw_Z_Xm16_K_merge args
    | "caml_avx512bw_int16x32_broadcast_maskz" ->
      instr (vpbroadcastw_Z_Xm16_K ~z:true) args
    | "caml_avx512bw_int16x32_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2w_Z_Z_Zm512_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512bw_int16x32_permutex2var_mask" ->
      instr (vpermt2w_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512bw_int16x32_permutex2var_maskz" ->
      instr (vpermt2w_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_permutex2var" -> instr vpermt2w_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_permutexvar_mask" ->
      instr vpermw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_permutexvar_maskz" ->
      instr (vpermw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_permutexvar" -> instr vpermw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_movepi_mask" -> instr vpmovb2m_K_Z args
    | "caml_avx512bw_int8x64_movm" -> instr vpmovm2b_Z_K args
    | "caml_avx512bw_int16x32_movm" -> instr vpmovm2w_Z_K args
    | "caml_avx512bw_int16x32_movepi_mask" -> instr vpmovw2m_K_Z args
    | "caml_avx512bw_int8x64_sad" -> instr vpsadbw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_shuffle_mask" ->
      instr vpshufb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_shuffle_maskz" ->
      instr (vpshufb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_shuffle" -> instr vpshufb_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_shuffle_high_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufhw_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int16x32_shuffle_high_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufhw_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int16x32_shuffle_high" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufhw_Z_Zm512 ~i args
    | "caml_avx512bw_int16x32_shuffle_low_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshuflw_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int16x32_shuffle_low_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshuflw_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int16x32_shuffle_low" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshuflw_Z_Zm512 ~i args
    | "caml_avx512bw_int8x64_interleave_high_mask" ->
      instr vpunpckhbw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_interleave_high_maskz" ->
      instr (vpunpckhbw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_interleave_high" -> instr vpunpckhbw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_interleave_high_mask" ->
      instr vpunpckhwd_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_interleave_high_maskz" ->
      instr (vpunpckhwd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_interleave_high" ->
      instr vpunpckhwd_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_interleave_low_mask" ->
      instr vpunpcklbw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_interleave_low_maskz" ->
      instr (vpunpcklbw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_interleave_low" -> instr vpunpcklbw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_interleave_low_mask" ->
      instr vpunpcklwd_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_interleave_low_maskz" ->
      instr (vpunpcklwd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_interleave_low" -> instr vpunpcklwd_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_mov_mask" -> instr vmovdqu16_Z_Z_K_merge args
    | "caml_avx512bw_int16x32_mov_maskz" ->
      instr (vmovdqu16_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_mov_mask" -> instr vmovdqu8_Z_Z_K_merge args
    | "caml_avx512bw_int8x64_mov_maskz" ->
      instr (vmovdqu8_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_abs" -> instr vpabsb_Z_Zm512 args
    | "caml_avx512bw_int8x64_abs_mask" -> instr vpabsb_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_abs_maskz" -> instr (vpabsb_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_abs" -> instr vpabsw_Z_Zm512 args
    | "caml_avx512bw_int16x32_abs_mask" -> instr vpabsw_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_abs_maskz" ->
      instr (vpabsw_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_add" -> instr vpaddb_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_add_mask" -> instr vpaddb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_add_maskz" ->
      instr (vpaddb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_add_saturating" -> instr vpaddsb_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_add_saturating_mask" ->
      instr vpaddsb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_add_saturating_maskz" ->
      instr (vpaddsb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_add_saturating" -> instr vpaddsw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_add_saturating_mask" ->
      instr vpaddsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_add_saturating_maskz" ->
      instr (vpaddsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_add_saturating_unsigned" ->
      instr vpaddusb_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_add_saturating_unsigned_mask" ->
      instr vpaddusb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_add_saturating_unsigned_maskz" ->
      instr (vpaddusb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_add_saturating_unsigned" ->
      instr vpaddusw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_add_saturating_unsigned_mask" ->
      instr vpaddusw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_add_saturating_unsigned_maskz" ->
      instr (vpaddusw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_add" -> instr vpaddw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_add_mask" -> instr vpaddw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_add_maskz" ->
      instr (vpaddw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_avg_unsigned" -> instr vpavgb_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_avg_unsigned_mask" ->
      instr vpavgb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_avg_unsigned_maskz" ->
      instr (vpavgb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_avg_unsigned" -> instr vpavgw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_avg_unsigned_mask" ->
      instr vpavgw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_avg_unsigned_maskz" ->
      instr (vpavgw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_maddubs" -> instr vpmaddubsw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_maddubs_mask" ->
      instr vpmaddubsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_maddubs_maskz" ->
      instr (vpmaddubsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_madd" -> instr vpmaddwd_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_madd_mask" ->
      instr vpmaddwd_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_madd_maskz" ->
      instr (vpmaddwd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_max_mask" -> instr vpmaxsb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_max_maskz" ->
      instr (vpmaxsb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_max" -> instr vpmaxsb_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_max_mask" -> instr vpmaxsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_max_maskz" ->
      instr (vpmaxsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_max" -> instr vpmaxsw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_max_unsigned_mask" ->
      instr vpmaxub_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_max_unsigned_maskz" ->
      instr (vpmaxub_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_max_unsigned" -> instr vpmaxub_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_max_unsigned_mask" ->
      instr vpmaxuw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_max_unsigned_maskz" ->
      instr (vpmaxuw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_max_unsigned" -> instr vpmaxuw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_min_mask" -> instr vpminsb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_min_maskz" ->
      instr (vpminsb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_min" -> instr vpminsb_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_min_mask" -> instr vpminsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_min_maskz" ->
      instr (vpminsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_min" -> instr vpminsw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_min_unsigned_mask" ->
      instr vpminub_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_min_unsigned_maskz" ->
      instr (vpminub_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_min_unsigned" -> instr vpminub_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_min_unsigned_mask" ->
      instr vpminuw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_min_unsigned_maskz" ->
      instr (vpminuw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_min_unsigned" -> instr vpminuw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_mul_round_mask" ->
      instr vpmulhrsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_mul_round_maskz" ->
      instr (vpmulhrsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_mul_round" -> instr vpmulhrsw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_mul_high_unsigned_mask" ->
      instr vpmulhuw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_mul_high_unsigned_maskz" ->
      instr (vpmulhuw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_mul_high_unsigned" ->
      instr vpmulhuw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_mul_high_mask" ->
      instr vpmulhw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_mul_high_maskz" ->
      instr (vpmulhw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_mul_high" -> instr vpmulhw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_mul_low_mask" ->
      instr vpmullw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_mul_low_maskz" ->
      instr (vpmullw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_mul_low" -> instr vpmullw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_sub_mask" -> instr vpsubb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_sub_maskz" ->
      instr (vpsubb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_sub" -> instr vpsubb_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_sub_saturating_mask" ->
      instr vpsubsb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_sub_saturating_maskz" ->
      instr (vpsubsb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_sub_saturating" -> instr vpsubsb_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_sub_saturating_mask" ->
      instr vpsubsw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_sub_saturating_maskz" ->
      instr (vpsubsw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_sub_saturating" -> instr vpsubsw_Z_Z_Zm512 args
    | "caml_avx512bw_int8x64_sub_saturating_unsigned_mask" ->
      instr vpsubusb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int8x64_sub_saturating_unsigned_maskz" ->
      instr (vpsubusb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int8x64_sub_saturating_unsigned" ->
      instr vpsubusb_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_sub_saturating_unsigned_mask" ->
      instr vpsubusw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_sub_saturating_unsigned_maskz" ->
      instr (vpsubusw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_sub_saturating_unsigned" ->
      instr vpsubusw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_sub_mask" -> instr vpsubw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_sub_maskz" ->
      instr (vpsubw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_sub" -> instr vpsubw_Z_Z_Zm512 args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating_mask" ->
      instr vpackssdw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating_maskz" ->
      instr (vpackssdw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating" ->
      instr vpackssdw_Z_Z_Zm512 args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating_mask" ->
      instr vpacksswb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating_maskz" ->
      instr (vpacksswb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating" ->
      instr vpacksswb_Z_Z_Zm512 args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating_unsigned_mask" ->
      instr vpackusdw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating_unsigned_maskz" ->
      instr (vpackusdw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_cvt_int32x16_int16x32_saturating_unsigned" ->
      instr vpackusdw_Z_Z_Zm512 args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating_unsigned_mask" ->
      instr vpackuswb_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating_unsigned_maskz" ->
      instr (vpackuswb_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_cvt_int16x32_int8x64_saturating_unsigned" ->
      instr vpackuswb_Z_Z_Zm512 args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating" ->
      instr vpmovswb_Ym256_Z args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_mask" ->
      instr vpmovswb_Y_Z_K_merge args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_maskz" ->
      instr (vpmovswb_Ym256_Z_K ~z:true) args
    | "caml_avx512bw_cvtsx_int8x32_int16x32" -> instr vpmovsxbw_Z_Ym256 args
    | "caml_avx512bw_cvtsx_int8x32_int16x32_mask" ->
      instr vpmovsxbw_Z_Ym256_K_merge args
    | "caml_avx512bw_cvtsx_int8x32_int16x32_maskz" ->
      instr (vpmovsxbw_Z_Ym256_K ~z:true) args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_unsigned" ->
      instr vpmovuswb_Ym256_Z args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_unsigned_mask" ->
      instr vpmovuswb_Y_Z_K_merge args
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_unsigned_maskz" ->
      instr (vpmovuswb_Ym256_Z_K ~z:true) args
    | "caml_avx512bw_cvt_int16x32_int8x32" -> instr vpmovwb_Ym256_Z args
    | "caml_avx512bw_cvt_int16x32_int8x32_mask" ->
      instr vpmovwb_Y_Z_K_merge args
    | "caml_avx512bw_cvt_int16x32_int8x32_maskz" ->
      instr (vpmovwb_Ym256_Z_K ~z:true) args
    | "caml_avx512bw_cvtzx_int8x32_int16x32" -> instr vpmovzxbw_Z_Ym256 args
    | "caml_avx512bw_cvtzx_int8x32_int16x32_mask" ->
      instr vpmovzxbw_Z_Ym256_K_merge args
    | "caml_avx512bw_cvtzx_int8x32_int16x32_maskz" ->
      instr (vpmovzxbw_Z_Ym256_K ~z:true) args
    | "caml_avx512bw_int8x64_set1_mask" -> instr vpbroadcastb_Z_r32_K_merge args
    | "caml_avx512bw_int8x64_set1_maskz" ->
      instr (vpbroadcastb_Z_r32_K ~z:true) args
    | "caml_avx512bw_int16x32_set1_mask" ->
      instr vpbroadcastw_Z_r32_K_merge args
    | "caml_avx512bw_int16x32_set1_maskz" ->
      instr (vpbroadcastw_Z_r32_K ~z:true) args
    | "caml_avx512bw_int8x64_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_Z_Zm512 ~i args
    | "caml_avx512bw_int8x64_cmpeq" -> instr vpcmpb_K_Z_Zm512 ~i:0 args
    | "caml_avx512bw_int8x64_cmpge" -> instr vpcmpb_K_Z_Zm512 ~i:5 args
    | "caml_avx512bw_int8x64_cmpgt" -> instr vpcmpb_K_Z_Zm512 ~i:6 args
    | "caml_avx512bw_int8x64_cmple" -> instr vpcmpb_K_Z_Zm512 ~i:2 args
    | "caml_avx512bw_int8x64_cmplt" -> instr vpcmpb_K_Z_Zm512 ~i:1 args
    | "caml_avx512bw_int8x64_cmpneq" -> instr vpcmpb_K_Z_Zm512 ~i:4 args
    | "caml_avx512bw_int8x64_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpb_K_Z_Zm512_K ~i args
    | "caml_avx512bw_int8x64_cmpeq_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:0 args
    | "caml_avx512bw_int8x64_cmpge_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:5 args
    | "caml_avx512bw_int8x64_cmpgt_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:6 args
    | "caml_avx512bw_int8x64_cmple_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:2 args
    | "caml_avx512bw_int8x64_cmplt_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:1 args
    | "caml_avx512bw_int8x64_cmpneq_mask" -> instr vpcmpb_K_Z_Zm512_K ~i:4 args
    | "caml_avx512bw_int8x64_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_Z_Zm512 ~i args
    | "caml_avx512bw_int8x64_cmpeq_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:0 args
    | "caml_avx512bw_int8x64_cmpge_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:5 args
    | "caml_avx512bw_int8x64_cmpgt_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:6 args
    | "caml_avx512bw_int8x64_cmple_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:2 args
    | "caml_avx512bw_int8x64_cmplt_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:1 args
    | "caml_avx512bw_int8x64_cmpneq_unsigned" ->
      instr vpcmpub_K_Z_Zm512 ~i:4 args
    | "caml_avx512bw_int8x64_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpub_K_Z_Zm512_K ~i args
    | "caml_avx512bw_int8x64_cmpeq_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:0 args
    | "caml_avx512bw_int8x64_cmpge_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:5 args
    | "caml_avx512bw_int8x64_cmpgt_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:6 args
    | "caml_avx512bw_int8x64_cmple_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:2 args
    | "caml_avx512bw_int8x64_cmplt_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:1 args
    | "caml_avx512bw_int8x64_cmpneq_unsigned_mask" ->
      instr vpcmpub_K_Z_Zm512_K ~i:4 args
    | "caml_avx512bw_int16x32_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_Z_Zm512 ~i args
    | "caml_avx512bw_int16x32_cmpeq_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:0 args
    | "caml_avx512bw_int16x32_cmpge_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:5 args
    | "caml_avx512bw_int16x32_cmpgt_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:6 args
    | "caml_avx512bw_int16x32_cmple_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:2 args
    | "caml_avx512bw_int16x32_cmplt_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:1 args
    | "caml_avx512bw_int16x32_cmpneq_unsigned" ->
      instr vpcmpuw_K_Z_Zm512 ~i:4 args
    | "caml_avx512bw_int16x32_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuw_K_Z_Zm512_K ~i args
    | "caml_avx512bw_int16x32_cmpeq_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:0 args
    | "caml_avx512bw_int16x32_cmpge_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:5 args
    | "caml_avx512bw_int16x32_cmpgt_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:6 args
    | "caml_avx512bw_int16x32_cmple_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:2 args
    | "caml_avx512bw_int16x32_cmplt_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:1 args
    | "caml_avx512bw_int16x32_cmpneq_unsigned_mask" ->
      instr vpcmpuw_K_Z_Zm512_K ~i:4 args
    | "caml_avx512bw_int16x32_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_Z_Zm512 ~i args
    | "caml_avx512bw_int16x32_cmpeq" -> instr vpcmpw_K_Z_Zm512 ~i:0 args
    | "caml_avx512bw_int16x32_cmpge" -> instr vpcmpw_K_Z_Zm512 ~i:5 args
    | "caml_avx512bw_int16x32_cmpgt" -> instr vpcmpw_K_Z_Zm512 ~i:6 args
    | "caml_avx512bw_int16x32_cmple" -> instr vpcmpw_K_Z_Zm512 ~i:2 args
    | "caml_avx512bw_int16x32_cmplt" -> instr vpcmpw_K_Z_Zm512 ~i:1 args
    | "caml_avx512bw_int16x32_cmpneq" -> instr vpcmpw_K_Z_Zm512 ~i:4 args
    | "caml_avx512bw_int16x32_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpw_K_Z_Zm512_K ~i args
    | "caml_avx512bw_int16x32_cmpeq_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:0 args
    | "caml_avx512bw_int16x32_cmpge_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:5 args
    | "caml_avx512bw_int16x32_cmpgt_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:6 args
    | "caml_avx512bw_int16x32_cmple_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:2 args
    | "caml_avx512bw_int16x32_cmplt_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:1 args
    | "caml_avx512bw_int16x32_cmpneq_mask" -> instr vpcmpw_K_Z_Zm512_K ~i:4 args
    | "caml_avx512bw_int8x64_test_mask" -> instr vptestmb_K_Z_Zm512_K args
    | "caml_avx512bw_int8x64_test" -> instr vptestmb_K_Z_Zm512 args
    | "caml_avx512bw_int16x32_test_mask" -> instr vptestmw_K_Z_Zm512_K args
    | "caml_avx512bw_int16x32_test" -> instr vptestmw_K_Z_Zm512 args
    | "caml_avx512bw_int8x64_testn_mask" -> instr vptestnmb_K_Z_Zm512_K args
    | "caml_avx512bw_int8x64_testn" -> instr vptestnmb_K_Z_Zm512 args
    | "caml_avx512bw_int16x32_testn_mask" -> instr vptestnmw_K_Z_Zm512_K args
    | "caml_avx512bw_int16x32_testn" -> instr vptestnmw_K_Z_Zm512 args
    | "caml_avx512bw_int16x32_sllv_mask" -> instr vpsllvw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_sllv_maskz" ->
      instr (vpsllvw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_sllv" -> instr vpsllvw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_sll_mask" -> instr vpsllw_Z_Z_Xm128_K_merge args
    | "caml_avx512bw_int16x32_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllw_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int16x32_sll_maskz" ->
      instr (vpsllw_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x32_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllw_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int16x32_sll" -> instr vpsllw_Z_Z_Xm128 args
    | "caml_avx512bw_int16x32_slli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllw_Z_Zm512 ~i args
    | "caml_avx512bw_int16x32_srav_mask" -> instr vpsravw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_srav_maskz" ->
      instr (vpsravw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_srav" -> instr vpsravw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_sra_mask" -> instr vpsraw_Z_Z_Xm128_K_merge args
    | "caml_avx512bw_int16x32_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraw_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int16x32_sra_maskz" ->
      instr (vpsraw_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x32_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsraw_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int16x32_sra" -> instr vpsraw_Z_Z_Xm128 args
    | "caml_avx512bw_int16x32_srai" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraw_Z_Zm512 ~i args
    | "caml_avx512bw_int16x32_srlv_mask" -> instr vpsrlvw_Z_Z_Zm512_K_merge args
    | "caml_avx512bw_int16x32_srlv_maskz" ->
      instr (vpsrlvw_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512bw_int16x32_srlv" -> instr vpsrlvw_Z_Z_Zm512 args
    | "caml_avx512bw_int16x32_srl_mask" -> instr vpsrlw_Z_Z_Xm128_K_merge args
    | "caml_avx512bw_int16x32_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlw_Z_Zm512_K_merge ~i args
    | "caml_avx512bw_int16x32_srl_maskz" ->
      instr (vpsrlw_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512bw_int16x32_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlw_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512bw_int16x32_srl" -> instr vpsrlw_Z_Z_Xm128 args
    | "caml_avx512bw_int16x32_srli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlw_Z_Zm512 ~i args
    | "caml_avx512bw_mask32_add" -> instr kaddd args
    | "caml_avx512bw_mask64_add" -> instr kaddq args
    | "caml_avx512bw_mask32_and" -> instr kandd args
    | "caml_avx512bw_mask64_and" -> instr kandq args
    | "caml_avx512bw_mask32_andnot" -> instr kandnd args
    | "caml_avx512bw_mask64_andnot" -> instr kandnq args
    | "caml_avx512bw_mask32_not" -> instr knotd args
    | "caml_avx512bw_mask64_not" -> instr knotq args
    | "caml_avx512bw_mask32_or" -> instr kord args
    | "caml_avx512bw_mask64_or" -> instr korq args
    | "caml_avx512bw_mask32_xnor" -> instr kxnord args
    | "caml_avx512bw_mask64_xnor" -> instr kxnorq args
    | "caml_avx512bw_mask32_xor" -> instr kxord args
    | "caml_avx512bw_mask64_xor" -> instr kxorq args
    | "caml_avx512bw_mask32_shift_left" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftld ~i args
    | "caml_avx512bw_mask64_shift_left" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftlq ~i args
    | "caml_avx512bw_mask32_shift_right" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftrd ~i args
    | "caml_avx512bw_mask64_shift_right" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftrq ~i args
    | "caml_avx512bw_mask32_to_int32" -> instr kmovd_r32_K args
    | "caml_avx512bw_mask64_to_int64" -> instr kmovq_r64_K args
    | "caml_avx512bw_int32_to_mask32" -> instr kmovd_K_r32 args
    | "caml_avx512bw_int64_to_mask64" -> instr kmovq_K_r64 args
    | "caml_avx512cd_int64x4_broadcastmask" -> instr vpbroadcastmb2q_Y_K args
    | "caml_avx512cd_int64x2_broadcastmask" -> instr vpbroadcastmb2q_X_K args
    | "caml_avx512cd_int32x8_broadcastmask" -> instr vpbroadcastmw2d_Y_K args
    | "caml_avx512cd_int32x4_broadcastmask" -> instr vpbroadcastmw2d_X_K args
    | "caml_avx512cd_int32x8_conflict" -> instr vpconflictd_Y_Ym256 args
    | "caml_avx512cd_int32x8_conflict_mask" ->
      instr vpconflictd_Y_Ym256_K_merge args
    | "caml_avx512cd_int32x8_conflict_maskz" ->
      instr (vpconflictd_Y_Ym256_K ~z:true) args
    | "caml_avx512cd_int32x4_conflict" -> instr vpconflictd_X_Xm128 args
    | "caml_avx512cd_int32x4_conflict_mask" ->
      instr vpconflictd_X_Xm128_K_merge args
    | "caml_avx512cd_int32x4_conflict_maskz" ->
      instr (vpconflictd_X_Xm128_K ~z:true) args
    | "caml_avx512cd_int64x4_conflict" -> instr vpconflictq_Y_Ym256 args
    | "caml_avx512cd_int64x4_conflict_mask" ->
      instr vpconflictq_Y_Ym256_K_merge args
    | "caml_avx512cd_int64x4_conflict_maskz" ->
      instr (vpconflictq_Y_Ym256_K ~z:true) args
    | "caml_avx512cd_int64x2_conflict" -> instr vpconflictq_X_Xm128 args
    | "caml_avx512cd_int64x2_conflict_mask" ->
      instr vpconflictq_X_Xm128_K_merge args
    | "caml_avx512cd_int64x2_conflict_maskz" ->
      instr (vpconflictq_X_Xm128_K ~z:true) args
    | "caml_avx512cd_int32x8_lzcnt" -> instr vplzcntd_Y_Ym256 args
    | "caml_avx512cd_int32x8_lzcnt_mask" -> instr vplzcntd_Y_Ym256_K_merge args
    | "caml_avx512cd_int32x8_lzcnt_maskz" ->
      instr (vplzcntd_Y_Ym256_K ~z:true) args
    | "caml_avx512cd_int32x4_lzcnt" -> instr vplzcntd_X_Xm128 args
    | "caml_avx512cd_int32x4_lzcnt_mask" -> instr vplzcntd_X_Xm128_K_merge args
    | "caml_avx512cd_int32x4_lzcnt_maskz" ->
      instr (vplzcntd_X_Xm128_K ~z:true) args
    | "caml_avx512cd_int64x4_lzcnt" -> instr vplzcntq_Y_Ym256 args
    | "caml_avx512cd_int64x4_lzcnt_mask" -> instr vplzcntq_Y_Ym256_K_merge args
    | "caml_avx512cd_int64x4_lzcnt_maskz" ->
      instr (vplzcntq_Y_Ym256_K ~z:true) args
    | "caml_avx512cd_int64x2_lzcnt" -> instr vplzcntq_X_Xm128 args
    | "caml_avx512cd_int64x2_lzcnt_mask" -> instr vplzcntq_X_Xm128_K_merge args
    | "caml_avx512cd_int64x2_lzcnt_maskz" ->
      instr (vplzcntq_X_Xm128_K ~z:true) args
    | "caml_avx512cd_int64x8_broadcastmask" -> instr vpbroadcastmb2q_Z_K args
    | "caml_avx512cd_int32x16_broadcastmask" -> instr vpbroadcastmw2d_Z_K args
    | "caml_avx512cd_int32x16_conflict" -> instr vpconflictd_Z_Zm512 args
    | "caml_avx512cd_int32x16_conflict_mask" ->
      instr vpconflictd_Z_Zm512_K_merge args
    | "caml_avx512cd_int32x16_conflict_maskz" ->
      instr (vpconflictd_Z_Zm512_K ~z:true) args
    | "caml_avx512cd_int64x8_conflict" -> instr vpconflictq_Z_Zm512 args
    | "caml_avx512cd_int64x8_conflict_mask" ->
      instr vpconflictq_Z_Zm512_K_merge args
    | "caml_avx512cd_int64x8_conflict_maskz" ->
      instr (vpconflictq_Z_Zm512_K ~z:true) args
    | "caml_avx512cd_int32x16_lzcnt" -> instr vplzcntd_Z_Zm512 args
    | "caml_avx512cd_int32x16_lzcnt_mask" -> instr vplzcntd_Z_Zm512_K_merge args
    | "caml_avx512cd_int32x16_lzcnt_maskz" ->
      instr (vplzcntd_Z_Zm512_K ~z:true) args
    | "caml_avx512cd_int64x8_lzcnt" -> instr vplzcntq_Z_Zm512 args
    | "caml_avx512cd_int64x8_lzcnt_mask" -> instr vplzcntq_Z_Zm512_K_merge args
    | "caml_avx512cd_int64x8_lzcnt_maskz" ->
      instr (vplzcntq_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float64x4_andnot_mask" ->
      instr vandnpd_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float64x4_andnot_maskz" ->
      instr (vandnpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float64x2_andnot_mask" ->
      instr vandnpd_X_X_Xm128_K_merge args
    | "caml_avx512dq_float64x2_andnot_maskz" ->
      instr (vandnpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float32x8_andnot_mask" ->
      instr vandnps_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float32x8_andnot_maskz" ->
      instr (vandnps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float32x4_andnot_mask" ->
      instr vandnps_X_X_Xm128_K_merge args
    | "caml_avx512dq_float32x4_andnot_maskz" ->
      instr (vandnps_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float64x4_and_mask" -> instr vandpd_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float64x4_and_maskz" ->
      instr (vandpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float64x2_and_mask" -> instr vandpd_X_X_Xm128_K_merge args
    | "caml_avx512dq_float64x2_and_maskz" ->
      instr (vandpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float32x8_and_mask" -> instr vandps_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float32x8_and_maskz" ->
      instr (vandps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float32x4_and_mask" -> instr vandps_X_X_Xm128_K_merge args
    | "caml_avx512dq_float32x4_and_maskz" ->
      instr (vandps_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float64x4_or_mask" -> instr vorpd_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float64x4_or_maskz" ->
      instr (vorpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float64x2_or_mask" -> instr vorpd_X_X_Xm128_K_merge args
    | "caml_avx512dq_float64x2_or_maskz" ->
      instr (vorpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float32x8_or_mask" -> instr vorps_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float32x8_or_maskz" ->
      instr (vorps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float32x4_or_mask" -> instr vorps_X_X_Xm128_K_merge args
    | "caml_avx512dq_float32x4_or_maskz" ->
      instr (vorps_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float64x4_xor_mask" -> instr vxorpd_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float64x4_xor_maskz" ->
      instr (vxorpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float64x2_xor_mask" -> instr vxorpd_X_X_Xm128_K_merge args
    | "caml_avx512dq_float64x2_xor_maskz" ->
      instr (vxorpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float32x8_xor_mask" -> instr vxorps_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_float32x8_xor_maskz" ->
      instr (vxorps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_float32x4_xor_mask" -> instr vxorps_X_X_Xm128_K_merge args
    | "caml_avx512dq_float32x4_xor_maskz" ->
      instr (vxorps_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_float32x8_broadcast_f32x2" ->
      instr vbroadcastf32x2_Y_Xm64 args
    | "caml_avx512dq_float32x8_broadcast_f32x2_mask" ->
      instr vbroadcastf32x2_Y_Xm64_K_merge args
    | "caml_avx512dq_float32x8_broadcast_f32x2_maskz" ->
      instr (vbroadcastf32x2_Y_Xm64_K ~z:true) args
    | "caml_avx512dq_int32x8_broadcast_i32x2" ->
      instr vbroadcasti32x2_Y_Xm64 args
    | "caml_avx512dq_int32x8_broadcast_i32x2_mask" ->
      instr vbroadcasti32x2_Y_Xm64_K_merge args
    | "caml_avx512dq_int32x8_broadcast_i32x2_maskz" ->
      instr (vbroadcasti32x2_Y_Xm64_K ~z:true) args
    | "caml_avx512dq_int32x4_broadcast_i32x2" ->
      instr vbroadcasti32x2_X_Xm64 args
    | "caml_avx512dq_int32x4_broadcast_i32x2_mask" ->
      instr vbroadcasti32x2_X_Xm64_K_merge args
    | "caml_avx512dq_int32x4_broadcast_i32x2_maskz" ->
      instr (vbroadcasti32x2_X_Xm64_K ~z:true) args
    | "caml_avx512dq_float64x4_extract_float64x2" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf64x2_Xm128_Y ~i args
    | "caml_avx512dq_float64x4_extract_float64x2_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf64x2_X_Y_K_merge ~i args
    | "caml_avx512dq_float64x4_extract_float64x2_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextractf64x2_Xm128_Y_K ~z:true) ~i args
    | "caml_avx512dq_int64x4_extract_int64x2" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti64x2_Xm128_Y ~i args
    | "caml_avx512dq_int64x4_extract_int64x2_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti64x2_X_Y_K_merge ~i args
    | "caml_avx512dq_int64x4_extract_int64x2_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextracti64x2_Xm128_Y_K ~z:true) ~i args
    | "caml_avx512dq_float64x4_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Ym256 ~i args
    | "caml_avx512dq_float64x4_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Ym256_K ~i args
    | "caml_avx512dq_float64x2_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Xm128 ~i args
    | "caml_avx512dq_float64x2_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Xm128_K ~i args
    | "caml_avx512dq_float32x8_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Ym256 ~i args
    | "caml_avx512dq_float32x8_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Ym256_K ~i args
    | "caml_avx512dq_float32x4_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Xm128 ~i args
    | "caml_avx512dq_float32x4_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Xm128_K ~i args
    | "caml_avx512dq_float64x4_insert_float64x2" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf64x2_Y_Y_Xm128 ~i args
    | "caml_avx512dq_float64x4_insert_float64x2_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf64x2_Y_Y_Xm128_K_merge ~i args
    | "caml_avx512dq_float64x4_insert_float64x2_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinsertf64x2_Y_Y_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_int64x4_insert_int64x2" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti64x2_Y_Y_Xm128 ~i args
    | "caml_avx512dq_int64x4_insert_int64x2_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti64x2_Y_Y_Xm128_K_merge ~i args
    | "caml_avx512dq_int64x4_insert_int64x2_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinserti64x2_Y_Y_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_int32x8_movepi_mask" -> instr vpmovd2m_K_Y args
    | "caml_avx512dq_int32x4_movepi_mask" -> instr vpmovd2m_K_X args
    | "caml_avx512dq_int32x8_movm" -> instr vpmovm2d_Y_K args
    | "caml_avx512dq_int32x4_movm" -> instr vpmovm2d_X_K args
    | "caml_avx512dq_int64x4_movm" -> instr vpmovm2q_Y_K args
    | "caml_avx512dq_int64x2_movm" -> instr vpmovm2q_X_K args
    | "caml_avx512dq_int64x4_movepi_mask" -> instr vpmovq2m_K_Y args
    | "caml_avx512dq_int64x2_movepi_mask" -> instr vpmovq2m_K_X args
    | "caml_avx512dq_float64x4_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512dq_float64x4_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_float64x4_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_Y_Y_Ym256 ~i args
    | "caml_avx512dq_float64x2_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_X_X_Xm128_K_merge ~i args
    | "caml_avx512dq_float64x2_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_float64x2_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_X_X_Xm128 ~i args
    | "caml_avx512dq_float32x8_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512dq_float32x8_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_float32x8_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_Y_Y_Ym256 ~i args
    | "caml_avx512dq_float32x4_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_X_X_Xm128_K_merge ~i args
    | "caml_avx512dq_float32x4_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_float32x4_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_X_X_Xm128 ~i args
    | "caml_avx512dq_float64x4_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_Y_Ym256_K_merge ~i args
    | "caml_avx512dq_float64x4_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_float64x4_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_Y_Ym256 ~i args
    | "caml_avx512dq_float64x2_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_X_Xm128_K_merge ~i args
    | "caml_avx512dq_float64x2_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_X_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_float64x2_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_X_Xm128 ~i args
    | "caml_avx512dq_float32x8_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_Y_Ym256_K_merge ~i args
    | "caml_avx512dq_float32x8_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_float32x8_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_Y_Ym256 ~i args
    | "caml_avx512dq_float32x4_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_X_Xm128_K_merge ~i args
    | "caml_avx512dq_float32x4_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_X_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_float32x4_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_X_Xm128 ~i args
    | "caml_avx512dq_cvt_float64x4_int64x4" -> instr vcvtpd2qq_Y_Ym256 args
    | "caml_avx512dq_cvt_float64x4_int64x4_mask" ->
      instr vcvtpd2qq_Y_Ym256_K_merge args
    | "caml_avx512dq_cvt_float64x4_int64x4_maskz" ->
      instr (vcvtpd2qq_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_float64x2_int64x2" -> instr vcvtpd2qq_X_Xm128 args
    | "caml_avx512dq_cvt_float64x2_int64x2_mask" ->
      instr vcvtpd2qq_X_Xm128_K_merge args
    | "caml_avx512dq_cvt_float64x2_int64x2_maskz" ->
      instr (vcvtpd2qq_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_float64x4_int64x4_unsigned" ->
      instr vcvtpd2uqq_Y_Ym256 args
    | "caml_avx512dq_cvt_float64x4_int64x4_unsigned_mask" ->
      instr vcvtpd2uqq_Y_Ym256_K_merge args
    | "caml_avx512dq_cvt_float64x4_int64x4_unsigned_maskz" ->
      instr (vcvtpd2uqq_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_float64x2_int64x2_unsigned" ->
      instr vcvtpd2uqq_X_Xm128 args
    | "caml_avx512dq_cvt_float64x2_int64x2_unsigned_mask" ->
      instr vcvtpd2uqq_X_Xm128_K_merge args
    | "caml_avx512dq_cvt_float64x2_int64x2_unsigned_maskz" ->
      instr (vcvtpd2uqq_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_float32x4_int64x4" -> instr vcvtps2qq_Y_Xm128 args
    | "caml_avx512dq_cvt_float32x4_int64x4_mask" ->
      instr vcvtps2qq_Y_Xm128_K_merge args
    | "caml_avx512dq_cvt_float32x4_int64x4_maskz" ->
      instr (vcvtps2qq_Y_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_float32x4_int64x2" -> instr vcvtps2qq_X_Xm64 args
    | "caml_avx512dq_cvt_float32x4_int64x2_mask" ->
      instr vcvtps2qq_X_Xm64_K_merge args
    | "caml_avx512dq_cvt_float32x4_int64x2_maskz" ->
      instr (vcvtps2qq_X_Xm64_K ~z:true) args
    | "caml_avx512dq_cvt_float32x4_int64x4_unsigned" ->
      instr vcvtps2uqq_Y_Xm128 args
    | "caml_avx512dq_cvt_float32x4_int64x4_unsigned_mask" ->
      instr vcvtps2uqq_Y_Xm128_K_merge args
    | "caml_avx512dq_cvt_float32x4_int64x4_unsigned_maskz" ->
      instr (vcvtps2uqq_Y_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_float32x4_int64x2_unsigned" ->
      instr vcvtps2uqq_X_Xm64 args
    | "caml_avx512dq_cvt_float32x4_int64x2_unsigned_mask" ->
      instr vcvtps2uqq_X_Xm64_K_merge args
    | "caml_avx512dq_cvt_float32x4_int64x2_unsigned_maskz" ->
      instr (vcvtps2uqq_X_Xm64_K ~z:true) args
    | "caml_avx512dq_cvt_int64x4_float64x4" -> instr vcvtqq2pd_Y_Ym256 args
    | "caml_avx512dq_cvt_int64x4_float64x4_mask" ->
      instr vcvtqq2pd_Y_Ym256_K_merge args
    | "caml_avx512dq_cvt_int64x4_float64x4_maskz" ->
      instr (vcvtqq2pd_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_int64x2_float64x2" -> instr vcvtqq2pd_X_Xm128 args
    | "caml_avx512dq_cvt_int64x2_float64x2_mask" ->
      instr vcvtqq2pd_X_Xm128_K_merge args
    | "caml_avx512dq_cvt_int64x2_float64x2_maskz" ->
      instr (vcvtqq2pd_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_int64x4_float32x4" -> instr vcvtqq2ps_X_Ym256 args
    | "caml_avx512dq_cvt_int64x4_float32x4_mask" ->
      instr vcvtqq2ps_X_Ym256_K_merge args
    | "caml_avx512dq_cvt_int64x4_float32x4_maskz" ->
      instr (vcvtqq2ps_X_Ym256_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x4_int64x4" -> instr vcvttpd2qq_Y_Ym256 args
    | "caml_avx512dq_cvtt_float64x4_int64x4_mask" ->
      instr vcvttpd2qq_Y_Ym256_K_merge args
    | "caml_avx512dq_cvtt_float64x4_int64x4_maskz" ->
      instr (vcvttpd2qq_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x2_int64x2" -> instr vcvttpd2qq_X_Xm128 args
    | "caml_avx512dq_cvtt_float64x2_int64x2_mask" ->
      instr vcvttpd2qq_X_Xm128_K_merge args
    | "caml_avx512dq_cvtt_float64x2_int64x2_maskz" ->
      instr (vcvttpd2qq_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x4_int64x4_unsigned" ->
      instr vcvttpd2uqq_Y_Ym256 args
    | "caml_avx512dq_cvtt_float64x4_int64x4_unsigned_mask" ->
      instr vcvttpd2uqq_Y_Ym256_K_merge args
    | "caml_avx512dq_cvtt_float64x4_int64x4_unsigned_maskz" ->
      instr (vcvttpd2uqq_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x2_int64x2_unsigned" ->
      instr vcvttpd2uqq_X_Xm128 args
    | "caml_avx512dq_cvtt_float64x2_int64x2_unsigned_mask" ->
      instr vcvttpd2uqq_X_Xm128_K_merge args
    | "caml_avx512dq_cvtt_float64x2_int64x2_unsigned_maskz" ->
      instr (vcvttpd2uqq_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x4_int64x4" -> instr vcvttps2qq_Y_Xm128 args
    | "caml_avx512dq_cvtt_float32x4_int64x4_mask" ->
      instr vcvttps2qq_Y_Xm128_K_merge args
    | "caml_avx512dq_cvtt_float32x4_int64x4_maskz" ->
      instr (vcvttps2qq_Y_Xm128_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x4_int64x2" -> instr vcvttps2qq_X_Xm64 args
    | "caml_avx512dq_cvtt_float32x4_int64x2_mask" ->
      instr vcvttps2qq_X_Xm64_K_merge args
    | "caml_avx512dq_cvtt_float32x4_int64x2_maskz" ->
      instr (vcvttps2qq_X_Xm64_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x4_int64x4_unsigned" ->
      instr vcvttps2uqq_Y_Xm128 args
    | "caml_avx512dq_cvtt_float32x4_int64x4_unsigned_mask" ->
      instr vcvttps2uqq_Y_Xm128_K_merge args
    | "caml_avx512dq_cvtt_float32x4_int64x4_unsigned_maskz" ->
      instr (vcvttps2uqq_Y_Xm128_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x4_int64x2_unsigned" ->
      instr vcvttps2uqq_X_Xm64 args
    | "caml_avx512dq_cvtt_float32x4_int64x2_unsigned_mask" ->
      instr vcvttps2uqq_X_Xm64_K_merge args
    | "caml_avx512dq_cvtt_float32x4_int64x2_unsigned_maskz" ->
      instr (vcvttps2uqq_X_Xm64_K ~z:true) args
    | "caml_avx512dq_cvt_int64x4_float64x4_unsigned" ->
      instr vcvtuqq2pd_Y_Ym256 args
    | "caml_avx512dq_cvt_int64x4_float64x4_unsigned_mask" ->
      instr vcvtuqq2pd_Y_Ym256_K_merge args
    | "caml_avx512dq_cvt_int64x4_float64x4_unsigned_maskz" ->
      instr (vcvtuqq2pd_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_int64x2_float64x2_unsigned" ->
      instr vcvtuqq2pd_X_Xm128 args
    | "caml_avx512dq_cvt_int64x2_float64x2_unsigned_mask" ->
      instr vcvtuqq2pd_X_Xm128_K_merge args
    | "caml_avx512dq_cvt_int64x2_float64x2_unsigned_maskz" ->
      instr (vcvtuqq2pd_X_Xm128_K ~z:true) args
    | "caml_avx512dq_cvt_int64x4_float32x4_unsigned" ->
      instr vcvtuqq2ps_X_Ym256 args
    | "caml_avx512dq_cvt_int64x4_float32x4_unsigned_mask" ->
      instr vcvtuqq2ps_X_Ym256_K_merge args
    | "caml_avx512dq_cvt_int64x4_float32x4_unsigned_maskz" ->
      instr (vcvtuqq2ps_X_Ym256_K ~z:true) args
    | "caml_avx512dq_int64x4_mul_low_mask" ->
      instr vpmullq_Y_Y_Ym256_K_merge args
    | "caml_avx512dq_int64x4_mul_low_maskz" ->
      instr (vpmullq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512dq_int64x4_mul_low" -> instr vpmullq_Y_Y_Ym256 args
    | "caml_avx512dq_int64x2_mul_low_mask" ->
      instr vpmullq_X_X_Xm128_K_merge args
    | "caml_avx512dq_int64x2_mul_low_maskz" ->
      instr (vpmullq_X_X_Xm128_K ~z:true) args
    | "caml_avx512dq_int64x2_mul_low" -> instr vpmullq_X_X_Xm128 args
    | "caml_avx512dq_float64x8_andnot" -> instr vandnpd_Z_Z_Zm512 args
    | "caml_avx512dq_float64x8_andnot_mask" ->
      instr vandnpd_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float64x8_andnot_maskz" ->
      instr (vandnpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float32x16_andnot" -> instr vandnps_Z_Z_Zm512 args
    | "caml_avx512dq_float32x16_andnot_mask" ->
      instr vandnps_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float32x16_andnot_maskz" ->
      instr (vandnps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float64x8_and" -> instr vandpd_Z_Z_Zm512 args
    | "caml_avx512dq_float64x8_and_mask" -> instr vandpd_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float64x8_and_maskz" ->
      instr (vandpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float32x16_and" -> instr vandps_Z_Z_Zm512 args
    | "caml_avx512dq_float32x16_and_mask" -> instr vandps_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float32x16_and_maskz" ->
      instr (vandps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float64x8_or_mask" -> instr vorpd_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float64x8_or_maskz" ->
      instr (vorpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float64x8_or" -> instr vorpd_Z_Z_Zm512 args
    | "caml_avx512dq_float32x16_or_mask" -> instr vorps_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float32x16_or_maskz" ->
      instr (vorps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float32x16_or" -> instr vorps_Z_Z_Zm512 args
    | "caml_avx512dq_float64x8_xor_mask" -> instr vxorpd_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float64x8_xor_maskz" ->
      instr (vxorpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float64x8_xor" -> instr vxorpd_Z_Z_Zm512 args
    | "caml_avx512dq_float32x16_xor_mask" -> instr vxorps_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_float32x16_xor_maskz" ->
      instr (vxorps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_float32x16_xor" -> instr vxorps_Z_Z_Zm512 args
    | "caml_avx512dq_float32x16_broadcast_f32x2" ->
      instr vbroadcastf32x2_Z_Xm64 args
    | "caml_avx512dq_float32x16_broadcast_f32x2_mask" ->
      instr vbroadcastf32x2_Z_Xm64_K_merge args
    | "caml_avx512dq_float32x16_broadcast_f32x2_maskz" ->
      instr (vbroadcastf32x2_Z_Xm64_K ~z:true) args
    | "caml_avx512dq_int32x16_broadcast_i32x2" ->
      instr vbroadcasti32x2_Z_Xm64 args
    | "caml_avx512dq_int32x16_broadcast_i32x2_mask" ->
      instr vbroadcasti32x2_Z_Xm64_K_merge args
    | "caml_avx512dq_int32x16_broadcast_i32x2_maskz" ->
      instr (vbroadcasti32x2_Z_Xm64_K ~z:true) args
    | "caml_avx512dq_float32x16_extract_float32x8" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf32x8_Ym256_Z ~i args
    | "caml_avx512dq_float32x16_extract_float32x8_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf32x8_Y_Z_K_merge ~i args
    | "caml_avx512dq_float32x16_extract_float32x8_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextractf32x8_Ym256_Z_K ~z:true) ~i args
    | "caml_avx512dq_float64x8_extract_float64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextractf64x2_Xm128_Z ~i args
    | "caml_avx512dq_float64x8_extract_float64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextractf64x2_X_Z_K_merge ~i args
    | "caml_avx512dq_float64x8_extract_float64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vextractf64x2_Xm128_Z_K ~z:true) ~i args
    | "caml_avx512dq_int32x16_extract_int32x8" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti32x8_Ym256_Z ~i args
    | "caml_avx512dq_int32x16_extract_int32x8_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti32x8_Y_Z_K_merge ~i args
    | "caml_avx512dq_int32x16_extract_int32x8_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextracti32x8_Ym256_Z_K ~z:true) ~i args
    | "caml_avx512dq_int64x8_extract_int64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextracti64x2_Xm128_Z ~i args
    | "caml_avx512dq_int64x8_extract_int64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextracti64x2_X_Z_K_merge ~i args
    | "caml_avx512dq_int64x8_extract_int64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vextracti64x2_Xm128_Z_K ~z:true) ~i args
    | "caml_avx512dq_float64x8_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Zm512 ~i args
    | "caml_avx512dq_float64x8_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasspd_K_Zm512_K ~i args
    | "caml_avx512dq_float32x16_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Zm512 ~i args
    | "caml_avx512dq_float32x16_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassps_K_Zm512_K ~i args
    | "caml_avx512dq_float64_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasssd_K_Xm64 ~i args
    | "caml_avx512dq_float64_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclasssd_K_Xm64_K ~i args
    | "caml_avx512dq_float32_fpclass" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassss_K_Xm32 ~i args
    | "caml_avx512dq_float32_fpclass_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfpclassss_K_Xm32_K ~i args
    | "caml_avx512dq_float32x16_insert_float32x8" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf32x8_Z_Z_Ym256 ~i args
    | "caml_avx512dq_float32x16_insert_float32x8_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf32x8_Z_Z_Ym256_K_merge ~i args
    | "caml_avx512dq_float32x16_insert_float32x8_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinsertf32x8_Z_Z_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_float64x8_insert_float64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinsertf64x2_Z_Z_Xm128 ~i args
    | "caml_avx512dq_float64x8_insert_float64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinsertf64x2_Z_Z_Xm128_K_merge ~i args
    | "caml_avx512dq_float64x8_insert_float64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vinsertf64x2_Z_Z_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_int32x16_insert_int32x8" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti32x8_Z_Z_Ym256 ~i args
    | "caml_avx512dq_int32x16_insert_int32x8_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti32x8_Z_Z_Ym256_K_merge ~i args
    | "caml_avx512dq_int32x16_insert_int32x8_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinserti32x8_Z_Z_Ym256_K ~z:true) ~i args
    | "caml_avx512dq_int64x8_insert_int64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinserti64x2_Z_Z_Xm128 ~i args
    | "caml_avx512dq_int64x8_insert_int64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinserti64x2_Z_Z_Xm128_K_merge ~i args
    | "caml_avx512dq_int64x8_insert_int64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vinserti64x2_Z_Z_Xm128_K ~z:true) ~i args
    | "caml_avx512dq_int32x16_movepi_mask" -> instr vpmovd2m_K_Z args
    | "caml_avx512dq_int32x16_movm" -> instr vpmovm2d_Z_K args
    | "caml_avx512dq_int64x8_movm" -> instr vpmovm2q_Z_K args
    | "caml_avx512dq_int64x8_movepi_mask" -> instr vpmovq2m_K_Z args
    | "caml_avx512dq_float64x8_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512dq_float64x8_range_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_Z_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float64x8_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512dq_float64x8_range_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_Z_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float64x8_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangepd_Z_Z_Zm512 ~i args
    | "caml_avx512dq_float64x8_range_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangepd_Z_Z_Z ~sae:true) ~i args
    | "caml_avx512dq_float32x16_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512dq_float32x16_range_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_Z_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float32x16_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512dq_float32x16_range_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_Z_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float32x16_range" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangeps_Z_Z_Zm512 ~i args
    | "caml_avx512dq_float32x16_range_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangeps_Z_Z_Z ~sae:true) ~i args
    | "caml_avx512dq_float64_range_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangesd_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float64_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangesd_X_X_Xm64_K_merge ~i args
    | "caml_avx512dq_float64_range_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangesd_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float64_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangesd_X_X_Xm64_K ~z:true) ~i args
    | "caml_avx512dq_float64_range_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangesd_X_X_X ~sae:true) ~i args
    | "caml_avx512dq_float32_range_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangess_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float32_range_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vrangess_X_X_Xm32_K_merge ~i args
    | "caml_avx512dq_float32_range_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangess_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float32_range_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangess_X_X_Xm32_K ~z:true) ~i args
    | "caml_avx512dq_float32_range_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vrangess_X_X_X ~sae:true) ~i args
    | "caml_avx512dq_float64x8_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_Z_Zm512_K_merge ~i args
    | "caml_avx512dq_float64x8_reduce_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float64x8_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512dq_float64x8_reduce_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float64x8_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducepd_Z_Zm512 ~i args
    | "caml_avx512dq_float64x8_reduce_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducepd_Z_Z ~sae:true) ~i args
    | "caml_avx512dq_float32x16_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_Z_Zm512_K_merge ~i args
    | "caml_avx512dq_float32x16_reduce_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float32x16_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512dq_float32x16_reduce_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float32x16_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreduceps_Z_Zm512 ~i args
    | "caml_avx512dq_float32x16_reduce_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreduceps_Z_Z ~sae:true) ~i args
    | "caml_avx512dq_float64_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducesd_X_X_Xm64_K_merge ~i args
    | "caml_avx512dq_float64_reduce_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducesd_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float64_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducesd_X_X_Xm64_K ~z:true) ~i args
    | "caml_avx512dq_float64_reduce_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducesd_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float64_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducesd_X_X_Xm64 ~i args
    | "caml_avx512dq_float64_reduce_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducesd_X_X_X ~sae:true) ~i args
    | "caml_avx512dq_float32_reduce_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducess_X_X_Xm32_K_merge ~i args
    | "caml_avx512dq_float32_reduce_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducess_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512dq_float32_reduce_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducess_X_X_Xm32_K ~z:true) ~i args
    | "caml_avx512dq_float32_reduce_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducess_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512dq_float32_reduce" ->
      let i, args = extract_constant args ~max:255 op in
      instr vreducess_X_X_Xm32 ~i args
    | "caml_avx512dq_float32_reduce_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vreducess_X_X_X ~sae:true) ~i args
    | "caml_avx512dq_cvt_float64x8_int64x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2qq_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2qq_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2qq_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2qq_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8" -> instr vcvtpd2qq_Z_Zm512 args
    | "caml_avx512dq_cvt_float64x8_int64x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2qq_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2qq_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2qq_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2qq_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8_mask" ->
      instr vcvtpd2qq_Z_Zm512_K_merge args
    | "caml_avx512dq_cvt_float64x8_int64x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2qq_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtpd2qq_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtpd2qq_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtpd2qq_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8_maskz" ->
      instr (vcvtpd2qq_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2uqq_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2uqq_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2uqq_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2uqq_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned" ->
      instr vcvtpd2uqq_Z_Zm512 args
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2uqq_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2uqq_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2uqq_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2uqq_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned_mask" ->
      instr vcvtpd2uqq_Z_Zm512_K_merge args
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2uqq_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtpd2uqq_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtpd2uqq_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtpd2uqq_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_float64x8_int64x8_unsigned_maskz" ->
      instr (vcvtpd2uqq_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvt_float32x8_int64x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2qq_Z_Y ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2qq_Z_Y ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2qq_Z_Y ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2qq_Z_Y ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8" -> instr vcvtps2qq_Z_Ym256 args
    | "caml_avx512dq_cvt_float32x8_int64x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2qq_Z_Y_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2qq_Z_Y_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2qq_Z_Y_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2qq_Z_Y_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8_mask" ->
      instr vcvtps2qq_Z_Ym256_K_merge args
    | "caml_avx512dq_cvt_float32x8_int64x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2qq_Z_Y_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtps2qq_Z_Y_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtps2qq_Z_Y_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtps2qq_Z_Y_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8_maskz" ->
      instr (vcvtps2qq_Z_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2uqq_Z_Y ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2uqq_Z_Y ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2uqq_Z_Y ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2uqq_Z_Y ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned" ->
      instr vcvtps2uqq_Z_Ym256 args
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2uqq_Z_Y_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2uqq_Z_Y_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2uqq_Z_Y_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2uqq_Z_Y_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned_mask" ->
      instr vcvtps2uqq_Z_Ym256_K_merge args
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2uqq_Z_Y_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtps2uqq_Z_Y_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtps2uqq_Z_Y_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtps2uqq_Z_Y_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_float32x8_int64x8_unsigned_maskz" ->
      instr (vcvtps2uqq_Z_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_int64x8_float64x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2pd_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtqq2pd_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtqq2pd_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtqq2pd_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8" -> instr vcvtqq2pd_Z_Zm512 args
    | "caml_avx512dq_cvt_int64x8_float64x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2pd_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtqq2pd_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtqq2pd_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtqq2pd_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8_mask" ->
      instr vcvtqq2pd_Z_Zm512_K_merge args
    | "caml_avx512dq_cvt_int64x8_float64x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2pd_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtqq2pd_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtqq2pd_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtqq2pd_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8_maskz" ->
      instr (vcvtqq2pd_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvt_int64x8_float32x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2ps_Y_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtqq2ps_Y_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtqq2ps_Y_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtqq2ps_Y_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8" -> instr vcvtqq2ps_Y_Zm512 args
    | "caml_avx512dq_cvt_int64x8_float32x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2ps_Y_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtqq2ps_Y_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtqq2ps_Y_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtqq2ps_Y_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8_mask" ->
      instr vcvtqq2ps_Y_Zm512_K_merge args
    | "caml_avx512dq_cvt_int64x8_float32x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtqq2ps_Y_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtqq2ps_Y_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtqq2ps_Y_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtqq2ps_Y_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8_maskz" ->
      instr (vcvtqq2ps_Y_Zm512_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2qq_Z_Z ~sae:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8" -> instr vcvttpd2qq_Z_Zm512 args
    | "caml_avx512dq_cvtt_float64x8_int64x8_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2qq_Z_Z_K_merge ~sae:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_mask" ->
      instr vcvttpd2qq_Z_Zm512_K_merge args
    | "caml_avx512dq_cvtt_float64x8_int64x8_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2qq_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_maskz" ->
      instr (vcvttpd2qq_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2uqq_Z_Z ~sae:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned" ->
      instr vcvttpd2uqq_Z_Zm512 args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2uqq_Z_Z_K_merge ~sae:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned_mask" ->
      instr vcvttpd2uqq_Z_Zm512_K_merge args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2uqq_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512dq_cvtt_float64x8_int64x8_unsigned_maskz" ->
      instr (vcvttpd2uqq_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2qq_Z_Y ~sae:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8" -> instr vcvttps2qq_Z_Ym256 args
    | "caml_avx512dq_cvtt_float32x8_int64x8_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2qq_Z_Y_K_merge ~sae:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_mask" ->
      instr vcvttps2qq_Z_Ym256_K_merge args
    | "caml_avx512dq_cvtt_float32x8_int64x8_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2qq_Z_Y_K ~sae:true ~z:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_maskz" ->
      instr (vcvttps2qq_Z_Ym256_K ~z:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2uqq_Z_Y ~sae:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned" ->
      instr vcvttps2uqq_Z_Ym256 args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2uqq_Z_Y_K_merge ~sae:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned_mask" ->
      instr vcvttps2uqq_Z_Ym256_K_merge args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2uqq_Z_Y_K ~sae:true ~z:true) args
    | "caml_avx512dq_cvtt_float32x8_int64x8_unsigned_maskz" ->
      instr (vcvttps2uqq_Z_Ym256_K ~z:true) args
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2pd_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtuqq2pd_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtuqq2pd_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtuqq2pd_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned" ->
      instr vcvtuqq2pd_Z_Zm512 args
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2pd_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtuqq2pd_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtuqq2pd_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtuqq2pd_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned_mask" ->
      instr vcvtuqq2pd_Z_Zm512_K_merge args
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2pd_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtuqq2pd_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtuqq2pd_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtuqq2pd_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float64x8_unsigned_maskz" ->
      instr (vcvtuqq2pd_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2ps_Y_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtuqq2ps_Y_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtuqq2ps_Y_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtuqq2ps_Y_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned" ->
      instr vcvtuqq2ps_Y_Zm512 args
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2ps_Y_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtuqq2ps_Y_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtuqq2ps_Y_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtuqq2ps_Y_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned_mask" ->
      instr vcvtuqq2ps_Y_Zm512_K_merge args
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtuqq2ps_Y_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtuqq2ps_Y_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtuqq2ps_Y_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtuqq2ps_Y_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512dq_cvt_int64x8_float32x8_unsigned_maskz" ->
      instr (vcvtuqq2ps_Y_Zm512_K ~z:true) args
    | "caml_avx512dq_int64x8_mul_low_mask" ->
      instr vpmullq_Z_Z_Zm512_K_merge args
    | "caml_avx512dq_int64x8_mul_low_maskz" ->
      instr (vpmullq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512dq_int64x8_mul_low" -> instr vpmullq_Z_Z_Zm512 args
    | "caml_avx512dq_mask8_add" -> instr kaddb args
    | "caml_avx512dq_mask16_add" -> instr kaddw args
    | "caml_avx512dq_mask8_and" -> instr kandb args
    | "caml_avx512dq_mask8_andnot" -> instr kandnb args
    | "caml_avx512dq_mask8_not" -> instr knotb args
    | "caml_avx512dq_mask8_or" -> instr korb args
    | "caml_avx512dq_mask8_xnor" -> instr kxnorb args
    | "caml_avx512dq_mask8_xor" -> instr kxorb args
    | "caml_avx512dq_mask8_shift_left" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftlb ~i args
    | "caml_avx512dq_mask8_shift_right" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftrb ~i args
    | "caml_avx512dq_mask8_to_int32" -> instr kmovb_r32_K args
    | "caml_avx512dq_int32_to_mask8" -> instr kmovb_K_r32 args
    | "caml_avx512f_float64x4_add_mask" -> instr vaddpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_add_maskz" ->
      instr (vaddpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_add_mask" -> instr vaddpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_add_maskz" ->
      instr (vaddpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_add_mask" -> instr vaddps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_add_maskz" ->
      instr (vaddps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_add_mask" -> instr vaddps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_add_maskz" ->
      instr (vaddps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_div_mask" -> instr vdivpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_div_maskz" ->
      instr (vdivpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_div_mask" -> instr vdivpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_div_maskz" ->
      instr (vdivpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_div_mask" -> instr vdivps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_div_maskz" ->
      instr (vdivps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_div_mask" -> instr vdivps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_div_maskz" ->
      instr (vdivps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231pd_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231pd_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231ps_Y_Y_Ym256_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_Y_Y_Ym256_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_Y_Y_Ym256_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231ps_X_X_Xm128_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_X_X_Xm128_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_X_X_Xm128_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_max_mask" -> instr vmaxpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_max_maskz" ->
      instr (vmaxpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_max_mask" -> instr vmaxpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_max_maskz" ->
      instr (vmaxpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_max_mask" -> instr vmaxps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_max_maskz" ->
      instr (vmaxps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_max_mask" -> instr vmaxps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_max_maskz" ->
      instr (vmaxps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_min_mask" -> instr vminpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_min_maskz" ->
      instr (vminpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_min_mask" -> instr vminpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_min_maskz" ->
      instr (vminpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_min_mask" -> instr vminps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_min_maskz" ->
      instr (vminps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_min_mask" -> instr vminps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_min_maskz" ->
      instr (vminps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_mul_mask" -> instr vmulpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_mul_maskz" ->
      instr (vmulpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_mul_mask" -> instr vmulpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_mul_maskz" ->
      instr (vmulpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_mul_mask" -> instr vmulps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_mul_maskz" ->
      instr (vmulps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_mul_mask" -> instr vmulps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_mul_maskz" ->
      instr (vmulps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_abs_mask" -> instr vpabsd_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_abs_maskz" -> instr (vpabsd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_abs_mask" -> instr vpabsd_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_abs_maskz" -> instr (vpabsd_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_abs" -> instr vpabsq_Y_Ym256 args
    | "caml_avx512f_int64x4_abs_mask" -> instr vpabsq_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_abs_maskz" -> instr (vpabsq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_abs" -> instr vpabsq_X_Xm128 args
    | "caml_avx512f_int64x2_abs_mask" -> instr vpabsq_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_abs_maskz" -> instr (vpabsq_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_add_mask" -> instr vpaddd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_add_maskz" ->
      instr (vpaddd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_add_mask" -> instr vpaddd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_add_maskz" ->
      instr (vpaddd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_add_mask" -> instr vpaddq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_add_maskz" ->
      instr (vpaddq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_add_mask" -> instr vpaddq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_add_maskz" ->
      instr (vpaddq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_max_mask" -> instr vpmaxsd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_max_maskz" ->
      instr (vpmaxsd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_max_mask" -> instr vpmaxsd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_max_maskz" ->
      instr (vpmaxsd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_max_mask" -> instr vpmaxsq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_max_maskz" ->
      instr (vpmaxsq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_max" -> instr vpmaxsq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_max_mask" -> instr vpmaxsq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_max_maskz" ->
      instr (vpmaxsq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_max" -> instr vpmaxsq_X_X_Xm128 args
    | "caml_avx512f_int32x8_max_unsigned_mask" ->
      instr vpmaxud_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_max_unsigned_maskz" ->
      instr (vpmaxud_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_max_unsigned_mask" ->
      instr vpmaxud_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_max_unsigned_maskz" ->
      instr (vpmaxud_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_max_unsigned_mask" ->
      instr vpmaxuq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_max_unsigned_maskz" ->
      instr (vpmaxuq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_max_unsigned" -> instr vpmaxuq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_max_unsigned_mask" ->
      instr vpmaxuq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_max_unsigned_maskz" ->
      instr (vpmaxuq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_max_unsigned" -> instr vpmaxuq_X_X_Xm128 args
    | "caml_avx512f_int32x8_min_mask" -> instr vpminsd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_min_maskz" ->
      instr (vpminsd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_min_mask" -> instr vpminsd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_min_maskz" ->
      instr (vpminsd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_min_mask" -> instr vpminsq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_min_maskz" ->
      instr (vpminsq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_min" -> instr vpminsq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_min_mask" -> instr vpminsq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_min_maskz" ->
      instr (vpminsq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_min" -> instr vpminsq_X_X_Xm128 args
    | "caml_avx512f_int32x8_min_unsigned_mask" ->
      instr vpminud_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_min_unsigned_maskz" ->
      instr (vpminud_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_min_unsigned_mask" ->
      instr vpminud_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_min_unsigned_maskz" ->
      instr (vpminud_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_min_unsigned_mask" ->
      instr vpminuq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_min_unsigned_maskz" ->
      instr (vpminuq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_min_unsigned" -> instr vpminuq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_min_unsigned_mask" ->
      instr vpminuq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_min_unsigned_maskz" ->
      instr (vpminuq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_min_unsigned" -> instr vpminuq_X_X_Xm128 args
    | "caml_avx512f_int32x8_mul_mask" -> instr vpmuldq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_mul_maskz" ->
      instr (vpmuldq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_mul_mask" -> instr vpmuldq_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_mul_maskz" ->
      instr (vpmuldq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_mul_low_mask" ->
      instr vpmulld_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_mul_low_maskz" ->
      instr (vpmulld_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_mul_low_mask" ->
      instr vpmulld_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_mul_low_maskz" ->
      instr (vpmulld_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_mul_unsigned_mask" ->
      instr vpmuludq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_mul_unsigned_maskz" ->
      instr (vpmuludq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_mul_unsigned_mask" ->
      instr vpmuludq_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_mul_unsigned_maskz" ->
      instr (vpmuludq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_sub_mask" -> instr vpsubd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_sub_maskz" ->
      instr (vpsubd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_sub_mask" -> instr vpsubd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_sub_maskz" ->
      instr (vpsubd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_sub_mask" -> instr vpsubq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_sub_maskz" ->
      instr (vpsubq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_sub_mask" -> instr vpsubq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_sub_maskz" ->
      instr (vpsubq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_rcp14_mask" -> instr vrcp14pd_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_rcp14_maskz" ->
      instr (vrcp14pd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x4_rcp14" -> instr vrcp14pd_Y_Ym256 args
    | "caml_avx512f_float64x2_rcp14_mask" -> instr vrcp14pd_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_rcp14_maskz" ->
      instr (vrcp14pd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x2_rcp14" -> instr vrcp14pd_X_Xm128 args
    | "caml_avx512f_float32x8_rcp14_mask" -> instr vrcp14ps_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_rcp14_maskz" ->
      instr (vrcp14ps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x8_rcp14" -> instr vrcp14ps_Y_Ym256 args
    | "caml_avx512f_float32x4_rcp14_mask" -> instr vrcp14ps_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_rcp14_maskz" ->
      instr (vrcp14ps_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x4_rcp14" -> instr vrcp14ps_X_Xm128 args
    | "caml_avx512f_float64x4_rsqrt14" -> instr vrsqrt14pd_Y_Ym256 args
    | "caml_avx512f_float64x4_rsqrt14_mask" ->
      instr vrsqrt14pd_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_rsqrt14_maskz" ->
      instr (vrsqrt14pd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_rsqrt14" -> instr vrsqrt14pd_X_Xm128 args
    | "caml_avx512f_float64x2_rsqrt14_mask" ->
      instr vrsqrt14pd_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_rsqrt14_maskz" ->
      instr (vrsqrt14pd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_rsqrt14" -> instr vrsqrt14ps_Y_Ym256 args
    | "caml_avx512f_float32x8_rsqrt14_mask" ->
      instr vrsqrt14ps_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_rsqrt14_maskz" ->
      instr (vrsqrt14ps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_rsqrt14" -> instr vrsqrt14ps_X_Xm128 args
    | "caml_avx512f_float32x4_rsqrt14_mask" ->
      instr vrsqrt14ps_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_rsqrt14_maskz" ->
      instr (vrsqrt14ps_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_sub_mask" -> instr vsubpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_sub_maskz" ->
      instr (vsubpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_sub_mask" -> instr vsubpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_sub_maskz" ->
      instr (vsubpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_sub_mask" -> instr vsubps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_sub_maskz" ->
      instr (vsubps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_sub_mask" -> instr vsubps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_sub_maskz" ->
      instr (vsubps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_align_right" ->
      let i, args = extract_constant args ~max:7 op in
      instr valignd_Y_Y_Ym256 ~i args
    | "caml_avx512f_int32x8_align_right_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr valignd_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_align_right_maskz" ->
      let i, args = extract_constant args ~max:7 op in
      instr (valignd_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x4_align_right" ->
      let i, args = extract_constant args ~max:3 op in
      instr valignd_X_X_Xm128 ~i args
    | "caml_avx512f_int32x4_align_right_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr valignd_X_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_align_right_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (valignd_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x4_align_right" ->
      let i, args = extract_constant args ~max:3 op in
      instr valignq_Y_Y_Ym256 ~i args
    | "caml_avx512f_int64x4_align_right_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr valignq_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_align_right_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (valignq_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x2_align_right" ->
      let i, args = extract_constant args ~max:1 op in
      instr valignq_X_X_Xm128 ~i args
    | "caml_avx512f_int64x2_align_right_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr valignq_X_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_align_right_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (valignq_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x4_blend" ->
      instr (vblendmpd_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_float64x2_blend" ->
      instr (vblendmpd_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_float32x8_blend" ->
      instr (vblendmps_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_float32x4_blend" ->
      instr (vblendmps_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_float64x4_broadcast_mask" ->
      instr vbroadcastsd_Y_Xm64_K_merge args
    | "caml_avx512f_float64x4_broadcast_maskz" ->
      instr (vbroadcastsd_Y_Xm64_K ~z:true) args
    | "caml_avx512f_float32x8_broadcast_mask" ->
      instr vbroadcastss_Y_Xm32_K_merge args
    | "caml_avx512f_float32x8_broadcast_maskz" ->
      instr (vbroadcastss_Y_Xm32_K ~z:true) args
    | "caml_avx512f_float32x4_broadcast_mask" ->
      instr vbroadcastss_X_Xm32_K_merge args
    | "caml_avx512f_float32x4_broadcast_maskz" ->
      instr (vbroadcastss_X_Xm32_K ~z:true) args
    | "caml_avx512f_float64x4_compress_mask" ->
      instr vcompresspd_Y_Y_K_merge args
    | "caml_avx512f_float64x4_compress_maskz" ->
      instr (vcompresspd_Ym256_Y_K ~z:true) args
    | "caml_avx512f_float64x2_compress_mask" ->
      instr vcompresspd_X_X_K_merge args
    | "caml_avx512f_float64x2_compress_maskz" ->
      instr (vcompresspd_Xm128_X_K ~z:true) args
    | "caml_avx512f_float32x8_compress_mask" ->
      instr vcompressps_Y_Y_K_merge args
    | "caml_avx512f_float32x8_compress_maskz" ->
      instr (vcompressps_Ym256_Y_K ~z:true) args
    | "caml_avx512f_float32x4_compress_mask" ->
      instr vcompressps_X_X_K_merge args
    | "caml_avx512f_float32x4_compress_maskz" ->
      instr (vcompressps_Xm128_X_K ~z:true) args
    | "caml_avx512f_float64x4_expand_mask" ->
      instr vexpandpd_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_expand_maskz" ->
      instr (vexpandpd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_expand_mask" ->
      instr vexpandpd_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_expand_maskz" ->
      instr (vexpandpd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_expand_mask" ->
      instr vexpandps_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_expand_maskz" ->
      instr (vexpandps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_expand_mask" ->
      instr vexpandps_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_expand_maskz" ->
      instr (vexpandps_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_extract_float32x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf32x4_Xm128_Y ~i args
    | "caml_avx512f_float32x8_extract_float32x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf32x4_X_Y_K_merge ~i args
    | "caml_avx512f_float32x8_extract_float32x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextractf32x4_Xm128_Y_K ~z:true) ~i args
    | "caml_avx512f_int32x8_extract_int32x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti32x4_Xm128_Y ~i args
    | "caml_avx512f_int32x8_extract_int32x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti32x4_X_Y_K_merge ~i args
    | "caml_avx512f_int32x8_extract_int32x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextracti32x4_Xm128_Y_K ~z:true) ~i args
    | "caml_avx512f_float64x4_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmpd_Y_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Y_Y_Ym256_K ~z:false) ~i args
    | "caml_avx512f_float64x4_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x2_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmpd_X_X_Xm128 ~i args
    | "caml_avx512f_float64x2_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_X_X_Xm128_K ~z:false) ~i args
    | "caml_avx512f_float64x2_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x8_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmps_Y_Y_Ym256 ~i args
    | "caml_avx512f_float32x8_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Y_Y_Ym256_K ~z:false) ~i args
    | "caml_avx512f_float32x8_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x4_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmps_X_X_Xm128 ~i args
    | "caml_avx512f_float32x4_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_X_X_Xm128_K ~z:false) ~i args
    | "caml_avx512f_float32x4_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x4_getexp" -> instr vgetexppd_Y_Ym256 args
    | "caml_avx512f_float64x4_getexp_mask" ->
      instr vgetexppd_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_getexp_maskz" ->
      instr (vgetexppd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_getexp" -> instr vgetexppd_X_Xm128 args
    | "caml_avx512f_float64x2_getexp_mask" ->
      instr vgetexppd_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_getexp_maskz" ->
      instr (vgetexppd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_getexp" -> instr vgetexpps_Y_Ym256 args
    | "caml_avx512f_float32x8_getexp_mask" ->
      instr vgetexpps_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_getexp_maskz" ->
      instr (vgetexpps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_getexp" -> instr vgetexpps_X_Xm128 args
    | "caml_avx512f_float32x4_getexp_mask" ->
      instr vgetexpps_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_getexp_maskz" ->
      instr (vgetexpps_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x2_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_X_Xm128 ~i args
    | "caml_avx512f_float64x2_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_X_Xm128_K_merge ~i args
    | "caml_avx512f_float64x2_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x8_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_Y_Ym256 ~i args
    | "caml_avx512f_float32x8_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float32x8_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x4_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_X_Xm128 ~i args
    | "caml_avx512f_float32x4_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_X_Xm128_K_merge ~i args
    | "caml_avx512f_float32x4_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x8_insert_float32x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf32x4_Y_Y_Xm128 ~i args
    | "caml_avx512f_float32x8_insert_float32x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf32x4_Y_Y_Xm128_K_merge ~i args
    | "caml_avx512f_float32x8_insert_float32x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinsertf32x4_Y_Y_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x8_insert_int32x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti32x4_Y_Y_Xm128 ~i args
    | "caml_avx512f_int32x8_insert_int32x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti32x4_Y_Y_Xm128_K_merge ~i args
    | "caml_avx512f_int32x8_insert_int32x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinserti32x4_Y_Y_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x8_blend" ->
      instr (vpblendmd_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_int32x4_blend" ->
      instr (vpblendmd_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_int64x4_blend" ->
      instr (vpblendmq_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_int64x2_blend" ->
      instr (vpblendmq_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_int32x8_broadcast_mask" ->
      instr vpbroadcastd_Y_Xm32_K_merge args
    | "caml_avx512f_int32x8_broadcast_maskz" ->
      instr (vpbroadcastd_Y_Xm32_K ~z:true) args
    | "caml_avx512f_int32x4_broadcast_mask" ->
      instr vpbroadcastd_X_Xm32_K_merge args
    | "caml_avx512f_int32x4_broadcast_maskz" ->
      instr (vpbroadcastd_X_Xm32_K ~z:true) args
    | "caml_avx512f_int64x4_broadcast_mask" ->
      instr vpbroadcastq_Y_Xm64_K_merge args
    | "caml_avx512f_int64x4_broadcast_maskz" ->
      instr (vpbroadcastq_Y_Xm64_K ~z:true) args
    | "caml_avx512f_int64x2_broadcast_mask" ->
      instr vpbroadcastq_X_Xm64_K_merge args
    | "caml_avx512f_int64x2_broadcast_maskz" ->
      instr (vpbroadcastq_X_Xm64_K ~z:true) args
    | "caml_avx512f_int32x8_compress_mask" -> instr vpcompressd_Y_Y_K_merge args
    | "caml_avx512f_int32x8_compress_maskz" ->
      instr (vpcompressd_Ym256_Y_K ~z:true) args
    | "caml_avx512f_int32x4_compress_mask" -> instr vpcompressd_X_X_K_merge args
    | "caml_avx512f_int32x4_compress_maskz" ->
      instr (vpcompressd_Xm128_X_K ~z:true) args
    | "caml_avx512f_int64x4_compress_mask" -> instr vpcompressq_Y_Y_K_merge args
    | "caml_avx512f_int64x4_compress_maskz" ->
      instr (vpcompressq_Ym256_Y_K ~z:true) args
    | "caml_avx512f_int64x2_compress_mask" -> instr vpcompressq_X_X_K_merge args
    | "caml_avx512f_int64x2_compress_maskz" ->
      instr (vpcompressq_Xm128_X_K ~z:true) args
    | "caml_avx512f_int32x8_permutexvar_mask" ->
      instr vpermd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_permutexvar_maskz" ->
      instr (vpermd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x8_permutexvar" -> instr vpermd_Y_Y_Ym256 args
    | "caml_avx512f_int32x8_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2d_Y_Y_Ym256_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int32x8_permutex2var_mask" ->
      instr (vpermt2d_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_int32x8_permutex2var_maskz" ->
      instr (vpermt2d_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x8_permutex2var" -> instr vpermt2d_Y_Y_Ym256 args
    | "caml_avx512f_int32x4_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2d_X_X_Xm128_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int32x4_permutex2var_mask" ->
      instr (vpermt2d_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_int32x4_permutex2var_maskz" ->
      instr (vpermt2d_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_permutex2var" -> instr vpermt2d_X_X_Xm128 args
    | "caml_avx512f_float64x4_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2pd_Y_Y_Ym256_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float64x4_permutex2var_mask" ->
      instr (vpermt2pd_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_float64x4_permutex2var_maskz" ->
      instr (vpermt2pd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x4_permutex2var" -> instr vpermt2pd_Y_Y_Ym256 args
    | "caml_avx512f_float64x2_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2pd_X_X_Xm128_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float64x2_permutex2var_mask" ->
      instr (vpermt2pd_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_float64x2_permutex2var_maskz" ->
      instr (vpermt2pd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x2_permutex2var" -> instr vpermt2pd_X_X_Xm128 args
    | "caml_avx512f_float32x8_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2ps_Y_Y_Ym256_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float32x8_permutex2var_mask" ->
      instr (vpermt2ps_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_float32x8_permutex2var_maskz" ->
      instr (vpermt2ps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x8_permutex2var" -> instr vpermt2ps_Y_Y_Ym256 args
    | "caml_avx512f_float32x4_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2ps_X_X_Xm128_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float32x4_permutex2var_mask" ->
      instr (vpermt2ps_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_float32x4_permutex2var_maskz" ->
      instr (vpermt2ps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x4_permutex2var" -> instr vpermt2ps_X_X_Xm128 args
    | "caml_avx512f_int64x4_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2q_Y_Y_Ym256_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int64x4_permutex2var_mask" ->
      instr (vpermt2q_Y_Y_Ym256_K ~z:false) args
    | "caml_avx512f_int64x4_permutex2var_maskz" ->
      instr (vpermt2q_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_permutex2var" -> instr vpermt2q_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2q_X_X_Xm128_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int64x2_permutex2var_mask" ->
      instr (vpermt2q_X_X_Xm128_K ~z:false) args
    | "caml_avx512f_int64x2_permutex2var_maskz" ->
      instr (vpermt2q_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_permutex2var" -> instr vpermt2q_X_X_Xm128 args
    | "caml_avx512f_float64x4_permute_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vpermilpd_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_permutevar_mask" ->
      instr vpermilpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_permute_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vpermilpd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x4_permutevar_maskz" ->
      instr (vpermilpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_permute_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vpermilpd_X_Xm128_K_merge ~i args
    | "caml_avx512f_float64x2_permutevar_mask" ->
      instr vpermilpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_permute_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vpermilpd_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x2_permutevar_maskz" ->
      instr (vpermilpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_permute_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float32x8_permutevar_mask" ->
      instr vpermilps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_permute_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermilps_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x8_permutevar_maskz" ->
      instr (vpermilps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_permute_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_X_Xm128_K_merge ~i args
    | "caml_avx512f_float32x4_permutevar_mask" ->
      instr vpermilps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_permute_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermilps_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x4_permutevar_maskz" ->
      instr (vpermilps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_permute_across_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermpd_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_permutexvar_mask" ->
      instr vpermpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_permute_across_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermpd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x4_permutexvar_maskz" ->
      instr (vpermpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x4_permute_across" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermpd_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_permutexvar" -> instr vpermpd_Y_Y_Ym256 args
    | "caml_avx512f_float32x8_permutexvar_mask" ->
      instr vpermps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_permutexvar_maskz" ->
      instr (vpermps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x8_permutexvar" -> instr vpermps_Y_Y_Ym256 args
    | "caml_avx512f_int64x4_permute_across_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_permutexvar_mask" ->
      instr vpermq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_permute_across_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_permutexvar_maskz" ->
      instr (vpermq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_permute_across" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermq_Y_Ym256 ~i args
    | "caml_avx512f_int64x4_permutexvar" -> instr vpermq_Y_Y_Ym256 args
    | "caml_avx512f_int32x8_expand_mask" -> instr vpexpandd_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_expand_maskz" ->
      instr (vpexpandd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_expand_mask" -> instr vpexpandd_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_expand_maskz" ->
      instr (vpexpandd_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_expand_mask" -> instr vpexpandq_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_expand_maskz" ->
      instr (vpexpandq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_expand_mask" -> instr vpexpandq_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_expand_maskz" ->
      instr (vpexpandq_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_shuffle_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufd_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_shuffle_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x4_shuffle_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufd_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_shuffle_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufd_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x8_interleave_high_mask" ->
      instr vpunpckhdq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_interleave_high_maskz" ->
      instr (vpunpckhdq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_interleave_high_mask" ->
      instr vpunpckhdq_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_interleave_high_maskz" ->
      instr (vpunpckhdq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_interleave_high_mask" ->
      instr vpunpckhqdq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_interleave_high_maskz" ->
      instr (vpunpckhqdq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_interleave_high_mask" ->
      instr vpunpckhqdq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_interleave_high_maskz" ->
      instr (vpunpckhqdq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_interleave_low_mask" ->
      instr vpunpckldq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_interleave_low_maskz" ->
      instr (vpunpckldq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_interleave_low_mask" ->
      instr vpunpckldq_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_interleave_low_maskz" ->
      instr (vpunpckldq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_interleave_low_mask" ->
      instr vpunpcklqdq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_interleave_low_maskz" ->
      instr (vpunpcklqdq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_interleave_low_mask" ->
      instr vpunpcklqdq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_interleave_low_maskz" ->
      instr (vpunpcklqdq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x4_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_Y_Ym256 ~i args
    | "caml_avx512f_float64x2_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_X_Xm128_K_merge ~i args
    | "caml_avx512f_float64x2_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x2_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_X_Xm128 ~i args
    | "caml_avx512f_float32x8_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float32x8_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x8_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_Y_Ym256 ~i args
    | "caml_avx512f_float32x4_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_X_Xm128_K_merge ~i args
    | "caml_avx512f_float32x4_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x4_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_X_Xm128 ~i args
    | "caml_avx512f_float64x4_scalef_mask" ->
      instr vscalefpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_scalef_maskz" ->
      instr (vscalefpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x4_scalef" -> instr vscalefpd_Y_Y_Ym256 args
    | "caml_avx512f_float64x2_scalef_mask" ->
      instr vscalefpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_scalef_maskz" ->
      instr (vscalefpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x2_scalef" -> instr vscalefpd_X_X_Xm128 args
    | "caml_avx512f_float32x8_scalef_mask" ->
      instr vscalefps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_scalef_maskz" ->
      instr (vscalefps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x8_scalef" -> instr vscalefps_Y_Y_Ym256 args
    | "caml_avx512f_float32x4_scalef_mask" ->
      instr vscalefps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_scalef_maskz" ->
      instr (vscalefps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x4_scalef" -> instr vscalefps_X_X_Xm128 args
    | "caml_avx512f_float32x8_shuffle_f32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshuff32x4_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float32x8_shuffle_f32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vshuff32x4_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x8_shuffle_f32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshuff32x4_Y_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_shuffle_f64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshuff64x2_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_shuffle_f64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vshuff64x2_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x4_shuffle_f64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshuff64x2_Y_Y_Ym256 ~i args
    | "caml_avx512f_int32x8_shuffle_i32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshufi32x4_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_shuffle_i32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vshufi32x4_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x8_shuffle_i32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshufi32x4_Y_Y_Ym256 ~i args
    | "caml_avx512f_int64x4_shuffle_i64x2_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshufi64x2_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_shuffle_i64x2_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vshufi64x2_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_shuffle_i64x2" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshufi64x2_Y_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vshufpd_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float64x4_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vshufpd_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float64x2_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vshufpd_X_X_Xm128_K_merge ~i args
    | "caml_avx512f_float64x2_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vshufpd_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float32x8_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufps_Y_Y_Ym256_K_merge ~i args
    | "caml_avx512f_float32x8_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufps_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_float32x4_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufps_X_X_Xm128_K_merge ~i args
    | "caml_avx512f_float32x4_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufps_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x4_interleave_high_mask" ->
      instr vunpckhpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_interleave_high_maskz" ->
      instr (vunpckhpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_interleave_high_mask" ->
      instr vunpckhpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_interleave_high_maskz" ->
      instr (vunpckhpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_interleave_high_mask" ->
      instr vunpckhps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_interleave_high_maskz" ->
      instr (vunpckhps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_interleave_high_mask" ->
      instr vunpckhps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_interleave_high_maskz" ->
      instr (vunpckhps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_interleave_low_mask" ->
      instr vunpcklpd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_interleave_low_maskz" ->
      instr (vunpcklpd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_interleave_low_mask" ->
      instr vunpcklpd_X_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_interleave_low_maskz" ->
      instr (vunpcklpd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_interleave_low_mask" ->
      instr vunpcklps_Y_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_interleave_low_maskz" ->
      instr (vunpcklps_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_interleave_low_mask" ->
      instr vunpcklps_X_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_interleave_low_maskz" ->
      instr (vunpcklps_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_Y_Ym256 ~i args
    | "caml_avx512f_float64x4_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_Y_Ym256_K ~i args
    | "caml_avx512f_float64x2_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_X_Xm128 ~i args
    | "caml_avx512f_float64x2_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_X_Xm128_K ~i args
    | "caml_avx512f_float32x8_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_Y_Ym256 ~i args
    | "caml_avx512f_float32x8_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_Y_Ym256_K ~i args
    | "caml_avx512f_float32x4_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_X_Xm128 ~i args
    | "caml_avx512f_float32x4_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_X_Xm128_K ~i args
    | "caml_avx512f_int32x8_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_Y_Ym256 ~i args
    | "caml_avx512f_int32x8_cmpeq" -> instr vpcmpd_K_Y_Ym256 ~i:0 args
    | "caml_avx512f_int32x8_cmpge" -> instr vpcmpd_K_Y_Ym256 ~i:5 args
    | "caml_avx512f_int32x8_cmpgt" -> instr vpcmpd_K_Y_Ym256 ~i:6 args
    | "caml_avx512f_int32x8_cmple" -> instr vpcmpd_K_Y_Ym256 ~i:2 args
    | "caml_avx512f_int32x8_cmplt" -> instr vpcmpd_K_Y_Ym256 ~i:1 args
    | "caml_avx512f_int32x8_cmpneq" -> instr vpcmpd_K_Y_Ym256 ~i:4 args
    | "caml_avx512f_int32x8_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_Y_Ym256_K ~i args
    | "caml_avx512f_int32x8_cmpeq_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:0 args
    | "caml_avx512f_int32x8_cmpge_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:5 args
    | "caml_avx512f_int32x8_cmpgt_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:6 args
    | "caml_avx512f_int32x8_cmple_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:2 args
    | "caml_avx512f_int32x8_cmplt_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:1 args
    | "caml_avx512f_int32x8_cmpneq_mask" -> instr vpcmpd_K_Y_Ym256_K ~i:4 args
    | "caml_avx512f_int32x4_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_X_Xm128 ~i args
    | "caml_avx512f_int32x4_cmpeq" -> instr vpcmpd_K_X_Xm128 ~i:0 args
    | "caml_avx512f_int32x4_cmpge" -> instr vpcmpd_K_X_Xm128 ~i:5 args
    | "caml_avx512f_int32x4_cmpgt" -> instr vpcmpd_K_X_Xm128 ~i:6 args
    | "caml_avx512f_int32x4_cmple" -> instr vpcmpd_K_X_Xm128 ~i:2 args
    | "caml_avx512f_int32x4_cmplt" -> instr vpcmpd_K_X_Xm128 ~i:1 args
    | "caml_avx512f_int32x4_cmpneq" -> instr vpcmpd_K_X_Xm128 ~i:4 args
    | "caml_avx512f_int32x4_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_X_Xm128_K ~i args
    | "caml_avx512f_int32x4_cmpeq_mask" -> instr vpcmpd_K_X_Xm128_K ~i:0 args
    | "caml_avx512f_int32x4_cmpge_mask" -> instr vpcmpd_K_X_Xm128_K ~i:5 args
    | "caml_avx512f_int32x4_cmpgt_mask" -> instr vpcmpd_K_X_Xm128_K ~i:6 args
    | "caml_avx512f_int32x4_cmple_mask" -> instr vpcmpd_K_X_Xm128_K ~i:2 args
    | "caml_avx512f_int32x4_cmplt_mask" -> instr vpcmpd_K_X_Xm128_K ~i:1 args
    | "caml_avx512f_int32x4_cmpneq_mask" -> instr vpcmpd_K_X_Xm128_K ~i:4 args
    | "caml_avx512f_int64x4_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_Y_Ym256 ~i args
    | "caml_avx512f_int64x4_cmpeq" -> instr vpcmpq_K_Y_Ym256 ~i:0 args
    | "caml_avx512f_int64x4_cmpge" -> instr vpcmpq_K_Y_Ym256 ~i:5 args
    | "caml_avx512f_int64x4_cmpgt" -> instr vpcmpq_K_Y_Ym256 ~i:6 args
    | "caml_avx512f_int64x4_cmple" -> instr vpcmpq_K_Y_Ym256 ~i:2 args
    | "caml_avx512f_int64x4_cmplt" -> instr vpcmpq_K_Y_Ym256 ~i:1 args
    | "caml_avx512f_int64x4_cmpneq" -> instr vpcmpq_K_Y_Ym256 ~i:4 args
    | "caml_avx512f_int64x4_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_Y_Ym256_K ~i args
    | "caml_avx512f_int64x4_cmpeq_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:0 args
    | "caml_avx512f_int64x4_cmpge_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:5 args
    | "caml_avx512f_int64x4_cmpgt_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:6 args
    | "caml_avx512f_int64x4_cmple_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:2 args
    | "caml_avx512f_int64x4_cmplt_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:1 args
    | "caml_avx512f_int64x4_cmpneq_mask" -> instr vpcmpq_K_Y_Ym256_K ~i:4 args
    | "caml_avx512f_int64x2_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_X_Xm128 ~i args
    | "caml_avx512f_int64x2_cmpeq" -> instr vpcmpq_K_X_Xm128 ~i:0 args
    | "caml_avx512f_int64x2_cmpge" -> instr vpcmpq_K_X_Xm128 ~i:5 args
    | "caml_avx512f_int64x2_cmpgt" -> instr vpcmpq_K_X_Xm128 ~i:6 args
    | "caml_avx512f_int64x2_cmple" -> instr vpcmpq_K_X_Xm128 ~i:2 args
    | "caml_avx512f_int64x2_cmplt" -> instr vpcmpq_K_X_Xm128 ~i:1 args
    | "caml_avx512f_int64x2_cmpneq" -> instr vpcmpq_K_X_Xm128 ~i:4 args
    | "caml_avx512f_int64x2_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_X_Xm128_K ~i args
    | "caml_avx512f_int64x2_cmpeq_mask" -> instr vpcmpq_K_X_Xm128_K ~i:0 args
    | "caml_avx512f_int64x2_cmpge_mask" -> instr vpcmpq_K_X_Xm128_K ~i:5 args
    | "caml_avx512f_int64x2_cmpgt_mask" -> instr vpcmpq_K_X_Xm128_K ~i:6 args
    | "caml_avx512f_int64x2_cmple_mask" -> instr vpcmpq_K_X_Xm128_K ~i:2 args
    | "caml_avx512f_int64x2_cmplt_mask" -> instr vpcmpq_K_X_Xm128_K ~i:1 args
    | "caml_avx512f_int64x2_cmpneq_mask" -> instr vpcmpq_K_X_Xm128_K ~i:4 args
    | "caml_avx512f_int32x8_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_Y_Ym256 ~i args
    | "caml_avx512f_int32x8_cmpeq_unsigned" -> instr vpcmpud_K_Y_Ym256 ~i:0 args
    | "caml_avx512f_int32x8_cmpge_unsigned" -> instr vpcmpud_K_Y_Ym256 ~i:5 args
    | "caml_avx512f_int32x8_cmpgt_unsigned" -> instr vpcmpud_K_Y_Ym256 ~i:6 args
    | "caml_avx512f_int32x8_cmple_unsigned" -> instr vpcmpud_K_Y_Ym256 ~i:2 args
    | "caml_avx512f_int32x8_cmplt_unsigned" -> instr vpcmpud_K_Y_Ym256 ~i:1 args
    | "caml_avx512f_int32x8_cmpneq_unsigned" ->
      instr vpcmpud_K_Y_Ym256 ~i:4 args
    | "caml_avx512f_int32x8_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_Y_Ym256_K ~i args
    | "caml_avx512f_int32x8_cmpeq_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:0 args
    | "caml_avx512f_int32x8_cmpge_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:5 args
    | "caml_avx512f_int32x8_cmpgt_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:6 args
    | "caml_avx512f_int32x8_cmple_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:2 args
    | "caml_avx512f_int32x8_cmplt_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:1 args
    | "caml_avx512f_int32x8_cmpneq_unsigned_mask" ->
      instr vpcmpud_K_Y_Ym256_K ~i:4 args
    | "caml_avx512f_int32x4_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_X_Xm128 ~i args
    | "caml_avx512f_int32x4_cmpeq_unsigned" -> instr vpcmpud_K_X_Xm128 ~i:0 args
    | "caml_avx512f_int32x4_cmpge_unsigned" -> instr vpcmpud_K_X_Xm128 ~i:5 args
    | "caml_avx512f_int32x4_cmpgt_unsigned" -> instr vpcmpud_K_X_Xm128 ~i:6 args
    | "caml_avx512f_int32x4_cmple_unsigned" -> instr vpcmpud_K_X_Xm128 ~i:2 args
    | "caml_avx512f_int32x4_cmplt_unsigned" -> instr vpcmpud_K_X_Xm128 ~i:1 args
    | "caml_avx512f_int32x4_cmpneq_unsigned" ->
      instr vpcmpud_K_X_Xm128 ~i:4 args
    | "caml_avx512f_int32x4_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_X_Xm128_K ~i args
    | "caml_avx512f_int32x4_cmpeq_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:0 args
    | "caml_avx512f_int32x4_cmpge_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:5 args
    | "caml_avx512f_int32x4_cmpgt_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:6 args
    | "caml_avx512f_int32x4_cmple_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:2 args
    | "caml_avx512f_int32x4_cmplt_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:1 args
    | "caml_avx512f_int32x4_cmpneq_unsigned_mask" ->
      instr vpcmpud_K_X_Xm128_K ~i:4 args
    | "caml_avx512f_int64x4_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_Y_Ym256 ~i args
    | "caml_avx512f_int64x4_cmpeq_unsigned" -> instr vpcmpuq_K_Y_Ym256 ~i:0 args
    | "caml_avx512f_int64x4_cmpge_unsigned" -> instr vpcmpuq_K_Y_Ym256 ~i:5 args
    | "caml_avx512f_int64x4_cmpgt_unsigned" -> instr vpcmpuq_K_Y_Ym256 ~i:6 args
    | "caml_avx512f_int64x4_cmple_unsigned" -> instr vpcmpuq_K_Y_Ym256 ~i:2 args
    | "caml_avx512f_int64x4_cmplt_unsigned" -> instr vpcmpuq_K_Y_Ym256 ~i:1 args
    | "caml_avx512f_int64x4_cmpneq_unsigned" ->
      instr vpcmpuq_K_Y_Ym256 ~i:4 args
    | "caml_avx512f_int64x4_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_Y_Ym256_K ~i args
    | "caml_avx512f_int64x4_cmpeq_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:0 args
    | "caml_avx512f_int64x4_cmpge_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:5 args
    | "caml_avx512f_int64x4_cmpgt_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:6 args
    | "caml_avx512f_int64x4_cmple_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:2 args
    | "caml_avx512f_int64x4_cmplt_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:1 args
    | "caml_avx512f_int64x4_cmpneq_unsigned_mask" ->
      instr vpcmpuq_K_Y_Ym256_K ~i:4 args
    | "caml_avx512f_int64x2_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_X_Xm128 ~i args
    | "caml_avx512f_int64x2_cmpeq_unsigned" -> instr vpcmpuq_K_X_Xm128 ~i:0 args
    | "caml_avx512f_int64x2_cmpge_unsigned" -> instr vpcmpuq_K_X_Xm128 ~i:5 args
    | "caml_avx512f_int64x2_cmpgt_unsigned" -> instr vpcmpuq_K_X_Xm128 ~i:6 args
    | "caml_avx512f_int64x2_cmple_unsigned" -> instr vpcmpuq_K_X_Xm128 ~i:2 args
    | "caml_avx512f_int64x2_cmplt_unsigned" -> instr vpcmpuq_K_X_Xm128 ~i:1 args
    | "caml_avx512f_int64x2_cmpneq_unsigned" ->
      instr vpcmpuq_K_X_Xm128 ~i:4 args
    | "caml_avx512f_int64x2_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_X_Xm128_K ~i args
    | "caml_avx512f_int64x2_cmpeq_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:0 args
    | "caml_avx512f_int64x2_cmpge_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:5 args
    | "caml_avx512f_int64x2_cmpgt_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:6 args
    | "caml_avx512f_int64x2_cmple_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:2 args
    | "caml_avx512f_int64x2_cmplt_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:1 args
    | "caml_avx512f_int64x2_cmpneq_unsigned_mask" ->
      instr vpcmpuq_K_X_Xm128_K ~i:4 args
    | "caml_avx512f_int32x8_test_mask" -> instr vptestmd_K_Y_Ym256_K args
    | "caml_avx512f_int32x8_test" -> instr vptestmd_K_Y_Ym256 args
    | "caml_avx512f_int32x4_test_mask" -> instr vptestmd_K_X_Xm128_K args
    | "caml_avx512f_int32x4_test" -> instr vptestmd_K_X_Xm128 args
    | "caml_avx512f_int64x4_test_mask" -> instr vptestmq_K_Y_Ym256_K args
    | "caml_avx512f_int64x4_test" -> instr vptestmq_K_Y_Ym256 args
    | "caml_avx512f_int64x2_test_mask" -> instr vptestmq_K_X_Xm128_K args
    | "caml_avx512f_int64x2_test" -> instr vptestmq_K_X_Xm128 args
    | "caml_avx512f_int32x8_testn_mask" -> instr vptestnmd_K_Y_Ym256_K args
    | "caml_avx512f_int32x8_testn" -> instr vptestnmd_K_Y_Ym256 args
    | "caml_avx512f_int32x4_testn_mask" -> instr vptestnmd_K_X_Xm128_K args
    | "caml_avx512f_int32x4_testn" -> instr vptestnmd_K_X_Xm128 args
    | "caml_avx512f_int64x4_testn_mask" -> instr vptestnmq_K_Y_Ym256_K args
    | "caml_avx512f_int64x4_testn" -> instr vptestnmq_K_Y_Ym256 args
    | "caml_avx512f_int64x2_testn_mask" -> instr vptestnmq_K_X_Xm128_K args
    | "caml_avx512f_int64x2_testn" -> instr vptestnmq_K_X_Xm128 args
    | "caml_avx512f_cvt_int32x4_float64x4_mask" ->
      instr vcvtdq2pd_Y_Xm128_K_merge args
    | "caml_avx512f_cvt_int32x4_float64x4_maskz" ->
      instr (vcvtdq2pd_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_int32x4_float64x2_mask" ->
      instr vcvtdq2pd_X_Xm128_K_merge args
    | "caml_avx512f_cvt_int32x4_float64x2_maskz" ->
      instr (vcvtdq2pd_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_int32x8_float32x8_mask" ->
      instr vcvtdq2ps_Y_Ym256_K_merge args
    | "caml_avx512f_cvt_int32x8_float32x8_maskz" ->
      instr (vcvtdq2ps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_int32x4_float32x4_mask" ->
      instr vcvtdq2ps_X_Xm128_K_merge args
    | "caml_avx512f_cvt_int32x4_float32x4_maskz" ->
      instr (vcvtdq2ps_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_float64x4_int32x4_mask" ->
      instr vcvtpd2dq_X_Ym256_K_merge args
    | "caml_avx512f_cvt_float64x4_int32x4_maskz" ->
      instr (vcvtpd2dq_X_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float64x4_float32x4_mask" ->
      instr vcvtpd2ps_X_Ym256_K_merge args
    | "caml_avx512f_cvt_float64x4_float32x4_maskz" ->
      instr (vcvtpd2ps_X_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float64x4_int32x4_unsigned" ->
      instr vcvtpd2udq_X_Ym256 args
    | "caml_avx512f_cvt_float64x4_int32x4_unsigned_mask" ->
      instr vcvtpd2udq_X_Ym256_K_merge args
    | "caml_avx512f_cvt_float64x4_int32x4_unsigned_maskz" ->
      instr (vcvtpd2udq_X_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float32x8_int32x8_mask" ->
      instr vcvtps2dq_Y_Ym256_K_merge args
    | "caml_avx512f_cvt_float32x8_int32x8_maskz" ->
      instr (vcvtps2dq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float32x4_int32x4_mask" ->
      instr vcvtps2dq_X_Xm128_K_merge args
    | "caml_avx512f_cvt_float32x4_int32x4_maskz" ->
      instr (vcvtps2dq_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_float32x8_int32x8_unsigned" ->
      instr vcvtps2udq_Y_Ym256 args
    | "caml_avx512f_cvt_float32x8_int32x8_unsigned_mask" ->
      instr vcvtps2udq_Y_Ym256_K_merge args
    | "caml_avx512f_cvt_float32x8_int32x8_unsigned_maskz" ->
      instr (vcvtps2udq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float32x4_int32x4_unsigned" ->
      instr vcvtps2udq_X_Xm128 args
    | "caml_avx512f_cvt_float32x4_int32x4_unsigned_mask" ->
      instr vcvtps2udq_X_Xm128_K_merge args
    | "caml_avx512f_cvt_float32x4_int32x4_unsigned_maskz" ->
      instr (vcvtps2udq_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvtt_float64x4_int32x4_mask" ->
      instr vcvttpd2dq_X_Ym256_K_merge args
    | "caml_avx512f_cvtt_float64x4_int32x4_maskz" ->
      instr (vcvttpd2dq_X_Ym256_K ~z:true) args
    | "caml_avx512f_cvtt_float64x4_int32x4_unsigned" ->
      instr vcvttpd2udq_X_Ym256 args
    | "caml_avx512f_cvtt_float64x4_int32x4_unsigned_mask" ->
      instr vcvttpd2udq_X_Ym256_K_merge args
    | "caml_avx512f_cvtt_float64x4_int32x4_unsigned_maskz" ->
      instr (vcvttpd2udq_X_Ym256_K ~z:true) args
    | "caml_avx512f_cvtt_float32x8_int32x8_mask" ->
      instr vcvttps2dq_Y_Ym256_K_merge args
    | "caml_avx512f_cvtt_float32x8_int32x8_maskz" ->
      instr (vcvttps2dq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_cvtt_float32x4_int32x4_mask" ->
      instr vcvttps2dq_X_Xm128_K_merge args
    | "caml_avx512f_cvtt_float32x4_int32x4_maskz" ->
      instr (vcvttps2dq_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvtt_float32x8_int32x8_unsigned" ->
      instr vcvttps2udq_Y_Ym256 args
    | "caml_avx512f_cvtt_float32x8_int32x8_unsigned_mask" ->
      instr vcvttps2udq_Y_Ym256_K_merge args
    | "caml_avx512f_cvtt_float32x8_int32x8_unsigned_maskz" ->
      instr (vcvttps2udq_Y_Ym256_K ~z:true) args
    | "caml_avx512f_cvtt_float32x4_int32x4_unsigned" ->
      instr vcvttps2udq_X_Xm128 args
    | "caml_avx512f_cvtt_float32x4_int32x4_unsigned_mask" ->
      instr vcvttps2udq_X_Xm128_K_merge args
    | "caml_avx512f_cvtt_float32x4_int32x4_unsigned_maskz" ->
      instr (vcvttps2udq_X_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_int32x4_float64x4_unsigned" ->
      instr vcvtudq2pd_Y_Xm128 args
    | "caml_avx512f_cvt_int32x4_float64x4_unsigned_mask" ->
      instr vcvtudq2pd_Y_Xm128_K_merge args
    | "caml_avx512f_cvt_int32x4_float64x4_unsigned_maskz" ->
      instr (vcvtudq2pd_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_int32x4_float64x2_unsigned" ->
      instr vcvtudq2pd_X_Xm64 args
    | "caml_avx512f_cvt_int32x4_float64x2_unsigned_mask" ->
      instr vcvtudq2pd_X_Xm64_K_merge args
    | "caml_avx512f_cvt_int32x4_float64x2_unsigned_maskz" ->
      instr (vcvtudq2pd_X_Xm64_K ~z:true) args
    | "caml_avx512f_cvt_int32x8_int16x8" -> instr vpmovdw_Xm128_Y args
    | "caml_avx512f_cvt_int32x8_int16x8_mask" -> instr vpmovdw_X_Y_K_merge args
    | "caml_avx512f_cvt_int32x8_int16x8_maskz" ->
      instr (vpmovdw_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvt_int64x4_int32x4" -> instr vpmovqd_Xm128_Y args
    | "caml_avx512f_cvt_int64x4_int32x4_mask" -> instr vpmovqd_X_Y_K_merge args
    | "caml_avx512f_cvt_int64x4_int32x4_maskz" ->
      instr (vpmovqd_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating" ->
      instr vpmovsdw_Xm128_Y args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_mask" ->
      instr vpmovsdw_X_Y_K_merge args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_maskz" ->
      instr (vpmovsdw_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating" ->
      instr vpmovsqd_Xm128_Y args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_mask" ->
      instr vpmovsqd_X_Y_K_merge args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_maskz" ->
      instr (vpmovsqd_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int32x8_mask" ->
      instr vpmovsxbd_Y_Xm64_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int32x8_maskz" ->
      instr (vpmovsxbd_Y_Xm64_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int32x4_mask" ->
      instr vpmovsxbd_X_Xm32_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int32x4_maskz" ->
      instr (vpmovsxbd_X_Xm32_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int64x4_mask" ->
      instr vpmovsxbq_Y_Xm32_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int64x4_maskz" ->
      instr (vpmovsxbq_Y_Xm32_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int64x2_mask" ->
      instr vpmovsxbq_X_Xm16_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int64x2_maskz" ->
      instr (vpmovsxbq_X_Xm16_K ~z:true) args
    | "caml_avx512f_cvtsx_int32x4_int64x4_mask" ->
      instr vpmovsxdq_Y_Xm128_K_merge args
    | "caml_avx512f_cvtsx_int32x4_int64x4_maskz" ->
      instr (vpmovsxdq_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvtsx_int32x4_int64x2_mask" ->
      instr vpmovsxdq_X_Xm64_K_merge args
    | "caml_avx512f_cvtsx_int32x4_int64x2_maskz" ->
      instr (vpmovsxdq_X_Xm64_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x8_int32x8_mask" ->
      instr vpmovsxwd_Y_Xm128_K_merge args
    | "caml_avx512f_cvtsx_int16x8_int32x8_maskz" ->
      instr (vpmovsxwd_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x8_int32x4_mask" ->
      instr vpmovsxwd_X_Xm64_K_merge args
    | "caml_avx512f_cvtsx_int16x8_int32x4_maskz" ->
      instr (vpmovsxwd_X_Xm64_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x8_int64x4_mask" ->
      instr vpmovsxwq_Y_Xm64_K_merge args
    | "caml_avx512f_cvtsx_int16x8_int64x4_maskz" ->
      instr (vpmovsxwq_Y_Xm64_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x8_int64x2_mask" ->
      instr vpmovsxwq_X_Xm32_K_merge args
    | "caml_avx512f_cvtsx_int16x8_int64x2_maskz" ->
      instr (vpmovsxwq_X_Xm32_K ~z:true) args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_unsigned" ->
      instr vpmovusdw_Xm128_Y args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_unsigned_mask" ->
      instr vpmovusdw_X_Y_K_merge args
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_unsigned_maskz" ->
      instr (vpmovusdw_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_unsigned" ->
      instr vpmovusqd_Xm128_Y args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_unsigned_mask" ->
      instr vpmovusqd_X_Y_K_merge args
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_unsigned_maskz" ->
      instr (vpmovusqd_Xm128_Y_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int32x8_mask" ->
      instr vpmovzxbd_Y_Xm64_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int32x8_maskz" ->
      instr (vpmovzxbd_Y_Xm64_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int32x4_mask" ->
      instr vpmovzxbd_X_Xm32_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int32x4_maskz" ->
      instr (vpmovzxbd_X_Xm32_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int64x4_mask" ->
      instr vpmovzxbq_Y_Xm32_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int64x4_maskz" ->
      instr (vpmovzxbq_Y_Xm32_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int64x2_mask" ->
      instr vpmovzxbq_X_Xm16_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int64x2_maskz" ->
      instr (vpmovzxbq_X_Xm16_K ~z:true) args
    | "caml_avx512f_cvtzx_int32x4_int64x4_mask" ->
      instr vpmovzxdq_Y_Xm128_K_merge args
    | "caml_avx512f_cvtzx_int32x4_int64x4_maskz" ->
      instr (vpmovzxdq_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvtzx_int32x4_int64x2_mask" ->
      instr vpmovzxdq_X_Xm64_K_merge args
    | "caml_avx512f_cvtzx_int32x4_int64x2_maskz" ->
      instr (vpmovzxdq_X_Xm64_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x8_int32x8_mask" ->
      instr vpmovzxwd_Y_Xm128_K_merge args
    | "caml_avx512f_cvtzx_int16x8_int32x8_maskz" ->
      instr (vpmovzxwd_Y_Xm128_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x8_int32x4_mask" ->
      instr vpmovzxwd_X_Xm64_K_merge args
    | "caml_avx512f_cvtzx_int16x8_int32x4_maskz" ->
      instr (vpmovzxwd_X_Xm64_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x8_int64x4_mask" ->
      instr vpmovzxwq_Y_Xm64_K_merge args
    | "caml_avx512f_cvtzx_int16x8_int64x4_maskz" ->
      instr (vpmovzxwq_Y_Xm64_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x8_int64x2_mask" ->
      instr vpmovzxwq_X_Xm32_K_merge args
    | "caml_avx512f_cvtzx_int16x8_int64x2_maskz" ->
      instr (vpmovzxwq_X_Xm32_K ~z:true) args
    | "caml_avx512f_float64x4_mov_mask" -> instr vmovupd_Y_Y_K_merge args
    | "caml_avx512f_float64x4_mov_maskz" ->
      instr (vmovupd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_mov_mask" -> instr vmovupd_X_X_K_merge args
    | "caml_avx512f_float64x2_mov_maskz" ->
      instr (vmovupd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_mov_mask" -> instr vmovups_Y_Y_K_merge args
    | "caml_avx512f_float32x8_mov_maskz" ->
      instr (vmovups_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_mov_mask" -> instr vmovups_X_X_K_merge args
    | "caml_avx512f_float32x4_mov_maskz" ->
      instr (vmovups_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_duplicate_even_mask" ->
      instr vmovddup_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_duplicate_even_maskz" ->
      instr (vmovddup_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x8_mov_mask" -> instr vmovdqu32_Y_Y_K_merge args
    | "caml_avx512f_int32x8_mov_maskz" ->
      instr (vmovdqu32_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_mov_mask" -> instr vmovdqu32_X_X_K_merge args
    | "caml_avx512f_int32x4_mov_maskz" ->
      instr (vmovdqu32_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_mov_mask" -> instr vmovdqu64_Y_Y_K_merge args
    | "caml_avx512f_int64x4_mov_maskz" ->
      instr (vmovdqu64_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_mov_mask" -> instr vmovdqu64_X_X_K_merge args
    | "caml_avx512f_int64x2_mov_maskz" ->
      instr (vmovdqu64_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_duplicate_odd_mask" ->
      instr vmovshdup_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_duplicate_odd_maskz" ->
      instr (vmovshdup_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_duplicate_odd_mask" ->
      instr vmovshdup_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_duplicate_odd_maskz" ->
      instr (vmovshdup_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_duplicate_even_mask" ->
      instr vmovsldup_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_duplicate_even_maskz" ->
      instr (vmovsldup_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_duplicate_even_mask" ->
      instr vmovsldup_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_duplicate_even_maskz" ->
      instr (vmovsldup_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_and_mask" -> instr vpandd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_and_maskz" ->
      instr (vpandd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_and_mask" -> instr vpandd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_and_maskz" ->
      instr (vpandd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_andnot_mask" -> instr vpandnd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_andnot_maskz" ->
      instr (vpandnd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_andnot_mask" -> instr vpandnd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_andnot_maskz" ->
      instr (vpandnd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_andnot_mask" -> instr vpandnq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_andnot_maskz" ->
      instr (vpandnq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_andnot_mask" -> instr vpandnq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_andnot_maskz" ->
      instr (vpandnq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_and_mask" -> instr vpandq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_and_maskz" ->
      instr (vpandq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_and_mask" -> instr vpandq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_and_maskz" ->
      instr (vpandq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_or_mask" -> instr vpord_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_or_maskz" -> instr (vpord_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_or_mask" -> instr vpord_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_or_maskz" -> instr (vpord_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_or_mask" -> instr vporq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_or_maskz" -> instr (vporq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_or_mask" -> instr vporq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_or_maskz" -> instr (vporq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_Y_Y_Ym256_K ~z:false) ~i args
    | "caml_avx512f_int32x8_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x8_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogd_Y_Y_Ym256 ~i args
    | "caml_avx512f_int32x4_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_X_X_Xm128_K ~z:false) ~i args
    | "caml_avx512f_int32x4_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x4_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogd_X_X_Xm128 ~i args
    | "caml_avx512f_int64x4_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_Y_Y_Ym256_K ~z:false) ~i args
    | "caml_avx512f_int64x4_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_Y_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogq_Y_Y_Ym256 ~i args
    | "caml_avx512f_int64x2_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_X_X_Xm128_K ~z:false) ~i args
    | "caml_avx512f_int64x2_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_X_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x2_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogq_X_X_Xm128 ~i args
    | "caml_avx512f_int32x8_xor_mask" -> instr vpxord_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_xor_maskz" ->
      instr (vpxord_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_xor_mask" -> instr vpxord_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_xor_maskz" ->
      instr (vpxord_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_xor_mask" -> instr vpxorq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_xor_maskz" ->
      instr (vpxorq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_xor_mask" -> instr vpxorq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_xor_maskz" ->
      instr (vpxorq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_xor" -> instr vpxorq_Y_Y_Ym256 args
    | "caml_avx512f_int32x8_xor" -> instr vpxord_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_xor" -> instr vpxorq_X_X_Xm128 args
    | "caml_avx512f_int32x4_xor" -> instr vpxord_X_X_Xm128 args
    | "caml_avx512f_int64x4_or" -> instr vporq_Y_Y_Ym256 args
    | "caml_avx512f_int32x8_or" -> instr vpord_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_or" -> instr vporq_X_X_Xm128 args
    | "caml_avx512f_int32x4_or" -> instr vpord_X_X_Xm128 args
    | "caml_avx512f_int32x8_set1_mask" -> instr vpbroadcastd_Y_r32_K_merge args
    | "caml_avx512f_int32x8_set1_maskz" ->
      instr (vpbroadcastd_Y_r32_K ~z:true) args
    | "caml_avx512f_int32x4_set1_mask" -> instr vpbroadcastd_X_r32_K_merge args
    | "caml_avx512f_int32x4_set1_maskz" ->
      instr (vpbroadcastd_X_r32_K ~z:true) args
    | "caml_avx512f_int64x4_set1_mask" -> instr vpbroadcastq_Y_r64_K_merge args
    | "caml_avx512f_int64x4_set1_maskz" ->
      instr (vpbroadcastq_Y_r64_K ~z:true) args
    | "caml_avx512f_int64x2_set1_mask" -> instr vpbroadcastq_X_r64_K_merge args
    | "caml_avx512f_int64x2_set1_maskz" ->
      instr (vpbroadcastq_X_r64_K ~z:true) args
    | "caml_avx512f_int32x8_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprold_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x8_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_Y_Ym256 ~i args
    | "caml_avx512f_int32x4_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprold_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x4_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_X_Xm128 ~i args
    | "caml_avx512f_int64x4_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprolq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_Y_Ym256 ~i args
    | "caml_avx512f_int64x2_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprolq_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x2_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_X_Xm128 ~i args
    | "caml_avx512f_int32x8_rolv_mask" -> instr vprolvd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_rolv_maskz" ->
      instr (vprolvd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x8_rolv" -> instr vprolvd_Y_Y_Ym256 args
    | "caml_avx512f_int32x4_rolv_mask" -> instr vprolvd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_rolv_maskz" ->
      instr (vprolvd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_rolv" -> instr vprolvd_X_X_Xm128 args
    | "caml_avx512f_int64x4_rolv_mask" -> instr vprolvq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_rolv_maskz" ->
      instr (vprolvq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_rolv" -> instr vprolvq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_rolv_mask" -> instr vprolvq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_rolv_maskz" ->
      instr (vprolvq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_rolv" -> instr vprolvq_X_X_Xm128 args
    | "caml_avx512f_int32x8_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprord_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x8_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_Y_Ym256 ~i args
    | "caml_avx512f_int32x4_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprord_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x4_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_X_Xm128 ~i args
    | "caml_avx512f_int64x4_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprorq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_Y_Ym256 ~i args
    | "caml_avx512f_int64x2_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprorq_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x2_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_X_Xm128 ~i args
    | "caml_avx512f_int32x8_rorv_mask" -> instr vprorvd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_rorv_maskz" ->
      instr (vprorvd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x8_rorv" -> instr vprorvd_Y_Y_Ym256 args
    | "caml_avx512f_int32x4_rorv_mask" -> instr vprorvd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_rorv_maskz" ->
      instr (vprorvd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_rorv" -> instr vprorvd_X_X_Xm128 args
    | "caml_avx512f_int64x4_rorv_mask" -> instr vprorvq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_rorv_maskz" ->
      instr (vprorvq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_rorv" -> instr vprorvq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_rorv_mask" -> instr vprorvq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_rorv_maskz" ->
      instr (vprorvq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_rorv" -> instr vprorvq_X_X_Xm128 args
    | "caml_avx512f_int32x8_sll_mask" -> instr vpslld_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int32x8_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpslld_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_sll_maskz" ->
      instr (vpslld_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpslld_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x4_sll_mask" -> instr vpslld_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpslld_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_sll_maskz" ->
      instr (vpslld_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpslld_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x4_sll_mask" -> instr vpsllq_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int64x4_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_sll_maskz" ->
      instr (vpsllq_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x2_sll_mask" -> instr vpsllq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllq_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_sll_maskz" ->
      instr (vpsllq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllq_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x8_sllv_mask" -> instr vpsllvd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_sllv_maskz" ->
      instr (vpsllvd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_sllv_mask" -> instr vpsllvd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_sllv_maskz" ->
      instr (vpsllvd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_sllv_mask" -> instr vpsllvq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_sllv_maskz" ->
      instr (vpsllvq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_sllv_mask" -> instr vpsllvq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_sllv_maskz" ->
      instr (vpsllvq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_sra_mask" -> instr vpsrad_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int32x8_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrad_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_sra_maskz" ->
      instr (vpsrad_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrad_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x4_sra_mask" -> instr vpsrad_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrad_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_sra_maskz" ->
      instr (vpsrad_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_srai_maskz" ->
      let i, args = extract_constant args ~max:63 op in
      instr (vpsrad_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x4_sra_mask" -> instr vpsraq_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int64x4_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_sra_maskz" ->
      instr (vpsraq_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsraq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x4_sra" -> instr vpsraq_Y_Y_Xm128 args
    | "caml_avx512f_int64x4_srai" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraq_Y_Ym256 ~i args
    | "caml_avx512f_int64x2_sra_mask" -> instr vpsraq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraq_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_sra_maskz" ->
      instr (vpsraq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_srai_maskz" ->
      let i, args = extract_constant args ~max:127 op in
      instr (vpsraq_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x2_sra" -> instr vpsraq_X_X_Xm128 args
    | "caml_avx512f_int64x2_srai" ->
      let i, args = extract_constant args ~max:127 op in
      instr vpsraq_X_Xm128 ~i args
    | "caml_avx512f_int32x8_srav_mask" -> instr vpsravd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_srav_maskz" ->
      instr (vpsravd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_srav_mask" -> instr vpsravd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_srav_maskz" ->
      instr (vpsravd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_srav_mask" -> instr vpsravq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_srav_maskz" ->
      instr (vpsravq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x4_srav" -> instr vpsravq_Y_Y_Ym256 args
    | "caml_avx512f_int64x2_srav_mask" -> instr vpsravq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_srav_maskz" ->
      instr (vpsravq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_srav" -> instr vpsravq_X_X_Xm128 args
    | "caml_avx512f_int32x8_srl_mask" -> instr vpsrld_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int32x8_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrld_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int32x8_srl_maskz" ->
      instr (vpsrld_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int32x8_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrld_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x4_srl_mask" -> instr vpsrld_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrld_X_Xm128_K_merge ~i args
    | "caml_avx512f_int32x4_srl_maskz" ->
      instr (vpsrld_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x4_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrld_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x4_srl_mask" -> instr vpsrlq_Y_Y_Xm128_K_merge args
    | "caml_avx512f_int64x4_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlq_Y_Ym256_K_merge ~i args
    | "caml_avx512f_int64x4_srl_maskz" ->
      instr (vpsrlq_Y_Y_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlq_Y_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int64x2_srl_mask" -> instr vpsrlq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlq_X_Xm128_K_merge ~i args
    | "caml_avx512f_int64x2_srl_maskz" ->
      instr (vpsrlq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x2_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlq_X_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int32x8_srlv_mask" -> instr vpsrlvd_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int32x8_srlv_maskz" ->
      instr (vpsrlvd_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int32x4_srlv_mask" -> instr vpsrlvd_X_X_Xm128_K_merge args
    | "caml_avx512f_int32x4_srlv_maskz" ->
      instr (vpsrlvd_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_int64x4_srlv_mask" -> instr vpsrlvq_Y_Y_Ym256_K_merge args
    | "caml_avx512f_int64x4_srlv_maskz" ->
      instr (vpsrlvq_Y_Y_Ym256_K ~z:true) args
    | "caml_avx512f_int64x2_srlv_mask" -> instr vpsrlvq_X_X_Xm128_K_merge args
    | "caml_avx512f_int64x2_srlv_maskz" ->
      instr (vpsrlvq_X_X_Xm128_K ~z:true) args
    | "caml_avx512f_float64x4_sqrt_mask" -> instr vsqrtpd_Y_Ym256_K_merge args
    | "caml_avx512f_float64x4_sqrt_maskz" ->
      instr (vsqrtpd_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float64x2_sqrt_mask" -> instr vsqrtpd_X_Xm128_K_merge args
    | "caml_avx512f_float64x2_sqrt_maskz" ->
      instr (vsqrtpd_X_Xm128_K ~z:true) args
    | "caml_avx512f_float32x8_sqrt_mask" -> instr vsqrtps_Y_Ym256_K_merge args
    | "caml_avx512f_float32x8_sqrt_maskz" ->
      instr (vsqrtps_Y_Ym256_K ~z:true) args
    | "caml_avx512f_float32x4_sqrt_mask" -> instr vsqrtps_X_Xm128_K_merge args
    | "caml_avx512f_float32x4_sqrt_maskz" ->
      instr (vsqrtps_X_Xm128_K ~z:true) args
    | "caml_avx512f_int32x16_mul_low_maskz" ->
      instr (vpmulld_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_add_maskz" ->
      instr (vaddpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddpd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vaddpd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vaddpd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vaddpd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_add_maskz" ->
      instr (vaddps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vaddps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vaddps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vaddps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vaddsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vaddsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vaddsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vaddsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vaddsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vaddsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_add_mask" -> instr vaddsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vaddsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vaddsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vaddsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_add_maskz" -> instr (vaddsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float32_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vaddss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vaddss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vaddss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vaddss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vaddss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vaddss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_add_mask" -> instr vaddss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vaddss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vaddss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vaddss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_add_maskz" -> instr (vaddss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float64x8_div" -> instr vdivpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_div_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivpd_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vdivpd_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vdivpd_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vdivpd_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_div_mask" -> instr vdivpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_div_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivpd_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vdivpd_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vdivpd_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vdivpd_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_div_maskz" ->
      instr (vdivpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_div_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivpd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vdivpd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vdivpd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vdivpd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_div" -> instr vdivps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_div_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivps_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vdivps_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vdivps_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vdivps_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_div_mask" -> instr vdivps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_div_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivps_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vdivps_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vdivps_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vdivps_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_div_maskz" ->
      instr (vdivps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_div_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vdivps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vdivps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vdivps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_div_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vdivsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vdivsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vdivsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_div_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vdivsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vdivsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vdivsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_div_mask" -> instr vdivsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_div_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vdivsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vdivsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vdivsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_div_maskz" -> instr (vdivsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float32_div_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vdivss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vdivss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vdivss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_div_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vdivss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vdivss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vdivss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_div_mask" -> instr vdivss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_div_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vdivss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vdivss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vdivss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vdivss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_div_maskz" -> instr (vdivss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float64x8_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132sd_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132sd_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132sd_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132sd_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmadd231sd_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmadd231sd_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmadd231sd_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmadd231sd_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231sd_X_X_Xm64_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmadd132sd_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmadd132sd_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132sd_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmadd132sd_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132sd_X_X_Xm64_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132sd_X_X_X_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132sd_X_X_X_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132sd_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132sd_X_X_X_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132sd_X_X_Xm64_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmadd231ss_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmadd231ss_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmadd231ss_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmadd231ss_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231ss_X_X_Xm32_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132ss_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132ss_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ss_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132ss_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmadd132ss_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmadd132ss_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ss_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmadd132ss_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ss_X_X_Xm32_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132ss_X_X_X_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132ss_X_X_X_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ss_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132ss_X_X_X_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ss_X_X_Xm32_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmaddsub132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmaddsub132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmaddsub132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmaddsub132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmaddsub132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr
          (vfmaddsub231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr
          (vfmaddsub231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmaddsub231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr
          (vfmaddsub231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_addsub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmaddsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmaddsub132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmaddsub132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmaddsub132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmaddsub132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmaddsub132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmaddsub231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr
          (vfmaddsub231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr
          (vfmaddsub231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmaddsub231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr
          (vfmaddsub231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmaddsub132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_addsub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmaddsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132sd_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132sd_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132sd_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132sd_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmsub231sd_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmsub231sd_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsub231sd_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmsub231sd_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231sd_X_X_Xm64_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsub132sd_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsub132sd_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132sd_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsub132sd_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132sd_X_X_Xm64_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132sd_X_X_X_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132sd_X_X_X_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132sd_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132sd_X_X_X_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132sd_X_X_Xm64_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132ss_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132ss_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ss_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132ss_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmsub231ss_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmsub231ss_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsub231ss_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmsub231ss_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231ss_X_X_Xm32_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsub132ss_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsub132ss_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ss_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsub132ss_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ss_X_X_Xm32_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132ss_X_X_X_K ~rnd:Rnd_near ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132ss_X_X_X_K ~rnd:Rnd_down ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ss_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132ss_X_X_X_K ~rnd:Rnd_zero ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ss_X_X_Xm32_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmsubadd132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsubadd132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsubadd132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsubadd132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsubadd132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr
          (vfmsubadd231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr
          (vfmsubadd231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsubadd231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr
          (vfmsubadd231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_subadd_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsubadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmsubadd132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsubadd132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsubadd132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsubadd132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsubadd132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsubadd231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr
          (vfmsubadd231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr
          (vfmsubadd231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsubadd231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr
          (vfmsubadd231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsubadd132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_subadd_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsubadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmadd132sd_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmadd132sd_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132sd_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmadd132sd_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmadd231sd_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmadd231sd_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmadd231sd_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmadd231sd_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231sd_X_X_Xm64_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132sd_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132sd_X_X_Xm64_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132sd_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132sd_X_X_X_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132sd_X_X_Xm64_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmadd132ss_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmadd132ss_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ss_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmadd132ss_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmadd231ss_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmadd231ss_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmadd231ss_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmadd231ss_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231ss_X_X_Xm32_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ss_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ss_X_X_Xm32_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ss_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132ss_X_X_X_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_add_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ss_X_X_Xm32_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_Z_Z_Zm512_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmsub132sd_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmsub132sd_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132sd_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmsub132sd_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmsub231sd_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmsub231sd_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmsub231sd_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmsub231sd_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231sd_X_X_Xm64_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132sd_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132sd_X_X_Xm64_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132sd_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132sd_X_X_X_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132sd_X_X_Xm64_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmsub132ss_X_X_X ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmsub132ss_X_X_X ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ss_X_X_X ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmsub132ss_X_X_X ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmsub231ss_X_X_X_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmsub231ss_X_X_X_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmsub231ss_X_X_X_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmsub231ss_X_X_X_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231ss_X_X_Xm32_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ss_X_X_X_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ss_X_X_Xm32_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_near ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_down ~z:true)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ss_X_X_X_K ~rnd:Rnd_up ~z:true) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132ss_X_X_X_K ~rnd:Rnd_zero ~z:true)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32_neg_mul_sub_maskz" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ss_X_X_Xm32_K ~z:true) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_maskz" ->
      instr (vmulpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_mul_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulpd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vmulpd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vmulpd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vmulpd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_mul_maskz" ->
      instr (vmulps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_mul_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vmulps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vmulps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vmulps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_mul_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vmulsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vmulsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vmulsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_mul_mask" -> instr vmulsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_mul_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vmulsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vmulsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vmulsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_mul_maskz" -> instr (vmulsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_mul_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vmulsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vmulsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vmulsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_mul_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vmulss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vmulss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vmulss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_mul_mask" -> instr vmulss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_mul_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vmulss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vmulss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vmulss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_mul_maskz" -> instr (vmulss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_mul_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vmulss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vmulss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vmulss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32x16_add_maskz" ->
      instr (vpaddd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_add" -> instr vpaddq_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_add_mask" -> instr vpaddq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_add_maskz" ->
      instr (vpaddq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_mul_mask" -> instr vpmuldq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_mul_maskz" ->
      instr (vpmuldq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_mul" -> instr vpmuldq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_mul_unsigned_mask" ->
      instr vpmuludq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_mul_unsigned_maskz" ->
      instr (vpmuludq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_mul_unsigned" -> instr vpmuludq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_sub_maskz" ->
      instr (vpsubd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_sub_mask" -> instr vpsubq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_sub_maskz" ->
      instr (vpsubq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_sub" -> instr vpsubq_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_sub_maskz" ->
      instr (vsubpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubpd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsubpd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsubpd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsubpd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_sub_maskz" ->
      instr (vsubps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsubps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsubps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsubps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsubsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsubsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsubsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_sub_mask" -> instr vsubsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsubsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsubsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsubsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_sub_maskz" -> instr (vsubsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vsubsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vsubsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vsubsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsubss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsubss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsubss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_sub_mask" -> instr vsubss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_sub_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsubss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsubss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsubss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_sub_maskz" -> instr (vsubss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vsubss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vsubss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vsubss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_mask16_and" -> instr kandw args
    | "caml_avx512f_mask16_andnot" -> instr kandnw args
    | "caml_avx512f_mask16_not" -> instr knotw args
    | "caml_avx512f_mask16_or" -> instr korw args
    | "caml_avx512f_mask16_xnor" -> instr kxnorw args
    | "caml_avx512f_mask16_xor" -> instr kxorw args
    | "caml_avx512f_mask16_shift_left" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftlw ~i args
    | "caml_avx512f_mask16_shift_right" ->
      let i, args = extract_constant args ~max:255 op in
      instr kshiftrw ~i args
    | "caml_avx512f_mask16_to_int32" -> instr kmovw_r32_K args
    | "caml_avx512f_int32_to_mask16" -> instr kmovw_K_r32 args
    | "caml_avx512f_mask16_unpack" -> instr kunpckbw args
    | "caml_avx512f_int32x16_align_right_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (valignd_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_align_right" ->
      let i, args = extract_constant args ~max:7 op in
      instr valignq_Z_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_align_right_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr valignq_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_align_right_maskz" ->
      let i, args = extract_constant args ~max:7 op in
      instr (valignq_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmpd_Z_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_fixupimm_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Z_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float64x8_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Z_Z_Zm512_K ~z:false) ~i args
    | "caml_avx512f_float64x8_fixupimm_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Z_Z_Z_K ~sae:true ~z:false) ~i args
    | "caml_avx512f_float64x8_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_fixupimm_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmpd_Z_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32x16_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmps_Z_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_fixupimm_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Z_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float32x16_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Z_Z_Zm512_K ~z:false) ~i args
    | "caml_avx512f_float32x16_fixupimm_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Z_Z_Z_K ~sae:true ~z:false) ~i args
    | "caml_avx512f_float32x16_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_fixupimm_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmps_Z_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64_fixupimm_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmsd_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float64_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmsd_X_X_Xm64 ~i args
    | "caml_avx512f_float64_fixupimm_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmsd_X_X_X_K ~sae:true ~z:false) ~i args
    | "caml_avx512f_float64_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmsd_X_X_Xm64_K ~z:false) ~i args
    | "caml_avx512f_float64_fixupimm_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmsd_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmsd_X_X_Xm64_K ~z:true) ~i args
    | "caml_avx512f_float32_fixupimm_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmss_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float32_fixupimm" ->
      let i, args = extract_constant args ~max:255 op in
      instr vfixupimmss_X_X_Xm32 ~i args
    | "caml_avx512f_float32_fixupimm_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmss_X_X_X_K ~sae:true ~z:false) ~i args
    | "caml_avx512f_float32_fixupimm_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmss_X_X_Xm32_K ~z:false) ~i args
    | "caml_avx512f_float32_fixupimm_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmss_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32_fixupimm_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vfixupimmss_X_X_Xm32_K ~z:true) ~i args
    | "caml_avx512f_float64x8_getexp_maskz" ->
      instr (vgetexppd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_getexp_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexppd_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float32x16_getexp_maskz" ->
      instr (vgetexpps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_getexp_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpps_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float64_getexp_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpsd_X_X_X ~sae:true) args
    | "caml_avx512f_float64_getexp" -> instr vgetexpsd_X_X_Xm64 args
    | "caml_avx512f_float64_getexp_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpsd_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float64_getexp_mask" ->
      instr vgetexpsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_getexp_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpsd_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float64_getexp_maskz" ->
      instr (vgetexpsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float32_getexp_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpss_X_X_X ~sae:true) args
    | "caml_avx512f_float32_getexp" -> instr vgetexpss_X_X_Xm32 args
    | "caml_avx512f_float32_getexp_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpss_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float32_getexp_mask" ->
      instr vgetexpss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_getexp_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpss_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float32_getexp_maskz" ->
      instr (vgetexpss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float64x8_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_getmant_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32x16_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_getmant_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64_getmant_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantsd_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float64_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantsd_X_X_Xm64 ~i args
    | "caml_avx512f_float64_getmant_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantsd_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512f_float64_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantsd_X_X_Xm64_K_merge ~i args
    | "caml_avx512f_float64_getmant_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantsd_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantsd_X_X_Xm64_K ~z:true) ~i args
    | "caml_avx512f_float32_getmant_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantss_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float32_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantss_X_X_Xm32 ~i args
    | "caml_avx512f_float32_getmant_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantss_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512f_float32_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantss_X_X_Xm32_K_merge ~i args
    | "caml_avx512f_float32_getmant_round_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantss_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32_getmant_maskz" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantss_X_X_Xm32_K ~z:true) ~i args
    | "caml_avx512f_int32x16_rorv_maskz" ->
      instr (vprorvd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_roundscale_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512f_float64x8_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_roundscale_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64x8_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalepd_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_roundscale_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalepd_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float32x16_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float32x16_roundscale_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512f_float32x16_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_roundscale_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_Z_Z_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32x16_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaleps_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_roundscale_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaleps_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float64_roundscale_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalesd_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512f_float64_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalesd_X_X_Xm64_K_merge ~i args
    | "caml_avx512f_float64_roundscale_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalesd_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float64_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalesd_X_X_Xm64_K ~z:true) ~i args
    | "caml_avx512f_float64_roundscale_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscalesd_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float64_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscalesd_X_X_Xm64 ~i args
    | "caml_avx512f_float32_roundscale_round_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaless_X_X_X_K_merge ~sae:true) ~i args
    | "caml_avx512f_float32_roundscale_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaless_X_X_Xm32_K_merge ~i args
    | "caml_avx512f_float32_roundscale_round_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaless_X_X_X_K ~sae:true ~z:true) ~i args
    | "caml_avx512f_float32_roundscale_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaless_X_X_Xm32_K ~z:true) ~i args
    | "caml_avx512f_float32_roundscale_round" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vrndscaless_X_X_X ~sae:true) ~i args
    | "caml_avx512f_float32_roundscale" ->
      let i, args = extract_constant args ~max:255 op in
      instr vrndscaless_X_X_Xm32 ~i args
    | "caml_avx512f_float64x8_scalef_mask" ->
      instr vscalefpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_scalef_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefpd_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vscalefpd_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vscalefpd_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vscalefpd_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_scalef_maskz" ->
      instr (vscalefpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_scalef_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefpd_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vscalefpd_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vscalefpd_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vscalefpd_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64x8_scalef" -> instr vscalefpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_scalef_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefpd_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vscalefpd_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vscalefpd_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vscalefpd_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_scalef_mask" ->
      instr vscalefps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_scalef_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefps_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vscalefps_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vscalefps_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vscalefps_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_scalef_maskz" ->
      instr (vscalefps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_scalef_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefps_Z_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vscalefps_Z_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vscalefps_Z_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vscalefps_Z_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_scalef" -> instr vscalefps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_scalef_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefps_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vscalefps_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vscalefps_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vscalefps_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_scalef_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vscalefsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vscalefsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vscalefsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_scalef_mask" ->
      instr vscalefsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_scalef_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vscalefsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vscalefsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vscalefsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_scalef_maskz" ->
      instr (vscalefsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_scalef_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vscalefsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vscalefsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vscalefsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_scalef" -> instr vscalefsd_X_X_Xm64 args
    | "caml_avx512f_float32_scalef_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vscalefss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vscalefss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vscalefss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_scalef_mask" ->
      instr vscalefss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_scalef_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vscalefss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vscalefss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vscalefss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_scalef_maskz" ->
      instr (vscalefss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_scalef_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vscalefss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vscalefss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vscalefss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vscalefss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_scalef" -> instr vscalefss_X_X_Xm32 args
    | "caml_avx512f_float64x8_broadcast" -> instr vbroadcastsd_Z_Xm64 args
    | "caml_avx512f_float64x8_broadcast_mask" ->
      instr vbroadcastsd_Z_Xm64_K_merge args
    | "caml_avx512f_float64x8_broadcast_maskz" ->
      instr (vbroadcastsd_Z_Xm64_K ~z:true) args
    | "caml_avx512f_float32x16_broadcast" -> instr vbroadcastss_Z_Xm32 args
    | "caml_avx512f_float32x16_broadcast_mask" ->
      instr vbroadcastss_Z_Xm32_K_merge args
    | "caml_avx512f_float32x16_broadcast_maskz" ->
      instr (vbroadcastss_Z_Xm32_K ~z:true) args
    | "caml_avx512f_float64x8_compress_mask" ->
      instr vcompresspd_Z_Z_K_merge args
    | "caml_avx512f_float64x8_compress_maskz" ->
      instr (vcompresspd_Zm512_Z_K ~z:true) args
    | "caml_avx512f_float32x16_compress_mask" ->
      instr vcompressps_Z_Z_K_merge args
    | "caml_avx512f_float32x16_compress_maskz" ->
      instr (vcompressps_Zm512_Z_K ~z:true) args
    | "caml_avx512f_float64x8_expand_mask" ->
      instr vexpandpd_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_expand_maskz" ->
      instr (vexpandpd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_expand_mask" ->
      instr vexpandps_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_expand_maskz" ->
      instr (vexpandps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_extract_float32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextractf32x4_Xm128_Z ~i args
    | "caml_avx512f_float32x16_extract_float32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextractf32x4_X_Z_K_merge ~i args
    | "caml_avx512f_float32x16_extract_float32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vextractf32x4_Xm128_Z_K ~z:true) ~i args
    | "caml_avx512f_float64x8_extract_float64x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf64x4_Ym256_Z ~i args
    | "caml_avx512f_float64x8_extract_float64x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextractf64x4_Y_Z_K_merge ~i args
    | "caml_avx512f_float64x8_extract_float64x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextractf64x4_Ym256_Z_K ~z:true) ~i args
    | "caml_avx512f_int32x16_extract_int32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextracti32x4_Xm128_Z ~i args
    | "caml_avx512f_int32x16_extract_int32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vextracti32x4_X_Z_K_merge ~i args
    | "caml_avx512f_int32x16_extract_int32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vextracti32x4_Xm128_Z_K ~z:true) ~i args
    | "caml_avx512f_int64x8_extract_int64x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti64x4_Ym256_Z ~i args
    | "caml_avx512f_int64x8_extract_int64x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vextracti64x4_Y_Z_K_merge ~i args
    | "caml_avx512f_int64x8_extract_int64x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vextracti64x4_Ym256_Z_K ~z:true) ~i args
    | "caml_avx512f_float32x16_insert_float32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinsertf32x4_Z_Z_Xm128 ~i args
    | "caml_avx512f_float32x16_insert_float32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinsertf32x4_Z_Z_Xm128_K_merge ~i args
    | "caml_avx512f_float32x16_insert_float32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vinsertf32x4_Z_Z_Xm128_K ~z:true) ~i args
    | "caml_avx512f_float64x8_insert_float64x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf64x4_Z_Z_Ym256 ~i args
    | "caml_avx512f_float64x8_insert_float64x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinsertf64x4_Z_Z_Ym256_K_merge ~i args
    | "caml_avx512f_float64x8_insert_float64x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinsertf64x4_Z_Z_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x16_insert_int32x4" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinserti32x4_Z_Z_Xm128 ~i args
    | "caml_avx512f_int32x16_insert_int32x4_mask" ->
      let i, args = extract_constant args ~max:3 op in
      instr vinserti32x4_Z_Z_Xm128_K_merge ~i args
    | "caml_avx512f_int32x16_insert_int32x4_maskz" ->
      let i, args = extract_constant args ~max:3 op in
      instr (vinserti32x4_Z_Z_Xm128_K ~z:true) ~i args
    | "caml_avx512f_int64x8_insert_int64x4" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti64x4_Z_Z_Ym256 ~i args
    | "caml_avx512f_int64x8_insert_int64x4_mask" ->
      let i, args = extract_constant args ~max:1 op in
      instr vinserti64x4_Z_Z_Ym256_K_merge ~i args
    | "caml_avx512f_int64x8_insert_int64x4_maskz" ->
      let i, args = extract_constant args ~max:1 op in
      instr (vinserti64x4_Z_Z_Ym256_K ~z:true) ~i args
    | "caml_avx512f_int32x16_broadcast" -> instr vpbroadcastd_Z_Xm32 args
    | "caml_avx512f_int32x16_broadcast_mask" ->
      instr vpbroadcastd_Z_Xm32_K_merge args
    | "caml_avx512f_int32x16_broadcast_maskz" ->
      instr (vpbroadcastd_Z_Xm32_K ~z:true) args
    | "caml_avx512f_int64x8_broadcast" -> instr vpbroadcastq_Z_Xm64 args
    | "caml_avx512f_int64x8_broadcast_mask" ->
      instr vpbroadcastq_Z_Xm64_K_merge args
    | "caml_avx512f_int64x8_broadcast_maskz" ->
      instr (vpbroadcastq_Z_Xm64_K ~z:true) args
    | "caml_avx512f_int32x16_compress_mask" ->
      instr vpcompressd_Z_Z_K_merge args
    | "caml_avx512f_int32x16_compress_maskz" ->
      instr (vpcompressd_Zm512_Z_K ~z:true) args
    | "caml_avx512f_int64x8_compress_mask" -> instr vpcompressq_Z_Z_K_merge args
    | "caml_avx512f_int64x8_compress_maskz" ->
      instr (vpcompressq_Zm512_Z_K ~z:true) args
    | "caml_avx512f_int32x16_permutexvar_mask" ->
      instr vpermd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_permutexvar_maskz" ->
      instr (vpermd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_permutexvar" -> instr vpermd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2d_Z_Z_Zm512_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int32x16_permutex2var_mask" ->
      instr (vpermt2d_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_int32x16_permutex2var_maskz" ->
      instr (vpermt2d_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_permutex2var" -> instr vpermt2d_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2pd_Z_Z_Zm512_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_permutex2var_mask" ->
      instr (vpermt2pd_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_float64x8_permutex2var_maskz" ->
      instr (vpermt2pd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_permutex2var" -> instr vpermt2pd_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2ps_Z_Z_Zm512_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_permutex2var_mask" ->
      instr (vpermt2ps_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_float32x16_permutex2var_maskz" ->
      instr (vpermt2ps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_permutex2var" -> instr vpermt2ps_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_permutex2var_mask2" -> (
      match args with
      | a :: b :: rest -> instr (vpermi2q_Z_Z_Zm512_K ~z:false) (b :: a :: rest)
      | _ -> None)
    | "caml_avx512f_int64x8_permutex2var_mask" ->
      instr (vpermt2q_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_int64x8_permutex2var_maskz" ->
      instr (vpermt2q_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_permutex2var" -> instr vpermt2q_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_permute_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilpd_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_permutevar_mask" ->
      instr vpermilpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_permute_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermilpd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_permutevar_maskz" ->
      instr (vpermilpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_permute" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilpd_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_permutevar" -> instr vpermilpd_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_permute_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float32x16_permutevar_mask" ->
      instr vpermilps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_permute_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermilps_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_permutevar_maskz" ->
      instr (vpermilps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_permute" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermilps_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_permutevar" -> instr vpermilps_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_permute_across_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermpd_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_permutexvar_mask" ->
      instr vpermpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_permute_across_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermpd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_permutexvar_maskz" ->
      instr (vpermpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_permute_across" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermpd_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_permutexvar" -> instr vpermpd_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_permutexvar_mask" ->
      instr vpermps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_permutexvar_maskz" ->
      instr (vpermps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_permutexvar" -> instr vpermps_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_permute_across_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_permutexvar_mask" ->
      instr vpermq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_permute_across_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpermq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_permutexvar_maskz" ->
      instr (vpermq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_permute_across" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpermq_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_permutexvar" -> instr vpermq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_expand_mask" ->
      instr vpexpandd_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_expand_maskz" ->
      instr (vpexpandd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_expand_mask" -> instr vpexpandq_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_expand_maskz" ->
      instr (vpexpandq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_shuffle_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpshufd_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_interleave_high_mask" ->
      instr vpunpckhdq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_interleave_high_maskz" ->
      instr (vpunpckhdq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_interleave_high" -> instr vpunpckhdq_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_interleave_high_mask" ->
      instr vpunpckhqdq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_interleave_high_maskz" ->
      instr (vpunpckhqdq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_interleave_high" -> instr vpunpckhqdq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_interleave_low_mask" ->
      instr vpunpckldq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_interleave_low_maskz" ->
      instr (vpunpckldq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_interleave_low" -> instr vpunpckldq_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_interleave_low_mask" ->
      instr vpunpcklqdq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_interleave_low_maskz" ->
      instr (vpunpcklqdq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_interleave_low" -> instr vpunpcklqdq_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_shuffle_f32x4_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshuff32x4_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float32x16_shuffle_f32x4_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshuff32x4_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_shuffle_f32x4" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshuff32x4_Z_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_shuffle_f64x2_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshuff64x2_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_shuffle_f64x2_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshuff64x2_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_shuffle_f64x2" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshuff64x2_Z_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_shuffle_i32x4_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufi32x4_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_shuffle_i32x4_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufi32x4_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_shuffle_i32x4" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufi32x4_Z_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_shuffle_i64x2_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufi64x2_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_shuffle_i64x2_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufi64x2_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_shuffle_i64x2" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufi64x2_Z_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufpd_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufpd_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float64x8_shuffle_inlane" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufpd_Z_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_shuffle_inlane_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufps_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float32x16_shuffle_inlane_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vshufps_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_float32x16_shuffle_inlane" ->
      let i, args = extract_constant args ~max:255 op in
      instr vshufps_Z_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_interleave_high_mask" ->
      instr vunpckhpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_interleave_high_maskz" ->
      instr (vunpckhpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_interleave_high" -> instr vunpckhpd_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_interleave_high_mask" ->
      instr vunpckhps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_interleave_high_maskz" ->
      instr (vunpckhps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_interleave_high" ->
      instr vunpckhps_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_interleave_low_mask" ->
      instr vunpcklpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_interleave_low_maskz" ->
      instr (vunpcklpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_interleave_low" -> instr vunpcklpd_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_interleave_low_mask" ->
      instr vunpcklps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_interleave_low_maskz" ->
      instr (vunpcklps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_interleave_low" -> instr vunpcklps_Z_Z_Zm512 args
    | "caml_avx512f_float64_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpsd_K_X_Xm64 ~i args
    | "caml_avx512f_float64_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpsd_K_X_Xm64_K ~i args
    | "caml_avx512f_float32_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpss_K_X_Xm32 ~i args
    | "caml_avx512f_float32_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpss_K_X_Xm32_K ~i args
    | "caml_avx512f_int32x16_cmplt" -> instr vpcmpd_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_int32x16_cmplt_mask" -> instr vpcmpd_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_int64x8_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_cmpeq" -> instr vpcmpeqq_K_Z_Zm512 args
    | "caml_avx512f_int64x8_cmpge" -> instr vpcmpq_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_int64x8_cmpgt" -> instr vpcmpgtq_K_Z_Zm512 args
    | "caml_avx512f_int64x8_cmple" -> instr vpcmpq_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_int64x8_cmplt" -> instr vpcmpq_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_int64x8_cmpneq" -> instr vpcmpq_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_int64x8_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpq_K_Z_Zm512_K ~i args
    | "caml_avx512f_int64x8_cmpeq_mask" -> instr vpcmpeqq_K_Z_Zm512_K args
    | "caml_avx512f_int64x8_cmpge_mask" -> instr vpcmpq_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_int64x8_cmpgt_mask" -> instr vpcmpgtq_K_Z_Zm512_K args
    | "caml_avx512f_int64x8_cmple_mask" -> instr vpcmpq_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_int64x8_cmplt_mask" -> instr vpcmpq_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_int64x8_cmpneq_mask" -> instr vpcmpq_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_int64x8_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_cmpeq_unsigned" -> instr vpcmpuq_K_Z_Zm512 ~i:0 args
    | "caml_avx512f_int64x8_cmpge_unsigned" -> instr vpcmpuq_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_int64x8_cmpgt_unsigned" -> instr vpcmpuq_K_Z_Zm512 ~i:6 args
    | "caml_avx512f_int64x8_cmple_unsigned" -> instr vpcmpuq_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_int64x8_cmplt_unsigned" -> instr vpcmpuq_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_int64x8_cmpneq_unsigned" ->
      instr vpcmpuq_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_int64x8_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpuq_K_Z_Zm512_K ~i args
    | "caml_avx512f_int64x8_cmpeq_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:0 args
    | "caml_avx512f_int64x8_cmpge_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_int64x8_cmpgt_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:6 args
    | "caml_avx512f_int64x8_cmple_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_int64x8_cmplt_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_int64x8_cmpneq_unsigned_mask" ->
      instr vpcmpuq_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_cvt_int32x8_float64x8" -> instr vcvtdq2pd_Z_Ym256 args
    | "caml_avx512f_cvt_int32x8_float64x8_mask" ->
      instr vcvtdq2pd_Z_Ym256_K_merge args
    | "caml_avx512f_cvt_int32x8_float64x8_maskz" ->
      instr (vcvtdq2pd_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_float32x16_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtdq2ps_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtdq2ps_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtdq2ps_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtdq2ps_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16" -> instr vcvtdq2ps_Z_Zm512 args
    | "caml_avx512f_cvt_int32x16_float32x16_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtdq2ps_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtdq2ps_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtdq2ps_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtdq2ps_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16_mask" ->
      instr vcvtdq2ps_Z_Zm512_K_merge args
    | "caml_avx512f_cvt_int32x16_float32x16_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtdq2ps_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtdq2ps_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtdq2ps_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtdq2ps_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16_maskz" ->
      instr (vcvtdq2ps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_cvt_float64x8_int32x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2dq_Y_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2dq_Y_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2dq_Y_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2dq_Y_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8" -> instr vcvtpd2dq_Y_Zm512 args
    | "caml_avx512f_cvt_float64x8_int32x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2dq_Y_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2dq_Y_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2dq_Y_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2dq_Y_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8_mask" ->
      instr vcvtpd2dq_Y_Zm512_K_merge args
    | "caml_avx512f_cvt_float64x8_int32x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2dq_Y_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtpd2dq_Y_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtpd2dq_Y_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtpd2dq_Y_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8_maskz" ->
      instr (vcvtpd2dq_Y_Zm512_K ~z:true) args
    | "caml_avx512f_cvt_float64x8_float32x8_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2ps_Y_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2ps_Y_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2ps_Y_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2ps_Y_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_float32x8" -> instr vcvtpd2ps_Y_Zm512 args
    | "caml_avx512f_cvt_float64x8_float32x8_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2ps_Y_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2ps_Y_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2ps_Y_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2ps_Y_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_float32x8_mask" ->
      instr vcvtpd2ps_Y_Zm512_K_merge args
    | "caml_avx512f_cvt_float64x8_float32x8_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2ps_Y_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtpd2ps_Y_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtpd2ps_Y_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtpd2ps_Y_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_float32x8_maskz" ->
      instr (vcvtpd2ps_Y_Zm512_K ~z:true) args
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2udq_Y_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2udq_Y_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2udq_Y_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2udq_Y_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned" ->
      instr vcvtpd2udq_Y_Zm512 args
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2udq_Y_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtpd2udq_Y_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtpd2udq_Y_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtpd2udq_Y_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned_mask" ->
      instr vcvtpd2udq_Y_Zm512_K_merge args
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtpd2udq_Y_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtpd2udq_Y_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtpd2udq_Y_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtpd2udq_Y_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_float64x8_int32x8_unsigned_maskz" ->
      instr (vcvtpd2udq_Y_Zm512_K ~z:true) args
    | "caml_avx512f_cvt_float32x16_int32x16_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2dq_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2dq_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2dq_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2dq_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16" -> instr vcvtps2dq_Z_Zm512 args
    | "caml_avx512f_cvt_float32x16_int32x16_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2dq_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2dq_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2dq_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2dq_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16_mask" ->
      instr vcvtps2dq_Z_Zm512_K_merge args
    | "caml_avx512f_cvt_float32x16_int32x16_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2dq_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtps2dq_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtps2dq_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtps2dq_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16_maskz" ->
      instr (vcvtps2dq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_cvt_float32x8_float64x8" -> instr vcvtps2pd_Z_Ym256 args
    | "caml_avx512f_cvt_float32x8_float64x8_mask" ->
      instr vcvtps2pd_Z_Ym256_K_merge args
    | "caml_avx512f_cvt_float32x8_float64x8_maskz" ->
      instr (vcvtps2pd_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2udq_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2udq_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2udq_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2udq_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned" ->
      instr vcvtps2udq_Z_Zm512 args
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2udq_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtps2udq_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtps2udq_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtps2udq_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned_mask" ->
      instr vcvtps2udq_Z_Zm512_K_merge args
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtps2udq_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtps2udq_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtps2udq_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtps2udq_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_float32x16_int32x16_unsigned_maskz" ->
      instr (vcvtps2udq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64_cvt_int32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsd2si_r32_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsd2si_r32_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsd2si_r32_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsd2si_r32_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_cvt_int64_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsd2si_r64_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsd2si_r64_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsd2si_r64_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsd2si_r64_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_cvt_int32" -> instr vcvtsd2si_r32_Xm64 args
    | "caml_avx512f_float64_cvt_int64" -> instr vcvtsd2si_r64_Xm64 args
    | "caml_avx512f_float64_cvt_int32_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsd2usi_r32_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsd2usi_r32_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsd2usi_r32_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsd2usi_r32_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_cvt_int64_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsd2usi_r64_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsd2usi_r64_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsd2usi_r64_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsd2usi_r64_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_cvt_int32_unsigned" ->
      instr vcvtsd2usi_r32_Xm64 args
    | "caml_avx512f_float64_cvt_int64_unsigned" ->
      instr vcvtsd2usi_r64_Xm64 args
    | "caml_avx512f_int64_cvt_float64_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsi2sd_X_X_r64 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsi2sd_X_X_r64 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsi2sd_X_X_r64 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsi2sd_X_X_r64 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32_cvt_float64" -> instr vcvtsi2sd_X_X_r32m32 args
    | "caml_avx512f_int64_cvt_float64" -> instr vcvtsi2sd_X_X_r64m64 args
    | "caml_avx512f_int32_cvt_float32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsi2ss_X_X_r32 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsi2ss_X_X_r32 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsi2ss_X_X_r32 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsi2ss_X_X_r32 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int64_cvt_float32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtsi2ss_X_X_r64 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtsi2ss_X_X_r64 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtsi2ss_X_X_r64 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtsi2ss_X_X_r64 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32_cvt_float32" -> instr vcvtsi2ss_X_X_r32m32 args
    | "caml_avx512f_int64_cvt_float32" -> instr vcvtsi2ss_X_X_r64m64 args
    | "caml_avx512f_float32_cvt_int32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtss2si_r32_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtss2si_r32_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtss2si_r32_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtss2si_r32_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_cvt_int64_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtss2si_r64_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtss2si_r64_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtss2si_r64_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtss2si_r64_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_cvt_int32" -> instr vcvtss2si_r32_Xm32 args
    | "caml_avx512f_float32_cvt_int64" -> instr vcvtss2si_r64_Xm32 args
    | "caml_avx512f_float32_cvt_int32_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtss2usi_r32_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtss2usi_r32_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtss2usi_r32_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtss2usi_r32_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_cvt_int64_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtss2usi_r64_X ~rnd:Rnd_near) args
      | 9 -> instr (vcvtss2usi_r64_X ~rnd:Rnd_down) args
      | 10 -> instr (vcvtss2usi_r64_X ~rnd:Rnd_up) args
      | 11 -> instr (vcvtss2usi_r64_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_cvt_int32_unsigned" ->
      instr vcvtss2usi_r32_Xm32 args
    | "caml_avx512f_float32_cvt_int64_unsigned" ->
      instr vcvtss2usi_r64_Xm32 args
    | "caml_avx512f_cvtt_float64x8_int32x8_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2dq_Y_Z ~sae:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8" -> instr vcvttpd2dq_Y_Zm512 args
    | "caml_avx512f_cvtt_float64x8_int32x8_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2dq_Y_Z_K_merge ~sae:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_mask" ->
      instr vcvttpd2dq_Y_Zm512_K_merge args
    | "caml_avx512f_cvtt_float64x8_int32x8_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2dq_Y_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_maskz" ->
      instr (vcvttpd2dq_Y_Zm512_K ~z:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2udq_Y_Z ~sae:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned" ->
      instr vcvttpd2udq_Y_Zm512 args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2udq_Y_Z_K_merge ~sae:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned_mask" ->
      instr vcvttpd2udq_Y_Zm512_K_merge args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttpd2udq_Y_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_cvtt_float64x8_int32x8_unsigned_maskz" ->
      instr (vcvttpd2udq_Y_Zm512_K ~z:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2dq_Z_Z ~sae:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16" -> instr vcvttps2dq_Z_Zm512 args
    | "caml_avx512f_cvtt_float32x16_int32x16_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2dq_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_mask" ->
      instr vcvttps2dq_Z_Zm512_K_merge args
    | "caml_avx512f_cvtt_float32x16_int32x16_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2dq_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_maskz" ->
      instr (vcvttps2dq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2udq_Z_Z ~sae:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned" ->
      instr vcvttps2udq_Z_Zm512 args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2udq_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned_mask" ->
      instr vcvttps2udq_Z_Zm512_K_merge args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttps2udq_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_cvtt_float32x16_int32x16_unsigned_maskz" ->
      instr (vcvttps2udq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64_cvtt_int32_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttsd2si_r32_X ~sae:true) args
    | "caml_avx512f_float64_cvtt_int64_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttsd2si_r64_X ~sae:true) args
    | "caml_avx512f_float64_cvtt_int32_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttsd2usi_r32_X ~sae:true) args
    | "caml_avx512f_float64_cvtt_int64_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttsd2usi_r64_X ~sae:true) args
    | "caml_avx512f_float32_cvtt_int32_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttss2si_r32_X ~sae:true) args
    | "caml_avx512f_float32_cvtt_int64_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttss2si_r64_X ~sae:true) args
    | "caml_avx512f_float32_cvtt_int32_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttss2usi_r32_X ~sae:true) args
    | "caml_avx512f_float32_cvtt_int64_unsigned_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vcvttss2usi_r64_X ~sae:true) args
    | "caml_avx512f_cvt_int32x8_float64x8_unsigned" ->
      instr vcvtudq2pd_Z_Ym256 args
    | "caml_avx512f_cvt_int32x8_float64x8_unsigned_mask" ->
      instr vcvtudq2pd_Z_Ym256_K_merge args
    | "caml_avx512f_cvt_int32x8_float64x8_unsigned_maskz" ->
      instr (vcvtudq2pd_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtudq2ps_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vcvtudq2ps_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vcvtudq2ps_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vcvtudq2ps_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned" ->
      instr vcvtudq2ps_Z_Zm512 args
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtudq2ps_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vcvtudq2ps_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vcvtudq2ps_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vcvtudq2ps_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned_mask" ->
      instr vcvtudq2ps_Z_Zm512_K_merge args
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtudq2ps_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vcvtudq2ps_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vcvtudq2ps_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vcvtudq2ps_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_cvt_int32x16_float32x16_unsigned_maskz" ->
      instr (vcvtudq2ps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64_unsigned_cvt_float64_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtusi2sd_X_X_r64 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtusi2sd_X_X_r64 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtusi2sd_X_X_r64 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtusi2sd_X_X_r64 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32_unsigned_cvt_float64" ->
      instr vcvtusi2sd_X_X_r32m32 args
    | "caml_avx512f_int64_unsigned_cvt_float64" ->
      instr vcvtusi2sd_X_X_r64m64 args
    | "caml_avx512f_int32_unsigned_cvt_float32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtusi2ss_X_X_r32 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtusi2ss_X_X_r32 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtusi2ss_X_X_r32 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtusi2ss_X_X_r32 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int64_unsigned_cvt_float32_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vcvtusi2ss_X_X_r64 ~rnd:Rnd_near) args
      | 9 -> instr (vcvtusi2ss_X_X_r64 ~rnd:Rnd_down) args
      | 10 -> instr (vcvtusi2ss_X_X_r64 ~rnd:Rnd_up) args
      | 11 -> instr (vcvtusi2ss_X_X_r64 ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32_unsigned_cvt_float32" ->
      instr vcvtusi2ss_X_X_r32m32 args
    | "caml_avx512f_int64_unsigned_cvt_float32" ->
      instr vcvtusi2ss_X_X_r64m64 args
    | "caml_avx512f_cvt_int32x16_int8x16" -> instr vpmovdb_Xm128_Z args
    | "caml_avx512f_cvt_int32x16_int8x16_mask" -> instr vpmovdb_X_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int8x16_maskz" ->
      instr (vpmovdb_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_int16x16" -> instr vpmovdw_Ym256_Z args
    | "caml_avx512f_cvt_int32x16_int16x16_mask" ->
      instr vpmovdw_Y_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int16x16_maskz" ->
      instr (vpmovdw_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int32x8" -> instr vpmovqd_Ym256_Z args
    | "caml_avx512f_cvt_int64x8_int32x8_mask" -> instr vpmovqd_Y_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int32x8_maskz" ->
      instr (vpmovqd_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int16x8" -> instr vpmovqw_Xm128_Z args
    | "caml_avx512f_cvt_int64x8_int16x8_mask" -> instr vpmovqw_X_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int16x8_maskz" ->
      instr (vpmovqw_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating" ->
      instr vpmovsdb_Xm128_Z args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_mask" ->
      instr vpmovsdb_X_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_maskz" ->
      instr (vpmovsdb_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating" ->
      instr vpmovsdw_Ym256_Z args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_mask" ->
      instr vpmovsdw_Y_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_maskz" ->
      instr (vpmovsdw_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating" ->
      instr vpmovsqd_Ym256_Z args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_mask" ->
      instr vpmovsqd_Y_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_maskz" ->
      instr (vpmovsqd_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating" ->
      instr vpmovsqw_Xm128_Z args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_mask" ->
      instr vpmovsqw_X_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_maskz" ->
      instr (vpmovsqw_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int32x16" -> instr vpmovsxbd_Z_Xm128 args
    | "caml_avx512f_cvtsx_int8x16_int32x16_mask" ->
      instr vpmovsxbd_Z_Xm128_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int32x16_maskz" ->
      instr (vpmovsxbd_Z_Xm128_K ~z:true) args
    | "caml_avx512f_cvtsx_int8x16_int64x8" -> instr vpmovsxbq_Z_Xm64 args
    | "caml_avx512f_cvtsx_int8x16_int64x8_mask" ->
      instr vpmovsxbq_Z_Xm64_K_merge args
    | "caml_avx512f_cvtsx_int8x16_int64x8_maskz" ->
      instr (vpmovsxbq_Z_Xm64_K ~z:true) args
    | "caml_avx512f_cvtsx_int32x8_int64x8" -> instr vpmovsxdq_Z_Ym256 args
    | "caml_avx512f_cvtsx_int32x8_int64x8_mask" ->
      instr vpmovsxdq_Z_Ym256_K_merge args
    | "caml_avx512f_cvtsx_int32x8_int64x8_maskz" ->
      instr (vpmovsxdq_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x16_int32x16" -> instr vpmovsxwd_Z_Ym256 args
    | "caml_avx512f_cvtsx_int16x16_int32x16_mask" ->
      instr vpmovsxwd_Z_Ym256_K_merge args
    | "caml_avx512f_cvtsx_int16x16_int32x16_maskz" ->
      instr (vpmovsxwd_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvtsx_int16x8_int64x8" -> instr vpmovsxwq_Z_Xm128 args
    | "caml_avx512f_cvtsx_int16x8_int64x8_mask" ->
      instr vpmovsxwq_Z_Xm128_K_merge args
    | "caml_avx512f_cvtsx_int16x8_int64x8_maskz" ->
      instr (vpmovsxwq_Z_Xm128_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_unsigned" ->
      instr vpmovusdb_Xm128_Z args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_unsigned_mask" ->
      instr vpmovusdb_X_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_unsigned_maskz" ->
      instr (vpmovusdb_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_unsigned" ->
      instr vpmovusdw_Ym256_Z args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_unsigned_mask" ->
      instr vpmovusdw_Y_Z_K_merge args
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_unsigned_maskz" ->
      instr (vpmovusdw_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_unsigned" ->
      instr vpmovusqd_Ym256_Z args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_unsigned_mask" ->
      instr vpmovusqd_Y_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_unsigned_maskz" ->
      instr (vpmovusqd_Ym256_Z_K ~z:true) args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_unsigned" ->
      instr vpmovusqw_Xm128_Z args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_unsigned_mask" ->
      instr vpmovusqw_X_Z_K_merge args
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_unsigned_maskz" ->
      instr (vpmovusqw_Xm128_Z_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int32x16" -> instr vpmovzxbd_Z_Xm128 args
    | "caml_avx512f_cvtzx_int8x16_int32x16_mask" ->
      instr vpmovzxbd_Z_Xm128_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int32x16_maskz" ->
      instr (vpmovzxbd_Z_Xm128_K ~z:true) args
    | "caml_avx512f_cvtzx_int8x16_int64x8" -> instr vpmovzxbq_Z_Xm64 args
    | "caml_avx512f_cvtzx_int8x16_int64x8_mask" ->
      instr vpmovzxbq_Z_Xm64_K_merge args
    | "caml_avx512f_cvtzx_int8x16_int64x8_maskz" ->
      instr (vpmovzxbq_Z_Xm64_K ~z:true) args
    | "caml_avx512f_cvtzx_int32x8_int64x8" -> instr vpmovzxdq_Z_Ym256 args
    | "caml_avx512f_cvtzx_int32x8_int64x8_mask" ->
      instr vpmovzxdq_Z_Ym256_K_merge args
    | "caml_avx512f_cvtzx_int32x8_int64x8_maskz" ->
      instr (vpmovzxdq_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x16_int32x16" -> instr vpmovzxwd_Z_Ym256 args
    | "caml_avx512f_cvtzx_int16x16_int32x16_mask" ->
      instr vpmovzxwd_Z_Ym256_K_merge args
    | "caml_avx512f_cvtzx_int16x16_int32x16_maskz" ->
      instr (vpmovzxwd_Z_Ym256_K ~z:true) args
    | "caml_avx512f_cvtzx_int16x8_int64x8" -> instr vpmovzxwq_Z_Xm128 args
    | "caml_avx512f_cvtzx_int16x8_int64x8_mask" ->
      instr vpmovzxwq_Z_Xm128_K_merge args
    | "caml_avx512f_cvtzx_int16x8_int64x8_maskz" ->
      instr (vpmovzxwq_Z_Xm128_K ~z:true) args
    | "caml_avx512f_float64x8_max_mask" -> instr vmaxpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_max_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxpd_Z_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float64x8_max_maskz" ->
      instr (vmaxpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_max_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxpd_Z_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float64x8_max" -> instr vmaxpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_max_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxpd_Z_Z_Z ~sae:true) args
    | "caml_avx512f_float32x16_max_mask" -> instr vmaxps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_max_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxps_Z_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float32x16_max_maskz" ->
      instr (vmaxps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_max_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxps_Z_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float32x16_max" -> instr vmaxps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_max_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxps_Z_Z_Z ~sae:true) args
    | "caml_avx512f_float64_max_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxsd_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float64_max_mask" -> instr vmaxsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_max_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxsd_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float64_max_maskz" -> instr (vmaxsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_max_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxsd_X_X_X ~sae:true) args
    | "caml_avx512f_float32_max_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxss_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float32_max_mask" -> instr vmaxss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_max_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxss_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float32_max_maskz" -> instr (vmaxss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_max_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vmaxss_X_X_X ~sae:true) args
    | "caml_avx512f_float64x8_min_mask" -> instr vminpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_min_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminpd_Z_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float64x8_min_maskz" ->
      instr (vminpd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_min_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminpd_Z_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float64x8_min" -> instr vminpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_min_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminpd_Z_Z_Z ~sae:true) args
    | "caml_avx512f_float32x16_min_mask" -> instr vminps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_min_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminps_Z_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float32x16_min_maskz" ->
      instr (vminps_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_min_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminps_Z_Z_Z_K ~sae:true ~z:true) args
    | "caml_avx512f_float32x16_min" -> instr vminps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_min_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminps_Z_Z_Z ~sae:true) args
    | "caml_avx512f_float64_min_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminsd_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float64_min_mask" -> instr vminsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_min_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminsd_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float64_min_maskz" -> instr (vminsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_min_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminsd_X_X_X ~sae:true) args
    | "caml_avx512f_float32_min_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminss_X_X_X_K_merge ~sae:true) args
    | "caml_avx512f_float32_min_mask" -> instr vminss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_min_round_maskz" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminss_X_X_X_K ~sae:true ~z:true) args
    | "caml_avx512f_float32_min_maskz" -> instr (vminss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_min_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vminss_X_X_X ~sae:true) args
    | "caml_avx512f_int32x16_abs" -> instr vpabsd_Z_Zm512 args
    | "caml_avx512f_int32x16_abs_mask" -> instr vpabsd_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_abs_maskz" -> instr (vpabsd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_abs" -> instr vpabsq_Z_Zm512 args
    | "caml_avx512f_int64x8_abs_mask" -> instr vpabsq_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_abs_maskz" -> instr (vpabsq_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_max_maskz" ->
      instr (vpmaxsd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_max_mask" -> instr vpmaxsq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_max_maskz" ->
      instr (vpmaxsq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_max" -> instr vpmaxsq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_max_unsigned_maskz" ->
      instr (vpmaxud_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_max_unsigned_mask" ->
      instr vpmaxuq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_max_unsigned_maskz" ->
      instr (vpmaxuq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_max_unsigned" -> instr vpmaxuq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_min_maskz" ->
      instr (vpminsd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_min_mask" -> instr vpminsq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_min_maskz" ->
      instr (vpminsq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_min" -> instr vpminsq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_min_unsigned_maskz" ->
      instr (vpminud_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_min_unsigned_mask" ->
      instr vpminuq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_min_unsigned_maskz" ->
      instr (vpminuq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_min_unsigned" -> instr vpminuq_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_mov_maskz" ->
      instr (vmovupd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_mov_maskz" ->
      instr (vmovups_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_duplicate_even_mask" ->
      instr vmovddup_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_duplicate_even_maskz" ->
      instr (vmovddup_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_duplicate_even" -> instr vmovddup_Z_Zm512 args
    | "caml_avx512f_int32x16_mov_maskz" ->
      instr (vmovdqu32_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_mov_maskz" ->
      instr (vmovdqu64_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64_move_mask" -> instr vmovsd_X_X_X_K_merge args
    | "caml_avx512f_float64_move_maskz" -> instr (vmovsd_X_X_X_K ~z:true) args
    | "caml_avx512f_float32x16_duplicate_odd_mask" ->
      instr vmovshdup_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_duplicate_odd_maskz" ->
      instr (vmovshdup_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_duplicate_odd" -> instr vmovshdup_Z_Zm512 args
    | "caml_avx512f_float32x16_duplicate_even_mask" ->
      instr vmovsldup_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_duplicate_even_maskz" ->
      instr (vmovsldup_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_duplicate_even" -> instr vmovsldup_Z_Zm512 args
    | "caml_avx512f_float32_move_mask" -> instr vmovss_X_X_X_K_merge args
    | "caml_avx512f_float32_move_maskz" -> instr (vmovss_X_X_X_K ~z:true) args
    | "caml_avx512f_int32x16_and_maskz" ->
      instr (vpandd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_andnot_maskz" ->
      instr (vpandnd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_andnot_maskz" ->
      instr (vpandnq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_and_maskz" ->
      instr (vpandq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_or_maskz" -> instr (vpord_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_or_maskz" -> instr (vporq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_Z_Z_Zm512_K ~z:false) ~i args
    | "caml_avx512f_int32x16_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogd_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogd_Z_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_ternarylogic_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_Z_Z_Zm512_K ~z:false) ~i args
    | "caml_avx512f_int64x8_ternarylogic_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpternlogq_Z_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_ternarylogic" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpternlogq_Z_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_test_mask" -> instr vptestmq_K_Z_Zm512_K args
    | "caml_avx512f_int64x8_test" -> instr vptestmq_K_Z_Zm512 args
    | "caml_avx512f_int32x16_testn_mask" -> instr vptestnmd_K_Z_Zm512_K args
    | "caml_avx512f_int32x16_testn" -> instr vptestnmd_K_Z_Zm512 args
    | "caml_avx512f_int64x8_testn_mask" -> instr vptestnmq_K_Z_Zm512_K args
    | "caml_avx512f_int64x8_testn" -> instr vptestnmq_K_Z_Zm512 args
    | "caml_avx512f_int32x16_xor_maskz" ->
      instr (vpxord_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_xor_maskz" ->
      instr (vpxorq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int8x64_set1" -> instr vpbroadcastb_Z_r32 args
    | "caml_avx512f_int32x16_set1_mask" -> instr vpbroadcastd_Z_r32_K_merge args
    | "caml_avx512f_int32x16_set1_maskz" ->
      instr (vpbroadcastd_Z_r32_K ~z:true) args
    | "caml_avx512f_int32x16_set1" -> instr vpbroadcastd_Z_r32 args
    | "caml_avx512f_int64x8_set1_mask" -> instr vpbroadcastq_Z_r64_K_merge args
    | "caml_avx512f_int64x8_set1_maskz" ->
      instr (vpbroadcastq_Z_r64_K ~z:true) args
    | "caml_avx512f_int64x8_set1" -> instr vpbroadcastq_Z_r64 args
    | "caml_avx512f_int16x32_set1" -> instr vpbroadcastw_Z_r32 args
    | "caml_avx512f_int32x16_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprold_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprold_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_rol_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_rol_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprolq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_rol" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprolq_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_rolv_mask" -> instr vprolvd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_rolv_maskz" ->
      instr (vprolvd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int32x16_rolv" -> instr vprolvd_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_rolv_mask" -> instr vprolvq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_rolv_maskz" ->
      instr (vprolvq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_rolv" -> instr vprolvq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprord_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprord_Z_Zm512 ~i args
    | "caml_avx512f_int64x8_ror_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_ror_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vprorq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_ror" ->
      let i, args = extract_constant args ~max:255 op in
      instr vprorq_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_rorv_mask" -> instr vprorvd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_rorv" -> instr vprorvd_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_rorv_mask" -> instr vprorvq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_rorv_maskz" ->
      instr (vprorvq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_rorv" -> instr vprorvq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_sll_mask" -> instr vpslld_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int32x16_sll_maskz" ->
      instr (vpslld_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int32x16_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpslld_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_sll" -> instr vpslld_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_sll_mask" -> instr vpsllq_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int64x8_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_sll_maskz" ->
      instr (vpsllq_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int64x8_slli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsllq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_sll" -> instr vpsllq_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_slli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsllq_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_sllv_maskz" ->
      instr (vpsllvd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_sllv_mask" -> instr vpsllvq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_sllv_maskz" ->
      instr (vpsllvq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_sllv" -> instr vpsllvq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_sra_mask" -> instr vpsrad_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int32x16_sra_maskz" ->
      instr (vpsrad_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int32x16_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrad_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_sra" -> instr vpsrad_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_sra_mask" -> instr vpsraq_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int64x8_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_sra_maskz" ->
      instr (vpsraq_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int64x8_srai_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsraq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_sra" -> instr vpsraq_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_srai" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsraq_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_srav_maskz" ->
      instr (vpsravd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_srav_mask" -> instr vpsravq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_srav_maskz" ->
      instr (vpsravq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_srav" -> instr vpsravq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_srl_mask" -> instr vpsrld_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int32x16_srl_maskz" ->
      instr (vpsrld_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int32x16_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrld_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int32x16_srl" -> instr vpsrld_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_srl_mask" -> instr vpsrlq_Z_Z_Xm128_K_merge args
    | "caml_avx512f_int64x8_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlq_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int64x8_srl_maskz" ->
      instr (vpsrlq_Z_Z_Xm128_K ~z:true) args
    | "caml_avx512f_int64x8_srli_maskz" ->
      let i, args = extract_constant args ~max:255 op in
      instr (vpsrlq_Z_Zm512_K ~z:true) ~i args
    | "caml_avx512f_int64x8_srl" -> instr vpsrlq_Z_Z_Xm128 args
    | "caml_avx512f_int64x8_srli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrlq_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_srlv_maskz" ->
      instr (vpsrlvd_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_srlv_mask" -> instr vpsrlvq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_srlv_maskz" ->
      instr (vpsrlvq_Z_Z_Zm512_K ~z:true) args
    | "caml_avx512f_int64x8_srlv" -> instr vpsrlvq_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_rcp14_mask" -> instr vrcp14pd_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_rcp14_maskz" ->
      instr (vrcp14pd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_rcp14" -> instr vrcp14pd_Z_Zm512 args
    | "caml_avx512f_float32x16_rcp14_mask" ->
      instr vrcp14ps_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_rcp14_maskz" ->
      instr (vrcp14ps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_rcp14" -> instr vrcp14ps_Z_Zm512 args
    | "caml_avx512f_float64_rcp14_mask" -> instr vrcp14sd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_rcp14_maskz" ->
      instr (vrcp14sd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_rcp14" -> instr vrcp14sd_X_X_Xm64 args
    | "caml_avx512f_float32_rcp14_mask" -> instr vrcp14ss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_rcp14_maskz" ->
      instr (vrcp14ss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_rcp14" -> instr vrcp14ss_X_X_Xm32 args
    | "caml_avx512f_float64x8_rsqrt14_mask" ->
      instr vrsqrt14pd_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_rsqrt14_maskz" ->
      instr (vrsqrt14pd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_rsqrt14" -> instr vrsqrt14pd_Z_Zm512 args
    | "caml_avx512f_float32x16_rsqrt14_mask" ->
      instr vrsqrt14ps_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_rsqrt14_maskz" ->
      instr (vrsqrt14ps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_rsqrt14" -> instr vrsqrt14ps_Z_Zm512 args
    | "caml_avx512f_float64_rsqrt14_mask" ->
      instr vrsqrt14sd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_rsqrt14_maskz" ->
      instr (vrsqrt14sd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_rsqrt14" -> instr vrsqrt14sd_X_X_Xm64 args
    | "caml_avx512f_float32_rsqrt14_mask" ->
      instr vrsqrt14ss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_rsqrt14_maskz" ->
      instr (vrsqrt14ss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_rsqrt14" -> instr vrsqrt14ss_X_X_Xm32 args
    | "caml_avx512f_float64x8_sqrt_mask" -> instr vsqrtpd_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_sqrt_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtpd_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtpd_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtpd_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtpd_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_sqrt_maskz" ->
      instr (vsqrtpd_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float64x8_sqrt_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtpd_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsqrtpd_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsqrtpd_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsqrtpd_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64x8_sqrt" -> instr vsqrtpd_Z_Zm512 args
    | "caml_avx512f_float64x8_sqrt_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtpd_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtpd_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtpd_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtpd_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_sqrt_mask" -> instr vsqrtps_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_sqrt_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtps_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtps_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtps_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtps_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_sqrt_maskz" ->
      instr (vsqrtps_Z_Zm512_K ~z:true) args
    | "caml_avx512f_float32x16_sqrt_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtps_Z_Z_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsqrtps_Z_Z_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsqrtps_Z_Z_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsqrtps_Z_Z_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32x16_sqrt" -> instr vsqrtps_Z_Zm512 args
    | "caml_avx512f_float32x16_sqrt_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtps_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtps_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtps_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtps_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_sqrt_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtsd_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtsd_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtsd_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtsd_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64_sqrt_mask" -> instr vsqrtsd_X_X_Xm64_K_merge args
    | "caml_avx512f_float64_sqrt_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtsd_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsqrtsd_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsqrtsd_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsqrtsd_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float64_sqrt_maskz" ->
      instr (vsqrtsd_X_X_Xm64_K ~z:true) args
    | "caml_avx512f_float64_sqrt_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtsd_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtsd_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtsd_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtsd_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_sqrt_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtss_X_X_X_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtss_X_X_X_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtss_X_X_X_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtss_X_X_X_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32_sqrt_mask" -> instr vsqrtss_X_X_Xm32_K_merge args
    | "caml_avx512f_float32_sqrt_round_maskz" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtss_X_X_X_K ~rnd:Rnd_near ~z:true) args
      | 9 -> instr (vsqrtss_X_X_X_K ~rnd:Rnd_down ~z:true) args
      | 10 -> instr (vsqrtss_X_X_X_K ~rnd:Rnd_up ~z:true) args
      | 11 -> instr (vsqrtss_X_X_X_K ~rnd:Rnd_zero ~z:true) args
      | _ -> None)
    | "caml_avx512f_float32_sqrt_maskz" ->
      instr (vsqrtss_X_X_Xm32_K ~z:true) args
    | "caml_avx512f_float32_sqrt_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsqrtss_X_X_X ~rnd:Rnd_near) args
      | 9 -> instr (vsqrtss_X_X_X ~rnd:Rnd_down) args
      | 10 -> instr (vsqrtss_X_X_X ~rnd:Rnd_up) args
      | 11 -> instr (vsqrtss_X_X_X ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_add" -> instr vaddpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddpd_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vaddpd_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vaddpd_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vaddpd_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_add_mask" -> instr vaddpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddpd_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vaddpd_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vaddpd_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vaddpd_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_add" -> instr vaddps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddps_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vaddps_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vaddps_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vaddps_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_add_mask" -> instr vaddps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vaddps_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vaddps_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vaddps_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vaddps_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmadd132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmadd231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmadd231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmadd231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmadd231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmadd132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmadd132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmadd132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmadd132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmadd231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmadd231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmadd231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmadd231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmadd231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmadd132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmsub132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmsub231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmsub231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsub231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmsub231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfmsub132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfmsub132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfmsub132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfmsub132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfmsub231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfmsub231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfmsub231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfmsub231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfmsub231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfmsub132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfmsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfnmadd132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmadd132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmadd132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmadd132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmadd231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmadd231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmadd231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmadd231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfnmadd132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmadd132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmadd132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmadd132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmadd231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmadd231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmadd231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmadd231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmadd231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmadd132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_add_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmadd132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfnmsub132pd_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmsub132pd_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmsub132pd_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132pd_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmsub132pd_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231pd_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmsub231pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmsub231pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmsub231pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmsub231pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132pd_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_neg_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132pd_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr vfnmsub132ps_Z_Z_Zm512 (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr (vfnmsub132ps_Z_Z_Z ~rnd:Rnd_near) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr (vfnmsub132ps_Z_Z_Z ~rnd:Rnd_down) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ps_Z_Z_Z ~rnd:Rnd_up) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr (vfnmsub132ps_Z_Z_Z ~rnd:Rnd_zero) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_mask3" -> (
      match args with
      | a :: b :: c :: rest ->
        instr (vfnmsub231ps_Z_Z_Zm512_K ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_round_mask3" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | a :: b :: c :: rest, 8 ->
        instr (vfnmsub231ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 9 ->
        instr (vfnmsub231ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 10 ->
        instr (vfnmsub231ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (c :: a :: b :: rest)
      | a :: b :: c :: rest, 11 ->
        instr (vfnmsub231ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false) (c :: a :: b :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_mask" -> (
      match args with
      | dst :: x :: y :: rest ->
        instr (vfnmsub132ps_Z_Z_Zm512_K ~z:false) (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float32x16_neg_mul_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match args, i with
      | dst :: x :: y :: rest, 8 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_near ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 9 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_down ~z:false)
          (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 10 ->
        instr (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_up ~z:false) (dst :: y :: x :: rest)
      | dst :: x :: y :: rest, 11 ->
        instr
          (vfnmsub132ps_Z_Z_Z_K ~rnd:Rnd_zero ~z:false)
          (dst :: y :: x :: rest)
      | _ -> None)
    | "caml_avx512f_float64x8_mul_mask" -> instr vmulpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_mul_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulpd_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vmulpd_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vmulpd_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vmulpd_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_mul" -> instr vmulpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_mul_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulpd_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vmulpd_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vmulpd_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vmulpd_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_mul_mask" -> instr vmulps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_mul_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulps_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vmulps_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vmulps_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vmulps_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_mul" -> instr vmulps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_mul_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vmulps_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vmulps_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vmulps_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vmulps_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32x16_add" -> instr vpaddd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_add_mask" -> instr vpaddd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_mul_low_mask" ->
      instr vpmulld_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_mul_low" -> instr vpmulld_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_sub_mask" -> instr vpsubd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_sub" -> instr vpsubd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_sub_mask" -> instr vsubpd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubpd_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsubpd_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsubpd_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsubpd_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float64x8_sub" -> instr vsubpd_Z_Z_Zm512 args
    | "caml_avx512f_float64x8_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubpd_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vsubpd_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vsubpd_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vsubpd_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_sub_mask" -> instr vsubps_Z_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_sub_round_mask" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubps_Z_Z_Z_K_merge ~rnd:Rnd_near) args
      | 9 -> instr (vsubps_Z_Z_Z_K_merge ~rnd:Rnd_down) args
      | 10 -> instr (vsubps_Z_Z_Z_K_merge ~rnd:Rnd_up) args
      | 11 -> instr (vsubps_Z_Z_Z_K_merge ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_float32x16_sub" -> instr vsubps_Z_Z_Zm512 args
    | "caml_avx512f_float32x16_sub_round" -> (
      let i, args = extract_constant args ~max:11 op in
      match i with
      | 8 -> instr (vsubps_Z_Z_Z ~rnd:Rnd_near) args
      | 9 -> instr (vsubps_Z_Z_Z ~rnd:Rnd_down) args
      | 10 -> instr (vsubps_Z_Z_Z ~rnd:Rnd_up) args
      | 11 -> instr (vsubps_Z_Z_Z ~rnd:Rnd_zero) args
      | _ -> None)
    | "caml_avx512f_int32x16_align_right" ->
      let i, args = extract_constant args ~max:15 op in
      instr valignd_Z_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_align_right_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr valignd_Z_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_getexp" -> instr vgetexppd_Z_Zm512 args
    | "caml_avx512f_float64x8_getexp_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexppd_Z_Z ~sae:true) args
    | "caml_avx512f_float64x8_getexp_mask" ->
      instr vgetexppd_Z_Zm512_K_merge args
    | "caml_avx512f_float64x8_getexp_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexppd_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float32x16_getexp" -> instr vgetexpps_Z_Zm512 args
    | "caml_avx512f_float32x16_getexp_round" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpps_Z_Z ~sae:true) args
    | "caml_avx512f_float32x16_getexp_mask" ->
      instr vgetexpps_Z_Zm512_K_merge args
    | "caml_avx512f_float32x16_getexp_round_mask" ->
      let _i, args = extract_constant args ~max:255 op in
      instr (vgetexpps_Z_Z_K_merge ~sae:true) args
    | "caml_avx512f_float64x8_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_getmant_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float64x8_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantpd_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float64x8_getmant_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantpd_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512f_float32x16_getmant" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_getmant_round" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_Z_Z ~sae:true) ~i args
    | "caml_avx512f_float32x16_getmant_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr vgetmantps_Z_Zm512_K_merge ~i args
    | "caml_avx512f_float32x16_getmant_round_mask" ->
      let i, args = extract_constant args ~max:15 op in
      instr (vgetmantps_Z_Z_K_merge ~sae:true) ~i args
    | "caml_avx512f_float64x8_blend" ->
      instr (vblendmpd_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_float32x16_blend" ->
      instr (vblendmps_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_int32x16_blend" ->
      instr (vpblendmd_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_int64x8_blend" ->
      instr (vpblendmq_Z_Z_Zm512_K ~z:false) args
    | "caml_avx512f_int32x16_shuffle_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufd_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_shuffle" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpshufd_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_Z_Zm512 ~i args
    | "caml_avx512f_float64x8_cmpeq" -> instr vcmppd_K_Z_Zm512 ~i:0 args
    | "caml_avx512f_float64x8_cmple" -> instr vcmppd_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_float64x8_cmplt" -> instr vcmppd_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_float64x8_cmpneq" -> instr vcmppd_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_float64x8_cmpnle" -> instr vcmppd_K_Z_Zm512 ~i:6 args
    | "caml_avx512f_float64x8_cmpnlt" -> instr vcmppd_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_float64x8_cmpord" -> instr vcmppd_K_Z_Zm512 ~i:7 args
    | "caml_avx512f_float64x8_cmpunord" -> instr vcmppd_K_Z_Zm512 ~i:3 args
    | "caml_avx512f_float64x8_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmppd_K_Z_Zm512_K ~i args
    | "caml_avx512f_float64x8_cmpeq_mask" -> instr vcmppd_K_Z_Zm512_K ~i:0 args
    | "caml_avx512f_float64x8_cmple_mask" -> instr vcmppd_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_float64x8_cmplt_mask" -> instr vcmppd_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_float64x8_cmpneq_mask" -> instr vcmppd_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_float64x8_cmpnle_mask" -> instr vcmppd_K_Z_Zm512_K ~i:6 args
    | "caml_avx512f_float64x8_cmpnlt_mask" -> instr vcmppd_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_float64x8_cmpord_mask" -> instr vcmppd_K_Z_Zm512_K ~i:7 args
    | "caml_avx512f_float64x8_cmpunord_mask" ->
      instr vcmppd_K_Z_Zm512_K ~i:3 args
    | "caml_avx512f_float32x16_cmp" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_Z_Zm512 ~i args
    | "caml_avx512f_float32x16_cmpeq" -> instr vcmpps_K_Z_Zm512 ~i:0 args
    | "caml_avx512f_float32x16_cmple" -> instr vcmpps_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_float32x16_cmplt" -> instr vcmpps_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_float32x16_cmpneq" -> instr vcmpps_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_float32x16_cmpnle" -> instr vcmpps_K_Z_Zm512 ~i:6 args
    | "caml_avx512f_float32x16_cmpnlt" -> instr vcmpps_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_float32x16_cmpord" -> instr vcmpps_K_Z_Zm512 ~i:7 args
    | "caml_avx512f_float32x16_cmpunord" -> instr vcmpps_K_Z_Zm512 ~i:3 args
    | "caml_avx512f_float32x16_cmp_mask" ->
      let i, args = extract_constant args ~max:31 op in
      instr vcmpps_K_Z_Zm512_K ~i args
    | "caml_avx512f_float32x16_cmpeq_mask" -> instr vcmpps_K_Z_Zm512_K ~i:0 args
    | "caml_avx512f_float32x16_cmple_mask" -> instr vcmpps_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_float32x16_cmplt_mask" -> instr vcmpps_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_float32x16_cmpneq_mask" ->
      instr vcmpps_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_float32x16_cmpnle_mask" ->
      instr vcmpps_K_Z_Zm512_K ~i:6 args
    | "caml_avx512f_float32x16_cmpnlt_mask" ->
      instr vcmpps_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_float32x16_cmpord_mask" ->
      instr vcmpps_K_Z_Zm512_K ~i:7 args
    | "caml_avx512f_float32x16_cmpunord_mask" ->
      instr vcmpps_K_Z_Zm512_K ~i:3 args
    | "caml_avx512f_int32x16_cmp" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_cmpeq" -> instr vpcmpeqd_K_Z_Zm512 args
    | "caml_avx512f_int32x16_cmpge" -> instr vpcmpd_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_int32x16_cmpgt" -> instr vpcmpgtd_K_Z_Zm512 args
    | "caml_avx512f_int32x16_cmple" -> instr vpcmpd_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_int32x16_cmpneq" -> instr vpcmpd_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_int32x16_cmp_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpd_K_Z_Zm512_K ~i args
    | "caml_avx512f_int32x16_cmpeq_mask" -> instr vpcmpeqd_K_Z_Zm512_K args
    | "caml_avx512f_int32x16_cmpge_mask" -> instr vpcmpd_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_int32x16_cmpgt_mask" -> instr vpcmpgtd_K_Z_Zm512_K args
    | "caml_avx512f_int32x16_cmple_mask" -> instr vpcmpd_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_int32x16_cmpneq_mask" -> instr vpcmpd_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_int32x16_cmp_unsigned" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_cmpeq_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:0 args
    | "caml_avx512f_int32x16_cmpge_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:5 args
    | "caml_avx512f_int32x16_cmpgt_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:6 args
    | "caml_avx512f_int32x16_cmple_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:2 args
    | "caml_avx512f_int32x16_cmplt_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:1 args
    | "caml_avx512f_int32x16_cmpneq_unsigned" ->
      instr vpcmpud_K_Z_Zm512 ~i:4 args
    | "caml_avx512f_int32x16_cmp_unsigned_mask" ->
      let i, args = extract_constant args ~max:7 op in
      instr vpcmpud_K_Z_Zm512_K ~i args
    | "caml_avx512f_int32x16_cmpeq_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:0 args
    | "caml_avx512f_int32x16_cmpge_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:5 args
    | "caml_avx512f_int32x16_cmpgt_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:6 args
    | "caml_avx512f_int32x16_cmple_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:2 args
    | "caml_avx512f_int32x16_cmplt_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:1 args
    | "caml_avx512f_int32x16_cmpneq_unsigned_mask" ->
      instr vpcmpud_K_Z_Zm512_K ~i:4 args
    | "caml_avx512f_float64x8_mov_mask" -> instr vmovupd_Z_Z_K_merge args
    | "caml_avx512f_float32x16_mov_mask" -> instr vmovups_Z_Z_K_merge args
    | "caml_avx512f_int32x16_mov_mask" -> instr vmovdqu32_Z_Z_K_merge args
    | "caml_avx512f_int64x8_mov_mask" -> instr vmovdqu64_Z_Z_K_merge args
    | "caml_avx512f_int32x16_and" -> instr vpandd_Z_Z_Zm512 args
    | "caml_avx512f_vec512_and" -> instr vpandd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_andnot" -> instr vpandnd_Z_Z_Zm512 args
    | "caml_avx512f_vec512_andnot" -> instr vpandnd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_andnot_mask" ->
      instr vpandnd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_andnot" -> instr vpandnq_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_andnot_mask" -> instr vpandnq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_and" -> instr vpandq_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_and_mask" -> instr vpandq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_or_mask" -> instr vpord_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_or" -> instr vpord_Z_Z_Zm512 args
    | "caml_avx512f_vec512_or" -> instr vpord_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_or_mask" -> instr vporq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_or" -> instr vporq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_test_mask" -> instr vptestmd_K_Z_Zm512_K args
    | "caml_avx512f_int32x16_test" -> instr vptestmd_K_Z_Zm512 args
    | "caml_avx512f_int32x16_xor_mask" -> instr vpxord_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_xor" -> instr vpxord_Z_Z_Zm512 args
    | "caml_avx512f_vec512_xor" -> instr vpxord_Z_Z_Zm512 args
    | "caml_avx512f_int64x8_xor_mask" -> instr vpxorq_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int64x8_xor" -> instr vpxorq_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_and_mask" -> instr vpandd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_max_mask" -> instr vpmaxsd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_max" -> instr vpmaxsd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_max_unsigned_mask" ->
      instr vpmaxud_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_max_unsigned" -> instr vpmaxud_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_min_mask" -> instr vpminsd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_min" -> instr vpminsd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_min_unsigned_mask" ->
      instr vpminud_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_min_unsigned" -> instr vpminud_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_slli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpslld_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_slli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpslld_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_sllv_mask" -> instr vpsllvd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_sllv" -> instr vpsllvd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_srai_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrad_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_srai" ->
      let i, args = extract_constant args ~max:63 op in
      instr vpsrad_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_srav_mask" -> instr vpsravd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_srav" -> instr vpsravd_Z_Z_Zm512 args
    | "caml_avx512f_int32x16_srli_mask" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrld_Z_Zm512_K_merge ~i args
    | "caml_avx512f_int32x16_srli" ->
      let i, args = extract_constant args ~max:255 op in
      instr vpsrld_Z_Zm512 ~i args
    | "caml_avx512f_int32x16_srlv_mask" -> instr vpsrlvd_Z_Z_Zm512_K_merge args
    | "caml_avx512f_int32x16_srlv" -> instr vpsrlvd_Z_Z_Zm512 args
    (* END GENERATED AVX512 *)
    | _ -> None

(* AVX512 memory operations (load/store, masked and unmasked). These lower to
   [Isimd_mem] rather than [Isimd]; the address argument occupies the binding's
   memory (RM_rm / mem-result) operand slot. Generated by
   tools/avx512gen/genmem.py. *)
let select_operation_avx512_mem ~dbg:_ op args =
  if not (Arch.Extension.enabled AVX512F)
  then None
  else
    let load instr = simd_load ~mode:Arch.identity_addressing instr args in
    let store instr = simd_store ~mode:Arch.identity_addressing instr args in
    ignore (load, store);
    match op with
    (* BEGIN GENERATED AVX512 MEM *)
    | "caml_avx512f_int32x8_load" -> load vmovdqa32_Y_Ym256
    | "caml_avx512f_int64x4_load" -> load vmovdqa64_Y_Ym256
    | "caml_avx512bw_int16x16_loadu" -> load vmovdqu16_Y_Ym256
    | "caml_avx512f_int32x8_loadu" -> load vmovdqu32_Y_Ym256
    | "caml_avx512f_int64x4_loadu" -> load vmovdqu64_Y_Ym256
    | "caml_avx512bw_int8x32_loadu" -> load vmovdqu8_Y_Ym256
    | "caml_avx512f_int32x8_load_mask" -> load vmovdqa32_Y_Ym256_K_merge
    | "caml_avx512f_int64x4_load_mask" -> load vmovdqa64_Y_Ym256_K_merge
    | "caml_avx512f_float64x4_load_mask" -> load vmovapd_Y_Ym256_K_merge
    | "caml_avx512f_float32x8_load_mask" -> load vmovaps_Y_Ym256_K_merge
    | "caml_avx512bw_int16x16_loadu_mask" -> load vmovdqu16_Y_Ym256_K_merge
    | "caml_avx512f_int32x8_loadu_mask" -> load vmovdqu32_Y_Ym256_K_merge
    | "caml_avx512f_int64x4_loadu_mask" -> load vmovdqu64_Y_Ym256_K_merge
    | "caml_avx512bw_int8x32_loadu_mask" -> load vmovdqu8_Y_Ym256_K_merge
    | "caml_avx512f_float64x4_loadu_mask" -> load vmovupd_Y_Ym256_K_merge
    | "caml_avx512f_float32x8_loadu_mask" -> load vmovups_Y_Ym256_K_merge
    | "caml_avx512f_int32x8_store_mask" -> store vmovdqa32_m256_Y_K
    | "caml_avx512f_int64x4_store_mask" -> store vmovdqa64_m256_Y_K
    | "caml_avx512f_float64x4_store_mask" -> store vmovapd_m256_Y_K
    | "caml_avx512f_float32x8_store_mask" -> store vmovaps_m256_Y_K
    | "caml_avx512bw_int16x16_storeu_mask" -> store vmovdqu16_m256_Y_K
    | "caml_avx512f_int32x8_storeu_mask" -> store vmovdqu32_m256_Y_K
    | "caml_avx512f_int64x4_storeu_mask" -> store vmovdqu64_m256_Y_K
    | "caml_avx512bw_int8x32_storeu_mask" -> store vmovdqu8_m256_Y_K
    | "caml_avx512f_float64x4_storeu_mask" -> store vmovupd_m256_Y_K
    | "caml_avx512f_float32x8_storeu_mask" -> store vmovups_m256_Y_K
    | "caml_avx512f_int32x8_load_maskz" -> load (vmovdqa32_Y_Ym256_K ~z:true)
    | "caml_avx512f_int64x4_load_maskz" -> load (vmovdqa64_Y_Ym256_K ~z:true)
    | "caml_avx512f_float64x4_load_maskz" -> load (vmovapd_Y_Ym256_K ~z:true)
    | "caml_avx512f_float32x8_load_maskz" -> load (vmovaps_Y_Ym256_K ~z:true)
    | "caml_avx512bw_int16x16_loadu_maskz" -> load (vmovdqu16_Y_Ym256_K ~z:true)
    | "caml_avx512f_int32x8_loadu_maskz" -> load (vmovdqu32_Y_Ym256_K ~z:true)
    | "caml_avx512f_int64x4_loadu_maskz" -> load (vmovdqu64_Y_Ym256_K ~z:true)
    | "caml_avx512bw_int8x32_loadu_maskz" -> load (vmovdqu8_Y_Ym256_K ~z:true)
    | "caml_avx512f_float64x4_loadu_maskz" -> load (vmovupd_Y_Ym256_K ~z:true)
    | "caml_avx512f_float32x8_loadu_maskz" -> load (vmovups_Y_Ym256_K ~z:true)
    | "caml_avx512f_int32x8_store" -> store vmovdqa32_m256_Y
    | "caml_avx512f_int64x4_store" -> store vmovdqa64_m256_Y
    | "caml_avx512bw_int16x16_storeu" -> store vmovdqu16_m256_Y
    | "caml_avx512f_int32x8_storeu" -> store vmovdqu32_m256_Y
    | "caml_avx512f_int64x4_storeu" -> store vmovdqu64_m256_Y
    | "caml_avx512bw_int8x32_storeu" -> store vmovdqu8_m256_Y
    | "caml_avx512f_int32x16_load" -> load vmovdqa32_Z_Zm512
    | "caml_avx512f_int64x8_load" -> load vmovdqa64_Z_Zm512
    | "caml_avx512f_float64x8_load" -> load vmovapd_Z_Zm512
    | "caml_avx512f_float32x16_load" -> load vmovaps_Z_Zm512
    | "caml_avx512bw_int16x32_loadu" -> load vmovdqu16_Z_Zm512
    | "caml_avx512f_int32x16_loadu" -> load vmovdqu32_Z_Zm512
    | "caml_avx512f_int64x8_loadu" -> load vmovdqu64_Z_Zm512
    | "caml_avx512bw_int8x64_loadu" -> load vmovdqu8_Z_Zm512
    | "caml_avx512f_float64x8_loadu" -> load vmovupd_Z_Zm512
    | "caml_avx512f_float32x16_loadu" -> load vmovups_Z_Zm512
    | "caml_avx512f_int32x16_load_mask" -> load vmovdqa32_Z_Zm512_K_merge
    | "caml_avx512f_int64x8_load_mask" -> load vmovdqa64_Z_Zm512_K_merge
    | "caml_avx512f_float64x8_load_mask" -> load vmovapd_Z_Zm512_K_merge
    | "caml_avx512f_float32x16_load_mask" -> load vmovaps_Z_Zm512_K_merge
    | "caml_avx512bw_int16x32_loadu_mask" -> load vmovdqu16_Z_Zm512_K_merge
    | "caml_avx512f_int32x16_loadu_mask" -> load vmovdqu32_Z_Zm512_K_merge
    | "caml_avx512f_int64x8_loadu_mask" -> load vmovdqu64_Z_Zm512_K_merge
    | "caml_avx512bw_int8x64_loadu_mask" -> load vmovdqu8_Z_Zm512_K_merge
    | "caml_avx512f_float64x8_loadu_mask" -> load vmovupd_Z_Zm512_K_merge
    | "caml_avx512f_float32x16_loadu_mask" -> load vmovups_Z_Zm512_K_merge
    | "caml_avx512f_int32x16_store_mask" -> store vmovdqa32_m512_Z_K
    | "caml_avx512f_int64x8_store_mask" -> store vmovdqa64_m512_Z_K
    | "caml_avx512f_float64x8_store_mask" -> store vmovapd_m512_Z_K
    | "caml_avx512f_float32x16_store_mask" -> store vmovaps_m512_Z_K
    | "caml_avx512bw_int16x32_storeu_mask" -> store vmovdqu16_m512_Z_K
    | "caml_avx512f_int32x16_storeu_mask" -> store vmovdqu32_m512_Z_K
    | "caml_avx512f_int64x8_storeu_mask" -> store vmovdqu64_m512_Z_K
    | "caml_avx512bw_int8x64_storeu_mask" -> store vmovdqu8_m512_Z_K
    | "caml_avx512f_float64x8_storeu_mask" -> store vmovupd_m512_Z_K
    | "caml_avx512f_float32x16_storeu_mask" -> store vmovups_m512_Z_K
    | "caml_avx512f_int32x16_load_maskz" -> load (vmovdqa32_Z_Zm512_K ~z:true)
    | "caml_avx512f_int64x8_load_maskz" -> load (vmovdqa64_Z_Zm512_K ~z:true)
    | "caml_avx512f_float64x8_load_maskz" -> load (vmovapd_Z_Zm512_K ~z:true)
    | "caml_avx512f_float32x16_load_maskz" -> load (vmovaps_Z_Zm512_K ~z:true)
    | "caml_avx512bw_int16x32_loadu_maskz" -> load (vmovdqu16_Z_Zm512_K ~z:true)
    | "caml_avx512f_int32x16_loadu_maskz" -> load (vmovdqu32_Z_Zm512_K ~z:true)
    | "caml_avx512f_int64x8_loadu_maskz" -> load (vmovdqu64_Z_Zm512_K ~z:true)
    | "caml_avx512bw_int8x64_loadu_maskz" -> load (vmovdqu8_Z_Zm512_K ~z:true)
    | "caml_avx512f_float64x8_loadu_maskz" -> load (vmovupd_Z_Zm512_K ~z:true)
    | "caml_avx512f_float32x16_loadu_maskz" -> load (vmovups_Z_Zm512_K ~z:true)
    | "caml_avx512f_int32x16_store" -> store vmovdqa32_m512_Z
    | "caml_avx512f_int64x8_store" -> store vmovdqa64_m512_Z
    | "caml_avx512f_float64x8_store" -> store vmovapd_m512_Z
    | "caml_avx512f_float32x16_store" -> store vmovaps_m512_Z
    | "caml_avx512bw_int16x32_storeu" -> store vmovdqu16_m512_Z
    | "caml_avx512f_int32x16_storeu" -> store vmovdqu32_m512_Z
    | "caml_avx512f_int64x8_storeu" -> store vmovdqu64_m512_Z
    | "caml_avx512bw_int8x64_storeu" -> store vmovdqu8_m512_Z
    | "caml_avx512f_float64x8_storeu" -> store vmovupd_m512_Z
    | "caml_avx512f_float32x16_storeu" -> store vmovups_m512_Z
    | "caml_avx512f_int32x4_load" -> load vmovdqa32_X_Xm128
    | "caml_avx512f_int64x2_load" -> load vmovdqa64_X_Xm128
    | "caml_avx512bw_int16x8_loadu" -> load vmovdqu16_X_Xm128
    | "caml_avx512f_int32x4_loadu" -> load vmovdqu32_X_Xm128
    | "caml_avx512f_int64x2_loadu" -> load vmovdqu64_X_Xm128
    | "caml_avx512bw_int8x16_loadu" -> load vmovdqu8_X_Xm128
    | "caml_avx512f_int32x4_load_mask" -> load vmovdqa32_X_Xm128_K_merge
    | "caml_avx512f_int64x2_load_mask" -> load vmovdqa64_X_Xm128_K_merge
    | "caml_avx512f_float64x2_load_mask" -> load vmovapd_X_Xm128_K_merge
    | "caml_avx512f_float32x4_load_mask" -> load vmovaps_X_Xm128_K_merge
    | "caml_avx512bw_int16x8_loadu_mask" -> load vmovdqu16_X_Xm128_K_merge
    | "caml_avx512f_int32x4_loadu_mask" -> load vmovdqu32_X_Xm128_K_merge
    | "caml_avx512f_int64x2_loadu_mask" -> load vmovdqu64_X_Xm128_K_merge
    | "caml_avx512bw_int8x16_loadu_mask" -> load vmovdqu8_X_Xm128_K_merge
    | "caml_avx512f_float64x2_loadu_mask" -> load vmovupd_X_Xm128_K_merge
    | "caml_avx512f_float32x4_loadu_mask" -> load vmovups_X_Xm128_K_merge
    | "caml_avx512f_int32x4_store_mask" -> store vmovdqa32_m128_X_K
    | "caml_avx512f_int64x2_store_mask" -> store vmovdqa64_m128_X_K
    | "caml_avx512f_float64x2_store_mask" -> store vmovapd_m128_X_K
    | "caml_avx512f_float32x4_store_mask" -> store vmovaps_m128_X_K
    | "caml_avx512bw_int16x8_storeu_mask" -> store vmovdqu16_m128_X_K
    | "caml_avx512f_int32x4_storeu_mask" -> store vmovdqu32_m128_X_K
    | "caml_avx512f_int64x2_storeu_mask" -> store vmovdqu64_m128_X_K
    | "caml_avx512bw_int8x16_storeu_mask" -> store vmovdqu8_m128_X_K
    | "caml_avx512f_float64x2_storeu_mask" -> store vmovupd_m128_X_K
    | "caml_avx512f_float32x4_storeu_mask" -> store vmovups_m128_X_K
    | "caml_avx512f_int32x4_load_maskz" -> load (vmovdqa32_X_Xm128_K ~z:true)
    | "caml_avx512f_int64x2_load_maskz" -> load (vmovdqa64_X_Xm128_K ~z:true)
    | "caml_avx512f_float64x2_load_maskz" -> load (vmovapd_X_Xm128_K ~z:true)
    | "caml_avx512f_float32x4_load_maskz" -> load (vmovaps_X_Xm128_K ~z:true)
    | "caml_avx512bw_int16x8_loadu_maskz" -> load (vmovdqu16_X_Xm128_K ~z:true)
    | "caml_avx512f_int32x4_loadu_maskz" -> load (vmovdqu32_X_Xm128_K ~z:true)
    | "caml_avx512f_int64x2_loadu_maskz" -> load (vmovdqu64_X_Xm128_K ~z:true)
    | "caml_avx512bw_int8x16_loadu_maskz" -> load (vmovdqu8_X_Xm128_K ~z:true)
    | "caml_avx512f_float64x2_loadu_maskz" -> load (vmovupd_X_Xm128_K ~z:true)
    | "caml_avx512f_float32x4_loadu_maskz" -> load (vmovups_X_Xm128_K ~z:true)
    | "caml_avx512f_int32x4_store" -> store vmovdqa32_m128_X
    | "caml_avx512f_int64x2_store" -> store vmovdqa64_m128_X
    | "caml_avx512bw_int16x8_storeu" -> store vmovdqu16_m128_X
    | "caml_avx512f_int32x4_storeu" -> store vmovdqu32_m128_X
    | "caml_avx512f_int64x2_storeu" -> store vmovdqu64_m128_X
    | "caml_avx512bw_int8x16_storeu" -> store vmovdqu8_m128_X
    | "caml_avx512f_float32x8_broadcast_f32x4" -> load vbroadcastf32x4_Y_m128
    | "caml_avx512f_float32x16_broadcast_f32x4" -> load vbroadcastf32x4_Z_m128
    | "caml_avx512dq_float64x4_broadcast_f64x2" -> load vbroadcastf64x2_Y_m128
    | "caml_avx512f_int32x8_broadcast_i32x4" -> load vbroadcasti32x4_Y_m128
    | "caml_avx512f_int32x16_broadcast_i32x4" -> load vbroadcasti32x4_Z_m128
    | "caml_avx512dq_int64x4_broadcast_i64x2" -> load vbroadcasti64x2_Y_m128
    | "caml_avx512dq_int64x8_broadcast_i64x2" -> load vbroadcasti64x2_Z_m128
    | "caml_avx512dq_int32x16_broadcast_i32x8" -> load vbroadcasti32x8_Z_m256
    | "caml_avx512f_int64x8_broadcast_i64x4" -> load vbroadcasti64x4_Z_m256
    | "caml_avx512f_int32x4_broadcast_d" -> load vpbroadcastd_X_Xm32
    | "caml_avx512f_int32x8_broadcast_d" -> load vpbroadcastd_Y_Xm32
    | "caml_avx512f_int32x16_broadcast_d" -> load vpbroadcastd_Z_Xm32
    | "caml_avx512f_int64x2_broadcast_q" -> load vpbroadcastq_X_Xm64
    | "caml_avx512f_int64x4_broadcast_q" -> load vpbroadcastq_Y_Xm64
    | "caml_avx512f_int64x8_broadcast_q" -> load vpbroadcastq_Z_Xm64
    | "caml_avx512f_int32x8_expandloadu_mask" -> load vpexpandd_Y_Ym256_K_merge
    | "caml_avx512f_int64x4_expandloadu_mask" -> load vpexpandq_Y_Ym256_K_merge
    | "caml_avx512f_float64x4_expandloadu_mask" ->
      load vexpandpd_Y_Ym256_K_merge
    | "caml_avx512f_float32x8_expandloadu_mask" ->
      load vexpandps_Y_Ym256_K_merge
    | "caml_avx512f_int32x8_expandloadu_maskz" ->
      load (vpexpandd_Y_Ym256_K ~z:true)
    | "caml_avx512f_int64x4_expandloadu_maskz" ->
      load (vpexpandq_Y_Ym256_K ~z:true)
    | "caml_avx512f_float64x4_expandloadu_maskz" ->
      load (vexpandpd_Y_Ym256_K ~z:true)
    | "caml_avx512f_float32x8_expandloadu_maskz" ->
      load (vexpandps_Y_Ym256_K ~z:true)
    | "caml_avx512f_int32x16_expandloadu_mask" -> load vpexpandd_Z_Zm512_K_merge
    | "caml_avx512f_int64x8_expandloadu_mask" -> load vpexpandq_Z_Zm512_K_merge
    | "caml_avx512f_float64x8_expandloadu_mask" ->
      load vexpandpd_Z_Zm512_K_merge
    | "caml_avx512f_float32x16_expandloadu_mask" ->
      load vexpandps_Z_Zm512_K_merge
    | "caml_avx512f_int32x16_expandloadu_maskz" ->
      load (vpexpandd_Z_Zm512_K ~z:true)
    | "caml_avx512f_int64x8_expandloadu_maskz" ->
      load (vpexpandq_Z_Zm512_K ~z:true)
    | "caml_avx512f_float64x8_expandloadu_maskz" ->
      load (vexpandpd_Z_Zm512_K ~z:true)
    | "caml_avx512f_float32x16_expandloadu_maskz" ->
      load (vexpandps_Z_Zm512_K ~z:true)
    | "caml_avx512f_int32x4_expandloadu_mask" -> load vpexpandd_X_Xm128_K_merge
    | "caml_avx512f_int64x2_expandloadu_mask" -> load vpexpandq_X_Xm128_K_merge
    | "caml_avx512f_float64x2_expandloadu_mask" ->
      load vexpandpd_X_Xm128_K_merge
    | "caml_avx512f_float32x4_expandloadu_mask" ->
      load vexpandps_X_Xm128_K_merge
    | "caml_avx512f_int32x4_expandloadu_maskz" ->
      load (vpexpandd_X_Xm128_K ~z:true)
    | "caml_avx512f_int64x2_expandloadu_maskz" ->
      load (vpexpandq_X_Xm128_K ~z:true)
    | "caml_avx512f_float64x2_expandloadu_maskz" ->
      load (vexpandpd_X_Xm128_K ~z:true)
    | "caml_avx512f_float32x4_expandloadu_maskz" ->
      load (vexpandps_X_Xm128_K ~z:true)
    | "caml_avx512f_int32x8_compressstoreu_mask" -> store vpcompressd_m256_Y_K
    | "caml_avx512f_int64x4_compressstoreu_mask" -> store vpcompressq_m256_Y_K
    | "caml_avx512f_float64x4_compressstoreu_mask" -> store vcompresspd_m256_Y_K
    | "caml_avx512f_float32x8_compressstoreu_mask" -> store vcompressps_m256_Y_K
    | "caml_avx512f_int32x16_compressstoreu_mask" -> store vpcompressd_m512_Z_K
    | "caml_avx512f_int64x8_compressstoreu_mask" -> store vpcompressq_m512_Z_K
    | "caml_avx512f_float64x8_compressstoreu_mask" -> store vcompresspd_m512_Z_K
    | "caml_avx512f_float32x16_compressstoreu_mask" ->
      store vcompressps_m512_Z_K
    | "caml_avx512f_int32x4_compressstoreu_mask" -> store vpcompressd_m128_X_K
    | "caml_avx512f_int64x2_compressstoreu_mask" -> store vpcompressq_m128_X_K
    | "caml_avx512f_float64x2_compressstoreu_mask" -> store vcompresspd_m128_X_K
    | "caml_avx512f_float32x4_compressstoreu_mask" -> store vcompressps_m128_X_K
    | "caml_avx512bw_cvt_int16x16_int8x16_storeu_mask" -> store vpmovwb_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int16x8_storeu_mask" -> store vpmovdw_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int8x8_storeu_mask" -> store vpmovdb_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int16x4_storeu_mask" -> store vpmovqw_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int32x4_storeu_mask" -> store vpmovqd_m128_Y_K
    | "caml_avx512f_cvt_int64x4_int8x4_storeu_mask" -> store vpmovqb_m32_Y_K
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_storeu_mask" ->
      store vpmovswb_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_storeu_mask" ->
      store vpmovsdw_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int8x8_saturating_storeu_mask" ->
      store vpmovsdb_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int16x4_saturating_storeu_mask" ->
      store vpmovsqw_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_storeu_mask" ->
      store vpmovsqd_m128_Y_K
    | "caml_avx512f_cvt_int64x4_int8x4_saturating_storeu_mask" ->
      store vpmovsqb_m32_Y_K
    | "caml_avx512bw_cvt_int16x16_int8x16_saturating_unsigned_storeu_mask" ->
      store vpmovuswb_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int16x8_saturating_unsigned_storeu_mask" ->
      store vpmovusdw_m128_Y_K
    | "caml_avx512f_cvt_int32x8_int8x8_saturating_unsigned_storeu_mask" ->
      store vpmovusdb_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int16x4_saturating_unsigned_storeu_mask" ->
      store vpmovusqw_m64_Y_K
    | "caml_avx512f_cvt_int64x4_int32x4_saturating_unsigned_storeu_mask" ->
      store vpmovusqd_m128_Y_K
    | "caml_avx512f_cvt_int64x4_int8x4_saturating_unsigned_storeu_mask" ->
      store vpmovusqb_m32_Y_K
    | "caml_avx512bw_cvt_int16x32_int8x32_storeu_mask" -> store vpmovwb_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int16x16_storeu_mask" -> store vpmovdw_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int8x16_storeu_mask" -> store vpmovdb_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int16x8_storeu_mask" -> store vpmovqw_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int32x8_storeu_mask" -> store vpmovqd_m256_Z_K
    | "caml_avx512f_cvt_int64x8_int8x8_storeu_mask" -> store vpmovqb_m64_Z_K
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_storeu_mask" ->
      store vpmovswb_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_storeu_mask" ->
      store vpmovsdw_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_storeu_mask" ->
      store vpmovsdb_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_storeu_mask" ->
      store vpmovsqw_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_storeu_mask" ->
      store vpmovsqd_m256_Z_K
    | "caml_avx512f_cvt_int64x8_int8x8_saturating_storeu_mask" ->
      store vpmovsqb_m64_Z_K
    | "caml_avx512bw_cvt_int16x32_int8x32_saturating_unsigned_storeu_mask" ->
      store vpmovuswb_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int16x16_saturating_unsigned_storeu_mask" ->
      store vpmovusdw_m256_Z_K
    | "caml_avx512f_cvt_int32x16_int8x16_saturating_unsigned_storeu_mask" ->
      store vpmovusdb_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int16x8_saturating_unsigned_storeu_mask" ->
      store vpmovusqw_m128_Z_K
    | "caml_avx512f_cvt_int64x8_int32x8_saturating_unsigned_storeu_mask" ->
      store vpmovusqd_m256_Z_K
    | "caml_avx512f_cvt_int64x8_int8x8_saturating_unsigned_storeu_mask" ->
      store vpmovusqb_m64_Z_K
    | "caml_avx512bw_cvt_int16x8_int8x8_storeu_mask" -> store vpmovwb_m64_X_K
    | "caml_avx512f_cvt_int32x4_int16x4_storeu_mask" -> store vpmovdw_m64_X_K
    | "caml_avx512f_cvt_int32x4_int8x4_storeu_mask" -> store vpmovdb_m32_X_K
    | "caml_avx512f_cvt_int64x2_int16x2_storeu_mask" -> store vpmovqw_m32_X_K
    | "caml_avx512f_cvt_int64x2_int32x2_storeu_mask" -> store vpmovqd_m128_X_K
    | "caml_avx512f_cvt_int64x2_int8x2_storeu_mask" -> store vpmovqb_m16_X_K
    | "caml_avx512bw_cvt_int16x8_int8x8_saturating_storeu_mask" ->
      store vpmovswb_m64_X_K
    | "caml_avx512f_cvt_int32x4_int16x4_saturating_storeu_mask" ->
      store vpmovsdw_m64_X_K
    | "caml_avx512f_cvt_int32x4_int8x4_saturating_storeu_mask" ->
      store vpmovsdb_m32_X_K
    | "caml_avx512f_cvt_int64x2_int16x2_saturating_storeu_mask" ->
      store vpmovsqw_m32_X_K
    | "caml_avx512f_cvt_int64x2_int32x2_saturating_storeu_mask" ->
      store vpmovsqd_m64_X_K
    | "caml_avx512f_cvt_int64x2_int8x2_saturating_storeu_mask" ->
      store vpmovsqb_m16_X_K
    | "caml_avx512bw_cvt_int16x8_int8x8_saturating_unsigned_storeu_mask" ->
      store vpmovuswb_m64_X_K
    | "caml_avx512f_cvt_int32x4_int16x4_saturating_unsigned_storeu_mask" ->
      store vpmovusdw_m64_X_K
    | "caml_avx512f_cvt_int32x4_int8x4_saturating_unsigned_storeu_mask" ->
      store vpmovusdb_m32_X_K
    | "caml_avx512f_cvt_int64x2_int16x2_saturating_unsigned_storeu_mask" ->
      store vpmovusqw_m32_X_K
    | "caml_avx512f_cvt_int64x2_int32x2_saturating_unsigned_storeu_mask" ->
      store vpmovusqd_m64_X_K
    | "caml_avx512f_cvt_int64x2_int8x2_saturating_unsigned_storeu_mask" ->
      store vpmovusqb_m16_X_K
    | "caml_avx512f_int32x8_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdd_M32Y_Y_K args
    | "caml_avx512f_int64x4_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdq_M32X_Y_K args
    | "caml_avx512f_float64x4_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdpd_M32X_Y_K args
    | "caml_avx512f_float32x8_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdps_M32Y_Y_K args
    | "caml_avx512f_int32x4_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterqd_M64Y_X_K args
    | "caml_avx512f_int64x4_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterqq_M64Y_Y_K args
    | "caml_avx512f_float64x4_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterqpd_M64Y_Y_K args
    | "caml_avx512f_float32x4_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterqps_M64Y_X_K args
    | "caml_avx512f_int32x16_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdd_M32Z_Z_K args
    | "caml_avx512f_int64x8_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdq_M32Y_Z_K args
    | "caml_avx512f_float64x8_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdpd_M32Y_Z_K args
    | "caml_avx512f_float32x16_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdps_M32Z_Z_K args
    | "caml_avx512f_int32x8_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterqd_M64Z_Y_K args
    | "caml_avx512f_int64x8_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterqq_M64Z_Z_K args
    | "caml_avx512f_float64x8_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterqpd_M64Z_Z_K args
    | "caml_avx512f_float32x8_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterqps_M64Z_Y_K args
    | "caml_avx512f_int32x4_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdd_M32X_X_K args
    | "caml_avx512f_int64x2_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterdq_M32X_X_K args
    | "caml_avx512f_float64x2_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdpd_M32X_X_K args
    | "caml_avx512f_float32x4_i32scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterdps_M32X_X_K args
    | "caml_avx512f_int64x2_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vpscatterqq_M64X_X_K args
    | "caml_avx512f_float64x2_i64scatter_mask" ->
      let i, args = extract_scale args op in
      simd_store ~mode:(Iindexed2scaled (i, 0)) vscatterqpd_M64X_X_K args
    | "caml_avx512f_load_mask16" -> load kmovw_K_Km16
    | "caml_avx512bw_load_mask32" -> load kmovd_K_Km32
    | "caml_avx512bw_load_mask64" -> load kmovq_K_Km64
    | "caml_avx512dq_load_mask8" -> load kmovb_K_Km8
    | "caml_avx512f_store_mask16" -> store kmovw_m16_K
    | "caml_avx512bw_store_mask32" -> store kmovd_m32_K
    | "caml_avx512bw_store_mask64" -> store kmovq_m64_K
    | "caml_avx512dq_store_mask8" -> store kmovb_m8_K
    (* END GENERATED AVX512 MEM *)
    | _ -> None

let select_operation_cfg ~dbg op args =
  let or_else try_ opt =
    match opt with Some x -> Some x | None -> try_ ~dbg op args
  in
  None
  |> or_else select_operation_clmul
  |> or_else select_operation_popcnt
  |> or_else select_operation_lzcnt
  |> or_else select_operation_bmi
  |> or_else select_operation_bmi2
  |> or_else select_operation_sse
  |> or_else select_operation_sse2
  |> or_else select_operation_sse3
  |> or_else select_operation_ssse3
  |> or_else select_operation_sse41
  |> or_else select_operation_sse42
  |> or_else select_operation_avx
  |> or_else select_operation_avx2
  |> or_else select_operation_f16c
  |> or_else select_operation_fma
  |> or_else select_operation_avx512
  |> or_else select_operation_avx512_mem

let rax = Proc.phys_reg Int (P RAX)

let rdi = Proc.phys_reg Int (P RDI)

let rcx = Proc.phys_reg Int (P RCX)

let rdx = Proc.phys_reg Int (P RDX)

let xmm0v = Proc.phys_reg Vec128 (P MM0)

let to_phys_reg (pinned_reg : Simd.reg) =
  match pinned_reg with
  | RAX -> rax
  | RDI -> rdi
  | RCX -> rcx
  | RDX -> rdx
  | XMM0 -> xmm0v

let maybe_pin arr i loc =
  match Simd.loc_is_pinned loc with
  | None -> ()
  | Some pinned_loc -> arr.(i) <- to_phys_reg pinned_loc

let pseudoregs_for_instr (simd : Simd.instr) arg_regs res_regs =
  Array.iteri
    (fun i ({ loc; _ } : Simd.arg) -> maybe_pin arg_regs i loc)
    simd.args;
  match simd.res with
  | Res_none -> arg_regs, res_regs
  | Arg rr ->
    let res_regs = ref res_regs in
    Array.fold_left
      (fun r a ->
        let len = Simd.loc_reg_count simd.args.(a).loc in
        let a = Simd.unarized_reg_index simd.args a in
        (* CR-someday mslater: we should require binding all overwritten args *)
        (if r = Array.length !res_regs
         then
           let fresh = Reg.createv_with_typs (Array.sub arg_regs a len) in
           res_regs := Array.append !res_regs fresh);
        for idx = 0 to len - 1 do
          assert (not (Reg.is_preassigned arg_regs.(a + idx)));
          arg_regs.(a + idx) <- !res_regs.(r + idx)
        done;
        r + len)
      0 rr
    |> ignore;
    arg_regs, !res_regs
  | Res rr ->
    Array.iteri (fun i ({ loc; _ } : Simd.arg) -> maybe_pin res_regs i loc) rr;
    arg_regs, res_regs

let pseudoregs_for_operation (simd : Simd.operation) arg res =
  let arg_regs = Array.copy arg in
  let res_regs = Array.copy res in
  let sse_or_avx =
    match simd.instr with
    | Instruction instr -> instr
    | Sequence
        { id =
            ( Sqrtss | Sqrtsd | Roundss | Roundsd | Pcompare_string _
            | Vpcompare_string _ | Ptestz | Ptestc | Ptestnzc | Vptestz_X
            | Vptestc_X | Vptestnzc_X | Vptestz_Y | Vptestc_Y | Vptestnzc_Y );
          instr
        } ->
      instr
  in
  pseudoregs_for_instr sse_or_avx arg_regs res_regs

let pseudoregs_for_mem_operation (op : Simd.Mem.operation) arg res =
  match op with Load op | Store op -> pseudoregs_for_operation op arg res

(* Error report *)

let report_error ppf = function
  | Bad_immediate msg -> Format_doc.pp_print_string ppf msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

(* Vectorize operations *)

let vector_width_in_bits = 128

(* CR-soon gyorsh: [vectorize_operation] is too long, refactor / split up. *)
let vectorize_operation (width_type : Vectorize_utils.Width_in_bits.t)
    ~arg_count ~res_count ~alignment_in_bytes (cfg_ops : Operation.t list) :
    Vectorize_utils.Vectorized_instruction.t list option =
  (* Assumes cfg_ops are isomorphic *)
  let sse_or_avx sse avx =
    let instr = if Arch.Extension.enabled AVX then avx else sse in
    Operation.Specific (Isimd (Simd.instruction instr None))
  in
  let width_in_bits = Vectorize_utils.Width_in_bits.to_int width_type in
  let length = List.length cfg_ops in
  assert (length * width_in_bits = vector_width_in_bits);
  let vector_width_in_bytes = vector_width_in_bits / 8 in
  let is_aligned_to_vector_width () =
    match alignment_in_bytes with
    | None -> Misc.fatal_error "Unexpected memory operation"
    | Some alignment_in_bytes ->
      alignment_in_bytes mod vector_width_in_bytes = 0
      && alignment_in_bytes / vector_width_in_bytes > 1
  in
  let vec128_chunk () : Cmm.memory_chunk =
    if is_aligned_to_vector_width ()
    then Onetwentyeight_aligned
    else Onetwentyeight_unaligned
  in
  let same_width memory_chunk =
    Vectorize_utils.Width_in_bits.equal width_type
      (Vectorize_utils.Width_in_bits.of_memory_chunk memory_chunk)
  in
  let make_default ~arg_count ~res_count operation :
      Vectorize_utils.Vectorized_instruction.t list option =
    Some
      [ Vectorize_utils.Vectorized_instruction.make_default ~arg_count
          ~res_count operation ]
  in
  let create_const_vec consts =
    let lows, highs = Misc.Stdlib.List.split_at (length / 2) consts in
    let pack_int64 nums =
      let mask =
        Int64.shift_right_logical Int64.minus_one (64 - width_in_bits)
      in
      List.fold_left
        (fun target num ->
          Int64.logor
            (Int64.shift_left target width_in_bits)
            (Int64.logand num mask))
        0L nums
    in
    Operation.Const_vec128 { word0 = pack_int64 highs; word1 = pack_int64 lows }
    |> make_default ~arg_count:0 ~res_count:1
  in
  let add_op =
    let sse, avx =
      match width_type with
      | W512 -> assert false
      | W256 -> assert false
      | W128 -> assert false
      | W64 -> paddq, vpaddq_X_X_Xm128
      | W32 -> paddd, vpaddd_X_X_Xm128
      | W16 -> paddw, vpaddw_X_X_Xm128
      | W8 -> paddb, vpaddb_X_X_Xm128
    in
    Some (sse_or_avx sse avx)
  in
  let mul_op =
    match width_type with
    | W512 -> None
    | W256 -> None
    | W128 -> None
    | W64 -> None
    | W32 -> Some (sse_or_avx pmulld vpmulld_X_X_Xm128)
    | W16 -> Some (sse_or_avx pmullw vpmullw_X_X_Xm128)
    | W8 -> None
  in
  let vectorize_intop (intop : Operation.integer_operation) =
    match intop with
    | Iadd -> Option.bind add_op (make_default ~arg_count ~res_count)
    | Isub ->
      let sse, avx =
        match width_type with
        | W512 -> assert false
        | W256 -> assert false
        | W128 -> assert false
        | W64 -> psubq_X_Xm128, vpsubq_X_X_Xm128
        | W32 -> psubd, vpsubd_X_X_Xm128
        | W16 -> psubw, vpsubw_X_X_Xm128
        | W8 -> psubb, vpsubb_X_X_Xm128
      in
      sse_or_avx sse avx |> make_default ~arg_count ~res_count
    | Imul -> Option.bind mul_op (make_default ~arg_count ~res_count)
    | Imulh { signed } -> (
      match width_type with
      | W512 -> None
      | W256 -> None
      | W128 -> None
      | W64 -> None
      | W32 -> None
      | W16 ->
        if signed
        then
          sse_or_avx pmulhw vpmulhw_X_X_Xm128
          |> make_default ~arg_count ~res_count
        else
          sse_or_avx pmulhuw_X_Xm128 vpmulhuw_X_X_Xm128
          |> make_default ~arg_count ~res_count
      | W8 -> None)
    | Iand ->
      sse_or_avx andps vandps_X_X_Xm128 |> make_default ~arg_count ~res_count
    | Ior ->
      sse_or_avx orps vorps_X_X_Xm128 |> make_default ~arg_count ~res_count
    | Ixor ->
      sse_or_avx xorps vxorps_X_X_Xm128 |> make_default ~arg_count ~res_count
    | Ilsl ->
      let sse, avx =
        match width_type with
        | W512 -> assert false
        | W256 -> assert false
        | W128 -> assert false
        | W64 -> psllq_X_Xm128, vpsllq_X_X_Xm128
        | W32 -> pslld_X_Xm128, vpslld_X_X_Xm128
        | W16 -> psllw_X_Xm128, vpsllw_X_X_Xm128
        | W8 -> assert false
      in
      sse_or_avx sse avx |> make_default ~arg_count ~res_count
    | Ilsr ->
      let sse, avx =
        match width_type with
        | W512 -> assert false
        | W256 -> assert false
        | W128 -> assert false
        | W64 -> psrlq_X_Xm128, vpsrlq_X_X_Xm128
        | W32 -> psrld_X_Xm128, vpsrld_X_X_Xm128
        | W16 -> psrlw_X_Xm128, vpsrlw_X_X_Xm128
        | W8 -> assert false
      in
      sse_or_avx sse avx |> make_default ~arg_count ~res_count
    | Iasr ->
      let ops =
        match width_type with
        | W512 -> assert false
        | W256 -> assert false
        | W128 -> assert false
        | W64 -> None
        | W32 -> Some (psrad_X_Xm128, vpsrad_X_X_Xm128)
        | W16 -> Some (psraw_X_Xm128, vpsraw_X_X_Xm128)
        | W8 -> None
      in
      Option.bind ops (fun (sse, avx) ->
          sse_or_avx sse avx |> make_default ~arg_count ~res_count)
    | Icomp intcomp -> (
      match intcomp with
      | Ceq ->
        let sse, avx =
          match width_type with
          | W512 -> assert false
          | W256 -> assert false
          | W128 -> assert false
          | W64 -> pcmpeqq, vpcmpeqq_X_X_Xm128
          | W32 -> pcmpeqd, vpcmpeqd_X_X_Xm128
          | W16 -> pcmpeqw, vpcmpeqw_X_X_Xm128
          | W8 -> pcmpeqb, vpcmpeqb_X_X_Xm128
        in
        sse_or_avx sse avx |> make_default ~arg_count ~res_count
      | Cgt ->
        let sse, avx =
          match width_type with
          | W512 -> assert false
          | W256 -> assert false
          | W128 -> assert false
          | W64 -> pcmpgtq, vpcmpgtq_X_X_Xm128
          | W32 -> pcmpgtd, vpcmpgtd_X_X_Xm128
          | W16 -> pcmpgtw, vpcmpgtw_X_X_Xm128
          | W8 -> pcmpgtb, vpcmpgtb_X_X_Xm128
        in
        sse_or_avx sse avx |> make_default ~arg_count ~res_count
      | Cne | Clt | Cle | Cge | Cult | Cugt | Cule | Cuge ->
        None
        (* These instructions seem to not have a simd counterpart yet, could
           also implement as a combination of other instructions if needed in
           the future *))
    | Idiv | Imod | Iclz | Ictz | Ipopcnt -> None
  in
  match List.hd cfg_ops with
  | Move -> Operation.Move |> make_default ~arg_count ~res_count
  | Const_int _ ->
    let extract_const_int (op : Operation.t) =
      match op with
      | Const_int n -> Int64.of_nativeint n
      | Move | Load _ | Store _ | Intop _ | Intop_imm _ | Specific _ | Alloc _
      | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_float32 _
      | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
      | Const_vec512 _ | Const_mask _ | Stackoffset _ | Int128op _
      | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
      | Begin_region | End_region | Pause | Name_for_debugger _ | Dls_get
      | Tls_get | Domain_index | Poll ->
        assert false
    in
    assert (arg_count = 0 && res_count = 1);
    let consts = List.map extract_const_int cfg_ops in
    create_const_vec consts
  | Load { memory_chunk; addressing_mode; mutability; is_atomic } ->
    if not (same_width memory_chunk)
    then None
    else
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = num_args_addressing && res_count = 1);
      let operation =
        Operation.Load
          { memory_chunk = vec128_chunk ();
            addressing_mode;
            mutability;
            is_atomic
          }
      in
      Some
        [ { operation;
            arguments =
              Array.init num_args_addressing (fun i ->
                  Vectorize_utils.Vectorized_instruction.Original i);
            results = [| Result 0 |]
          } ]
  | Store (memory_chunk, addressing_mode, is_assignment) ->
    if not (same_width memory_chunk)
    then None
    else
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = num_args_addressing + 1 && res_count = 0);
      let operation =
        Operation.Store (vec128_chunk (), addressing_mode, is_assignment)
      in
      Some
        [ { operation;
            arguments =
              Array.append
                [| Vectorize_utils.Vectorized_instruction.Argument 0 |]
                (Array.init num_args_addressing (fun i ->
                     Vectorize_utils.Vectorized_instruction.Original (i + 1)));
            results = [||]
          } ]
  | Intop intop -> vectorize_intop intop
  | Intop_imm (intop, _) -> (
    let extract_intop_imm_int (op : Operation.t) =
      match op with
      | Intop_imm (_, n) -> Int64.of_int n
      | Move | Load _ | Store _ | Intop _ | Specific _ | Alloc _
      | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
      | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
      | Const_vec256 _ | Const_vec512 _ | Const_mask _ | Stackoffset _
      | Int128op _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
      | Opaque | Begin_region | End_region | Name_for_debugger _ | Dls_get
      | Tls_get | Domain_index | Poll | Pause ->
        assert false
    in
    let consts = List.map extract_intop_imm_int cfg_ops in
    match create_const_vec consts, vectorize_intop intop with
    | Some [const_instruction], Some [intop_instruction] ->
      if
        Array.length const_instruction.results = 1
        && Array.length intop_instruction.arguments = 2
      then (
        assert (arg_count = 1 && res_count = 1);
        const_instruction.results.(0)
          <- Vectorize_utils.Vectorized_instruction.New_Vec128 0;
        intop_instruction.arguments.(1)
          <- Vectorize_utils.Vectorized_instruction.New_Vec128 0;
        Some [const_instruction; intop_instruction])
      else None
    | _ -> None)
  | Specific op -> (
    match op with
    | Ilea addressing_mode -> (
      let extract_scale_displ (op : Operation.t) =
        match op with
        | Specific spec_op -> (
          match spec_op with
          | Ilea addressing_mode -> (
            match addressing_mode with
            | Iindexed displ -> None, Some displ
            | Iindexed2 displ -> None, Some displ
            | Iscaled (scale, displ) -> Some scale, Some displ
            | Iindexed2scaled (scale, displ) -> Some scale, Some displ
            | Ibased _ -> None, None)
          | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Ibswap _
          | Isextend32 | Izextend32 | Ikmovq | Irdtsc | Irdpmc | Ilfence
          | Isfence | Imfence | Ipackf32 | Isimd _ | Isimd_mem _ | Iprefetch _
          | Icldemote _ | Illvm_intrinsic _ ->
            assert false)
        | Move | Load _ | Store _ | Intop _ | Intop_imm _ | Alloc _
        | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
        | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
        | Const_vec256 _ | Const_vec512 _ | Const_mask _ | Stackoffset _
        | Int128op _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
        | Opaque | Begin_region | End_region | Name_for_debugger _ | Dls_get
        | Tls_get | Domain_index | Poll | Pause ->
          assert false
      in
      let get_scale op =
        match extract_scale_displ op with
        | Some scale, _ -> scale |> Int64.of_int
        | _ -> assert false
      in
      let get_displ op =
        match extract_scale_displ op with
        | _, Some displ -> displ |> Int64.of_int
        | _ -> assert false
      in
      let make_move arg res =
        { Vectorize_utils.Vectorized_instruction.operation = Move;
          arguments = [| arg |];
          results = [| res |]
        }
      in
      let make_binary_operation arg_0 arg_1 res operation =
        { Vectorize_utils.Vectorized_instruction.operation;
          arguments = [| arg_0; arg_1 |];
          results = [| res |]
        }
      in
      let make_const res consts =
        match create_const_vec consts with
        | Some [const_instruction] ->
          assert (
            Array.length const_instruction.arguments = 0
            && Array.length const_instruction.results = 1);
          const_instruction.results.(0) <- res;
          const_instruction
        | _ -> assert false
      in
      match addressing_mode with
      | Iindexed _ -> (
        match add_op with
        | Some add ->
          assert (arg_count = 1 && res_count = 1);
          let displs = List.map get_displ cfg_ops in
          (* reg + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_const (New_Vec128 0) displs;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) add ]
        | None -> None)
      | Iindexed2 _ -> (
        match add_op with
        | Some add ->
          assert (arg_count = 2 && res_count = 1);
          let displs = List.map get_displ cfg_ops in
          (* reg + reg + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_binary_operation (Result 0) (Argument 1) (Result 0) add;
              make_const (New_Vec128 0) displs;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) add ]
        | None -> None)
      | Iscaled _ -> (
        match add_op, mul_op with
        | Some add, Some mul ->
          assert (arg_count = 1 && res_count = 1);
          let scales = List.map get_scale cfg_ops in
          let displs = List.map get_displ cfg_ops in
          (* reg * scale + displ *)
          Some
            [ make_move (Argument 0) (Result 0);
              make_const (New_Vec128 0) scales;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) mul;
              make_const (New_Vec128 1) displs;
              make_binary_operation (Result 0) (New_Vec128 1) (Result 0) add ]
        | _ -> None)
      | Iindexed2scaled _ -> (
        match add_op, mul_op with
        | Some add, Some mul ->
          assert (arg_count = 2 && res_count = 1);
          let scales = List.map get_scale cfg_ops in
          let displs = List.map get_displ cfg_ops in
          (* reg + reg * scale + displ *)
          Some
            [ make_move (Argument 1) (Result 0);
              make_const (New_Vec128 0) scales;
              make_binary_operation (Result 0) (New_Vec128 0) (Result 0) mul;
              make_binary_operation (Result 0) (Argument 0) (Result 0) add;
              make_const (New_Vec128 1) displs;
              make_binary_operation (Result 0) (New_Vec128 1) (Result 0) add ]
        | _ -> None)
      | Ibased _ -> None)
    | Isextend32 -> (
      match width_type with
      | W512 -> None
      | W256 -> None
      | W128 -> None
      | W64 ->
        sse_or_avx pmovsxdq vpmovsxdq_X_Xm64
        |> make_default ~arg_count ~res_count
      | W32 ->
        None
        (* If the upper bits of the original register containing the smaller
           register is determined to be unused without relying on this file,
           these can also be vectorized to be a move *)
      | W16 -> None
      | W8 -> None)
    | Izextend32 -> (
      match width_type with
      | W512 -> None
      | W256 -> None
      | W128 -> None
      | W64 ->
        sse_or_avx pmovzxdq vpmovzxdq_X_Xm64
        |> make_default ~arg_count ~res_count
      | W32 -> None (* See previous comment *)
      | W16 -> None
      | W8 -> None)
    | Ikmovq -> None
    | Istore_int (_n, addressing_mode, is_assignment) -> (
      if not (Vectorize_utils.Width_in_bits.equal width_type W64)
      then None
      else
        let extract_store_int_imm (op : Operation.t) =
          match op with
          | Specific (Istore_int (n, _addr, _is_assign)) -> Int64.of_nativeint n
          | Specific
              ( Ifloatarithmem _ | Ioffset_loc _ | Iprefetch _ | Icldemote _
              | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence | Ipackf32
              | Isimd _ | Isimd_mem _ | Ilea _ | Ibswap _ | Isextend32
              | Izextend32 | Ikmovq | Illvm_intrinsic _ )
          | Intop_imm _ | Move | Load _ | Store _ | Intop _ | Int128op _
          | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
          | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
          | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Const_mask _
          | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _
          | Probe_is_enabled _ | Opaque | Begin_region | End_region
          | Name_for_debugger _ | Dls_get | Tls_get | Domain_index | Poll
          | Pause ->
            assert false
        in
        let consts = List.map extract_store_int_imm cfg_ops in
        match create_const_vec consts with
        | None -> None
        | Some [const_instruction] ->
          let num_args_addressing = Arch.num_args_addressing addressing_mode in
          assert (arg_count = num_args_addressing);
          assert (res_count = 0);
          assert (Array.length const_instruction.results = 1);
          let new_reg = Vectorize_utils.Vectorized_instruction.New_Vec128 0 in
          const_instruction.results.(0) <- new_reg;
          let address_args =
            Array.init num_args_addressing (fun i ->
                Vectorize_utils.Vectorized_instruction.Original i)
          in
          let store_operation =
            Operation.Store
              (Onetwentyeight_unaligned, addressing_mode, is_assignment)
          in
          let store_instruction : Vectorize_utils.Vectorized_instruction.t =
            { operation = store_operation;
              arguments = Array.append [| new_reg |] address_args;
              results = [||]
            }
          in
          Some [const_instruction; store_instruction]
        | Some _ -> None)
    | Ifloatarithmem (float_width, float_op, addressing_mode) ->
      let float_width_in_bits : Vectorize_utils.Width_in_bits.t =
        match float_width with Float64 -> W64 | Float32 -> W32
      in
      assert (Vectorize_utils.Width_in_bits.equal float_width_in_bits width_type);
      let num_args_addressing = Arch.num_args_addressing addressing_mode in
      assert (arg_count = 1 + num_args_addressing);
      assert (res_count = 1);
      let results = [| Vectorize_utils.Vectorized_instruction.Result 0 |] in
      let address_args =
        Array.init num_args_addressing (fun i ->
            Vectorize_utils.Vectorized_instruction.Original (i + 1))
      in
      let append_result res args =
        let args = Array.append res args in
        if Proc.has_three_operand_float_ops ()
        then args.(0) <- Vectorize_utils.Vectorized_instruction.Argument 0;
        args
      in
      let sse, avx =
        match float_width, float_op with
        | Float64, Ifloatadd -> addpd, vaddpd_X_X_Xm128
        | Float64, Ifloatsub -> subpd, vsubpd_X_X_Xm128
        | Float64, Ifloatmul -> mulpd, vmulpd_X_X_Xm128
        | Float64, Ifloatdiv -> divpd, vdivpd_X_X_Xm128
        | Float32, Ifloatadd -> addps, vaddps_X_X_Xm128
        | Float32, Ifloatsub -> subps, vsubps_X_X_Xm128
        | Float32, Ifloatmul -> mulps, vmulps_X_X_Xm128
        | Float32, Ifloatdiv -> divps, vdivps_X_X_Xm128
      in
      if is_aligned_to_vector_width ()
      then
        let arguments = append_result results address_args in
        simd_load_sse_or_avx ~mode:addressing_mode sse avx arguments
        |> Option.map (fun (operation, arguments) ->
            [ { Vectorize_utils.Vectorized_instruction.operation;
                arguments;
                results
              } ])
      else
        (* Emit a load followed by an arithmetic operation, effectively
           reverting the decision from Arch.selection. It will probably not be
           beneficial with 128-bit accesses. *)
        let new_reg =
          [| Vectorize_utils.Vectorized_instruction.New_Vec128 0 |]
        in
        let load : Vectorize_utils.Vectorized_instruction.t =
          { operation =
              Operation.Load
                { memory_chunk = vec128_chunk ();
                  addressing_mode;
                  mutability = Mutable;
                  is_atomic = false
                };
            arguments = address_args;
            results = new_reg
          }
        in
        let arguments = append_result results new_reg in
        let arith : Vectorize_utils.Vectorized_instruction.t =
          { operation = sse_or_avx sse avx; arguments; results }
        in
        Some [load; arith]
    | Isimd_mem _ ->
      Misc.fatal_error "Unexpected simd operation with memory arguments"
    | Ioffset_loc _ | Ibswap _ | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence
    | Ipackf32 | Isimd _ | Iprefetch _ | Icldemote _ ->
      None
    | Illvm_intrinsic intr ->
      Misc.fatal_errorf
        "Simd_selection: Unexpected llvm_intrinsic %s: not using LLVM backend"
        intr)
  | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
  | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
  | Const_vec256 _ | Const_vec512 _ | Const_mask _ | Stackoffset _ | Int128op _
  | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque | Pause
  | Begin_region | End_region | Name_for_debugger _ | Dls_get | Tls_get
  | Domain_index | Poll ->
    None
