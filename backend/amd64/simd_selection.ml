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

(* SIMD instruction selection for AMD64 *)

open Arch
open Amd64_simd_instrs

type error = Bad_immediate of string

exception Error of error

module Seq = Simd.Seq

let instr instr ?i args = Some (Simd.instruction instr i, args)

let seq seq ?i args = Some (Simd.sequence seq i, args)

let sse_or_avx sse vex ?i args =
  let sse_or_avx = if Arch.Extension.enabled AVX then vex else sse in
  Some (Simd.instruction sse_or_avx i, args)

let seq_or_avx sse vex ?i args =
  let seq = if Arch.Extension.enabled AVX then vex else sse in
  Some (Simd.sequence seq i, args)

let seq_or_avx_zeroed ~dbg seq instr ?i args =
  if Arch.Extension.enabled AVX
  then
    Some
      ( Simd.instruction instr i,
        Cmm_helpers.vec128 ~dbg { word0 = 0L; word1 = 0L } :: args )
  else Some (Simd.sequence seq i, args)

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
      | Cexit (_, _, _) ))
    :: _ ->
    bad_immediate "Did not get integer immediate for %s" name

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

let select_operation_bmi2 ~dbg:_ op args =
  if not (Arch.Extension.enabled BMI2)
  then None
  else
    match op with
    | "caml_bmi2_int64_extract_bits" ->
      sse_or_avx pext_r64_r64_r64m64 pext_r64_r64_r64m64 args
    | "caml_bmi2_int64_deposit_bits" ->
      sse_or_avx pdep_r64_r64_r64m64 pdep_r64_r64_r64m64 args
    | _ -> None

let select_operation_sse ~dbg op args =
  match op with
  | "caml_sse_float32_sqrt" | "sqrtf" ->
    seq_or_avx_zeroed ~dbg Seq.sqrtss vsqrtss args
  | "caml_simd_float32_max" | "caml_sse_float32_max" ->
    sse_or_avx maxss vmaxss args
  | "caml_simd_float32_min" | "caml_sse_float32_min" ->
    sse_or_avx minss vminss args
  | "caml_sse_cast_float32_int64" | "caml_simd_cast_float32_int64" ->
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
  | "caml_sse_vec128_high_64_to_low_64" -> sse_or_avx movhlps vmovhlps args
  | "caml_sse_vec128_low_64_to_high_64" -> sse_or_avx movlhps vmovlhps args
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
  | "caml_sse2_float64_sqrt" | "sqrt" ->
    seq_or_avx_zeroed ~dbg Seq.sqrtsd vsqrtsd args
  | "caml_simd_float64_max" | "caml_sse2_float64_max" ->
    sse_or_avx maxsd vmaxsd args
  | "caml_simd_float64_min" | "caml_sse2_float64_min" ->
    sse_or_avx minsd vminsd args
  | "caml_sse2_cast_float64_int64" ->
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
      sse_or_avx pextrb vpextrb ~i args
    | "caml_sse41_int16x8_extract" ->
      let i, args = extract_constant args ~max:7 op in
      sse_or_avx pextrw_r64m16_X vpextrw_r64m16_X ~i args
    | "caml_sse41_int32x4_extract" ->
      let i, args = extract_constant args ~max:3 op in
      sse_or_avx pextrd vpextrd ~i args
    | "caml_sse41_int64x2_extract" ->
      let i, args = extract_constant args ~max:1 op in
      sse_or_avx pextrq vpextrq ~i args
    | "caml_sse41_int8x16_insert" ->
      let i, args = extract_constant args ~max:15 op in
      sse_or_avx pinsrb vpinsrb ~i args
    | "caml_sse41_int16x8_insert" ->
      let i, args = extract_constant args ~max:7 op in
      sse_or_avx pinsrw_X_r32m16 vpinsrw ~i args
    | "caml_sse41_int32x4_insert" ->
      let i, args = extract_constant args ~max:3 op in
      sse_or_avx pinsrd vpinsrd ~i args
    | "caml_sse41_int64x2_insert" ->
      let i, args = extract_constant args ~max:1 op in
      sse_or_avx pinsrq vpinsrq ~i args
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
    | "caml_simd_float64_round_near" | "caml_sse41_float64_round_near" ->
      seq_or_avx_zeroed ~dbg Seq.roundsd vroundsd
        ~i:(int_of_float_rounding RoundNearest)
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
    | "caml_simd_float32_round_near" | "caml_sse41_float32_round_near" ->
      seq_or_avx_zeroed ~dbg Seq.roundss vroundss
        ~i:(int_of_float_rounding RoundNearest)
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
    | "caml_avx_vec256_broadcast_128" -> instr vbroadcastf128 args
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
      instr vpermpd ~i args
    | "caml_avx2_vec256_permutev_32" -> instr vpermps args
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

let select_simd_instr ~dbg op args =
  let or_else try_ opt =
    match opt with Some x -> Some x | None -> try_ ~dbg op args
  in
  None
  |> or_else select_operation_clmul
  |> or_else select_operation_bmi2
  |> or_else select_operation_sse
  |> or_else select_operation_sse2
  |> or_else select_operation_sse3
  |> or_else select_operation_ssse3
  |> or_else select_operation_sse41
  |> or_else select_operation_sse42
  |> or_else select_operation_avx
  |> or_else select_operation_avx2

let select_operation_cfg ~dbg op args =
  select_simd_instr ~dbg op args
  |> Option.map (fun (op, args) -> Operation.Specific (Isimd op), args)

let pseudoregs_for_mem_operation (op : Simd.Mem.operation) arg res =
  match op with
  | Add_f64 | Sub_f64 | Mul_f64 | Div_f64 | Add_f32 | Sub_f32 | Mul_f32
  | Div_f32 ->
    if Proc.has_three_operand_float_ops ()
    then None
    else Some ([| res.(0); arg.(1) |], res)

let rax = Proc.phys_reg Int 0

let rcx = Proc.phys_reg Int 5

let rdx = Proc.phys_reg Int 4

let xmm0v = Proc.phys_reg Vec128 100

let to_phys_reg (pinned_reg : Simd.reg) =
  match pinned_reg with RAX -> rax | RCX -> rcx | RDX -> rdx | XMM0 -> xmm0v

let maybe_pin arr i loc =
  match Simd.loc_is_pinned loc with
  | None -> ()
  | Some pinned_loc -> arr.(i) <- to_phys_reg pinned_loc

let pseudoregs_for_instr (simd : Simd.instr) arg_regs res_regs =
  Array.iteri
    (fun i (simd_arg : Simd.arg) -> maybe_pin arg_regs i simd_arg.loc)
    simd.args;
  (match simd.res with
  | First_arg ->
    assert (not (Reg.is_preassigned arg_regs.(0)));
    arg_regs.(0) <- res_regs.(0)
  | Res { loc; _ } -> maybe_pin res_regs 0 loc);
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

(* Error report *)

let report_error ppf = function
  | Bad_immediate msg -> Format.pp_print_string ppf msg

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
    | Idiv | Imod | Iclz _ | Ictz _ | Ipopcnt -> None
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
      | Const_vec512 _ | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _
      | Probe_is_enabled _ | Opaque | Begin_region | End_region | Pause
      | Name_for_debugger _ | Dls_get | Poll ->
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
      | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Intop_atomic _
      | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque | Begin_region
      | End_region | Name_for_debugger _ | Dls_get | Poll | Pause ->
        assert false
    in
    let consts = List.map extract_intop_imm_int cfg_ops in
    match create_const_vec consts, vectorize_intop intop with
    | Some [const_instruction], Some [intop_instruction] ->
      if Array.length const_instruction.results = 1
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
          | Isextend32 | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence
          | Imfence | Ipackf32 | Isimd _ | Isimd_mem _ | Iprefetch _
          | Icldemote _ ->
            assert false)
        | Move | Load _ | Store _ | Intop _ | Intop_imm _ | Alloc _
        | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
        | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
        | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Intop_atomic _
        | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Name_for_debugger _ | Dls_get | Poll | Pause ->
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
              | Izextend32 )
          | Intop_imm _ | Move | Load _ | Store _ | Intop _ | Alloc _
          | Reinterpret_cast _ | Static_cast _ | Spill | Reload | Const_int _
          | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
          | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Intop_atomic _
          | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque | Begin_region
          | End_region | Name_for_debugger _ | Dls_get | Poll | Pause ->
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
      if is_aligned_to_vector_width ()
      then
        let sse_op : Simd.Mem.operation =
          match float_width, float_op with
          | Float64, Ifloatadd -> Add_f64
          | Float64, Ifloatsub -> Sub_f64
          | Float64, Ifloatmul -> Mul_f64
          | Float64, Ifloatdiv -> Div_f64
          | Float32, Ifloatadd -> Add_f32
          | Float32, Ifloatsub -> Sub_f32
          | Float32, Ifloatmul -> Mul_f32
          | Float32, Ifloatdiv -> Div_f32
        in
        let arguments = append_result results address_args in
        Some
          [ { operation =
                Operation.Specific (Isimd_mem (sse_op, addressing_mode));
              arguments;
              results
            } ]
      else
        (* Emit a load followed by an arithmetic operation, effectively
           reverting the decision from Arch.selection. It will probably not be
           beneficial with 128-bit accesses. *)
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
      None)
  | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
  | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
  | Const_vec256 _ | Const_vec512 _ | Stackoffset _ | Intop_atomic _ | Floatop _
  | Csel _ | Probe_is_enabled _ | Opaque | Pause | Begin_region | End_region
  | Name_for_debugger _ | Dls_get | Poll ->
    None
