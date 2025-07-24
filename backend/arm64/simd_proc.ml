(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-42"]

(* SIMD register behavior for ARM64 *)

open! Int_replace_polymorphic_compare [@@warning "-66"]

(* [R] stands for register (not stack)
 *
 * [f32] Float32 in scalar register <Sn>
 * [s32] Int in general purpose register  <Wn>
 * [s64] Int in general purpose register <Xn>
 * [f32x2] vector of two Float32 values represented
 *  using machtype Float and emitted in vector register Vd.2S
 *)

(* CR gyorsh: should it be named using the corresponding arm64 vector reg names?
   [RS] [RW] [RX] [R2S] and so on, instead of the above.

   The type refers to the hard register to be used, not to the machtype of the
   corresponding Reg.t. *)
type register_behavior =
  (* vector *)
  | Rf32x2_Rf32x2_to_Rf32x2
  | Rf32x4_Rf32x4_to_Rf32x4
  | Rf64x2_Rf64x2_to_Rf64x2
  | Rs64x2_Rs64x2_to_Rs64x2
  | Rf32x4_Rf32x4_to_Rs32x4
  | Rs32x4_to_Rs32x4
  | Rs32x4_to_Rf32x4
  | Rf32x4_to_Rf32x4
  | Rf32x4_to_Rs32x4
  | Rf32x2_to_Rf64x2
  | Rf64x2_to_Rf32x2
  | Rs8x16_to_Rs8x16
  | Rs8x16_Rs8x16_to_Rs8x16
  | Rs64x2_to_Rs64x2
  | Rf64x2_to_Rs64x2
  | Rf64x2_Rf64x2_to_Rs64x2
  | Rs32x4_Rs32x4_to_Rs32x4
  | Rf64x2_to_Rf64x2
  | Rs64x2_to_Rf64x2
  | Rs32x2_to_Rs64x2
  | Rs16x8_Rs16x8_to_Rs16x8
  | Rs16x8_Rs16x8_to_Rs32x4
  | Rs16x4_Rs16x4_to_Rs32x4
  | Rs16x8_to_Rs16x8
  | Rs64x2_to_Rs32x2
  | Rs32x4_to_Rs16x4
  | Rs16x8_to_Rs8x8
  | Rs32x4_Rs64x2_to_First
  | Rs16x8_Rs32x4_to_First
  | Rs8x16_Rs16x8_to_First
  | Rs16x4_to_Rs32x4
  | Rs8x8_to_Rs16x8
  (* scalar *)
  | Rf32_Rf32_to_Rf32
  | Rf64_Rf64_to_Rf64
  | Rf32_to_Rf32
  | Rf64_to_Rf64
  | Rf32_to_Rs64
  (* extract *)
  | Rs8x16_to_Rs8 of { lane : int }
  | Rs16x8_to_Rs16 of { lane : int }
  | Rs32x4_to_Rs32 of { lane : int }
  | Rs64x2_to_Rs64 of { lane : int }
  (* insert *)
  | Rs8x16_Rs8_to_First of { lane : int }
  | Rs16x8_Rs16_to_First of { lane : int }
  | Rs32x4_Rs32_to_First of { lane : int }
  | Rs64x2_Rs64_to_First of { lane : int }
  | Rs64x2_Rs64x2_to_First of
      { src_lane : int;
        dst_lane : int
      }
  (* dup *)
  | Rs8x16lane_to_Rs8x16 of { lane : int }
  | Rs16x8lane_to_Rs16x8 of { lane : int }
  | Rs32x4lane_to_Rs32x4 of { lane : int }
  | Rs64x2lane_to_Rs64x2 of { lane : int }

let register_behavior (op : Simd.operation) =
  match op with
  (* unary *)
  | Round_f32_s64 -> Rf32_to_Rs64
  | Round_f32 _ -> Rf32_to_Rf32
  | Round_f64 _ -> Rf64_to_Rf64
  (* binary *)
  | Fmin_f32 | Fmax_f32 | Min_scalar_f32 | Max_scalar_f32 -> Rf32_Rf32_to_Rf32
  | Fmin_f64 | Fmax_f64 | Min_scalar_f64 | Max_scalar_f64 -> Rf64_Rf64_to_Rf64
  | Zip1_f32 -> Rf32x2_Rf32x2_to_Rf32x2
  | Addq_f32 | Subq_f32 | Mulq_f32 | Divq_f32 | Minq_f32 | Maxq_f32 | Paddq_f32
  | Zip1q_f32 | Zip2q_f32 ->
    Rf32x4_Rf32x4_to_Rf32x4
  | Recpeq_f32 | Sqrtq_f32 | Rsqrteq_f32 | Roundq_f32 _ -> Rf32x4_to_Rf32x4
  | Addq_f64 | Subq_f64 | Mulq_f64 | Divq_f64 | Zip1q_f64 | Zip2q_f64 | Minq_f64
  | Maxq_f64 | Paddq_f64 ->
    Rf64x2_Rf64x2_to_Rf64x2
  | Sqrtq_f64 | Rsqrteq_f64 | Roundq_f64 _ -> Rf64x2_to_Rf64x2
  | Addq_s64 | Subq_s64 | Paddq_s64 | Shlq_u64 | Shlq_s64 ->
    Rs64x2_Rs64x2_to_Rs64x2
  | Cvtq_s32_f32 | Cvtnq_s32_f32 -> Rf32x4_to_Rs32x4
  | Cvtq_f32_s32 -> Rs32x4_to_Rf32x4
  | Cvt_f64_f32 ->
    (* Input should be in Vec128 register but only the bottom f32x2 is used by
       this instruction. *)
    Rf32x2_to_Rf64x2
  | Cvt_f32_f64 ->
    (* Output should be in Vec128 register but only the bottom f32x2 is used by
       this instruction. *)
    Rf64x2_to_Rf32x2
  | Movl_s32 | Movl_u32 ->
    (* Input should be in Vec128 register but only the bottom s32x2 is used by
       this instruction. *)
    Rs32x2_to_Rs64x2
  | Movl_s16 | Movl_u16 -> Rs16x4_to_Rs32x4
  | Movl_s8 | Movl_u8 -> Rs8x8_to_Rs16x8
  | Movn_s64 | Qmovn_s64 ->
    (* Output should be in Vec128 register but only the bottom s32x2 is used by
       this instruction. *)
    Rs64x2_to_Rs32x2
  | Movn_high_s64 | Qmovn_high_s64 -> Rs32x4_Rs64x2_to_First
  | Qmovn_u32 | Qmovn_s32 | Movn_s32 -> Rs32x4_to_Rs16x4
  | Qmovn_high_s32 | Qmovn_high_u32 | Movn_high_s32 -> Rs16x8_Rs32x4_to_First
  | Qmovn_u16 | Qmovn_s16 | Movn_s16 -> Rs16x8_to_Rs8x8
  | Qmovn_high_s16 | Qmovn_high_u16 | Movn_high_s16 -> Rs8x16_Rs16x8_to_First
  | Cmp_f32 _ -> Rf32x4_Rf32x4_to_Rs32x4
  | Cmpz_f32 _ -> Rf32x4_to_Rs32x4
  | Cmp_f64 _ -> Rf64x2_Rf64x2_to_Rs64x2
  | Cmpz_f64 _ | Cvtq_s64_f64 | Cvtnq_s64_f64 -> Rf64x2_to_Rs64x2
  | Cvtq_f64_s64 -> Rs64x2_to_Rf64x2
  | Cmp_s32 _ -> Rf32x4_Rf32x4_to_Rs32x4
  | Cmp_s64 _ -> Rs64x2_Rs64x2_to_Rs64x2
  | Cmpz_s64 _ -> Rs64x2_to_Rs64x2
  | Negq_s32 | Cmpz_s32 _ | Absq_s32 | Shlq_n_u32 _ | Shrq_n_u32 _
  | Shrq_n_s32 _ ->
    Rs32x4_to_Rs32x4
  | Copyq_laneq_s64 { src_lane; dst_lane } ->
    Rs64x2_Rs64x2_to_First { src_lane; dst_lane }
  | Setq_lane_s8 { lane } -> Rs8x16_Rs8_to_First { lane }
  | Setq_lane_s16 { lane } -> Rs16x8_Rs16_to_First { lane }
  | Setq_lane_s32 { lane } -> Rs32x4_Rs32_to_First { lane }
  | Setq_lane_s64 { lane } -> Rs64x2_Rs64_to_First { lane }
  | Getq_lane_s8 { lane } -> Rs8x16_to_Rs8 { lane }
  | Getq_lane_s16 { lane } -> Rs16x8_to_Rs16 { lane }
  | Getq_lane_s32 { lane } -> Rs32x4_to_Rs32 { lane }
  | Getq_lane_s64 { lane } -> Rs64x2_to_Rs64 { lane }
  | Dupq_lane_s8 { lane } -> Rs8x16lane_to_Rs8x16 { lane }
  | Dupq_lane_s16 { lane } -> Rs16x8lane_to_Rs16x8 { lane }
  | Dupq_lane_s32 { lane } -> Rs32x4lane_to_Rs32x4 { lane }
  | Dupq_lane_s64 { lane } -> Rs64x2lane_to_Rs64x2 { lane }
  | Orrq_s8 | Andq_s8 | Eorq_s8 | Orrq_s16 | Andq_s16 | Eorq_s16 | Eorq_s32
  | Andq_s32 | Orrq_s32 | Orrq_s64 | Andq_s64 | Eorq_s64 ->
    (* Bitwise operation, lane width does not matter. The only two encodings
       provided are 8B and 16B. *)
    Rs8x16_Rs8x16_to_Rs8x16
  | Mvnq_s32 | Mvnq_s64 | Mvnq_s16 | Mvnq_s8 -> Rs8x16_to_Rs8x16
  | Addq_s32 | Subq_s32 | Minq_s32 | Maxq_s32 | Minq_u32 | Maxq_u32 | Paddq_s32
  | Shlq_u32 | Shlq_s32 | Mulq_s32 ->
    Rs32x4_Rs32x4_to_Rs32x4
  | Absq_s64 | Shlq_n_u64 _ | Shrq_n_u64 _ | Shrq_n_s64 _ | Negq_s64 ->
    Rs64x2_to_Rs64x2
  | Addq_s16 | Paddq_s16 | Qaddq_s16 | Qaddq_u16 | Subq_s16 | Qsubq_s16
  | Qsubq_u16 | Minq_s16 | Maxq_s16 | Minq_u16 | Maxq_u16 | Shlq_u16 | Shlq_s16
  | Cmp_s16 _ | Zip1q_s16 | Zip2q_s16 | Mulq_s16 ->
    Rs16x8_Rs16x8_to_Rs16x8
  | Absq_s16 | Negq_s16 | Cntq_u16 | Shlq_n_u16 _ | Shrq_n_u16 _ | Shrq_n_s16 _
  | Cmpz_s16 _ ->
    Rs16x8_to_Rs16x8
  | Addq_s8 | Paddq_s8 | Qaddq_s8 | Qaddq_u8 | Subq_s8 | Qsubq_s8 | Qsubq_u8
  | Minq_s8 | Maxq_s8 | Minq_u8 | Maxq_u8 | Shlq_u8 | Shlq_s8 | Cmp_s8 _
  | Zip1q_s8 | Zip2q_s8 | Extq_u8 _ ->
    Rs8x16_Rs8x16_to_Rs8x16
  | Absq_s8 | Negq_s8 | Cntq_u8 | Shlq_n_u8 _ | Shrq_n_u8 _ | Shrq_n_s8 _
  | Cmpz_s8 _ ->
    Rs8x16_to_Rs8x16
  | Mullq_s16 | Mullq_u16 -> Rs16x4_Rs16x4_to_Rs32x4
  | Mullq_high_s16 | Mullq_high_u16 -> Rs16x8_Rs16x8_to_Rs32x4
