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

(* SIMD instructions for ARM64 *)

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

type operation_class = Pure

module Rounding_mode = struct
  type t =
    | Current (* Default is Nearest *)
    | Neg_inf
    | Pos_inf
    | Zero
    | Nearest

  let instruction_suffix = function
    | Neg_inf -> "Neg_inf"
    | Pos_inf -> "Pos_inf"
    | Zero -> "Zero"
    | Current -> "Current"
    | Nearest -> "Nearest"

  let equal t1 t2 =
    match t1, t2 with
    | Current, Current
    | Neg_inf, Neg_inf
    | Pos_inf, Pos_inf
    | Zero, Zero
    | Nearest, Nearest ->
      true
    | (Current | Neg_inf | Pos_inf | Zero | Nearest), _ -> false
end

module Float_cond = struct
  type t = Arm64_ast.Instruction_name.Float_cond.t =
    | EQ
    | GT
    | LE
    | GE
    | LT
    | NE
    | CC
    | CS
    | LS
    | HI

  let to_string t =
    match t with
    | EQ -> "eq"
    | GT -> "gt"
    | LE -> "le"
    | GE -> "ge"
    | LT -> "lt"
    | NE -> "ne"
    | CC -> "cc"
    | CS -> "cs"
    | LS -> "ls"
    | HI -> "hi"

  let equal t1 t2 =
    match t1, t2 with
    | EQ, EQ
    | GT, GT
    | LE, LE
    | GE, GE
    | LT, LT
    | NE, NE
    | CC, CC
    | CS, CS
    | LS, LS
    | HI, HI ->
      true
    | (EQ | GT | LE | GE | LT | NE | CC | CS | LS | HI), _ -> false
end

module Cond = struct
  type t =
    | EQ
    | GE
    | GT
    | LE
    | LT

  let to_string t =
    match t with
    | EQ -> "eq"
    | GE -> "ge"
    | GT -> "ne"
    | LE -> "le"
    | LT -> "lt"

  let equal t1 t2 =
    match t1, t2 with
    | EQ, EQ | GE, GE | GT, GT | LE, LE | LT, LT -> true
    | (EQ | GE | GT | LE | LT), _ -> false
end

(* CR-soon gyorsh: rename vector intrinsics to start with "v" to match the
   naming convention of arm64 intrinsics for C. *)
(* CR-soon gyorsh: Consider factoring out operand types and eliminate simd_proc.
   For example, Addq_s16, Addq_s32, Addq_s64 can be representated by the same
   constructor with argument that determines [Simd_proc.t]. See also amd64
   representation of simd instructions. *)
type operation =
  | Round_f32 of Rounding_mode.t
  | Round_f64 of Rounding_mode.t
  | Roundq_f32 of Rounding_mode.t
  | Roundq_f64 of Rounding_mode.t
  | Round_f32_s64
  (* [Min_scalar_f32/Max_scalar_f32] are emitted as a sequence of instructions
     that matches amd64 semantics of the same intrinsic
     [caml_simd_float32_min/max], regardless of the value of [FPCR.AH]. *)
  | Min_scalar_f32
  | Max_scalar_f32
  | Min_scalar_f64
  | Max_scalar_f64
  (* [Fmin/Fmax] are emitted as the corresponding arm64 single instructions. *)
  | Fmin_f32
  | Fmax_f32
  | Fmin_f64
  | Fmax_f64
  | Extq_u8 of int
  | Zip1_f32
  | Zip1q_s8
  | Zip2q_s8
  | Zip1q_s16
  | Zip2q_s16
  | Zip1q_f32
  | Zip2q_f32
  | Zip1q_f64
  | Zip2q_f64
  | Addq_s64
  | Subq_s64
  | Mulq_s32
  | Mulq_s16
  | Addq_s32
  | Subq_s32
  | Minq_s32
  | Maxq_s32
  | Minq_u32
  | Maxq_u32
  | Addq_f32
  | Subq_f32
  | Mulq_f32
  | Divq_f32
  | Minq_f32
  | Maxq_f32
  | Addq_f64
  | Subq_f64
  | Mulq_f64
  | Divq_f64
  | Minq_f64
  | Maxq_f64
  | Absq_s32
  | Absq_s64
  | Recpeq_f32
  | Sqrtq_f32
  | Rsqrteq_f32
  | Sqrtq_f64
  | Rsqrteq_f64
  | Addq_s16
  | Paddq_s16
  | Qaddq_s16
  | Qaddq_u16
  | Subq_s16
  | Qsubq_s16
  | Qsubq_u16
  | Absq_s16
  | Minq_s16
  | Maxq_s16
  | Minq_u16
  | Maxq_u16
  | Mvnq_s16
  | Orrq_s16
  | Andq_s16
  | Eorq_s16
  | Negq_s16
  | Cntq_u16
  | Addq_s8
  | Paddq_s8
  | Qaddq_s8
  | Qaddq_u8
  | Subq_s8
  | Qsubq_s8
  | Qsubq_u8
  | Absq_s8
  | Minq_s8
  | Maxq_s8
  | Minq_u8
  | Maxq_u8
  | Mvnq_s8
  | Orrq_s8
  | Andq_s8
  | Eorq_s8
  | Negq_s8
  | Cntq_u8
  | Cvtq_s32_f32
  | Cvtnq_s32_f32
  | Cvtq_f32_s32
  | Cvt_f64_f32
  | Cvt_f32_f64
  | Cvtq_f64_s64
  | Cvtq_s64_f64
  | Cvtnq_s64_f64
  | Movl_s32 (* sign extend *)
  | Movl_u32 (* zero extend *)
  | Movl_s16
  | Movl_u16
  | Movl_s8
  | Movl_u8
  | Paddq_f32
  | Paddq_f64
  | Paddq_s32
  | Paddq_s64
  | Cmp_f32 of Float_cond.t
  | Cmp_f64 of Float_cond.t
  | Cmpz_f32 of Float_cond.t
  | Cmpz_f64 of Float_cond.t
  | Cmp_s32 of Cond.t
  | Cmp_s64 of Cond.t
  | Cmpz_s32 of Cond.t
  | Cmpz_s64 of Cond.t
  | Cmp_s16 of Cond.t
  | Cmpz_s16 of Cond.t
  | Cmp_s8 of Cond.t
  | Cmpz_s8 of Cond.t
  | Mvnq_s32
  | Orrq_s32
  | Andq_s32
  | Eorq_s32
  | Negq_s32
  | Mvnq_s64
  | Orrq_s64
  | Andq_s64
  | Eorq_s64
  | Negq_s64
  | Shlq_u32
  | Shlq_u64
  | Shlq_s32
  | Shlq_s64
  | Shlq_u16
  | Shlq_s16
  | Shlq_u8
  | Shlq_s8
  | Shlq_n_u32 of int
  | Shlq_n_u64 of int
  | Shrq_n_u32 of int
  | Shrq_n_u64 of int
  | Shrq_n_s32 of int
  | Shrq_n_s64 of int
  | Shlq_n_u16 of int
  | Shrq_n_u16 of int
  | Shrq_n_s16 of int
  | Shlq_n_u8 of int
  | Shrq_n_u8 of int
  | Shrq_n_s8 of int
  | Getq_lane_s32 of { lane : int (* 0 <= lane <= 3 *) }
  | Getq_lane_s64 of { lane : int (* 0 <= lane <= 1 *) }
  | Setq_lane_s32 of { lane : int (* 0 <= lane <= 3 *) }
  | Setq_lane_s64 of { lane : int (* 0 <= lane <= 1 *) }
  | Dupq_lane_s32 of { lane : int (* 0 <= lane <= 3 *) }
  | Dupq_lane_s64 of { lane : int (* 0 <= lane <= 1 *) }
  | Getq_lane_s16 of { lane : int (* 0 <= lane <= 7 *) }
  | Setq_lane_s16 of { lane : int (* 0 <= lane <= 7 *) }
  | Dupq_lane_s16 of { lane : int (* 0 <= lane <= 7 *) }
  | Getq_lane_s8 of { lane : int (* 0 <= lane <= 15 *) }
  | Setq_lane_s8 of { lane : int (* 0 <= lane <= 15 *) }
  | Dupq_lane_s8 of { lane : int (* 0 <= lane <= 15 *) }
  | Copyq_laneq_s64 of
      { src_lane : int;
        dst_lane : int
      }
  | Qmovn_high_s64
  | Qmovn_s64
  | Qmovn_high_s32
  | Qmovn_s32
  | Qmovn_high_u32
  | Qmovn_u32
  | Qmovn_high_s16
  | Qmovn_s16
  | Qmovn_high_u16
  | Qmovn_u16
  | Movn_high_s64
  | Movn_s64
  | Movn_high_s32
  | Movn_s32
  | Movn_high_s16
  | Movn_s16
  | Mullq_s16
  | Mullq_u16
  | Mullq_high_s16
  | Mullq_high_u16

let print_name op =
  match op with
  | Round_f32 rm -> "Round_f32_" ^ Rounding_mode.instruction_suffix rm
  | Round_f64 rm -> "Round_f64_" ^ Rounding_mode.instruction_suffix rm
  | Roundq_f32 rm -> "Roundq_f32_" ^ Rounding_mode.instruction_suffix rm
  | Roundq_f64 rm -> "Roundq_f64_" ^ Rounding_mode.instruction_suffix rm
  | Round_f32_s64 -> "Round_f32_i"
  | Extq_u8 c -> "Extq_u8_" ^ Int.to_string c
  | Zip1_f32 -> "Zip1_f32"
  | Zip1q_s8 -> "Zip1q_s8"
  | Zip2q_s8 -> "Zip2q_s8"
  | Zip1q_s16 -> "Zip1q_s16"
  | Zip2q_s16 -> "Zip2q_s16"
  | Zip1q_f32 -> "Zip1q_f32"
  | Zip2q_f32 -> "Zip2q_f32"
  | Zip1q_f64 -> "Zip1q_f64"
  | Zip2q_f64 -> "Zip2q_f64"
  | Fmin_f32 -> "Fmin_f32"
  | Fmax_f32 -> "Fmax_f32"
  | Fmin_f64 -> "Fmin_f64"
  | Fmax_f64 -> "Fmax_f64"
  | Min_scalar_f32 -> "Min_scalar_f32"
  | Max_scalar_f32 -> "Max_scalar_f32"
  | Min_scalar_f64 -> "Min_scalar_f64"
  | Max_scalar_f64 -> "Max_scalar_f64"
  | Addq_s64 -> "Addq_s64"
  | Subq_s64 -> "Subq_s64"
  | Addq_f32 -> "Addq_f32"
  | Subq_f32 -> "Subq_f32"
  | Mulq_f32 -> "Mulq_f64"
  | Divq_f32 -> "Divq_f64"
  | Minq_f32 -> "Minq_f32"
  | Maxq_f32 -> "Maxq_f32"
  | Addq_f64 -> "Addq_f64"
  | Subq_f64 -> "Subq_f64"
  | Mulq_f64 -> "Mulq_f64"
  | Divq_f64 -> "Divq_f64"
  | Minq_f64 -> "Minq_f64"
  | Maxq_f64 -> "Maxq_f64"
  | Recpeq_f32 -> "Recpeq_f64"
  | Sqrtq_f32 -> "Sqrtq_f3"
  | Rsqrteq_f32 -> "Rsqrtq_f32"
  | Sqrtq_f64 -> "Sqrtq_f64"
  | Rsqrteq_f64 -> "Rsqrtq_f64"
  | Cvtq_s32_f32 -> "Cvtq_s32_f32"
  | Cvtnq_s32_f32 -> "Cvtnq_s32_f32"
  | Cvtq_f32_s32 -> "Cvtq_f32_s32"
  | Cvt_f64_f32 -> "Cvt_f64_f32"
  | Cvt_f32_f64 -> "Cvt_f32_f64"
  | Cvtq_f64_s64 -> "Cvtq_f64_s64"
  | Cvtq_s64_f64 -> "Cvtq_s64_f64"
  | Cvtnq_s64_f64 -> "Cvtnq_s64_f64"
  | Movl_s32 -> "Movl_s32"
  | Movl_u32 -> "Movl_u32"
  | Movl_s16 -> "Movl_s16"
  | Movl_u16 -> "Movl_u16"
  | Movl_s8 -> "Movl_s8"
  | Movl_u8 -> "Movl_u8"
  | Paddq_f32 -> "Paddq_f32"
  | Paddq_f64 -> "Paddq_f64"
  | Cmp_f32 cond -> "Cmp_f32_" ^ Float_cond.to_string cond
  | Cmpz_f32 cond -> "Cmpz_f32_" ^ Float_cond.to_string cond
  | Cmp_s32 cond -> "Cmp_s32_" ^ Cond.to_string cond
  | Cmpz_s32 cond -> "Cmpz_s32_" ^ Cond.to_string cond
  | Cmp_f64 cond -> "Cmp_f64_" ^ Float_cond.to_string cond
  | Cmpz_f64 cond -> "Cmpz_f64_" ^ Float_cond.to_string cond
  | Cmp_s64 cond -> "Cmp_s64_" ^ Cond.to_string cond
  | Cmpz_s64 cond -> "Cmpz_s64_" ^ Cond.to_string cond
  | Cmp_s16 cond -> "Cmp_s16_" ^ Cond.to_string cond
  | Cmpz_s16 cond -> "Cmpz_s16_" ^ Cond.to_string cond
  | Cmp_s8 cond -> "Cmp_s8_" ^ Cond.to_string cond
  | Cmpz_s8 cond -> "Cmpz_s8_" ^ Cond.to_string cond
  | Mulq_s32 -> "Mulq_s32"
  | Mulq_s16 -> "Mulq_s16"
  | Mvnq_s32 -> "Mvnq_s32"
  | Orrq_s32 -> "Orrq_s32"
  | Andq_s32 -> "Andq_s32"
  | Eorq_s32 -> "Eorq_s32"
  | Negq_s32 -> "Negq_s32"
  | Addq_s32 -> "Addq_s32"
  | Subq_s32 -> "Subq_s32"
  | Minq_s32 -> "Minq_s32"
  | Maxq_s32 -> "Maxq_s32"
  | Minq_u32 -> "Minq_u32"
  | Maxq_u32 -> "Maxq_u32"
  | Absq_s32 -> "Absq_s32"
  | Absq_s64 -> "Absq_s64"
  | Paddq_s32 -> "Paddq_s32"
  | Paddq_s64 -> "Paddq_s64"
  | Mvnq_s64 -> "Mvnq_s32"
  | Orrq_s64 -> "Orrq_s64"
  | Andq_s64 -> "Andq_s64"
  | Eorq_s64 -> "Eorq_s64"
  | Negq_s64 -> "Negq_s64"
  | Addq_s16 -> "Addq_s16"
  | Paddq_s16 -> "Paddq_s16"
  | Qaddq_s16 -> "Qaddq_s16"
  | Qaddq_u16 -> "Qaddq_u16"
  | Subq_s16 -> "Subq_s16"
  | Qsubq_s16 -> "Qsubq_s16"
  | Qsubq_u16 -> "Qsubq_u16"
  | Absq_s16 -> "Absq_s16"
  | Minq_s16 -> "Minq_s16"
  | Maxq_s16 -> "Maxq_s16"
  | Minq_u16 -> "Minq_u16"
  | Maxq_u16 -> "Maxq_u16"
  | Mvnq_s16 -> "Mvnq_s16"
  | Orrq_s16 -> "Orrq_s16"
  | Andq_s16 -> "Andq_s16"
  | Eorq_s16 -> "Eorq_s16"
  | Negq_s16 -> "Negq_s16"
  | Cntq_u16 -> "Cntq_s16"
  | Addq_s8 -> "Addq_s8"
  | Paddq_s8 -> "Paddq_s8"
  | Qaddq_s8 -> "Qaddq_s8"
  | Qaddq_u8 -> "Qaddq_u8"
  | Subq_s8 -> "Subq_s8"
  | Qsubq_s8 -> "Qsubq_s8"
  | Qsubq_u8 -> "Qsubq_u8"
  | Absq_s8 -> "Absq_s8"
  | Minq_s8 -> "Minq_s8"
  | Maxq_s8 -> "Maxq_s8"
  | Minq_u8 -> "Minq_u8"
  | Maxq_u8 -> "Maxq_u8"
  | Mvnq_s8 -> "Mvnq_s8"
  | Orrq_s8 -> "Orrq_s8"
  | Andq_s8 -> "Andq_s8"
  | Eorq_s8 -> "Eorq_s8"
  | Negq_s8 -> "Negq_s8"
  | Cntq_u8 -> "Cntq_s8"
  | Shlq_n_u32 n -> "Shlq_n_u32" ^ Int.to_string n
  | Shlq_n_u64 n -> "Shlq_n_u64" ^ Int.to_string n
  | Shrq_n_u32 n -> "Shrq_n_u32" ^ Int.to_string n
  | Shrq_n_u64 n -> "Shrq_n_u64" ^ Int.to_string n
  | Shrq_n_s32 n -> "Shrq_n_s32" ^ Int.to_string n
  | Shrq_n_s64 n -> "Shrq_n_s64" ^ Int.to_string n
  | Shlq_n_u16 n -> "Shlq_n_u16" ^ Int.to_string n
  | Shrq_n_u16 n -> "Shrq_n_u16" ^ Int.to_string n
  | Shrq_n_s16 n -> "Shrq_n_s16" ^ Int.to_string n
  | Shlq_n_u8 n -> "Shlq_n_u8" ^ Int.to_string n
  | Shrq_n_u8 n -> "Shrq_n_u8" ^ Int.to_string n
  | Shrq_n_s8 n -> "Shrq_n_s8" ^ Int.to_string n
  | Shlq_u32 -> "Ushlq_u32"
  | Shlq_u64 -> "Ushlq_u64"
  | Shlq_s32 -> "Sshlq_s32"
  | Shlq_s64 -> "Sshlq_s64"
  | Shlq_u16 -> "Ushlq_u16"
  | Shlq_s16 -> "Sshlq_s16"
  | Shlq_u8 -> "Ushlq_u8"
  | Shlq_s8 -> "Sshlq_s8"
  | Setq_lane_s32 { lane } -> "Setq_lane_s32_" ^ Int.to_string lane
  | Setq_lane_s64 { lane } -> "Setq_lane_s64_" ^ Int.to_string lane
  | Getq_lane_s32 { lane } -> "Getq_lane_s32_" ^ Int.to_string lane
  | Getq_lane_s64 { lane } -> "Getq_lane_s64_" ^ Int.to_string lane
  | Dupq_lane_s32 { lane } -> "Dupq_lane_s32_" ^ Int.to_string lane
  | Dupq_lane_s64 { lane } -> "Dupq_lane_s64_" ^ Int.to_string lane
  | Setq_lane_s16 { lane } -> "Setq_lane_s16_" ^ Int.to_string lane
  | Getq_lane_s16 { lane } -> "Setq_lane_s16_" ^ Int.to_string lane
  | Dupq_lane_s16 { lane } -> "Setq_lane_s16_" ^ Int.to_string lane
  | Setq_lane_s8 { lane } -> "Setq_lane_s8_" ^ Int.to_string lane
  | Getq_lane_s8 { lane } -> "Setq_lane_s8_" ^ Int.to_string lane
  | Dupq_lane_s8 { lane } -> "Setq_lane_s8_" ^ Int.to_string lane
  | Copyq_laneq_s64 { src_lane; dst_lane } ->
    Printf.sprintf "Copyq_laneq_s64_%d_to_%d" src_lane dst_lane
  | Qmovn_high_s64 -> "Qmovn_high_s64"
  | Qmovn_s64 -> "Qmovn_s64"
  | Qmovn_high_s32 -> "Qmovn_high_s32"
  | Qmovn_s32 -> "Qmovn_s32"
  | Qmovn_high_u32 -> "Qmovn_high_u32"
  | Qmovn_u32 -> "Qmovn_u32"
  | Qmovn_high_s16 -> "Qmovn_high_s16"
  | Qmovn_s16 -> "Qmovn_s16"
  | Qmovn_high_u16 -> "Qmovn_high_u16"
  | Qmovn_u16 -> "Qmovn_u16"
  | Movn_high_s64 -> "Movn_high_s64"
  | Movn_s64 -> "Movn_s64"
  | Movn_high_s32 -> "Movn_high_s32"
  | Movn_s32 -> "Movn_s32"
  | Movn_high_s16 -> "Movn_high_s16"
  | Movn_s16 -> "Movn_s16"
  | Mullq_s16 -> "Mullq_s16"
  | Mullq_u16 -> "Mullq_u16"
  | Mullq_high_s16 -> "Mullq_high_s16"
  | Mullq_high_u16 -> "Mullq_high_u16"

let print_operation printreg op ppf arg =
  (* CR gyorsh: does not support memory operands (except stack operands). *)
  Format.fprintf ppf "%s %a" (print_name op)
    (Format.pp_print_seq ~pp_sep:Format.pp_print_space printreg)
    (arg |> Array.to_seq)

let equal_operation op1 op2 =
  match op1, op2 with
  | Round_f32 mode, Round_f32 mode'
  | Round_f64 mode, Round_f64 mode'
  | Roundq_f32 mode, Roundq_f32 mode'
  | Roundq_f64 mode, Roundq_f64 mode' ->
    Rounding_mode.equal mode mode'
  | Round_f32_s64, Round_f32_s64
  | Min_scalar_f32, Min_scalar_f32
  | Max_scalar_f32, Max_scalar_f32
  | Min_scalar_f64, Min_scalar_f64
  | Max_scalar_f64, Max_scalar_f64
  | Fmin_f32, Fmin_f32
  | Fmax_f32, Fmax_f32
  | Fmin_f64, Fmin_f64
  | Fmax_f64, Fmax_f64
  | Zip1_f32, Zip1_f32
  | Zip1q_s8, Zip1q_s8
  | Zip2q_s8, Zip2q_s8
  | Zip1q_s16, Zip1q_s16
  | Zip2q_s16, Zip2q_s16
  | Zip1q_f32, Zip1q_f32
  | Zip2q_f32, Zip2q_f32
  | Zip1q_f64, Zip1q_f64
  | Zip2q_f64, Zip2q_f64
  | Addq_s64, Addq_s64
  | Subq_s64, Subq_s64
  | Addq_f32, Addq_f32
  | Subq_f32, Subq_f32
  | Mulq_f32, Mulq_f32
  | Divq_f32, Divq_f32
  | Minq_f32, Minq_f32
  | Maxq_f32, Maxq_f32
  | Addq_f64, Addq_f64
  | Subq_f64, Subq_f64
  | Mulq_f64, Mulq_f64
  | Divq_f64, Divq_f64
  | Minq_f64, Minq_f64
  | Maxq_f64, Maxq_f64
  | Recpeq_f32, Recpeq_f32
  | Sqrtq_f32, Sqrtq_f32
  | Rsqrteq_f32, Rsqrteq_f32
  | Sqrtq_f64, Sqrtq_f64
  | Rsqrteq_f64, Rsqrteq_f64
  | Cvtq_s32_f32, Cvtq_s32_f32
  | Cvtnq_s32_f32, Cvtnq_s32_f32
  | Cvtq_f32_s32, Cvtq_f32_s32
  | Cvt_f64_f32, Cvt_f64_f32
  | Cvt_f32_f64, Cvt_f32_f64
  | Cvtq_f64_s64, Cvtq_f64_s64
  | Cvtq_s64_f64, Cvtq_s64_f64
  | Cvtnq_s64_f64, Cvtnq_s64_f64
  | Movl_s32, Movl_s32
  | Movl_u32, Movl_u32
  | Movl_s16, Movl_s16
  | Movl_u16, Movl_u16
  | Movl_s8, Movl_s8
  | Movl_u8, Movl_u8
  | Mulq_s32, Mulq_s32
  | Mulq_s16, Mulq_s16
  | Paddq_f32, Paddq_f32
  | Mvnq_s32, Mvnq_s32
  | Orrq_s32, Orrq_s32
  | Andq_s32, Andq_s32
  | Eorq_s32, Eorq_s32
  | Negq_s32, Negq_s32
  | Addq_s32, Addq_s32
  | Subq_s32, Subq_s32
  | Minq_s32, Minq_s32
  | Maxq_s32, Maxq_s32
  | Minq_u32, Minq_u32
  | Maxq_u32, Maxq_u32
  | Absq_s32, Absq_s32
  | Absq_s64, Absq_s64
  | Paddq_f64, Paddq_f64
  | Paddq_s32, Paddq_s32
  | Paddq_s64, Paddq_s64
  | Mvnq_s64, Mvnq_s64
  | Orrq_s64, Orrq_s64
  | Andq_s64, Andq_s64
  | Eorq_s64, Eorq_s64
  | Negq_s64, Negq_s64
  | Shlq_u32, Shlq_u32
  | Shlq_u64, Shlq_u64
  | Shlq_s32, Shlq_s32
  | Shlq_s64, Shlq_s64
  | Addq_s16, Addq_s16
  | Paddq_s16, Paddq_s16
  | Qaddq_s16, Qaddq_s16
  | Qaddq_u16, Qaddq_u16
  | Subq_s16, Subq_s16
  | Qsubq_s16, Qsubq_s16
  | Qsubq_u16, Qsubq_u16
  | Absq_s16, Absq_s16
  | Minq_s16, Minq_s16
  | Maxq_s16, Maxq_s16
  | Minq_u16, Minq_u16
  | Maxq_u16, Maxq_u16
  | Mvnq_s16, Mvnq_s16
  | Orrq_s16, Orrq_s16
  | Andq_s16, Andq_s16
  | Eorq_s16, Eorq_s16
  | Negq_s16, Negq_s16
  | Cntq_u16, Cntq_u16
  | Shlq_u16, Shlq_u16
  | Shlq_s16, Shlq_s16
  | Addq_s8, Addq_s8
  | Paddq_s8, Paddq_s8
  | Qaddq_s8, Qaddq_s8
  | Qaddq_u8, Qaddq_u8
  | Subq_s8, Subq_s8
  | Qsubq_s8, Qsubq_s8
  | Qsubq_u8, Qsubq_u8
  | Absq_s8, Absq_s8
  | Minq_s8, Minq_s8
  | Maxq_s8, Maxq_s8
  | Minq_u8, Minq_u8
  | Maxq_u8, Maxq_u8
  | Mvnq_s8, Mvnq_s8
  | Orrq_s8, Orrq_s8
  | Andq_s8, Andq_s8
  | Eorq_s8, Eorq_s8
  | Negq_s8, Negq_s8
  | Cntq_u8, Cntq_u8
  | Shlq_u8, Shlq_u8
  | Shlq_s8, Shlq_s8
  | Qmovn_high_s64, Qmovn_high_s64
  | Qmovn_s64, Qmovn_s64
  | Qmovn_high_s32, Qmovn_high_s32
  | Qmovn_s32, Qmovn_s32
  | Qmovn_high_u32, Qmovn_high_u32
  | Qmovn_u32, Qmovn_u32
  | Qmovn_high_s16, Qmovn_high_s16
  | Qmovn_s16, Qmovn_s16
  | Qmovn_high_u16, Qmovn_high_u16
  | Qmovn_u16, Qmovn_u16
  | Movn_high_s64, Movn_high_s64
  | Movn_s64, Movn_s64
  | Movn_high_s32, Movn_high_s32
  | Movn_s32, Movn_s32
  | Movn_high_s16, Movn_high_s16
  | Movn_s16, Movn_s16
  | Mullq_s16, Mullq_s16
  | Mullq_u16, Mullq_u16
  | Mullq_high_s16, Mullq_high_s16
  | Mullq_high_u16, Mullq_high_u16 ->
    true
  | Extq_u8 n1, Extq_u8 n2
  | Shrq_n_s32 n1, Shrq_n_s32 n2
  | Shrq_n_s64 n1, Shrq_n_s64 n2
  | Shlq_n_u32 n1, Shlq_n_u32 n2
  | Shlq_n_u64 n1, Shlq_n_u64 n2
  | Shrq_n_u32 n1, Shrq_n_u32 n2
  | Shrq_n_u64 n1, Shrq_n_u64 n2
  | Shlq_n_u16 n1, Shlq_n_u16 n2
  | Shrq_n_u16 n1, Shrq_n_u16 n2
  | Shrq_n_s16 n1, Shrq_n_s16 n2
  | Shlq_n_u8 n1, Shlq_n_u8 n2
  | Shrq_n_u8 n1, Shrq_n_u8 n2
  | Shrq_n_s8 n1, Shrq_n_s8 n2 ->
    Int.equal n1 n2
  | Getq_lane_s32 { lane = l }, Getq_lane_s32 { lane = l' }
  | Getq_lane_s64 { lane = l }, Getq_lane_s64 { lane = l' }
  | Setq_lane_s32 { lane = l }, Setq_lane_s32 { lane = l' }
  | Setq_lane_s64 { lane = l }, Setq_lane_s64 { lane = l' }
  | Dupq_lane_s32 { lane = l }, Dupq_lane_s32 { lane = l' }
  | Dupq_lane_s64 { lane = l }, Dupq_lane_s64 { lane = l' }
  | Getq_lane_s16 { lane = l }, Getq_lane_s16 { lane = l' }
  | Setq_lane_s16 { lane = l }, Setq_lane_s16 { lane = l' }
  | Dupq_lane_s16 { lane = l }, Dupq_lane_s16 { lane = l' }
  | Getq_lane_s8 { lane = l }, Getq_lane_s8 { lane = l' }
  | Setq_lane_s8 { lane = l }, Setq_lane_s8 { lane = l' }
  | Dupq_lane_s8 { lane = l }, Dupq_lane_s8 { lane = l' } ->
    Int.equal l l'
  | ( Copyq_laneq_s64 { src_lane = src; dst_lane = dst },
      Copyq_laneq_s64 { src_lane = src'; dst_lane = dst' } ) ->
    Int.equal src src' && Int.equal dst dst'
  | Cmp_s64 c, Cmp_s64 c'
  | Cmpz_s64 c, Cmpz_s64 c'
  | Cmp_s16 c, Cmp_s16 c'
  | Cmpz_s16 c, Cmpz_s16 c'
  | Cmp_s8 c, Cmp_s8 c'
  | Cmpz_s8 c, Cmpz_s8 c'
  | Cmp_s32 c, Cmp_s32 c'
  | Cmpz_s32 c, Cmpz_s32 c' ->
    Cond.equal c c'
  | Cmp_f32 c, Cmp_f32 c'
  | Cmpz_f32 c, Cmpz_f32 c'
  | Cmp_f64 c, Cmp_f64 c'
  | Cmpz_f64 c, Cmpz_f64 c' ->
    Float_cond.equal c c'
  | ( ( Round_f32 _ | Round_f64 _ | Roundq_f32 _ | Roundq_f64 _ | Round_f32_s64
      | Min_scalar_f32 | Max_scalar_f32 | Min_scalar_f64 | Max_scalar_f64
      | Fmin_f32 | Fmax_f32 | Fmin_f64 | Fmax_f64 | Extq_u8 _ | Zip1_f32
      | Zip1q_s8 | Zip2q_s8 | Zip1q_s16 | Zip2q_s16 | Zip1q_f32 | Zip2q_f32
      | Zip1q_f64 | Zip2q_f64 | Addq_s64 | Subq_s64 | Addq_f32 | Subq_f32
      | Mulq_f32 | Divq_f32 | Minq_f32 | Maxq_f32 | Minq_f64 | Addq_f64
      | Subq_f64 | Mulq_f64 | Divq_f64 | Maxq_f64 | Recpeq_f32 | Sqrtq_f32
      | Rsqrteq_f32 | Sqrtq_f64 | Rsqrteq_f64 | Cvtq_s32_f32 | Cvtnq_s32_f32
      | Cvtq_f32_s32 | Cvt_f64_f32 | Cvt_f32_f64 | Cvtq_f64_s64 | Cvtq_s64_f64
      | Cvtnq_s64_f64 | Movl_s32 | Movl_u32 | Movl_s16 | Movl_u16 | Movl_s8
      | Movl_u8 | Paddq_f32 | Cmp_f32 _ | Cmpz_f32 _ | Cmpz_s32 _ | Cmp_f64 _
      | Cmpz_f64 _ | Cmp_s32 _ | Cmp_s64 _ | Cmpz_s64 _ | Mvnq_s32 | Orrq_s32
      | Andq_s32 | Eorq_s32 | Negq_s32 | Getq_lane_s32 _ | Getq_lane_s64 _
      | Dupq_lane_s32 _ | Dupq_lane_s64 _ | Mulq_s32 | Mulq_s16 | Addq_s32
      | Subq_s32 | Minq_s32 | Maxq_s32 | Minq_u32 | Maxq_u32 | Absq_s32
      | Absq_s64 | Paddq_f64 | Paddq_s32 | Paddq_s64 | Mvnq_s64 | Orrq_s64
      | Andq_s64 | Eorq_s64 | Negq_s64 | Shlq_u32 | Shlq_u64 | Shlq_n_u32 _
      | Shlq_n_u64 _ | Shrq_n_u32 _ | Shrq_n_u64 _ | Shrq_n_s32 _ | Shrq_n_s64 _
      | Shlq_s32 | Shlq_s64 | Setq_lane_s32 _ | Setq_lane_s64 _ | Addq_s16
      | Paddq_s16 | Qaddq_s16 | Qaddq_u16 | Subq_s16 | Qsubq_s16 | Qsubq_u16
      | Absq_s16 | Minq_s16 | Maxq_s16 | Minq_u16 | Maxq_u16 | Mvnq_s16
      | Orrq_s16 | Andq_s16 | Eorq_s16 | Negq_s16 | Cntq_u16 | Shlq_u16
      | Shlq_s16 | Cmp_s16 _ | Cmpz_s16 _ | Shlq_n_u16 _ | Shrq_n_u16 _
      | Shrq_n_s16 _ | Getq_lane_s16 _ | Setq_lane_s16 _ | Dupq_lane_s16 _
      | Addq_s8 | Paddq_s8 | Qaddq_s8 | Qaddq_u8 | Subq_s8 | Qsubq_s8 | Qsubq_u8
      | Absq_s8 | Minq_s8 | Maxq_s8 | Minq_u8 | Maxq_u8 | Mvnq_s8 | Orrq_s8
      | Andq_s8 | Eorq_s8 | Negq_s8 | Cntq_u8 | Shlq_u8 | Shlq_s8 | Cmp_s8 _
      | Cmpz_s8 _ | Shlq_n_u8 _ | Shrq_n_u8 _ | Shrq_n_s8 _ | Getq_lane_s8 _
      | Setq_lane_s8 _ | Dupq_lane_s8 _ | Copyq_laneq_s64 _ | Qmovn_high_s64
      | Qmovn_s64 | Qmovn_high_s32 | Qmovn_s32 | Qmovn_high_u32 | Qmovn_u32
      | Qmovn_high_s16 | Qmovn_s16 | Qmovn_high_u16 | Qmovn_u16 | Movn_high_s64
      | Movn_s64 | Movn_high_s32 | Movn_s32 | Movn_high_s16 | Movn_s16
      | Mullq_s16 | Mullq_u16 | Mullq_high_s16 | Mullq_high_u16 ),
      _ ) ->
    false

let class_of_operation op =
  match op with
  | Round_f32 _ | Round_f64 _ | Roundq_f32 _ | Roundq_f64 _ | Round_f32_s64
  | Min_scalar_f32 | Max_scalar_f32 | Min_scalar_f64 | Max_scalar_f64 | Fmin_f32
  | Fmax_f32 | Fmin_f64 | Fmax_f64 | Extq_u8 _ | Zip1_f32 | Zip1q_s8 | Zip1q_s16
  | Zip2q_s8 | Zip2q_s16 | Zip1q_f32 | Zip2q_f32 | Zip1q_f64 | Zip2q_f64
  | Addq_s64 | Subq_s64 | Addq_f32 | Subq_f32 | Mulq_f32 | Divq_f32 | Minq_f32
  | Maxq_f32 | Addq_f64 | Subq_f64 | Mulq_f64 | Divq_f64 | Minq_f64 | Maxq_f64
  | Recpeq_f32 | Sqrtq_f32 | Rsqrteq_f32 | Sqrtq_f64 | Rsqrteq_f64
  | Cvtq_s32_f32 | Cvtnq_s32_f32 | Cvtq_f32_s32 | Cvt_f64_f32 | Cvt_f32_f64
  | Cvtq_f64_s64 | Cvtq_s64_f64 | Cvtnq_s64_f64 | Movl_s32 | Movl_u32 | Movl_s16
  | Movl_u16 | Movl_s8 | Movl_u8 | Paddq_f32 | Cmp_f32 _ | Cmpz_f32 _
  | Cmpz_s32 _ | Cmp_f64 _ | Cmpz_f64 _ | Cmp_s32 _ | Cmp_s64 _ | Cmpz_s64 _
  | Mvnq_s32 | Orrq_s32 | Andq_s32 | Eorq_s32 | Negq_s32 | Getq_lane_s32 _
  | Getq_lane_s64 _ | Dupq_lane_s32 _ | Dupq_lane_s64 _ | Mulq_s32 | Mulq_s16
  | Addq_s32 | Subq_s32 | Minq_s32 | Maxq_s32 | Minq_u32 | Maxq_u32 | Absq_s32
  | Absq_s64 | Paddq_f64 | Paddq_s32 | Paddq_s64 | Mvnq_s64 | Orrq_s64
  | Andq_s64 | Eorq_s64 | Negq_s64 | Shlq_u32 | Shlq_u64 | Shlq_s32 | Shlq_s64
  | Shlq_n_u32 _ | Shlq_n_u64 _ | Shrq_n_u32 _ | Shrq_n_u64 _ | Shrq_n_s32 _
  | Shrq_n_s64 _ | Setq_lane_s32 _ | Setq_lane_s64 _ | Addq_s16 | Paddq_s16
  | Qaddq_s16 | Qaddq_u16 | Subq_s16 | Qsubq_s16 | Qsubq_u16 | Absq_s16
  | Minq_s16 | Maxq_s16 | Minq_u16 | Maxq_u16 | Mvnq_s16 | Orrq_s16 | Andq_s16
  | Eorq_s16 | Negq_s16 | Cntq_u16 | Shlq_u16 | Shlq_s16 | Cmp_s16 _
  | Cmpz_s16 _ | Shlq_n_u16 _ | Shrq_n_u16 _ | Shrq_n_s16 _ | Getq_lane_s16 _
  | Setq_lane_s16 _ | Dupq_lane_s16 _ | Addq_s8 | Paddq_s8 | Qaddq_s8 | Qaddq_u8
  | Subq_s8 | Qsubq_s8 | Qsubq_u8 | Absq_s8 | Minq_s8 | Maxq_s8 | Minq_u8
  | Maxq_u8 | Mvnq_s8 | Orrq_s8 | Andq_s8 | Eorq_s8 | Negq_s8 | Cntq_u8
  | Shlq_u8 | Shlq_s8 | Cmp_s8 _ | Cmpz_s8 _ | Shlq_n_u8 _ | Shrq_n_u8 _
  | Shrq_n_s8 _ | Getq_lane_s8 _ | Setq_lane_s8 _ | Dupq_lane_s8 _
  | Copyq_laneq_s64 _ | Qmovn_high_s64 | Qmovn_s64 | Qmovn_high_s32 | Qmovn_s32
  | Qmovn_high_u32 | Qmovn_u32 | Qmovn_high_s16 | Qmovn_s16 | Qmovn_high_u16
  | Qmovn_u16 | Movn_high_s64 | Movn_s64 | Movn_high_s32 | Movn_s32
  | Movn_high_s16 | Movn_s16 | Mullq_s16 | Mullq_u16 | Mullq_high_s16
  | Mullq_high_u16 ->
    Pure

let operation_is_pure op = match class_of_operation op with Pure -> true
