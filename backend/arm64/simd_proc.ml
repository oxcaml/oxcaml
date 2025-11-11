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

(* Convert Simd.Cond.t to Arm64_ast.Cond.t *)
let simd_cond_to_ast_cond (c : Simd.Cond.t) : Arm64_ast.Cond.t =
  match c with EQ -> EQ | GE -> GE | GT -> GT | LE -> LE | LT -> LT

(* Convert Simd.Rounding_mode.t to Arm64_ast.Rounding_mode.t *)
let simd_rounding_to_ast_rounding (rm : Simd.Rounding_mode.t) :
    Arm64_ast.Rounding_mode.t =
  match rm with
  | Current -> X (* Use current FPCR mode *)
  | Neg_inf -> M
  | Pos_inf -> P
  | Zero -> Z
  | Nearest -> N

(* Types of operands for a SIMD instruction:

   - `V2S, `V4S, `V2D, `V8H, `V16B, etc: Vector register formats

   - `Reg: General scalar register (S/D for floats, W/X for ints)

   - `W: 32-bit general-purpose register

   - `Lane_B, `Lane_H, `Lane_S, `Lane_D: Lane-indexed vector registers *)
type _ operand_shape =
  (* Binary vector operations: (result * arg0 * arg1) *)
  | V2S_V2S_V2S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V4S_V4S_V4S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2D_V2D_V2D
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
        operand_shape
  | V16B_V16B_V16B
      : ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t)
        operand_shape
  (* EXT: three V16B operands plus immediate *)
  | V16B_V16B_V16B_imm6 :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Imm of [> `Six]] Arm64_ast.Operand.t)
         operand_shape
  | V8H_V8H_V8H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
        operand_shape
  | V4S_V8H_V8H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
        operand_shape
  | V4S_V4H_V4H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4H]]]] Arm64_ast.Operand.t)
        operand_shape
  (* Unary vector operations: (result * arg) *)
  | V4S_V4S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2D_V2S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2S_V2D_cvt
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
        operand_shape
  | V16B_V16B
      : ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2D_V2D
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2D_V2D_imm6 :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Imm of [> `Six]] Arm64_ast.Operand.t)
         operand_shape
  | V4S_V4S_imm6 :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
         * [> `Imm of [> `Six]] Arm64_ast.Operand.t)
         operand_shape
  | V8H_V8H_imm6 :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
         * [> `Imm of [> `Six]] Arm64_ast.Operand.t)
         operand_shape
  | V16B_V16B_imm6 :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Imm of [> `Six]] Arm64_ast.Operand.t)
         operand_shape
  | V8H_V8H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
        operand_shape
  | V2S_V2D_narrow
      : ([> `Reg of [> `Neon of [> `Vector of [> `V2S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
        operand_shape
  | V4H_V4S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V8B_V8H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V8B]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
        operand_shape
  | V4S_V4H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4H]]]] Arm64_ast.Operand.t)
        operand_shape
  | V8H_V8B
      : ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8B]]]] Arm64_ast.Operand.t)
        operand_shape
  (* Narrowing operations with "to_First": (result * new_data) Result is
     i.res.(0) which also contains first arg, second is i.arg.(1) *)
  | V4S_V2D
      : ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
        operand_shape
  | V8H_V4S
      : ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
        operand_shape
  | V16B_V8H
      : ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
        operand_shape
  (* Scalar operations: (result * arg0 * arg1) or (result * arg) *)
  (* CR mshinwell: figure out how to combine these into one case. Seems like
     there might be trouble with variance here? *)
  | RegD_RegD_RegD
      : (([> `Reg of [> `Neon of [> `Scalar of [> `D]]]] as 'r)
         Arm64_ast.Operand.t
        * 'r Arm64_ast.Operand.t
        * 'r Arm64_ast.Operand.t)
        operand_shape
  | RegS_RegS_RegS
      : (([> `Reg of [> `Neon of [> `Scalar of [> `S]]]] as 'r)
         Arm64_ast.Operand.t
        * 'r Arm64_ast.Operand.t
        * 'r Arm64_ast.Operand.t)
        operand_shape
  | RegD_RegD
      : ([> `Reg of [> `Neon of [> `Scalar of [> `D]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Scalar of [> `D]]]] Arm64_ast.Operand.t)
        operand_shape
  | RegS_RegS
      : ([> `Reg of [> `Neon of [> `Scalar of [> `S]]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Scalar of [> `S]]]] Arm64_ast.Operand.t)
        operand_shape
  | RegD_RegX
      : (([> `Reg of [> `Neon of [> `Scalar of [> `D]]]] as 'r)
         Arm64_ast.Operand.t
        * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t)
        operand_shape
  | RegS_RegX
      : (([> `Reg of [> `Neon of [> `Scalar of [> `S]]]] as 'r)
         Arm64_ast.Operand.t
        * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t)
        operand_shape
  | RegX_RegD
      : ([> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Scalar of [> `D]]]] Arm64_ast.Operand.t)
        operand_shape
  | RegX_RegS
      : ([> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
        * [> `Reg of [> `Neon of [> `Scalar of [> `S]]]] Arm64_ast.Operand.t)
        operand_shape
  (* Extract lane: (scalar_result * vector_arg_with_lane) *)
  | Reg_LaneB :
      int
      -> ([> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `B]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | Reg_LaneH :
      int
      -> ([> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `H]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | Reg_LaneS :
      int
      -> ([> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `S]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | Reg_LaneD :
      int
      -> ([> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  (* Sign-extend lane to X register (for SMOV) *)
  | RegX_V16B :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t)
         operand_shape
  | RegX_V8H :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
         operand_shape
  | RegX_V4S :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
         operand_shape
  (* Extract 64-bit lane to X register (for UMOV) *)
  | RegX_V2D :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
         operand_shape
  (* Insert lane from GP register: (lane_index * vector_result * gp_arg) *)
  | V16B_W :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | V8H_W :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | V4S_W :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | V2D_X :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t)
         operand_shape
  (* Copy lane to lane: (dst_lane * src_lane * dst_vector * src_vector) *)
  | V2D_V2D_lanes :
      int * int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
         operand_shape
  (* Insert lane: (vector_result_with_lane * scalar_arg) *)
  | LaneB_W :
      int
      -> ([> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `B]]]]]
          Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | LaneH_W :
      int
      -> ([> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `H]]]]]
          Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | LaneS_W :
      int
      -> ([> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `S]]]]]
          Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `W]]] Arm64_ast.Operand.t)
         operand_shape
  | LaneD_Reg :
      int
      -> ([> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]]
          Arm64_ast.Operand.t
         * [> `Reg of [> `GP of [> `X]]] Arm64_ast.Operand.t)
         operand_shape
  | LaneD_LaneD :
      int * int
      -> ([> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]]
          Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  (* Dup lane: (vector_result * vector_arg_with_lane) *)
  | V16B_LaneB :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `B]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | V8H_LaneH :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `H]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | V4S_LaneS :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `S]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  | V2D_LaneD :
      int
      -> ([> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Lane of [> `Scalar of [> `D]]]]]
           Arm64_ast.Operand.t)
         operand_shape
  (* DUP operand shapes: (Lane_index * dest_vector * src_vector) *)
  | DUP_V16B :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V16B]]]] Arm64_ast.Operand.t)
         operand_shape
  | DUP_V8H :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V8H]]]] Arm64_ast.Operand.t)
         operand_shape
  | DUP_V4S :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V4S]]]] Arm64_ast.Operand.t)
         operand_shape
  | DUP_V2D :
      int
      -> (Arm64_ast.Neon_reg_name.Lane_index.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t
         * [> `Reg of [> `Neon of [> `Vector of [> `V2D]]]] Arm64_ast.Operand.t)
         operand_shape

type simd_operation_with_operand_regs =
  | S :
      'a Arm64_ast.Instruction_name.t * 'a operand_shape
      -> simd_operation_with_operand_regs
  | Transformed_in_emit

(** Given a SIMD operation, return the (typed) instruction that corresponds
    to it, along with the expected operand shapes.  GADT witnesses tie these
    two values together. *)
let simd_operation_with_operand_regs (op : Simd.operation) :
    simd_operation_with_operand_regs =
  match[@warning "-4"] op with
  (* Vector floating-point arithmetic *)
  | Addq_f32 -> S (FADD_vector, V4S_V4S_V4S)
  | Addq_f64 -> S (FADD_vector, V2D_V2D_V2D)
  | Subq_f32 -> S (FSUB_vector, V4S_V4S_V4S)
  | Subq_f64 -> S (FSUB_vector, V2D_V2D_V2D)
  | Mulq_f32 -> S (FMUL_vector, V4S_V4S_V4S)
  | Mulq_f64 -> S (FMUL_vector, V2D_V2D_V2D)
  | Divq_f32 -> S (FDIV_vector, V4S_V4S_V4S)
  | Divq_f64 -> S (FDIV_vector, V2D_V2D_V2D)
  (* Vector integer arithmetic *)
  | Addq_s64 -> S (ADD_vector, V2D_V2D_V2D)
  | Addq_s32 -> S (ADD_vector, V4S_V4S_V4S)
  | Addq_s16 -> S (ADD_vector, V8H_V8H_V8H)
  | Addq_s8 -> S (ADD_vector, V16B_V16B_V16B)
  | Subq_s64 -> S (SUB_vector, V2D_V2D_V2D)
  | Subq_s32 -> S (SUB_vector, V4S_V4S_V4S)
  | Subq_s16 -> S (SUB_vector, V8H_V8H_V8H)
  | Subq_s8 -> S (SUB_vector, V16B_V16B_V16B)
  | Mulq_s32 -> S (MUL_vector, V4S_V4S_V4S)
  | Mulq_s16 -> S (MUL_vector, V8H_V8H_V8H)
  | Negq_s32 -> S (NEG_vector, V4S_V4S)
  | Negq_s64 -> S (NEG_vector, V2D_V2D)
  | Negq_s16 -> S (NEG_vector, V8H_V8H)
  | Negq_s8 -> S (NEG_vector, V16B_V16B)
  (* Vector min/max *)
  | Minq_f32 -> S (FMIN_vector, V4S_V4S_V4S)
  | Minq_f64 -> S (FMIN_vector, V2D_V2D_V2D)
  | Maxq_f32 -> S (FMAX_vector, V4S_V4S_V4S)
  | Maxq_f64 -> S (FMAX_vector, V2D_V2D_V2D)
  | Minq_s32 -> S (SMIN_vector, V4S_V4S_V4S)
  | Minq_s16 -> S (SMIN_vector, V8H_V8H_V8H)
  | Minq_s8 -> S (SMIN_vector, V16B_V16B_V16B)
  | Maxq_s32 -> S (SMAX_vector, V4S_V4S_V4S)
  | Maxq_s16 -> S (SMAX_vector, V8H_V8H_V8H)
  | Maxq_s8 -> S (SMAX_vector, V16B_V16B_V16B)
  | Minq_u32 -> S (UMIN_vector, V4S_V4S_V4S)
  | Minq_u16 -> S (UMIN_vector, V8H_V8H_V8H)
  | Minq_u8 -> S (UMIN_vector, V16B_V16B_V16B)
  | Maxq_u32 -> S (UMAX_vector, V4S_V4S_V4S)
  | Maxq_u16 -> S (UMAX_vector, V8H_V8H_V8H)
  | Maxq_u8 -> S (UMAX_vector, V16B_V16B_V16B)
  (* Vector logical operations *)
  | Orrq_s32 | Orrq_s64 | Orrq_s16 | Orrq_s8 -> S (ORR_vector, V16B_V16B_V16B)
  | Andq_s32 | Andq_s64 | Andq_s16 | Andq_s8 -> S (AND_vector, V16B_V16B_V16B)
  | Eorq_s32 | Eorq_s64 | Eorq_s16 | Eorq_s8 -> S (EOR_vector, V16B_V16B_V16B)
  | Mvnq_s32 | Mvnq_s64 | Mvnq_s16 | Mvnq_s8 -> S (MVN_vector, V16B_V16B)
  (* Vector absolute value and sqrt *)
  | Absq_s32 -> S (ABS_vector, V4S_V4S)
  | Absq_s64 -> S (ABS_vector, V2D_V2D)
  | Absq_s16 -> S (ABS_vector, V8H_V8H)
  | Absq_s8 -> S (ABS_vector, V16B_V16B)
  | Sqrtq_f32 -> S (FSQRT_vector, V4S_V4S)
  | Sqrtq_f64 -> S (FSQRT_vector, V2D_V2D)
  | Rsqrteq_f32 -> S (FRSQRTE_vector, V4S_V4S)
  | Rsqrteq_f64 -> S (FRSQRTE_vector, V2D_V2D)
  | Recpeq_f32 -> S (FRECPE_vector, V4S_V4S)
  (* Vector conversions *)
  | Cvtq_s32_f32 -> S (FCVTZS_vector, V4S_V4S)
  | Cvtq_s64_f64 -> S (FCVTZS_vector, V2D_V2D)
  | Cvtnq_s32_f32 -> S (FCVTNS_vector, V4S_V4S)
  | Cvtnq_s64_f64 -> S (FCVTNS_vector, V2D_V2D)
  | Cvtq_f32_s32 -> S (SCVTF_vector, V4S_V4S)
  | Cvtq_f64_s64 -> S (SCVTF_vector, V2D_V2D)
  | Cvt_f64_f32 -> S (FCVTL_vector, V2D_V2S)
  | Cvt_f32_f64 -> S (FCVTN_vector, V2S_V2D_cvt)
  (* Vector extend/narrow operations *)
  | Movl_s32 -> S (SXTL, V2D_V2S)
  | Movl_s16 -> S (SXTL, V4S_V4H)
  | Movl_s8 -> S (SXTL, V8H_V8B)
  | Movl_u32 -> S (UXTL, V2D_V2S)
  | Movl_u16 -> S (UXTL, V4S_V4H)
  | Movl_u8 -> S (UXTL, V8H_V8B)
  | Movn_s64 -> S (XTN, V2S_V2D_narrow)
  | Movn_s32 -> S (XTN, V4H_V4S)
  | Movn_s16 -> S (XTN, V8B_V8H)
  | Movn_high_s64 -> S (XTN2, V4S_V2D)
  | Movn_high_s32 -> S (XTN2, V8H_V4S)
  | Movn_high_s16 -> S (XTN2, V16B_V8H)
  | Qmovn_s64 -> S (SQXTN, V2S_V2D_narrow)
  | Qmovn_s32 -> S (SQXTN, V4H_V4S)
  | Qmovn_s16 -> S (SQXTN, V8B_V8H)
  | Qmovn_high_s64 -> S (SQXTN2, V4S_V2D)
  | Qmovn_high_s32 -> S (SQXTN2, V8H_V4S)
  | Qmovn_high_s16 -> S (SQXTN2, V16B_V8H)
  | Qmovn_u32 -> S (UQXTN, V4H_V4S)
  | Qmovn_u16 -> S (UQXTN, V8B_V8H)
  | Qmovn_high_u32 -> S (UQXTN2, V8H_V4S)
  | Qmovn_high_u16 -> S (UQXTN2, V16B_V8H)
  (* Vector pairwise operations *)
  | Paddq_f32 -> S (FADDP_vector, V4S_V4S_V4S)
  | Paddq_f64 -> S (FADDP_vector, V2D_V2D_V2D)
  | Paddq_s32 -> S (ADDP_vector, V4S_V4S_V4S)
  | Paddq_s64 -> S (ADDP_vector, V2D_V2D_V2D)
  | Paddq_s16 -> S (ADDP_vector, V8H_V8H_V8H)
  | Paddq_s8 -> S (ADDP_vector, V16B_V16B_V16B)
  (* Vector zip operations *)
  | Zip1_f32 -> S (ZIP1, V2S_V2S_V2S)
  | Zip1q_s8 -> S (ZIP1, V16B_V16B_V16B)
  | Zip1q_s16 -> S (ZIP1, V8H_V8H_V8H)
  | Zip1q_f32 -> S (ZIP1, V4S_V4S_V4S)
  | Zip1q_f64 -> S (ZIP1, V2D_V2D_V2D)
  | Zip2q_s8 -> S (ZIP2, V16B_V16B_V16B)
  | Zip2q_s16 -> S (ZIP2, V8H_V8H_V8H)
  | Zip2q_f32 -> S (ZIP2, V4S_V4S_V4S)
  | Zip2q_f64 -> S (ZIP2, V2D_V2D_V2D)
  (* Vector shift operations *)
  | Shlq_u32 -> S (USHL_vector, V4S_V4S_V4S)
  | Shlq_u64 -> S (USHL_vector, V2D_V2D_V2D)
  | Shlq_u16 -> S (USHL_vector, V8H_V8H_V8H)
  | Shlq_u8 -> S (USHL_vector, V16B_V16B_V16B)
  | Shlq_s32 -> S (SSHL_vector, V4S_V4S_V4S)
  | Shlq_s64 -> S (SSHL_vector, V2D_V2D_V2D)
  | Shlq_s16 -> S (SSHL_vector, V8H_V8H_V8H)
  | Shlq_s8 -> S (SSHL_vector, V16B_V16B_V16B)
  (* Vector multiply long *)
  | Mullq_s16 -> S (SMULL_vector, V4S_V4H_V4H)
  | Mullq_u16 -> S (UMULL_vector, V4S_V4H_V4H)
  | Mullq_high_s16 -> S (SMULL2_vector, V4S_V8H_V8H)
  | Mullq_high_u16 -> S (UMULL2_vector, V4S_V8H_V8H)
  (* Vector saturating arithmetic *)
  | Qaddq_s16 -> S (SQADD_vector, V8H_V8H_V8H)
  | Qaddq_s8 -> S (SQADD_vector, V16B_V16B_V16B)
  | Qaddq_u16 -> S (UQADD_vector, V8H_V8H_V8H)
  | Qaddq_u8 -> S (UQADD_vector, V16B_V16B_V16B)
  | Qsubq_s16 -> S (SQSUB_vector, V8H_V8H_V8H)
  | Qsubq_s8 -> S (SQSUB_vector, V16B_V16B_V16B)
  | Qsubq_u16 -> S (UQSUB_vector, V8H_V8H_V8H)
  | Qsubq_u8 -> S (UQSUB_vector, V16B_V16B_V16B)
  | Shlq_n_u64 n -> S (SHL, V2D_V2D_imm6 n)
  | Shlq_n_u32 n -> S (SHL, V4S_V4S_imm6 n)
  | Shlq_n_u16 n -> S (SHL, V8H_V8H_imm6 n)
  | Shlq_n_u8 n -> S (SHL, V16B_V16B_imm6 n)
  | Shrq_n_u64 n -> S (USHR, V2D_V2D_imm6 n)
  | Shrq_n_u32 n -> S (USHR, V4S_V4S_imm6 n)
  | Shrq_n_u16 n -> S (USHR, V8H_V8H_imm6 n)
  | Shrq_n_u8 n -> S (USHR, V16B_V16B_imm6 n)
  | Shrq_n_s64 n -> S (SSHR, V2D_V2D_imm6 n)
  | Shrq_n_s32 n -> S (SSHR, V4S_V4S_imm6 n)
  | Shrq_n_s16 n -> S (SSHR, V8H_V8H_imm6 n)
  | Shrq_n_s8 n -> S (SSHR, V16B_V16B_imm6 n)
  (* Sign-extend lane to X register *)
  | Getq_lane_s32 { lane } -> S (SMOV, RegX_V4S lane)
  | Getq_lane_s16 { lane } -> S (SMOV, RegX_V8H lane)
  | Getq_lane_s8 { lane } -> S (SMOV, RegX_V16B lane)
  (* Extract bytes from two vectors *)
  | Extq_u8 n -> S (EXT, V16B_V16B_V16B_imm6 n)
  (* Compare against zero *)
  | Cmpz_f32 c -> S (FCM_zero c, V4S_V4S)
  | Cmpz_f64 c -> S (FCM_zero c, V2D_V2D)
  | Cmpz_s32 c -> S (CM_zero (simd_cond_to_ast_cond c), V4S_V4S)
  | Cmpz_s64 c -> S (CM_zero (simd_cond_to_ast_cond c), V2D_V2D)
  | Cmpz_s16 c -> S (CM_zero (simd_cond_to_ast_cond c), V8H_V8H)
  | Cmpz_s8 c -> S (CM_zero (simd_cond_to_ast_cond c), V16B_V16B)
  (* Float vector compare - LT/LE should be transformed in simd_selection *)
  | Cmp_f32 ((EQ | GE | GT) as c) -> S (FCM_register c, V4S_V4S_V4S)
  | Cmp_f64 ((EQ | GE | GT) as c) -> S (FCM_register c, V2D_V2D_V2D)
  | Cmp_f32 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f32 LT/LE should be transformed in Simd_selection"
  | Cmp_f64 (LT | LE | NE | CC | CS | LS | HI) ->
    Misc.fatal_error "Cmp_f64 LT/LE should be transformed in Simd_selection"
  (* Scalar float min/max *)
  | Fmin_f32 -> S (FMIN, RegS_RegS_RegS)
  | Fmax_f32 -> S (FMAX, RegS_RegS_RegS)
  | Fmin_f64 -> S (FMIN, RegD_RegD_RegD)
  | Fmax_f64 -> S (FMAX, RegD_RegD_RegD)
  (* Scalar rounding *)
  | Round_f32 rm -> S (FRINT (simd_rounding_to_ast_rounding rm), RegS_RegS)
  | Round_f64 rm -> S (FRINT (simd_rounding_to_ast_rounding rm), RegD_RegD)
  (* Vector rounding *)
  | Roundq_f32 rm -> S (FRINT_vector (simd_rounding_to_ast_rounding rm), V4S_V4S)
  | Roundq_f64 rm -> S (FRINT_vector (simd_rounding_to_ast_rounding rm), V2D_V2D)
  (* Float to signed integer with rounding *)
  | Round_f32_s64 -> S (FCVTNS, RegX_RegS)
  | Round_f64_s64 -> S (FCVTNS, RegX_RegD)
  (* Integer vector compares. See comments in Simd_selection. *)
  | Cmp_s32 ((EQ | GE | GT) as c) ->
    S (CM_register (simd_cond_to_ast_cond c), V4S_V4S_V4S)
  | Cmp_s64 ((EQ | GE | GT) as c) ->
    S (CM_register (simd_cond_to_ast_cond c), V2D_V2D_V2D)
  | Cmp_s16 ((EQ | GE | GT) as c) ->
    S (CM_register (simd_cond_to_ast_cond c), V8H_V8H_V8H)
  | Cmp_s8 ((EQ | GE | GT) as c) ->
    S (CM_register (simd_cond_to_ast_cond c), V16B_V16B_V16B)
  | Cmp_s32 (LT | LE) | Cmp_s64 (LT | LE) | Cmp_s16 (LT | LE) | Cmp_s8 (LT | LE)
    ->
    Transformed_in_emit
  (* Lane operations - extract 64-bit lane *)
  | Getq_lane_s64 { lane } -> S (UMOV, RegX_V2D lane)
  (* Lane operations - insert from GP register *)
  | Setq_lane_s8 { lane } -> S (INS, V16B_W lane)
  | Setq_lane_s16 { lane } -> S (INS, V8H_W lane)
  | Setq_lane_s32 { lane } -> S (INS, V4S_W lane)
  | Setq_lane_s64 { lane } -> S (INS, V2D_X lane)
  (* Copy lane to lane *)
  | Copyq_laneq_s64 { src_lane; dst_lane } ->
    S (INS_V, V2D_V2D_lanes (src_lane, dst_lane))
  (* Duplicate lane *)
  | Dupq_lane_s8 { lane } -> S (DUP, DUP_V16B lane)
  | Dupq_lane_s16 { lane } -> S (DUP, DUP_V8H lane)
  | Dupq_lane_s32 { lane } -> S (DUP, DUP_V4S lane)
  | Dupq_lane_s64 { lane } -> S (DUP, DUP_V2D lane)
  (* Population count - CNT only works on 8-bit elements *)
  | Cntq_u8 -> S (CNT_vector, V16B_V16B)
  | Cntq_u16 | Min_scalar_f32 | Min_scalar_f64 | Max_scalar_f32 | Max_scalar_f64
    ->
    Transformed_in_emit
