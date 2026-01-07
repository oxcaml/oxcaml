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

(* Helper to extract Q (vector width) and size (element size) from vector
   type *)
let vector_q_size (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int * int =
  match vec with
  | V8B -> 0, 0b00 (* 64-bit, B elements *)
  | V16B -> 1, 0b00 (* 128-bit, B elements *)
  | V4H -> 0, 0b01 (* 64-bit, H elements *)
  | V8H -> 1, 0b01 (* 128-bit, H elements *)
  | V2S -> 0, 0b10 (* 64-bit, S elements *)
  | V4S -> 1, 0b10 (* 128-bit, S elements *)
  | V1D -> 0, 0b11 (* 64-bit, D elements *)
  | V2D -> 1, 0b11 (* 128-bit, D elements *)

(* Helper for FP vector operations - returns Q and the FP precision bit (sz) *)
let vector_q_fp_sz (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int * int
    =
  match vec with
  | V2S -> 0, 0 (* 64-bit, single-precision *)
  | V4S -> 1, 0 (* 128-bit, single-precision *)
  | V2D -> 1, 1 (* 128-bit, double-precision *)
  | V8B | V16B | V4H | V8H | V1D ->
    Misc.fatal_error "FP vector operations only support S and D element types"

(* Helper to compute imm5 for SIMD copy instructions (DUP, INS, SMOV, UMOV).
   imm5 encodes element size and lane index:

   - B: xxxx1 (index in bits 4:1)

   - H: xxx10 (index in bits 4:2)

   - S: xx100 (index in bits 4:3)

   - D: x1000 (index in bit 4) *)
let simd_copy_imm5 (type v s) (vec : (v, s) Neon_reg_name.Vector.t) lane_idx =
  match vec with
  | V8B | V16B -> (lane_idx lsl 1) lor 0b00001
  | V4H | V8H -> (lane_idx lsl 2) lor 0b00010
  | V2S | V4S -> (lane_idx lsl 3) lor 0b00100
  | V1D | V2D -> (lane_idx lsl 4) lor 0b01000

(* Helper to compute imm4 for INS (element) source index. imm4 encodes the
   source element index in the same size-dependent way as imm5, but with one
   fewer bit (4 bits instead of 5):

   - B: xxxx (index in bits 3:0)

   - H: xxx0 (index in bits 3:1)

   - S: xx00 (index in bits 3:2)

   - D: x000 (index in bit 3) *)
let simd_ins_element_imm4 (type v s) (vec : (v, s) Neon_reg_name.Vector.t)
    lane_idx =
  match vec with
  | V8B | V16B -> lane_idx
  | V4H | V8H -> lane_idx lsl 1
  | V2S | V4S -> lane_idx lsl 2
  | V1D | V2D -> lane_idx lsl 3

(* Advanced SIMD copy - C4.1.95.17 *)
(* Encoding: 0 Q op 01110 00 imm5 0 imm4 1 Rn Rd *)
let encode_simd_copy ~q ~op ~imm5 ~imm4 ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0, Q at bit 30, op at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int op) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  (* bits 23:22 = 00 *)
  let result = logor result (shift_left (of_int imm5) 16) in
  (* bit 15 = 0 *)
  let result = logor result (shift_left (of_int imm4) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD extract - C4.1.95.16 *)
(* Encoding: 0 Q 101110 op2 0 Rm 0 imm4 0 Rn Rd *)
let encode_simd_extract ~q ~rm ~imm4 ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int 0b101110) 24) in
  (* op2=00, bit 21=0 *)
  let result = logor result (shift_left (of_int rm) 16) in
  (* bit 15=0 *)
  let result = logor result (shift_left (of_int imm4) 11) in
  (* bit 10=0 *)
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD modified immediate - C4.1.95.19 *)
(* Encoding: 0 Q op 0111100000 a b c cmode 01 d e f g h Rd *)
(* For MOVI: op=0, cmode determines the variant *)
let encode_simd_modified_imm ~q ~op ~abc ~cmode ~defgh ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int op) 29) in
  let result = logor result (shift_left (of_int 0b0111100000) 19) in
  let result = logor result (shift_left (of_int abc) 16) in
  let result = logor result (shift_left (of_int cmode) 12) in
  let result = logor result (shift_left (of_int 0b01) 10) in
  let result = logor result (shift_left (of_int defgh) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD across lanes - C4.1.95.22 *)
(* Encoding: 0 Q U 01110 size 11000 opcode 10 Rn Rd *)
let encode_simd_across_lanes ~q ~u ~size ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b11000) 17) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD permute *)
(* Encoding: 0 Q 0 01110 size 0 Rm 0 opcode 10 Rn Rd *)
let encode_simd_permute ~q ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, bit 29 = 0 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  (* bit 21 = 0 *)
  let result = logor result (shift_left (of_int rm) 16) in
  (* bit 15 = 0 *)
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Helper for widening operations - returns source element size from dest type *)
(* For SMULL/UMULL: dest has 2x wider elements than source *)
let vector_widening_size (type v s) (vec : (v, s) Neon_reg_name.Vector.t) : int
    =
  match vec with
  | V8H -> 0b00 (* 16-bit dest -> 8-bit source *)
  | V4S -> 0b01 (* 32-bit dest -> 16-bit source *)
  | V2D -> 0b10 (* 64-bit dest -> 32-bit source *)
  | V8B | V16B | V4H | V2S | V1D ->
    Misc.fatal_error
      "Widening operations require H, S, or D destination elements"

(* Advanced SIMD two-register miscellaneous - C4.1.95.21 *)
let encode_simd_two_reg_misc ~q ~u ~size ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b10000) 17) in
  let result = logor result (shift_left (of_int opcode) 12) in
  let result = logor result (shift_left (of_int 0b10) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD three same - C4.1.95.24 *)
(* Encoding: 0 Q U 01110 size 1 Rm opcode 1 Rn Rd *)
let encode_simd_three_same ~q ~u ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* Bit 31 is always 0, Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD three different - C4.1.95.23 *)
(* Encoding: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd *)
let encode_simd_three_different ~q ~u ~size ~rm ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01110) 24) in
  let result = logor result (shift_left (of_int size) 22) in
  let result = logor result (shift_left (of_int 0b1) 21) in
  let result = logor result (shift_left (of_int rm) 16) in
  let result = logor result (shift_left (of_int opcode) 12) in
  (* bits 11-10 = 00 *)
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result

(* Advanced SIMD shift by immediate - C4.1.95.26 *)
(* Encoding: 0 Q U 01111 0 immh immb opcode 1 Rn Rd *)
(* immh encodes element size: 0001=8b, 001x=16b, 01xx=32b, 1xxx=64b *)
let encode_simd_shift_imm ~q ~u ~immh ~immb ~opcode ~rn ~rd =
  let open Int32 in
  let result = zero in
  (* bit 31 = 0 (implicit), Q at bit 30, U at bit 29 *)
  let result = logor result (shift_left (of_int q) 30) in
  let result = logor result (shift_left (of_int u) 29) in
  let result = logor result (shift_left (of_int 0b01111) 24) in
  (* bit 23 = 0 *)
  let result = logor result (shift_left (of_int immh) 19) in
  let result = logor result (shift_left (of_int immb) 16) in
  let result = logor result (shift_left (of_int opcode) 11) in
  let result = logor result (shift_left (of_int 0b1) 10) in
  let result = logor result (shift_left (of_int rn) 5) in
  let result = logor result (of_int rd) in
  result
