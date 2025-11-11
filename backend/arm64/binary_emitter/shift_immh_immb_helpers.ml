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

(* Helper to compute immh and immb for SHL instruction *)
(* For SHL: shift = (immh:immb) - element_width *)
(* So (immh:immb) = shift + element_width *)
let shl_immh_immb (type v s) (vec : (v, s) Neon_reg_name.Vector.t) shift =
  let esize, immh_base =
    match vec with
    | V8B | V16B -> 8, 0b0001
    | V4H | V8H -> 16, 0b0010
    | V2S | V4S -> 32, 0b0100
    | V1D | V2D -> 64, 0b1000
  in
  let immh_immb = shift + esize in
  let immh = (immh_immb lsr 3) lor immh_base in
  let immb = immh_immb land 0b111 in
  immh, immb

(* Helper to compute immh and immb for SSHR/USHR instructions *)
(* For SSHR/USHR: shift = element_width*2 - (immh:immb) *)
(* So (immh:immb) = element_width*2 - shift *)
let shr_immh_immb (type v s) (vec : (v, s) Neon_reg_name.Vector.t) shift =
  let esize =
    match vec with
    | V8B | V16B -> 8
    | V4H | V8H -> 16
    | V2S | V4S -> 32
    | V1D | V2D -> 64
  in
  let immh_immb = (esize * 2) - shift in
  let immh = immh_immb lsr 3 in
  let immb = immh_immb land 0b111 in
  immh, immb

(* Helper to compute immh for SXTL/UXTL (SSHLL/USHLL with shift=0) *)
(* immh encodes source element size: 0001=8b, 0010=16b, 0100=32b *)
(* Destination has 2x wider elements than source *)
let sxtl_immh (type v s) (vec : (v, s) Neon_reg_name.Vector.t) =
  match vec with
  | V8H -> 0b0001 (* 16-bit dest -> 8-bit source *)
  | V4S -> 0b0010 (* 32-bit dest -> 16-bit source *)
  | V2D -> 0b0100 (* 64-bit dest -> 32-bit source *)
  | V8B | V16B | V4H | V2S | V1D ->
    Misc.fatal_error "SXTL/UXTL requires H, S, or D destination elements"
