(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(* Keep 64 bits while composing identities; truncate only for serialization. *)
type t = int64

let of_int = Int64.of_int

(* BLAKE2 has a fixed specification, unlike [String.hash]. Read the first 8 of
   its 16 bytes little-endian, so the result does not depend on the host. *)
let of_string s = String.get_int64_le (Digest.BLAKE128.string s) 0

(* MurmurHash3's 64-bit finalizer (fmix64). *)
let mix x =
  let x = Int64.logxor x (Int64.shift_right_logical x 33) in
  let x = Int64.mul x 0xff51afd7ed558ccdL in
  let x = Int64.logxor x (Int64.shift_right_logical x 33) in
  let x = Int64.mul x 0xc4ceb9fe1a85ec53L in
  Int64.logxor x (Int64.shift_right_logical x 33)

(* Mix every combination: without it, [combine a (combine b c)] would equal
   [combine (a + b) c], since both are [(a + b) * p + c]. *)
let combine a b =
  (* [p] is the 64-bit golden ratio (2^64 / phi) *)
  let p = 0x9e3779b97f4a7c15L in
  mix (Int64.add (Int64.mul a p) b)

let node ~tag components = List.fold_left combine (of_int tag) components

(* The low 32 bits after mixing. *)
let to_int32 x = Int64.to_int32 (mix x)
