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

let encode_condition (cond : Cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | MI -> 0b0100
  | PL -> 0b0101
  | VS -> 0b0110
  | VC -> 0b0111
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101

(* Floating-point condition codes use the same encoding as integer conditions.
   The difference is semantic: after FCMP, the flags have different meanings. *)
let encode_float_condition (cond : Float_cond.t) : int =
  match cond with
  | EQ -> 0b0000
  | NE -> 0b0001
  | CS -> 0b0010
  | CC -> 0b0011
  | HI -> 0b1000
  | LS -> 0b1001
  | GE -> 0b1010
  | LT -> 0b1011
  | GT -> 0b1100
  | LE -> 0b1101
