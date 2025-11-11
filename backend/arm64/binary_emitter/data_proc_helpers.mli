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

val encode_data_proc_2_source :
  sf:int ->
  s:int ->
  opcode:int ->
  rm:[`GP of 'a] Reg.t ->
  rn:[`GP of 'b] Reg.t ->
  rd:[`GP of 'c] Reg.t ->
  int32

val encode_data_proc_1_source :
  sf:int ->
  s:int ->
  opcode2:int ->
  opcode:int ->
  rn:[`GP of 'a] Reg.t ->
  rd:[`GP of 'b] Reg.t ->
  int32

val encode_data_proc_3_source :
  sf:int ->
  op54:int ->
  op31:int ->
  o0:int ->
  rm:[`GP of 'a] Reg.t ->
  ra:[`GP of 'b] Reg.t ->
  rn:[`GP of 'c] Reg.t ->
  rd:[`GP of 'd] Reg.t ->
  int32
