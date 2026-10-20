(******************************************************************************
 *                                  OxCaml                                    *
 *                                Jane Street                                 *
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

(** Translation of external (C) calls to Cmm, including passing and returning
    unboxed products as C structs.

    Unboxed products that the C ABI would pass in memory cannot currently be
    compiled; [translate_external_call] reports a located error for them. *)

val translate_external_call :
  To_cmm_env.t ->
  To_cmm_result.t ->
  free_vars:To_cmm_env.free_vars ->
  Apply_expr.t ->
  callee_simple:Simple.t option ->
  args:Cmm.expression list ->
  return_arity:[`Complex] Flambda_arity.t ->
  return_ty:Cmm_helpers.Extended_machtype.t ->
  Debuginfo.t ->
  needs_caml_c_call:bool ->
  is_c_builtin:bool ->
  effects:Effects.t ->
  coeffects:Coeffects.t ->
  Cmm.expression
  * To_cmm_env.free_vars
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t
