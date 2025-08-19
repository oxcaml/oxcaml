(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

(** Bind a fresh JSIR variable to [expr], and map [fvar] to this new variable in the
    environment. *)
val bind_expr_to_var :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Variable.t ->
  Jsir.expr ->
  To_jsir_env.t * To_jsir_result.t

val bind_expr_to_var' :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Variable.t ->
  Jsir.expr ->
  Jsir.Var.t * To_jsir_env.t * To_jsir_result.t

(** Convert a Flambda [Reg_width_const.t] into a [Jsir.constant]. *)
val reg_width_const : Reg_width_const.t -> Jsir.constant

(** Convert a Flambda [Simple.t] into a [Jsir.Var.t], potentially by adding new
    instructions in the result. *)
val simple :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Simple.t ->
  Jsir.Var.t * To_jsir_result.t

val simples :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Simple.t list ->
  Jsir.Var.t list * To_jsir_result.t

(** Take in [Bound_parameters.t] and bind each parameter to fresh JSIR variables in the
    environment, and return the variables in order *)
val bound_parameters :
  env:To_jsir_env.t -> Bound_parameters.t -> Jsir.Var.t list * To_jsir_env.t

(** Make a new block. *)
val block :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  tag:Tag.t ->
  mut:Mutability.t ->
  fields:Simple.t list ->
  Jsir.expr * To_jsir_env.t * To_jsir_result.t
