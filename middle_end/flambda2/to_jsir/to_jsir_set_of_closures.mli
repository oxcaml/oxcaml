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

open! Jsoo_imports.Import

(** Translate a dynamically allocated [Set_of_closures.t] into a JSIR [Closure].
*)
val dynamic_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  bound_vars:Bound_var.t list ->
  Set_of_closures.t ->
  To_jsir_env.t * To_jsir_result.t

type initialisation = private
  { value_slot_bindings : To_jsir_result.var_binding list;
        (** Bind value-slot variables to the values captured by the closures. *)
    closures : To_jsir_shared.expr_binding list;
        (** Bind function variables to their corresponding [Jsir.Closure]
            expressions. *)
    aliases : To_jsir_result.var_binding list
        (** Bind symbol and function-slot variables to their corresponding
            function variables. *)
  }

(** Prepare a static set of closures without emitting its bindings. *)
val prepare_static_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  closure_symbols:Symbol.t Function_slot.Lmap.t ->
  Set_of_closures.t ->
  To_jsir_result.t * initialisation
