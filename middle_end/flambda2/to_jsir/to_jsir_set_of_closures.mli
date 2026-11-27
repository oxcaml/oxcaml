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

type translate_body :=
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Flambda.expr ->
  To_jsir_env.t * To_jsir_result.t

(** Translate a dynamically allocated [Set_of_closures.t]. *)
val dynamic_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  translate_body:translate_body ->
  bound_vars:Bound_var.t list ->
  Set_of_closures.t ->
  To_jsir_env.t * To_jsir_result.t

type instance

type static = private
  { instances : instance list;
        (** Closure instances to translate and bind, excluding shared closures.
        *)
    aliases : To_jsir_result.var_binding list
        (** Aliases mapping each closure symbol's variable to its closure
            variable. *)
  }

(** Reserve closures for a static set of closures and record its slots in the
    environment without translating code. The caller must subsequently run
    [translate_instances] and [bind_instances] on [instances] and register
    [aliases]. *)
val prepare_static_set_of_closures :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  closure_symbols:Symbol.t Function_slot.Lmap.t ->
  Set_of_closures.t ->
  To_jsir_env.t * To_jsir_result.t * static

val translate_instances :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  translate_body:translate_body ->
  instance list ->
  To_jsir_result.t

(** Emit [Jsir.Closure] bindings for the given instances. Mutually recursive
    closures must be bound consecutively, as required by JSOO. *)
val bind_instances : res:To_jsir_result.t -> instance list -> To_jsir_result.t

(** Value slots projected by the set's code. *)
val used_value_slots :
  env:To_jsir_env.t -> Set_of_closures.t -> Value_slot.Set.t

(** The locations of the closures of a statically allocated [Set_of_closures.t],
    for its closure symbols. Closures without a location are left out. *)
val closure_symbol_locs :
  env:To_jsir_env.t ->
  closure_symbols:Symbol.t Function_slot.Lmap.t ->
  Set_of_closures.t ->
  Parse_info.t Symbol.Map.t
