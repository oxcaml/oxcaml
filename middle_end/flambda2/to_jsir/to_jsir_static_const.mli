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

(** Bind a fresh variable to the result of [Static_const.t], and map the symbol
    to this new variable in the environment, for "block-like" constants (i.e.
    not [Set_of_closures]).

    See [Static_const.match_against_bound_static_pattern] *)
val block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_result.t

(** For a block-like constant that is part of a cycle of static definitions,
    bind the symbol (which must already be in the environment) to a mutable
    block of the right tag and size. Fields naming a symbol in
    [symbols_in_cycle] (the symbols defined by the cycle) hold placeholders,
    while all other fields are filled in. [initialise_block_like] must later be
    called with the same arguments, once all the fields' values are defined.
    Raises for constants other than [Block] and [Immutable_value_array]. *)
val preallocate_block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  symbols_in_cycle:Symbol.Set.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_result.t

(** Set the placeholder fields of a block allocated by [preallocate_block_like]
    to their values. *)
val initialise_block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  symbols_in_cycle:Symbol.Set.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_result.t

(** Register a static block of code in the environment without emitting
    instructions. Reserves a single closure if the code is [To_jsir_env.Shared],
    and otherwise stores the [Code.t] as [To_jsir_env.Instantiated]. *)
val prepare_code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  code_id:Code_id.t ->
  Code.t ->
  To_jsir_env.t * To_jsir_result.t

(** Reserve a fresh block address, parameter variables, and closure variable for
    a closure of the given code. The caller must later translate its body with
    [code] and bind its variable to [closure_expr]. *)
val new_closure :
  res:To_jsir_result.t -> Code.t -> To_jsir_env.closure * To_jsir_result.t

val closure_expr : To_jsir_env.closure -> Jsir.expr

val closure_loc : Code.t -> Parse_info.t option

(** Translate [code] into the block reserved by [closure]. *)
val code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  translate_body:
    (env:To_jsir_env.t ->
    res:To_jsir_result.t ->
    Flambda.expr ->
    To_jsir_env.t * To_jsir_result.t) ->
  closure:To_jsir_env.closure ->
  Code.t ->
  To_jsir_result.t
