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

(** Bind a fresh variable to the result of [Static_const.t], and map the symbol
    to this new variable in the environment, for "block-like" constants (i.e.
    not [Set_of_closures]).

    See [Static_const.match_against_bound_static_pattern] *)
val block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_env.t * To_jsir_result.t

(** For a block-like constant that is part of a cycle of static definitions,
    bind the symbol (which must already be in the environment) to a mutable
    block of the right tag and size, whose non-constant fields hold
    placeholders. [initialise_block_like] must later be called with the same
    arguments, once all the fields' values are defined. Raises for constants
    other than [Block] and [Immutable_value_array]. *)
val preallocate_block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_result.t

(** Set the non-constant fields of a block allocated by [preallocate_block_like]
    to their values. *)
val initialise_block_like :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  Static_const.t ->
  To_jsir_result.t

(** Prepare a static block of code to be translated: reserve its address and
    parameter variables, and add them and any used value or function slots to
    the environment. This does not emit any instructions (see [code_closure]).
*)
val prepare_code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  code_id:Code_id.t ->
  Flambda.function_params_and_body Code0.t ->
  To_jsir_env.t * To_jsir_result.t

(** If the code does not depend on its closure, bind its shared closure (see
    [To_jsir_env.code_id]) in the current block; otherwise, do nothing. The code
    must have been prepared with [prepare_code]. *)
val code_closure :
  env:To_jsir_env.t -> res:To_jsir_result.t -> Code_id.t -> To_jsir_result.t

(** Translate a static block of code. *)
val code :
  env:To_jsir_env.t ->
  res:To_jsir_result.t ->
  translate_body:
    (env:To_jsir_env.t ->
    res:To_jsir_result.t ->
    Flambda.expr ->
    To_jsir_env.t * To_jsir_result.t) ->
  code_id:Code_id.t ->
  Flambda.function_params_and_body Code0.t ->
  To_jsir_env.t * To_jsir_result.t
