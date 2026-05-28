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

open Lambda

type value_halves = Slambdaeval.halves =
  { slv_comptime : Slambdaeval.value Slambdaeval.Or_missing.t;
    slv_runtime : lambda
  }

(** [eval ~cu_static_data inspect tlambda] fractures [tlambda] into [slambda],
    passes it through [inspect], then evaluates it. It returns a [value_halves]
    so that the caller can save/manipulate the compile-time part of the value
    represented by the [tlambda].

    [inspect] can arbitrarily modify the [slambda] but it's expected to be used
    by drivers to print the slambda if requested and return it unchanged.

    [cu_static_data] is invoked when evaluation encounters a reference to a
    static global from another compilation unit; it should return the
    compile-time value associated with that unit. *)
val eval :
  cu_static_data:
    (Compilation_unit.t -> Slambdaeval.value Slambdaeval.Or_missing.t) ->
  (slambda -> slambda) ->
  lambda ->
  value_halves
