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

module Or_missing : sig
  type 'a t =
    | Present of 'a
    | Missing
end

type closure

type halves =
  { slv_comptime : value Or_missing.t;
    slv_runtime : lambda
  }

and value =
  | SLVhalves of halves
  | SLVlayout of layout
  | SLVrecord of value Or_missing.t array
  | SLVclosure of closure

module CU_data : sig
  type t

  type raw

  val write : t -> sections:File_sections.Builder.t -> raw

  val read : raw -> sections:File_sections.t -> t

  val package : t array -> t

  val print : Format_doc.formatter -> t -> unit
end

val eval :
  cu_static_data:(Compilation_unit.t -> CU_data.t option) ->
  slambda ->
  CU_data.t * lambda

val print : Format_doc.formatter -> value Or_missing.t -> unit
