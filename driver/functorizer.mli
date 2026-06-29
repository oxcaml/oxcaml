(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

(** [-functorize] driver: bundle parameterised modules into a single functor. *)

val validate_inputs : string list -> Compilation_unit.Name.Set.t
(** Reject empty input lists and duplicates. *)

val interface : Compilation_unit.Name.Set.t -> Misc.filepath -> unit
(** Write a [.cmi] (+ [.cmti] + [.cmsi]) for the bundle. *)

val implementation :
  Compilation_unit.Name.Set.t ->
  find_impl_by_name:(Compilation_unit.t -> Lambda.main_module_block_format) ->
  compile_program:(Compile_common.info -> Lambda.program -> unit) ->
  Compile_common.info ->
  unit
(** Write a [.cmo]/[.cmx] (+ [.cmi]/[.cmt]/[.cms]) for the bundle. Must be
    called inside [Compile_common.with_info]'s callback. *)
