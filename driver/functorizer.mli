(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2024 Jane Street Group LLC                                  *
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

module CU := Compilation_unit

type unit_info = { ui_unit : CU.t; ui_format : Lambda.main_module_block_format }

val read_unit_info_of_cmi : Misc.filepath -> unit_info
(** Read a [unit_info] from a .cmi file for topology traversal. The
    [mb_runtime_params] ordering in the returned [ui_format] is derived from the
    interface and need not match the canonical runtime ordering; it is only used
    for dependency discovery. *)

val collect_all_modules :
  find_unit_info_by_name:(string -> unit_info) ->
  unit_info list ->
  unit_info list
(** Collect all modules (inputs + transitive parameterized deps) in topological
    order so that every module's dependencies appear before it. *)

val collect_all_params : unit_info list -> Global_module.t list
(** Collect all unique parameter globals across all modules, preserving the
    order in which they are first encountered. *)
