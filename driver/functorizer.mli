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
(** Read a [unit_info] from a .cmi file. Used for the [-functorize-intf] path.
    The [mb_runtime_params] ordering in the returned [ui_format] is derived from
    the interface and need not match the canonical runtime ordering; it is only
    used for dependency discovery and CMI type generation. *)

val functorize_intf :
  srcs:Misc.filepath list ->
  Misc.filepath ->
  read_unit_info:(Misc.filepath -> unit_info) ->
  find_unit_info_by_name:(string -> unit_info) ->
  unit
(** [functorize_intf ~srcs target ~read_unit_info ~find_unit_info_by_name] reads
    the given .cmi files, collects all parameterised transitive dependencies,
    and writes a bundle-functor .cmi to [target]. [read_unit_info] and
    [find_unit_info_by_name] both read from .cmi files. *)

val functorize_impl :
  srcs:Misc.filepath list ->
  Misc.filepath ->
  read_unit_info:(Misc.filepath -> unit_info) ->
  find_unit_info_by_name:(string -> unit_info) ->
  compile:
    (source_file:Misc.filepath ->
    output_prefix:string ->
    compilation_unit:CU.t ->
    all_params:Global_module.t list ->
    modules:(CU.t * Lambda.main_module_block_format) list ->
    unit) ->
  unit
(** [functorize_impl ~srcs target ~read_unit_info ~find_unit_info_by_name
     ~compile] reads the given .cmx/.cmo files, collects all parameterised
    transitive dependencies, and compiles a bundle-functor .cmx/.cmo to
    [target]. [read_unit_info] reads from .cmx/.cmo files so that
    [mb_runtime_params] comes directly from the compiled objects with the
    correct canonical ordering. *)
