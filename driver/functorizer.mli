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

type intf_unit_info = {
  ui_unit : CU.t;
  ui_params : Global_module.Parameter_name.t list;
  ui_deps : Global_module.t list;
}
(** Cmi-derived metadata sufficient for phase-1 (intf-mode) functorization:
    compilation unit name, declared parameters, and signature-level
    parameterized-module deps. *)

type impl_unit_info = {
  intf : intf_unit_info;
  ui_format : Lambda.main_module_block_format;
}
(** Phase-2 (impl-mode) extension of [intf_unit_info] adding the runtime calling
    convention read from the cmo/cmx. *)

val read_intf_unit_info_of_cmi : Misc.filepath -> intf_unit_info
(** Phase-1 reader. Loads the cmi via [Env.import_cmi_for_link] so the bundle's
    saved cmi lists it as an import. *)

val impl_unit_info_with_cmi_data :
  ui_unit:CU.t -> ui_format:Lambda.main_module_block_format -> impl_unit_info
(** Phase-2 helper: given a [.cmo]/[.cmx]'s compilation unit and runtime format,
    locate the matching .cmi on the load path, load it via
    [Env.import_cmi_for_link], and assemble the full [impl_unit_info] with
    declared parameters and signature-level deps populated. Must be called
    inside the [implementation_aux] callback so the [Env] state persists into
    the bundle's type-check and save. *)

val collect_all_modules :
  find_unit_info_by_name:(string -> 'a) ->
  get_intf:('a -> intf_unit_info) ->
  'a list ->
  'a list
(** Collect all modules (inputs + transitive parameterized deps) in topological
    order so that every module's dependencies appear before it. Parameterised
    over the input type, with [get_intf] extracting the [intf_unit_info] needed
    for topology. *)

val collect_all_params : intf_unit_info list -> Global_module.t list
(** Collect all unique parameter globals across all modules, preserving the
    order in which they are first encountered. *)

type bundle_sig = {
  signature : Types.signature;
  all_params : Global_module.t list;
  all_modules : intf_unit_info list;
      (** Topologically sorted: every module's deps appear before it. *)
}

val compute_bundle_sig :
  find_unit_info_by_name:(string -> intf_unit_info) ->
  intf_unit_info list ->
  bundle_sig
(** Phase A of [-functorize]: build the bundle's signature purely from cmi data.
    Walks transitive parameterised deps via [find_unit_info_by_name], collects
    the union of declared parameters across all modules, and produces a
    functor-wrapped signature. Reads only cmis (no cmo/cmx). The returned
    [all_modules] is the topologically sorted list the code-gen phase should
    iterate to instantiate each module in dependency order. *)

val make_compilation_unit : Misc.filepath -> CU.t
(** Derive the bundle's compilation unit from the output target's basename. *)

val find_unit_info_by_name_cmi : string -> intf_unit_info
(** Locate a module's .cmi by name on the load path and load it. *)

val functorize_intf : Env.t -> Misc.filepath list -> Misc.filepath -> unit
(** Backend-agnostic interface mode for [-functorize]. Inputs are .cmi files;
    outputs are .cmi + .cmti + .cmsi. The bundle's compilation unit is derived
    from [target]'s basename. *)

val functorize_impl_with :
  initial_env:Env.t ->
  info:Compile_common.info ->
  input_files:Misc.filepath list ->
  read_unit_info_of_input:(Misc.filepath -> impl_unit_info) ->
  find_intf_unit_info_by_name:(string -> intf_unit_info) ->
  find_impl_unit_info_by_name:(string -> impl_unit_info) ->
  compile_program:(Compile_common.info -> Lambda.program -> unit) ->
  unit
(** Backend-agnostic [-functorize] implementation flow. Runs phase A (scan +
    signature from cmis), invokes the typing-side cmi save/check, then runs
    phase B (gather runtime layout from cmo/cmx) and hands the resulting Lambda
    program to the backend's [compile_program] callback. Must be called inside a
    [with_info] callback so that [Env.import_cmi_for_link] writes to the
    bundle's persistent env. *)
