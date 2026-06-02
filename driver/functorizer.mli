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

type bundle_module = { bm_id : Ident.t; bm_sign : Types.signature }

type rev_bundle = bundle_module list
(** Under-construction list of instantiated modules, in *reverse* topo order:
    newest item at the head, deepest dep at the tail. This makes [insert_module]
    insertions O(1). Consumers should reverse this once to obtain topo order.
    Each module's signature has had its [bound_globals] substituted to reference
    earlier modules / parameters. *)

module Bindings : Map.S with type key = string

type bindings = Ident.t Bindings.t
(** Records, for every globally-named compunit already accounted for, the Local
    Ident it is bound to — either a functor parameter or a bundled module. Plain
    (non-parameterised) compunits never go into the bundle and so never appear
    here; they remain global references at link time. Parameter declaration
    order is the caller's responsibility. *)

val empty_rev_bundle : rev_bundle
val empty_bindings : bindings

val insert_module :
  name:string ->
  Signature_with_global_bindings.t ->
  rev_bundle:rev_bundle ->
  bindings:bindings ->
  Ident.t * rev_bundle * bindings
(** [insert_module ~name swg ~rev_bundle ~bindings] returns the fresh Local
    Ident for [name], the extended [rev_bundle] (newest item consed at the head
    — caller must [List.rev] to obtain topo order), and the extended [bindings].
    The caller must pre-register every parameter the bundle exposes in
    [bindings] before calling; the helper rejects unknown [Parameter] compunits
    in [bound_globals].

    Each entry in [swg.bound_globals] is resolved by:
    - reusing the existing entry in [bindings] (a previously-inserted module,
      the new module's own placeholder, or a caller-registered parameter), or
    - loading the dep's cmi and either:
    - inserting it recursively as a new dependency module (parameterised case);
    - failing (parameter compunit, caller bug);
    - leaving the reference as a global (plain compunit). *)

val insert_instantiation :
  name:string ->
  find_impl_unit_info_by_name:(string -> impl_unit_info) ->
  instantiations:Translmod.instantiation list ->
  bindings:bindings ->
  Ident.t * Translmod.instantiation list * bindings
(** Impl-side analogue of [insert_module]. Idempotent: if [name] is already in
    [bindings], returns the existing Ident. Otherwise looks up [name]'s
    [impl_unit_info] via the callback, recursively inserts any
    [Rp_main_module_block] runtime dep, and appends a fresh
    [Translmod.instantiation]. The caller must pre-register every functor
    parameter ident in [bindings] under the parameter's name. *)

type bundle_sig = {
  body : Types.signature;
      (** [Sig_module] entries for each bundled module, in topo order. *)
  param_ids : Ident.t list;
      (** Local idents for the functor's parameters, in declaration order. *)
  modules : intf_unit_info list;
      (** Metadata for each bundled module, in topo order. *)
}

val compute_bundle_sig : intf_unit_info list -> bundle_sig
(** Build the bundle's body signature from a set of input cmis. Each input must
    be a parameterised compunit; transitive parameterised deps are pulled in via
    [insert_module]. Does not wrap the result in any [Mty_functor] layers —
    that's [compute_bundle_functor_type]'s job. *)

type bundle_signature = {
  signature : Types.signature;
      (** Two-element signature for the bundle's saved cmi:
          [module Intf (P1)...(Pn) : sig module type S = sig <body> end end]
          [module Make (P1)...(Pn) () : Intf(P1)...(Pn).S] *)
  intf_id : Ident.t;  (** Local Ident for the [Intf] module. *)
  make_id : Ident.t;  (** Local Ident for the [Make] module. *)
  all_params : Global_module.t list;
  all_modules : intf_unit_info list;
      (** Topologically sorted: every module's deps appear before it. *)
}

val compute_bundle_signature : intf_unit_info list -> bundle_signature
(** Build the bundle's saved-cmi signature. Wraps [compute_bundle_sig]'s body
    into two siblings: [Intf] (an applicative functor whose result type contains
    [module type S = sig <body> end]) and [Make] (a generative functor returning
    [Intf(Config).S]). [Intf] has no runtime computation; [Make] holds the
    actual code-generation. *)

val make_compilation_unit : Misc.filepath -> CU.t
(** Derive the bundle's compilation unit from the output target's basename. *)

val functorize_intf : Env.t -> Misc.filepath list -> Misc.filepath -> unit
(** Backend-agnostic interface mode for [-functorize]. Inputs are .cmi files;
    outputs are .cmi + .cmti + .cmsi. The bundle's compilation unit is derived
    from [target]'s basename. *)

val functorize_impl_with :
  initial_env:Env.t ->
  info:Compile_common.info ->
  input_files:Misc.filepath list ->
  read_unit_info_of_input:(Misc.filepath -> impl_unit_info) ->
  find_impl_unit_info_by_name:(string -> impl_unit_info) ->
  compile_program:(Compile_common.info -> Lambda.program -> unit) ->
  unit
(** Backend-agnostic [-functorize] implementation flow. Runs phase A (scan +
    signature from cmis), invokes the typing-side cmi save/check, then runs
    phase B (gather runtime layout from cmo/cmx) and hands the resulting Lambda
    program to the backend's [compile_program] callback. Must be called inside a
    [with_info] callback so that [Env.import_cmi_for_link] writes to the
    bundle's persistent env. *)
