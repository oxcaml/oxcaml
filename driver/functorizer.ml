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

module CU = Compilation_unit
module GM = Global_module
open Lambda

type intf_unit_info = {
  ui_unit : CU.t;
  ui_params : GM.Parameter_name.t list;
  ui_deps : GM.t list;
}

type impl_unit_info = {
  intf : intf_unit_info;
  ui_format : main_module_block_format;
}

(* Build [intf_unit_info] from a loaded cmi.  Reads [ui_deps] from the
   signature-with-globals' [bound_globals] (the modules referenced by [sign]
   that need substitution), not from the cmi's raw [cmi_globals] field.
   Fails if the cmi is a parameter module — those aren't valid inputs to
   functorization. *)
let intf_unit_info_of_loaded_cmi ~cmi_file (loaded : Persistent_env.loaded_cmi)
    : intf_unit_info =
  let ui_unit =
    match loaded.cu with
    | Some cu -> cu
    | None ->
        Misc.fatal_errorf "cannot functorize a parameter module: %s" cmi_file
  in
  let self_name = CU.name_as_string ui_unit in
  let ui_deps =
    Array.to_list loaded.sign_with_globals.bound_globals
    |> List.filter_map (fun (gm, _prec) ->
        if String.equal gm.GM.head self_name then None else Some gm)
  in
  { ui_unit; ui_params = loaded.params; ui_deps }

(* Phase-1 (intf mode) reader: builds [intf_unit_info] from a .cmi.  Loads via
   [Env.import_cmi_for_link] so the cmi is registered as an import of the
   bundle. *)
let read_intf_unit_info_of_cmi (cmi_file : string) : intf_unit_info =
  Env.import_cmi_for_link cmi_file |> intf_unit_info_of_loaded_cmi ~cmi_file

(* Phase-2 helper: given a [.cmo]/[.cmx]'s name and runtime format, locate the
   matching .cmi on the load path, load it via [Env.import_cmi_for_link], and
   assemble the full [impl_unit_info]. *)
let impl_unit_info_with_cmi_data ~(ui_unit : CU.t)
    ~(ui_format : main_module_block_format) : impl_unit_info =
  let cmi_file =
    Load_path.find_normalized
      (String.uncapitalize_ascii (CU.name_as_string ui_unit) ^ ".cmi")
  in
  let intf =
    Env.import_cmi_for_link cmi_file |> intf_unit_info_of_loaded_cmi ~cmi_file
  in
  (* The compilation unit from the cmo/cmx is canonical (it may include a
     pack prefix that the cmi's [cmi_impl] also has).  We trust the caller's
     [ui_unit] here. *)
  let intf = { intf with ui_unit } in
  { intf; ui_format }

(* ----- Bundle insertion helper -----

   A [bundle] is the under-construction list of instantiated modules, in
   topological order (deps appear before users).  Each module's signature
   has already had its [bound_globals] substituted to reference earlier
   modules / parameters.

   The companion [bindings] map records, for every globally-named compunit
   already accounted for, the Local Ident it is bound to — either as a
   functor parameter or as a bundled module.  Plain (non-parameterised)
   compunits never go into the bundle and so never appear in [bindings];
   they remain global references at link time.

   Parameter declaration order is the caller's responsibility: when the
   caller wraps the resulting bundle in functor layers, it picks the order
   in which parameter idents in [bindings] (those whose names don't appear
   in the bundle as modules) become functor parameters. *)

type bundle_module = {
  bm_id : Ident.t;
  bm_sign : Types.signature;
}

type bundle = bundle_module list

module Bindings = Misc.Stdlib.String.Map

type bindings = Ident.t Bindings.t

let empty_bundle : bundle = []

let empty_bindings : bindings = Bindings.empty

(* Load the cmi for compunit [name] via [Env.import_cmi_for_link]. *)
let load_cmi name : Persistent_env.loaded_cmi =
  let cmi_file =
    Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
  in
  Env.import_cmi_for_link cmi_file

let classify_cmi (loaded : Persistent_env.loaded_cmi) =
  match loaded.cu with
  | None -> `Parameter
  | Some _ -> if loaded.params = [] then `Plain else `Parameterised

(* Insert [swg] into [bundle] as a module declaration named [name].

   The caller is responsible for pre-registering every parameter the bundle
   should expose; this helper rejects unknown [Parameter] compunits in
   [bound_globals] rather than guessing an order.

   To break cycles in [bound_globals] (e.g. [-no-alias-deps] approximations
   that produce mutual cmi-globals references), [name]'s Local Ident is
   recorded in [bindings] up-front, before processing [bound_globals].

   For each entry in [swg.bound_globals]:
     - if its head is already in [bindings] (an earlier insert, the new
       module itself, an ancestor on the recursion stack, or a
       caller-registered parameter), the existing Ident is reused;
     - otherwise the dep's cmi is loaded:
         * [Parameterised]: [insert_module] is invoked recursively;
         * [Parameter]: error — caller must have added it;
         * [Plain]: no substitution — the reference stays as a global.

   The collected substitution is applied to [swg]'s signature, then the new
   entry is appended to [bundle]. *)
let rec insert_module ~name (swg : Signature_with_global_bindings.t) ~bundle
    ~bindings : bundle * bindings =
  if Bindings.mem name bindings
  then
    Misc.fatal_errorf "Functorizer.insert_module: %s already in bundle" name;
  let new_id = Ident.create_local name in
  let bindings = Bindings.add name new_id bindings in
  let loaded = load_cmi name in
  (* First pass: walk [bound_globals] for its side-effect — insert any
     missing parameterised dep modules into the bundle and grow [bindings]
     accordingly. *)
  let process_bound (bundle, bindings) ((gm, _prec) : GM.With_precision.t) =
    let head = gm.GM.head in
    if Bindings.mem head bindings then bundle, bindings
    else
      let loaded = load_cmi head in
      match classify_cmi loaded with
      | `Plain -> bundle, bindings
      | `Parameter ->
        Misc.fatal_errorf
          "Functorizer.insert_module: parameter %s not pre-registered in \
           bindings (the caller must add parameters before calling \
           insert_module)"
          head
      | `Parameterised ->
        insert_module ~name:head loaded.sign_with_globals ~bundle ~bindings
  in
  let bundle, bindings =
    Array.fold_left process_bound (bundle, bindings) swg.bound_globals
  in
  (* The signature can reference exactly two flavours of global compunit:
       - heads of [bound_globals] entries — other parameterised modules
         this one depends on;
       - the compunit's own declared parameters ([cmi_params]) — these
         may appear only nested inside [bound_globals] entries (e.g.,
         [P] as the [{P}] in [Basic{P}]) and so wouldn't be picked up by
         iterating [bound_globals] alone.
     Build subst entries for both.  Plain compunits aren't in [bindings];
     skipping them leaves their references as globals (correct). *)
  let add_subst_for head s =
    match Bindings.find_opt head bindings with
    | None -> s
    | Some id ->
      let name_id = Ident.create_global (GM.Name.create_no_args head) in
      Subst.add_module name_id (Path.Pident id) s
  in
  let subst =
    let s = Subst.identity in
    let s =
      Array.fold_left
        (fun s ((gm, _) : GM.With_precision.t) -> add_subst_for gm.GM.head s)
        s swg.bound_globals
    in
    List.fold_left
      (fun s p -> add_subst_for (GM.Parameter_name.to_string p) s)
      s loaded.params
  in
  let sign_lazy, _staticity = swg.sign in
  let sign_lazy = Subst.Lazy.signature Keep subst sign_lazy in
  let sign = Subst.Lazy.force_signature sign_lazy in
  let bundle = bundle @ [{ bm_id = new_id; bm_sign = sign }] in
  bundle, bindings

let make_md md_type : Types.module_declaration =
  {
    md_type;
    md_modalities = Mode.Modality.(Const.id |> of_const);
    md_attributes = [];
    md_loc = Location.none;
    md_uid = Types.Uid.internal_not_actually_unique;
  }

type bundle_sig = {
  body : Types.signature;
      (** [Sig_module] entries for each bundled module, in topo order. *)
  param_ids : Ident.t list;
      (** Local idents for the functor's parameters, in declaration order. *)
  modules : intf_unit_info list;
      (** Metadata for each bundled module, in topo order. *)
}

(* Build the bundle's body signature from a set of input intf_unit_infos.
   Inputs must all be parameterised compunits.  For each, [insert_module]
   appends a substituted module declaration to the bundle and recursively
   pulls in any transitive deps. *)
let compute_bundle_sig (src_infos : intf_unit_info list) : bundle_sig =
  (* Collect parameter names across all inputs (declaration order: each
     input's [cmi_params] in order, deduped on first appearance), then
     register each as a Local Ident in the initial bindings. *)
  let param_ids =
    let seen = Hashtbl.create 4 in
    List.concat_map
      (fun (ui : intf_unit_info) ->
        List.filter_map
          (fun p ->
            let p_name = GM.Parameter_name.to_string p in
            if Hashtbl.mem seen p_name then None
            else begin
              Hashtbl.add seen p_name ();
              Some (Ident.create_local p_name)
            end)
          ui.ui_params)
      src_infos
  in
  let initial_bindings =
    List.fold_left
      (fun bindings id -> Bindings.add (Ident.name id) id bindings)
      empty_bindings param_ids
  in
  let bundle, _bindings =
    List.fold_left
      (fun (bundle, bindings) (ui : intf_unit_info) ->
        let name = CU.name_as_string ui.ui_unit in
        let loaded = load_cmi name in
        (match classify_cmi loaded with
        | `Parameterised -> ()
        | `Parameter ->
          Compenv.fatal
            (Printf.sprintf "functorize input '%s' is a parameter module" name)
        | `Plain ->
          Compenv.fatal
            (Printf.sprintf "functorize input '%s' is not a parameterised module"
               name));
        insert_module ~name loaded.sign_with_globals ~bundle ~bindings)
      (empty_bundle, initial_bindings) src_infos
  in
  let body =
    List.map
      (fun bm ->
        Types.Sig_module
          ( bm.bm_id,
            Mp_present,
            make_md (Mty_signature bm.bm_sign),
            Trec_not,
            Exported ))
      bundle
  in
  let modules =
    List.map
      (fun bm ->
        let name = Ident.name bm.bm_id in
        let loaded = load_cmi name in
        let ui_unit =
          match loaded.cu with Some cu -> cu | None -> assert false
        in
        { ui_unit; ui_params = loaded.params; ui_deps = [] })
      bundle
  in
  { body; param_ids; modules }

type bundle_functor_type = {
  functor_type : Types.module_type;
      (** [Mty_functor(Named p, ... Mty_functor(Unit, body))] — one
          [Mty_functor(Named ...)] layer per parameter, in declaration
          order. *)
  all_params : GM.t list;
  all_modules : intf_unit_info list;
}

(* Wrap [compute_bundle_sig]'s output as a functor type:
     [Mty_functor (Unit, body)] inside [Mty_functor (Named p, ...)] layers
     for each parameter, in declaration order. *)
let compute_bundle_functor_type (src_infos : intf_unit_info list) :
    bundle_functor_type =
  let { body; param_ids; modules } = compute_bundle_sig src_infos in
  let signature_of_compunit name : Types.signature =
    let loaded = load_cmi name in
    let sign, _ = loaded.sign_with_globals.sign in
    Subst.Lazy.force_signature sign
  in
  let with_unit =
    Types.Mty_functor (Unit, Mty_signature body, Mode.Alloc.legacy)
  in
  let functor_type =
    List.fold_right
      (fun param_id body ->
        let name = Ident.name param_id in
        let param_type = Types.Mty_signature (signature_of_compunit name) in
        Types.Mty_functor
          ( Named (Some param_id, param_type, Mode.Alloc.legacy),
            body,
            Mode.Alloc.legacy ))
      param_ids with_unit
  in
  let all_params =
    List.map
      (fun id -> GM.create_exn (Ident.name id) [] ~hidden_args:[])
      param_ids
  in
  { functor_type; all_params; all_modules = modules }

(* Wrap [functor_type] in a single-element signature [Sig_module Func] —
   the conventional shape of a bundle's saved cmi. *)
let wrap_as_func_module (functor_type : Types.module_type) : Types.signature =
  [
    Types.Sig_module
      ( Ident.create_local "Func",
        Mp_present,
        make_md functor_type,
        Trec_not,
        Exported );
  ]

let make_compilation_unit target =
  let output_basename = Filename.basename (Filename.remove_extension target) in
  CU.of_string (String.capitalize_ascii output_basename)

(* Interface mode: backend-agnostic.  Inputs are .cmi files; outputs are
   .cmi + .cmti + .cmsi.  Doesn't need [with_info] because the only Env work
   here is registering input cmis as imports (via [Env.import_cmi_for_link])
   and saving the bundle's cmi — both happen against the live persistent env
   set up by the caller's [Compmisc.init_path]. *)
let functorize_intf initial_env files target =
  let compilation_unit = make_compilation_unit target in
  let for_pack_prefix = CU.Prefix.from_clflags () in
  let target_artifact =
    Unit_info.Artifact.from_filename ~for_pack_prefix target
  in
  let unit_info =
    Unit_info.of_artifact Intf target_artifact ~dummy_source_file:target
  in
  Env.set_unit_name (Some unit_info);
  let src_infos = List.map read_intf_unit_info_of_cmi files in
  let { functor_type; _ } = compute_bundle_functor_type src_infos in
  let sg = wrap_as_func_module functor_type in
  Misc.try_finally
    (fun () ->
      Typemod.functorize_interface initial_env ~sg unit_info compilation_unit)
    ~exceptionally:(fun () -> Misc.remove_file target)

(* Backend-agnostic [-functorize] implementation flow.  Runs phase A (scan +
   signature from cmis), invokes [Typemod.functorize_implementation] to either
   save or check the bundle's cmi, then runs phase B (gather runtime layout
   from cmo/cmx) and hands the resulting Lambda program to the backend's
   [compile_program] callback.  Must be called inside a [with_info] callback so
   that [Env.import_cmi_for_link] writes to the bundle's persistent env. *)
let functorize_impl_with ~initial_env ~(info : Compile_common.info) ~input_files
    ~(read_unit_info_of_input : Misc.filepath -> impl_unit_info)
    ~(find_impl_unit_info_by_name : string -> impl_unit_info)
    ~(compile_program : Compile_common.info -> Lambda.program -> unit) : unit =
  let src_impls = List.map read_unit_info_of_input input_files in
  let src_intfs =
    List.map (fun (impl : impl_unit_info) -> impl.intf) src_impls
  in
  let { functor_type; all_params; all_modules } =
    compute_bundle_functor_type src_intfs
  in
  let sg = wrap_as_func_module functor_type in
  let modules_cu =
    List.map (fun (ui : intf_unit_info) -> ui.ui_unit) all_modules
  in
  let target = info.target in
  let modulename = info.module_name in
  Ident.reinit ();
  let coercion =
    if !Clflags.dont_write_files then Typedtree.Tcoerce_none
    else
      let save_cmt_cms cmi_opt =
        let decl_deps = Cmt_format.get_declaration_dependencies () in
        Cmt_format.save_cmt (Unit_info.cmt target) modulename
          Cmt_format.Functorize initial_env cmi_opt None;
        Cms_format.save_cms (Unit_info.cms target) modulename
          Cmt_format.Functorize initial_env None decl_deps
      in
      match !Clflags.cmi_file with
      | Some cmi_file ->
        let shape =
          let uid = Types.Uid.of_compilation_unit_id modulename in
          List.fold_left
            (fun map cu ->
              let name = CU.name_as_string cu in
              let id = Ident.create_persistent name in
              Shape.Map.add_module map id (Shape.for_persistent_unit name))
            Shape.Map.empty modules_cu
          |> Shape.str ~uid
        in
        let for_pack_prefix = CU.for_pack_prefix modulename in
        let cmi_artifact =
          Unit_info.Artifact.from_filename ~for_pack_prefix cmi_file
        in
        let name = CU.to_global_name_without_prefix modulename in
        let dclsig, staticity = Env.read_signature name cmi_artifact in
        let cc, _shape =
          let modes =
            Includecore.Specific
              ( (Env.mode_unit ~staticity:Mode.Staticity.Dynamic, None),
                Env.mode_unit ~staticity )
          in
          Includemod.compunit initial_env ~mark:true
            "(obtained by functorizing)" ~modes sg cmi_file dclsig shape
        in
        save_cmt_cms None;
        cc
      | None ->
        let name = CU.name modulename in
        let kind =
          Cmi_format.Normal { cmi_impl = modulename; cmi_arg_for = None }
        in
        let cmi =
          Env.save_signature_with_imports
            ~alerts:Misc.Stdlib.String.Map.empty
            (sg, Mode.Staticity.Dynamic)
            name kind (Unit_info.cmi target)
            (Array.of_list (Env.imports ()))
        in
        save_cmt_cms (Some cmi);
        Typedtree.Tcoerce_none
  in
  if not Clflags.(should_stop_after Compiler_pass.Typing) then begin
    (* Locate each exposed module's impl: from [src_impls] (a user-provided
       input), or via [find_impl_unit_info_by_name] (a transitive dep that
       Phase A discovered from cmi_globals). *)
    let find_impl ui_unit =
      try
        List.find
          (fun (impl : impl_unit_info) -> CU.equal impl.intf.ui_unit ui_unit)
          src_impls
      with Not_found ->
        find_impl_unit_info_by_name (CU.name_as_string ui_unit)
    in
    let exposed_impls =
      List.map (fun (ui : intf_unit_info) -> find_impl ui.ui_unit) all_modules
    in
    (* Phase B may discover impl-level deps that Phase A didn't surface (i.e.,
       a module's [cu_format] references a parameterised module that's not in
       its [cmi_globals]).  These are hidden deps: they're instantiated once
       inside the bundle's body so other modules can reference them, but they
       don't appear in the bundle's returned struct.  In practice this branch
       is rarely taken — the type-checker's [penv.globals] usually captures
       every parameterised reference, so [cmi_globals] is a superset of impl
       deps. *)
    let impl_deps_of (impl : impl_unit_info) =
      match impl.ui_format with
      | Mb_struct _ -> []
      | Mb_instantiating_functor { mb_runtime_params; _ } ->
          List.filter_map
            (fun (rp : Lambda.runtime_param) ->
              match rp with
              | Rp_main_module_block global -> Some global.GM.head
              | Rp_argument_block _ | Rp_unit -> None)
            mb_runtime_params
    in
    (* DFS from exposed_impls following impl_deps_of edges; load hidden deps
       on demand.  Output is a topologically sorted list in which every
       module's runtime deps appear before it. *)
    let loaded : (string, impl_unit_info) Hashtbl.t = Hashtbl.create 16 in
    List.iter
      (fun (impl : impl_unit_info) ->
        Hashtbl.replace loaded (CU.name_as_string impl.intf.ui_unit) impl)
      exposed_impls;
    let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    let topo : impl_unit_info list ref = ref [] in
    let rec dfs ~required_by name =
      if not (Hashtbl.mem visited name) then begin
        Hashtbl.add visited name ();
        let impl =
          match Hashtbl.find_opt loaded name with
          | Some impl -> impl
          | None ->
              let impl =
                try find_impl_unit_info_by_name name
                with Not_found ->
                  Location.raise_errorf ~loc:Location.none
                    "Cannot find '%s' for module '%s',@ required by '%s'."
                    (String.uncapitalize_ascii name)
                    name required_by
              in
              Hashtbl.add loaded name impl;
              impl
        in
        List.iter (dfs ~required_by:name) (impl_deps_of impl);
        topo := impl :: !topo
      end
    in
    List.iter
      (fun (impl : impl_unit_info) ->
        let name = CU.name_as_string impl.intf.ui_unit in
        dfs ~required_by:name name)
      exposed_impls;
    (* Functor parameter idents: one per [all_params], in declaration order. *)
    let param_idents =
      List.map (fun gm -> Ident.create_local (gm.GM.head ^ "_arg")) all_params
    in
    let param_map : Ident.t GM.Parameter_name.Map.t =
      List.fold_left2
        (fun m gm id ->
          let p_name = GM.Parameter_name.of_string gm.GM.head in
          GM.Parameter_name.Map.add p_name id m)
        GM.Parameter_name.Map.empty
        all_params param_idents
    in
    (* Build instantiations in runtime-dep order; populate [block_map]
       incrementally so each module's runtime args can refer to earlier
       let-bindings. *)
    let block_map : (string, Ident.t) Hashtbl.t = Hashtbl.create 16 in
    let instantiations =
      List.map
        (fun (impl : impl_unit_info) : Translmod.instantiation ->
          let cu = impl.intf.ui_unit in
          let cu_name = CU.name_as_string cu in
          let ident = Ident.create_local (cu_name ^ "__block") in
          let args : Translmod.runtime_arg list =
            match impl.ui_format with
            | Mb_struct _ -> []
            | Mb_instantiating_functor { mb_runtime_params; _ } ->
              List.map
                (fun (rp : Lambda.runtime_param) : Translmod.runtime_arg ->
                  match rp with
                  | Rp_argument_block global -> (
                    match GM.find_in_parameter_map global param_map with
                    | Some id -> Lvar id
                    | None ->
                      Misc.fatal_errorf
                        "transl_functorize: no argument-block mapping for %s"
                        global.GM.head)
                  | Rp_main_module_block global -> (
                    match Hashtbl.find_opt block_map global.GM.head with
                    | Some id -> Lvar id
                    | None ->
                      Misc.fatal_errorf
                        "transl_functorize: missing block for dep module %s \
                         (dependency ordering bug?)"
                        global.GM.head)
                  | Rp_unit -> Unit)
                mb_runtime_params
          in
          Hashtbl.add block_map cu_name ident;
          { ident; cu; args })
        (List.rev !topo)
    in
    let exposed_modules =
      List.map (fun (ui : intf_unit_info) -> ui.ui_unit) all_modules
    in
    let program =
      Translmod.transl_functorize info.module_name ~params:param_idents
        ~instantiations ~modules:exposed_modules ~coercion
    in
    compile_program info program
  end
