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

type bundle_sig = {
  signature : Types.signature;
  all_params : GM.t list;
  all_modules : intf_unit_info list;
}

(* Build the bundle's signature from a set of input intf_unit_infos.

   Uses alias-chain compression to discover the "real" set of bundle modules:
   for each reference to another module in an input's signature, follow the
   alias chain until we hit something that's either (a) a non-parameterised
   compunit (leave as a global reference; no need to bundle), or (b) the
   most-simplified form inside a parameterised cmi (intern that compunit as
   a real bundle module).  This avoids the cycle that arises with
   [-no-alias-deps]: alias-only intermediate modules (like dune's [d__]
   wrappers) don't appear in the bundle; only the modules at the *end* of
   each alias chain do. *)
let compute_bundle_sig (src_infos : intf_unit_info list) : bundle_sig =
  let make_md md_type : Types.module_declaration =
    {
      md_type;
      md_modalities = Mode.Modality.(Const.id |> of_const);
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Types.Uid.internal_not_actually_unique;
    }
  in
  let load name : Persistent_env.loaded_cmi =
    let cmi_file =
      Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
    in
    Env.import_cmi_for_link cmi_file
  in
  let signature_of_compunit name : Types.signature =
    let loaded = load name in
    let sign, _ = loaded.sign_with_globals.sign in
    Subst.Lazy.force_signature sign
  in
  (* Parameters: each parameter compunit gets a Local ident; references to it
     get substituted via this map. *)
  let param_locals : (string, Ident.t) Hashtbl.t = Hashtbl.create 4 in
  let param_order : Ident.t list ref = ref [] in
  let intern_param name =
    match Hashtbl.find_opt param_locals name with
    | Some id -> id
    | None ->
        let id = Ident.create_local name in
        Hashtbl.add param_locals name id;
        param_order := id :: !param_order;
        id
  in
  (* Real (bundled) modules: each gets a Local ident.  Holds the rewritten
     signature once processed. *)
  let real_locals : (string, Ident.t) Hashtbl.t = Hashtbl.create 16 in
  let real_order : string list ref = ref [] in
  let real_signatures : (string, Types.signature) Hashtbl.t =
    Hashtbl.create 16
  in
  let queue : string Queue.t = Queue.create () in
  let intern_real name =
    match Hashtbl.find_opt real_locals name with
    | Some id -> id
    | None ->
        let id = Ident.create_local name in
        Hashtbl.add real_locals name id;
        real_order := name :: !real_order;
        (* Intern any parameters this module declares.  References to
         parameters appear in type expressions (e.g., [val x : P.t]) that
         the rewrite walk doesn't traverse, so we register them eagerly
         based on the cmi's declared [cmi_params]. *)
        let loaded = load name in
        List.iter
          (fun p ->
            let _ : Ident.t = intern_param (GM.Parameter_name.to_string p) in
            ())
          loaded.params;
        Queue.push name queue;
        id
  in
  (* Classify a compunit by its cmi.  Returns:
       - [`Parameter] if the cmi is [-as-parameter] (no signature body to
         include; we just record a parameter binding).
       - [`Parameterised] if [cmi_params <> []]; intern as a real bundle
         module.
       - [`Plain] otherwise; leave references as globals. *)
  let classify name =
    let loaded = load name in
    match loaded.cu with
    | None -> `Parameter
    | Some _ -> if loaded.params = [] then `Plain else `Parameterised
  in
  (* Navigate [parent_mty] to find a sub-module named [field] and return its
     declaration. *)
  let find_module_in_sig field (items : Types.signature) =
    List.find_map
      (function
        | Types.Sig_module (id, _, md, _, _) when Ident.name id = field ->
            Some md
        | _ -> None)
      items
  in
  (* Given a path whose root is a Global compunit, return the [md_type] it
     points to (with aliases NOT yet followed at this step). *)
  let rec md_type_of_path = function
    | Path.Pident i when Ident.is_global i ->
        Types.Mty_signature (signature_of_compunit (Ident.name i))
    | Path.Pdot (parent, field) -> (
        let parent_mty = md_type_of_path parent in
        let parent_sig =
          match parent_mty with
          | Types.Mty_signature s -> s
          | Types.Mty_alias inner -> (
              (* If we hit an alias while navigating, resolve it to the signature *)
              match md_type_of_path inner with
              | Types.Mty_signature s -> s
              | _ ->
                  Misc.fatal_errorf
                    "compute_bundle_sig: alias %s doesn't resolve to a \
                     signature"
                    (Path.name inner))
          | _ ->
              Misc.fatal_errorf
                "compute_bundle_sig: parent of %s is not a signature" field
        in
        match find_module_in_sig field parent_sig with
        | Some md -> md.md_type
        | None ->
            Misc.fatal_errorf "compute_bundle_sig: field %s not in parent sig"
              field)
    | other ->
        Misc.fatal_errorf "compute_bundle_sig: unsupported path %s"
          (Path.name other)
  in
  (* Compress an alias chain.  If the path's root is a non-parameterised
     compunit, stop immediately: such a reference doesn't need bundling
     (main.ml will resolve it as a normal global at link time).  Otherwise
     follow aliases until we either bottom out at the most-simplified form,
     then substitute the root with a Local if the terminal points into a
     parameterised compunit or to a parameter compunit. *)
  let rec path_root_name = function
    | Path.Pident i when Ident.is_global i -> Some (Ident.name i)
    | Path.Pdot (p, _) -> path_root_name p
    | _ -> None
  in
  let rec compress_path p =
    match path_root_name p with
    | None -> p
    | Some name -> (
        match classify name with
        | `Plain -> p
        | `Parameter -> substitute_root p
        | `Parameterised -> (
            match md_type_of_path p with
            | Types.Mty_alias inner -> compress_path inner
            | _ -> substitute_root p))
  and substitute_root = function
    | Path.Pident i when Ident.is_global i -> (
        let name = Ident.name i in
        match classify name with
        | `Parameter -> Path.Pident (intern_param name)
        | `Parameterised -> Path.Pident (intern_real name)
        | `Plain -> Path.Pident i)
    | Path.Pdot (parent, field) -> Path.Pdot (substitute_root parent, field)
    | other -> other
  in
  (* Walk a module type, applying [compress_path] to every alias path and
     [substitute_root] to other module references. *)
  let rec rewrite_modtype = function
    | Types.Mty_alias p -> Types.Mty_alias (compress_path p)
    | Types.Mty_signature items ->
        Types.Mty_signature (List.map rewrite_item items)
    | Types.Mty_functor (param, body, mres) ->
        Types.Mty_functor
          (rewrite_functor_param param, rewrite_modtype body, mres)
    | Types.Mty_ident p -> Types.Mty_ident (substitute_root p)
    | Types.Mty_strengthen (mty, p, a) ->
        Types.Mty_strengthen (rewrite_modtype mty, substitute_root p, a)
  and rewrite_functor_param : Types.functor_parameter -> Types.functor_parameter
      = function
    | Types.Named (id, arg, marg) -> Types.Named (id, rewrite_modtype arg, marg)
    | Types.Unit -> Types.Unit
  and rewrite_item = function
    | Types.Sig_module (id, pres, md, rs, vis) ->
        let md = { md with md_type = rewrite_modtype md.md_type } in
        Types.Sig_module (id, pres, md, rs, vis)
    | other -> other
  in
  (* Every input must be a parameterised module; a non-parameterised input
     has no business being in a bundle (the bundle's purpose is to package
     parameterised modules for instantiation). *)
  List.iter
    (fun (ui : intf_unit_info) ->
      let name = CU.name_as_string ui.ui_unit in
      match classify name with
      | `Parameterised ->
          let _ : Ident.t = intern_real name in
          ()
      | `Parameter ->
          Compenv.fatal
            (Printf.sprintf "functorize input '%s' is a parameter module" name)
      | `Plain ->
          Compenv.fatal
            (Printf.sprintf
               "functorize input '%s' is not a parameterised module" name))
    src_infos;
  (* Process the queue.  Rewriting may intern more real_locals or
     param_locals, which get queued and processed in turn. *)
  while not (Queue.is_empty queue) do
    let name = Queue.pop queue in
    if not (Hashtbl.mem real_signatures name) then begin
      let sg = signature_of_compunit name in
      let rewritten = List.map rewrite_item sg in
      Hashtbl.add real_signatures name rewritten
    end
  done;
  (* Build a [Subst.t] mapping [Pident (Global X)] -> [Pident <local X>] for
     every parameter and every real module we've interned, and apply it to
     each rewritten signature.  This second pass catches paths inside type
     expressions, value descriptions, and other non-[Mty_alias] structures
     that [rewrite_modtype] above didn't touch. *)
  let subst =
    let s = Subst.identity in
    let s =
      Hashtbl.fold
        (fun name id s ->
          Subst.add_module
            (Ident.create_global (GM.Name.create_no_args name))
            (Path.Pident id) s)
        param_locals s
    in
    Hashtbl.fold
      (fun name id s ->
        Subst.add_module
          (Ident.create_global (GM.Name.create_no_args name))
          (Path.Pident id) s)
      real_locals s
  in
  Hashtbl.filter_map_inplace
    (fun _name sg ->
      match Subst.modtype Keep subst (Types.Mty_signature sg) with
      | Types.Mty_signature sg' -> Some sg'
      | _ -> Some sg)
    real_signatures;
  (* Topologically sort real_locals by dep edges discovered in rewritten
     signatures: a real module A depends on real module B iff A's rewritten
     signature contains a reference to B's Local. *)
  let deps_of_real name =
    let sg = Hashtbl.find real_signatures name in
    let deps = ref [] in
    let rec scan_path = function
      | Path.Pident i when not (Ident.is_global i) -> (
          let n = Ident.name i in
          match Hashtbl.find_opt real_locals n with
          | Some id when Ident.same id i && n <> name -> deps := n :: !deps
          | _ -> ())
      | Path.Pdot (p, _) -> scan_path p
      | _ -> ()
    in
    let rec scan_mty = function
      | Types.Mty_alias p -> scan_path p
      | Types.Mty_signature items -> List.iter scan_item items
      | Types.Mty_functor (param, body, _) ->
          scan_functor_param param;
          scan_mty body
      | Types.Mty_ident p -> scan_path p
      | Types.Mty_strengthen (m, p, _) ->
          scan_mty m;
          scan_path p
    and scan_functor_param : Types.functor_parameter -> unit = function
      | Types.Named (_, arg, _) -> scan_mty arg
      | Types.Unit -> ()
    and scan_item = function
      | Types.Sig_module (_, _, md, _, _) -> scan_mty md.md_type
      | _ -> ()
    in
    List.iter scan_item sg;
    !deps
  in
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let topo : string list ref = ref [] in
  let rec dfs name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.add visited name ();
      List.iter dfs (deps_of_real name);
      topo := name :: !topo
    end
  in
  (* Visit in reverse-insertion order so user-provided inputs are leaves *)
  List.iter dfs (List.rev !real_order);
  let bundle_names = List.rev !topo in
  (* Build bundle signature items in topo order *)
  let result_items =
    List.map
      (fun name ->
        let local_id = Hashtbl.find real_locals name in
        let sg = Hashtbl.find real_signatures name in
        Types.Sig_module
          (local_id, Mp_present, make_md (Mty_signature sg), Trec_not, Exported))
      bundle_names
  in
  let with_unit =
    Types.Mty_functor (Unit, Mty_signature result_items, Mode.Alloc.legacy)
  in
  let param_globals_ordered =
    (* all_params order: parameters as they were interned, in reverse of
       insertion (since we use a list ref accumulator) *)
    List.rev !param_order
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
      param_globals_ordered with_unit
  in
  let signature =
    [
      Types.Sig_module
        ( Ident.create_local "Func",
          Mp_present,
          make_md functor_type,
          Trec_not,
          Exported );
    ]
  in
  let all_params =
    List.map
      (fun id -> GM.create_exn (Ident.name id) [] ~hidden_args:[])
      param_globals_ordered
  in
  let all_modules =
    (* For backwards compat with callers that read [all_modules] to compute
       the impl-side topo: synthesize an intf_unit_info for each real module
       interned.  We don't have the original ui_params for non-input deps,
       so leave it empty (the impl phase will re-read the cmi anyway). *)
    List.map
      (fun name ->
        let ui_unit =
          match (load name).cu with Some cu -> cu | None -> assert false
        in
        let params = (load name).params in
        { ui_unit; ui_params = params; ui_deps = [] })
      bundle_names
  in
  { signature; all_params; all_modules }

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
  let { signature = sg; _ } = compute_bundle_sig src_infos in
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
  let { signature = sg; all_params; all_modules } =
    compute_bundle_sig src_intfs
  in
  let modules_cu =
    List.map (fun (ui : intf_unit_info) -> ui.ui_unit) all_modules
  in
  let coercion =
    Typemod.functorize_implementation initial_env ~sg ~modules:modules_cu
      info.target info.module_name
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
    let exposed_names =
      List.fold_left
        (fun acc (ui : intf_unit_info) ->
          CU.Name.Set.add (CU.name ui.ui_unit) acc)
        CU.Name.Set.empty all_modules
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
    let modules =
      List.rev_map
        (fun (impl : impl_unit_info) : Translmod.bundle_module ->
          {
            cu = impl.intf.ui_unit;
            format = impl.ui_format;
            exposed = CU.Name.Set.mem (CU.name impl.intf.ui_unit) exposed_names;
          })
        !topo
    in
    let program =
      Translmod.transl_functorize info.module_name ~all_params ~modules
        ~coercion
    in
    compile_program info program
  end
