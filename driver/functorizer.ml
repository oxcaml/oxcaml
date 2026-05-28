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

(* Names of parameterized modules that [ui] depends on.  Sourced from
   [ui_deps] (the cmi-recorded globals) rather than [ui_format], because the
   runtime layout drops deps that aren't actually referenced at runtime
   (e.g., parameterized modules that only appear via [-no-alias-deps]
   aliases). *)
let parameterized_deps_of (ui : intf_unit_info) : string list =
  List.map (fun gm -> gm.GM.head) ui.ui_deps

(* Collect all modules (inputs + transitive parameterized deps) in topological
   order so that every module's dependencies appear before it.  Parameterised
   over the input type ['a] (typically [intf_unit_info] or [impl_unit_info])
   via [get_intf], which extracts the intf-level info used for topology.
   Single DFS pass: deps are loaded on demand and the post-order produces the
   topological result. *)
let collect_all_modules (type a) ~(find_unit_info_by_name : string -> a)
    ~(get_intf : a -> intf_unit_info) (src_infos : a list) : a list =
  let loaded : (string, a) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun ui ->
      Hashtbl.replace loaded (CU.name_as_string (get_intf ui).ui_unit) ui)
    src_infos;
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let result : a list ref = ref [] in
  let rec dfs ~required_by name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.add visited name ();
      let ui =
        match Hashtbl.find_opt loaded name with
        | Some ui -> ui
        | None ->
            let ui =
              try find_unit_info_by_name name
              with Not_found ->
                Location.raise_errorf ~loc:Location.none
                  "Cannot find '%s' for module '%s',@ required by '%s'."
                  (String.uncapitalize_ascii name)
                  name required_by
            in
            Hashtbl.add loaded name ui;
            ui
      in
      List.iter (dfs ~required_by:name) (parameterized_deps_of (get_intf ui));
      result := ui :: !result
    end
  in
  List.iter
    (fun ui ->
      let name = CU.name_as_string (get_intf ui).ui_unit in
      dfs ~required_by:name name)
    src_infos;
  List.rev !result

(* Collect all unique declared parameters across all modules, preserving the
   order in which they are first encountered.  Reads from [ui_params] (the
   cmi-declared list) rather than the runtime layout, because the latter
   collapses unused parameters to [Rp_unit] and would lose parameters that
   aren't referenced at compile time. *)
let collect_all_params (modules : intf_unit_info list) : GM.t list =
  let seen = ref GM.Set.empty in
  let result = ref [] in
  List.iter
    (fun (ui : intf_unit_info) ->
      List.iter
        (fun p ->
          let head = GM.Parameter_name.to_string p in
          let global = GM.create_exn head [] ~hidden_args:[] in
          if not (GM.Set.mem global !seen) then begin
            seen := GM.Set.add global !seen;
            result := global :: !result
          end)
        ui.ui_params)
    modules;
  List.rev !result

type bundle_sig = {
  signature : Types.signature;
  all_params : GM.t list;
  all_modules : intf_unit_info list;
}

(* Build the bundle's signature from a set of input intf_unit_infos.  Walks
   transitive parameterised deps via [find_unit_info_by_name], inlines each
   module's signature into the bundle's body, and wraps the body in a functor
   over the bundle's collected parameters.  Reads only cmi data (via the
   provided [find_unit_info_by_name] callback and [Env.import_cmi_for_link]);
   no cmo/cmx reads. *)
let compute_bundle_sig ~(find_unit_info_by_name : string -> intf_unit_info)
    (src_infos : intf_unit_info list) : bundle_sig =
  let all_modules =
    collect_all_modules ~find_unit_info_by_name ~get_intf:Fun.id src_infos
  in
  let all_params = collect_all_params all_modules in
  let modules =
    List.map (fun (ui : intf_unit_info) -> ui.ui_unit) all_modules
  in
  let find_module_type name =
    let cmi_file =
      Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
    in
    let loaded = Env.import_cmi_for_link cmi_file in
    let sign, _staticity = loaded.sign_with_globals.sign in
    Types.Mty_signature (Subst.Lazy.force_signature sign)
  in
  let make_md md_type : Types.module_declaration =
    {
      md_type;
      md_modalities = Mode.Modality.(Const.id |> of_const);
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Types.Uid.internal_not_actually_unique;
    }
  in
  let param_local_idents =
    List.map (fun (gm : GM.t) -> (gm, Ident.create_local gm.GM.head)) all_params
  in
  let module_local_idents =
    List.map
      (fun cu ->
        let name = CU.name_as_string cu in
        (name, Ident.create_local name))
      modules
  in
  let add_to_subst name local_id s =
    Subst.add_module
      (Ident.create_global (GM.Name.create_no_args name))
      (Path.Pident local_id) s
  in
  let subst =
    let s = Subst.identity in
    let s =
      List.fold_left
        (fun s (gm, id) -> add_to_subst gm.GM.head id s)
        s param_local_idents
    in
    List.fold_left
      (fun s (name, id) -> add_to_subst name id s)
      s module_local_idents
  in
  let result_items =
    List.map
      (fun cu ->
        let name = CU.name_as_string cu in
        let local_id = List.assoc name module_local_idents in
        let mt = Subst.modtype Keep subst (find_module_type name) in
        Types.Sig_module (local_id, Mp_present, make_md mt, Trec_not, Exported))
      modules
  in
  let with_unit =
    Types.Mty_functor (Unit, Mty_signature result_items, Mode.Alloc.legacy)
  in
  let functor_type =
    List.fold_right
      (fun (global, param_id) body ->
        let param_type = find_module_type global.GM.head in
        Types.Mty_functor
          ( Named (Some param_id, param_type, Mode.Alloc.legacy),
            body,
            Mode.Alloc.legacy ))
      param_local_idents with_unit
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
  { signature; all_params; all_modules }

let make_compilation_unit target =
  let output_basename = Filename.basename (Filename.remove_extension target) in
  CU.of_string (String.capitalize_ascii output_basename)

let find_unit_info_by_name_cmi name : intf_unit_info =
  let filename =
    Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
  in
  read_intf_unit_info_of_cmi filename

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
  let { signature = sg; _ } =
    compute_bundle_sig ~find_unit_info_by_name:find_unit_info_by_name_cmi
      src_infos
  in
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
    ~(find_intf_unit_info_by_name : string -> intf_unit_info)
    ~(find_impl_unit_info_by_name : string -> impl_unit_info)
    ~(compile_program : Compile_common.info -> Lambda.program -> unit) : unit =
  let src_impls = List.map read_unit_info_of_input input_files in
  let src_intfs =
    List.map (fun (impl : impl_unit_info) -> impl.intf) src_impls
  in
  let { signature = sg; all_params; all_modules } =
    compute_bundle_sig ~find_unit_info_by_name:find_intf_unit_info_by_name
      src_intfs
  in
  let modules_cu =
    List.map (fun (ui : intf_unit_info) -> ui.ui_unit) all_modules
  in
  let coercion =
    Typemod.functorize_implementation initial_env ~sg ~modules:modules_cu
      info.target info.module_name
  in
  if not Clflags.(should_stop_after Compiler_pass.Typing) then begin
    let modules =
      List.map
        (fun (ui : intf_unit_info) ->
          let impl =
            try
              List.find
                (fun (impl : impl_unit_info) ->
                  CU.equal impl.intf.ui_unit ui.ui_unit)
                src_impls
            with Not_found ->
              find_impl_unit_info_by_name (CU.name_as_string ui.ui_unit)
          in
          (ui.ui_unit, impl.ui_format))
        all_modules
    in
    let program =
      Translmod.transl_functorize info.module_name ~all_params ~modules
        ~coercion
    in
    compile_program info program
  end
