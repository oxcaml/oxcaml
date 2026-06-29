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

module CU = Compilation_unit
module GM = Global_module

type state = {
  rev_modules : (GM.t * Ident.t * Types.signature) list;
      (** Bundled modules, reversed (newest first). *)
  rev_params : (GM.Parameter_name.t * Ident.t) list;
      (** Discovered parameters, reversed (newest first). *)
  param_ids : Ident.t GM.Parameter_name.Map.t;
      (** Dedup: parameter → its local Ident. *)
  module_ids : Ident.t GM.Map.t;  (** Dedup: bundled module → its local Ident. *)
}

let empty_state =
  {
    rev_modules = [];
    rev_params = [];
    param_ids = GM.Parameter_name.Map.empty;
    module_ids = GM.Map.empty;
  }

let register_parameter p_name state =
  let id = Ident.create_local (GM.Parameter_name.to_string p_name) in
  ( id,
    {
      state with
      rev_params = (p_name, id) :: state.rev_params;
      param_ids = GM.Parameter_name.Map.add p_name id state.param_ids;
    } )

let maybe_register_parameter p_name state =
  match GM.Parameter_name.Map.find_opt p_name state.param_ids with
  | Some id -> (id, state)
  | None -> register_parameter p_name state

let format_referenced_from chain =
  chain
  |> List.map (fun n ->
      Printf.sprintf ", referenced from '%s'" (CU.Name.to_string n))
  |> String.concat ""

let validate_and_load ~chain (gm : GM.t) : Signature_with_global_bindings.t =
  let name = GM.to_name gm in
  let name_str = GM.Name.to_string name in
  match (gm.GM.visible_args, gm.GM.hidden_args) with
  | _ :: _, _ :: _ ->
      Compenv.fatal
        (Printf.sprintf
           "Cannot functorize: partial instance '%s' is not supported%s"
           name_str
           (format_referenced_from chain))
  | _ :: _, [] ->
      Misc.fatal_errorf
        "Functorizer.validate_and_load: %s is fully instantiated but appeared \
         in bound_globals"
        name_str
  | [], [] ->
      Misc.fatal_errorf
        "Functorizer.validate_and_load: %s is plain but appeared in \
         bound_globals"
        name_str
  | [], _ :: _ ->
      let cu, cmi_params, swg =
        Env.find_import ~chain (CU.Name.of_head_of_global_name name)
      in
      assert (Option.is_some cu);
      let tracked_set =
        gm.GM.hidden_args
        |> List.map (fun (a : _ GM.Argument.t) -> a.param)
        |> GM.Parameter_name.Set.of_list
      in
      let cmi_set = GM.Parameter_name.Set.of_list cmi_params in
      assert (GM.Parameter_name.Set.equal tracked_set cmi_set);
      swg

let rec insert_module ~chain (gm : GM.t)
    (swg : Signature_with_global_bindings.t) state =
  assert (not (List.is_empty gm.hidden_args));
  let local_name =
    let base = GM.Name.to_string (GM.to_name gm) in
    (* Transitive deps (non-empty [chain]) get a [DEP__] prefix to discourage
       users from accessing them through the bundle.
       CR-soon zqian: make these nonmentionable instead. *)
    if List.is_empty chain then base else "DEP__" ^ base
  in
  let new_id = Ident.create_local local_name in
  let state =
    { state with module_ids = GM.Map.add gm new_id state.module_ids }
  in
  let chain = CU.Name.of_head_of_global_name (GM.to_name gm) :: chain in
  let inner_subst, state =
    Array.fold_left
      (fun (subst, state) ((orig_gm, prec) : GM.With_precision.t) ->
        let orig_name = GM.to_name orig_gm in
        let orig_ident = Ident.create_global orig_name in
        match prec with
        | Approximate ->
            (* [Approximate] = [-no-alias-deps] alias whose cmi was never
               validated.  Replace with a [Pruned_<head>] placeholder. *)
            let pruned_name =
              GM.Name.create_exn ("Pruned_" ^ orig_name.head) orig_name.args
            in
            ( Subst.add_module orig_ident
                (Path.Pident (Ident.create_global pruned_name))
                subst,
              state )
        | Exact ->
            let swg = validate_and_load ~chain orig_gm in
            let id, state = maybe_insert_module ~chain orig_gm swg state in
            (Subst.add_module orig_ident (Path.Pident id) subst, state))
      (Subst.identity, state) swg.bound_globals
  in
  let inner_subst, state =
    List.fold_left
      (fun (subst, state) (a : _ GM.Argument.t) ->
        let id, state = maybe_register_parameter a.param state in
        let n = GM.Name.of_parameter_name a.param in
        (Subst.add_module (Ident.create_global n) (Path.Pident id) subst, state))
      (inner_subst, state) gm.hidden_args
  in
  let sign_lazy, _staticity = swg.sign in
  (* CR-soon zqian: introduce substitution as a constructor of the module
     type algebra, which allows lazy substitution to persist across files. *)
  let sign_lazy = Subst.Lazy.signature Keep inner_subst sign_lazy in
  let sign = Subst.Lazy.force_signature sign_lazy in
  (new_id, { state with rev_modules = (gm, new_id, sign) :: state.rev_modules })

and maybe_insert_module ~chain (gm : GM.t) swg state =
  match GM.Map.find_opt gm state.module_ids with
  | Some id -> (id, state)
  | None -> insert_module ~chain gm swg state

let make_md md_type : Types.module_declaration =
  {
    md_type;
    md_modalities = Mode.Modality.(Const.id |> of_const);
    md_attributes = [];
    md_loc = Location.none;
    md_uid = Types.Uid.internal_not_actually_unique;
  }

type result = {
  modules : (GM.t * Ident.t * Types.signature) list;
      (** Each bundled module's global name, Local Ident, and substituted
          signature, in topo order. *)
  params : (GM.Parameter_name.t * Ident.t) list;
      (** Each functor parameter's name and Local Ident, in declaration order.
      *)
}

let validate_inputs (input_module_names : string list) : CU.Name.Set.t =
  if List.is_empty input_module_names then
    Compenv.fatal "Must specify at least one module name with -functorize";
  List.fold_left
    (fun set name ->
      let cu_name = CU.Name.of_string name in
      if CU.Name.Set.mem cu_name set then
        Compenv.fatal (Printf.sprintf "Duplicate -functorize input: '%s'" name);
      CU.Name.Set.add cu_name set)
    CU.Name.Set.empty input_module_names

let compute (src_names : CU.Name.Set.t) : result =
  let chain = [] in
  let state =
    CU.Name.Set.fold
      (fun cu_name state ->
        match Env.find_import ~chain cu_name with
        | None, _, _ ->
            Compenv.fatal
              (Printf.sprintf
                 "Invalid -functorize input: '%s' is a parameter module"
                 (CU.Name.to_string cu_name))
        | Some _, [], _ ->
            Compenv.fatal
              (Printf.sprintf
                 "Invalid -functorize input: '%s' is not a parameterised module"
                 (CU.Name.to_string cu_name))
        | Some _, cmi_params, swg ->
            let gm =
              GM.create_exn
                (CU.Name.to_string cu_name)
                [] ~hidden_args:cmi_params
            in
            let _id, state = maybe_insert_module ~chain gm swg state in
            state)
      src_names empty_state
  in
  { modules = List.rev state.rev_modules; params = List.rev state.rev_params }

let wrap_in_named_functor_layers (params : (GM.Parameter_name.t * Ident.t) list)
    (body : Types.module_type) : Types.module_type =
  List.fold_right
    (fun (p_name, param_id) body ->
      let cu, params, swg =
        Env.find_import ~chain:[] (CU.Name.of_parameter_name p_name)
      in
      assert (Option.is_none cu && List.is_empty params);
      assert (Array.length swg.bound_globals = 0);
      let sign, _ = swg.sign in
      let param_type = Types.Mty_signature (Subst.Lazy.force_signature sign) in
      Types.Mty_functor
        ( Named (Some param_id, param_type, Mode.Alloc.legacy),
          body,
          Mode.Alloc.legacy ))
    params body

let compute_signature (params : (GM.Parameter_name.t * Ident.t) list)
    (modules : (GM.t * Ident.t * Types.signature) list) : Types.signature =
  let body =
    List.map
      (fun (_name, id, sign) ->
        Types.Sig_module
          (id, Mp_present, make_md (Mty_signature sign), Trec_not, Exported))
      modules
  in
  let intf_id = Ident.create_local "Intf" in
  let make_id = Ident.create_local "Make" in
  let s_id = Ident.create_local "S" in
  let s_decl : Types.modtype_declaration =
    {
      mtd_type = Some (Mty_signature body);
      mtd_attributes = [];
      mtd_loc = Location.none;
      mtd_uid = Types.Uid.internal_not_actually_unique;
    }
  in
  let intf_result = [ Types.Sig_modtype (s_id, s_decl, Exported) ] in
  let intf_mty =
    wrap_in_named_functor_layers params (Mty_signature intf_result)
  in
  (* Make's params are fresh idents (same names as [params] but different
     stamps), so [Make]'s functor binders are distinct from [Intf]'s. *)
  let make_params =
    List.map (fun (p_name, id) -> (p_name, Ident.rename id)) params
  in
  let intf_applied_path =
    List.fold_left
      (fun p (_p_name, arg_id) -> Path.Papply (p, Path.Pident arg_id))
      (Path.Pident intf_id) make_params
  in
  let make_result = Types.Mty_ident (Path.Pdot (intf_applied_path, "S")) in
  let make_with_unit =
    Types.Mty_functor (Unit, make_result, Mode.Alloc.legacy)
  in
  let make_mty = wrap_in_named_functor_layers make_params make_with_unit in
  [
    Types.Sig_module (intf_id, Mp_present, make_md intf_mty, Trec_not, Exported);
    Types.Sig_module (make_id, Mp_present, make_md make_mty, Trec_not, Exported);
  ]

let make_compilation_unit target =
  let output_basename = Filename.basename (Filename.remove_extension target) in
  CU.of_string (String.capitalize_ascii output_basename)

let interface input_module_names target =
  let compilation_unit = make_compilation_unit target in
  let for_pack_prefix = CU.Prefix.from_clflags () in
  let target_artifact =
    Unit_info.Artifact.from_filename ~for_pack_prefix target
  in
  let unit_info =
    Unit_info.of_artifact Intf target_artifact ~dummy_source_file:target
  in
  Env.set_current_unit unit_info;
  let { modules; params } = compute input_module_names in
  let sg = compute_signature params modules in
  Misc.try_finally
    (fun () ->
      Ident.reinit ();
      if not !Clflags.dont_write_files then begin
        let name = CU.name compilation_unit in
        let kind =
          Cmi_format.Normal { cmi_impl = compilation_unit; cmi_arg_for = None }
        in
        let cmi =
          Env.save_signature ~alerts:Misc.Stdlib.String.Map.empty
            (sg, Mode.Staticity.Dynamic)
            name kind (Unit_info.cmi unit_info)
        in
        let decl_deps = Cmt_format.get_declaration_dependencies () in
        Cmt_format.save_cmt (Unit_info.cmti unit_info) compilation_unit
          Cmt_format.Functorize (Compmisc.initial_env ()) (Some cmi) None;
        Cms_format.save_cms (Unit_info.cmsi unit_info) compilation_unit
          Cmt_format.Functorize (Compmisc.initial_env ()) None decl_deps
      end)
    ~exceptionally:(fun () -> Misc.remove_file target)

let implementation (input_module_names : CU.Name.Set.t)
    ~(find_impl_by_name : CU.t -> Lambda.main_module_block_format)
    ~(compile_program : Compile_common.info -> Lambda.program -> unit)
    (info : Compile_common.info) : unit =
  let unit_info = info.target in
  let { modules; params } = compute input_module_names in
  let sg = compute_signature params modules in
  let params = List.map fst params in
  let modules = List.map (fun (name, _id, _sign) -> name) modules in
  let modulename = info.module_name in
  Ident.reinit ();
  let coercion =
    if !Clflags.dont_write_files then Typedtree.Tcoerce_none
    else
      (* Build cmt/cms artifacts directly via [Artifact.from_filename] so they
         get [raw_source_file = None].  The bundle has no source [.ml];
         passing the output (the [source_file] [unit_info] was built with)
         would make [save_cmt]/[save_cms] [Digest.file] it, which doesn't
         exist yet at type-check time.  The cmt's [Functorize] binary_annots
         variant already records that this was a functorize output. *)
      let for_pack_prefix = CU.for_pack_prefix modulename in
      let target_artifact ext =
        let filename = Unit_info.prefix unit_info ^ ext in
        Unit_info.Artifact.from_filename ~for_pack_prefix filename
      in
      let save_cmt_cms cmi_opt =
        let decl_deps = Cmt_format.get_declaration_dependencies () in
        Cmt_format.save_cmt (target_artifact ".cmt") modulename
          Cmt_format.Functorize (Compmisc.initial_env ()) cmi_opt None;
        Cms_format.save_cms (target_artifact ".cms") modulename
          Cmt_format.Functorize (Compmisc.initial_env ()) None decl_deps
      in
      match !Clflags.cmi_file with
      | Some cmi_file ->
          let shape =
            let uid = Types.Uid.of_compilation_unit_id modulename in
            List.fold_left
              (fun map (gm : GM.t) ->
                let name_str = GM.Name.to_string (GM.to_name gm) in
                let id = Ident.create_persistent name_str in
                Shape.Map.add_module map id (Shape.for_persistent_unit name_str))
              Shape.Map.empty modules
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
            Includemod.compunit (Compmisc.initial_env ()) ~mark:true
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
            Env.save_signature_with_imports ~alerts:Misc.Stdlib.String.Map.empty
              (sg, Mode.Staticity.Dynamic)
              name kind (Unit_info.cmi unit_info)
              (Array.of_list (Env.imports ()))
          in
          save_cmt_cms (Some cmi);
          Typedtree.Tcoerce_none
  in
  if not Clflags.(should_stop_after Compiler_pass.Typing) then begin
    let program =
      Translmod.transl_functorize modulename params modules ~find_impl_by_name
        ~coercion
    in
    compile_program info program
  end
