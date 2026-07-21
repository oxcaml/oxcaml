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
  rev_modules : (GM.t * Ident.t * Subst.Lazy.signature) list;
      (** Bundled modules with unsubstituted signatures; substituted in one pass
          at the end of [compute]. *)
  rev_params : (GM.Parameter_name.t * Ident.t) list;
  param_map : Ident.t GM.Parameter_name.Map.t;
  module_map : Ident.t option GM.Name.Map.t;
      (** Bundled module → its local ident. [None] means the module was only
          ever referenced [Approximate] and gets substituted to [Pruned_<head>];
          can be upgraded to [Some] by a later [Exact] reference. *)
}

let empty_state =
  {
    rev_modules = [];
    rev_params = [];
    param_map = GM.Parameter_name.Map.empty;
    module_map = GM.Name.Map.empty;
  }

let register_parameter p_name state =
  let id = Ident.create_local (GM.Parameter_name.to_string p_name) in
  ( id,
    {
      state with
      rev_params = (p_name, id) :: state.rev_params;
      param_map = GM.Parameter_name.Map.add p_name id state.param_map;
    } )

let maybe_register_parameter p_name state =
  match GM.Parameter_name.Map.find_opt p_name state.param_map with
  | Some id -> (id, state)
  | None -> register_parameter p_name state

(* [gm] must not be fully instantiated. *)
let validate_and_load ~chain (gm : GM.t) : Signature_with_global_bindings.t =
  let name = GM.to_name gm in
  let cu, cmi_params, swg =
    Env.find_import ~chain (CU.Name.of_head_of_global_name name)
  in
  assert (Option.is_some cu);
  let tracked_set =
    gm.GM.hidden_args @ gm.GM.visible_args
    |> List.map (fun (a : _ GM.Argument.t) -> a.param)
    |> GM.Parameter_name.Set.of_list
  in
  let cmi_set = GM.Parameter_name.Set.of_list cmi_params in
  assert (GM.Parameter_name.Set.equal tracked_set cmi_set);
  swg

let rec insert_module_exact ~chain (gm : GM.t)
    (swg : Signature_with_global_bindings.t) state =
  let local_name =
    let base = GM.Name.to_string (GM.to_name gm) in
    (* Transitive deps (non-empty [chain]) get a [DEP__] prefix to discourage
       users from accessing them through the bundle.
       CR-soon zqian: make these nonmentionable instead. *)
    if List.is_empty chain then base else "DEP__" ^ base
  in
  let new_id = Ident.create_local local_name in
  let state =
    {
      state with
      module_map =
        GM.Name.Map.add (GM.to_name gm) (Some new_id) state.module_map;
    }
  in
  let chain = CU.Name.of_head_of_global_name (GM.to_name gm) :: chain in

  let swg =
    match gm.visible_args with
    | [] -> swg
    | _ :: _ ->
        let args =
          List.map
            (fun (a : GM.t GM.Argument.t) -> (a.param, a.value))
            gm.visible_args
        in
        Signature_with_global_bindings.subst swg args
  in
  let state =
    Array.fold_left
      (fun state gm_prec -> maybe_insert_module ~chain gm_prec state)
      state swg.bound_globals
  in

  let state =
    List.fold_left
      (fun state (a : GM.t GM.Argument.t) ->
        if GM.is_complete a.value then state
        else
          let swg = validate_and_load ~chain a.value in
          maybe_insert_module_exact ~chain a.value swg state)
      state gm.visible_args
  in
  let state =
    List.fold_left
      (fun state (a : _ GM.Argument.t) ->
        let _id, state = maybe_register_parameter a.param state in
        state)
      state gm.hidden_args
  in
  let sign_lazy, _staticity = swg.sign in
  { state with rev_modules = (gm, new_id, sign_lazy) :: state.rev_modules }

and maybe_insert_module_exact ~chain (gm : GM.t) swg state =
  match GM.Name.Map.find_opt (GM.to_name gm) state.module_map with
  | Some (Some _) -> state
  | Some None | None -> insert_module_exact ~chain gm swg state

and maybe_insert_module ~chain ((gm, prec) : GM.With_precision.t) state =
  match prec with
  | Approximate -> (
      let name = GM.to_name gm in
      match GM.Name.Map.find_opt name state.module_map with
      | Some _ -> state
      | None ->
          { state with module_map = GM.Name.Map.add name None state.module_map }
      )
  | Exact ->
      let swg = validate_and_load ~chain gm in
      maybe_insert_module_exact ~chain gm swg state

let make_md md_type : Types.module_declaration =
  {
    md_type;
    md_modalities = Mode.Modality.(Const.id |> of_const);
    md_attributes = [];
    md_loc = Location.none;
    md_uid = Types.Uid.internal_not_actually_unique;
  }

type result = {
  modules : (GM.t * Ident.t * Types.signature) list;  (** Topo-sorted. *)
  params : (GM.Parameter_name.t * Ident.t) list;  (** Declaration order. *)
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
            maybe_insert_module_exact ~chain gm swg state)
      src_names empty_state
  in
  let subst =
    GM.Name.Map.fold
      (fun (name : GM.Name.t) id_opt subst ->
        let orig_ident = Ident.create_global name in
        let target =
          match id_opt with
          | Some id -> Path.Pident id
          | None ->
              let pruned =
                GM.Name.create_exn ("Pruned_" ^ name.head) name.args
              in
              Path.Pident (Ident.create_global pruned)
        in
        Subst.add_module orig_ident target subst)
      state.module_map Subst.identity
  in
  let subst =
    List.fold_left
      (fun subst (p_name, p_id) ->
        let n = GM.Name.of_parameter_name p_name in
        Subst.add_module (Ident.create_global n) (Path.Pident p_id) subst)
      subst state.rev_params
  in
  let modules =
    List.rev_map
      (fun (gm, id, sign_lazy) ->
        (* CR-soon zqian: introduce substitution as a constructor of the
           module type algebra, which allows lazy substitution to persist
           across files. *)
        let sign_lazy = Subst.Lazy.signature Keep subst sign_lazy in
        let sign = Subst.Lazy.force_signature sign_lazy in
        (gm, id, sign))
      state.rev_modules
  in
  { modules; params = List.rev state.rev_params }

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
  (* Fresh idents so [Make]'s binders are distinct from [Intf]'s. *)
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

let interface input_module_names (info : Compile_common.info) =
  let unit_info = info.target in
  let compilation_unit = info.module_name in
  let { modules; params } = compute input_module_names in
  let sg = compute_signature params modules in
  Ident.reinit ();
  Misc.try_finally
    (fun () ->
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
    ~exceptionally:(fun () ->
      Misc.remove_file (Unit_info.Artifact.filename (Unit_info.cmi unit_info)))

let implementation (input_module_names : CU.Name.Set.t) ~ext
    ~(read_format :
       Misc.filepath ->
       Lambda.main_module_block_format * Lambda.arg_descr option)
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
                ( (Persistent_env.mode_pers_mod Mode.Staticity.Dynamic, None),
                  Persistent_env.mode_pers_mod staticity )
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
      Translmod.transl_functorization modulename params modules ~ext
        ~read_format ~coercion
    in
    compile_program info program
  end

(** Common byte/native driver: dispatches to [interface] for a [.cmi] target and
    [implementation] otherwise, wrapping everything in
    [Compile_common.with_info]. [with_info] is [Compile_common.with_info]
    pre-configured with the caller's backend and tool name. *)
let functorize input_module_names target
    ~(with_info :
       dump_ext:string -> Unit_info.t -> (Compile_common.info -> unit) -> unit)
    ~impl_ext ~read_format ~compile_program =
  let is_intf = Filename.check_suffix target ".cmi" in
  let output_prefix = Filename.remove_extension target in
  let unit_info =
    Compile_common.unit_info_from_cu_or_output_prefix ~source_file:target
      (if is_intf then Unit_info.Intf else Unit_info.Impl)
      ~output_prefix ~compilation_unit:Inferred_from_output_prefix
  in
  with_info ~dump_ext:(if is_intf then "cmi" else impl_ext) unit_info
  @@ fun info ->
  if is_intf then interface input_module_names info
  else
    Misc.try_finally
      (fun () ->
        implementation input_module_names ~ext:("." ^ impl_ext) ~read_format
          ~compile_program info)
      ~exceptionally:(fun () ->
        Misc.remove_file target;
        Misc.remove_file
          (Unit_info.Artifact.filename (Unit_info.cmi info.target)))
