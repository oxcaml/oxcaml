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

type unit_info = { ui_unit : CU.t; ui_format : main_module_block_format }

(* Read unit_info from a .cmi file for the -functorize-intf path.
   The mb_runtime_params ordering derived here need not be canonical since it is
   only used to discover which modules are parameterised and what their
   parameters are — it is never used to generate lambda code. *)
let read_unit_info_of_cmi (cmi_file : string) : unit_info =
  let cmi = Cmi_format.read_cmi cmi_file in
  let ui_unit =
    match cmi.cmi_kind with
    | Normal { cmi_impl; _ } -> cmi_impl
    | Parameter ->
        Misc.fatal_errorf "cannot functorize a parameter module: %s" cmi_file
  in
  let ui_format =
    match cmi.cmi_params with
    | [] -> Mb_struct { mb_repr = Module_value_only { field_count = 0 } }
    | _ ->
        let self_name = CU.name_as_string ui_unit in
        (* Build a set of module names that have real (non-alias) CRC entries,
           i.e., genuine compile-time dependencies. *)
        let real_imports : (string, unit) Hashtbl.t = Hashtbl.create 8 in
        Array.iter
          (fun imp ->
            match Import_info.crc imp with
            | None -> ()
            | Some _ ->
                Hashtbl.replace real_imports
                  (CU.Name.to_string (Import_info.name imp))
                  ())
          cmi.cmi_crcs;
        let rp_params =
          List.map
            (fun p ->
              let head = GM.Parameter_name.to_string p in
              Rp_argument_block (GM.create_exn head [] ~hidden_args:[]))
            cmi.cmi_params
        in
        let rp_deps =
          Array.to_list cmi.cmi_globals
          |> List.filter_map (fun (gm, _prec) ->
              let name = gm.GM.head in
              if String.equal name self_name then None
              else if not (Hashtbl.mem real_imports name) then None
              else Some (Rp_main_module_block gm))
        in
        Mb_instantiating_functor
          {
            mb_runtime_params = rp_params @ rp_deps;
            mb_returned_repr = Module_value_only { field_count = 0 };
          }
  in
  { ui_unit; ui_format }

(* Return the names of parameterized modules that [ui] depends on at runtime.
   These correspond to [Rp_main_module_block] entries in its runtime params. *)
let parameterized_deps_of (ui : unit_info) : string list =
  match ui.ui_format with
  | Mb_struct _ -> []
  | Mb_instantiating_functor { mb_runtime_params; _ } ->
      List.filter_map
        (fun (rp : runtime_param) ->
          match rp with
          | Rp_main_module_block global -> Some global.GM.head
          | _ -> None)
        mb_runtime_params

(* Collect all modules (inputs + transitive parameterized deps) in
   topological order so that every module's dependencies appear before it. *)
let collect_all_modules ~(find_unit_info_by_name : string -> unit_info)
    (src_infos : unit_info list) : unit_info list =
  let all : (string, unit_info) Hashtbl.t = Hashtbl.create 16 in
  let queue : unit_info Queue.t = Queue.create () in
  List.iter (fun ui -> Queue.push ui queue) src_infos;
  while not (Queue.is_empty queue) do
    let ui = Queue.pop queue in
    let name = CU.name_as_string ui.ui_unit in
    if not (Hashtbl.mem all name) then begin
      Hashtbl.add all name ui;
      List.iter
        (fun dep_name ->
          if not (Hashtbl.mem all dep_name) then
            match find_unit_info_by_name dep_name with
            | dep_ui -> Queue.push dep_ui queue
            | exception Not_found ->
                Location.raise_errorf ~loc:Location.none
                  "Cannot find '%s' for module '%s',@ required by '%s'."
                  (String.uncapitalize_ascii dep_name)
                  dep_name name)
        (parameterized_deps_of ui)
    end
  done;
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let result : string list ref = ref [] in
  let rec dfs name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.add visited name ();
      (match Hashtbl.find_opt all name with
      | Some ui -> List.iter dfs (parameterized_deps_of ui)
      | None -> ());
      result := name :: !result
    end
  in
  List.iter (fun ui -> dfs (CU.name_as_string ui.ui_unit)) src_infos;
  Hashtbl.iter (fun name _ -> dfs name) all;
  List.rev !result |> List.filter_map (Hashtbl.find_opt all)

(* Collect all unique parameter globals across all modules, preserving the
   order in which they are first encountered. *)
let collect_all_params (modules : unit_info list) : GM.t list =
  let seen = ref GM.Set.empty in
  let result = ref [] in
  List.iter
    (fun ui ->
      match ui.ui_format with
      | Mb_struct _ -> ()
      | Mb_instantiating_functor { mb_runtime_params; _ } ->
          List.iter
            (fun (rp : runtime_param) ->
              match rp with
              | Rp_argument_block global ->
                  if not (GM.Set.mem global !seen) then begin
                    seen := GM.Set.add global !seen;
                    result := global :: !result
                  end
              | _ -> ())
            mb_runtime_params)
    modules;
  List.rev !result

let compute_bundle_sig ~(all_params : GM.t list)
    ~(modules : (CU.t * main_module_block_format) list) : Types.signature =
  let open Types in
  let find_module_type name =
    let cmi_file =
      Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
    in
    Mty_signature (Cmi_format.read_cmi cmi_file).cmi_sign
  in
  let make_md md_type =
    {
      md_type;
      md_modalities = Mode.Modality.(of_const Const.id);
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Uid.internal_not_actually_unique;
    }
  in
  let param_local_idents =
    List.map (fun (gm : GM.t) -> (gm, Ident.create_local gm.GM.head)) all_params
  in
  let module_local_idents =
    List.map
      (fun (cu, _) ->
        let name = CU.name_as_string cu in
        (name, Ident.create_local name))
      modules
  in
  let subst =
    let add name local_id s =
      Subst.add_module
        (Ident.create_global (GM.Name.create_no_args name))
        (Path.Pident local_id) s
    in
    let s = Subst.identity in
    let s =
      List.fold_left
        (fun s (gm, id) -> add gm.GM.head id s)
        s param_local_idents
    in
    let s =
      List.fold_left (fun s (name, id) -> add name id s) s module_local_idents
    in
    s
  in
  let result_items =
    List.map
      (fun (cu, _fmt) ->
        let name = CU.name_as_string cu in
        let local_id = List.assoc name module_local_idents in
        let mt = Subst.modtype Keep subst (find_module_type name) in
        Sig_module (local_id, Mp_present, make_md mt, Trec_not, Exported))
      modules
  in
  let with_unit =
    Mty_functor (Unit, Mty_signature result_items, Mode.Alloc.legacy)
  in
  let functor_type =
    List.fold_right
      (fun (global, param_id) body ->
        let param_type = find_module_type global.GM.head in
        Mty_functor
          ( Named (Some param_id, param_type, Mode.Alloc.legacy),
            body,
            Mode.Alloc.legacy ))
      param_local_idents with_unit
  in
  [
    Sig_module
      ( Ident.create_local "Func",
        Mp_present,
        make_md functor_type,
        Trec_not,
        Exported );
  ]

let generate_cmi ~(compilation_unit : CU.t) ~(all_params : GM.t list)
    ~(modules : (CU.t * main_module_block_format) list) ~source_file
    ~output_prefix =
  let outer_sig = compute_bundle_sig ~all_params ~modules in
  let target =
    Unit_info.make_with_known_compilation_unit ~source_file Impl
      output_prefix compilation_unit
  in
  ignore
    (Env.save_signature ~alerts:Misc.Stdlib.String.Map.empty outer_sig
       (CU.name compilation_unit)
       (Cmi_format.Normal
          { cmi_impl = compilation_unit; cmi_arg_for = None })
       (Unit_info.cmi target))

let functorize_intf ~srcs target ~read_unit_info ~find_unit_info_by_name =
  let src_infos = List.map read_unit_info srcs in
  let all_modules = collect_all_modules ~find_unit_info_by_name src_infos in
  let all_params = collect_all_params all_modules in
  let output_prefix = Filename.remove_extension target in
  let output_basename = Filename.basename output_prefix in
  let compilation_unit =
    CU.of_string (String.capitalize_ascii output_basename)
  in
  let modules = List.map (fun ui -> (ui.ui_unit, ui.ui_format)) all_modules in
  let source_file = List.hd srcs in
  generate_cmi ~compilation_unit ~all_params ~modules ~source_file ~output_prefix

let functorize_impl ~srcs target ~read_unit_info ~find_unit_info_by_name
    ~compile =
  let src_infos = List.map read_unit_info srcs in
  let all_modules = collect_all_modules ~find_unit_info_by_name src_infos in
  let all_params = collect_all_params all_modules in
  let output_prefix = Filename.remove_extension target in
  let output_basename = Filename.basename output_prefix in
  let compilation_unit =
    CU.of_string (String.capitalize_ascii output_basename)
  in
  let modules = List.map (fun ui -> (ui.ui_unit, ui.ui_format)) all_modules in
  let source_file = List.hd srcs in
  compile ~source_file ~output_prefix ~compilation_unit ~all_params ~modules
