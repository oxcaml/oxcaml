(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025--2026 Jane Street Group LLC                             *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* TODO: It seems like this file is running into similar issues to the Dynlink
   code, whereby state in compilerlibs needs to be updated, meaning that it
   could conflict with other use of compilerlibs in an application. That said,
   we're relying on using the same compilerlibs state for .cmi and .cmx lookups
   via this module when called from mdx, instead of using bundles. *)

type bundle = private string

external bundled_cmis_this_exe : unit -> bundle = "caml_bundled_cmis_this_exe"

external bundled_cmxs_this_exe : unit -> bundle = "caml_bundled_cmxs_this_exe"

external bundle_available : bundle -> bool = "caml_bundle_available"

let find_bundle_in_exe ~ext get_this_exe =
  let bundle = get_this_exe () in
  if bundle_available bundle
  then bundle
  else
    failwith
      ("Executable does not contain ." ^ ext
     ^ " bundle and [use_existing_compilerlibs_state_for_artifacts]"
     ^ " has not been called")

let cmis = ref Compilation_unit.Name.Map.empty

let cmxs = ref []

let read_bundles ~marshalled_cmi_bundle ~marshalled_cmx_bundle =
  let bundled_cmis : Cmi_format.cmi_infos Compilation_unit.Name.Map.t =
    Marshal.from_string marshalled_cmi_bundle 0
  in
  let new_cmis =
    Compilation_unit.Name.Map.map
      (fun (cmi : Cmi_format.cmi_infos) : Cmi_format.cmi_infos_lazy ->
        let sign, staticity = cmi.cmi_sign in
        { cmi with cmi_sign = Subst.Lazy.of_signature sign, staticity })
      bundled_cmis
  in
  let bundled_cmxs : (Cmx_format.unit_infos_raw * string array) list =
    Marshal.from_string marshalled_cmx_bundle 0
  in
  let new_cmxs =
    List.map
      (fun ((uir, sections) : Cmx_format.unit_infos_raw * _) ->
        let sections =
          File_sections.from_array
            (Array.map (fun s -> Marshal.from_string s 0) sections)
        in
        let ui : Cmx_format.unit_infos =
          { ui_unit = uir.uir_unit;
            ui_defines = uir.uir_defines;
            ui_format = uir.uir_format;
            ui_arg_descr = uir.uir_arg_descr;
            ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
            ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
            ui_quoted_cmi = uir.uir_quoted_cmi |> Array.to_list;
            ui_quoted_cmx = uir.uir_quoted_cmx |> Array.to_list;
            ui_generic_fns = uir.uir_generic_fns;
            ui_export_info = uir.uir_export_info;
            ui_zero_alloc_info = Zero_alloc_info.of_raw uir.uir_zero_alloc_info;
            ui_force_link = uir.uir_force_link;
            ui_requires_metaprogramming = uir.uir_requires_metaprogramming;
            ui_external_symbols = uir.uir_external_symbols |> Array.to_list;
            ui_file_sections = sections
          }
        in
        ui)
      bundled_cmxs
  in
  cmis := new_cmis;
  cmxs := new_cmxs

let read_bundles_from_exe () =
  assert (not (Opttoploop.using_existing_compilerlibs_state_for_artifacts ()));
  let marshalled_cmi_bundle =
    find_bundle_in_exe ~ext:"cmi" bundled_cmis_this_exe
  in
  let marshalled_cmx_bundle =
    find_bundle_in_exe ~ext:"cmx" bundled_cmxs_this_exe
  in
  let marshalled_cmi_bundle = (marshalled_cmi_bundle :> string) in
  let marshalled_cmx_bundle = (marshalled_cmx_bundle :> string) in
  read_bundles ~marshalled_cmi_bundle ~marshalled_cmx_bundle

module Injector = struct
  type t = CamlinternalQuote.Injector.t
end

external inject : Injector.t -> 'a eval -> 'a expr = "%inject"

(* Allocates the module block of an injector's compilation unit outside the
   OCaml heap (so that its address is stable and can be bound to the module
   block symbol for the JIT) and registers it as a GC root. *)
external alloc_injected_module_block : Obj.t array -> Obj.t
  = "caml_eval_alloc_injected_module_block"

(* In-memory cmis for the compilation units of injectors, served via
   [Persistent_env.Persistent_signature.load]. *)
let injector_cmis : Cmi_format.cmi_infos_lazy Compilation_unit.Name.Map.t ref =
  ref Compilation_unit.Name.Map.empty

let persistent_signature_load_hook_installed = ref false

(* Serve cmis from, in order: the injector units, then (unless the existing
   compilerlibs state is in use, e.g. under mdx) the bundle marshalled into the
   executable, falling back to the pre-existing loader. *)
let install_persistent_signature_load_hook () =
  if not !persistent_signature_load_hook_installed
  then begin
    persistent_signature_load_hook_installed := true;
    let default_load = !Persistent_env.Persistent_signature.load in
    Persistent_env.Persistent_signature.load
      := fun ~allow_hidden ~unit_name ->
           match
             Compilation_unit.Name.Map.find_opt unit_name !injector_cmis
           with
           | Some cmi ->
             Some
               { Persistent_env.Persistent_signature.filename =
                   Compilation_unit.Name.to_string unit_name;
                 cmi;
                 visibility = Visible { cmx_guaranteed = false }
               }
           | None ->
             if Opttoploop.using_existing_compilerlibs_state_for_artifacts ()
             then default_load ~allow_hidden ~unit_name
             else
               Option.map
                 (fun cmi ->
                   { Persistent_env.Persistent_signature.filename =
                       Compilation_unit.Name.to_string unit_name;
                     cmi;
                     visibility = Visible { cmx_guaranteed = false }
                   })
                 (Compilation_unit.Name.Map.find_opt unit_name !cmis)
  end

(* TODO: Determine machine_width properly from target configuration (same TODO
   as in [Jit]). *)
let injector_machine_width = Target_system.Machine_width.Sixty_four

(* Rebuild classic-mode approximations for the injected values from their
   marshalled [Value_approximation.Standalone.t] data (collected by
   [%reify_approx] at the injection sites), and package them as the injector
   unit's cmx export information, so that the compiler can optimise uses of
   injected values (in particular, inline injected functions). The module block
   symbol is given a block approximation whose fields are the injected values'
   approximations, which is how classic mode exposes the members of a
   compilation unit. Requires [Compilenv]'s cmx cache to be populated (code is
   recovered from the original units' cmx data via the symbols recorded in the
   approximations). *)
let build_injector_export_info (entries : CamlinternalQuote.Injector.entry list)
    compilation_unit :
    Flambda2_cmx.Flambda_cmx_format.raw option * File_sections.t =
  let module VA = Flambda2_classic_mode_types.Value_approximation in
  let module F2_symbol = Flambda2_identifiers.Symbol in
  let module FK = Flambda2_kinds.Flambda_kind in
  let cmx_loader =
    Flambda2_cmx.Flambda_cmx.create_loader
      ~get_module_info:Compilenv.get_unit_export_info
  in
  let debug =
    match Sys.getenv_opt "OCAML_EVAL_DEBUG_INJECT" with
    | Some ("true" | "1") -> true
    | None | Some _ -> false
  in
  let find_code ~code_id ~code_id_linkage_name ~function_slot:_ ~symbol
      ~lookup_symbol : _ VA.Standalone.closure_resolution =
    (* Prefer recovering the closure's full approximation wholesale from the
       original unit's cmx data, via the manufactured lookup symbol or the
       closure's own symbol: the code IDs and function slots there are the
       original ones, which existing code and types refer to. Fall back to
       looking the code up by the (stamp-bearing) linkage name of the original
       code ID. *)
    let via_symbol sym : _ VA.Standalone.closure_resolution option =
      match Flambda2_cmx.Flambda_cmx.load_symbol_approx cmx_loader sym with
      | exception exn ->
        (* e.g. [Not_found] if the symbol is absent from its unit's cmx data
           (which can happen if the data was produced by a different
           configuration than expected). *)
        if debug
        then
          Format.eprintf "inject: exception looking up %a: %s@." F2_symbol.print
            sym (Printexc.to_string exn);
        None
      | Closure_approximation _ as authoritative ->
        if debug
        then
          Format.eprintf "inject: authoritative approx via %a: %a@."
            F2_symbol.print sym VA.print authoritative;
        Some (VA.Standalone.Resolved authoritative)
      | (Unknown _ | Value_symbol _ | Value_const _ | Block_approximation _) as
        approx ->
        if debug
        then
          Format.eprintf "inject: no closure approx for %a: %a@."
            F2_symbol.print sym VA.print approx;
        None
    in
    let via_linkage_name () : _ VA.Standalone.closure_resolution option =
      let code_unit =
        Flambda2_identifiers.Code_id.get_compilation_unit code_id
      in
      let (_ : Flambda2_types.Typing_env.Serializable.t option) =
        Flambda2_cmx.Flambda_cmx.load_cmx_file_contents cmx_loader code_unit
      in
      match
        Flambda2_cmx.Exported_code.find_by_linkage_name
          (Flambda2_cmx.Flambda_cmx.get_imported_code cmx_loader ())
          code_id_linkage_name
      with
      | Some (original_code_id, code) ->
        if debug
        then
          Format.eprintf "inject: code for linkage name %s: present %b@."
            (Linkage_name.to_string code_id_linkage_name)
            (Flambda2_terms.Code_or_metadata.code_present code);
        Some (VA.Standalone.Code (original_code_id, code))
      | None ->
        if debug
        then
          Format.eprintf "inject: no code found for linkage name %s@."
            (Linkage_name.to_string code_id_linkage_name);
        None
    in
    let result =
      match Option.map via_symbol lookup_symbol with
      | Some (Some resolution) -> Some resolution
      | Some None | None -> (
        match Option.map via_symbol symbol with
        | Some (Some resolution) -> Some resolution
        | Some None | None -> via_linkage_name ())
    in
    match result with
    | Some resolution -> resolution
    | None -> VA.Standalone.Unknown_code
  in
  let field_approxs =
    List.map
      (fun (entry : CamlinternalQuote.Injector.entry) ->
        let unknown : _ VA.t = Unknown FK.value in
        let approx =
          match entry.approx with
          | "" -> unknown
          | marshalled -> (
            match VA.Standalone.of_marshalled_string marshalled with
            | standalone -> VA.Standalone.to_approximation standalone ~find_code
            | exception _ -> unknown)
        in
        if debug
        then
          Format.eprintf "inject: %s: marshalled %d bytes, approx %a@."
            entry.name
            (String.length entry.approx)
            VA.print approx;
        approx)
      entries
  in
  let module_symbol = F2_symbol.for_compilation_unit compilation_unit in
  let module_approx : _ VA.t =
    Block_approximation
      ( Flambda2_kinds.Tag.Scannable.zero,
        FK.Scannable_block_shape.Value_only,
        Array.of_list field_approxs,
        Flambda2_bound_identifiers.Alloc_mode.For_types.heap )
  in
  let approxs = F2_symbol.Map.singleton module_symbol module_approx in
  let all_code =
    (* The code reachable from the approximations was loaded into the loader's
       cache by [load_symbol_approx]; re-export it (as imported) like classic
       mode does. *)
    Flambda2_cmx.Exported_code.mark_as_imported
      (Flambda2_cmx.Flambda_cmx.get_imported_code cmx_loader ())
  in
  let sections = File_sections.Builder.create 0 in
  let _reachable_names, raw =
    Flambda2_cmx.Flambda_cmx.prepare_cmx_from_approx
      ~machine_width:injector_machine_width ~approxs ~module_symbol
      ~extra_root_names:Flambda2_nominal.Name_occurrences.empty
      ~exported_offsets:Flambda2_simplify_shared.Exported_offsets.empty
      ~used_value_slots:Flambda2_identifiers.Value_slot.Set.empty ~sections
      all_code
  in
  raw, File_sections.Builder.build sections

(* Build the compilation unit corresponding to an injector: allocate and
   register its module block, register its cmi (each injected value is exposed
   with a polymorphic type ['a]), and add the in-memory cmx information
   (including approximations of the injected values) to [Compilenv]'s cache.
   Must be called after [Compilenv] has been reset and its cache populated for
   the current evaluation. Does nothing if nothing was injected. *)
let setup_injector_unit (injector : CamlinternalQuote.Injector.t) : unit =
  match CamlinternalQuote.Injector.contents injector with
  | [] -> ()
  | entries -> (
    let cu_name = CamlinternalQuote.Injector.compilation_unit_name injector in
    let compilation_unit =
      Compilation_unit.create Compilation_unit.Prefix.empty
        (Compilation_unit.Name.of_string cu_name)
    in
    (* Module block, bound to the unit's module block symbol. *)
    let values =
      List.map
        (fun (entry : CamlinternalQuote.Injector.entry) -> entry.value)
        entries
      |> Array.of_list
    in
    let block = alloc_injected_module_block values in
    let linkage_name =
      Symbol.for_compilation_unit compilation_unit
      |> Symbol.linkage_name |> Linkage_name.to_string
    in
    Jit.jit_register_symbol linkage_name block;
    (* cmi: type a synthetic interface exposing each value as [val vN : 'a],
       mirroring what compiling such an .mli would produce. *)
    let intf_source =
      List.map
        (fun (entry : CamlinternalQuote.Injector.entry) ->
          Printf.sprintf "val %s : 'a\n" entry.name)
        entries
      |> String.concat ""
    in
    let saved_input_lexbuf = !Location.input_lexbuf in
    let lexbuf = Lexing.from_string intf_source in
    Location.input_lexbuf := Some lexbuf;
    Location.init lexbuf ("//" ^ cu_name ^ "//");
    let ast = Parse.interface lexbuf in
    Location.input_lexbuf := saved_input_lexbuf;
    let env = Compmisc.initial_env () in
    let tsg =
      Typemod.type_interface ~sourcefile:(cu_name ^ ".mli") compilation_unit env
        ast
    in
    (* Apply the same normalisations as saving a cmi would (localised idents,
       normalised sort variables, ...), and force the result so that no pending
       [Saving] substitution remains when the persistent-env loader applies its
       own [Loading] substitution. *)
    Btype.cleanup_abbrev ();
    Subst.reset_additional_action_id ();
    let lazy_sign =
      Subst.Lazy.of_signature tsg.Typedtree.sig_type
      |> Subst.Lazy.signature Make_local
           (Subst.with_additional_action Prepare_for_saving Subst.identity)
      |> Subst.Lazy.force_signature |> Subst.Lazy.of_signature
    in
    let cmi : Cmi_format.cmi_infos_lazy =
      { cmi_name = Compilation_unit.name compilation_unit;
        cmi_kind = Normal { cmi_impl = compilation_unit; cmi_arg_for = None };
        cmi_globals = [||];
        cmi_sign = lazy_sign, Mode.Staticity.Dynamic;
        cmi_params = [];
        cmi_crcs = [||];
        cmi_flags = []
      }
    in
    injector_cmis
      := Compilation_unit.Name.Map.add
           (Compilation_unit.name compilation_unit)
           cmi !injector_cmis;
    let export_info, file_sections =
      build_injector_export_info entries compilation_unit
    in
    let unit_infos : Cmx_format.unit_infos =
      { ui_unit = compilation_unit;
        ui_defines = [compilation_unit];
        ui_arg_descr = None;
        ui_imports_cmi = [];
        ui_imports_cmx = [];
        ui_quoted_cmi = [];
        ui_quoted_cmx = [];
        ui_format =
          Mb_struct
            { mb_repr = Module_value_only { field_count = List.length entries }
            };
        ui_generic_fns = { curry_fun = []; apply_fun = []; send_fun = [] };
        ui_export_info = export_info;
        ui_zero_alloc_info = Zero_alloc_info.create ();
        ui_force_link = false;
        ui_requires_metaprogramming = false;
        ui_external_symbols = [];
        ui_file_sections = file_sections
      }
    in
    Compilenv.cache_unit_info unit_infos;
    match Sys.getenv_opt "OCAML_EVAL_DEBUG_INJECT" with
    | Some ("true" | "1") -> (
      let loader =
        Flambda2_cmx.Flambda_cmx.create_loader
          ~get_module_info:Compilenv.get_unit_export_info
      in
      match
        Flambda2_cmx.Flambda_cmx.load_cmx_file_contents loader compilation_unit
      with
      | None ->
        Format.eprintf "inject: self-check: no cmx contents for %s@." cu_name
      | Some env ->
        Format.eprintf "inject: self-check: typing env for %s:@ %a@." cu_name
          Flambda2_types.Typing_env.Serializable.print env)
    | None | Some _ -> ())

(* Debugging aid: [OCAML_EVAL_DUMP] is a comma-separated list of intermediate
   representations to dump (to stderr) during the runtime compilation of
   evaluated quotes, e.g. OCAML_EVAL_DUMP=rawflambda,flambda. Supported entries:
   "rawflambda" (Flambda 2 after closure conversion, which is the final Flambda
   2 IR in classic mode); "flambda" (after simplification); "cmm". Useful e.g.
   for checking whether the approximations of injected values are being used
   (direct calls or inlining of injected functions rather than indirect
   applications). *)
let set_dump_flags_from_env () =
  match Sys.getenv_opt "OCAML_EVAL_DUMP" with
  | None | Some "" -> false
  | Some spec ->
    let enable_one name =
      match name with
      | "" -> ()
      | "rawflambda" -> Clflags.dump_rawflambda := true
      | "flambda" -> Clflags.dump_flambda := true
      | "cmm" -> Clflags.dump_cmm := true
      | _ ->
        Printf.eprintf "Eval: ignoring unknown OCAML_EVAL_DUMP entry %S\n%!"
          name
    in
    List.iter enable_one (String.split_on_char ',' spec);
    true

let counter = ref 0

let eval0 ~(injector : CamlinternalQuote.Injector.t option)
    (code : CamlinternalQuote.Code.t) : Obj.t =
  (* TODO: assert the JIT is supported *)
  let id = !counter in
  incr counter;
  if
    id = 0
    && not (Opttoploop.using_existing_compilerlibs_state_for_artifacts ())
  then read_bundles_from_exe ();
  (* TODO: reset all the things *)
  (* TODO: these flags should maybe be snapshotted and restored *)
  Clflags.no_cwd := true;
  Clflags.native_code := true;
  Clflags.dont_write_files := true;
  Clflags.shared := true;
  Clflags.dlcode := false;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  Clflags.set_o3 ();
  (* We need this in case the quote contains unused module aliases that point to
     modules we don't have the CMI for. It's weird but it would compile if the
     initial compile also had this set, and setting this doesn't hurt. *)
  Clflags.no_alias_deps := true;
  (* ensure Stdlib is linked during eval *)
  Clflags.nopervasives := false;
  Clflags.no_std_include := false;
  (* TODO: Set a bunch of flags to match the initial compile; nopervasives is
     false to ensure Stdlib is available *)
  Location.reset ();
  Env.reset_cache ~preserve_persistent_env:true;
  install_persistent_signature_load_hook ();
  (* TODO: set commandline flags *)
  (* Compilation happens here during partial application, not when thunk is
     called *)
  let code = CamlinternalQuote.Code.Closed.close code in
  let exp = CamlinternalQuote.Code.Closed.to_exp code in
  let code_string =
    Format.asprintf "let eval = (%a)" CamlinternalQuote.Exp.print exp
  in
  let lexbuf = Lexing.from_string code_string in
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf "//eval//";
  let ast = Parse.implementation lexbuf in
  (* Unlikely to clash, might be too weird. *)
  let input_name = Printf.sprintf "Eval___%i" id in
  let compilation_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string input_name)
  in
  let unit_info = Unit_info.make_dummy ~input_name compilation_unit in
  Compilenv.reset unit_info
  (* TODO: It would be nice to not reset everything here so we don't have to
     refill the cache. *);
  let _ =
    List.for_all
      (fun (info : Cmx_format.unit_infos) ->
        Compilenv.cache_unit_info info;
        true)
      !cmxs
  in
  (* Set up the injector's compilation unit, if any. (This must come after
     [Compilenv] has been populated above: recovering the code referenced by the
     injected values' approximations consults the cmx cache.) *)
  Option.iter setup_injector_unit injector;
  let env = Compmisc.initial_env () in
  let typed_impl =
    Typemod.type_implementation unit_info compilation_unit env ast
  in
  let tlambda_program =
    Translmod.transl_implementation compilation_unit ~loc:(Location.curr lexbuf)
      ( typed_impl.structure,
        typed_impl.coercion,
        Option.map
          (fun (ai : Typedtree.argument_interface) ->
            ai.ai_coercion_from_primary)
          typed_impl.argument_interface )
  in
  Warnings.check_fatal () (* TODO: more error handling? *);
  (* TODO: assert program.arg_block_idx is none? *)
  (* We ignore the comptime bit here because eval'd stuff is dynamic, we could
     consider packaging the comptime component up in the result if the quoted
     mode is static, which would let us do something like:
     [{
      val eval : <[ 'a @ static ]> expr -> <[ 'a ]> eval with_static_data
      val inject
        :  (('a. 'a with_static_data -> <[ 'a @ static ]> expr) -> 'b expr)
        -> 'b eval
     }] *)
  let lambda =
    let { Slambda.slv_comptime = _; slv_runtime = raw_lambda } =
      Slambda.eval Fun.id tlambda_program.code
    in
    Simplif.simplify_lambda
      ~restrict_to_upstream_dwarf:!Dwarf_flags.restrict_to_upstream_dwarf
      ~gdwarf_may_alter_codegen:!Dwarf_flags.gdwarf_may_alter_codegen
      raw_lambda
  in
  let program = { tlambda_program with code = lambda } in
  let ppf =
    if set_dump_flags_from_env ()
    then Format.err_formatter
    else
      (* A formatter that eats everything, since no dumps are enabled. *)
      Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
  in
  (match Jit.jit_load_program ~phrase_name:input_name ppf program with
  | Result _ -> ()
  | Exception exn -> raise exn);
  let linkage_name =
    Symbol.for_compilation_unit compilation_unit
    |> Symbol.linkage_name |> Linkage_name.to_string
  in
  let struct_obj =
    match Jit.jit_lookup_symbol linkage_name with
    | Some struct_obj -> struct_obj
    | None ->
      failwith
        ("Cannot find module block symbol '" ^ linkage_name
       ^ "' which should have been output by the JIT")
  in
  let obj = Obj.field struct_obj 0 in
  obj

let compile_mutex = Mutex.create ()

let protected_eval ~injector (code : CamlinternalQuote.Code.t) : Obj.t =
  Mutex.protect compile_mutex (fun () ->
      (* TODO: Consider if some warnings are important enough to show. *)
      let without_warnings f =
        (* Debugging aid: show the warnings of the runtime compilation of
           evaluated quotes (e.g. warning 55 from [@inlined] on calls to
           injected functions whose approximations are unknown). *)
        match Sys.getenv_opt "OCAML_EVAL_SHOW_WARNINGS" with
        | Some ("true" | "1") -> f ()
        | None | Some _ -> Warnings.without_warnings f
      in
      try without_warnings (fun () -> eval0 ~injector code)
      with exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        Location.report_exception Format.std_formatter exn;
        Printexc.raise_with_backtrace exn backtrace)

let[@inline never] eval (expr : 'a expr) : 'a eval =
  let code : CamlinternalQuote.Code.t = Obj.magic (Obj.magic_many expr) in
  Obj.obj (protected_eval ~injector:None code)

let[@inline never] eval_with_injector f =
  let injector = CamlinternalQuote.Injector.create () in
  let expr = f injector in
  let code : CamlinternalQuote.Code.t = Obj.magic (Obj.magic_many expr) in
  Obj.obj (protected_eval ~injector:(Some injector) code)
