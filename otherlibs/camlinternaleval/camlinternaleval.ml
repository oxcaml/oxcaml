(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Implementation of [%eval]                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type compiler_settings = {
  debug : bool;
  unsafe : bool;
  noassert : bool;
  native_code : bool;
}

let () =
  Clflags.no_cwd := true;
  Clflags.native_code := true;
  Clflags.dont_write_files := true;
  Clflags.shared := true;
  Clflags.dlcode := false

external bundled_cmis : unit -> string = "bundled_cmis"
external bundled_cmxs : unit -> string = "bundled_cmxs"

let () =
  let marshaled = bundled_cmis () in
  let bundled_cmis : Cmi_format.cmi_infos Compilation_unit.Name.Map.t =
    Marshal.from_string marshaled 0
  in
  let bundled_cmis =
    Compilation_unit.Name.Map.map
      (fun (cmi : Cmi_format.cmi_infos) : Cmi_format.cmi_infos_lazy ->
        { cmi with cmi_sign = Subst.Lazy.of_signature cmi.cmi_sign })
      bundled_cmis
  in
  Persistent_env.Persistent_signature.load :=
    fun ~allow_hidden:_ ~unit_name ->
      Option.map
        (fun cmi ->
          {
            Persistent_env.Persistent_signature.filename =
              Compilation_unit.Name.to_string unit_name;
            cmi;
            visibility = Visible;
          })
        (Compilation_unit.Name.Map.find_opt unit_name bundled_cmis)

let cmxs =
  let marshaled = bundled_cmxs () in
  let bundled_cmxs : (Cmx_format.unit_infos_raw * string array) list =
    Marshal.from_string marshaled 0
  in
  List.map
    (fun ((uir, sections) : Cmx_format.unit_infos_raw * _) ->
      let sections =
        Oxcaml_utils.File_sections.from_array
          (Array.map (fun s -> Marshal.from_string s 0) sections)
      in
      let export_info =
        Option.map
          (Flambda2_cmx.Flambda_cmx_format.from_raw ~sections)
          uir.uir_export_info
      in
      let ui : Cmx_format.unit_infos =
        {
          ui_unit = uir.uir_unit;
          ui_defines = uir.uir_defines;
          ui_format = uir.uir_format;
          ui_arg_descr = uir.uir_arg_descr;
          ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
          ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
          ui_quoted_globals = uir.uir_quoted_globals |> Array.to_list;
          ui_generic_fns = uir.uir_generic_fns;
          ui_export_info = export_info;
          ui_zero_alloc_info = Zero_alloc_info.of_raw uir.uir_zero_alloc_info;
          ui_force_link = uir.uir_force_link;
          ui_external_symbols = uir.uir_external_symbols |> Array.to_list;
        }
      in
      ui)
    bundled_cmxs

let counter = ref 0

let eval code =
  (* TODO: assert Linux x86-64 *)
  let id = !counter in
  incr counter;

  (* TODO: reset all the things *)
  Location.reset ();
  Env.reset_cache ~preserve_persistent_env:true;

  (* TODO: set commandline flags *)

  (* Compilation happens here during partial application, not when thunk is called *)
  let exp = CamlinternalQuote.Code.to_exp code in
  let code_string =
    Format.asprintf "let eval = (%a)" CamlinternalQuote.Exp.print exp
  in
  let lexbuf = Lexing.from_string code_string in
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf "//eval//";

  let ast = Parse.implementation lexbuf in
  (* Definitely won't clash, might be too weird. *)
  let input_name = Printf.sprintf "Eval__%i" id in
  let compilation_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string input_name)
  in
  let unit_info = Unit_info.make_dummy ~input_name compilation_unit in

  (* let () = Compmisc.init_parameters () in *)
  Compilenv.reset unit_info
  (* TODO: It would be nice to not reset everything here so we don't have to
     refill the cache. *);
  let _ =
    List.for_all
      (fun info ->
        Compilenv.cache_unit_info info;
        true)
      cmxs
  in
  let env = Compmisc.initial_env () in
  let typed_impl =
    Typemod.type_implementation unit_info compilation_unit env ast
  in

  let program =
    Translmod.transl_implementation compilation_unit
      ( typed_impl.structure,
        typed_impl.coercion,
        Option.map
          (fun (ai : Typedtree.argument_interface) ->
            ai.ai_coercion_from_primary)
          typed_impl.argument_interface )
  in
  Warnings.check_fatal () (* TODO: more error handling? *);
  (* TODO: assert program.arg_block_idx is none? *)
  let program = { program with code = Simplif.simplify_lambda program.code } in
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  (match Jit.jit_load ~phrase_name:input_name ppf program with
  | Result _ -> ()
  | Exception exn -> raise exn);

  (* Compilenv.save_unit_info
     (Unit_info.Artifact.filename (Unit_info.cmx unit_info))
     ~main_module_block_format:program.main_module_block_format ~arg_descr:None; *)
  let linkage_name =
    Symbol.for_compilation_unit compilation_unit
    |> Symbol.linkage_name |> Linkage_name.to_string
  in
  let obj = Jit.jit_lookup_symbol linkage_name |> Option.get in
  Obj.field obj 0

let compile_mutex = Mutex.create ()

let eval code =
  Mutex.protect compile_mutex (fun () ->
      try eval code
      with exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        Location.report_exception Format.std_formatter exn;
        Printexc.raise_with_backtrace exn backtrace)
