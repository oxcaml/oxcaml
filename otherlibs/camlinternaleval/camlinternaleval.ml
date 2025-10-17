(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(* CR metaprogramming jrickard: This file has not been code reviewed *)

(* CR mshinwell: It seems like this file is running into similar issues to the
   Dynlink code, whereby state in compilerlibs needs to be updated, meaning that
   it could conflict with other use of compilerlibs in an application. That
   said, we're relying on using the same compilerlibs state for .cmi and .cmx
   lookups via this module when called from mdx, instead of using bundles. *)

module type Jit_intf = sig
  val jit_load :
    phrase_name:string ->
    Format.formatter ->
    Lambda.program ->
    (Obj.t, exn) Result.t

  val jit_lookup_symbol : string -> Obj.t option
end

module Default_jit = struct
  let jit_load ~phrase_name fmt prog : _ Result.t =
    match Jit.jit_load ~phrase_name fmt prog with
    | Result obj -> Ok obj
    | Exception exn -> Error exn

  let jit_lookup_symbol = Jit.jit_lookup_symbol
end

let jit = ref (module Default_jit : Jit_intf)

let set_jit new_jit = jit := new_jit

module Jit = struct
  let jit_load ~phrase_name fmt prog =
    let module Jit = (val !jit : Jit_intf) in
    Jit.jit_load ~phrase_name fmt prog

  let jit_lookup_symbol sym =
    let module Jit = (val !jit : Jit_intf) in
    Jit.jit_lookup_symbol sym
end

type bundle = private string

external bundled_cmis_this_exe : unit -> bundle = "caml_bundled_cmis_this_exe"

external bundled_cmxs_this_exe : unit -> bundle = "caml_bundled_cmxs_this_exe"

external bundle_not_available : bundle -> bool = "caml_bundle_not_available"

let find_bundle_in_exe ~ext get_this_exe =
  let bundle = get_this_exe () in
  if bundle_not_available bundle
  then
    failwith
      ("Executable does not contain ." ^ ext
     ^ " bundle and [use_existing_compilerlibs_state_for_artifacts]"
     ^ " has not been called")
  else bundle

let cmis = ref Compilation_unit.Name.Map.empty

let cmxs = ref []

let read_bundles ~marshalled_cmi_bundle ~marshalled_cmx_bundle =
  let bundled_cmis : Cmi_format.cmi_infos Compilation_unit.Name.Map.t =
    Marshal.from_string marshalled_cmi_bundle 0
  in
  let new_cmis =
    Compilation_unit.Name.Map.map
      (fun (cmi : Cmi_format.cmi_infos) : Cmi_format.cmi_infos_lazy ->
        { cmi with cmi_sign = Subst.Lazy.of_signature cmi.cmi_sign })
      bundled_cmis
  in
  let bundled_cmxs : (Cmx_format.unit_infos_raw * string array) list =
    Marshal.from_string marshalled_cmx_bundle 0
  in
  let new_cmxs =
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
          { ui_unit = uir.uir_unit;
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
            ui_external_symbols = uir.uir_external_symbols |> Array.to_list
          }
        in
        ui)
      bundled_cmxs
  in
  cmis := new_cmis;
  cmxs := new_cmxs

let use_existing_compilerlibs_state_for_artifacts = ref false

let read_bundles_from_exe () =
  assert (not !use_existing_compilerlibs_state_for_artifacts);
  let marshalled_cmi_bundle =
    find_bundle_in_exe ~ext:"cmi" bundled_cmis_this_exe
  in
  let marshalled_cmx_bundle =
    find_bundle_in_exe ~ext:"cmx" bundled_cmxs_this_exe
  in
  let marshalled_cmi_bundle = (marshalled_cmi_bundle :> string) in
  let marshalled_cmx_bundle = (marshalled_cmx_bundle :> string) in
  read_bundles ~marshalled_cmi_bundle ~marshalled_cmx_bundle

let counter = ref 0

let eval code =
  (* TODO: assert Linux x86-64 *)
  let id = !counter in
  incr counter;
  if id = 0 && not !use_existing_compilerlibs_state_for_artifacts
  then read_bundles_from_exe ();
  (* TODO: reset all the things *)
  (* CR mshinwell: I think these flags should maybe be snapshotted and restored *)
  Clflags.no_cwd := true;
  Clflags.native_code := true;
  Clflags.dont_write_files := true;
  Clflags.shared := true;
  Clflags.dlcode := false;
  Location.reset ();
  Env.reset_cache ~preserve_persistent_env:true;
  (* TODO: set commandline flags *)

  (* Compilation happens here during partial application, not when thunk is
     called *)
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
      (fun (info : Cmx_format.unit_infos) ->
        Compilenv.cache_unit_info info;
        true)
      !cmxs
  in
  (if not !use_existing_compilerlibs_state_for_artifacts
  then
    Persistent_env.Persistent_signature.load
      := fun ~allow_hidden:_ ~unit_name ->
           Option.map
             (fun cmi ->
               { Persistent_env.Persistent_signature.filename =
                   Compilation_unit.Name.to_string unit_name;
                 cmi;
                 visibility = Visible
               })
             (Compilation_unit.Name.Map.find_opt unit_name !cmis));
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
  let program =
    { program with
      code =
        Simplif.simplify_lambda
          ~restrict_to_upstream_dwarf:!Dwarf_flags.restrict_to_upstream_dwarf
          ~gdwarf_may_alter_codegen:!Dwarf_flags.gdwarf_may_alter_codegen
          program.code
    }
  in
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  (match Jit.jit_load ~phrase_name:input_name ppf program with
  | Ok _ -> ()
  | Error exn -> raise exn);
  (* Compilenv.save_unit_info (Unit_info.Artifact.filename (Unit_info.cmx
     unit_info)) ~main_module_block_format:program.main_module_block_format
     ~arg_descr:None; *)
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

let use_existing_compilerlibs_state_for_artifacts () =
  use_existing_compilerlibs_state_for_artifacts := true
