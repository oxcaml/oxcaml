(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

type error =
  | Program_not_found of
      { target : Clflags.Jsir_target.t;
        program : string
      }
  | Program_error of
      { target : Clflags.Jsir_target.t;
        subcommand : string;
        exit_code : int;
        input_left_in : string option
      }
  | Unsupported of
      { target : Clflags.Jsir_target.t;
        option : string
      }

exception Error of error

let env_var : Clflags.Jsir_target.t -> string = function
  | Js -> "OXCAML_JS_OF_OCAML"
  | Wasm -> "OXCAML_WASM_OF_OCAML"

let program target =
  match Sys.getenv_opt (env_var target) with
  | Some path -> path
  | None ->
    let name = Clflags.Jsir_target.program_name target in
    let in_bindir = Filename.concat Config.bindir name in
    if Sys.file_exists in_bindir then in_bindir else name

let target_args : Clflags.Jsir_target.t -> string list = function
  | Js -> []
  | Wasm -> ["--enable=portable-int"]

let effects_args ~target ~(phase : Clflags.Jsoo_phase.t) passthrough_args =
  match phase with
  | Archive | Link | All -> []
  | Compile | Runtime ->
    let effects =
      match Clflags.jsoo_effects () with
      | Some effects -> effects
      | None ->
        Misc.fatal_error "Jscomp.effects_args: the target is not a JSIR one"
    in
    (* [Optmaindriver] checks this too, but [OCAMLPARAM] can set [jsoo-effects]
       after the backend has been chosen. *)
    if not (List.mem effects (Clflags.Jsoo_effects.supported_by target))
    then
      raise
        (Error
           (Unsupported
              { target;
                option =
                  "-jsoo-effects " ^ Clflags.Jsoo_effects.to_string effects
              }));
    if List.exists (String.starts_with ~prefix:"--effects") passthrough_args
    then []
    else ["--effects=" ^ Clflags.Jsoo_effects.to_string effects]

let run ?input_left_in ~target ~(phase : Clflags.Jsoo_phase.t) subcommand args =
  let passthrough_args =
    List.rev !(Clflags.jsoo_opts All) @ List.rev !(Clflags.jsoo_opts phase)
  in
  let args =
    (subcommand :: args) @ target_args target
    @ effects_args ~target ~phase passthrough_args
    @ passthrough_args
  in
  let program = program target in
  let cmdline = Filename.quote_command program args in
  match Ccomp.command cmdline with
  | 0 -> ()
  | exit_code ->
    raise
      (Error (Program_error { target; subcommand; exit_code; input_left_in }))
  | exception Sys_error _ ->
    raise (Error (Program_not_found { target; program }))

let debuginfo_args () = if !Clflags.debug then ["--debuginfo"] else []

let write_jsir_file ~(target : Clflags.Jsir_target.t) ~filename
    ~compilation_unit ~required_globals
    ({ program; imported_compilation_units } : Optcomp_intf.jsir_program) =
  let imported_compilation_units =
    Compilation_unit.Set.union imported_compilation_units required_globals
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
      output_string oc Config.cmj_magic_number;
      let body : Jsoo_imports.Code.jsir_file =
        { target = (match target with Js -> Js | Wasm -> Wasm);
          program = Jsoo_imports.Code.Marshalable_program.of_program program;
          (* [Var] numbers variables with mutable state. js_of_ocaml needs to
             know the highest number used so that the variables it creates do
             not clash with these. *)
          last_var = Jsoo_imports.Code.Var.idx (Jsoo_imports.Code.Var.last ());
          imported_compilation_units =
            Compilation_unit.Set.elements imported_compilation_units
            |> List.map Compilation_unit.full_path_as_string;
          exported_compilation_unit =
            Compilation_unit.full_path_as_string compilation_unit;
          force_link = !Clflags.link_everything
        }
      in
      output_value oc body)

(* The files of a target that correspond to the native backend's. *)
type extensions =
  { obj : string;
    lib : string;
    flambda_obj : string;
    flambda_lib : string;
    ir : string;
    runtime : string
  }

let extensions : Clflags.Jsir_target.t -> extensions = function
  | Js ->
    { obj = ".cmo-js";
      lib = ".cma-js";
      flambda_obj = ".cmx-js";
      flambda_lib = ".cmxa-js";
      ir = ".cm-jsir-js";
      runtime = ".runtime.js"
    }
  | Wasm ->
    { obj = ".cmo-wasm";
      lib = ".cma-wasm";
      flambda_obj = ".cmx-wasm";
      flambda_lib = ".cmxa-wasm";
      ir = ".cm-jsir-wasm";
      runtime = ".runtime.wasma"
    }

(* In wasm_of_ocaml, linking an executable creates a directory that we should
   clean up after. *)
let remove_link_outputs ~(target : Clflags.Jsir_target.t) output_name =
  Misc.remove_file output_name;
  match target with
  | Js -> ()
  | Wasm ->
    let base =
      try Filename.chop_extension output_name
      with Invalid_argument _ -> output_name
    in
    Misc.remove_dir_contents (base ^ ".assets")

let make (target : Clflags.Jsir_target.t)
    ~(lambda_to_jsir :
       ppf_dump:Format.formatter ->
       prefixname:string ->
       keep_symbol_tables:bool ->
       Lambda.program ->
       Optcomp_intf.jsir_program) =
  let extensions = extensions target in
  let program_name = Clflags.Jsir_target.program_name target in
  (module Optcompile.Make (struct
    let backend : Compile_common.opt_backend =
      match target with Js -> Js_of_ocaml | Wasm -> Wasm_of_ocaml

    let supports_metaprogramming = false

    let ext_obj = extensions.obj

    let ext_lib = extensions.lib

    let ext_flambda_obj = extensions.flambda_obj

    let ext_flambda_lib = extensions.flambda_lib

    let default_executable_name = Config.default_executable_name ^ ".js"

    let emit = None

    let compile_implementation ~keep_symbol_tables ~sourcefile:_ ~prefixname
        ~ppf_dump (program : Lambda.program) =
      Compilation_unit.Set.iter Compilenv.require_global
        program.required_globals;
      let jsir =
        lambda_to_jsir ~ppf_dump ~prefixname ~keep_symbol_tables program
        |> Misc.print_if ppf_dump Clflags.dump_jsir
             (fun ppf ({ program; _ } : Optcomp_intf.jsir_program) ->
               Jsoo_imports.Code.Print.program ppf (fun _ _ -> "") program)
      in
      if Clflags.(should_stop_after Compiler_pass.Middle_end)
      then ()
      else
        let ir_file = prefixname ^ extensions.ir in
        let obj = prefixname ^ ext_obj in
        write_jsir_file ~target ~filename:ir_file
          ~compilation_unit:program.compilation_unit
          ~required_globals:program.required_globals jsir;
        let remove_ir_file () =
          if not !Clflags.keep_asm_file then Misc.remove_file ir_file
        in
        Misc.try_finally
          ~exceptionally:(fun () -> Misc.remove_file obj)
          (fun () ->
            Profile.record_call program_name (fun () ->
                run ~target "compile" ~phase:Compile ~input_left_in:ir_file
                  (debuginfo_args () @ ["-o"; obj; ir_file]));
            remove_ir_file ())

    let create_archive archive_name objfiles =
      let linkall = if !Clflags.link_everything then ["--linkall"] else [] in
      run ~target "link" ~phase:Archive
        (linkall @ ["-a"; "-o"; archive_name] @ objfiles)

    (* JSOO cannot yet link packed units. *)
    let link_partial _target _objfiles =
      raise (Error (Unsupported { target; option = "-pack" }))

    let link _linkenv (objfiles : Linkenv.objfile_to_link list) output_name
        ~cached_genfns_imports:_ ~genfns:_ ~units_tolink:_ ~uses_eval:_
        ~quoted_cmi:_ ~quoted_cmx:_ ~ppf_dump:_ =
      (* As with C stubs, stub names are passed on exactly as they were recorded
         in the library files or given on the command line. Several libraries
         may record the same stub file; js_of_ocaml accepts duplicates but there
         is no point in passing them. The first occurrence is kept. *)
      let stubs =
        List.rev !Clflags.ccobjs
        |> List.fold_left
             (fun (seen, acc) stub ->
               if Misc.Stdlib.String.Set.mem stub seen
               then seen, acc
               else Misc.Stdlib.String.Set.add stub seen, stub :: acc)
             (Misc.Stdlib.String.Set.empty, [])
        |> snd |> List.rev
      in
      let objfiles =
        List.map (fun ({ path; _ } : Linkenv.objfile_to_link) -> path) objfiles
      in
      let runtime = output_name ^ extensions.runtime in
      Misc.try_finally
        ~always:(fun () ->
          if not !Clflags.keep_startup_file then Misc.remove_file runtime)
        ~exceptionally:(fun () -> remove_link_outputs ~target output_name)
        (fun () ->
          run ~target "build-runtime" ~phase:Runtime
            (debuginfo_args () @ ["-o"; runtime] @ stubs);
          let linkall =
            if !Clflags.link_everything then ["--linkall"] else []
          in
          run ~target "link" ~phase:Link
            (linkall @ ["-o"; output_name; runtime] @ objfiles))

    let link_shared _objfiles _output_name ~genfns:_ ~units_tolink:_ ~ppf_dump:_
        =
      raise (Error (Unsupported { target; option = "-shared" }))

    let support_files_for_eval () = []

    let set_load_path_for_eval () = ()
  end) : Optcompile.S)

let report_error_doc ppf = function
  | Program_not_found { target; program } ->
    Format_doc.fprintf ppf
      "@[<hov>Cannot run %s (%a):@ it must be in %a or in the PATH,@ or the %s \
       environment variable must give its path@]"
      (Clflags.Jsir_target.program_name target)
      Location.Doc.quoted_filename program Location.Doc.quoted_filename
      Config.bindir (env_var target)
  | Program_error { target; subcommand; exit_code; input_left_in } -> (
    Format_doc.fprintf ppf "Error while running %s %s (exit code %d)"
      (Clflags.Jsir_target.program_name target)
      subcommand exit_code;
    match input_left_in with
    | None -> ()
    | Some file ->
      Format_doc.fprintf ppf ",@ input left in file %a"
        Location.Doc.quoted_filename file)
  | Unsupported { target; option } ->
    Format_doc.fprintf ppf "%s is not supported when targeting %s" option
      (Clflags.Jsir_target.program_name target)

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)

let report_error = Format_doc.compat report_error_doc
