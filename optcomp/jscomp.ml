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
  | Jsir_file_not_found of string
  | Unit_not_in_object_file of
      { compilation_unit : string;
        object_file : string
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

(* The files of a target that correspond to the native backend's. *)
type extensions =
  { obj : string;
    lib : string;
    flambda_obj : string;
    flambda_lib : string;
    ir : string;
    ir_lib : string;
    runtime : string
  }

let extensions : Clflags.Jsir_target.t -> extensions = function
  | Js ->
    { obj = ".cmo-js";
      lib = ".cma-js";
      flambda_obj = ".cmx-js";
      flambda_lib = ".cmxa-js";
      ir = ".cm-jsir-js";
      ir_lib = ".cma-jsir-js";
      runtime = ".runtime.js"
    }
  | Wasm ->
    { obj = ".cmo-wasm";
      lib = ".cma-wasm";
      flambda_obj = ".cmx-wasm";
      flambda_lib = ".cmxa-wasm";
      ir = ".cm-jsir-wasm";
      ir_lib = ".cma-jsir-wasm";
      runtime = ".runtime.wasma"
    }

(* JSIR files (.cm-jsir-js or .cm-jsir-wasm) and archives of them (.cma-jsir-js
   or .cma-jsir-wasm): a magic number followed by a marshaled value. *)

(* [Config] has no magic number for JSIR archives. This one has the version of
   the one for JSIR files. *)
let jsir_archive_magic_number =
  let version_length = 3 in
  "Caml1999K"
  ^ String.sub Config.cmj_magic_number
      (String.length Config.cmj_magic_number - version_length)
      version_length

let jsir_target : Clflags.Jsir_target.t -> Jsoo_imports.Code.jsir_target =
  function
  | Js -> Js
  | Wasm -> Wasm

let jsir_file ~target ~compilation_unit ~imported_compilation_units
    ~symbol_imports ~symbol_exports program : Jsoo_imports.Code.jsir_file =
  { target = jsir_target target;
    program = Jsoo_imports.Code.Marshalable_program.of_program program;
    (* [Var] numbers variables with mutable state. js_of_ocaml needs to know the
       highest number used so that the variables it creates do not clash with
       these. *)
    last_var = Jsoo_imports.Code.Var.idx (Jsoo_imports.Code.Var.last ());
    imported_compilation_units;
    exported_compilation_unit = compilation_unit;
    force_link = !Clflags.link_everything;
    symbol_imports;
    symbol_exports
  }

let write_marshaled ~magic_number ~filename value =
  let oc = open_out_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
      output_string oc magic_number;
      output_value oc value)

let read_marshaled ~magic_number ~filename =
  if not (Sys.file_exists filename)
  then raise (Error (Jsir_file_not_found filename));
  let ic = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
      let magic =
        try really_input_string ic (String.length magic_number)
        with End_of_file -> ""
      in
      if not (String.equal magic magic_number)
      then raise (Linkenv.Error (Not_an_object_file filename));
      input_value ic)

let write_jsir_file ~filename (file : Jsoo_imports.Code.jsir_file) =
  write_marshaled ~magic_number:Config.cmj_magic_number ~filename file

let read_jsir_file ~filename : Jsoo_imports.Code.jsir_file =
  read_marshaled ~magic_number:Config.cmj_magic_number ~filename

let write_jsir_archive ~filename (files : Jsoo_imports.Code.jsir_file list) =
  write_marshaled ~magic_number:jsir_archive_magic_number ~filename files

let read_jsir_archive ~filename : Jsoo_imports.Code.jsir_file list =
  read_marshaled ~magic_number:jsir_archive_magic_number ~filename

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

(* The name of the unit js_of_ocaml compiles when linking a whole program.
   wasm_of_ocaml uses it to name the initialisation function of the module. *)
let program_unit_name output_name =
  Filename.basename output_name
  |> Filename.remove_extension
  |> String.map (fun c ->
      match c with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> c | _ -> '_')

(* The stubs to pass to [build-runtime]. As with C stubs, stub names are passed
   on exactly as they were recorded in the library files or given on the command
   line. Several libraries may record the same stub file; js_of_ocaml accepts
   duplicates but there is no point in passing them. The first occurrence is
   kept. *)
let stubs () =
  List.rev !Clflags.ccobjs
  |> List.fold_left
       (fun (seen, acc) stub ->
         if Misc.Stdlib.String.Set.mem stub seen
         then seen, acc
         else Misc.Stdlib.String.Set.add stub seen, stub :: acc)
       (Misc.Stdlib.String.Set.empty, [])
  |> snd |> List.rev

let linkall_args () = if !Clflags.link_everything then ["--linkall"] else []

let make (target : Clflags.Jsir_target.t) ~link_jsir
    ~(lambda_to_jsir :
       ppf_dump:Format.formatter ->
       prefixname:string ->
       keep_symbol_tables:bool ->
       Lambda.program ->
       Optcomp_intf.jsir_program)
    ~(jsir_linker :
       Jsoo_imports.Code.jsir_file list -> Jsoo_imports.Code.program) =
  let extensions = extensions target in
  let program_name = Clflags.Jsir_target.program_name target in
  (module Optcompile.Make (struct
    let backend : Compile_common.opt_backend =
      match target with Js -> Js_of_ocaml | Wasm -> Wasm_of_ocaml

    let supports_metaprogramming = false

    (* When linking whole programs, the object files are the JSIR files. *)
    let ext_obj = if link_jsir then extensions.ir else extensions.obj

    let ext_lib = if link_jsir then extensions.ir_lib else extensions.lib

    let ext_flambda_obj = extensions.flambda_obj

    let ext_flambda_lib = extensions.flambda_lib

    let default_executable_name = Config.default_executable_name ^ ".js"

    let emit = None

    let compile_implementation ~keep_symbol_tables ~sourcefile:_ ~prefixname
        ~ppf_dump (program : Lambda.program) =
      Compilation_unit.Set.iter Compilenv.require_global
        program.required_globals;
      let { Optcomp_intf.program = jsir;
            imported_compilation_units;
            symbol_imports;
            symbol_exports
          } =
        lambda_to_jsir ~ppf_dump ~prefixname ~keep_symbol_tables program
        |> Misc.print_if ppf_dump Clflags.dump_jsir
             (fun ppf ({ program; _ } : Optcomp_intf.jsir_program) ->
               Jsoo_imports.Code.Print.program ppf (fun _ _ -> "") program)
      in
      if Clflags.(should_stop_after Compiler_pass.Middle_end)
      then ()
      else
        let imported_compilation_units =
          Compilation_unit.Set.union imported_compilation_units
            program.required_globals
          |> Compilation_unit.Set.elements
          |> List.map Compilation_unit.full_path_as_string
        in
        let ir_file = prefixname ^ extensions.ir in
        write_jsir_file ~filename:ir_file
          (jsir_file ~target
             ~compilation_unit:
               (Compilation_unit.full_path_as_string program.compilation_unit)
             ~imported_compilation_units ~symbol_imports ~symbol_exports jsir);
        if not link_jsir
        then
          let obj = prefixname ^ extensions.obj in
          Misc.try_finally
            ~exceptionally:(fun () -> Misc.remove_file obj)
            (fun () ->
              Profile.record_call program_name (fun () ->
                  run ~target "compile" ~phase:Compile ~input_left_in:ir_file
                    (debuginfo_args () @ ["-o"; obj; ir_file]));
              if not !Clflags.keep_asm_file then Misc.remove_file ir_file)

    let create_archive archive_name objfiles =
      if link_jsir
      then
        List.map (fun filename -> read_jsir_file ~filename) objfiles
        |> write_jsir_archive ~filename:archive_name
      else
        run ~target "link" ~phase:Archive
          (linkall_args () @ ["-a"; "-o"; archive_name] @ objfiles)

    (* JSOO cannot yet link packed units. *)
    let link_partial _target _objfiles =
      raise (Error (Unsupported { target; option = "-pack" }))

    (* Separate compilation: js_of_ocaml links the JavaScript (WebAssembly) of
       the units. *)
    let link_separately (objfiles : Linkenv.objfile_to_link list) output_name =
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
            (debuginfo_args () @ ["-o"; runtime] @ stubs ());
          run ~target "link" ~phase:Link
            (linkall_args () @ ["-o"; output_name; runtime] @ objfiles))

    (* Whole program: the JSIR of the units is linked into one program, which
       js_of_ocaml compiles and links with the runtime. *)
    let link_jsir_units (objfiles : Linkenv.objfile_to_link list) output_name
        ~(units_tolink : Linkenv.unit_link_info list) =
      (* The JSIR of the units to link: those of the object files, and the
         required members of the archives. *)
      let files = Hashtbl.create 64 in
      List.iter
        (fun ({ path; units } : Linkenv.objfile_to_link) ->
          let units =
            List.map Compilation_unit.full_path_as_string units
            |> Misc.Stdlib.String.Set.of_list
          in
          let members =
            if Filename.check_suffix path ext_lib
            then read_jsir_archive ~filename:path
            else [read_jsir_file ~filename:path]
          in
          List.iter
            (fun (file : Jsoo_imports.Code.jsir_file) ->
              if Misc.Stdlib.String.Set.mem file.exported_compilation_unit units
              then Hashtbl.replace files file.exported_compilation_unit file)
            members)
        objfiles;
      (* In initialisation order. *)
      let files =
        List.map
          (fun ({ name; file_name; _ } : Linkenv.unit_link_info) ->
            let compilation_unit = Compilation_unit.full_path_as_string name in
            match Hashtbl.find_opt files compilation_unit with
            | Some file -> file
            | None ->
              let object_file =
                Filename.remove_extension file_name
                ^
                if Filename.check_suffix file_name ext_flambda_lib
                then ext_lib
                else ext_obj
              in
              raise
                (Error
                   (Unit_not_in_object_file { compilation_unit; object_file })))
          units_tolink
      in
      let program =
        Profile.record_call "jsir_link" (fun () -> jsir_linker files)
      in
      let imported_compilation_units =
        List.fold_left
          (fun imports (file : Jsoo_imports.Code.jsir_file) ->
            Misc.Stdlib.String.Set.add_seq
              (List.to_seq file.imported_compilation_units)
              imports)
          Misc.Stdlib.String.Set.empty files
      in
      let imported_compilation_units =
        List.fold_left
          (fun imports (file : Jsoo_imports.Code.jsir_file) ->
            Misc.Stdlib.String.Set.remove file.exported_compilation_unit imports)
          imported_compilation_units files
        |> Misc.Stdlib.String.Set.elements
      in
      let ir_file = output_name ^ extensions.ir in
      let obj = output_name ^ extensions.obj in
      let runtime = output_name ^ extensions.runtime in
      (* The JSIR file is left in place when js_of_ocaml fails on it. *)
      Misc.try_finally
        ~always:(fun () ->
          Misc.remove_file obj;
          if not !Clflags.keep_startup_file then Misc.remove_file runtime)
        ~exceptionally:(fun () -> remove_link_outputs ~target output_name)
        (fun () ->
          write_jsir_file ~filename:ir_file
            (jsir_file ~target
               ~compilation_unit:(program_unit_name output_name)
               ~imported_compilation_units ~symbol_imports:[] ~symbol_exports:[]
               program);
          Profile.record_call program_name (fun () ->
              run ~target "compile" ~phase:Compile ~input_left_in:ir_file
                (debuginfo_args () @ ["-o"; obj; ir_file]);
              run ~target "build-runtime" ~phase:Runtime
                (debuginfo_args () @ ["-o"; runtime] @ stubs ());
              run ~target "link" ~phase:Link
                (linkall_args () @ ["-o"; output_name; runtime; obj]));
          if not !Clflags.keep_asm_file then Misc.remove_file ir_file)

    let link _linkenv objfiles output_name ~cached_genfns_imports:_ ~genfns:_
        ~units_tolink ~uses_eval:_ ~quoted_cmi:_ ~quoted_cmx:_ ~ppf_dump:_ =
      if link_jsir
      then link_jsir_units objfiles output_name ~units_tolink
      else link_separately objfiles output_name

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
  | Jsir_file_not_found file ->
    Format_doc.fprintf ppf
      "@[<hov>Cannot find JSIR file %a@ (was it built with -link-jsir?)@]"
      Location.Doc.quoted_filename file
  | Unit_not_in_object_file { compilation_unit; object_file } ->
    Format_doc.fprintf ppf
      "@[<hov>The object file %a@ does not contain the compilation unit %s@]"
      Location.Doc.quoted_filename object_file compilation_unit

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)

let report_error = Format_doc.compat report_error_doc
