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

(* The js_of_ocaml backend of ocamlopt ([-target js_of_ocaml]).

   Code generation stops at the js_of_ocaml IR (JSIR), produced from Flambda 2
   by [Flambda2_to_jsir]. The JSIR of a compilation unit is written to a [.cmj]
   file, from which the [js_of_ocaml] executable produces JavaScript, much like
   the native backend writes assembly for the assembler to turn into an object
   file. The [.cmj] file is an intermediate like the [.s] file: it is deleted
   unless [-S] is given.

   The correspondence with the native backend's files is:

   - [.cmjx]: unit metadata, in the same format as [.cmx];

   - [.cmjo]: the JavaScript for a unit, analogous to [.o];

   - [.cmjxa] and [.cmja]: libraries, analogous to [.cmxa] and [.a];

   - [.js] files given on the command line: JavaScript stubs, analogous to C
   stubs. They are recorded in [.cmjxa] files and passed to [js_of_ocaml
   build-runtime] when linking an executable. *)

type error =
  | Js_of_ocaml_not_found of string
  | Js_of_ocaml_error of
      { subcommand : string;
        exit_code : int;
        input_left_in : string option
      }
  | Unsupported of string

exception Error of error

let js_of_ocaml_env_var = "OXCAML_JS_OF_OCAML"

let js_of_ocaml_program () =
  match Sys.getenv_opt js_of_ocaml_env_var with
  | Some path -> path
  | None ->
    let in_bindir = Filename.concat Config.bindir "js_of_ocaml" in
    if Sys.file_exists in_bindir then in_bindir else "js_of_ocaml"

(* [phase] selects the [-jsoo-opt-*] options to add to the [-jsoo-opt] ones. *)
let run_js_of_ocaml ?input_left_in ~(phase : Clflags.Jsoo_phase.t) subcommand
    args =
  let args =
    subcommand
    :: (args
       @ List.rev !(Clflags.jsoo_opts All)
       @ List.rev !(Clflags.jsoo_opts phase))
  in
  let program = js_of_ocaml_program () in
  let cmdline = Filename.quote_command program args in
  match Ccomp.command cmdline with
  | 0 -> ()
  | exit_code ->
    raise (Error (Js_of_ocaml_error { subcommand; exit_code; input_left_in }))
  | exception Sys_error _ ->
    (* [Ccomp.command] raises when the shell cannot run the program. *)
    raise (Error (Js_of_ocaml_not_found program))

let debuginfo_args () = if !Clflags.debug then ["--debuginfo"] else []

(* Stubs are stored in [.cmjxa] files as they were given on the command line
   when the library was created, so a bare file name may refer to a file
   installed next to the library. *)
let find_stub name =
  match Load_path.find name with
  | path -> path
  | exception Not_found ->
    if Sys.file_exists name
    then name
    else raise (Linkenv.Error (Linkenv.File_not_found name))

let write_cmj ~filename ~compilation_unit
    ({ program; imported_compilation_units } : Optcomp_intf.jsir_program) =
  let oc = open_out_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
      output_string oc Config.cmj_magic_number;
      let body : Jsoo_imports.Code.cmj_body =
        { program = Jsoo_imports.Code.Marshalable_program.of_program program;
          (* [Var] numbers variables with mutable state. js_of_ocaml needs to
             know the highest number used so that the variables it creates do
             not clash with these. *)
          last_var = Jsoo_imports.Code.Var.idx (Jsoo_imports.Code.Var.last ());
          imported_compilation_units =
            Compilation_unit.Set.elements imported_compilation_units
            |> List.map Compilation_unit.full_path_as_string;
          exported_compilation_unit =
            Compilation_unit.full_path_as_string compilation_unit
        }
      in
      output_value oc body)

let make
    ~(lambda_to_jsir :
       ppf_dump:Format.formatter ->
       prefixname:string ->
       keep_symbol_tables:bool ->
       Lambda.program ->
       Optcomp_intf.jsir_program) =
  (module Optcompile.Make (struct
    let backend = Compile_common.Js_of_ocaml

    let supports_metaprogramming = false

    let ext_obj = ".cmjo"

    let ext_lib = ".cmja"

    let ext_flambda_obj = ".cmjx"

    let ext_flambda_lib = ".cmjxa"

    let default_executable_name = Config.default_executable_name ^ ".js"

    let emit = None

    let compile_implementation ~keep_symbol_tables ~sourcefile:_ ~prefixname
        ~ppf_dump (program : Lambda.program) =
      let jsir =
        lambda_to_jsir ~ppf_dump ~prefixname ~keep_symbol_tables program
        |> Misc.print_if ppf_dump Clflags.dump_jsir
             (fun ppf ({ program; _ } : Optcomp_intf.jsir_program) ->
               Jsoo_imports.Code.Print.program ppf (fun _ _ -> "") program)
      in
      let cmj = prefixname ^ ".cmj" in
      let cmjo = prefixname ^ ext_obj in
      write_cmj ~filename:cmj ~compilation_unit:program.compilation_unit jsir;
      let remove_cmj () =
        if not !Clflags.keep_asm_file then Misc.remove_file cmj
      in
      Misc.try_finally
        ~exceptionally:(fun () -> Misc.remove_file cmjo)
        (fun () ->
          Profile.record_call "js_of_ocaml" (fun () ->
              run_js_of_ocaml "compile" ~phase:Compile ~input_left_in:cmj
                (debuginfo_args () @ ["-o"; cmjo; cmj]));
          remove_cmj ())

    let create_archive archive_name objfiles =
      run_js_of_ocaml "link" ~phase:Archive
        (["-a"; "-o"; archive_name] @ objfiles)

    let link_partial target objfiles =
      run_js_of_ocaml "link" ~phase:Archive (["-a"; "-o"; target] @ objfiles)

    let link _linkenv (objfiles : Linkenv.objfile_to_link list) output_name
        ~cached_genfns_imports:_ ~genfns:_ ~units_tolink:_ ~uses_eval:_
        ~quoted_cmi:_ ~quoted_cmx:_ ~ppf_dump:_ =
      (* Several libraries may record the same stub file; js_of_ocaml accepts
         duplicates but there is no point in passing them. The first occurrence
         is kept. *)
      let stubs =
        List.rev_map find_stub !Clflags.ccobjs
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
      let runtime = output_name ^ ".runtime.js" in
      Misc.try_finally
        ~always:(fun () ->
          if not !Clflags.keep_startup_file then Misc.remove_file runtime)
        ~exceptionally:(fun () -> Misc.remove_file output_name)
        (fun () ->
          run_js_of_ocaml "build-runtime" ~phase:Runtime
            (debuginfo_args () @ ["-o"; runtime] @ stubs);
          let linkall =
            if !Clflags.link_everything then ["--linkall"] else []
          in
          run_js_of_ocaml "link" ~phase:Link
            (linkall @ ["-o"; output_name; runtime] @ objfiles))

    let link_shared _objfiles _output_name ~genfns:_ ~units_tolink:_ ~ppf_dump:_
        =
      raise (Error (Unsupported "-shared"))

    let support_files_for_eval () = []

    let set_load_path_for_eval () = ()
  end) : Optcompile.S)

let report_error_doc ppf = function
  | Js_of_ocaml_not_found program ->
    Format_doc.fprintf ppf
      "@[<hov>Cannot run js_of_ocaml (%a):@ it must be in %a or in the PATH,@ \
       or the %s environment variable must give its path@]"
      Location.Doc.quoted_filename program Location.Doc.quoted_filename
      Config.bindir js_of_ocaml_env_var
  | Js_of_ocaml_error { subcommand; exit_code; input_left_in } -> (
    Format_doc.fprintf ppf "Error while running js_of_ocaml %s (exit code %d)"
      subcommand exit_code;
    match input_left_in with
    | None -> ()
    | Some file ->
      Format_doc.fprintf ppf ",@ input left in file %a"
        Location.Doc.quoted_filename file)
  | Unsupported option ->
    Format_doc.fprintf ppf "%s is not supported when targeting js_of_ocaml"
      option

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)

let report_error = Format_doc.compat report_error_doc
