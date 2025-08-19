exception Error of string

let js_of_ocaml_dir = Sys.getenv "JS_OF_OCAML_DIR"

let js_of_ocaml_exe =
  try Sys.getenv "JS_OF_OCAML_EXE"
  with Not_found ->
    Filename.concat js_of_ocaml_dir "compiler/bin-js_of_ocaml/js_of_ocaml.exe"

let stdlib_js_dir = Sys.getenv "OCAMLJ_STDLIB_JS_DIR"

let remove_file file = try Sys.remove file with Sys_error _ -> ()

let command cmdline =
  if !Clflags.verbose
  then (
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline ());
  let res = Sys.command cmdline in
  if res <> 0 then raise (Error (Printf.sprintf "Command failed: %s" cmdline));
  res

let compile_cmj_to_js cmj_file output_js =
  let cmd =
    Printf.sprintf
      "%s --debuginfo --pretty -I %s/runtime/js -o %s --enable with-js-error \
       --source-map-inline %s"
      (Filename.quote js_of_ocaml_exe)
      (Filename.quote js_of_ocaml_dir)
      (Filename.quote output_js) (Filename.quote cmj_file)
  in
  ignore (command cmd)

let build_runtime output_runtime =
  let cmd =
    Printf.sprintf
      "%s build-runtime -I %s/runtime/js -o %s --setenv \
       FORCE_DROP_INLINE_TEST=true --setenv FORCE_DROP_BENCH=true --enable \
       with-js-error"
      (Filename.quote js_of_ocaml_exe)
      (Filename.quote js_of_ocaml_dir)
      (Filename.quote output_runtime)
  in
  ignore (command cmd)

let link_js_files runtime_js js_files output_js =
  let stdlib_js = Filename.concat stdlib_js_dir "stdlib.js" in
  let std_exit_js = Filename.concat stdlib_js_dir "std_exit.js" in
  let all_files = (runtime_js :: stdlib_js :: js_files) @ [std_exit_js] in
  let cmd =
    Printf.sprintf "%s link %s -o %s --source-map --empty-source-map"
      (Filename.quote js_of_ocaml_exe)
      (String.concat " " (List.map Filename.quote all_files))
      (Filename.quote output_js)
  in
  ignore (command cmd)

let add_node_shebang js_file =
  let content =
    let ic = open_in js_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in
  let oc = open_out js_file in
  output_string oc "#!/usr/bin/env node\n";
  output_string oc content;
  close_out oc;
  ignore (Sys.command (Printf.sprintf "chmod +x %s" (Filename.quote js_file)))

let link ~ppf_dump:_ objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
      let objfiles = if !Clflags.nopervasives then objfiles else objfiles in
      let temp_dir = Filename.get_temp_dir_name () in
      let runtime_js = Filename.concat temp_dir "runtime.js" in
      let js_files = ref [] in
      try
        build_runtime runtime_js;
        List.iter
          (fun objfile ->
            if Filename.check_suffix objfile ".cmj"
            then (
              let base = Filename.remove_extension objfile in
              let js_file =
                Filename.concat temp_dir (Filename.basename base ^ ".cmo.js")
              in
              compile_cmj_to_js objfile js_file;
              js_files := js_file :: !js_files)
            else if Filename.check_suffix objfile ".cmja"
            then ())
          objfiles;
        let final_js =
          if Filename.check_suffix output_name ".js"
          then output_name
          else output_name ^ ".js"
        in
        link_js_files runtime_js (List.rev !js_files) final_js;
        add_node_shebang final_js;
        remove_file runtime_js;
        List.iter remove_file !js_files
      with e ->
        remove_file runtime_js;
        List.iter remove_file !js_files;
        raise e)

let reset () = ()
