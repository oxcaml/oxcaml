let ensure_browser_toplevel_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      Js_of_ocaml_toplevel.JsooTop.initialize ();
      initialized := true)

let run_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  let suffix = "_" ^ Filename.basename filename in
  let source_path = Filename.temp_file "browser_switch_run_" suffix in
  let output_prefix = source_path ^ ".build" in
  Browser_switch_common.write_source_file ~source_path ~source;
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
    Browser_switch_common.capture_diagnostics (fun ppf ->
        Browser_switch_common.prepare_compiler environment ~filename;
        Fun.protect
          (fun () ->
            let cmo_path =
              Browser_switch_common.compile_source_file ~source_path ~output_prefix
            in
            if browser
            then (
              ensure_browser_toplevel_initialized ();
              Toploop.initialize_toplevel_env ();
              Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
              List.iter
                Topdirs.dir_directory
                Browser_switch_package_manifest.browser_package_include_dirs)
            else Toploop.initialize_toplevel_env ();
            let baseline_symtable = Symtable.current_state () in
            Fun.protect
              (fun () ->
                Toploop.override_sys_argv [| filename |];
                Toploop.input_name := filename;
                Sys.interactive := false;
                Toploop.load_file ppf cmo_path)
              ~finally:(fun () -> Symtable.restore_state baseline_symtable))
          ~finally:(fun () ->
            Browser_switch_common.cleanup_build_artifacts ~source_path ~output_prefix)))
  |> Browser_switch_common.replace_all ~pattern:source_path ~with_:filename

let initialize_toplevel_for_environment environment =
  match environment with
  | Browser_switch_common.Native -> Toploop.initialize_toplevel_env ()
  | Browser_switch_common.Browser ->
    ensure_browser_toplevel_initialized ();
    Toploop.initialize_toplevel_env ();
    Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
    List.iter
      Topdirs.dir_directory
      Browser_switch_package_manifest.browser_package_include_dirs

let capture_toplevel_output f =
  let buffer, ppf = Browser_switch_common.make_formatter_buffer () in
  (try ignore (f ppf) with
   | Browser_switch_common.Missing_cmi _ as exn -> raise exn
   | exn -> Location.report_exception ppf exn);
  Browser_switch_common.flush_formatter ppf;
  Buffer.contents buffer

let utop_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  Browser_switch_common.with_missing_cmi_detection environment (fun () ->
    capture_toplevel_output (fun ppf ->
      Browser_switch_common.prepare_compiler environment ~filename;
      initialize_toplevel_for_environment environment;
      Toploop.override_sys_argv [| filename |];
      Toploop.input_name := filename;
      Sys.interactive := false;
      let lexbuf = Browser_switch_common.prepare_lexbuf ~filename source in
      let phrases = !Toploop.parse_use_file lexbuf in
      List.for_all
        (fun phrase ->
          Warnings.reset_fatal ();
          let phrase = Toploop.preprocess_phrase ppf phrase in
          Env.reset_cache_toplevel ();
          Toploop.execute_phrase true ppf phrase)
        phrases
      |> ignore))
