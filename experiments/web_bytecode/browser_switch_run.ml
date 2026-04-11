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
  Browser_switch_common.capture_diagnostics (fun ppf ->
      Browser_switch_common.prepare_compiler environment ~filename;
      if browser
      then (
        ensure_browser_toplevel_initialized ();
        Toploop.initialize_toplevel_env ();
        Topdirs.dir_directory Browser_switch_common.browser_cmis_dir;
        List.iter
          Topdirs.dir_directory
          Browser_switch_package_manifest.browser_package_include_dirs)
      else Toploop.initialize_toplevel_env ();
      Toploop.override_sys_argv [| filename |];
      Toploop.input_name := filename;
      Sys.interactive := false;
      let lexbuf = Browser_switch_common.prepare_lexbuf ~filename source in
      let parse_use_file = !Toploop.parse_use_file in
      let phrases = parse_use_file lexbuf in
      List.for_all
        (fun phrase ->
          Warnings.reset_fatal ();
          let phrase = Toploop.preprocess_phrase ppf phrase in
          Env.reset_cache_toplevel ();
          Toploop.execute_phrase false ppf phrase)
        phrases)
