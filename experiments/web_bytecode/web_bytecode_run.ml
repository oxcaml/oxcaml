let run_string ~browser ~filename ~source =
  let environment =
    if browser then Web_bytecode_common.Browser
    else Web_bytecode_common.Native
  in
  Web_bytecode_common.capture_diagnostics (fun ppf ->
      Web_bytecode_common.prepare_compiler environment ~filename;
      if browser then Topdirs.dir_directory Web_bytecode_common.browser_cmis_dir;
      Toploop.initialize_toplevel_env ();
      Toploop.override_sys_argv [| filename |];
      Toploop.input_name := filename;
      Sys.interactive := false;
      let lexbuf = Web_bytecode_common.prepare_lexbuf ~filename source in
      let phrases = !Toploop.parse_use_file lexbuf in
      List.for_all
        (fun phrase ->
          Warnings.reset_fatal ();
          let phrase = Toploop.preprocess_phrase ppf phrase in
          Env.reset_cache_toplevel ();
          Toploop.execute_phrase false ppf phrase)
        phrases)
