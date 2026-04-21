let run_toplevel_string ~browser ~filename ~source ~print_outcome ppf =
  let environment =
    if browser then Web_bytecode_common.Browser
    else Web_bytecode_common.Native
  in
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
      Toploop.execute_phrase print_outcome ppf phrase)
    phrases

let run_string ~browser ~filename ~source =
  Web_bytecode_common.capture_diagnostics (fun ppf ->
      run_toplevel_string ~browser ~filename ~source ~print_outcome:false ppf)

let utop_string ~browser ~filename ~source =
  run_toplevel_string
    ~browser
    ~filename
    ~source
    ~print_outcome:true
    Format.std_formatter
  |> ignore;
  Format.pp_print_flush Format.std_formatter ();
  ""
