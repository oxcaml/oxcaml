let ensure_browser_toplevel_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      Js_of_ocaml_toplevel.JsooTop.initialize ();
      initialized := true)

let ensure_browser_findlib_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      let roots = Browser_switch_package_manifest.browser_package_roots in
      let install_dir =
        match roots with
        | root :: _ -> root
        | [] -> invalid_arg "browser package roots must not be empty"
      in
      Findlib.init_manually
        ~stdlib:Browser_switch_package_manifest.browser_cmis_dir
        ~ldconf:""
        ~install_dir
        ~meta_dir:""
        ~search_path:roots
        ();
      Topfind.log := ignore;
      Topfind.add_predicates [ "byte"; "toploop" ];
      Topfind.don't_load Browser_switch_package_manifest.browser_dont_load_packages;
      initialized := true)

let ensure_browser_curated_packages_registered =
  let loaded = ref false in
  fun () ->
    if not !loaded
    then (
      ensure_browser_findlib_initialized ();
      Topfind.don't_load_deeply Browser_switch_package_manifest.browser_preload_packages;
      loaded := true)

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
        ensure_browser_curated_packages_registered ())
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
