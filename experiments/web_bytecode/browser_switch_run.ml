let ensure_browser_toplevel_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      Js_of_ocaml_toplevel.JsooTop.initialize ();
      initialized := true)

let cleanup_if_exists path =
  try Sys.remove path
  with Sys_error _ -> ()

let cleanup_build_artifacts ~source_path ~output_prefix =
  List.iter cleanup_if_exists
    [ source_path;
      output_prefix ^ ".cmo";
      output_prefix ^ ".cmi";
      output_prefix ^ ".cmt";
      output_prefix ^ ".cms";
      output_prefix ^ ".annot" ]

let replace_all ~pattern ~with_ text =
  let pattern_length = String.length pattern in
  if pattern_length = 0
  then text
  else (
    let buffer = Buffer.create (String.length text) in
    let rec loop search_start =
      if search_start >= String.length text
      then Buffer.contents buffer
      else (
        match String.index_from_opt text search_start pattern.[0] with
        | None ->
          Buffer.add_substring buffer text search_start
            (String.length text - search_start);
          Buffer.contents buffer
        | Some index ->
          if index + pattern_length <= String.length text
             && String.sub text index pattern_length = pattern
          then (
            Buffer.add_substring buffer text search_start (index - search_start);
            Buffer.add_string buffer with_;
            loop (index + pattern_length))
          else (
            Buffer.add_substring buffer text search_start (index + 1 - search_start);
            loop (index + 1)))
    in
    loop 0)

let write_source_file ~source_path ~source =
  let oc = open_out_bin source_path in
  Fun.protect
    (fun () -> output_string oc source)
    ~finally:(fun () -> close_out_noerr oc)

let compile_source_file ~source_path ~output_prefix =
  let saved_dont_write_files = !Clflags.dont_write_files in
  Fun.protect
    (fun () ->
      Clflags.dont_write_files := false;
      Compile.implementation
        ~start_from:Clflags.Compiler_pass.Parsing
        ~source_file:source_path
        ~output_prefix
        ~keep_symbol_tables:false;
      output_prefix ^ ".cmo")
    ~finally:(fun () -> Clflags.dont_write_files := saved_dont_write_files)

let run_string ~browser ~filename ~source =
  let environment =
    if browser then Browser_switch_common.Browser
    else Browser_switch_common.Native
  in
  let suffix = "_" ^ Filename.basename filename in
  let source_path = Filename.temp_file "browser_switch_run_" suffix in
  let output_prefix = source_path ^ ".build" in
  write_source_file ~source_path ~source;
  Browser_switch_common.capture_diagnostics (fun ppf ->
      Browser_switch_common.prepare_compiler environment ~filename;
      Fun.protect
        (fun () ->
          let cmo_path = compile_source_file ~source_path ~output_prefix in
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
          cleanup_build_artifacts ~source_path ~output_prefix))
  |> replace_all ~pattern:source_path ~with_:filename
