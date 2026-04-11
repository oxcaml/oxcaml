let capture_output f =
  flush_all ();
  let read_fd, write_fd = Unix.pipe () in
  let saved_stdout = Unix.dup Unix.stdout in
  let saved_stderr = Unix.dup Unix.stderr in
  let restore () =
    flush_all ();
    Unix.dup2 saved_stdout Unix.stdout;
    Unix.dup2 saved_stderr Unix.stderr;
    Unix.close saved_stdout;
    Unix.close saved_stderr
  in
  Unix.dup2 write_fd Unix.stdout;
  Unix.dup2 write_fd Unix.stderr;
  Unix.close write_fd;
  let result =
    try Ok (f ())
    with exn -> Error exn
  in
  restore ();
  let input = Unix.in_channel_of_descr read_fd in
  let output =
    Fun.protect
      (fun () -> In_channel.input_all input)
      ~finally:(fun () -> close_in_noerr input)
  in
  match result with
  | Ok value -> output, value
  | Error exn -> raise exn

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

let check_string ~filename ~source =
  Web_bytecode_native_packages.ensure_initialized ();
  Web_bytecode_check.check_string ~browser:false ~filename ~source

let run_string ~filename ~source =
  Web_bytecode_native_packages.ensure_initialized ();
  let suffix = "_" ^ Filename.basename filename in
  let source_path = Filename.temp_file "web_bytecode_run_" suffix in
  let output_prefix = source_path ^ ".build" in
  write_source_file ~source_path ~source;
  let output, diagnostics =
    capture_output (fun () ->
        Web_bytecode_common.capture_diagnostics (fun ppf ->
            Web_bytecode_common.prepare_compiler Web_bytecode_common.Native ~filename;
            Fun.protect
              (fun () ->
                let cmo_path = compile_source_file ~source_path ~output_prefix in
                Toploop.initialize_toplevel_env ();
                Web_bytecode_native_packages.init_toplevel_packages ();
                let baseline_symtable = Symtable.current_state () in
                Fun.protect
                  (fun () ->
                    Toploop.override_sys_argv [| filename |];
                    Toploop.input_name := filename;
                    Sys.interactive := false;
                    Toploop.load_file ppf cmo_path)
                  ~finally:(fun () -> Symtable.restore_state baseline_symtable))
              ~finally:(fun () ->
                cleanup_build_artifacts ~source_path ~output_prefix)))
  in
  let diagnostics = replace_all ~pattern:source_path ~with_:filename diagnostics in
  if String.equal diagnostics "" then output else output ^ diagnostics
