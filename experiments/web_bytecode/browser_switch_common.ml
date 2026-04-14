type environment =
  | Native
  | Browser

exception Missing_cmi of string

let browser_cmis_dir = Browser_switch_package_manifest.browser_cmis_dir
let browser_package_roots = Browser_switch_package_manifest.browser_package_roots
let browser_include_dirs =
  browser_cmis_dir :: Browser_switch_package_manifest.browser_package_include_dirs

let make_formatter_buffer () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  buffer, ppf

let flush_formatter ppf =
  Format.pp_print_flush ppf ();
  flush_all ()

let capture_diagnostics f =
  let buffer, ppf = make_formatter_buffer () in
  let ok =
    try f ppf with
    | Missing_cmi _ as exn -> raise exn
    | exn ->
      Location.report_exception ppf exn;
      let backtrace = Printexc.get_backtrace () in
      if not (String.equal backtrace "")
      then Format.fprintf ppf "@.%s" backtrace;
      false
  in
  flush_formatter ppf;
  if ok then "" else Buffer.contents buffer

let compilation_unit filename =
  let name =
    Unit_info.modname_from_source filename
    |> Compilation_unit.Name.of_string
  in
  Compilation_unit.create Compilation_unit.Prefix.empty name

let file_prefix filename =
  match Filename.extension filename with
  | "" -> filename
  | ext -> String.sub filename 0 (String.length filename - String.length ext)

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

let prepare_lexbuf ~filename source =
  let lexbuf = Lexing.from_string source in
  Location.input_name := filename;
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf filename;
  lexbuf

let missing_cmi_filename unit_name =
  let requested = Compilation_unit.Name.to_string unit_name ^ ".cmi" in
  match Misc.normalized_unit_filename requested with
  | Ok normalized -> normalized
  | Error _ -> requested

let reset_flags environment =
  Clflags.annotations := false;
  Clflags.binary_annotations := false;
  Clflags.dont_write_files := true;
  Clflags.native_code := false;
  Clflags.no_std_include := environment = Browser;
  Clflags.no_cwd := environment = Browser;
  Clflags.include_dirs :=
    (match environment with
     | Native -> []
     | Browser -> browser_include_dirs);
  Clflags.hidden_include_dirs := [];
  Clflags.open_modules := [];
  Clflags.use_threads := false

let prepare_compiler environment ~filename =
  reset_flags environment;
  Location.reset ();
  Lexer.reset_syntax_mode ();
  Typemod.reset ~preserve_persistent_env:false;
  Env.reset_cache ~preserve_persistent_env:false;
  let dir =
    match environment with
    | Native -> Filename.dirname filename
    | Browser -> ""
  in
  Compmisc.init_path ~dir ();
  (match environment with
   | Native -> ()
   | Browser ->
     List.iter
       (fun include_dir -> Load_path.add_dir ~hidden:false include_dir)
       browser_include_dirs);
  Compmisc.init_parameters ()

let with_missing_cmi_detection environment f =
  match environment with
  | Native -> f ()
  | Browser ->
    let previous_load = !Persistent_env.Persistent_signature.load in
    Fun.protect
      (fun () ->
        Persistent_env.Persistent_signature.load :=
          (fun ~allow_hidden ~unit_name ->
            match previous_load ~allow_hidden ~unit_name with
            | Some _ as result -> result
            | None -> raise (Missing_cmi (missing_cmi_filename unit_name)));
        f ())
      ~finally:(fun () ->
        Persistent_env.Persistent_signature.load := previous_load)
