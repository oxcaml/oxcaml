type environment =
  | Native
  | Browser

let browser_cmis_dir = "/static/cmis"

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
    try f ppf
    with exn ->
      Location.report_exception ppf exn;
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

let prepare_lexbuf ~filename source =
  let lexbuf = Lexing.from_string source in
  Location.input_name := filename;
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf filename;
  lexbuf

let reset_flags environment =
  Clflags.annotations := false;
  Clflags.binary_annotations := false;
  Clflags.binary_annotations_cms := false;
  Clflags.dont_write_files := true;
  Clflags.native_code := false;
  Clflags.no_std_include := environment = Browser;
  Clflags.no_cwd := environment = Browser;
  Clflags.include_dirs :=
    (match environment with
     | Native -> []
     | Browser -> [browser_cmis_dir]);
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
  Compmisc.init_parameters ()
