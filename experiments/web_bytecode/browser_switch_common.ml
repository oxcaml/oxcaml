type environment =
  | Native
  | Browser

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
    try f ppf
    with exn ->
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

let prepare_lexbuf ~filename source =
  let lexbuf = Lexing.from_string source in
  Location.input_name := filename;
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf filename;
  lexbuf

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
  Compmisc.init_parameters ()
