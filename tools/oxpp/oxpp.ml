(* oxpp: preprocessor letting OxCaml's own sources use erasable OxCaml syntax
   extensions while the boot build still compiles them with the system compiler.

   Used through dune's [(preprocess (action ...))]: parses FILE with OxCaml's
   parser and writes a marshalled AST on stdout, in the format the compiler
   accepts for preprocessed sources (magic number, source file name, AST). In
   the "default" dune context the consumer is the system (upstream) compiler, so
   the AST is erased and converted to OCaml 5.4's parsetree; in the "main"
   context the consumer is OxCaml itself and the AST is passed through
   unchanged. *)

type context =
  | Default
  | Main

type kind =
  | Impl
  | Intf

type ast =
  | Structure of Parsetree.structure
  | Signature of Parsetree.signature

let context = ref Default

let kind = ref None

let file = ref None

let set_context = function
  | "default" -> context := Default
  | "main" -> context := Main
  | name -> raise (Arg.Bad (Printf.sprintf "unknown dune context %S" name))

let set_warnings ~error spec = ignore (Warnings.parse_options error spec)

let spec =
  [ ( "-context",
      Arg.String set_context,
      "NAME dune context the output is for: \"default\" (system compiler: \
       erase OxCaml syntax and emit an OCaml 5.4 AST) or \"main\" (OxCaml \
       compiler: emit the OxCaml AST unchanged)" );
    ( "-impl",
      Arg.Unit (fun () -> kind := Some Impl),
      " FILE is an implementation" );
    "-intf", Arg.Unit (fun () -> kind := Some Intf), " FILE is an interface";
    ( "-extension",
      Arg.String Language_extension.enable_of_string_exn,
      "EXT enable the language extension EXT" );
    ( "-no-extension",
      Arg.String Language_extension.disable_of_string_exn,
      "EXT disable the language extension EXT" );
    ( "-w",
      Arg.String (set_warnings ~error:false),
      "SPEC enable or disable parse-time warnings (default: +a)" );
    ( "-warn-error",
      Arg.String (set_warnings ~error:true),
      "SPEC treat parse-time warnings as errors (default: +a)" ) ]

let usage = "Usage: oxpp [options] FILE\nOptions are:"

let set_file name =
  match !file with
  | None -> file := Some name
  | Some _ -> raise (Arg.Bad "only one input file may be given")

let read_file name =
  let ic = open_in_bin name in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Mirrors what Pparse does before parsing a source file. *)
let parse kind name =
  let source = read_file name in
  Location.input_name := name;
  let lexbuf = Lexing.from_string source in
  Location.input_lexbuf := Some lexbuf;
  Location.init lexbuf name;
  Lexer.reset_syntax_mode ();
  let ast =
    match kind with
    | Impl -> Structure (Parse.implementation lexbuf)
    | Intf -> Signature (Parse.interface lexbuf)
  in
  Warnings.check_fatal ();
  ast

let emit magic name ast =
  set_binary_mode_out stdout true;
  output_string stdout magic;
  output_value stdout (name : string);
  output_value stdout ast

let main () =
  set_warnings ~error:false "+a";
  set_warnings ~error:true "+a";
  Arg.parse spec set_file usage;
  let name =
    match !file with
    | Some name -> name
    | None ->
      Arg.usage spec usage;
      exit 2
  in
  let kind =
    match !kind with
    | Some kind -> kind
    | None -> if Filename.check_suffix name ".mli" then Intf else Impl
  in
  match parse kind name, !context with
  | Structure str, Main -> emit Config.ast_impl_magic_number name str
  | Structure str, Default ->
    emit Ast_504.Config.ast_impl_magic_number name (Downgrade.structure str)
  | Signature sg, Main -> emit Config.ast_intf_magic_number name sg
  | Signature sg, Default ->
    emit Ast_504.Config.ast_intf_magic_number name (Downgrade.signature sg)

let () =
  try main () with
  | Arg.Bad message ->
    prerr_string message;
    prerr_newline ();
    exit 2
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
