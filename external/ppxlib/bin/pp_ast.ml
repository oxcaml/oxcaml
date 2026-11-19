open Ppxlib

module Kind = struct
  type t = Signature | Structure | Expression | Pattern | Core_type

  let to_utils_kind = function
    | Structure -> Ppxlib_private.Utils.Kind.Impl
    | Signature -> Ppxlib_private.Utils.Kind.Intf
    | _ -> assert false
end

module Ast = struct
  type t =
    | Str of structure
    | Sig of signature
    | Exp of expression
    | Pat of pattern
    | Typ of core_type
end

let rec repr_to_yojson : Pp_ast.repr -> Yojson.Basic.t = function
  | Unit -> `Null
  | Int i -> `Int i
  | String s -> `String s
  | Special s -> `String s
  | Bool b -> `Bool b
  | Char c -> `String (String.make 1 c)
  | Float f -> `Float f
  | Int32 i32 -> `Int (Int32.to_int i32)
  | Int64 i64 -> `Int (Int64.to_int i64)
  | Nativeint ni -> `Int (Nativeint.to_int ni)
  | Array l -> `List (List.map repr_to_yojson l)
  | Tuple l -> `List (List.map repr_to_yojson l)
  | List l -> `List (List.map repr_to_yojson l)
  | Record fields ->
      `Assoc (List.map (fun (k, v) -> (k, repr_to_yojson v)) fields)
  | Constr (cname, []) -> `String cname
  | Constr (cname, [ x ]) -> `Assoc [ (cname, repr_to_yojson x) ]
  | Constr (cname, l) -> `Assoc [ (cname, `List (List.map repr_to_yojson l)) ]

let json_printer fmt value =
  Yojson.Basic.pretty_print fmt (repr_to_yojson value)

module Input = struct
  type t = Stdin | File of string | Source of string

  let to_lexbuf t =
    let all_source =
      match t with
      | Stdin -> Stdppx.In_channel.input_all stdin
      | File fn -> Stdppx.In_channel.(with_file fn ~f:input_all)
      | Source s -> s
    in
    Lexing.from_string all_source

  let from_string = function
    | "-" -> Stdin
    | s when Sys.file_exists s -> File s
    | s -> Source s

  let to_driver_fn = function
    | Stdin -> "-"
    | File fn -> fn
    | Source _ -> assert false
end

let parse_node ~kind ~input_name input =
  let lexbuf = Input.to_lexbuf input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_name };
  Astlib.Location.set_input_lexbuf (Some lexbuf);
  match (kind : Kind.t) with
  | Expression -> Ast.Exp (Parse.expression lexbuf)
  | Pattern -> Ast.Pat (Parse.pattern lexbuf)
  | Core_type -> Ast.Typ (Parse.core_type lexbuf)
  | Signature -> Ast.Str (Parse.implementation lexbuf)
  | Structure -> Ast.Sig (Parse.interface lexbuf)

let load_input ~kind ~input_name input =
  match ((kind : Kind.t), (input : Input.t)) with
  | (Structure | Signature), (Stdin | File _) -> (
      let kind = Kind.to_utils_kind kind in
      let fn = Input.to_driver_fn input in
      match Driver.load_input ~kind ~input_name ~relocate:false fn with
      | Error (loc_err, _ver) -> Location.Error.raise loc_err
      | Ok (_ast_input_name, _version, ast) -> (
          match (ast : Ppxlib_private.Utils.Intf_or_impl.t) with
          | Impl str -> Ast.Str str
          | Intf sig_ -> Ast.Sig sig_))
  | (Expression | Pattern | Core_type), _ | _, Source _ ->
      parse_node ~kind ~input_name input

let pp_ast ~config ast =
  match (ast : Ast.t) with
  | Str str -> Pp_ast.structure ~config Format.std_formatter str
  | Sig sig_ -> Pp_ast.signature ~config Format.std_formatter sig_
  | Exp exp -> Pp_ast.expression ~config Format.std_formatter exp
  | Pat pat -> Pp_ast.pattern ~config Format.std_formatter pat
  | Typ typ -> Pp_ast.core_type ~config Format.std_formatter typ

let named f = Cmdliner.Term.(app (const f))

let show_attrs =
  let doc = "Show atributes in the pretty printed output" in
  named
    (fun x -> `Show_attrs x)
    Cmdliner.Arg.(value & flag & info ~doc [ "show-attrs" ])

let show_locs =
  let doc = "Show locations in the pretty printed output" in
  named
    (fun x -> `Show_locs x)
    Cmdliner.Arg.(value & flag & info ~doc [ "show-locs" ])

let loc_mode =
  let full_locs =
    let doc =
      "Display locations in long form. Has no effect without --show-locs."
    in
    (`Full, Cmdliner.Arg.info ~doc [ "full-locs" ])
  in
  named (fun x -> `Loc_mode x) Cmdliner.Arg.(value & vflag `Short [ full_locs ])

let json =
  let doc = "Show AST as json" in
  named (fun x -> `Json x) Cmdliner.Arg.(value & flag & info ~doc [ "json" ])

let kind =
  let make_vflag (flag, (kind : Kind.t), doc) =
    (Some kind, Cmdliner.Arg.info ~doc [ flag ])
  in
  let kinds =
    List.map make_vflag
      [
        ("str", Structure, "Treat the input as a $(b,.ml) file");
        ("sig", Signature, "Treat the input as a $(b,.mli) file");
        ("exp", Expression, "Treat the input as a single OCaml expression");
        ("pat", Pattern, "Treat the input as a single OCaml pattern");
        ("typ", Core_type, "Treat the input as a single OCaml core_type");
      ]
  in
  named (fun x -> `Kind x) Cmdliner.Arg.(value & vflag None kinds)

let input =
  let docv = "INPUT" in
  let doc =
    "The $(docv) AST. Can be a binary AST file, a source file or a valid OCaml \
     source string. Pass $(b,-) to read from stdin instead."
  in
  named
    (fun x -> `Input x)
    Cmdliner.Arg.(required & pos 0 (some string) None & info ~doc ~docv [])

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run (`Show_attrs show_attrs) (`Show_locs show_locs) (`Loc_mode loc_mode)
    (`Json json) (`Kind kind) (`Input input) =
  let open Stdppx.Result in
  let kind =
    match kind with
    | Some k -> Ok k
    | None -> (
        match Ppxlib_private.Utils.Kind.of_filename input with
        | Some Intf -> Ok Kind.Signature
        | Some Impl -> Ok Kind.Structure
        | None ->
            errorf
              "Could not guess kind from input %S. Please use relevant CLI \
               flag."
              input)
  in
  kind >>= fun kind ->
  let input = Input.from_string input in
  let input_name =
    match input with Stdin -> "<stdin>" | File fn -> fn | Source _ -> "<cli>"
  in
  let ast = load_input ~kind ~input_name input in
  let custom_printer = if json then Some json_printer else None in
  let config =
    Pp_ast.Config.make ~show_attrs ~show_locs ~loc_mode ?printer:custom_printer
      ()
  in
  pp_ast ~config ast;
  Format.printf "%!\n";
  Ok ()

let term =
  Cmdliner.Term.(
    const run $ show_attrs $ show_locs $ loc_mode $ json $ kind $ input)

let tool_name = "ppxlib-pp-ast"

let info =
  let p fmt = Printf.ksprintf (fun s -> `P s) fmt in
  let open Cmdliner in
  Cmd.info tool_name ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"Pretty prints ppxlib's ASTs from OCaml sources"
    ~man:
      [
        `S "DESCRIPTION";
        p
          "$(b,%s) can be used to pretty print the OCaml AST for a given piece \
           of source code."
          tool_name;
        `P
          "It prints the AST version used by ppxlib internally. This means the \
           code will be parsed (or the AST unmarshalled) using the installed \
           compiler and eventually migrated to ppxlib's expected version.";
        `P
          "It can read the source code directly from the command line or read \
           it from a file/the standard input. In the latter case you can pass \
           regular $(b,.ml) or $(b,.mli) files, marshalled AST files as the \
           ones produced by the ppxlib driver or a fragment of OCaml source \
           corresponding to an expression, a pattern or a core_type.";
        `P
          "When the input is not an $(b,.ml) or $(b,.mli) file you will have \
           to explicitly pass the expected AST node using flags such as \
           $(b,--str) or $(b,--exp).";
        `P "By default the output looks like this:";
        p "$(b,\\$ %s --exp \"x + 2\")" tool_name;
        `Noblank;
        `Pre
          (String.concat "\n"
             [
               {|Pexp_apply|};
               {|  ( Pexp_ident (Lident "+")|};
               {|  , [ ( Nolabel, Pexp_ident (Lident "x"))|};
               {|    ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))|};
               {|    ]|};
               {|  )|};
             ]);
        `P
          "If you are already familiar with the OCaml AST you will note that \
           it prints a somewhat lighter version to keep the output concise. \
           Locations and attributes are not printed. Some specific record \
           types, such as $(b,expression) or $(b,pattern), are \"skipped\" to \
           avoid too much nesting. This does not hurt comprehension of the \
           underlying AST as these records only wrap a variant type to attach \
           metadata to it. The tool supports a set of flags you can use to \
           force printing of such metadata.";
        `S "EXAMPLES";
        p "$(b,%s test.ml)" tool_name;
        p "$(b,cat test.ml | %s -)" tool_name;
        p "$(b,%s test.pp.ml)" tool_name;
        p "$(b,%s --exp \"x + 2\")" tool_name;
        p "$(b,%s --typ \"(int, string\\) Result.t\")" tool_name;
        p "$(b,%s --show-locs --full-locs --pat \"_::tl\")" tool_name;
      ]

let () =
  let exit_code = Cmdliner.Cmd.eval_result (Cmdliner.Cmd.v info term) in
  exit exit_code
