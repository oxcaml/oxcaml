module Lexer_raw = Lexer
module Parser_raw = Parser

module Syntax_error = struct
  type t =
    { msg : string
    ; loc : Location.t
    }
end

exception Syntax_error of Syntax_error.t

let parse_error_location lexbuf =
  match !Parse_errors.error_location with
  | None -> Location.curr lexbuf
  | Some (loc_start, loc_end) -> { loc_start; loc_end; loc_ghost = false }

let raise_error lexbuf msg =
  raise (Syntax_error { msg; loc = parse_error_location lexbuf })

let get_token lexbuf =
  let tok = Lexer_raw.token lexbuf in
  (tok, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let parse_exn
      (type a)
      (lexbuf : Lexing.lexbuf)
      ~(f : Lexing.position -> a Parser_raw.MenhirInterpreter.checkpoint)
  : a
  =
  let initial = lexbuf.lex_start_p in
  Location.input_name := initial.pos_fname;
  let module I = Parser_raw.MenhirInterpreter in
  let module PE = Lrgrep_runtime.Interpreter(Parse_errors.Table_error_message)(I) in
  let rec loop : _ I.env -> _ -> _ I.checkpoint -> _ = fun env tok -> function
    | I.InputNeeded env' as cp ->
      let tok' = get_token lexbuf in
      loop env' tok' (I.offer cp tok')
    | I.Shifting (_, _, _) | I.AboutToReduce (_, _) as cp ->
      loop env tok (I.resume cp)
    | I.Accepted x -> x
    | I.Rejected -> assert false
    | I.HandlingError _ ->
      match PE.run env with
      | [] -> raise_error lexbuf "Syntax error (no handler for it)"
      | matches ->
        if false then
          Printf.eprintf "Matches: %s\n"
            (String.concat ", " (List.map (fun (x, _) -> string_of_int x) matches));
        let rec loop = function
          | [] -> raise_error lexbuf
                    "Syntax error (partial handler did not handle the case)"
          | m :: ms ->
            match Parse_errors.execute_error_message m initial tok with
            | None -> loop ms
            | Some err -> raise_error lexbuf err
        in
        loop matches
  in
  match f lexbuf.lex_curr_p with
  | I.InputNeeded env as cp ->
    Docstrings.init ();
    Lexer_raw.init ();
    let tok = get_token lexbuf in
    let ast = loop env tok (I.offer cp tok) in
    Parsing.clear_parser ();
    Docstrings.warn_bad_docstrings ();
    ast
  | _ -> assert false

let parse
      (type a)
      lexbuf
      ~(f : _ -> a Parser_raw.MenhirInterpreter.checkpoint)
  =
  Parse_errors.error_location := None;
  match parse_exn ~f lexbuf with
  | (_ : a) -> Ok ()
  | exception (Syntax_error syntax_error) -> Error syntax_error
  | exception _exn ->
    Error { msg = "Syntax error"; loc = Location.curr lexbuf }
;;

let use_file lexbuf = parse lexbuf ~f:Parser.Incremental.use_file
let toplevel_phrase lexbuf = parse lexbuf ~f:Parser.Incremental.toplevel_phrase
let parse_val_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_val_longident
let parse_pattern lexbuf = parse lexbuf ~f:Parser.Incremental.parse_pattern
let parse_mty_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_mty_longident
let parse_module_type lexbuf = parse lexbuf ~f:Parser.Incremental.parse_module_type
let parse_module_expr lexbuf = parse lexbuf ~f:Parser.Incremental.parse_module_expr
let parse_mod_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_mod_longident
let parse_mod_ext_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_mod_ext_longident
let parse_expression lexbuf = parse lexbuf ~f:Parser.Incremental.parse_expression
let parse_core_type lexbuf = parse lexbuf ~f:Parser.Incremental.parse_core_type
let parse_constr_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_constr_longident
let parse_any_longident lexbuf = parse lexbuf ~f:Parser.Incremental.parse_any_longident
let interface lexbuf = parse lexbuf ~f:Parser.Incremental.interface
let implementation lexbuf = parse lexbuf ~f:Parser.Incremental.implementation

let parse_file ~path lexbuf =
  if Filename.check_suffix path ".ml"
  then implementation lexbuf
  else if Filename.check_suffix path ".mli"
  then interface lexbuf
  else if Filename.check_suffix path ".mlt"
  then use_file lexbuf
  else Error { msg = "Unknown file type"; loc = parse_error_location lexbuf }
