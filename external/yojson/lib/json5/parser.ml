open Let_syntax.Result

let parser_error pos error =
  let location = Errors.string_of_position pos in
  let msg = Printf.sprintf "%s: %s" location error in
  Error msg

let rec parse_list acc = function
  | [] -> Error "Unexpected end of input"
  | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
  | (Lexer.CLOSE_BRACKET, _) :: xs -> Ok (acc, xs)
  | xs -> (
      let* v, xs = parse xs in
      match xs with
      | [] -> Error "Unexpected end of input"
      | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
      | (Lexer.CLOSE_BRACKET, _) :: xs | (COMMA, _) :: (CLOSE_BRACKET, _) :: xs
        ->
          Ok (v :: acc, xs)
      | (COMMA, _) :: xs -> parse_list (v :: acc) xs
      | (x, pos) :: _ ->
          let s =
            Format.asprintf "Unexpected list token: %a" Lexer.pp_token x
          in
          parser_error pos s)

and parse_assoc acc = function
  | [] -> Error "Unexpected end of input"
  | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
  | (CLOSE_BRACE, _) :: xs -> Ok (acc, xs)
  | (STRING k, _) :: xs | (IDENTIFIER_NAME k, _) :: xs -> (
      match xs with
      | [] -> Error "Unexpected end of input"
      | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
      | (Lexer.COLON, _) :: xs -> (
          let* v, xs = parse xs in
          let item = (k, v) in
          match xs with
          | [] -> Error "Unexpected end of input"
          | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
          | (CLOSE_BRACE, _) :: xs | (COMMA, _) :: (CLOSE_BRACE, _) :: xs ->
              Ok (item :: acc, xs)
          | (COMMA, _) :: xs -> parse_assoc (item :: acc) xs
          | (x, pos) :: _ ->
              let s =
                Format.asprintf "Unexpected assoc list token: %a" Lexer.pp_token
                  x
              in
              parser_error pos s)
      | (x, pos) :: _ ->
          let s =
            Format.asprintf "Expected %a but found %a" Lexer.pp_token
              Lexer.COLON Lexer.pp_token x
          in
          parser_error pos s)
  | (x, pos) :: _ ->
      let s =
        Format.asprintf "Expected string or identifier but found %a"
          Lexer.pp_token x
      in
      parser_error pos s

and parse = function
  | [] -> Error "Unexpected end of input"
  | [ (Lexer.EOF, pos) ] -> parser_error pos "Unexpected end of input"
  | (token, pos) :: xs -> (
      match token with
      | TRUE -> Ok (Ast.Bool true, xs)
      | FALSE -> Ok (Bool false, xs)
      | NULL -> Ok (Null, xs)
      | INT v -> Ok (IntLit v, xs)
      | FLOAT v -> Ok (FloatLit v, xs)
      | INT_OR_FLOAT v -> Ok (FloatLit v, xs)
      | STRING s -> Ok (StringLit s, xs)
      | OPEN_BRACKET ->
          let+ l, xs = parse_list [] xs in
          (Ast.List (List.rev l), xs)
      | OPEN_BRACE ->
          let+ a, xs = parse_assoc [] xs in
          (Ast.Assoc (List.rev a), xs)
      | x ->
          let s = Format.asprintf "Unexpected token: %a" Lexer.pp_token x in
          parser_error pos s)

let parse_from_lexbuf ?(fname = "") ?(lnum = 1) lexbuffer =
  Sedlexing.set_filename lexbuffer fname;
  let pos =
    { Lexing.pos_fname = fname; pos_lnum = lnum; pos_bol = 0; pos_cnum = 0 }
  in
  Sedlexing.set_position lexbuffer pos;
  let* tokens = Lexer.lex [] lexbuffer in
  let+ ast, _unparsed = parse tokens in
  ast

let parse_from_string ?fname ?lnum input =
  parse_from_lexbuf (Sedlexing.Utf8.from_string input) ?fname ?lnum

let parse_from_channel ?fname ?lnum ic =
  parse_from_lexbuf (Sedlexing.Utf8.from_channel ic) ?fname ?lnum

let parse_from_file ?fname ?lnum filename =
  let ic = open_in filename in
  let out = parse_from_channel ?fname ?lnum ic in
  close_in ic;
  out
