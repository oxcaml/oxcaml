let from_lexbuf ~input_name lexbuf =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = input_name };
  try Parser.file Lexer.token lexbuf with
  | Error.Error _ as exn -> raise exn
  | Parser.Error ->
    let loc = Location.of_lexbuf lexbuf in
    Error.failf loc "syntax error"

let from_string ~input_name source =
  from_lexbuf ~input_name (Lexing.from_string source)

let from_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      let source = really_input_string ic len in
      from_string ~input_name:path source)
