open Let_syntax.Result

type token =
  | OPEN_PAREN
  | CLOSE_PAREN
  | OPEN_BRACE
  | CLOSE_BRACE
  | OPEN_BRACKET
  | CLOSE_BRACKET
  | COLON
  | COMMA
  | TRUE
  | FALSE
  | NULL
  | FLOAT of string
  | INT_OR_FLOAT of string
  | INT of string
  | STRING of string
  | IDENTIFIER_NAME of string
  | EOF

let pp_token ppf =
  let ps = Format.pp_print_string ppf in
  let pf = Format.fprintf ppf in
  function
  | OPEN_PAREN -> ps "'('"
  | CLOSE_PAREN -> ps "')'"
  | OPEN_BRACE -> ps "'{'"
  | CLOSE_BRACE -> ps "'}'"
  | OPEN_BRACKET -> ps "'['"
  | CLOSE_BRACKET -> ps "']'"
  | COLON -> ps "':'"
  | COMMA -> ps "','"
  | TRUE -> ps "'true'"
  | FALSE -> ps "'false'"
  | NULL -> ps "'null'"
  | FLOAT s -> pf "FLOAT %S" s
  | INT_OR_FLOAT s -> pf "INT_OR_STRING %S" s
  | INT s -> pf "INT %S" s
  | STRING s -> pf "%S" s
  | IDENTIFIER_NAME s -> pf "IDENTIFIER_NAME %S" s
  | EOF -> ps "EOF"

let lexer_error lexbuf =
  let pos_start, _pos_end = Sedlexing.lexing_positions lexbuf in
  let location = Errors.string_of_position pos_start in
  let msg =
    Printf.sprintf "%s: Unexpected character '%s'" location
      (Sedlexing.Utf8.lexeme lexbuf)
  in
  Error msg

let source_character = [%sedlex.regexp? any]
let line_terminator = [%sedlex.regexp? 0x000A | 0x000D | 0x2028 | 0x2029]

let line_terminator_sequence =
  [%sedlex.regexp? 0x000A | 0x000D, Opt 0x000A | 0x2028 | 0x2029]

(* NUMBERS, 7.8.3 *)
let non_zero_digit = [%sedlex.regexp? '1' .. '9']
let decimal_digit = [%sedlex.regexp? '0' .. '9']
let decimal_digits = [%sedlex.regexp? Plus decimal_digit]
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let exponent_indicator = [%sedlex.regexp? 'e' | 'E']

let signed_integer =
  [%sedlex.regexp? decimal_digits | '+', decimal_digits | '-', decimal_digits]

let exponent_part = [%sedlex.regexp? exponent_indicator, signed_integer]

let decimal_integer_literal =
  [%sedlex.regexp? '0' | non_zero_digit, Opt decimal_digits]

let hex_integer_literal =
  [%sedlex.regexp? "0x", Plus hex_digit | "0X", Plus hex_digit]

(* float *)
let float_literal =
  [%sedlex.regexp?
    ( decimal_integer_literal, '.', Opt decimal_digits, Opt exponent_part
    | '.', decimal_digits, Opt exponent_part )]

let json5_float =
  [%sedlex.regexp? float_literal | '+', float_literal | '-', float_literal]

(* int_or_float *)
let int_or_float_literal =
  [%sedlex.regexp? decimal_integer_literal, Opt exponent_part]

let json5_int_or_float =
  [%sedlex.regexp?
    int_or_float_literal | '+', int_or_float_literal | '-', int_or_float_literal]

(* int/hex *)
let int_literal =
  [%sedlex.regexp? decimal_digits | '+', decimal_digits | '-', decimal_digits]

let json5_int =
  [%sedlex.regexp?
    ( hex_integer_literal
    | '+', hex_integer_literal
    | '-', hex_integer_literal
    | int_literal )]

(* STRING LITERALS, 7.8.4 *)
let unicode_escape_sequence =
  [%sedlex.regexp? 'u', hex_digit, hex_digit, hex_digit, hex_digit]

let single_escape_character = [%sedlex.regexp? Chars {|'"\\bfnrtv|}]

let escape_character =
  [%sedlex.regexp? single_escape_character | decimal_digit | 'x' | 'u']

let non_escape_character =
  [%sedlex.regexp? Sub (source_character, (escape_character | line_terminator))]

let character_escape_sequence =
  [%sedlex.regexp? single_escape_character | non_escape_character]

let line_continuation = [%sedlex.regexp? '\\', line_terminator_sequence]
let hex_escape_sequence = [%sedlex.regexp? 'x', hex_digit, hex_digit]

let escape_sequence =
  [%sedlex.regexp?
    ( character_escape_sequence
    | '0', Opt (decimal_digit, decimal_digit)
    | hex_escape_sequence | unicode_escape_sequence )]

let single_string_character =
  [%sedlex.regexp?
    ( Sub (source_character, ('\'' | '\\' | line_terminator))
    | '\\', escape_sequence
    | line_continuation )]

let double_string_character =
  [%sedlex.regexp?
    ( Sub (source_character, ('"' | '\\' | line_terminator))
    | '\\', escape_sequence
    | line_continuation )]

let string_literal =
  [%sedlex.regexp?
    ( '"', Star double_string_character, '"'
    | '\'', Star single_string_character, '\'' )]

(* IDENTIFIER_NAME (keys in objects) *)
let unicode_combining_mark = [%sedlex.regexp? mn | mc]
let unicode_digit = [%sedlex.regexp? nd]
let unicode_connector_punctuation = [%sedlex.regexp? pc]
let unicode_letter = [%sedlex.regexp? lu | ll | lt | lm | lo | nl]
let zwnj = [%sedlex.regexp? 0x200C]
let zwj = [%sedlex.regexp? 0x200D]

let identifier_start =
  [%sedlex.regexp? unicode_letter | '$' | '_' | '\\', unicode_escape_sequence]

let identifier_part =
  [%sedlex.regexp?
    ( identifier_start | unicode_combining_mark | unicode_digit
    | unicode_connector_punctuation | zwnj | zwj )]

let identifier_name = [%sedlex.regexp? identifier_start, Star identifier_part]

(* COMMENTS 7.4 *)
let single_line_comment_char =
  [%sedlex.regexp? Sub (source_character, line_terminator)]

let single_line_comment = [%sedlex.regexp? "//", Star single_line_comment_char]
let multi_line_not_asterisk_char = [%sedlex.regexp? Sub (source_character, '*')]
let multi_line_not_slash_char = [%sedlex.regexp? Sub (source_character, '/')]

let multi_line_comment_char =
  [%sedlex.regexp?
    multi_line_not_asterisk_char | '*', Plus multi_line_not_slash_char]

let multi_line_comment =
  [%sedlex.regexp? "/*", Star multi_line_comment_char, Opt '*', "*/"]

let comment = [%sedlex.regexp? multi_line_comment | single_line_comment]

let white_space =
  [%sedlex.regexp? 0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xFEFF | zs]

let string_lex_single lexbuf strbuf =
  let lexeme = Sedlexing.Utf8.lexeme in
  let rec lex lexbuf strbuf =
    match%sedlex lexbuf with
    | '\'' -> Ok (Buffer.contents strbuf)
    | '\\', escape_sequence ->
        let* s = Unescape.unescape (lexeme lexbuf) in
        Buffer.add_string strbuf s;
        lex lexbuf strbuf
    | line_continuation -> lex lexbuf strbuf
    | Sub (source_character, ('\'' | line_terminator)) ->
        Buffer.add_string strbuf (lexeme lexbuf);
        lex lexbuf strbuf
    | _ -> lexer_error lexbuf
  in
  lex lexbuf strbuf

let string_lex_double lexbuf strbuf =
  let lexeme = Sedlexing.Utf8.lexeme in
  let rec lex lexbuf strbuf =
    match%sedlex lexbuf with
    | '"' -> Ok (Buffer.contents strbuf)
    | '\\', escape_sequence ->
        let* s = Unescape.unescape (lexeme lexbuf) in
        Buffer.add_string strbuf s;
        lex lexbuf strbuf
    | line_continuation -> lex lexbuf strbuf
    | Sub (source_character, ('"' | line_terminator)) ->
        Buffer.add_string strbuf (lexeme lexbuf);
        lex lexbuf strbuf
    | _ -> lexer_error lexbuf
  in
  lex lexbuf strbuf

let string_lex lexbuf quote =
  let strbuf = Buffer.create 200 in
  match quote with
  | "'" -> string_lex_single lexbuf strbuf
  | {|"|} -> string_lex_double lexbuf strbuf
  | _ -> Error (Printf.sprintf "Invalid string quote %S" quote)

let rec lex tokens buf =
  let lexeme = Sedlexing.Utf8.lexeme in
  let pos, _ = Sedlexing.lexing_positions buf in
  match%sedlex buf with
  | '(' -> lex ((OPEN_PAREN, pos) :: tokens) buf
  | ')' -> lex ((CLOSE_PAREN, pos) :: tokens) buf
  | '{' -> lex ((OPEN_BRACE, pos) :: tokens) buf
  | '}' -> lex ((CLOSE_BRACE, pos) :: tokens) buf
  | '[' -> lex ((OPEN_BRACKET, pos) :: tokens) buf
  | ']' -> lex ((CLOSE_BRACKET, pos) :: tokens) buf
  | ':' -> lex ((COLON, pos) :: tokens) buf
  | ',' -> lex ((COMMA, pos) :: tokens) buf
  | Chars {|"'|} ->
      let* s = string_lex buf (lexeme buf) in
      lex ((STRING s, pos) :: tokens) buf
  | multi_line_comment | single_line_comment | white_space | line_terminator ->
      lex tokens buf
  | "true" -> lex ((TRUE, pos) :: tokens) buf
  | "false" -> lex ((FALSE, pos) :: tokens) buf
  | "null" -> lex ((NULL, pos) :: tokens) buf
  | json5_float ->
      let s = lexeme buf in
      lex ((FLOAT s, pos) :: tokens) buf
  | json5_int ->
      let s = lexeme buf in
      lex ((INT s, pos) :: tokens) buf
  | json5_int_or_float ->
      let s = lexeme buf in
      lex ((INT_OR_FLOAT s, pos) :: tokens) buf
  | identifier_name ->
      let s = lexeme buf in
      lex ((IDENTIFIER_NAME s, pos) :: tokens) buf
  | eof -> Ok (List.rev ((EOF, pos) :: tokens))
  | _ -> lexer_error buf
