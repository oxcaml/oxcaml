{
open Parser
}

let whitespace = [' ' '\t' '\r']
let newline = '\n'
let ident_start = ['A'-'Z' 'a'-'z' '_']
let ident_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let ident = ident_start ident_char*

rule token = parse
  | whitespace+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "->" { ARROW }
  | "-|" { ADJOINT }
  | "⊣" { ADJOINT }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | ":" { COLON }
  | ";" { SEMI }
  | "^" { CARET }
  | ident as text { IDENT (Location.locate lexbuf text) }
  | eof { EOF }
  | _ as ch { Error.failf (Location.of_lexbuf lexbuf) "unexpected character '%c'" ch }
