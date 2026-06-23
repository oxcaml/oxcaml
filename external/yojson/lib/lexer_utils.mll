rule read_junk buf n = parse
  | eof { () }
  | _ {
     if n <= 0 then ()
     else begin
       Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
       read_junk buf (n - 1) lexbuf
     end
     }

{
let read_junk_without_positions buf n (lexbuf : Lexing.lexbuf) =
  let lex_abs_pos = lexbuf.lex_abs_pos in
  let lex_start_pos = lexbuf.lex_start_pos in
  read_junk buf n lexbuf;
  lexbuf.lex_start_pos <- lex_start_pos + 1;
  lexbuf.lex_abs_pos <- lex_abs_pos
}
