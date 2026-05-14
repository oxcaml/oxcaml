(******************************************************************************
 *                                 Sexplib                                    *
 *                                                                            *
 * Copyright (c) 2005--2026 Jane Street Group, LLC                            *
 *   <opensource-contacts@janestreet.com>                                     *
 ******************************************************************************)

(* This file has been adapted from
   https://github.com/janestreet/sexplib/blob/16ed74eb78c67e76e6cf9219c1b06b93b8c01c53/src/lexer.mll
 *)

{
  open Sexp_parser
  open Lexing

  let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

  let dec_code c1 c2 c3 =
    100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

  let hex_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 =
      if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48 in
    let d2 = Char.code c2 in
    let val2 =
      if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48 in
    val1 * 16 + val2

  let new_line_with_offset lexbuf diff =
    let lex_curr_p = lexbuf.lex_curr_p in
    if lex_curr_p != dummy_pos then
      lexbuf.lex_curr_p <-
        {
          lex_curr_p with
          pos_lnum = lex_curr_p.pos_lnum + 1;
          pos_bol = lex_curr_p.pos_cnum - diff;
        }

  (* same length computation as in [Lexing.lexeme] *)
  let lexeme_len { lex_start_pos; lex_curr_pos; _ } = lex_curr_pos - lex_start_pos
}

let lf = '\n'
let lf_cr = ['\n' '\r']
let dos_newline = "\r\n"
let blank = [' ' '\t' '\012']
let unquoted = [^ ';' '(' ')' '"'] # blank # lf_cr

let unquoted_start =
    unquoted # ['#' '|'] | '#' unquoted # ['|'] | '|' unquoted # ['#']

let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']

rule main buf = parse
    | lf | dos_newline                            { new_line lexbuf;
                                                    main buf lexbuf }
    | blank+                                      { main buf lexbuf }
    | ';' (_ # lf_cr)*                            { main buf lexbuf }
    | '('                                         { LPAREN }
    | ')'                                         { RPAREN }
    | '"'                                         {
        let pos = Lexing.lexeme_start_p lexbuf in
        STRING (scan_string buf pos lexbuf)
    }
    | "#;"                                        { HASH_SEMI }
    | "#|"                                        {
        let pos = Lexing.lexeme_start_p lexbuf in
        scan_block_comment buf [pos] lexbuf;
        main buf lexbuf
    }
    | "|#"                                        {
        failwith "illegal end of comment"
    }
    | "#" "#"+ "|" unquoted* (* unquoted_start can match ##, so ##| (which should be
                              refused) would not not be parsed by this case if the regexp
                              on the left was not there *)
    | "|" "|"+ "#" unquoted*
    | unquoted_start unquoted* ("#|" | "|#") unquoted*
      { failwith "comment tokens in unquoted atom" }
    | "#" | "|" | unquoted_start unquoted* as str { STRING str }
    | eof                                         { EOF }

and scan_string buf start = parse
  | '"'
      {
        let str = Buffer.contents buf in
        Buffer.clear buf;
        str
      }
  | '\\' lf [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 2 in
        new_line_with_offset lexbuf len;
        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 3 in
        new_line_with_offset lexbuf len;
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3)
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then (
          let { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } = lexeme_end_p lexbuf in
          let msg =
            Printf.sprintf
              "Lexer.scan_string: \
               illegal escape at line %d char %d: `\\%c%c%c'"
              pos_lnum (pos_cnum - pos_bol - 3)
              c1 c2 c3 in
          failwith msg);
        Buffer.add_char buf (Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);
        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        scan_string buf start lexbuf
      }
  | lf
      {
        new_line lexbuf;
        Buffer.add_char buf '\n';
        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_subbytes buf lexbuf.lex_buffer ofs len;
        scan_string buf start lexbuf
      }
  | eof
      {
        let msg =
          Printf.sprintf
            "Lexer.scan_string: unterminated string at line %d char %d"
            start.pos_lnum (start.pos_cnum - start.pos_bol)
        in
        failwith msg
      }

and scan_block_comment buf locs = parse
  | ('#'* | '|'*) lf
      { new_line lexbuf; scan_block_comment buf locs lexbuf }
  | (('#'* | '|'*) [^ '"' '#' '|'] # lf)+
      { scan_block_comment buf locs lexbuf }
  | ('#'* | '|'*) '"'
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        ignore (scan_string buf pos lexbuf);
        scan_block_comment buf locs lexbuf
      }
  | '#'+ '|'
    {
      let cur = lexeme_end_p lexbuf in
      let start = { cur with pos_cnum = cur.pos_cnum - 2 } in
      scan_block_comment buf (start :: locs) lexbuf
    }
  | '|'+ '#'
      {
        match locs with
        | [_] -> () (* the comment is finished *)
        | _ :: (_ :: _ as t) -> scan_block_comment buf t lexbuf
        | [] -> assert false  (* impossible *)
      }
  | eof
      {
        match locs with
        | [] -> assert false
        | { pos_lnum; pos_bol; pos_cnum; pos_fname = _ } :: _ ->
            let msg =
              Printf.sprintf "Sexp_lexer.scan_block_comment: \
                unterminated block comment at line %d char %d"
                pos_lnum (pos_cnum - pos_bol)
            in
            failwith msg
      }

{
  let main ?buf =
    let buf =
      match buf with
      | None -> Buffer.create 64
      | Some buf ->
        Buffer.clear buf;
        buf
    in
    main buf
}
