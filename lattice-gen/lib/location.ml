type t =
  { start_pos : Lexing.position;
    end_pos : Lexing.position
  }

type 'a located =
  { txt : 'a;
    loc : t
  }

let make start_pos end_pos = { start_pos; end_pos }

let of_lexbuf lexbuf =
  make lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p

let locate lexbuf txt = { txt; loc = of_lexbuf lexbuf }

let merge left right = make left.start_pos right.end_pos

let line_column pos =
  let line = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  line, column

let to_string t =
  let line, column = line_column t.start_pos in
  Printf.sprintf "%s:%d:%d" t.start_pos.Lexing.pos_fname line column

let pp ppf t = Format.pp_print_string ppf (to_string t)
