open Lexing

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

let none =
  let pos = { Lexing.dummy_pos with pos_fname = "_none_" } in
  { loc_start = pos; loc_end = pos; loc_ghost = true }

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
}

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
}

let print_loc ppf loc =
  let file_valid = function
    | "_none_" ->
        (* This is a dummy placeholder, but we print it anyway to please editors
           that parse locations in error messages (e.g. Emacs). *)
        true
    | "" | "//toplevel//" -> false
    | _ -> true
  in
  let line_valid line = line > 0 in
  let chars_valid ~startchar ~endchar = startchar <> -1 && endchar <> -1 in

  let file = loc.loc_start.pos_fname in
  let startline = loc.loc_start.pos_lnum in
  let endline = loc.loc_end.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_end.pos_bol in

  let first = ref true in
  let capitalize s =
    if !first then (first := false; String.capitalize_ascii s)
    else s in
  let comma () =
    if !first then () else Format.fprintf ppf ", " in

  Format.fprintf ppf "@{<loc>";

  if file_valid file then
    Format.fprintf ppf "%s \"%s\"" (capitalize "file") file;

  (* Print "line 1" in the case of a dummy line number. This is to please the
     existing setup of editors that parse locations in error messages (e.g.
     Emacs). *)
  comma ();
  let startline = if line_valid startline then startline else 1 in
  let endline = if line_valid endline then endline else startline in
  begin if startline = endline then
    Format.fprintf ppf "%s %i" (capitalize "line") startline
  else
    Format.fprintf ppf "%s %i-%i" (capitalize "lines") startline endline
  end;

  if chars_valid ~startchar ~endchar then (
    comma ();
    Format.fprintf ppf "%s %i-%i" (capitalize "characters") startchar endchar
  );

  Format.fprintf ppf "@}"
