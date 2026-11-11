{
open StdLabels

type version = int * int

type version_range =
  | Only of version (* ex: [%%expect_in 5.3 *)
  | Up_to of version (* ex: [%%expect_in <= 5.3 *)
  | From of version (* ex: [%%expect_in >= 5.3 *)
  | Between of version * version (* ex: [%%expect_in 5.0 <=> 5.3 *)

(*[%%ignore], [%%expect] or [%%expect_in]*)
type expect_block =
  | Ignore
  | Regular
  | Versioned of (version_range * string) list

type chunk =
  { phrases : string
  ; phrases_start : Lexing.position
  ; expect : expect_block
  }

let make_version major minor =
  let major = int_of_string major in
  let minor = int_of_string minor in
  (major, minor)

let extract_string all_file start lexbuf =
  let pos = start.Lexing.pos_cnum in
  let len = Lexing.lexeme_start lexbuf - pos in
  String.sub all_file ~pos ~len
}

let digit = ['0'-'9']

(* Entrypoint
   Parses blocks of code to execute seperated by [%%expect{|...|}] statement.
   Code blocks can be separated by a single [%%expect{|...|}] statement or by a
   series of [%%expect_in <version-range> {|...|}]. *)
rule code all_file phrases_start = parse
  | "[%%expect{|\n" {
    let phrases = extract_string all_file phrases_start lexbuf in
    Lexing.new_line lexbuf;
    let chunk = {phrases; phrases_start; expect = Regular} in
    chunk :: expectation all_file lexbuf
  }
  | "[%%ignore]\n" {
    let phrases = extract_string all_file phrases_start lexbuf in
    Lexing.new_line lexbuf;
    let chunk = {phrases; phrases_start; expect = Ignore} in
    chunk :: code all_file lexbuf.lex_curr_p lexbuf
  }
  | "[%%expect_in " (digit+ as major) '.' (digit+ as minor) " {|\n" {
      let phrases = extract_string all_file phrases_start lexbuf in
      Lexing.new_line lexbuf;
      let version = make_version major minor in
      let range = Only version in
      let start = lexbuf.lex_curr_p in
      versioned_expectation_content all_file
        (phrases_start, phrases) [] (range, start) lexbuf
  }
  | "[%%expect_in >= " (digit+ as major) '.' (digit+ as minor) " {|\n" {
      let phrases = extract_string all_file phrases_start lexbuf in
      Lexing.new_line lexbuf;
      let version = make_version major minor in
      let range = From version in
      let start = lexbuf.lex_curr_p in
      versioned_expectation_content all_file
        (phrases_start, phrases) [] (range, start) lexbuf
  }
  | "[%%expect_in <= " (digit+ as major) '.' (digit+ as minor) " {|\n" {
      let phrases = extract_string all_file phrases_start lexbuf in
      Lexing.new_line lexbuf;
      let version = make_version major minor in
      let range = Up_to version in
      let start = lexbuf.lex_curr_p in
      versioned_expectation_content all_file
        (phrases_start, phrases) [] (range, start) lexbuf
  }
  | "[%%expect_in "
    (digit+ as major1) '.' (digit+ as minor1)
    " <=> "
    (digit+ as major2) '.' (digit+ as minor2)
    " {|\n" {
      let phrases = extract_string all_file phrases_start lexbuf in
      Lexing.new_line lexbuf;
      let v1 = make_version major1 minor1 in
      let v2 = make_version major2 minor2 in
      let range = Between (v1, v2) in
      let start = lexbuf.lex_curr_p in
      versioned_expectation_content all_file
        (phrases_start, phrases) [] (range, start) lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    code all_file phrases_start lexbuf
  }
  | eof {
    let pos = phrases_start.Lexing.pos_cnum in
    let len = String.length all_file - pos in
    if pos > 0 then begin
      let phrases = String.sub all_file ~pos ~len in
      if String.trim phrases = "" then
        []
      else
        [{phrases_start; phrases; expect = Regular}]
    end else
      []
  }

and expectation all_file = parse
  | "|}]\n" {
      Lexing.new_line lexbuf;
      code all_file lexbuf.lex_curr_p lexbuf
    }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    expectation all_file lexbuf
  }

(* Parses the content of a [%%expect_in .. {| ... |}] block along with following
   blocks in the same group *)
and versioned_expectation_content all_file code_chunk vexpects curr = parse
  | "|}]\n[%%expect_in " (digit+ as major) '.' (digit+ as minor) " {|\n" {
    let range, start = curr in
    let s = extract_string all_file start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let version = make_version major minor in
    let next_range = Only version in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content all_file code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n[%%expect_in >= " (digit+ as major) '.' (digit+ as minor) " {|\n" {
    let range, start = curr in
    let s = extract_string all_file start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let version = make_version major minor in
    let next_range = From version in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content all_file code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n[%%expect_in <= " (digit+ as major) '.' (digit+ as minor) " {|\n" {
    let range, start = curr in
    let s = extract_string all_file start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let version = make_version major minor in
    let next_range = Up_to version in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content all_file code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n[%%expect_in "
    (digit+ as major1) '.' (digit+ as minor1)
    " <=> "
    (digit+ as major2) '.' (digit+ as minor2)
    " {|\n" {
    let range, start = curr in
    let s = extract_string all_file start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let v1 = make_version major1 minor1 in
    let v2 = make_version major2 minor2 in
    let next_range = Between (v1, v2) in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content all_file code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n" {
    let range, start = curr in
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub all_file ~pos ~len in
    Lexing.new_line lexbuf;
    let vexpects = List.rev ((range, s)::vexpects) in
    let phrases_start, phrases = code_chunk in
    let chunk = {phrases; phrases_start; expect = Versioned vexpects} in
    chunk :: code all_file lexbuf.lex_curr_p lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    versioned_expectation_content all_file code_chunk vexpects curr lexbuf
  }

{
  let split_file ~file_contents lexbuf =
    code file_contents lexbuf.Lexing.lex_curr_p lexbuf
}
