(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The lexer definition *)

{
open Misc
open Std
open Lexing
open Parser_raw

type keywords = (string, Parser_raw.token) Hashtbl.t

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Empty_character_literal
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option

exception Error of error * Location.t

(* Monad in which the lexer evaluates *)
type 'a result =
  | Return of 'a
  | Refill of (unit -> 'a result)
  | Fail of error * Location.t

let return a = Return a

let fail lexbuf e = Fail (e, Location.curr lexbuf)
let fail_loc e l = Fail (e,l)

let rec (>>=) (m : 'a result) (f : 'a -> 'b result) : 'b result =
  match m with
  | Return a -> f a
  | Refill u ->
    Refill (fun () -> u () >>= f)
  | Fail _ as e -> e

type preprocessor = (Lexing.lexbuf -> Parser_raw.token) -> Lexing.lexbuf -> Parser_raw.token

type state = {
  keywords: keywords;
  mutable buffer: Buffer.t;
  mutable string_start_loc: Location.t;
  mutable comment_start_loc: Location.t list;
  mutable preprocessor: preprocessor option;
}

let make ?preprocessor keywords = {
  keywords;
  buffer = Buffer.create 17;
  string_start_loc = Location.none;
  comment_start_loc = [];
  preprocessor;
}

let lABEL m = m >>= fun v -> return (LABEL v)
let oPTLABEL m = m >>= fun v -> return (OPTLABEL v)

let rec catch m f = match m with
  | Fail (e,l) -> f e l
  | Refill next -> Refill (fun () -> catch (next ()) f)
  | Return _ -> m

(* The table of keywords *)

let keyword_table : keywords =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "exclave_", EXCLAVE;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "global_", GLOBAL;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "kind_abbrev_", KIND_ABBREV;
    "kind_of_", KIND_OF;
    "lazy", LAZY;
    "let", LET;
    "local_", LOCAL;
    "match", MATCH;
    "method", METHOD;
    "mod", MOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "once_", ONCE;
    "open", OPEN;
    "or", OR;
    "overwrite_", OVERWRITE;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "stack_", STACK;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "unique_", UNIQUE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr");
]

let keywords l = create_hashtable 11 l

let lookup_keyword name =
  match Hashtbl.find keyword_table name with
  | kw -> kw
  | exception Not_found ->
     LIDENT name

(* To buffer string literals *)

let list_keywords =
  let add_kw str _tok kws = str :: kws in
  let init = Hashtbl.fold add_kw keyword_table [] in
  fun keywords ->
    Hashtbl.fold add_kw keywords init

let store_string_char buf c = Buffer.add_char buf c
let store_substring buf s ~pos ~len = Buffer.add_substring buf s pos len

let store_normalized_newline buf newline =
  (* #12502: we normalize "\r\n" to "\n" at lexing time,
     to avoid behavior difference due to OS-specific
     newline characters in string literals.

     (For example, Git for Windows will translate \n in versioned
     files into \r\n sequences when checking out files on Windows. If
     your code contains multiline quoted string literals, the raw
     content of the string literal would be different between Git for
     Windows users and all other users. Thanks to newline
     normalization, the value of the literal as a string constant will
     be the same no matter which programming tools are used.)

     Many programming languages use the same approach, for example
     Java, Javascript, Kotlin, Python, Swift and C++.
  *)
  (* Our 'newline' regexp accepts \r*\n, but we only wish
     to normalize \r?\n into \n -- see the discussion in #12502.
     All carriage returns except for the (optional) last one
     are reproduced in the output. We implement this by skipping
     the first carriage return, if any. *)
  let len = String.length newline in
  if len = 1
  then store_string_char buf '\n'
  else store_substring buf newline ~pos:1 ~len:(len - 1)

(* To store the position of the beginning of a string and comment *)
let in_comment state = state.comment_start_loc <> []

let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol)

(* Syntax mode configuration for the #syntax directive *)
module Syntax_mode = struct
  let quotations = ref Config.syntax_quotations
end

let _reset_syntax_mode () =
  Syntax_mode.quotations := Config.syntax_quotations

(* See the comment on the [directive] lexer. *)
type directive_lexing_already_consumed =
   | Hash
   | Hash_and_line_num of { line_num : string }

type deferred_token =
  { token : token
  ; start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

(* This queue will only ever have 0 or 1 elements in it. We use it
   instead of an [option ref] for its convenient interface.
*)
let deferred_tokens : deferred_token Queue.t = Queue.create ()

(* Effectively splits the text in the lexer's current "window" (defined below)
   into two halves. The current call to the lexer will return the first half of
   the text in the window, and the next call to the lexer will return the second
   half (of length [len]) of the text in the window.

   "window" refers to the text matched by a production of the lexer. It spans
   from [lexer.lex_start_p] to [lexer.lex_curr_p].

   The function accomplishes this splitting by doing two things:
    - It sets the current window of the lexbuf to only account for the
      first half of the text. (The first half is of length: |text|-len.)
    - It enqueues a token into [deferred_tokens] such that, the next time the
      lexer is called, it will return the specified [token] *and* set the window
      of the lexbuf to account for the second half of the text. (The second half
      is of length: |text|.)

   This business with setting the window of the lexbuf is only so that error
   messages point at the right place in the program text.
*)
let enqueue_token_from_end_of_lexbuf_window (lexbuf : Lexing.lexbuf) token ~len =
  let suffix_end = lexbuf.lex_curr_p in
  let suffix_start =
    { suffix_end with pos_cnum = suffix_end.pos_cnum - len }
  in
  lexbuf.lex_curr_p <- suffix_start;
  Queue.add
    { token; start_pos = suffix_start; end_pos = suffix_end }
    deferred_tokens

(* Note [Lexing hack for float#]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   This note describes a non-backward-compatible Jane Street--internal change to
   the lexer.

   We want the lexer to lex [float#] differently than [float #]. [float#] is the
   new syntax for the unboxed float type. It veers close to the syntax for the
   type of all objects belonging to a class [c], which is [#c]. The way we
   navigate this veering is by producing the following tokens for these source
   program examples, where LIDENT(s) is an LIDENT with text [s].

   float#c   ==> LIDENT(float) HASH_SUFFIX LIDENT(c)
   float# c  ==> LIDENT(float) HASH_SUFFIX LIDENT(c)
   float # c ==> LIDENT(float) HASH LIDENT(c)
   float #c  ==> LIDENT(float) HASH LIDENT(c)

   (A) The parser interprets [LIDENT(float) HASH_SUFFIX LIDENT(c)] as
       "the type constructor [c] applied to the unboxed float type."
   (B) The parser interprets [LIDENT(float) HASH LIDENT(c)] as
       "the type constructor [#c] applied to the usual boxed float type."

   This is not a backward-compatible change. In upstream ocaml, the lexer
   produces [LIDENT(float) HASH LIDENT(c)] for all the above source programs.

   But, this isn't problematic: everybody puts a space before '#c' to mean (B).
   No existing code writes things like [float#c] or indeed [float# c].

   We accomplish this hack by setting some global mutable state upon seeing
   an identifier immediately followed by a hash. When that state is set, we
   will produce [HASH_SUFFIX] the next time the lexer is called. This is
   done in [enqueue_hash_suffix_from_end_of_lexbuf_window].

   Note [Lexing hack for hash operators]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   To complicate the above story, we don't want to treat the # in the
   below program as HASH_SUFFIX:

   x#~#y

   We instead want:

   x#~#y ==> LIDENT(x) HASHOP(#~#) LIDENT(y)

   This is to allow for infix hash operators. We add an additional hack, in
   the style of Note [Lexing hack for float#], where the lexer consumes [x#~#]
   all at once, but produces LIDENT(x) from the current call to the lexer and
   HASHOP(#~#) from the next call to the lexer. This is done in
   [enqueue_hashop_from_end_of_lexbuf_window].
 *)

let enqueue_hash_suffix_from_end_of_lexbuf_window lexbuf =
  enqueue_token_from_end_of_lexbuf_window lexbuf HASH_SUFFIX ~len:1

let enqueue_hashop_from_end_of_lexbuf_window lexbuf ~hashop =
  enqueue_token_from_end_of_lexbuf_window lexbuf (HASHOP hashop)
    ~len:(String.length hashop)

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_uchar state lexbuf u =
  if in_comment state
  then Buffer.add_string state.buffer (Lexing.lexeme lexbuf)
  else Buffer.add_utf_8_uchar state.buffer u


let compute_quoted_string_idloc {Location.loc_start = orig_loc; _ } shift id =
  let id_start_pos = orig_loc.Lexing.pos_cnum + shift in
  let loc_start =
    Lexing.{orig_loc with pos_cnum = id_start_pos }
  in
  let loc_end =
    Lexing.{orig_loc with pos_cnum = id_start_pos + String.length id }
  in
  {Location. loc_start ; loc_end ; loc_ghost = false }

let wrap_string_lexer f state lexbuf =
  Buffer.reset state.buffer;
  state.string_start_loc <- Location.curr lexbuf;
  f state lexbuf >>= fun loc_end ->
  lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
  let loc =
    Location.{
      loc_ghost = false;
      loc_start = state.string_start_loc.Location.loc_end;
      loc_end;
    }
  in
  state.string_start_loc <- Location.none;
  return (Buffer.contents state.buffer, loc)

let directive_error
    (lexbuf : Lexing.lexbuf) explanation ~directive ~already_consumed
  =
  let directive_prefix =
    match already_consumed with
    | Hash -> "#"
    | Hash_and_line_num { line_num } -> "#" ^ line_num
  in
  (* Set the lexbuf's current window to extend to the start of
     the directive so the error message's location is more accurate.
  *)
  lexbuf.lex_start_p <-
    { lexbuf.lex_start_p with
      pos_cnum =
        lexbuf.lex_start_p.pos_cnum - String.length directive_prefix
    };
  raise (Error (Invalid_directive (directive_prefix ^ directive, Some explanation),
                Location.curr lexbuf))

(* to translate escape sequences *)

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  fail lexbuf error

let char_for_decimal_code state lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "%d is outside the range of legal characters (0-255)." c)
  else return (Char.chr c)

let char_for_octal_code state lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else return (Char.chr c)

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let uchar_for_uchar_escape lexbuf =
  let illegal_escape lexbuf reason =
    let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
    raise (Error (error, Location.curr lexbuf))
  in
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)

let keyword_or state s default =
  try Hashtbl.find state.keywords s
      with Not_found -> try Hashtbl.find keyword_table s
  with Not_found -> default

let is_keyword name =
  match lookup_keyword name with
  | LIDENT _ -> false
  | _ -> true
let () = Lexer.is_keyword_ref := is_keyword

let check_label_name lexbuf name =
  if is_keyword name
  then fail lexbuf (Keyword_as_label name)
  else return name

(* Update the current location with file name and line number. *)

let update_loc lexbuf _file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = pos.pos_fname
    (*match file with
      | None -> pos.pos_fname
      | Some s -> s*)
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.deprecated (Location.curr lexbuf)
    "ISO-Latin1 characters in identifiers"
;;

let float ~maybe_hash lit modifier =
  match maybe_hash with
  | "#" -> return (HASH_FLOAT (lit, modifier))
  | "" -> return (FLOAT (lit, modifier))
  | unexpected -> fatal_error ("expected # or empty string: " ^ unexpected)

let int ~maybe_hash lit modifier =
  match maybe_hash with
  | "#" -> return (HASH_INT (lit, modifier))
  | "" -> return (INT (lit, modifier))
  | unexpected -> fatal_error ("expected # or empty string: " ^ unexpected)
;;

let produce_and_backtrack lexbuf token back =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - back;
  let curpos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - back };
  return token

let char ~maybe_hash lit =
  match maybe_hash with
  | "#" -> HASH_CHAR (lit)
  | "" -> CHAR (lit)
  | unexpected -> fatal_error ("expected # or empty string: " ^ unexpected)

let skip_hash ~maybe_hash =
  match maybe_hash with
  | "#" -> 1
  | "" -> 0
  | unexpected -> fatal_error ("expected # or empty string: " ^ unexpected)

(* Error report *)

open Format

let prepare_error loc = function
  | Illegal_character c ->
      Location.errorf ~loc "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape (s, explanation) ->
      Location.errorf ~loc
        "Illegal backslash escape in string or character (%s)%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)
  | Reserved_sequence (s, explanation) ->
      Location.errorf ~loc
        "Reserved character sequence: %s%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf " %s" expl)
  | Unterminated_comment _ ->
      Location.errorf ~loc "Comment not terminated"
  | Unterminated_string ->
      Location.errorf ~loc "String literal not terminated"
  | Unterminated_string_in_comment (_, literal_loc) ->
      Location.errorf ~loc
        "This comment contains an unterminated string literal"
        ~sub:[Location.msg ~loc:literal_loc "String literal begins here"]
  | Empty_character_literal ->
      let msg = "Illegal empty character literal ''" in
      let sub =
        [Location.msg
           "Hint: Did you mean ' ' or a type variable 'a?"] in
      Location.error ~loc ~sub msg
  | Keyword_as_label kwd ->
      Location.errorf ~loc
        "%a is a keyword, it cannot be used as label name" Style.inline_code kwd
  | Invalid_literal s ->
      Location.errorf ~loc "Invalid literal %s" s
  | Invalid_directive (dir, explanation) ->
      Location.errorf ~loc "Invalid lexer directive %S%t" dir
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
        Some (prepare_error loc err)
      | _ ->
        None
    )

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' '\128'-'\255']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 = identchar
  (*['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']*)
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolcharnopercent =
  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let symbolchar_or_hash =
  symbolchar | '#'
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

let ident = (lowercase | uppercase) identchar*
let extattrident = ident ('.' ident)*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*) ?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']
let raw_ident_escape = "\\#"


refill {fun k lexbuf -> Refill (fun () -> k lexbuf)}


rule token state = parse
  | ("\\" as bs) newline {
      match state.preprocessor with
      | None -> fail lexbuf (Illegal_character bs)
      | Some _ ->
        update_loc lexbuf None 1 false 0;
        token state lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        match state.preprocessor with
        | None -> token state lexbuf
        | Some _ -> return EOL
      }
  | blank +
      { token state lexbuf }
  | ".<"
      { return DOTLESS }
  | ">."
      { return (keyword_or state (Lexing.lexeme lexbuf) (INFIXOP0 ">.")) }
  | ".~"
      { return (keyword_or state (Lexing.lexeme lexbuf) DOTTILDE) }
  | "_"
      { return UNDERSCORE }
  | "~"
      { return TILDE }
      (*
  | ".~"
      { fail lexbuf
          (Reserved_sequence (".~", Some "is reserved for use in MetaOCaml")) }
      *)
  | "~" raw_ident_escape (lowercase identchar * as name) ':'
      { return (LABEL name) }
  | "~" (lowercase identchar * as name) ':'
      { lABEL (check_label_name lexbuf name) }
  | "~" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf;
        return (LABEL name) }
  | "?"
      { return QUESTION }
  | "?" raw_ident_escape (lowercase identchar * as name) ':'
      { return (OPTLABEL name) }
  | "?" (lowercase identchar * as name) ':'
      { oPTLABEL (check_label_name lexbuf name) }
  | "?" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf; return (OPTLABEL name) }
  (* Lowercase identifiers are split into 3 cases, and the order matters
     (longest to shortest).
  *)
  | (lowercase identchar * as name) ('#' symbolchar_or_hash+ as hashop)
      (* See Note [Lexing hack for hash operators] *)
      { enqueue_hashop_from_end_of_lexbuf_window lexbuf ~hashop;
        return (try Hashtbl.find state.keywords name
              with Not_found ->
              lookup_keyword name) }
  | (lowercase identchar * as name) '#'
      (* See Note [Lexing hack for float#] *)
      { enqueue_hash_suffix_from_end_of_lexbuf_window lexbuf;
        return (try Hashtbl.find state.keywords name
              with Not_found ->
              lookup_keyword name) }
  | raw_ident_escape (lowercase identchar * as name)
    { return (LIDENT name) }
  | lowercase identchar * as name
    { return (try Hashtbl.find state.keywords name
              with Not_found ->
              lookup_keyword name) }
  (* Lowercase latin1 identifiers are split into 3 cases, and the order matters
     (longest to shortest).
  *)
  | (lowercase_latin1 identchar_latin1 * as name)
      ('#' symbolchar_or_hash+ as hashop)
      (* See Note [Lexing hack for hash operators] *)
      { warn_latin1 lexbuf;
        enqueue_hashop_from_end_of_lexbuf_window lexbuf ~hashop;
        return (LIDENT name) }
  | (lowercase_latin1 identchar_latin1 * as name) '#'
      (* See Note [Lexing hack for float#] *)
      { warn_latin1 lexbuf;
        enqueue_hash_suffix_from_end_of_lexbuf_window lexbuf;
        return (LIDENT name) }
  | lowercase_latin1 identchar_latin1 * as name
      { warn_latin1 lexbuf; return (LIDENT name) }
  | uppercase identchar * as name
    { (* Capitalized keywords for OUnit *)
      return (try Hashtbl.find state.keywords name
              with Not_found ->
              try Hashtbl.find keyword_table name
              with Not_found ->
                UIDENT name) }
  | uppercase_latin1 identchar_latin1 * as name
      { warn_latin1 lexbuf; return (UIDENT name) }
  (* This matches either an integer literal or a directive. If the text "#2"
     appears at the beginning of a line that lexes as a directive, then it
     should be treated as a directive and not an unboxed int. This is acceptable
     because "#2" isn't a valid unboxed int anyway because it lacks a suffix;
     the parser rejects unboxed-ints-lacking-suffixes with a more descriptive
     error message.
  *)
  | ('#'? as maybe_hash) (int_literal as lit)
      { if at_beginning_of_line lexbuf.lex_start_p && maybe_hash = "#" then
          try directive state (Hash_and_line_num { line_num = lit }) lexbuf
          with Failure _ -> int ~maybe_hash lit None
        else int ~maybe_hash lit None
      }
  | ('#'? as maybe_hash) (int_literal as lit) (literal_modifier as modif)
      { int ~maybe_hash lit (Some modif) }
  | ('#'? as maybe_hash)
    (float_literal | hex_float_literal as lit)
      { float ~maybe_hash lit None }
  | ('#'? as maybe_hash)
    (float_literal | hex_float_literal as lit) (literal_modifier as modif)
      { float ~maybe_hash lit (Some modif) }
  | '#'? (float_literal | hex_float_literal | int_literal) identchar+ as invalid
      { fail lexbuf (Invalid_literal invalid) }
  | "\""
      { wrap_string_lexer string state lexbuf >>= fun (str, loc) ->
        return (STRING (str, loc, None)) }
  | "\'\'"
      { wrap_string_lexer string state lexbuf >>= fun (str, loc) ->
        return (STRING (str, loc, None)) }
  | "{" (lowercase* as delim) "|"
      { wrap_string_lexer (quoted_string delim) state lexbuf
        >>= fun (str, loc) ->
        return (STRING (str, loc, Some delim)) }
  | "{%" (extattrident as id) "|"
      { let orig_loc = Location.curr lexbuf in
        wrap_string_lexer (quoted_string "") state lexbuf
        >>= fun (str, loc) ->
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        return (QUOTED_STRING_EXPR (id, idloc, str, loc, Some "")) }
  | "{%" (extattrident as id) blank+ (lowercase* as delim) "|"
      { let orig_loc = Location.curr lexbuf in
        wrap_string_lexer (quoted_string delim) state lexbuf
        >>= fun (str, loc) ->
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        return (QUOTED_STRING_EXPR (id, idloc, str, loc, Some delim)) }
  | "{%%" (extattrident as id) "|"
      { let orig_loc = Location.curr lexbuf in
        wrap_string_lexer (quoted_string "") state lexbuf
        >>= fun (str, loc) ->
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        return (QUOTED_STRING_ITEM (id, idloc, str, loc, Some "")) }
  | "{%%" (extattrident as id) blank+ (lowercase* as delim) "|"
      { let orig_loc = Location.curr lexbuf in
        wrap_string_lexer (quoted_string delim) state lexbuf
        >>= fun (str, loc) ->
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        return (QUOTED_STRING_ITEM (id, idloc, str, loc, Some delim)) }
  | ('#'? as maybe_hash)
    "\'" newline "\'"
    { update_loc lexbuf None 1 false 1;
      (* newline is ('\013'* '\010') *)
      return (char ~maybe_hash '\n') }
  | ('#'? as maybe_hash)
    "\'" ([^ '\\' '\'' '\010' '\013'] as c) "\'"
    { return (char ~maybe_hash c) }
  | ('#'? as maybe_hash)
    "\'\\" (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) "\'"
    { return (char ~maybe_hash (char_for_backslash c)) }
  | ('#'? as maybe_hash)
    "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
    { char_for_octal_code state lexbuf (3 + skip_hash ~maybe_hash)
      >>= fun c -> return (char ~maybe_hash c) }
  | ('#'? as maybe_hash)
    "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
    { char_for_decimal_code state lexbuf (2 + skip_hash ~maybe_hash)
      >>= fun c -> return (char ~maybe_hash c) }
  | ('#'? as maybe_hash)
    "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
    { return (char ~maybe_hash
              (char_for_hexadecimal_code lexbuf (3 + skip_hash ~maybe_hash))) }
  | '#'? "\'" ("\\" [^ '#'] as esc)
      { fail lexbuf (Illegal_escape (esc, None)) }
  | "(*"
      { let start_loc = Location.curr lexbuf in
        state.comment_start_loc <- [start_loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { start_loc with
                              Location.loc_end = end_loc.Location.loc_end }))
      }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_start;
        state.comment_start_loc <- [loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { loc with Location.loc_end = end_loc.Location.loc_end }))
      }
  | "*)" {
      let loc = Location.curr lexbuf in
      Location.prerr_warning loc Warnings.Comment_not_end;
      produce_and_backtrack lexbuf STAR 1
    }
  | "#"
      { if not (at_beginning_of_line lexbuf.lex_start_p)
        then return HASH
        else try directive state Hash lexbuf with Failure _ -> return HASH
      }
  | "#"  { return HASH }
  | "&"  { return AMPERSAND }
  | "&&" { return AMPERAMPER }
  | "`"  { return BACKQUOTE }
  | "\'" { return QUOTE }
  | "("  { return LPAREN }
  | ")"  { return RPAREN }
  | "#(" { return HASHLPAREN }
  | "#{" { return HASHLBRACE }
  | "*"  { return STAR }
  | ","  { return COMMA }
  | "->" { return MINUSGREATER }
  | "$" {
      if !(Syntax_mode.quotations) then
        return DOLLAR
      else
        return (INFIXOP0 "$")
    }
  | "."  { return DOT }
  | ".." { return DOTDOT }
  | ".#" { return DOTHASH }
  | "." (dotsymbolchar symbolchar* as op) { return (DOTOP op) }
  | ":"  { return COLON }
  | "::" { return COLONCOLON }
  | ":=" { return COLONEQUAL }
  | ":>" { return COLONGREATER }
  | ";"  { return SEMI }
  | ";;" { return SEMISEMI }
  | "<"  { return LESS }
  | "<[" {
      if !(Syntax_mode.quotations) then
        return LESSLBRACKET
      else
        (* Put back the '[' and return just LESS *)
        produce_and_backtrack lexbuf LESS 1
    }
  | "<-" { return LESSMINUS }
  | "="  { return EQUAL }
  | "["  { return LBRACKET }
  | "[|" { return LBRACKETBAR }
  | "[:" { return LBRACKETCOLON }
  | "[<" { return LBRACKETLESS }
  | "[>" { return LBRACKETGREATER }
  | "]"  { return RBRACKET }
  | "]>" {
      if !(Syntax_mode.quotations) then
        return RBRACKETGREATER
      else
        (* Put back the '>' and return just RBRACKET *)
        produce_and_backtrack lexbuf RBRACKET 1
    }
  | "{"  { return LBRACE }
  | "{<" { return LBRACELESS }
  | "|"  { return BAR }
  | "||" { return BARBAR }
  | "|]" { return BARRBRACKET }
  | ":]" { return COLONRBRACKET }
  | ">"  { return GREATER }
  | ">]" { return GREATERRBRACKET }
  | "}"  { return RBRACE }
  | ">}" { return GREATERRBRACE }
  | "[@" { return LBRACKETAT }
  | "[@@"  { return LBRACKETATAT }
  | "[@@@" { return LBRACKETATATAT }
  | "[%" { return LBRACKETPERCENT }
  | "[%%" { return LBRACKETPERCENTPERCENT }
  | "!"  { return BANG }
  | "!=" { return (INFIXOP0 "!=") }
  | "+"  { return PLUS }
  | "+." { return PLUSDOT }
  | "+=" { return PLUSEQ }
  | "-"  { return MINUS }
  | "-." { return MINUSDOT }

  | "!" symbolchar_or_hash + as op
            { return (PREFIXOP op) }
  | ['~' '?'] symbolchar_or_hash + as op
            { return (PREFIXOP op) }
  | ['=' '<' '|' '&' '$' '>'] symbolchar * as op
            { return (keyword_or state op
                       (INFIXOP0 op)) }
  | "@" { return AT }
  | "@@" { return ATAT }
  | ['@' '^'] symbolchar * as op
            { return (INFIXOP1 op) }
  | ['+' '-'] symbolchar * as op
            { return (INFIXOP2 op) }
  | "**" symbolchar * as op
            { return (INFIXOP4 op) }
  | '%'     { return PERCENT }
  | ['*' '/' '%'] symbolchar * as op
            { return (INFIXOP3 op) }
  (* Old style js_of_ocaml support is implemented by generating a custom token *)
  | '#' symbolchar_or_hash + as op
            { return (try Hashtbl.find state.keywords op
                      with Not_found -> HASHOP op) }
  | "let" kwdopchar dotsymbolchar * as op
            { return (LETOP op) }
  | "and" kwdopchar dotsymbolchar * as op
            { return (ANDOP op) }
  | eof { return EOF }

  | _ as illegal_char
      { fail lexbuf (Illegal_character illegal_char) }

(* An example of a directive is:

#4 "filename.ml"

   Here, 4 is the line number and filename.ml is the file name. The '#' must
   appear in column 0.

   The [directive] lexer is called when some portion of the start of
   the line was already consumed, either just the '#' or the '#4'. That's
   indicated by the [already_consumed] argument. The caller is responsible
   for checking that the '#' appears in column 0.
*)
and directive state already_consumed = parse
  (* Expects to receive a line number from exactly one source (either the lexbuf or
     the [already_consumed] argument, but not both) and will fail if this isn't
     the case. *)
  | ([' ' '\t']* (['0'-'9']+? as line_num_opt) [' ' '\t']*
     ("\"" ([^ '\010' '\013' '\"' ] * as name) "\"") as directive)
        [^ '\010' '\013'] *
      { let num =
          match already_consumed, line_num_opt with
          | Hash_and_line_num { line_num }, "" -> line_num
          | Hash, "" ->
              directive_error lexbuf "expected line number"
                ~already_consumed ~directive
          | Hash_and_line_num _, _ ->
              directive_error lexbuf "expected just one line number"
                ~already_consumed ~directive
          | Hash, num -> num
        in
        match int_of_string num with
        | exception _ ->
            (* PR#7165 *)
            directive_error lexbuf "line number out of range"
              ~already_consumed ~directive
        | line_num ->
           (* Documentation says that the line number should be
              positive, but we have never guarded against this and it
              might have useful hackish uses. *)
            update_loc lexbuf (Some name) (line_num - 1) true 0;
            token state lexbuf
      }
  | "syntax" [' ' '\t']+ (lowercase identchar* as mode) [' ' '\t']+
    (lowercase identchar* as toggle) [^ '\010' '\013']*
      { let toggle =
          match toggle with
          | "on" -> true
          | "off" -> false
          | _ ->
              directive_error lexbuf
                ("syntax directive can only be toggled on or off; "
                 ^ toggle ^ " not recognized")
                ~already_consumed ~directive:"syntax"
        in
        match mode with
        | "quotations" ->
            Syntax_mode.quotations := toggle;
            token state lexbuf
        | _ ->
            directive_error lexbuf ("unknown syntax mode " ^ mode)
              ~already_consumed ~directive:"syntax"
      }
and comment state = parse
    "(*"
      { state.comment_start_loc <- (Location.curr lexbuf) :: state.comment_start_loc;
      Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
      comment state lexbuf
    }
  | "*)"
      { match state.comment_start_loc with
        | [] -> assert false
        | [_] -> state.comment_start_loc <- []; return (Location.curr lexbuf)
        | _ :: l -> state.comment_start_loc <- l;
                  Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
                  comment state lexbuf
       }
  | "\""
      {
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_char state.buffer '\"';
        let buffer = state.buffer in
        state.buffer <- Buffer.create 15;
        (catch (string state lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail_loc (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail_loc e l
           )
        ) >>= fun _loc ->
      state.string_start_loc <- Location.none;
      Buffer.add_string buffer (String.escaped (Buffer.contents state.buffer));
      state.buffer <- buffer;
      Buffer.add_char state.buffer '\"';
      comment state lexbuf }
  | "{" ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
      {
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        (catch (quoted_string delim state lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail_loc (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail_loc e l
           )
        ) >>= fun _loc ->
        state.string_start_loc <- Location.none;
        Buffer.add_char state.buffer '|';
        Buffer.add_string state.buffer delim;
        Buffer.add_char state.buffer '}';
        comment state lexbuf }

  | "\'\'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'" (newline as nl) "'"
      { update_loc lexbuf None 1 false 1;
        store_string_char state.buffer '\'';
        store_normalized_newline state.buffer nl;
        store_string_char state.buffer '\'';
        comment state lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | eof
      { match state.comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev state.comment_start_loc) in
          state.comment_start_loc <- [];
          fail_loc (Unterminated_comment start) loc
      }
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        comment state lexbuf
      }
  | (lowercase | uppercase) identchar *
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | _
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }

and string state = parse
    '\"'
      { return lexbuf.lex_start_p }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string state lexbuf
      }
  | '\\' ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' ']
      { Buffer.add_char state.buffer
          (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string state lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { char_for_decimal_code state lexbuf 1 >>= fun c ->
        Buffer.add_char state.buffer c;
        string state lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { Buffer.add_char state.buffer (char_for_hexadecimal_code lexbuf 2);
        string state lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { store_escaped_uchar state lexbuf (uchar_for_uchar_escape lexbuf);
        string state lexbuf }
  | '\\' _
      { if in_comment state
        then string state lexbuf
        else begin
(*  Should be an error, but we are very lax.
                  fail (Illegal_escape (Lexing.lexeme lexbuf),
                        (Location.curr lexbuf)
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 1);
          string state lexbuf
        end
      }
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        string state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        string state lexbuf }

and quoted_string delim state = parse
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        quoted_string delim state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | "|" lowercase* "}"
      {
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim ~pos:1 ~len:(String.length edelim - 2) in
        if delim = edelim then return lexbuf.lex_start_p
        else (Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
              quoted_string delim state lexbuf)
      }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        quoted_string delim state lexbuf }

and skip_sharp_bang state = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf None 3 false 0; token state lexbuf }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf None 1 false 0; token state lexbuf }
  | "" { token state lexbuf }

{
  type comment = string * Location.t

  (* preprocessor support not implemented, not compatible with monadic
     interface *)

  let token state lexbuf =
    match Queue.take_opt deferred_tokens with
    | None -> token state lexbuf
    | Some { token; start_pos; end_pos } ->
        lexbuf.lex_start_p <- start_pos;
        lexbuf.lex_curr_p <- end_pos;
        return token

  let rec token_without_comments state lexbuf =
    token state lexbuf >>= function
    | COMMENT _ ->
      token_without_comments state lexbuf
    | tok -> return tok
}
