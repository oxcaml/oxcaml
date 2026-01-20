
{
open Flambda_parser

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid literal %s" s

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let error ~lexbuf e = raise (Error (e, current_location lexbuf))

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "always", KWD_ALWAYS;
    "and", KWD_AND;
    "andwhere", KWD_ANDWHERE;
    "any", KWD_ANY;
    "apply", KWD_APPLY;
    "array", KWD_ARRAY;
    "available", KWD_AVAILABLE;
    "boxed", KWD_BOXED;
    "ccall", KWD_CCALL;
    "closure", KWD_CLOSURE;
    "code", KWD_CODE;
    "cont", KWD_CONT;
    "default", KWD_DEFAULT;
    "define_root_symbol", KWD_DEFINE_ROOT_SYMBOL;
    "deleted", KWD_DELETED;
    "depth", KWD_DEPTH;
    "direct", KWD_DIRECT;
    "do_not_inline", KWD_DO_NOT_INLINE;
    "done", KWD_DONE;
    "end", KWD_END;
    "error", KWD_ERROR;
    "exn", KWD_EXN;
    "float", KWD_FLOAT;
    "float32", KWD_FLOAT32;
    "halt_and_catch_fire", KWD_HCF;
    "hint", KWD_HINT;
    "id", KWD_ID;
    "imm", KWD_IMM;
    "immutable_unique", KWD_IMMUTABLE_UNIQUE;
    "in", KWD_IN;
    "inf", KWD_INF;
    "inline", KWD_INLINE;
    "inlined", KWD_INLINED;
    "inlining_state", KWD_INLINING_STATE;
    "int32", KWD_INT32;
    "int64", KWD_INT64;
    "invalid", KWD_INVALID;
    "let", KWD_LET;
    "local", KWD_LOCAL;
    "loopify", KWD_LOOPIFY;
    "mutable", KWD_MUTABLE;
    "nativeint", KWD_NATIVEINT;
    "never", KWD_NEVER;
    "newer_version_of", KWD_NEWER_VERSION_OF;
    "noalloc", KWD_NOALLOC;
    "notrace", KWD_NOTRACE;
    "null", KWD_NULL;
    "of", KWD_OF;
    "pop", KWD_POP;
    "push", KWD_PUSH;
    "rec", KWD_REC;
    "rec_info", KWD_REC_INFO;
    "region", KWD_REGION;
    "regular", KWD_REGULAR;
    "reraise", KWD_RERAISE;
    "set_of_closures", KWD_SET_OF_CLOSURES;
    "size", KWD_SIZE;
    "succ", KWD_SUCC;
    "switch", KWD_SWITCH;
    "tagged", KWD_TAGGED;
    "tailrec", KWD_TAILREC;
    "toplevel", KWD_TOPLEVEL;
    "tupled", KWD_TUPLED;
    "unit", KWD_UNIT;
    "unreachable", KWD_UNREACHABLE;
    "unroll", KWD_UNROLL;
    "val", KWD_VAL;
    "where", KWD_WHERE;
    "with", KWD_WITH;

    (* Constructors for static constants *)
    "Block", STATIC_CONST_BLOCK;
    "Value_array", STATIC_CONST_VALUE_ARRAY;
    "Float_array", STATIC_CONST_FLOAT_ARRAY;
    "Float_block", STATIC_CONST_FLOAT_BLOCK;
    "Empty_array", STATIC_CONST_EMPTY_ARRAY;
]

let ident_or_keyword str =
  try Hashtbl.find keyword_table str
  with Not_found -> IDENT str

let is_keyword str =
  Hashtbl.mem keyword_table str

let unquote_ident str =
  match str with
  | "" -> ""
  | _ ->
    begin
      match String.get str 0 with
      | '`' -> String.sub str 1 (String.length str - 2)
      | _ -> str
    end

let symbol cunit_ident cunit_linkage_name ident =
  let cunit =
    Option.map (fun cunit_ident ->
      { Fexpr.ident = unquote_ident cunit_ident;
        linkage_name = Option.map unquote_ident cunit_linkage_name }
    ) cunit_ident
  in
  SYMBOL (cunit, unquote_ident ident)

}

let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let quoted_ident = '`' [^ '`' '\n']* '`'
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
let sign = ['-']
let int_literal =
  sign? (decimal_literal | hex_literal | oct_literal | bin_literal)
let dec_float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let float_literal = sign? (dec_float_literal | hex_float_literal)
let num_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | "\n"
      { Lexing.new_line lexbuf; token lexbuf }
  | blank +
      { token lexbuf }
  | "(*"
      { comment 1 lexbuf;
        token lexbuf }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "_"
      { BLANK }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACK }
  | "]"
      { RBRACK }
  | "[|"
      { LBRACKPIPE }
  | "|]"
      { RBRACKPIPE }
  | "*"  { STAR }
  | "->" { MINUSGREATER }
  | "@" { AT }
  | "|"  { PIPE }
  | "~"  { TILDE }
  | "&"  { AMP }
  | "^"  { CARET }
  | "===>" { BIGARROW }
  | identstart identchar* as ident
         { ident_or_keyword ident }
  | quoted_ident as ident
         { IDENT (unquote_ident ident) }
  | '$'
    (((identchar* | quoted_ident) as cunit_ident)
     ('/' ((identchar* | quoted_ident) as cunit_linkage_name))?
     '.')?
    ((identchar+ | quoted_ident) as ident)
         { symbol cunit_ident cunit_linkage_name ident }
  | '%' identchar+ as p
         { PRIM p }
  | (int_literal as lit) (num_modifier as modif)?
         { INT (lit, modif) }
  | (float_literal as lit) (num_modifier as modif)?
         { FLOAT (lit |> Float.of_string, modif) }
  | (float_literal | int_literal) identchar+ as lit
         { error ~lexbuf (Invalid_literal lit) }
  | '"' (([^ '"'] | '\\' '"')* as s) '"'
         (* CR-someday lmaurer: Escape sequences, multiline strings *)
         { STRING (Scanf.unescaped s) }
  | eof  { EOF }
  | _ as ch
         { error ~lexbuf (Illegal_character ch) }

and comment n = parse
  | "\n"
         { Lexing.new_line lexbuf; comment n lexbuf }
  | "*)"
         { if n = 1 then ()
           else comment (n-1) lexbuf }
  | "(*"
         { comment (n+1) lexbuf }
  | _
         { comment n lexbuf }
