(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let last_token = ref Parser.EOF

let token lexbuf =
  let token = Lexer.token lexbuf in
  last_token := token;
  token

let rec skip_phrase lexbuf =
  match token lexbuf with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf
  | exception (Lexer.Error (Lexer.Unterminated_comment _, _)
              | Lexer.Error (Lexer.Unterminated_string, _)
              | Lexer.Error (Lexer.Reserved_sequence _, _)
              | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
              | Lexer.Error (Lexer.Illegal_character _, _)) ->
      skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  match !last_token with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf

type 'a parser =
  (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

let wrap (parser : 'a parser) lexbuf : 'a =
  try
    Docstrings.init ();
    Lexer.init ();
    let ast = parser token lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    last_token := Parser.EOF;
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

(* We pass [--strategy simplified] to Menhir, which means that we wish to use
   its "simplified" strategy for handling errors. When a syntax error occurs,
   the current token is replaced with an [error] token. The parser then
   continues shifting and reducing, as far as possible. After (possibly)
   shifting the [error] token, though, the parser remains in error-handling
   mode, and does not request the next token, so the current token remains
   [error].

   In OCaml's grammar, the [error] token always appears at the end of a
   production, and this production always raises an exception. In such
   a situation, the strategy described above means that:

   - either the parser will not be able to shift [error],
     and will raise [Parser.Error];

   - or it will be able to shift [error] and will then reduce
     a production whose semantic action raises an exception.

   In either case, the parser will not attempt to read one token past
   the syntax error. *)

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
and core_type = wrap Parser.parse_core_type
and expression = wrap Parser.parse_expression
and pattern = wrap Parser.parse_pattern
let module_type = wrap Parser.parse_module_type
let module_expr = wrap Parser.parse_module_expr

let longident = wrap Parser.parse_any_longident
let val_ident = wrap Parser.parse_val_longident
let constr_ident= wrap Parser.parse_constr_longident
let extended_module_path = wrap Parser.parse_mod_ext_longident
let simple_module_path = wrap Parser.parse_mod_longident
let type_ident = wrap Parser.parse_mty_longident

(* Error reporting for Syntaxerr *)
(* The code has been moved here so that one can reuse Pprintast.tyvar *)

module Style = Misc.Style

let prepare_error err =
  let open Syntaxerr in
  match err with
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf
        ~loc:closing_loc
        ~sub:[
          Location.msg ~loc:opening_loc
            "This %a might be unmatched" Style.inline_code opening
        ]
        "Syntax error: %a expected" Style.inline_code closing

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %a expected."
        Style.inline_code nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %a not expected."
        Style.inline_code nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form %a \
         are not supported when the option %a is set."
        Style.inline_code "F(X).t"
        Style.inline_code "-no-app-func"
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable %a \
         is reserved for the local type %a."
        (Style.as_inline_code Pprintast.tyvar) var
        Style.inline_code var
  | Other loc ->
      Location.errorf ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc
        "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, ipt) ->
      let invalid ppf ipt = match ipt with
        | Syntaxerr.Parameterized_types ->
            Format.fprintf ppf "parametrized types are not supported"
        | Constrained_types ->
            Format.fprintf ppf "constrained types are not supported"
        | Private_types ->
            Format.fprintf ppf  "private types are not supported"
        | Not_with_type ->
            Format.fprintf ppf "only %a constraints are supported"
              Style.inline_code "with type t ="
        | Neither_identifier_nor_with_type ->
            Format.fprintf ppf
              "only module type identifier and %a constraints are supported"
              Style.inline_code "with type"
        | Misplaced_attribute ->
            Format.fprintf ppf "an attribute cannot go here"
      in
      Location.errorf ~loc "invalid package type: %a" invalid ipt
  | Removed_string_set loc ->
      Location.errorf ~loc
        "Syntax error: strings are immutable, there is no assignment \
         syntax for them.\n\
         @{<hint>Hint@}: Mutable sequences of bytes are available in \
         the Bytes module.\n\
         @{<hint>Hint@}: Did you mean to use %a?"
        Style.inline_code "Bytes.set"
  | Missing_unboxed_literal_suffix loc ->
      Location.errorf ~loc
        "Syntax error: Unboxed integer literals require width suffixes."
  | Malformed_instance_identifier loc ->
      Location.errorf ~loc
        "Syntax error: Unexpected in module instance"
  | Quotation_reserved (loc, symb) ->
      Location.errorf ~loc
        "Syntax error: `%s` is reserved for use in runtime metaprogramming."
        symb
  | Let_mutable_not_allowed_at_structure_level loc ->
      Location.errorf ~loc
        "Syntax error: Mutable let bindings are not allowed \
         at the structure level."
  | Let_mutable_not_allowed_in_class_definition loc ->
      Location.errorf ~loc
        "Syntax error: Mutable let bindings are not allowed \
         inside class definitions."
  | Let_mutable_not_allowed_with_function_bindings loc ->
      Location.errorf ~loc
        "Syntax error: Mutable let is not allowed with function bindings.\n\
         @{<hint>Hint@}: If you really want a mutable function variable, \
         use the de-sugared syntax:\n  %a"
         Style.inline_code "let mutable f = fun x -> .."

let () =
  Location.register_error_of_exn
    (function
      | Syntaxerr.Error err -> Some (prepare_error err)
      | _ -> None
    )
