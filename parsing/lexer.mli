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

(** The lexical analyzer

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val init : ?keyword_edition:((int*int) option * string list) -> unit -> unit
val token: Lexing.lexbuf -> Parser.token
val skip_hash_bang: Lexing.lexbuf -> unit

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Empty_character_literal
  | Keyword_as_label of string
  | Capitalized_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
  | Invalid_encoding of string
  | Invalid_char_in_ident of Uchar.t
  | Non_lowercase_delimiter of string
  | Capitalized_raw_identifier of string
  | Unknown_keyword of string

exception Error of error * Location.t

val in_comment : unit -> bool
val in_string : unit -> bool

val is_keyword : string -> bool

val print_warnings : bool ref
val handle_docstrings : bool ref
val comments : unit -> (string * Location.t) list
val token_with_comments : Lexing.lexbuf -> Parser.token

(* Syntax mode configuration *)
module Syntax_mode : sig
  (* Whether quotation syntax is currently lexed: the setting of the most
     recent [#syntax quotations] directive, or [!Clflags.syntax_quotations]
     if none has been seen since the last [reset_syntax_mode]. *)
  val quotations_enabled : unit -> bool
end

(* Forget any [#syntax] directive seen so far, so that lexing continues with
   the invocation defaults. Called at the start of each source file. *)
val reset_syntax_mode : unit -> unit

(* [protect_syntax_mode f] runs [f], then restores the [#syntax] directive
   state to what it was beforehand. The toplevel uses this for [#use]d files:
   they inherit the session's current mode, and their own directives stay
   local to them instead of leaking into the session. *)
val protect_syntax_mode : (unit -> 'a) -> 'a

(*
  [set_preprocessor init preprocessor] registers [init] as the function
to call to initialize the preprocessor when the lexer is initialized,
and [preprocessor] a function that is called when a new token is needed
by the parser, as [preprocessor lexer lexbuf] where [lexer] is the
lexing function.

When a preprocessor is configured by calling [set_preprocessor], the lexer
changes its behavior to accept backslash-newline as a token-separating blank.
*)

val set_preprocessor :
  (unit -> unit) ->
  ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token) ->
  unit
