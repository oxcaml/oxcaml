(** Parsers in this file produce only [Ok ()] or [Error (_ : Syntax_error.t)]. *)

module Syntax_error : sig
  type t =
    { msg : string
    ; loc : Location.t
    }
end

(** Entry points in the OCaml grammar. *)

val use_file: Lexing.lexbuf -> (unit, Syntax_error.t) result
val toplevel_phrase: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_pattern: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_module_type: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_module_expr: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_expression: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_core_type: Lexing.lexbuf -> (unit, Syntax_error.t) result
val interface: Lexing.lexbuf -> (unit, Syntax_error.t) result
val implementation: Lexing.lexbuf -> (unit, Syntax_error.t) result

(** Longident parsers for various contexts. *)

val parse_any_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_constr_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_mod_ext_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_mod_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_mty_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result
val parse_val_longident: Lexing.lexbuf -> (unit, Syntax_error.t) result

(** Based on the file extension, parse a [.ml], [.mli], or [.mlt] file. *)
val parse_file : path:string -> Lexing.lexbuf -> (unit, Syntax_error.t) result
