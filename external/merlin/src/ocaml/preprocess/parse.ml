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

let token lexbuf =
  let state = Lexer_raw.make @@ Lexer_raw.keywords [] in
  let rec lexer = function
    | Lexer_raw.Fail (e, l) -> raise (Lexer_raw.Error (e, l))
    | Lexer_raw.Return token -> token
    | Lexer_raw.Refill k -> lexer (k ())
  in
  lexer (Lexer_raw.token_without_comments state lexbuf)

let simple_module_path lexbuf = Parser_raw.parse_mod_longident token lexbuf
