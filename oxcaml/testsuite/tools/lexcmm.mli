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

val token: Lexing.lexbuf -> Parsecmm.token

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error

<<<<<<< HEAD:oxcaml/testsuite/tools/lexcmm.mli
val report_error: Lexing.lexbuf -> error -> unit
||||||| parent of fb010ad9da (Format_doc: preserve the type of Foo.report_error, add Foo.report_error_doc (#13311)):asmcomp/asmlibrarian.mli
val report_error: error Format_doc.printer
=======
val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
>>>>>>> fb010ad9da (Format_doc: preserve the type of Foo.report_error, add Foo.report_error_doc (#13311)):asmcomp/asmlibrarian.mli
