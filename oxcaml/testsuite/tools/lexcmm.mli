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

<<<<<<< HEAD:oxcaml/testsuite/tools/lexcmm.mli
val token: Lexing.lexbuf -> Parsecmm.token

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error

val report_error: Lexing.lexbuf -> error -> unit
=======
(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Archiver_error of string
  | Link_error of Linkdeps.error

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
>>>>>>> upstream/5.4:asmcomp/asmlibrarian.mli
