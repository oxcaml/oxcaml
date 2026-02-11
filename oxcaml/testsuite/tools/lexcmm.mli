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
||||||| 23e84b8c4d:asmcomp/asmlibrarian.mli
(* Build libraries of .cmx files *)

open Format

val create_archive: string list -> string -> unit
=======
(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/asmlibrarian.mli

type error =
<<<<<<< HEAD:oxcaml/testsuite/tools/lexcmm.mli
    Illegal_character
  | Unterminated_comment
  | Unterminated_string
||||||| 23e84b8c4d:asmcomp/asmlibrarian.mli
    File_not_found of string
  | Archiver_error of string
=======
    File_not_found of string
  | Archiver_error of string
  | Link_error of Linkdeps.error
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/asmlibrarian.mli

exception Error of error

<<<<<<< HEAD:oxcaml/testsuite/tools/lexcmm.mli
val report_error: Lexing.lexbuf -> error -> unit
||||||| 23e84b8c4d:asmcomp/asmlibrarian.mli
val report_error: formatter -> error -> unit
=======
val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a:asmcomp/asmlibrarian.mli
