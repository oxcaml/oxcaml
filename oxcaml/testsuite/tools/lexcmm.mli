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
||||||| parent of 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages):asmcomp/asmlibrarian.mli
(* Build libraries of .cmx files *)

open Format

val create_archive: string list -> string -> unit
=======
(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit
>>>>>>> 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages):asmcomp/asmlibrarian.mli

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error

<<<<<<< HEAD:oxcaml/testsuite/tools/lexcmm.mli
val report_error: Lexing.lexbuf -> error -> unit
||||||| parent of 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages):asmcomp/asmlibrarian.mli
val report_error: formatter -> error -> unit
=======
val report_error: error Format_doc.printer
>>>>>>> 1b09b92c85 (Merge pull request #13169 from Octachron/format_doc_for_error_messages):asmcomp/asmlibrarian.mli
