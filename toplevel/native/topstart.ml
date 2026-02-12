(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

<<<<<<< oxcaml:toplevel/native/topstart.ml
let _ = Stdlib.exit (Topmain.main())
||||||| upstream-base:asmcomp/asmpackager.mli
(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

val package_files
   : ppf_dump:Format.formatter
  -> Env.t
  -> string list
  -> string
  -> backend:(module Backend_intf.S)
  -> unit

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string

exception Error of error

val report_error: Format.formatter -> error -> unit
=======
(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

val package_files
   : ppf_dump:Format.formatter
  -> Env.t
  -> string list
  -> string
  -> backend:(module Backend_intf.S)
  -> unit

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
>>>>>>> upstream-incoming:asmcomp/asmpackager.mli
