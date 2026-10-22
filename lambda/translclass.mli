(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda
open Debuginfo.Scoped_location

(** Whether a class definition refers to a class of its recursive group. *)
type class_kind =
  | Recursive_class
  | Non_recursive_class

val transl_class :
  scopes:scopes -> Ident.t list -> Ident.t ->
  string list -> class_expr -> Asttypes.virtual_flag ->
  lambda * class_kind

(** Compile a recursive group of class definitions, as returned by
    [transl_class], in front of a body. *)
val compile_class_bindings :
  (Ident.t * debug_uid * class_kind * lambda) list -> lambda -> lambda

type error = Tags of string * string

exception Error of Location.t * error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
