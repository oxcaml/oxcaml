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

open Misc

(* Link .cmo files and produce a bytecode executable. *)

module Dep : Set.OrderedType with
  type t = Compilation_unit.t * Compilation_unit.t
module DepSet : Set.S with type elt = Dep.t

val link : filepath list -> filepath -> unit
val reset : unit -> unit

<<<<<<< HEAD
val check_consistency: filepath -> Cmo_format.compilation_unit_descr -> unit
||||||| 23e84b8c4d
val check_consistency: filepath -> Cmo_format.compilation_unit -> unit
=======
val check_consistency: filepath -> Cmo_format.compilation_unit -> unit
val linkdeps_unit :
  Linkdeps.t -> filename:string -> Cmo_format.compilation_unit -> unit
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

val extract_crc_interfaces: unit -> Import_info.t list

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of Compilation_unit.Name.t * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
<<<<<<< HEAD
  | Required_compunit_unavailable of Compilation_unit.t * Compilation_unit.t
||||||| 23e84b8c4d
  | Required_compunit_unavailable of (Cmo_format.compunit * Cmo_format.compunit)
=======
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  | Camlheader of string * filepath
<<<<<<< HEAD
  | Wrong_link_order of DepSet.t
  | Multiple_definition of Compilation_unit.t * filepath * filepath
||||||| 23e84b8c4d
  | Wrong_link_order of DepSet.t
  | Multiple_definition of Cmo_format.compunit * filepath * filepath
=======
  | Link_error of Linkdeps.error
  | Needs_custom_runtime of filepath
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
