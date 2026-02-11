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

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Misc
open Format

val link :
  (module Compiler_owee.Unix_intf.S) ->
  Linkenv.t ->
  string list ->
  string ->
  cached_genfns_imports:Generic_fns.Partition.Set.t ->
  genfns:Generic_fns.Tbl.t ->
  units_tolink:Linkenv.unit_link_info list ->
  uses_eval:bool ->
  quoted_globals:Compilation_unit.Name.Set.t ->
  ppf_dump:Format.formatter ->
  unit

val link_shared :
  (module Compiler_owee.Unix_intf.S) ->
  string list ->
  string ->
  genfns:Generic_fns.Tbl.t ->
  units_tolink:Linkenv.unit_link_info list ->
  ppf_dump:Format.formatter ->
  unit

<<<<<<< HEAD
val call_linker_shared : ?native_toplevel:bool -> string list -> string -> unit
||||||| 23e84b8c4d
val call_linker_shared: string list -> string -> unit

val reset : unit -> unit
val check_consistency: filepath -> Cmx_format.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> crcs
val extract_crc_implementations: unit -> crcs

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname

exception Error of error

val report_error: formatter -> error -> unit
=======
val call_linker_shared: string list -> string -> unit

val reset : unit -> unit
val check_consistency: filepath -> Cmx_format.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> crcs
val extract_crc_implementations: unit -> crcs

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Missing_cmx of filepath * modname
  | Link_error of Linkdeps.error

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
