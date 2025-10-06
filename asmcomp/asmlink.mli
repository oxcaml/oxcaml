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
  ppf_dump:formatter ->
  string list ->
  string ->
  unit

val link_shared :
  (module Compiler_owee.Unix_intf.S) ->
  ppf_dump:formatter ->
  string list ->
  string ->
  unit

val call_linker_shared : ?native_toplevel:bool -> string list -> string -> unit

val reset : unit -> unit

val check_consistency : filepath -> Cmx_format.unit_infos -> Digest.t -> unit

val extract_crc_interfaces : unit -> Import_info.t list

val extract_crc_implementations : unit -> Import_info.t list

type error =
  | Assembler_error of filepath
  | Dwarf_fission_objcopy_on_macos
  | Dwarf_fission_dsymutil_not_macos
  | Dsymutil_error of int
  | Objcopy_error of int

exception Error of error

val report_error : formatter -> error -> unit
