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
  Linkenv.objfile_to_link list ->
  string ->
  cached_genfns_imports:Generic_fns.Partition.Set.t ->
  genfns:Generic_fns.Tbl.t ->
  units_tolink:Linkenv.unit_link_info list ->
  uses_eval:bool ->
  quoted_cmi:Compilation_unit.Name.Set.t ->
  quoted_cmx:Compilation_unit.Set.t ->
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

val call_linker_shared : ?native_toplevel:bool -> string list -> string -> unit

(** The archive of cached generic functions that a link with
    [-use-cached-generic-functions] uses, whether or not the file exists: the
    one that [-cached-generic-functions-path] names, redirected like any other
    path inside the library root ([Codegen_variant.in_selected_root]), or else
    the archive of the library tree that serves the selected codegen variant.
    The linker resolves the archive with this function, so this is the file for
    a build to depend on. *)
val cached_generic_functions_archive : unit -> string
