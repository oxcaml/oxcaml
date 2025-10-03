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

val link: (module Compiler_owee.Unix_intf.S)
    -> string list
    -> string
    -> cached_genfns_imports:Generic_fns.Partition.Set.t
    -> genfns:Generic_fns.Tbl.t
    -> units_tolink:Linkenv.unit_link_info list
    -> ppf_dump:Format.formatter
    -> unit

val link_shared: (module Compiler_owee.Unix_intf.S)
    -> string list -> string
    -> genfns:Generic_fns.Tbl.t
    -> units_tolink:Linkenv.unit_link_info list
    -> ppf_dump:Format.formatter
    -> unit


val call_linker_shared: ?native_toplevel:bool -> string list -> string -> unit
