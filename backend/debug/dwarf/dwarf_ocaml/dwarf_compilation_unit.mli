(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Construction of DWARF "compile_unit" DIEs and associated entities. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Dwarf_high
open Dwarf_low

val compile_unit_proto_die_without_code_ranges :
  sourcefile:string -> unit_name:Ident.t -> Proto_die.t

val add_code_ranges_to_compile_unit_proto_die :
  Proto_die.t ->
  code_layout:Dwarf_state.code_layout ->
  ranges:Dwarf_state.function_range list ->
  debug_ranges_table:Debug_ranges_table.t ->
  unit
