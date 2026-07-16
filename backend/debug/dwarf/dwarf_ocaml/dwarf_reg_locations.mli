(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Construction of DWARF location descriptions for registers. *)

open! Dwarf_high
open! Dwarf_low

val reg_location_description :
  Reg.t ->
  offset:Stack_reg_offset.t option ->
  need_rvalue:bool ->
  Simple_location_description.t

(** An rvalue whose value is the address of the given symbol, referencing the
    symbol in the manner required by its visibility. (In particular, local
    symbols are defined as assembler-temporary labels, not linker symbols, and
    must be referenced as such.) *)
val address_of_cmm_symbol_rvalue :
  Cmm.symbol ->
  Simple_location_description_lang.normal
  Simple_location_description_lang.rvalue
