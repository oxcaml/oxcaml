(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Selection of the symbol from which the offsets in location and range list
    entries are computed, together with any DWARF-4 base address selection entry
    that must precede such entries. *)

open Asm_targets

val start_of_code_symbol_and_base_entries :
  Dwarf_state.t ->
  function_symbol:Asm_symbol.t ->
  create_base_address_selection_entry:(base_address_symbol:Asm_symbol.t -> 'a) ->
  Asm_symbol.t * 'a list
