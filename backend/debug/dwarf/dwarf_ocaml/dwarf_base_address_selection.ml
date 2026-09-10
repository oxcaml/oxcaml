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

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

let start_of_code_symbol_and_base_entries state ~function_symbol
    ~create_base_address_selection_entry =
  match !Dwarf_flags.gdwarf_version with
  | Five ->
    (* The offsets in DWARF-5 location and range list entries are relative to
       the function symbol, which is established as the base address of each
       list. *)
    function_symbol, []
  | Four -> (
    match Dwarf_state.code_layout state with
    | Function_sections ->
      ( function_symbol,
        [ create_base_address_selection_entry
            ~base_address_symbol:function_symbol ] )
    | Continuous_code_section { code_begin; _ } -> code_begin, [])
