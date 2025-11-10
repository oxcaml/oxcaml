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

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_high
open Dwarf_low
module DAH = Dwarf_attribute_helpers

let compile_unit_proto_die_without_code_ranges ~sourcefile ~unit_name =
  let source_filename = Filename.basename sourcefile in
  let comp_dir =
    let comp_dir = Sys.getcwd () in
    match Misc.get_build_path_prefix_map () with
    | None -> comp_dir
    | Some map -> Build_path_prefix_map.rewrite map comp_dir
  in
  let attribute_values =
    [ DAH.create_name source_filename;
      DAH.create_comp_dir comp_dir;
      (* The [OCaml] attribute value here is only defined in DWARF-5, but it
         doesn't mean anything else in DWARF-4, so we always emit it. This saves
         special-case logic in gdb based on the producer name. *)
      DAH.create_language OCaml;
      DAH.create_producer "ocamlopt";
      DAH.create_ocaml_unit_name unit_name;
      DAH.create_ocaml_compiler_version Sys.ocaml_version;
      DAH.create_stmt_list
        ~debug_line_label:(Asm_label.for_dwarf_section Asm_section.Debug_line)
    ]
  in
  Proto_die.create ~parent:None ~tag:Compile_unit ~attribute_values ()

let code_ranges_attributes ~code_layout
    ~(ranges : Dwarf_state.function_range list) ~debug_ranges_table =
  (* Use DW_AT_ranges for non-contiguous ranges. For function sections, each
     function is in a different section, so we need base address selection
     entries to switch the base. For contiguous code, we can use a simpler
     approach. *)
  match code_layout with
  | Dwarf_state.Continuous_code_section { code_begin; code_end } ->
    [ DAH.create_low_pc_from_symbol code_begin;
      DAH.create_high_pc_from_symbol ~low_pc:code_begin code_end ]
  | Dwarf_state.Function_sections ->
    let range_list_entries =
      List.concat_map
        (fun (range : Dwarf_state.function_range) ->
          [ Dwarf_4_range_list_entry.create_base_address_selection_entry
              ~base_address_symbol:range.function_symbol;
            Dwarf_4_range_list_entry.create_range_list_entry
              ~start_of_code_symbol:range.function_symbol
              ~first_address_when_in_scope:range.start_label
              ~first_address_when_not_in_scope:range.end_label
              ~first_address_when_not_in_scope_offset:
                range.offset_past_end_label ])
        ranges
    in
    let range_list = Dwarf_4_range_list.create ~range_list_entries in
    let ranges_attr =
      Debug_ranges_table.insert debug_ranges_table ~range_list
    in
    [ranges_attr]

let add_code_ranges_to_compile_unit_proto_die die ~code_layout ~ranges
    ~debug_ranges_table =
  let address_attributes =
    code_ranges_attributes ~code_layout ~ranges ~debug_ranges_table
  in
  List.iter
    (fun attr -> Proto_die.add_or_replace_attribute_value die attr)
    address_attributes
