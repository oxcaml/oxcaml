(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_low
module A = Asm_directives

let emit0_delayed ~asm_directives:_ = ()

type type_unit =
  { root : Proto_die.t;
    header_label : Asm_label.t;
    signature : Int64.t;
    primary : Proto_die.reference
  }

let emit0 ~asm_directives ~compilation_unit_proto_die
    ~compilation_unit_header_label ~(type_units : type_unit list)
    ~debug_loc_table ~debug_ranges_table ~address_table ~location_list_table =
  (* CR-soon mshinwell: the [compilation_unit_die] member of the record returned
     from [Assign_abbrevs.run] is now unused *)
  let assigned_abbrevs =
    Profile.record "assign_abbrevs"
      (fun () -> Assign_abbrevs.run ~proto_die_root:compilation_unit_proto_die)
      ()
  in
  List.iter
    (fun location_list -> Debug_loc_table.insert debug_loc_table location_list)
    assigned_abbrevs.dwarf_4_location_lists;
  let debug_abbrev_label = Asm_label.for_dwarf_section Debug_abbrev in
  let debug_info =
    Profile.record "debug_info_section"
      (fun () ->
        Debug_info_section.create ~dies:assigned_abbrevs.dies
          ~debug_abbrev_label ~compilation_unit_header_label)
      ()
  in
  (* Each type unit gets its own abbreviations table, emitted after the
     compilation unit's table in .debug_abbrev. *)
  let type_units =
    List.map
      (fun { root; header_label; signature; primary } ->
        let assigned = Assign_abbrevs.run ~proto_die_root:root in
        (match assigned.dwarf_4_location_lists with
        | [] -> ()
        | _ :: _ ->
          Misc.fatal_error
            "Location lists must not occur within DWARF type units");
        let debug_abbrev_label = Asm_label.create (DWARF Debug_abbrev) in
        let type_unit =
          Debug_types_section.create_type_unit ~header_label ~signature
            ~primary_die_label:primary ~dies:assigned.dies ~debug_abbrev_label
        in
        type_unit, assigned.abbrev_table, debug_abbrev_label)
      type_units
  in
  Profile.record "dwarf_world_emit"
    (fun () ->
      A.switch_to_section (DWARF Debug_info);
      Profile.record "debug_info_section"
        (Debug_info_section.emit ~asm_directives)
        debug_info;
      (match type_units with
      | [] -> ()
      | _ :: _ ->
        A.switch_to_section (DWARF Debug_types);
        Profile.record "debug_types_section"
          (Debug_types_section.emit ~asm_directives)
          (Debug_types_section.create
             ~type_units:
               (List.map (fun (type_unit, _, _) -> type_unit) type_units)));
      A.switch_to_section (DWARF Debug_abbrev);
      Profile.record "abbreviations_table"
        (Abbreviations_table.emit ~asm_directives)
        assigned_abbrevs.abbrev_table;
      List.iter
        (fun (_, abbrev_table, debug_abbrev_label) ->
          A.define_label debug_abbrev_label;
          Abbreviations_table.emit ~asm_directives abbrev_table)
        type_units;
      A.switch_to_section (DWARF Debug_str);
      A.emit_cached_strings ();
      match !Dwarf_flags.gdwarf_version with
      | Four ->
        A.switch_to_section (DWARF Debug_loc);
        Profile.record "debug_loc"
          (Debug_loc_table.emit ~asm_directives)
          debug_loc_table;
        A.switch_to_section (DWARF Debug_ranges);
        Profile.record "debug_ranges"
          (Debug_ranges_table.emit ~asm_directives)
          debug_ranges_table
      | Five ->
        Profile.record "addr_table"
          (Address_table.emit ~asm_directives)
          address_table;
        A.switch_to_section (DWARF Debug_loclists);
        Profile.record "loclists_table"
          (Location_list_table.emit ~asm_directives)
          location_list_table)
    ()

let emit ~asm_directives ~compilation_unit_proto_die
    ~compilation_unit_header_label ~type_units ~debug_loc_table
    ~debug_ranges_table ~address_table ~location_list_table
    ~binary_backend_available =
  if
    (* CR mshinwell: support the internal assembler *)
    binary_backend_available
  then ()
  else
    emit0 ~asm_directives ~compilation_unit_proto_die
      ~compilation_unit_header_label ~type_units ~debug_loc_table
      ~debug_ranges_table ~address_table ~location_list_table

let emit_delayed ~asm_directives ~binary_backend_available =
  if
    (* CR mshinwell: support the internal assembler *)
    binary_backend_available
  then ()
  else emit0_delayed ~asm_directives
