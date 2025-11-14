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

(* CR mshinwell: fix properly using -enable-dev PR's changes *)
[@@@ocaml.warning "-27-32"]

open! Int_replace_polymorphic_compare
open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module ARAV = Available_ranges_all_vars
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear
module SLDL = Simple_location_description_lang
module V = Backend_var

type proto_dies_for_var =
  { value_die_lvalue : Proto_die.reference;
    type_die : Proto_die.reference
  }

let arch_size_addr = Targetint.of_int_exn Arch.size_addr

let proto_dies_for_variable var ~proto_dies_for_vars =
  match Backend_var.Tbl.find proto_dies_for_vars var with
  | exception Not_found -> None
  | result -> Some result

let normal_type_for_var ?reference ~parent ident_for_type is_parameter =
  let name_attribute =
    match ident_for_type with
    | None -> []
    | Some (compilation_unit, var) ->
      let name =
        Dwarf_name_laundry.base_type_die_name_for_var compilation_unit var
          is_parameter
      in
      [DAH.create_name name]
  in
  (* CR mshinwell: This should not create duplicates when the name is missing *)
  Proto_die.create ?reference ~parent ~tag:Base_type
    ~attribute_values:
      (name_attribute
      @ [ DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr ])
    ()

let type_die_reference_for_var var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some dies -> Some dies.type_die

let construct_type_of_value_description state ~parent ident_for_type
    is_parameter ~proto_dies_for_vars ~reference =
  normal_type_for_var ~reference ~parent ident_for_type is_parameter

type location_description =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let _to_silence_warning x = Composite x

let reg_location_description reg ~offset ~need_rvalue :
    location_description option =
  match
    Dwarf_reg_locations.reg_location_description reg ~offset ~need_rvalue
  with
  | None -> None
  | Some simple_loc_desc -> Some (Simple simple_loc_desc)

(* Helper functions for phantom variable DIE references *)
let die_location_of_variable_lvalue state var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { value_die_lvalue; _ } ->
    let location =
      SLDL.Lvalue.location_from_another_die ~die_label:value_die_lvalue
        ~compilation_unit_header_label:(DS.compilation_unit_header_label state)
    in
    Some location

let die_location_of_variable_rvalue state var ~proto_dies_for_vars =
  match proto_dies_for_variable var ~proto_dies_for_vars with
  | None -> None
  | Some { value_die_lvalue; _ } ->
    (* For rvalues, we reference the same DIE but the debugger knows to evaluate
       it as an rvalue *)
    let location =
      SLDL.Rvalue.location_from_another_die ~die_label:value_die_lvalue
        ~compilation_unit_header_label:(DS.compilation_unit_header_label state)
    in
    Some location

(* Phantom variables are always immutable, so we emit lvalue descriptions for
   consistency with normal variables, and emit rvalue descriptions only when
   required. *)
let rec phantom_var_location_description state
    ~(defining_expr : Linear.phantom_defining_expr) ~need_rvalue
    ~proto_dies_for_vars ~parent : location_description option =
  let module SLD = Simple_location_description in
  let lvalue lvalue = Some (Simple (SLDL.compile (SLDL.of_lvalue lvalue))) in
  let lvalue_without_address lvalue =
    Some (Simple (SLDL.compile (SLDL.of_lvalue_without_address lvalue)))
  in
  let rvalue rvalue = Some (Simple (SLDL.compile (SLDL.of_rvalue rvalue))) in
  match defining_expr with
  | Lphantom_const_int i ->
    let i = SLDL.Rvalue.signed_int_const i in
    if need_rvalue
    then rvalue i
    else lvalue_without_address (SLDL.Lvalue_without_address.of_rvalue i)
  | Lphantom_const_symbol symbol ->
    let symbol = SLDL.Rvalue.const_symbol (Asm_symbol.create symbol) in
    if need_rvalue
    then rvalue symbol
    else lvalue_without_address (SLDL.Lvalue_without_address.of_rvalue symbol)
  | Lphantom_read_symbol_field { sym; field } ->
    let symbol = Asm_symbol.create sym in
    let field = Targetint.of_int field in
    if need_rvalue
    then rvalue (SLDL.Rvalue.read_symbol_field symbol ~field)
    else lvalue (SLDL.Lvalue.in_symbol_field symbol ~field)
  | Lphantom_var var -> (
    if (* Variables referenced by phantom lets may themselves become unavailable
          at certain program points. We wrap the location description in a
          composite location description (with only one piece) so GDB correctly
          detects unavailability rather than producing a stack underflow
          error. *)
       need_rvalue
    then
      match die_location_of_variable_rvalue state var ~proto_dies_for_vars with
      | None -> None
      | Some rvalue ->
        let location = SLDL.compile (SLDL.of_rvalue rvalue) in
        let composite =
          Composite_location_description.pieces_of_simple_location_descriptions
            [location, arch_size_addr]
        in
        Some (Composite composite)
    else
      match die_location_of_variable_lvalue state var ~proto_dies_for_vars with
      | None -> None
      | Some lvalue ->
        let location = SLDL.compile (SLDL.of_lvalue lvalue) in
        let composite =
          Composite_location_description.pieces_of_simple_location_descriptions
            [location, arch_size_addr]
        in
        Some (Composite composite))
  | Lphantom_read_field { var; field } ->
    (* For now, show field access as unavailable since we cannot dereference
       values built with implicit pointers. *)
    None
  | Lphantom_offset_var { var; offset_in_words } -> (
    match die_location_of_variable_lvalue state var ~proto_dies_for_vars with
    | None -> None
    | Some location ->
      let offset_in_words = Targetint.of_int_exn offset_in_words in
      if need_rvalue
      then None
      else lvalue (SLDL.Lvalue.offset_pointer location ~offset_in_words))
  | Lphantom_block { tag; fields } ->
    (* A phantom block construction: instead of the block existing in the target
       program's address space, it is going to be conjured up in the debugger's
       address space using DWARF instructions. References between such blocks
       use "implicit pointers" rather than normal pointers in the target's
       address space. *)
    let header =
      (* Create a proper OCaml block header with the tag and field count *)
      let header_value =
        Cmm_helpers.black_block_header tag (List.length fields)
      in
      SLDL.compile
        (SLDL.of_rvalue
           (SLDL.Rvalue.signed_int_const
              (Targetint.of_int64 (Int64.of_nativeint header_value))))
    in
    let header_size = arch_size_addr in
    let field_size = arch_size_addr in
    (* Process each field - get its rvalue location *)
    let field_pieces =
      List.map
        (fun field_var ->
          let simple_location_description =
            match
              die_location_of_variable_rvalue state field_var
                ~proto_dies_for_vars
            with
            | None ->
              (* This field isn't accessible - use empty location *)
              []
            | Some rvalue -> SLDL.compile (SLDL.of_rvalue rvalue)
          in
          simple_location_description, field_size)
        fields
    in
    (* Combine header and fields into a composite location description *)
    let all_pieces = (header, header_size) :: field_pieces in
    let composite_location_description =
      Composite_location_description.pieces_of_simple_location_descriptions
        all_pieces
    in
    (* Create a Proto_die for the phantom block with the composite location,
       then return an implicit pointer to it *)
    let proto_die =
      Proto_die.create ~parent ~tag:Variable
        ~attribute_values:
          [ DAH.create_composite_location_description
              composite_location_description ]
        ()
    in
    let offset_in_bytes = Targetint.zero in
    let die_label = Proto_die.reference proto_die in
    let version =
      match !Dwarf_flags.gdwarf_version with
      | Four -> Dwarf_version.four
      | Five -> Dwarf_version.five
    in
    if need_rvalue
    then
      rvalue (SLDL.Rvalue.implicit_pointer ~offset_in_bytes ~die_label version)
    else
      lvalue_without_address
        (SLDL.Lvalue_without_address.implicit_pointer ~offset_in_bytes
           ~die_label version)

let single_location_description state ~parent ~subrange ~proto_dies_for_vars
    ~need_rvalue =
  let location_description =
    match ARAV.Subrange.info subrange with
    | Non_phantom { reg; offset } ->
      reg_location_description reg ~offset ~need_rvalue
    | Phantom defining_expr ->
      phantom_var_location_description state ~defining_expr ~need_rvalue
        ~proto_dies_for_vars ~parent
  in
  match location_description with
  | None -> None
  | Some (Simple simple) ->
    Some (Single_location_description.of_simple_location_description simple)
  | Some (Composite composite) ->
    Some
      (Single_location_description.of_composite_location_description composite)

type location_list_entry =
  | Dwarf_4 of Dwarf_4_location_list_entry.t
  | Dwarf_5 of Location_list_entry.t

let location_list_entry state ~subrange single_location_description :
    location_list_entry =
  let start_pos =
    Asm_label.create_int Text (ARAV.Subrange.start_pos subrange |> Label.to_int)
  in
  let start_pos_offset = ARAV.Subrange.start_pos_offset subrange in
  let end_pos =
    Asm_label.create_int Text (ARAV.Subrange.end_pos subrange |> Label.to_int)
  in
  let end_pos_offset = ARAV.Subrange.end_pos_offset subrange in
  match !Dwarf_flags.gdwarf_version with
  | Four ->
    let location_list_entry =
      Dwarf_4_location_list_entry.create_location_list_entry
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
        ~first_address_when_in_scope:start_pos
        ~first_address_when_in_scope_offset:(Some start_pos_offset)
        ~first_address_when_not_in_scope:end_pos
        ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
        ~single_location_description
    in
    Dwarf_4 location_list_entry
  | Five ->
    let start_inclusive =
      Address_table.add (DS.address_table state) start_pos
        ~adjustment:start_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let end_exclusive =
      Address_table.add (DS.address_table state) end_pos
        ~adjustment:end_pos_offset
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
    in
    let loc_desc =
      Counted_location_description.create single_location_description
    in
    let location_list_entry : Location_list_entry.entry =
      (* DWARF-5 spec page 45 line 1. *)
      Startx_endx { start_inclusive; end_exclusive; payload = loc_desc }
    in
    Dwarf_5
      (Location_list_entry.create location_list_entry
         ~start_of_code_symbol:(DS.start_of_code_symbol state))

let dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars
    (var : Backend_var.t) ~ident_for_type ~range =
  let range_info = ARAV.Range.info range in
  let provenance = ARAV.Range_info.provenance range_info in
  let (parent_proto_die : Proto_die.t), hidden =
    match provenance with
    | None ->
      (* Any variable (e.g. try_region) or parameter (e.g. my_closure) without
         provenance gets hidden. *)
      function_proto_die, true
    | Some _provenance -> function_proto_die, false
  in
  let is_parameter = ARAV.Range_info.is_parameter range_info in
  let type_and_name_attributes =
    match type_die_reference_for_var var ~proto_dies_for_vars with
    | None -> []
    | Some reference ->
      let name_for_var =
        (* For the moment assume function parameter names are unique, they
           almost always will be, and avoiding the stamps looks much better in
           the debugger. *)
        match is_parameter with
        | Parameter _ -> Backend_var.name_for_debugger var
        | Local -> Backend_var.unique_name_for_debugger var
      in
      let proto_die_reference =
        match provenance with
        | Some provenance ->
          let die_reference =
            Dwarf_type.variable_to_die state
              (Backend_var.Provenance.debug_uid provenance)
              ~parent_proto_die
          in
          die_reference
        | None -> Proto_die.reference (DS.value_type_proto_die state)
      in
      let type_attribute =
        [DAH.create_type_from_reference ~proto_die_reference]
      in
      let name_attribute = [DAH.create_name name_for_var] in
      name_attribute @ type_attribute
  in
  let location_attribute_value, location_list_in_debug_loc_table =
    (* Build a location list that identifies where the value of [var] may be
       found at runtime, indexed by program counter range. The representations
       of location lists (and range lists, used below to describe lexical
       blocks) changed completely between DWARF-4 and DWARF-5. *)
    let dwarf_4_location_list_entries, location_list =
      ARAV.Range.fold range
        ~init:([], Location_list.create ())
        ~f:(fun (dwarf_4_location_list_entries, location_list) subrange ->
          let single_location_description =
            single_location_description state ~parent:(Some function_proto_die)
              ~subrange ~proto_dies_for_vars ~need_rvalue:false
          in
          match single_location_description with
          | None -> dwarf_4_location_list_entries, location_list
          | Some single_location_description -> (
            let location_list_entry =
              location_list_entry state ~subrange single_location_description
            in
            match location_list_entry with
            | Dwarf_4 location_list_entry ->
              let dwarf_4_location_list_entries =
                location_list_entry :: dwarf_4_location_list_entries
              in
              dwarf_4_location_list_entries, location_list
            | Dwarf_5 location_list_entry ->
              let location_list =
                Location_list.add location_list location_list_entry
              in
              dwarf_4_location_list_entries, location_list))
    in
    match !Dwarf_flags.gdwarf_version with
    | Four ->
      let location_list_entries = dwarf_4_location_list_entries in
      let location_list = Dwarf_4_location_list.create ~location_list_entries in
      ( [Debug_loc_table.attribute_to_reference_location_list location_list],
        Some location_list )
    | Five ->
      let location_list_index =
        Location_list_table.add (DS.location_list_table state) location_list
      in
      [DAH.create_location location_list_index], None
  in
  let tag : Dwarf_tag.t =
    match is_parameter with
    | Parameter _index ->
      (* The lvalue DIE is the "normal" one for variables and parameters; it is
         the one that is marked with a name, for example. To avoid erroneous
         display of, or confusion around, rvalue DIEs we always mark them as
         variables not parameters. *)
      Formal_parameter
    | Local -> Variable
  in
  let reference =
    match proto_dies_for_variable var ~proto_dies_for_vars with
    | None -> None
    | Some proto_dies -> Some proto_dies.value_die_lvalue
  in
  let sort_priority =
    match is_parameter with
    | Local -> None
    | Parameter { index } ->
      (* Ensure that parameters appear in the correct order in the debugger. *)
      Some index
  in
  if not hidden
  then
    Proto_die.create_ignore ?reference ?sort_priority
      ?location_list_in_debug_loc_table ~parent:(Some parent_proto_die) ~tag
      ~attribute_values:(type_and_name_attributes @ location_attribute_value)
      ()

let iterate_over_variable_like_things state ~available_ranges_all_vars ~f =
  ARAV.iter available_ranges_all_vars ~f:(fun var range ->
      let ident_for_type = Some (Compilation_unit.get_current_exn (), var) in
      f var ~ident_for_type ~range)

let dwarf state ~function_proto_die available_ranges_all_vars =
  let proto_dies_for_vars = Backend_var.Tbl.create 42 in
  iterate_over_variable_like_things state ~available_ranges_all_vars
    ~f:(fun var ~ident_for_type:_ ~range:_ ->
      let value_die_lvalue = Proto_die.create_reference () in
      let type_die = Proto_die.create_reference () in
      assert (not (Backend_var.Tbl.mem proto_dies_for_vars var));
      Backend_var.Tbl.add proto_dies_for_vars var { value_die_lvalue; type_die });
  iterate_over_variable_like_things state ~available_ranges_all_vars
    ~f:(dwarf_for_variable state ~function_proto_die ~proto_dies_for_vars)
