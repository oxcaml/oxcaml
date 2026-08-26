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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Asm_targets
module DIE = Debugging_information_entry
module A = Asm_directives

(* A DWARF-4 type unit destined for the .debug_types section (DWARF-4
   specification section 7.5.1.2). *)
type type_unit =
  { header_label : Asm_label.t;
    signature : Int64.t;
    primary_die_label : Asm_label.t;
    dies : DIE.t list;
    debug_abbrev_label : Asm_label.t
  }

type t = { type_units : type_unit list }

let create_type_unit ~header_label ~signature ~primary_die_label ~dies
    ~debug_abbrev_label =
  { header_label; signature; primary_die_label; dies; debug_abbrev_label }

let create ~type_units = { type_units }

let dwarf_version () =
  match !Dwarf_flags.gdwarf_version with
  | Four -> Dwarf_version.four
  | Five ->
    Misc.fatal_error
      "Type units in .debug_types may only be emitted for DWARF-4"

let debug_abbrev_offset type_unit =
  Dwarf_value.offset_into_debug_abbrev ~comment:"abbrevs. for this type unit"
    type_unit.debug_abbrev_label

let address_width_in_bytes_on_target =
  Dwarf_value.int8 ~comment:"Dwarf_arch_sizes.size_addr"
    (Numbers.Int8.of_int_exn Dwarf_arch_sizes.size_addr)

let signature_value type_unit =
  Dwarf_value.int64 ~comment:"type signature" type_unit.signature

let type_offset_value type_unit =
  match !Dwarf_flags.gdwarf_format with
  | Thirty_two ->
    Dwarf_value.distance_between_labels_32_bit ~comment:"type offset"
      ~upper:type_unit.primary_die_label ~lower:type_unit.header_label ()
  | Sixty_four ->
    Dwarf_value.distance_between_labels_64_bit ~comment:"type offset"
      ~upper:type_unit.primary_die_label ~lower:type_unit.header_label ()

let size_without_first_word type_unit =
  let ( + ) = Dwarf_int.add in
  let total_die_size =
    List.fold_left
      (fun size die -> size + DIE.size die)
      (Dwarf_int.zero ()) type_unit.dies
  in
  Dwarf_version.size (dwarf_version ())
  + Dwarf_value.size (debug_abbrev_offset type_unit)
  + Dwarf_value.size address_width_in_bytes_on_target
  + Dwarf_value.size (signature_value type_unit)
  + Dwarf_value.size (type_offset_value type_unit)
  + total_die_size

let size_type_unit type_unit =
  let size_without_first_word = size_without_first_word type_unit in
  let initial_length = Initial_length.create size_without_first_word in
  Dwarf_int.add (Initial_length.size initial_length) size_without_first_word

let size t =
  List.fold_left
    (fun size type_unit -> Dwarf_int.add size (size_type_unit type_unit))
    (Dwarf_int.zero ()) t.type_units

let emit_type_unit ~asm_directives type_unit =
  let size_without_first_word = size_without_first_word type_unit in
  let initial_length = Initial_length.create size_without_first_word in
  A.define_label type_unit.header_label;
  Initial_length.emit ~asm_directives initial_length;
  Dwarf_version.emit ~asm_directives (dwarf_version ());
  Dwarf_value.emit ~asm_directives (debug_abbrev_offset type_unit);
  Dwarf_value.emit ~asm_directives address_width_in_bytes_on_target;
  Dwarf_value.emit ~asm_directives (signature_value type_unit);
  Dwarf_value.emit ~asm_directives (type_offset_value type_unit);
  A.new_line ();
  List.iter (fun die -> DIE.emit ~asm_directives die) type_unit.dies

let emit ~asm_directives t =
  List.iter
    (fun type_unit -> emit_type_unit ~asm_directives type_unit)
    t.type_units
