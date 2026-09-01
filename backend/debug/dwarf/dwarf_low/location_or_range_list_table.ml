(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Asm_targets
module Uint8 = Numbers.Uint8
module Uint32 = Numbers.Uint32
module Uint64 = Numbers.Uint64
module A = Asm_directives

module Make (Location_or_range_list : sig
  type t

  val emit : asm_directives:Asm_directives_dwarf.t -> t -> unit

  val section : Asm_section.dwarf_section
end) =
struct
  type one_list =
    { list : Location_or_range_list.t;
      label : Asm_label.t
    }

  type t =
    { base_addr : Asm_label.t;
      mutable num_lists : int;
      mutable lists : one_list list (* in reverse order of addition *)
    }

  module Index = struct
    type t = Asm_label.t * Uint64.t

    let create label index = label, Uint64.of_nonnegative_int_exn index

    let to_label (label, _) = label

    let to_uint64 (_, index) = index
  end

  let create () =
    { base_addr = Asm_label.create (DWARF Location_or_range_list.section);
      num_lists = 0;
      lists = []
    }

  let add t list =
    let which_index = t.num_lists in
    let one_list =
      { list; label = Asm_label.create (DWARF Location_or_range_list.section) }
    in
    t.lists <- one_list :: t.lists;
    t.num_lists <- t.num_lists + 1;
    Index.create one_list.label which_index

  let base_addr t = t.base_addr

  let offset_array_supported () = !Dwarf_flags.gdwarf_offsets

  let offset_entry_count t =
    if offset_array_supported ()
    then Uint32.of_nonnegative_int_exn (List.length t.lists)
    else Uint32.zero

  let emit ~asm_directives t =
    let unit_start = Asm_label.create (DWARF Location_or_range_list.section) in
    let unit_end = Asm_label.create (DWARF Location_or_range_list.section) in
    (* The unit length is the distance from just after the unit length field
       itself to the end of the table (DWARF-5 spec page 242 lines 12--20). It
       is computed by the assembler since the sizes of some list entries are not
       known at compile time. *)
    Initial_length.emit_as_label_difference ~asm_directives ~upper:unit_end
      ~lower:unit_start;
    A.define_label unit_start;
    Dwarf_version.emit ~asm_directives Dwarf_version.five;
    A.uint8 ~comment:"Dwarf_arch_sizes.size_addr"
      (Uint8.of_nonnegative_int_exn Dwarf_arch_sizes.size_addr);
    A.uint8 ~comment:"Segment selector size" Uint8.zero;
    A.uint32 ~comment:"Offset entry count" (offset_entry_count t);
    A.comment "Base label:";
    A.define_label t.base_addr;
    let lists = List.rev t.lists in
    if offset_array_supported ()
    then (
      A.comment "Offset array:";
      List.iteri
        (fun index { label; _ } ->
          let comment =
            if !Clflags.keep_asm_file
            then Some (Printf.sprintf "offset to list number %d" index)
            else None
          in
          (* Offsets are relative to the first byte after the header, i.e. the
             position of [t.base_addr] (DWARF-5 spec page 242 line 28 and page
             243 line 1). *)
          Dwarf_value.emit ~asm_directives
            (Dwarf_value.distance_between_labels_format_width ?comment
               ~upper:label ~lower:t.base_addr ()))
        lists);
    A.comment "Range or location list(s):";
    List.iter
      (fun { list; label } ->
        A.define_label label;
        Location_or_range_list.emit ~asm_directives list)
      lists;
    A.define_label unit_end
end
