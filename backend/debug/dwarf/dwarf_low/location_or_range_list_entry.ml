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
module Int8 = Numbers.Int8
module A = Asm_directives

type 'payload entry =
  | End_of_list
  | Base_addressx of Address_index.t
  | Offset_pair_between_labels of
      { start_inclusive : Asm_label.t;
        start_adjustment_in_bytes : int;
        end_exclusive : Asm_label.t;
        end_adjustment_in_bytes : int;
        payload : 'payload
      }
  | Base_address of Asm_symbol.t
  | Start_end of
      { start_inclusive : Asm_label.t;
        end_exclusive : Asm_label.t;
        end_adjustment : int;
        payload : 'payload
      }
  | Start_length of
      { start_inclusive : Asm_label.t;
        length : Targetint.t;
        payload : 'payload
      }

module type S = sig
  type payload

  type nonrec entry = payload entry

  type t

  val create : entry -> start_of_code_symbol:Asm_symbol.t -> t

  val section : Asm_section.dwarf_section

  (* Note that there is no [size] function: the sizes of
     [Offset_pair_between_labels] entries (assembler-computed ULEB128 label
     differences) are not known at compile time. *)
  val emit : asm_directives:Asm_targets.Asm_directives_dwarf.t -> t -> unit
end

module Make (P : sig
  module Payload : Dwarf_emittable.S

  val code_for_entry_kind : _ entry -> int

  val section : Asm_section.dwarf_section
end) =
struct
  module Payload = P.Payload

  type payload = Payload.t

  type nonrec entry = Payload.t entry

  type t =
    { entry : entry;
      (* The base address established for the enclosing list, from which
         [Offset_pair_between_labels] offsets are computed. *)
      start_of_code_symbol : Asm_symbol.t
    }

  let create entry ~start_of_code_symbol = { entry; start_of_code_symbol }

  let section = P.section

  let emit ~asm_directives t =
    (* DWARF-5 spec page 44 lines 14--15. *)
    A.comment "List entry:";
    let comment =
      if !Clflags.keep_asm_file
      then
        let comment =
          match t.entry with
          | End_of_list -> "End_of_list"
          | Base_addressx _ -> "Base_addressx"
          | Offset_pair_between_labels _ -> "Offset_pair_between_labels"
          | Base_address _ -> "Base_address"
          | Start_end _ -> "Start_end"
          | Start_length _ -> "Start_length"
        in
        Some comment
      else None
    in
    A.int8 ?comment (Int8.of_int_exn (P.code_for_entry_kind t.entry));
    (match t.entry with
    | End_of_list -> ()
    | Base_addressx addr_index ->
      Address_index.emit ~asm_directives ~comment:"base address" addr_index
    | Offset_pair_between_labels
        { start_inclusive;
          start_adjustment_in_bytes;
          end_exclusive;
          end_adjustment_in_bytes;
          payload
        } ->
      A.delta_uleb128_label_minus_symbol ~upper:start_inclusive
        ~upper_offset:(Int64.of_int start_adjustment_in_bytes)
        ~lower:t.start_of_code_symbol;
      A.delta_uleb128_label_minus_symbol ~upper:end_exclusive
        ~upper_offset:(Int64.of_int end_adjustment_in_bytes)
        ~lower:t.start_of_code_symbol;
      Payload.emit ~asm_directives payload
    | Base_address sym -> A.symbol sym
    | Start_end { start_inclusive; end_exclusive; end_adjustment; payload } ->
      (* The addresses in [DW_LLE/RLE_start_end] and [DW_LLE/RLE_start_length]
         entries are absolute (and relocatable), not offsets from a base. *)
      Dwarf_value.emit ~asm_directives
        (Dwarf_value.code_address_from_label ~comment:"start_inclusive"
           start_inclusive);
      Dwarf_value.emit ~asm_directives
        (Dwarf_value.code_address_from_label_plus_offset
           ~comment:"end_exclusive" end_exclusive
           ~offset_in_bytes:(Targetint.of_int_exn end_adjustment));
      Payload.emit ~asm_directives payload
    | Start_length { start_inclusive; length; payload } ->
      Dwarf_value.emit ~asm_directives
        (Dwarf_value.code_address_from_label ~comment:"start_inclusive"
           start_inclusive);
      Dwarf_value.emit ~asm_directives
        (Dwarf_value.uleb128 ~comment:"length"
           (Targetint.nonnegative_to_uint64_exn length));
      Payload.emit ~asm_directives payload);
    if !Clflags.keep_asm_file then A.new_line ()
end
