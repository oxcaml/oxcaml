(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
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
module A = Asm_directives

module Entry = struct
  module T0 = struct
    type t = Asm_label_or_symbol.t

    let compare = Asm_label_or_symbol.compare

    let equal = Asm_label_or_symbol.equal

    let hash = Asm_label_or_symbol.hash

    let print = Asm_label_or_symbol.print

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end

  include T0
  include Identifiable.Make (T0)
end

type t =
  { base_addr : Asm_label.t;
    mutable next_index : Address_index.t;
    mutable table : Entry.t Address_index.Map.t;
    mutable rev_table : Address_index.t Entry.Map.t
  }

let create () =
  { base_addr = Asm_label.create (DWARF Debug_addr);
    next_index = Address_index.zero;
    table = Address_index.Map.empty;
    rev_table = Entry.Map.empty
  }

let add_entry t (entry : Entry.t) =
  match Entry.Map.find entry t.rev_table with
  | exception Not_found ->
    let index = t.next_index in
    t.next_index <- Address_index.succ index;
    t.rev_table <- Entry.Map.add entry index t.rev_table;
    t.table <- Address_index.Map.add index entry t.table;
    index
  | index -> index

let add_symbol t symbol = add_entry t (Asm_label_or_symbol.Symbol symbol)

let base_addr t = t.base_addr

let initial_length t =
  let num_entries = Int64.of_int (Address_index.Map.cardinal t.table) in
  let size_entries =
    Int64.mul num_entries (Int64.of_int Dwarf_arch_sizes.size_addr)
  in
  Initial_length.create (Dwarf_int.of_int64_exn (Int64.add 4L size_entries))

let size t =
  let initial_length = initial_length t in
  Dwarf_int.add
    (Initial_length.size initial_length)
    (Initial_length.to_dwarf_int initial_length)

let entry_to_dwarf_value entry =
  (* The table must contain relocatable absolute addresses: on ELF the static
     linker relocates them directly, and DWARF linkers such as dsymutil
     translate them using the debug map. *)
  Dwarf_value.code_address_from_label_or_symbol ~comment:"address" entry

let emit ~asm_directives t =
  Initial_length.emit ~asm_directives (initial_length t);
  Dwarf_version.emit ~asm_directives Dwarf_version.five;
  A.uint8 (Uint8.of_nonnegative_int_exn Dwarf_arch_sizes.size_addr);
  A.uint8 Uint8.zero;
  A.define_label t.base_addr;
  Address_index.Map.iter
    (fun _index entry ->
      Dwarf_value.emit ~asm_directives (entry_to_dwarf_value entry))
    t.table
