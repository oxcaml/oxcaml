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
module A = Asm_directives
module Uint8 = Numbers.Uint8

module Make (Entry : Location_or_range_list_entry.S) = struct
  type t = Entry.t list

  let create () = []

  let add t entry = entry :: t

  let section = Entry.section

  let size t =
    (* The extra byte is for the terminating end-of-list entry
       ([DW_LLE_end_of_list] or [DW_RLE_end_of_list], both code 0). *)
    List.fold_left
      (fun size entry -> Dwarf_int.add size (Entry.size entry))
      (Dwarf_int.one ()) t

  let emit ~asm_directives t =
    A.comment "Start of list:";
    A.new_line ();
    List.iter (fun entry -> Entry.emit ~asm_directives entry) (List.rev t);
    A.uint8 ~comment:"End of list" Uint8.zero
end
