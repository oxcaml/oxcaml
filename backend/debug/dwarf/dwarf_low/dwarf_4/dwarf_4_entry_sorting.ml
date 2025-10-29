(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Sort entries by VMA within groups that share a base address. Base address
   selection entries partition the list: we cannot reorder entries across base
   address boundaries, but we can sort entries between consecutive base address
   entries. *)
let sort_preserving_base_addresses ~is_base_address_selection_entry
    ~compare_ascending_vma entries =
  let rec process acc = function
    | [] -> List.sort compare_ascending_vma acc
    | entry :: rest ->
      if is_base_address_selection_entry entry
      then
        let sorted_segment = List.sort compare_ascending_vma acc in
        sorted_segment @ (entry :: process [] rest)
      else process (entry :: acc) rest
  in
  process [] entries
