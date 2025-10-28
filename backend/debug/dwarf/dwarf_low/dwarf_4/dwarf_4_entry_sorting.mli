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

(** Utilities for sorting DWARF-4 location and range list entries. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Sort entries by VMA within groups that share a base address. Base address
    selection entries create boundaries: entries cannot be reordered across
    these boundaries, but can be sorted within each group between consecutive
    base address selection entries. *)
val sort_preserving_base_addresses :
  is_base_address_selection_entry:('a -> bool) ->
  compare_ascending_vma:('a -> 'a -> int) ->
  'a list ->
  'a list
