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

(** Functor for the production of DWARF location and range list entries. *)

open Asm_targets

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The "DW_LLE_" and "DW_RLE_" prefixes are omitted. *)
type 'payload entry =
  | End_of_list
  | Base_addressx of Address_index.t
  | Startx_endx of
      { start_inclusive : Address_index.t;
        end_exclusive : Address_index.t;
        payload : 'payload
      }
  | Startx_length of
      { start_inclusive : Address_index.t;
        length : Targetint.t;
        payload : 'payload
      }
  | Offset_pair of
      { start_offset_inclusive : Targetint.t;
        end_offset_exclusive : Targetint.t;
        payload : 'payload
      }
      (** We emit [Default_location] since it is only applicable for location
          lists and we have no use for it at present. *)
  | Offset_pair_between_labels of
      { start_inclusive : Asm_label.t;
        start_adjustment_in_bytes : int;
        end_exclusive : Asm_label.t;
        end_adjustment_in_bytes : int;
        payload : 'payload
      }
      (** As [Offset_pair], but with the offsets computed by the assembler as
          the distances from the enclosing entry's [start_of_code_symbol] (which
          must correspond to the base address established for the list, and lie
          in the same section as the labels) to the given labels plus
          adjustments. Such entries have no compile-time-computable size, so
          lists containing them may only be used with emission strategies that
          do not require sizes (see [Location_or_range_list_table]). *)
  | Base_address of Asm_symbol.t
  | Start_end of
      { start_inclusive : Asm_label.t;
        end_exclusive : Asm_label.t;
        end_adjustment : int;
            (** [end_adjustment] is not present in the DWARF specification. It
                is used for one-byte adjustments to labels' addresses to ensure
                correct delimiting of ranges. *)
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

  include Dwarf_emittable.S with type t := t
end

module Make (P : sig
  module Payload : Dwarf_emittable.S

  val code_for_entry_kind : _ entry -> int

  val section : Asm_section.dwarf_section
end) : S with type payload = P.Payload.t
