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

(** Representation of the DWARF-4 .debug_types section: a series of type units,
    each identified by an 8-byte signature and referenced from other units via
    [DW_FORM_ref_sig8]. (DWARF-4 specification section 3.1.3 and 7.5.1.2.) *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets

type type_unit

type t

(** [header_label] and [primary_die_label] must be labels in the .debug_types
    section: respectively the first byte of the unit's header and the DIE
    described by the unit's type_offset header field. [debug_abbrev_label] must
    be the label on the start of the unit's abbreviations table within
    .debug_abbrev. *)
val create_type_unit :
  header_label:Asm_label.t ->
  signature:Int64.t ->
  primary_die_label:Asm_label.t ->
  dies:Debugging_information_entry.t list ->
  debug_abbrev_label:Asm_label.t ->
  type_unit

val create : type_units:type_unit list -> t

include Dwarf_emittable.S with type t := t
