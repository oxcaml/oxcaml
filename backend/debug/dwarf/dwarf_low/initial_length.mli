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

(** A value of type [t] represents an "initial length" (DWARF-4 standard section
    7.4). *)
type t

val create : Dwarf_int.t -> t

val to_dwarf_int : t -> Dwarf_int.t

include Dwarf_emittable.S with type t := t

(** Emit an initial length whose value is computed by the assembler as the
    distance between the two labels (which must be in the same section),
    including the 64-bit indicator when the DWARF format is 64-bit. *)
val emit_as_label_difference :
  asm_directives:Asm_targets.Asm_directives_dwarf.t ->
  upper:Asm_targets.Asm_label.t ->
  lower:Asm_targets.Asm_label.t ->
  unit
