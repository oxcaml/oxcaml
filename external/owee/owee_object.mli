(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Jane Street Group LLC                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Unified interface for reading object files (ELF and Mach-O). *)

(** The detected object file format. *)
type format =
  | Elf
  | Mach_o
  | Unknown

(** Detect the format of an object file from its buffer. *)
val detect_format : Owee_buf.t -> format

(** A relocation with offset, symbol name, and addend. *)
type relocation = {
  r_offset : int;
  r_symbol : string;
  r_addend : int64;
}

(** Check if two relocations are equal (same symbol and addend). *)
val relocs_equal : relocation -> relocation -> bool

(** Extract the main text section content. Returns None if not found. *)
val extract_text_section : Owee_buf.t -> string option

(** Extract the data section content (.data for ELF, __data for Mach-O).
    Returns None if not found. *)
val extract_data_section : Owee_buf.t -> string option

(** Extract the read-only data section content (.rodata for ELF, __const for Mach-O).
    Returns None if not found. *)
val extract_rodata_section : Owee_buf.t -> string option

(** Extract relocations for the text section. *)
val extract_text_relocations : Owee_buf.t -> relocation list

(** Extract relocations for the data section (.data for ELF, __data for Mach-O). *)
val extract_data_relocations : Owee_buf.t -> relocation list

(** Extract relocations for the read-only data section (.rodata for ELF, __const for Mach-O). *)
val extract_rodata_relocations : Owee_buf.t -> relocation list

(** Extract individual text sections (for function-sections mode).
    Returns a list of (section_name, content) pairs.
    Only meaningful for ELF; returns empty list for Mach-O. *)
val extract_individual_text_sections : Owee_buf.t -> (string * string) list

(** Extract relocations for individual text sections (for function-sections mode).
    Returns a list of (section_name, relocations) pairs.
    Only meaningful for ELF; returns empty list for Mach-O. *)
val extract_individual_text_relocations
  : Owee_buf.t -> (string * relocation list) list

(** Returns true if the object file uses RELA relocations (explicit addend).
    ELF uses RELA, Mach-O uses REL (addend encoded in instruction). *)
val uses_rela_relocations : Owee_buf.t -> bool
