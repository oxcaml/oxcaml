(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** ELF relocation section parsing.

    This module provides support for reading RELA (relocations with addends)
    sections from ELF files. *)

(** {1 Section Types} *)

val sht_rela : int
(** Section type for RELA sections (relocations with addends). *)

val sht_symtab : int
(** Section type for symbol table sections. *)

val shn_undef : int
(** Special section index indicating an undefined symbol. *)

val shn_loreserve : int
(** Start of reserved section indices (0xff00). Section indices >= this value
    require extended section indices via SHT_SYMTAB_SHNDX. *)

val shn_xindex : int
(** Special section index (0xffff) indicating that the actual section index
    is stored in the corresponding SHT_SYMTAB_SHNDX entry. *)

(** {1 x86-64 Relocation Types} *)

(** Abstract type representing an x86-64 relocation type. *)
module Reloc_type : sig
  type t

  val equal : t -> t -> bool
  (** Test equality of relocation types. *)

  val to_int64 : t -> int64
  (** Convert to the raw ELF relocation type value. *)

  val of_int64 : int64 -> t
  (** Create from a raw ELF relocation type value. *)

  val plt32 : t
  (** R_X86_64_PLT32 relocation type. *)

  val rex_gotpcrelx : t
  (** R_X86_64_REX_GOTPCRELX relocation type. *)

  val r64 : t
  (** R_X86_64_64 relocation type (64-bit absolute). *)

  val pc32 : t
  (** R_X86_64_PC32 relocation type (32-bit PC-relative). *)

  val name : t -> string
  (** [name t] returns a human-readable name for the relocation type.
      Known types are returned as short names like "PLT32",
      unknown types are returned as "type=N". *)
end

(** {1 RELA Entry Parsing} *)

(** A parsed RELA entry. *)
type rela_entry =
  { r_offset : int64;
    (** Offset within the section being relocated. *)
    r_sym : int;
    (** Symbol table index. *)
    r_type : Reloc_type.t;
    (** Relocation type. *)
    r_addend : int64
    (** Addend for the relocation. *)
  }

(** [iter_rela_entries ~rela_body ~f] iterates over all RELA entries in
    the given section body, calling [f] for each entry. *)
val iter_rela_entries : rela_body:Owee_buf.t -> f:(rela_entry -> unit) -> unit

(** {1 Symbol Name Lookup} *)

(** [read_symbol_name ~symtab_body ~strtab_body ~sym_index] reads the name
    of the symbol at the given index from the symbol table.

    Returns [None] if the index is out of bounds or the name cannot be read. *)
val read_symbol_name :
  symtab_body:Owee_buf.t -> strtab_body:Owee_buf.t -> sym_index:int -> string option

(** [read_symbol_shndx ~symtab_body ~sym_index] reads the section header index
    (st_shndx) of the symbol at the given index.

    Returns [None] if the index is out of bounds.
    A value of [shn_undef] (0) indicates an undefined symbol. *)
val read_symbol_shndx : symtab_body:Owee_buf.t -> sym_index:int -> int option

(** {1 Entry Sizes} *)

val rela_entry_size : int
(** Size of an Elf64_Rela entry in bytes (24). *)

val sym_entry_size : int
(** Size of an Elf64_Sym entry in bytes (24). *)

(** {1 Writing RELA Entries} *)

(** [write_rela_entry ~cursor entry] writes a RELA entry at the current
    cursor position and advances the cursor by [rela_entry_size] bytes. *)
val write_rela_entry : cursor:Owee_buf.cursor -> rela_entry -> unit

(** {1 Symbol Table Writing} *)

(** Symbol binding attributes for st_info. *)
module Symbol_binding : sig
  type t

  val to_int : t -> int
  (** Convert to the raw ELF binding value. *)

  val local : t
  val global : t
  val weak : t
end

(** Symbol type attributes for st_info. *)
module Symbol_type : sig
  type t

  val to_int : t -> int
  (** Convert to the raw ELF symbol type value. *)

  val notype : t
  val object_ : t
  val func : t
  val section : t
  val file : t
end

(** Symbol visibility attributes for st_other. *)
module Symbol_visibility : sig
  type t

  val to_int : t -> int
  (** Convert to the raw ELF visibility value. *)

  val default : t
  val internal : t
  val hidden : t
  val protected : t
end

(** [make_st_info ~binding ~typ] creates the st_info byte from binding and
    type attributes. *)
val make_st_info : binding:Symbol_binding.t -> typ:Symbol_type.t -> int

(** A symbol table entry to write. *)
type sym_entry =
  { st_name : int;
    (** Index into string table. *)
    st_info : int;
    (** Symbol type and binding. *)
    st_other : int;
    (** Symbol visibility. *)
    st_shndx : int;
    (** Section header index. *)
    st_value : int64;
    (** Symbol value. *)
    st_size : int64
    (** Symbol size. *)
  }

(** [write_sym_entry ~cursor entry] writes a symbol table entry at the
    current cursor position and advances the cursor by [sym_entry_size]
    bytes. *)
val write_sym_entry : cursor:Owee_buf.cursor -> sym_entry -> unit
