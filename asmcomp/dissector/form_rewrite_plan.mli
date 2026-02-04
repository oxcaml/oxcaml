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

(* CR mshinwell: This file needs to be code reviewed *)

(** Form a rewrite plan for ELF section rewriting.

    This module analyzes the input ELF file and builds a plan describing all the
    modifications needed: new sections, relocated symbols, and file layout. The
    plan can then be executed by [Rewrite_sections]. *)

(** Information about an original symbol from the ELF symbol table. *)
module Symbol_entry : sig
  type t

  (** Returns the symbol name. *)
  val name : t -> string

  (** Returns st_info (binding and type packed into one byte). *)
  val st_info : t -> int

  (** Returns st_other (visibility). *)
  val st_other : t -> int

  (** Returns st_shndx (section index). *)
  val st_shndx : t -> int

  (** Returns st_value (symbol value/address). *)
  val st_value : t -> int64

  (** Returns st_size (symbol size). *)
  val st_size : t -> int64
end

(** Layout of a section in the output file. *)
module Section_layout : sig
  type t

  (** Returns the file offset of the section. *)
  val offset : t -> int64

  (** Returns the size of the section in bytes. *)
  val size : t -> int64
end

(** Layout of all sections in the output file. *)
module Layout : sig
  type t

  (** Returns the layout of the IGOT section. *)
  val igot : t -> Section_layout.t

  (** Returns the layout of the .rela.igot section. *)
  val rela_igot : t -> Section_layout.t

  (** Returns the layout of the IPLT section. *)
  val iplt : t -> Section_layout.t

  (** Returns the layout of the .rela.iplt section. *)
  val rela_iplt : t -> Section_layout.t

  (** Returns the layout of the symbol table section. *)
  val symtab : t -> Section_layout.t

  (** Returns the layout of SYMTAB_SHNDX section, if present. *)
  val symtab_shndx : t -> Section_layout.t option

  (** Returns the layout of the string table section. *)
  val strtab : t -> Section_layout.t

  (** Returns the layout of the section header string table. *)
  val shstrtab : t -> Section_layout.t

  (** Returns the file offset of the section headers. *)
  val section_headers_offset : t -> int64

  (** Returns the total size of the output file. *)
  val total_size : t -> int64
end

(** Rewritten relocation entries for a single .rela.text* section. *)
module Rewritten_rela_section : sig
  type t

  (** Original file offset of this section. *)
  val section_offset : t -> int64

  (** The rewritten relocation entries. *)
  val entries : t -> Compiler_owee.Owee_elf_relocation.rela_entry list
end

(** A rewrite plan for an ELF file. *)
type t

(** Returns the original symbols from the input file's symbol table. *)
val original_symbols : t -> Symbol_entry.t array

(** Returns a map from symbol name to index in the output symbol table. *)
val symbol_to_index : t -> int Misc.Stdlib.String.Tbl.t

(** Returns the total number of symbols in the output symbol table. *)
val total_symbols : t -> int

(** Returns the list of rewritten .rela.text* sections, each with their original
    file offset and rewritten entries. *)
val rewritten_rela_sections : t -> Rewritten_rela_section.t list

(** Returns the string table for symbol names. *)
val strtab : t -> Compiler_owee.Owee_elf_string_table.t

(** Returns the section header string table. *)
val shstrtab : t -> Compiler_owee.Owee_elf_string_table.t

(** Returns a map from original section names to (offset, renamed_name) pairs in
    shstrtab. For Large_code partitions, the stored values point to the renamed
    name (e.g., .caml.p1.text instead of .text). *)
val section_name_offsets : t -> (int * string) Misc.Stdlib.String.Tbl.t

(** Returns the offset of the IGOT section name in shstrtab. *)
val igot_name_offset : t -> int

(** Returns the IGOT section name string. *)
val igot_name_str : t -> string

(** Returns the offset of the .rela.igot section name in shstrtab. *)
val rela_igot_name_offset : t -> int

(** Returns the .rela.igot section name string. *)
val rela_igot_name_str : t -> string

(** Returns the offset of the IPLT section name in shstrtab. *)
val iplt_name_offset : t -> int

(** Returns the IPLT section name string. *)
val iplt_name_str : t -> string

(** Returns the offset of the .rela.iplt section name in shstrtab. *)
val rela_iplt_name_offset : t -> int

(** Returns the .rela.iplt section name string. *)
val rela_iplt_name_str : t -> string

(** Returns the section index of the IGOT section. *)
val igot_idx : t -> int

(** Returns the section index of the .rela.igot section. *)
val rela_igot_idx : t -> int

(** Returns the section index of the IPLT section. *)
val iplt_idx : t -> int

(** Returns the section index of the .rela.iplt section. *)
val rela_iplt_idx : t -> int

(** Returns the total number of sections in the output file. *)
val num_sections : t -> int

(** Returns the section index of the symbol table. *)
val symtab_idx : t -> int

(** Returns the index of the SYMTAB_SHNDX section in the input file. *)
val symtab_shndx_idx : t -> int option

(** Returns the index of a newly created SYMTAB_SHNDX section, if one needs to
    be created because the input lacks one but new indices >= SHN_LORESERVE. *)
val new_symtab_shndx_idx : t -> int option

(** Returns the name offset in shstrtab for a new SYMTAB_SHNDX section. *)
val symtab_shndx_name_offset : t -> int option

(** Returns the layout of all sections in the output file. *)
val layout : t -> Layout.t

(** [compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_sections
     ~partition_kind ~igot_and_iplt ~relocations] analyzes the ELF structure and
    builds a rewrite plan.

    [rela_text_sections] is a list of (section, body) pairs for all .rela.text*
    sections in the input file. This handles both traditional single .rela.text
    sections and function sections (.rela.text.foo, .rela.text.bar, etc.).

    For [Large_code] partitions, section names are renamed with a prefix (e.g.,
    .text -> .caml.p1.text). *)
val compute :
  header:Compiler_owee.Owee_elf.header ->
  sections:Compiler_owee.Owee_elf.section array ->
  symtab_body:Compiler_owee.Owee_buf.t ->
  strtab_body:Compiler_owee.Owee_buf.t ->
  rela_text_sections:
    (Compiler_owee.Owee_elf.section * Compiler_owee.Owee_buf.t) list ->
  partition_kind:Partition.kind ->
  igot_and_iplt:Build_igot_and_iplt.t ->
  relocations:Extract_relocations.t ->
  t
