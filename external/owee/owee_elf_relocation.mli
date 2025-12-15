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

val r_x86_64_plt32 : int64
(** R_X86_64_PLT32 relocation type. *)

val r_x86_64_rex_gotpcrelx : int64
(** R_X86_64_REX_GOTPCRELX relocation type. *)

(** {1 RELA Entry Parsing} *)

(** A parsed RELA entry. *)
type rela_entry =
  { r_offset : int64;
    (** Offset within the section being relocated. *)
    r_sym : int;
    (** Symbol table index. *)
    r_type : int64;
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

(** {1 Additional Relocation Types} *)

val r_x86_64_64 : int64
(** R_X86_64_64 relocation type (64-bit absolute). *)

val r_x86_64_pc32 : int64
(** R_X86_64_PC32 relocation type (32-bit PC-relative). *)

(** [reloc_type_name r_type] returns a human-readable name for the
    relocation type. Known types are returned as short names like "PLT32",
    unknown types are returned as "type=N". *)
val reloc_type_name : int64 -> string

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
module Stb : sig
  val local : int
  val global : int
  val weak : int
end

(** Symbol type attributes for st_info. *)
module Stt : sig
  val notype : int
  val object_ : int
  val func : int
  val section : int
  val file : int
end

(** Symbol visibility attributes for st_other. *)
module Stv : sig
  val default : int
  val internal : int
  val hidden : int
  val protected : int
end

(** [make_st_info ~binding ~typ] creates the st_info byte from binding and
    type attributes. *)
val make_st_info : binding:int -> typ:int -> int

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
