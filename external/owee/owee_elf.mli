open Owee_buf

(** Minimalist ELF 64 decoder *)

type identification = {
  elf_class      : u8;
  elf_data       : u8;
  elf_version    : u8;
  elf_osabi      : u8;
  elf_abiversion : u8;
}

type header = {
  e_ident     : identification;
  e_type      : u16;
  e_machine   : u16;
  e_version   : u32;
  e_entry     : u64;
  e_phoff     : u64;
  e_shoff     : u64;
  e_flags     : u32;
  e_ehsize    : u16;
  e_phentsize : u16;
  e_phnum     : u16;
  e_shentsize : u16;
  e_shnum     : u16;
  e_shstrndx  : u16;
}

type section = {
  sh_name      : u32;
  sh_type      : u32;
  sh_flags     : u64;
  sh_addr      : u64;
  sh_offset    : u64;
  sh_size      : u64;
  sh_link      : u32;
  sh_info      : u32;
  sh_addralign : u64;
  sh_entsize   : u64;
  sh_name_str  : string;
}

(** ELF section header types (sh_type field). *)
module Section_type : sig
  val sht_null : u32
  (** Inactive section header. *)

  val sht_progbits : u32
  (** Program-defined data. *)

  val sht_symtab : u32
  (** Symbol table. *)

  val sht_strtab : u32
  (** String table. *)

  val sht_rela : u32
  (** Relocation entries with addends. *)

  val sht_symtab_shndx : u32
  (** Extended section indices for symbols (when section count >= SHN_LORESERVE). *)
end

(** ELF section header flags (sh_flags field). *)
module Section_flags : sig
  (** Section attribute flags. These can be combined with [Int64.logor]. *)

  val shf_write : u64
  (** Section contains writable data. *)

  val shf_alloc : u64
  (** Section occupies memory during process execution. *)

  val shf_execinstr : u64
  (** Section contains executable machine instructions. *)

  val shf_info_link : u64
  (** sh_info field contains a section header table index. *)

  val is_set : u64 -> flag:u64 -> bool
  (** [is_set flags ~flag] returns true if [flag] is set in [flags]. *)

  val is_alloc : u64 -> bool
  (** [is_alloc flags] returns true if the section occupies memory during
      execution (i.e., [shf_alloc] is set). *)
end

(** Create a PROGBITS section header. *)
val make_progbits_section :
  sh_name:u32 ->
  sh_name_str:string ->
  sh_flags:u64 ->
  sh_offset:u64 ->
  sh_size:u64 ->
  sh_addralign:u64 ->
  section

(** Create a RELA section header. *)
val make_rela_section :
  sh_name:u32 ->
  sh_name_str:string ->
  sh_offset:u64 ->
  sh_size:u64 ->
  sh_link:u32 ->
  sh_info:u32 ->
  section

(** Create a SYMTAB_SHNDX section header for extended section indices.
    sh_link should be the index of the associated symbol table. *)
val make_symtab_shndx_section :
  sh_name:u32 ->
  sh_name_str:string ->
  sh_offset:u64 ->
  sh_size:u64 ->
  sh_link:u32 ->
  section

(** From a buffer pointing to an ELF image, [read_elf] decodes the header and
    section table. *)
val read_elf : Owee_buf.t -> header * section array

(** [write_elf] writes a header and section table to a buffer *)
val write_elf : Owee_buf.t -> header -> section array -> unit

(** From a buffer pointing to an ELF image, [section_body elf section] returns
    a sub-buffer with the contents of the [section] of the ELF image. *)
val section_body : Owee_buf.t -> section -> Owee_buf.t

(** Convenience function to find a section in the section table given its name. *)
val find_section : section array -> string -> section option

(** From a buffer pointing to an ELF image, find the body of a section given its name. *)
val find_section_body
   : Owee_buf.t
  -> section array
  -> section_name:string
  -> Owee_buf.t option

val debug_line_pointers
  : Owee_buf.t
  -> section array
  -> Owee_debug_line.pointers_to_other_sections

module String_table : sig
  type t

  (* CR-someday mshinwell: [index] should probably be [Int32.t] *)
  (** Extract a string from the string table at the given index. *)
  val get_string : t -> index:int -> string option
end

(** Fish out the string table from the given ELF buffer and section array. *)
val find_string_table : Owee_buf.t -> section array -> String_table.t option

module Symbol_table : sig
  (** One or more ELF symbol tables, conjoined. *)
  type t

  module Symbol : sig
    type t

    type type_attribute = private
      | Notype
      | Object
      | Func
      | Section
      | File
      | Common
      | TLS
      | GNU_ifunc
      | Other of int

    type binding_attribute = private
      | Local
      | Global
      | Weak
      | GNU_unique
      | Other of int

    type visibility = private
      | Default
      | Internal
      | Hidden
      | Protected

    val name : t -> String_table.t -> string option

    (** For avoidance of doubt, when [t] is a function symbol, [value]
        returns the address of the top of the function. *)
    val value : t -> Int64.t

    val size_in_bytes : t -> Int64.t
    val type_attribute : t -> type_attribute
    val binding_attribute : t -> binding_attribute
    val visibility : t -> visibility
    val section_header_table_index : t -> int
  end

  (** The symbols in the table whose value and size determine that they
      cover [address]. *)
  val symbols_enclosing_address
     : t
    -> address:Int64.t
    -> Symbol.t list

  (** As for [symbols_enclosing_address], but only returns function
      symbols. *)
  val functions_enclosing_address
     : t
    -> address:Int64.t
    -> Symbol.t list

  (** [iter t ~f] calls f on each symbol found in [t]. *)
  val iter : t -> f:(Symbol.t -> unit) -> unit
end

(** Fish out both the dynamic and static symbol tables (.dynsym and .symtab)
    from the given ELF buffer and section array. *)
val find_symbol_table : Owee_buf.t -> section array -> Symbol_table.t option

(** [iter_symbols ~symtab_body ~strtab_body ~f] iterates over all symbols in
    the symbol table, calling [f] with each symbol's name and raw fields.
    This is a lower-level interface than Symbol_table for cases where you need
    to process all symbols sequentially. *)
val iter_symbols :
  symtab_body:Owee_buf.t ->
  strtab_body:Owee_buf.t ->
  f:(name:string ->
     st_info:int ->
     st_other:int ->
     st_shndx:int ->
     st_value:int64 ->
     st_size:int64 ->
     unit) ->
  unit
