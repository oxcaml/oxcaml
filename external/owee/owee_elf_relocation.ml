(** ELF relocation section parsing. *)

(* ELF section types *)
let sht_rela = 4
let sht_symtab = 2

(* x86-64 relocation types *)
let r_x86_64_plt32 = 4L
let r_x86_64_rex_gotpcrelx = 42L
let r_x86_64_64 = 1L
let r_x86_64_pc32 = 2L

let reloc_type_name r_type =
  if Int64.equal r_type r_x86_64_plt32 then "PLT32"
  else if Int64.equal r_type r_x86_64_rex_gotpcrelx then "REX_GOTPCRELX"
  else if Int64.equal r_type r_x86_64_pc32 then "PC32"
  else if Int64.equal r_type r_x86_64_64 then "64"
  else Printf.sprintf "type=%Ld" r_type

(* Size of an Elf64_Rela entry in bytes *)
let rela_entry_size = 24

(* Size of an Elf64_Sym entry in bytes *)
let sym_entry_size = 24

(* Special section index for undefined symbols *)
let shn_undef = 0

(* Start of reserved section indices *)
let shn_loreserve = 0xff00

(* Section index stored in SHT_SYMTAB_SHNDX *)
let shn_xindex = 0xffff

type rela_entry =
  { r_offset : int64;
    r_sym : int;
    r_type : int64;
    r_addend : int64
  }

(* Extract symbol index from r_info (upper 32 bits) *)
let r_sym_of_info r_info = Int64.to_int (Int64.shift_right_logical r_info 32)

(* Extract relocation type from r_info (lower 32 bits) *)
let r_type_of_info r_info = Int64.logand r_info 0xFFFFFFFFL

let iter_rela_entries ~rela_body ~f =
  let size = Owee_buf.size rela_body in
  let num_entries = size / rela_entry_size in
  for i = 0 to num_entries - 1 do
    let entry_offset = i * rela_entry_size in
    let cursor = Owee_buf.cursor rela_body ~at:entry_offset in
    let r_offset = Owee_buf.Read.u64 cursor in
    let r_info = Owee_buf.Read.u64 cursor in
    let r_addend = Owee_buf.Read.u64 cursor in
    let entry =
      { r_offset;
        r_sym = r_sym_of_info r_info;
        r_type = r_type_of_info r_info;
        r_addend
      }
    in
    f entry
  done

(* Elf64_Sym layout:
   st_name  (4 bytes, offset 0)  - index into string table
   st_info  (1 byte,  offset 4)  - type and binding
   st_other (1 byte,  offset 5)  - visibility
   st_shndx (2 bytes, offset 6)  - section header index
   st_value (8 bytes, offset 8)  - value
   st_size  (8 bytes, offset 16) - size *)

let read_symbol_name ~symtab_body ~strtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset >= Owee_buf.size symtab_body
  then None
  else
    let cursor = Owee_buf.cursor symtab_body ~at:sym_offset in
    let st_name = Owee_buf.Read.u32 cursor in
    (* Read null-terminated string from strtab *)
    if st_name >= Owee_buf.size strtab_body
    then None
    else
      let cursor = Owee_buf.cursor strtab_body ~at:st_name in
      Owee_buf.Read.zero_string cursor ()

let read_symbol_shndx ~symtab_body ~sym_index =
  let sym_offset = sym_index * sym_entry_size in
  if sym_offset >= Owee_buf.size symtab_body
  then None
  else
    (* st_shndx is at offset 6 within the symbol entry *)
    let cursor = Owee_buf.cursor symtab_body ~at:(sym_offset + 6) in
    Some (Owee_buf.Read.u16 cursor)

(* Construct r_info from symbol index and relocation type *)
let make_r_info ~sym ~typ =
  Int64.logor (Int64.shift_left (Int64.of_int sym) 32) typ

let write_rela_entry ~cursor entry =
  Owee_buf.Write.u64 cursor entry.r_offset;
  Owee_buf.Write.u64 cursor (make_r_info ~sym:entry.r_sym ~typ:entry.r_type);
  Owee_buf.Write.u64 cursor entry.r_addend

(* Symbol binding attributes *)
module Stb = struct
  let local = 0
  let global = 1
  let weak = 2
end

(* Symbol type attributes *)
module Stt = struct
  let notype = 0
  let object_ = 1
  let func = 2
  let section = 3
  let file = 4
end

(* Symbol visibility attributes (stored in st_other) *)
module Stv = struct
  let default = 0
  let internal = 1
  let hidden = 2
  let protected = 3
end

let make_st_info ~binding ~typ = (binding lsl 4) lor typ

type sym_entry =
  { st_name : int;
    st_info : int;
    st_other : int;
    st_shndx : int;
    st_value : int64;
    st_size : int64
  }

let write_sym_entry ~cursor entry =
  Owee_buf.Write.u32 cursor entry.st_name;
  Owee_buf.Write.u8 cursor entry.st_info;
  Owee_buf.Write.u8 cursor entry.st_other;
  Owee_buf.Write.u16 cursor entry.st_shndx;
  Owee_buf.Write.u64 cursor entry.st_value;
  Owee_buf.Write.u64 cursor entry.st_size
