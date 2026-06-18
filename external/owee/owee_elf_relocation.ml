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

(* Section index constants *)
let shn_undef = 0
let shn_loreserve = 0xff00
let shn_xindex = 0xffff

module Section_index = struct
  type t = int

  let of_int t = t
  let to_int t = t

  let undef = shn_undef
  let xindex = shn_xindex

  let is_undef t = t = shn_undef
  let is_defined t = t <> shn_undef
  let needs_extended t = t >= shn_loreserve
end

(* Relocation types *)
module Reloc_type = struct
  type t = int64

  let equal = Int64.equal
  let to_int64 t = t
  let of_int64 t = t

  (* x86-64 relocation types *)
  let plt32 = 4L
  let rex_gotpcrelx = 42L
  let r64 = 1L
  let pc32 = 2L

  (* AArch64 relocation types *)
  let aarch64_abs64 = 257L
  let aarch64_prel32 = 261L
  let aarch64_call26 = 283L
  let aarch64_jump26 = 282L
  let aarch64_adr_prel_pg_hi21 = 275L
  let aarch64_add_abs_lo12_nc = 277L
  let aarch64_ldst64_abs_lo12_nc = 286L
  let aarch64_adr_got_page = 311L
  let aarch64_ld64_got_lo12_nc = 312L

  let name t =
    if Int64.equal t plt32 then "PLT32"
    else if Int64.equal t rex_gotpcrelx then "REX_GOTPCRELX"
    else if Int64.equal t pc32 then "PC32"
    else if Int64.equal t r64 then "64"
    else if Int64.equal t aarch64_abs64 then "AARCH64_ABS64"
    else if Int64.equal t aarch64_prel32 then "AARCH64_PREL32"
    else if Int64.equal t aarch64_call26 then "AARCH64_CALL26"
    else if Int64.equal t aarch64_jump26 then "AARCH64_JUMP26"
    else if Int64.equal t aarch64_adr_prel_pg_hi21
    then "AARCH64_ADR_PREL_PG_HI21"
    else if Int64.equal t aarch64_add_abs_lo12_nc
    then "AARCH64_ADD_ABS_LO12_NC"
    else if Int64.equal t aarch64_ldst64_abs_lo12_nc
    then "AARCH64_LDST64_ABS_LO12_NC"
    else if Int64.equal t aarch64_adr_got_page
    then "AARCH64_ADR_GOT_PAGE"
    else if Int64.equal t aarch64_ld64_got_lo12_nc
    then "AARCH64_LD64_GOT_LO12_NC"
    else Printf.sprintf "type=%Ld" t
end

(* Size of an Elf64_Rela entry in bytes *)
let rela_entry_size = 24

(* Size of an Elf64_Sym entry in bytes *)
let sym_entry_size = 24

type rela_entry =
  { r_offset : int64;
    r_sym : int;
    r_type : Reloc_type.t;
    r_addend : int64
  }

(* Extract symbol index from r_info (upper 32 bits) *)
let r_sym_of_info r_info = Int64.to_int (Int64.shift_right_logical r_info 32)

(* Extract relocation type from r_info (lower 32 bits) *)
let r_type_of_info r_info = Int64.logand r_info 0xFFFFFFFFL

let iter_rela_entries ~rela_body ~f =
  let size = Owee_buf.size rela_body in
  if size mod rela_entry_size <> 0
  then
    Owee_buf.invalid_formatf
      "RELA section size %d is not a multiple of entry size %d" size
      rela_entry_size;
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
    Some (Section_index.of_int (Owee_buf.Read.u16 cursor))

(* Construct r_info from symbol index and relocation type *)
let make_r_info ~sym ~typ =
  Int64.logor (Int64.shift_left (Int64.of_int sym) 32) (Reloc_type.to_int64 typ)

let write_rela_entry ~cursor entry =
  Owee_buf.Write.u64 cursor entry.r_offset;
  Owee_buf.Write.u64 cursor (make_r_info ~sym:entry.r_sym ~typ:entry.r_type);
  Owee_buf.Write.u64 cursor entry.r_addend

(* Symbol binding attributes *)
module Symbol_binding = struct
  type t = int

  let to_int t = t

  let local = 0
  let global = 1
  let weak = 2
end

(* Symbol type attributes *)
module Symbol_type = struct
  type t = int

  let to_int t = t

  let notype = 0
  let object_ = 1
  let func = 2
  let section = 3
  let file = 4
end

(* Symbol visibility attributes (stored in st_other) *)
module Symbol_visibility = struct
  type t = int

  let to_int t = t

  let default = 0
  let internal = 1
  let hidden = 2
  let protected = 3
end

let make_st_info ~binding ~typ =
  (Symbol_binding.to_int binding lsl 4) lor Symbol_type.to_int typ

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
