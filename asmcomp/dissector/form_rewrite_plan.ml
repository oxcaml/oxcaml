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

module Elf = Compiler_owee.Owee_elf
module Rela = Compiler_owee.Owee_elf_relocation
module Strtab = Compiler_owee.Owee_elf_string_table
module String = Misc.Stdlib.String

let log_verbose = Dissector_log.log_verbose

let align_up64 value alignment =
  let alignment = Int64.of_int alignment in
  let mask = Int64.sub alignment 1L in
  Int64.logand (Int64.add value mask) (Int64.lognot mask)

module Symbol_entry = struct
  type t =
    { name : string;
      st_info : int;
      st_other : int;
      st_shndx : int;
      st_value : int64;
      st_size : int64
    }

  let name s = s.name

  let st_info s = s.st_info

  let st_other s = s.st_other

  let st_shndx s = s.st_shndx

  let st_value s = s.st_value

  let st_size s = s.st_size
end

module Section_layout = struct
  type t =
    { offset : int64;
      size : int64
    }

  let offset l = l.offset

  let size l = l.size
end

module Layout = struct
  type t =
    { igot : Section_layout.t;
      rela_igot : Section_layout.t;
      iplt : Section_layout.t;
      rela_iplt : Section_layout.t;
      symtab : Section_layout.t;
      symtab_shndx : Section_layout.t option;
      strtab : Section_layout.t;
      shstrtab : Section_layout.t;
      section_headers_offset : int64;
      total_size : int64
    }

  let igot l = l.igot

  let rela_igot l = l.rela_igot

  let iplt l = l.iplt

  let rela_iplt l = l.rela_iplt

  let symtab l = l.symtab

  let symtab_shndx l = l.symtab_shndx

  let strtab l = l.strtab

  let shstrtab l = l.shstrtab

  let section_headers_offset l = l.section_headers_offset

  let total_size l = l.total_size
end

(* Rewritten relocation entries for a single .rela.text* section *)
module Rewritten_rela_section = struct
  type t =
    { section_offset : int64; (* Original file offset of this section *)
      entries : Rela.rela_entry list
    }

  let section_offset t = t.section_offset

  let entries t = t.entries
end

type t =
  { original_symbols : Symbol_entry.t array;
    symbol_to_index : int String.Tbl.t;
    total_symbols : int;
    rewritten_rela_sections : Rewritten_rela_section.t list;
    strtab : Strtab.t;
    shstrtab : Strtab.t;
    section_name_offsets : (int * string) String.Tbl.t;
    igot_name_offset : int;
    igot_name_str : string;
    rela_igot_name_offset : int;
    rela_igot_name_str : string;
    iplt_name_offset : int;
    iplt_name_str : string;
    rela_iplt_name_offset : int;
    rela_iplt_name_str : string;
    igot_idx : int;
    rela_igot_idx : int;
    iplt_idx : int;
    rela_iplt_idx : int;
    num_sections : int;
    symtab_idx : int;
    symtab_shndx_idx : int option;
    (* When we need to create a new SYMTAB_SHNDX section (input doesn't have one
       but new section indices >= SHN_LORESERVE) *)
    new_symtab_shndx_idx : int option;
    symtab_shndx_name_offset : int option;
    layout : Layout.t
  }

let original_symbols t = t.original_symbols

let symbol_to_index t = t.symbol_to_index

let total_symbols t = t.total_symbols

let rewritten_rela_sections t = t.rewritten_rela_sections

let strtab t = t.strtab

let shstrtab t = t.shstrtab

let section_name_offsets t = t.section_name_offsets

let igot_name_offset t = t.igot_name_offset

let igot_name_str t = t.igot_name_str

let rela_igot_name_offset t = t.rela_igot_name_offset

let rela_igot_name_str t = t.rela_igot_name_str

let iplt_name_offset t = t.iplt_name_offset

let iplt_name_str t = t.iplt_name_str

let rela_iplt_name_offset t = t.rela_iplt_name_offset

let rela_iplt_name_str t = t.rela_iplt_name_str

let igot_idx t = t.igot_idx

let rela_igot_idx t = t.rela_igot_idx

let iplt_idx t = t.iplt_idx

let rela_iplt_idx t = t.rela_iplt_idx

let num_sections t = t.num_sections

let symtab_idx t = t.symtab_idx

let symtab_shndx_idx t = t.symtab_shndx_idx

let new_symtab_shndx_idx t = t.new_symtab_shndx_idx

let symtab_shndx_name_offset t = t.symtab_shndx_name_offset

let layout t = t.layout

let read_symbols ~symtab_body ~strtab_body =
  let symbols = ref [] in
  Elf.iter_symbols ~symtab_body ~strtab_body
    ~f:(fun ~name ~st_info ~st_other ~st_shndx ~st_value ~st_size ->
      symbols
        := { Symbol_entry.name; st_info; st_other; st_shndx; st_value; st_size }
           :: !symbols);
  Array.of_list (List.rev !symbols)

let build_symbol_index_map ~original_symbols ~igot_and_iplt strtab =
  let symbol_to_index = String.Tbl.create 256 in
  Array.iteri
    (fun index sym ->
      let name = Symbol_entry.name sym in
      if not (String.Tbl.mem symbol_to_index name)
      then String.Tbl.add symbol_to_index name index;
      ignore (Strtab.add strtab name))
    original_symbols;
  let next_index = ref (Array.length original_symbols) in
  List.iter
    (fun entry ->
      let igot_sym = Igot.Entry.igot_symbol entry in
      String.Tbl.add symbol_to_index igot_sym !next_index;
      ignore (Strtab.add strtab igot_sym);
      incr next_index)
    (Igot.entries (Build_igot_and_iplt.igot igot_and_iplt));
  List.iter
    (fun entry ->
      let iplt_sym = Iplt.Entry.iplt_symbol entry in
      String.Tbl.add symbol_to_index iplt_sym !next_index;
      ignore (Strtab.add strtab iplt_sym);
      incr next_index)
    (Iplt.entries (Build_igot_and_iplt.iplt igot_and_iplt));
  symbol_to_index, !next_index

(* Build a map from original symbol name to the new synthetic symbol name. For
   PLT relocations, maps to IPLT symbol; for GOT relocations, maps to IGOT
   symbol. This handles function sections correctly since we match by symbol
   name rather than offset. *)
let build_symbol_rewrite_map ~igot_and_iplt ~relocations =
  let plt_map = String.Tbl.create 256 in
  let got_map = String.Tbl.create 256 in
  List.iter
    (fun entry ->
      match
        Build_igot_and_iplt.iplt_symbol_for_plt_reloc igot_and_iplt entry
      with
      | Some sym ->
        let orig_sym = Extract_relocations.Relocation_entry.symbol_name entry in
        if not (String.Tbl.mem plt_map orig_sym)
        then String.Tbl.add plt_map orig_sym sym
      | None -> ())
    (Extract_relocations.convert_to_plt relocations);
  List.iter
    (fun entry ->
      match
        Build_igot_and_iplt.igot_symbol_for_got_reloc igot_and_iplt entry
      with
      | Some sym ->
        let orig_sym = Extract_relocations.Relocation_entry.symbol_name entry in
        if not (String.Tbl.mem got_map orig_sym)
        then String.Tbl.add got_map orig_sym sym
      | None -> ())
    (Extract_relocations.convert_to_got relocations);
  plt_map, got_map

(* Rewrite a single .rela.text* section. Looks up each relocation's target
   symbol and rewrites PLT32/GOTPCRELX relocations to PC32 relocations targeting
   the synthetic IPLT/IGOT symbols.

   - PLT32: function calls -> PC32 to IPLT entry - GOTPCRELX: GOT references ->
   PC32 to IGOT entry *)
let rewrite_rela_section ~rela_body ~symtab_body ~strtab_body ~symbol_to_index
    ~plt_rewrite_map ~got_rewrite_map =
  let entries = ref [] in
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      let new_entry =
        (* Look up the original symbol name for this relocation *)
        let sym_name_opt =
          Rela.read_symbol_name ~symtab_body ~strtab_body ~sym_index:entry.r_sym
        in
        match sym_name_opt with
        | None -> entry
        | Some sym_name -> (
          (* Check if this relocation type/symbol should be rewritten *)
          let rewrite_to =
            if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.plt32
            then String.Tbl.find_opt plt_rewrite_map sym_name
            else if Rela.Reloc_type.equal entry.r_type
                      Rela.Reloc_type.rex_gotpcrelx
            then String.Tbl.find_opt got_rewrite_map sym_name
            else None
          in
          match rewrite_to with
          | Some new_sym_name -> (
            match String.Tbl.find_opt symbol_to_index new_sym_name with
            | Some idx ->
              log_verbose "  rewrite reloc at 0x%Lx: %s %s -> PC32 to %s"
                entry.r_offset
                (Rela.Reloc_type.name entry.r_type)
                sym_name new_sym_name;
              { entry with r_sym = idx; r_type = Rela.Reloc_type.pc32 }
            | None ->
              log_verbose
                "  rewrite reloc at 0x%Lx: %s -> %s NOT FOUND in symtab"
                entry.r_offset
                (Rela.Reloc_type.name entry.r_type)
                new_sym_name;
              entry)
          | None -> entry)
      in
      entries := new_entry :: !entries);
  List.rev !entries

(* Each entry in SYMTAB_SHNDX is 4 bytes (Elf64_Word) *)
let symtab_shndx_entry_size = 4

(* Compute file layout. Since we rewrite .rela.text* sections in place, we don't
   allocate new space for them - only for the new IGOT/IPLT sections and the
   updated symtab/strtab/shstrtab. If the input has a SYMTAB_SHNDX section, we
   also allocate space for the extended version. *)
let compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
    ~strtab_size ~shstrtab_size ~num_sections ~shentsize ~has_symtab_shndx =
  let current = ref original_data_end in
  let alloc alignment size =
    current := align_up64 !current alignment;
    let offset = !current in
    let size = Int64.of_int size in
    current := Int64.add offset size;
    { Section_layout.offset; size }
  in
  let igot_t = Build_igot_and_iplt.igot igot_and_iplt in
  let iplt_t = Build_igot_and_iplt.iplt igot_and_iplt in
  let igot = alloc 16 (Igot.section_size igot_t) in
  let rela_igot =
    let count = List.length (Igot.relocations igot_t) in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let iplt = alloc 16 (Iplt.section_size iplt_t) in
  let rela_iplt =
    let count = List.length (Iplt.relocations iplt_t) in
    alloc 8 (count * Rela.rela_entry_size)
  in
  let symtab = alloc 8 (total_symbols * Rela.sym_entry_size) in
  (* Allocate SYMTAB_SHNDX section if input has one *)
  let symtab_shndx =
    if has_symtab_shndx
    then Some (alloc 4 (total_symbols * symtab_shndx_entry_size))
    else None
  in
  let strtab = alloc 1 strtab_size in
  let shstrtab = alloc 1 shstrtab_size in
  let section_headers_offset = align_up64 !current 8 in
  let total_size =
    Int64.add section_headers_offset (Int64.of_int (num_sections * shentsize))
  in
  { Layout.igot;
    rela_igot;
    iplt;
    rela_iplt;
    symtab;
    symtab_shndx;
    strtab;
    shstrtab;
    section_headers_offset;
    total_size
  }

(* Sections that should be renamed for Large_code partitions *)
let sections_to_rename = [".text"; ".rodata"; ".data"; ".bss"; ".eh_frame"]

(* Check if a section base name (without .rela prefix) needs renaming *)
let base_needs_rename base =
  List.exists
    (fun s -> String.equal base s || String.starts_with ~prefix:s base)
    sections_to_rename

(* Rename a section name based on partition kind.

   For Large_code partitions, .text -> .caml.p1.text, .rela.text ->
   .rela.caml.p1.text, etc. *)
let rename_section ~(partition_kind : Partition.kind) name =
  match partition_kind with
  | Main -> name
  | Large_code _ ->
    let prefix = Partition.section_prefix partition_kind in
    let is_rela = String.starts_with ~prefix:".rela" name in
    if is_rela
    then
      (* For .rela.* sections, check if the base (after .rela) needs renaming *)
      let base = String.sub name 5 (String.length name - 5) in
      if base_needs_rename base then ".rela" ^ prefix ^ base else name
    else if base_needs_rename name
    then prefix ^ name
    else name

(* [rela_text_sections] is a list of (section, body) pairs for all .rela.text*
   sections in the input file. Each section's relocations will be rewritten to
   use the synthetic IGOT/IPLT symbols. *)
let compute ~header ~sections ~symtab_body ~strtab_body ~rela_text_sections
    ~partition_kind ~igot_and_iplt ~relocations =
  log_verbose "forming rewrite plan for partition %s"
    (Partition.symbol_prefix partition_kind);
  let original_symbols = read_symbols ~symtab_body ~strtab_body in
  let strtab = Strtab.create () in
  let symbol_to_index, total_symbols =
    build_symbol_index_map ~original_symbols ~igot_and_iplt strtab
  in
  let plt_rewrite_map, got_rewrite_map =
    build_symbol_rewrite_map ~igot_and_iplt ~relocations
  in
  (* Rewrite all .rela.text* sections *)
  let rewritten_rela_sections =
    List.map
      (fun (section, rela_body) ->
        log_verbose "  rewriting section %s" section.Elf.sh_name_str;
        let entries =
          rewrite_rela_section ~rela_body ~symtab_body ~strtab_body
            ~symbol_to_index ~plt_rewrite_map ~got_rewrite_map
        in
        { Rewritten_rela_section.section_offset = section.Elf.sh_offset;
          entries
        })
      rela_text_sections
  in
  let shstrtab = Strtab.create () in
  let section_name_offsets = String.Tbl.create 64 in
  Array.iter
    (fun (s : Elf.section) ->
      let renamed = rename_section ~partition_kind s.sh_name_str in
      let offset = Strtab.add shstrtab renamed in
      String.Tbl.add section_name_offsets s.sh_name_str (offset, renamed))
    sections;
  let igot_name_str = rename_section ~partition_kind ".data.igot" in
  let igot_name_offset = Strtab.add shstrtab igot_name_str in
  let rela_igot_name_str = rename_section ~partition_kind ".rela.data.igot" in
  let rela_igot_name_offset = Strtab.add shstrtab rela_igot_name_str in
  let iplt_name_str = rename_section ~partition_kind ".text.iplt" in
  let iplt_name_offset = Strtab.add shstrtab iplt_name_str in
  let rela_iplt_name_str = rename_section ~partition_kind ".rela.text.iplt" in
  let rela_iplt_name_offset = Strtab.add shstrtab rela_iplt_name_str in
  let num_original = Array.length sections in
  let igot_idx = num_original in
  let rela_igot_idx = num_original + 1 in
  let iplt_idx = num_original + 2 in
  let rela_iplt_idx = num_original + 3 in
  let find_section_by_type sh_type =
    let rec loop i =
      if i >= Array.length sections
      then None
      else if Elf.Section_type.(equal (of_u32 sections.(i).Elf.sh_type) sh_type)
      then Some i
      else loop (i + 1)
    in
    loop 0
  in
  let symtab_idx =
    match find_section_by_type Elf.Section_type.sht_symtab with
    | Some i -> i
    | None -> 0
  in
  (* Find the SYMTAB_SHNDX section if present *)
  let symtab_shndx_idx =
    find_section_by_type Elf.Section_type.sht_symtab_shndx
  in
  (* We need SYMTAB_SHNDX if the input has one, OR if our new section indices
     are >= SHN_LORESERVE (65280). *)
  let needs_symtab_shndx =
    Option.is_some symtab_shndx_idx
    || Rela.Section_index.(needs_extended (of_int igot_idx))
  in
  (* If we need SYMTAB_SHNDX but the input doesn't have it, we need to create a
     new section. *)
  let need_new_symtab_shndx =
    needs_symtab_shndx && Option.is_none symtab_shndx_idx
  in
  let new_symtab_shndx_idx, symtab_shndx_name_offset, num_sections =
    if need_new_symtab_shndx
    then
      let idx = num_original + 4 in
      let name_offset = Strtab.add shstrtab ".symtab_shndx" in
      Some idx, Some name_offset, num_original + 5
    else None, None, num_original + 4
  in
  let original_data_end =
    Array.fold_left
      (fun acc (s : Elf.section) -> max acc (Int64.add s.sh_offset s.sh_size))
      0L sections
  in
  let layout =
    compute_file_layout ~original_data_end ~igot_and_iplt ~total_symbols
      ~strtab_size:(Strtab.length strtab)
      ~shstrtab_size:(Strtab.length shstrtab) ~num_sections
      ~shentsize:header.Elf.e_shentsize ~has_symtab_shndx:needs_symtab_shndx
  in
  { original_symbols;
    symbol_to_index;
    total_symbols;
    rewritten_rela_sections;
    strtab;
    shstrtab;
    section_name_offsets;
    igot_name_offset;
    igot_name_str;
    rela_igot_name_offset;
    rela_igot_name_str;
    iplt_name_offset;
    iplt_name_str;
    rela_iplt_name_offset;
    rela_iplt_name_str;
    igot_idx;
    rela_igot_idx;
    iplt_idx;
    rela_iplt_idx;
    num_sections;
    symtab_idx;
    symtab_shndx_idx;
    new_symtab_shndx_idx;
    symtab_shndx_name_offset;
    layout
  }
