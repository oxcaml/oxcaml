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
module Buf = Compiler_owee.Owee_buf
module Rela = Compiler_owee.Owee_elf_relocation

let log_verbose = Dissector_log.log_verbose

(* A partition's partially-linked object file. *)
module Mapped_object_file = struct
  type t =
    { filename : string;
      buf : Buf.t;
      header : Elf.header;
      sections : Elf.section array;
      symbols : Elf.symbol array;
      rela_text_sections : (Elf.section * Buf.t) list
    }

  let filename t = t.filename

  let buf t = t.buf

  let header t = t.header

  let sections t = t.sections

  let symbols t = t.symbols

  let rela_text_sections t = t.rela_text_sections

  (* Find all sections with names starting with prefix *)
  let find_sections_with_prefix sections prefix =
    Array.to_list sections
    |> List.filter (fun (section : Elf.section) ->
        String.starts_with ~prefix section.sh_name_str)

  (* Find the symbol table section *)
  let find_symtab_section sections =
    Array.find_opt
      (fun (section : Elf.section) ->
        Elf.Section_type.(equal (of_u32 section.sh_type) sht_symtab))
      sections

  let read (unix : (module Compiler_owee.Unix_intf.S)) ~filename =
    let module Unix = (val unix) in
    log_verbose "extracting relocations from %s" filename;
    let buf = Buf.map_binary (module Unix) filename in
    let header, sections = Elf.read_elf buf in
    (* Find all .rela.text* sections (handles function sections: .rela.text,
       .rela.text.foo, .rela.text.bar, etc.) *)
    let rela_text_sections = find_sections_with_prefix sections ".rela.text" in
    (match rela_text_sections with
    | [] -> log_verbose "  no .rela.text* sections found"
    | _ ->
      log_verbose "  found %d .rela.text* sections"
        (List.length rela_text_sections));
    (* Find symbol table *)
    let symtab_section =
      match find_symtab_section sections with
      | Some section -> section
      | None ->
        Misc.fatal_errorf "Dissector: no symbol table found in %s" filename
    in
    (* Find string table (sh_link of symtab points to it) *)
    let strtab_index = symtab_section.sh_link in
    if strtab_index >= Array.length sections
    then
      Misc.fatal_errorf "Dissector: symtab sh_link out of range in %s" filename;
    let strtab_section = sections.(strtab_index) in
    let symtab_body = Elf.section_body buf symtab_section in
    let strtab_body = Elf.section_body buf strtab_section in
    let symbols = Elf.read_symbols ~symtab_body ~strtab_body in
    let rela_text_sections =
      List.map
        (fun (section : Elf.section) -> section, Elf.section_body buf section)
        rela_text_sections
    in
    { filename; buf; header; sections; symbols; rela_text_sections }
end

type t =
  { plt_symbols : Relocatable_symbol_name.t list;
    got_symbols : Relocatable_symbol_name.t list;
    num_plt : int;
    num_got : int
  }

let plt_symbols t = t.plt_symbols

let got_symbols t = t.got_symbols

let num_plt t = t.num_plt

let num_got t = t.num_got

(* Record a relocation; we only care about which symbols relocations occur
   against, not their frequency, so we use a bitmap to deduplicate. *)
let record_relocation ~seen ~acc ~num ~sym_index ~symbol_name =
  (* [num] counts every site, not deduplicated symbols: it feeds the
     per-partition log summary in [Dissector.run]. *)
  incr num;
  if not (Misc.Bitmap.get seen sym_index)
  then begin
    Misc.Bitmap.set seen sym_index;
    acc := Relocatable_symbol_name.of_string symbol_name :: !acc
  end

(* Parse RELA entries and extract PLT32 and REX_GOTPCRELX relocations for
   undefined symbols (st_shndx = SHN_UNDEF). Only undefined symbols need PLT/GOT
   entries since defined symbols can be resolved directly.

   - PLT32: function calls, rewritten to use IPLT

   - REX_GOTPCRELX: GOT-relative references, rewritten to use IGOT.

   PC32 relocations to undefined symbols are an error. They occur when code is
   compiled with -nodynlink, which is incompatible with the dissector. *)
let parse_rela_section ~rela_body ~symbols ~record_plt ~record_got =
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      (* Check for PC32 relocations to undefined symbols - these are an error *)
      if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.pc32
      then
        match Elf.symbol_table_lookup symbols ~sym_index:entry.r_sym with
        | Some sym when Rela.Section_index.is_undef sym.Elf.st_shndx ->
          let symbol_name =
            if String.equal sym.Elf.name "" then "<unknown>" else sym.Elf.name
          in
          Misc.fatal_errorf
            "Dissector: R_X86_64_PC32 relocation to undefined symbol %s at \
             offset 0x%Lx. This occurs when code is compiled with -nodynlink. \
             The dissector requires code to be compiled without -nodynlink \
             (i.e., with dynamic linking support enabled)."
            symbol_name entry.r_offset
        | _ -> ()
      else if
        Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.plt32
        || Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.rex_gotpcrelx
      then
        (* Only process relocations for undefined symbols *)
        match Elf.symbol_table_lookup symbols ~sym_index:entry.r_sym with
        | None ->
          log_verbose "  reloc %s at 0x%Lx: no symbol shndx"
            (Rela.Reloc_type.name entry.r_type)
            entry.r_offset
        | Some sym when Rela.Section_index.is_defined sym.Elf.st_shndx ->
          log_verbose "  reloc %s at 0x%Lx: symbol defined (shndx=%d), skipping"
            (Rela.Reloc_type.name entry.r_type)
            entry.r_offset
            (Rela.Section_index.to_int sym.Elf.st_shndx)
        | Some sym ->
          let symbol_name = sym.Elf.name in
          log_verbose "  reloc %s at 0x%Lx -> %s (UNDEF)"
            (Rela.Reloc_type.name entry.r_type)
            entry.r_offset symbol_name;
          if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.plt32
          then record_plt ~sym_index:entry.r_sym ~symbol_name
          else record_got ~sym_index:entry.r_sym ~symbol_name)

let extract_from_rela_text_sections ~symbols sections =
  (* Symbols are deduplicated, keyed on the symbol table index. One "seen"
     bitmap per list: a symbol may need both an IPLT and an IGOT entry. *)
  let num_symbols = Array.length symbols in
  let plt_seen = Misc.Bitmap.make num_symbols in
  let got_seen = Misc.Bitmap.make num_symbols in
  let plt_symbols = ref [] in
  let got_symbols = ref [] in
  let num_plt = ref 0 in
  let num_got = ref 0 in
  let record_plt ~sym_index ~symbol_name =
    record_relocation ~seen:plt_seen ~acc:plt_symbols ~num:num_plt ~sym_index
      ~symbol_name
  in
  let record_got ~sym_index ~symbol_name =
    record_relocation ~seen:got_seen ~acc:got_symbols ~num:num_got ~sym_index
      ~symbol_name
  in
  List.iter
    (fun ((rela_section : Elf.section), rela_body) ->
      log_verbose "  processing section %s" rela_section.sh_name_str;
      parse_rela_section ~rela_body ~symbols ~record_plt ~record_got)
    sections;
  (* CR sspies: The reversal here affects the order in which entries are written
     into the respective tables. The order should not matter, so we can
     eventually remove this reversal as well. *)
  { plt_symbols = List.rev !plt_symbols;
    got_symbols = List.rev !got_symbols;
    num_plt = !num_plt;
    num_got = !num_got
  }

let extract (input : Mapped_object_file.t) =
  let symbols = Mapped_object_file.symbols input in
  extract_from_rela_text_sections ~symbols
    (Mapped_object_file.rela_text_sections input)
