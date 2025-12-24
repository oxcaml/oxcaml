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

module Elf = Compiler_owee.Owee_elf
module Rela = Compiler_owee.Owee_elf_relocation

let log_verbose = Dissector_log.log_verbose

module Relocation_entry = struct
  type t =
    { symbol_name : string;
      offset : int64
    }

  let symbol_name t = t.symbol_name

  let offset t = t.offset
end

type t =
  { convert_to_plt : Relocation_entry.t list;
    convert_to_got : Relocation_entry.t list
  }

let convert_to_plt t = t.convert_to_plt

let convert_to_got t = t.convert_to_got

let empty = { convert_to_plt = []; convert_to_got = [] }

(* Accumulator for efficient merging - stores lists in reverse order *)
type accumulator =
  { acc_plt : Relocation_entry.t list;
    acc_got : Relocation_entry.t list
  }

let empty_accumulator = { acc_plt = []; acc_got = [] }

(* Add entries to accumulator - O(n) where n is size of entries being added *)
let accumulate acc entries =
  { acc_plt = List.rev_append entries.convert_to_plt acc.acc_plt;
    acc_got = List.rev_append entries.convert_to_got acc.acc_got
  }

(* Finalize accumulator into result - reverses the lists *)
let finalize acc =
  { convert_to_plt = List.rev acc.acc_plt;
    convert_to_got = List.rev acc.acc_got
  }

(* Parse RELA entries and extract PLT32 and REX_GOTPCRELX relocations for
   undefined symbols (st_shndx = SHN_UNDEF). Only undefined symbols need PLT/GOT
   entries since defined symbols can be resolved directly.

   - PLT32: function calls, rewritten to use IPLT - REX_GOTPCRELX: GOT-relative
   references, rewritten to use IGOT

   PC32 relocations to undefined symbols are an error - they occur when code is
   compiled with -nodynlink, which is incompatible with the dissector. *)
let parse_rela_section ~rela_body ~symtab_body ~strtab_body =
  let convert_to_plt = ref [] in
  let convert_to_got = ref [] in
  Rela.iter_rela_entries ~rela_body ~f:(fun entry ->
      (* Check for PC32 relocations to undefined symbols - these are an error *)
      if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.pc32
      then
        match Rela.read_symbol_shndx ~symtab_body ~sym_index:entry.r_sym with
        | Some shndx when shndx = Rela.shn_undef ->
          let symbol_name =
            match
              Rela.read_symbol_name ~symtab_body ~strtab_body
                ~sym_index:entry.r_sym
            with
            | Some name -> name
            | None -> "<unknown>"
          in
          Misc.fatal_errorf
            "Dissector: R_X86_64_PC32 relocation to undefined symbol %s at \
             offset 0x%Lx. This occurs when code is compiled with -nodynlink. \
             The dissector requires code to be compiled without -nodynlink \
             (i.e., with dynamic linking support enabled)."
            symbol_name entry.r_offset
        | _ -> ()
      else if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.plt32
              || Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.rex_gotpcrelx
      then
        (* Only process relocations for undefined symbols *)
        match Rela.read_symbol_shndx ~symtab_body ~sym_index:entry.r_sym with
        | None ->
          log_verbose "  reloc %s at 0x%Lx: no symbol shndx"
            (Rela.Reloc_type.name entry.r_type)
            entry.r_offset
        | Some shndx when shndx <> Rela.shn_undef ->
          log_verbose "  reloc %s at 0x%Lx: symbol defined (shndx=%d), skipping"
            (Rela.Reloc_type.name entry.r_type)
            entry.r_offset shndx
        | Some _ -> (
          match
            Rela.read_symbol_name ~symtab_body ~strtab_body
              ~sym_index:entry.r_sym
          with
          | None ->
            log_verbose "  reloc %s at 0x%Lx: no symbol name"
              (Rela.Reloc_type.name entry.r_type)
              entry.r_offset
          | Some symbol_name ->
            log_verbose "  reloc %s at 0x%Lx -> %s (UNDEF)"
              (Rela.Reloc_type.name entry.r_type)
              entry.r_offset symbol_name;
            let reloc_entry =
              { Relocation_entry.symbol_name; offset = entry.r_offset }
            in
            if Rela.Reloc_type.equal entry.r_type Rela.Reloc_type.plt32
            then convert_to_plt := reloc_entry :: !convert_to_plt
            else convert_to_got := reloc_entry :: !convert_to_got));
  { convert_to_plt = List.rev !convert_to_plt;
    convert_to_got = List.rev !convert_to_got
  }

(* Find a section by name *)
let find_section sections name =
  Array.find_opt
    (fun (section : Elf.section) -> String.equal section.sh_name_str name)
    sections

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

(* Internal version that adds to an accumulator *)
let extract_into_accumulator (unix : (module Compiler_owee.Unix_intf.S))
    ~filename acc =
  let module Unix = (val unix) in
  log_verbose "extracting relocations from %s" filename;
  let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
  let _header, sections = Elf.read_elf buf in
  (* Find all .rela.text* sections (handles function sections: .rela.text,
     .rela.text.foo, .rela.text.bar, etc.) *)
  let rela_text_sections = find_sections_with_prefix sections ".rela.text" in
  match rela_text_sections with
  | [] ->
    log_verbose "  no .rela.text* sections found";
    acc
  | _ -> (
    log_verbose "  found %d .rela.text* sections"
      (List.length rela_text_sections);
    (* Find symbol table *)
    match find_symtab_section sections with
    | None -> acc
    | Some symtab_section ->
      (* Find string table (sh_link of symtab points to it) *)
      let strtab_index = symtab_section.sh_link in
      if strtab_index >= Array.length sections
      then acc
      else
        let strtab_section = sections.(strtab_index) in
        let symtab_body = Elf.section_body buf symtab_section in
        let strtab_body = Elf.section_body buf strtab_section in
        (* Process all .rela.text* sections and accumulate results *)
        List.fold_left
          (fun acc (rela_section : Elf.section) ->
            log_verbose "  processing section %s" rela_section.sh_name_str;
            let rela_body = Elf.section_body buf rela_section in
            let result =
              parse_rela_section ~rela_body ~symtab_body ~strtab_body
            in
            accumulate acc result)
          acc rela_text_sections)

let extract unix ~filename =
  finalize (extract_into_accumulator unix ~filename empty_accumulator)

let extract_from_linked_partitions unix linked_partitions =
  let acc =
    List.fold_left
      (fun acc linked ->
        extract_into_accumulator unix
          ~filename:(Partition.Linked.linked_object linked)
          acc)
      empty_accumulator linked_partitions
  in
  finalize acc
