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

type format =
  | Elf
  | Macho
  | Unknown

let detect_format buf =
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" -> Elf
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe" | "\xfe\xed\xfa\xce"
  | "\xce\xfa\xed\xfe" ->
    Macho
  | _ -> Unknown

type relocation = {
  r_offset : int;
  r_symbol : string;
  r_addend : int64;
}

let relocs_equal a b =
  String.equal a.r_symbol b.r_symbol && a.r_addend = b.r_addend

let is_text_section_name name =
  String.length name >= 5 && String.sub name 0 5 = ".text"

let extract_elf_text_section buf =
  let _header, sections = Owee_elf.read_elf buf in
  match Owee_elf.find_section sections ".text" with
  | Some sec -> Some (Owee_elf.section_body_string buf sec)
  | None -> None

let extract_elf_data_section buf =
  let _header, sections = Owee_elf.read_elf buf in
  match Owee_elf.find_section sections ".data" with
  | Some sec -> Some (Owee_elf.section_body_string buf sec)
  | None -> (
    match Owee_elf.find_section sections ".rodata" with
    | Some sec -> Some (Owee_elf.section_body_string buf sec)
    | None -> None)

let extract_macho_text_section buf =
  let _header, commands = Owee_macho.read buf in
  match Owee_macho.find_segment commands "__TEXT" with
  | Some seg -> (
    match Owee_macho.find_section seg "__text" with
    | Some sec -> Some (Owee_macho.section_body_string buf seg sec)
    | None -> None)
  | None -> (
    match Owee_macho.find_section_any_segment commands "__text" with
    | Some (seg, sec) -> Some (Owee_macho.section_body_string buf seg sec)
    | None -> None)

let extract_macho_data_section buf =
  let _header, commands = Owee_macho.read buf in
  let try_section seg_name sect_name =
    match Owee_macho.find_segment commands seg_name with
    | Some seg -> (
      match Owee_macho.find_section seg sect_name with
      | Some sec -> Some (Owee_macho.section_body_string buf seg sec)
      | None -> None)
    | None -> None
  in
  match try_section "__DATA" "__data" with
  | Some _ as result -> result
  | None -> (
    match try_section "__DATA" "__const" with
    | Some _ as result -> result
    | None -> (
      match Owee_macho.find_section_any_segment commands "__data" with
      | Some (seg, sec) -> Some (Owee_macho.section_body_string buf seg sec)
      | None -> None))

let extract_text_section buf =
  match detect_format buf with
  | Elf -> extract_elf_text_section buf
  | Macho -> extract_macho_text_section buf
  | Unknown -> None

let extract_data_section buf =
  match detect_format buf with
  | Elf -> extract_elf_data_section buf
  | Macho -> extract_macho_data_section buf
  | Unknown -> None

let convert_elf_reloc (r : Owee_elf.relocation) : relocation =
  { r_offset = r.r_offset; r_symbol = r.r_symbol; r_addend = r.r_addend }

let convert_macho_reloc (r : Owee_macho.resolved_relocation) : relocation =
  { r_offset = r.r_offset; r_symbol = r.r_symbol; r_addend = r.r_addend }

let extract_elf_text_relocations buf =
  let _header, sections = Owee_elf.read_elf buf in
  Owee_elf.extract_section_relocations buf sections ~section_name:".text"
  |> List.map convert_elf_reloc

let extract_elf_data_relocations buf =
  let _header, sections = Owee_elf.read_elf buf in
  Owee_elf.extract_section_relocations buf sections ~section_name:".data"
  |> List.map convert_elf_reloc

let extract_macho_relocations_by_section buf sect_name =
  let _header, commands = Owee_macho.read buf in
  match Owee_macho.get_symbol_table commands with
  | None -> []
  | Some (symbols, _strtab) ->
    let relocs = ref [] in
    List.iter
      (function
        | Owee_macho.LC_SEGMENT_64 seg ->
          let seg = Lazy.force seg in
          Array.iter
            (fun section ->
              if String.equal section.Owee_macho.sec_sectname sect_name
              then
                relocs
                  := Owee_macho.extract_section_relocations symbols section
                     @ !relocs)
            seg.Owee_macho.seg_sections
        | _ -> ())
      commands;
    List.map convert_macho_reloc !relocs
    |> List.sort (fun a b -> compare a.r_offset b.r_offset)

let extract_text_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_text_relocations buf
  | Macho -> extract_macho_relocations_by_section buf "__text"
  | Unknown -> []

let extract_data_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_data_relocations buf
  | Macho -> extract_macho_relocations_by_section buf "__data"
  | Unknown -> []

let extract_elf_individual_text_sections buf =
  let _header, sections = Owee_elf.read_elf buf in
  Array.to_list sections
  |> List.filter (fun sec -> is_text_section_name sec.Owee_elf.sh_name_str)
  |> List.map (fun sec ->
         sec.Owee_elf.sh_name_str, Owee_elf.section_body_string buf sec)

let extract_individual_text_sections buf =
  match detect_format buf with
  | Elf -> extract_elf_individual_text_sections buf
  | Macho -> []
  | Unknown -> []

let extract_elf_individual_text_relocations buf =
  let _header, sections = Owee_elf.read_elf buf in
  Array.to_list sections
  |> List.filter (fun sec -> is_text_section_name sec.Owee_elf.sh_name_str)
  |> List.map (fun sec ->
         let relocs =
           Owee_elf.extract_section_relocations buf sections
             ~section_name:sec.Owee_elf.sh_name_str
           |> List.map convert_elf_reloc
         in
         sec.Owee_elf.sh_name_str, relocs)

let extract_individual_text_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_individual_text_relocations buf
  | Macho -> []
  | Unknown -> []

let uses_rela_relocations buf =
  match detect_format buf with
  | Elf -> true
  | Macho -> false
  | Unknown -> false
