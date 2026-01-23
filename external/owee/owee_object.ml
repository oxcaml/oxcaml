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
  | Mach_o
  | Unknown

let detect_format buf =
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" -> Elf
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe" | "\xfe\xed\xfa\xce"
  | "\xce\xfa\xed\xfe" ->
    Mach_o
  | _ -> Unknown

type relocation = {
  r_offset : int;
  r_symbol : string;
  r_addend : int64;
}

let relocs_equal a b =
  String.equal a.r_symbol b.r_symbol && a.r_addend = b.r_addend

(* Check if an ELF section name is a text section (name starts with ".text").
   This is only meaningful for ELF; Mach-O uses __text in __TEXT segment. *)
let is_elf_text_section_name name = String.starts_with ~prefix:".text" name

let extract_elf_text_section buf =
  let _header, sections = Owee_elf.read_elf buf in
  match Owee_elf.find_section sections ".text" with
  | Some sec -> Some (Owee_elf.section_body_string buf sec)
  | None -> None

let extract_elf_data_section buf =
  let _header, sections = Owee_elf.read_elf buf in
  match Owee_elf.find_section sections ".data" with
  | Some sec -> Some (Owee_elf.section_body_string buf sec)
  | None -> None

let extract_elf_rodata_section buf =
  let _header, sections = Owee_elf.read_elf buf in
  match Owee_elf.find_section sections ".rodata" with
  | Some sec -> Some (Owee_elf.section_body_string buf sec)
  | None -> None

let find_macho_section_body buf commands ~seg_name ~sect_name =
  match Owee_macho.find_segment commands seg_name with
  | Some seg -> (
    match Owee_macho.find_section seg sect_name with
    | Some sec -> Some (Owee_macho.section_body_string buf seg sec)
    | None -> None)
  | None -> None

let extract_macho_text_section buf =
  let _header, commands = Owee_macho.read buf in
  find_macho_section_body buf commands ~seg_name:"__TEXT" ~sect_name:"__text"

let extract_macho_data_section buf =
  let _header, commands = Owee_macho.read buf in
  find_macho_section_body buf commands ~seg_name:"__DATA" ~sect_name:"__data"

let extract_macho_rodata_section buf =
  let _header, commands = Owee_macho.read buf in
  find_macho_section_body buf commands ~seg_name:"__DATA" ~sect_name:"__const"

let extract_text_section buf =
  match detect_format buf with
  | Elf -> extract_elf_text_section buf
  | Mach_o -> extract_macho_text_section buf
  | Unknown -> None

let extract_data_section buf =
  match detect_format buf with
  | Elf -> extract_elf_data_section buf
  | Mach_o -> extract_macho_data_section buf
  | Unknown -> None

let extract_rodata_section buf =
  match detect_format buf with
  | Elf -> extract_elf_rodata_section buf
  | Mach_o -> extract_macho_rodata_section buf
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

let extract_elf_rodata_relocations buf =
  let _header, sections = Owee_elf.read_elf buf in
  Owee_elf.extract_section_relocations buf sections ~section_name:".rodata"
  |> List.map convert_elf_reloc

(* Build an array of section names indexed by (ordinal - 1).
   Section ordinals in Mach-O are 1-based indices into the flat list of all
   sections across all segments, ordered by load command order. *)
let build_macho_section_names commands =
  let sections = ref [] in
  List.iter
    (function
      | Owee_macho.LC_SEGMENT_64 seg ->
        let seg = Lazy.force seg in
        Array.iter
          (fun sec -> sections := sec.Owee_macho.sec_sectname :: !sections)
          seg.Owee_macho.seg_sections
      | _ -> ())
    commands;
  Array.of_list (List.rev !sections)

let extract_macho_relocations buf ~seg_name ~sect_name =
  let _header, commands = Owee_macho.read buf in
  match Owee_macho.get_symbol_table commands with
  | None -> []
  | Some (symbols, _strtab) -> (
    match Owee_macho.find_segment commands seg_name with
    | None -> []
    | Some seg ->
      let section_names = build_macho_section_names commands in
      let relocs =
        Array.fold_left
          (fun acc section ->
            if String.equal section.Owee_macho.sec_sectname sect_name
            then
              Owee_macho.extract_section_relocations ~section_names symbols
                section
              @ acc
            else acc)
          [] seg.Owee_macho.seg_sections
      in
      List.map convert_macho_reloc relocs
      |> List.sort (fun a b -> compare a.r_offset b.r_offset))

let extract_text_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_text_relocations buf
  | Mach_o -> extract_macho_relocations buf ~seg_name:"__TEXT" ~sect_name:"__text"
  | Unknown -> []

let extract_macho_rodata_relocations buf =
  extract_macho_relocations buf ~seg_name:"__DATA" ~sect_name:"__const"

let extract_data_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_data_relocations buf
  | Mach_o -> extract_macho_relocations buf ~seg_name:"__DATA" ~sect_name:"__data"
  | Unknown -> []

let extract_rodata_relocations buf =
  match detect_format buf with
  | Elf -> extract_elf_rodata_relocations buf
  | Mach_o -> extract_macho_rodata_relocations buf
  | Unknown -> []

let extract_elf_individual_text_sections buf =
  let _header, sections = Owee_elf.read_elf buf in
  Array.to_list sections
  |> List.filter (fun sec -> is_elf_text_section_name sec.Owee_elf.sh_name_str)
  |> List.map (fun sec ->
         sec.Owee_elf.sh_name_str, Owee_elf.section_body_string buf sec)

let extract_individual_text_sections buf =
  match detect_format buf with
  | Elf -> extract_elf_individual_text_sections buf
  | Mach_o -> []
  | Unknown -> []

let extract_elf_individual_text_relocations buf =
  let _header, sections = Owee_elf.read_elf buf in
  Array.to_list sections
  |> List.filter (fun sec -> is_elf_text_section_name sec.Owee_elf.sh_name_str)
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
  | Mach_o -> []
  | Unknown -> []

(* Returns true if the object file uses RELA relocations (explicit addend).
   For ELF, we return true because this library only supports ELFCLASS64
   (see owee_elf.ml), and all 64-bit ELF targets (x86_64, aarch64) use RELA.
   Some 32-bit ELF targets use REL, but those are not supported. *)
let uses_rela_relocations buf =
  match detect_format buf with
  | Elf -> true
  | Mach_o -> false
  | Unknown -> false
