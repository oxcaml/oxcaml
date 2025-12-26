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

type section_mismatch = {
  section_name : string;
  byte_offset : int;
  instruction_offset : int option;
  expected : string;
  actual : string;
  expected_size : int;
  actual_size : int;
}

type relocation_mismatch = {
  section_name : string;
  offset : int;
  expected : string;
  actual : string;
}

type mismatch =
  | Section_content of section_mismatch
  | Section_size of { section_name : string; expected : int; actual : int }
  | Relocation of relocation_mismatch
  | Missing_section of string
  | Missing_binary_sections_dir of string

type result =
  | Match of { text_size : int; data_size : int }
  | Mismatch of mismatch
  | Object_file_error of string

(* Helper to read a file as bytes *)
let read_file_bytes filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Helper to format bytes as hex dump *)
let hex_dump ?(max_bytes=32) (s : string) (offset : int) : string =
  let len = min max_bytes (String.length s - offset) in
  if len <= 0 then "(empty)"
  else begin
    let buf = Buffer.create (len * 3) in
    for i = 0 to len - 1 do
      if i > 0 && i mod 4 = 0 then Buffer.add_char buf ' ';
      Printf.bprintf buf "%02x" (Char.code s.[offset + i])
    done;
    Buffer.contents buf
  end

(* Find the first byte difference between two strings *)
let find_first_difference s1 s2 =
  let len = min (String.length s1) (String.length s2) in
  let rec loop i =
    if i >= len then
      if String.length s1 <> String.length s2 then Some len
      else None
    else if s1.[i] <> s2.[i] then Some i
    else loop (i + 1)
  in
  loop 0

(* Instruction size for alignment in text section *)
let instruction_size () =
  match Target_system.architecture () with
  | AArch64 -> Some 4
  | X86_64 -> None  (* Variable length instructions *)
  | _ -> None

(* Align offset to instruction boundary *)
let align_to_instruction offset =
  match instruction_size () with
  | Some size -> (offset / size) * size
  | None -> offset

(* Compare section contents *)
let compare_section ~section_name ~expected ~actual =
  let expected_size = String.length expected in
  let actual_size = String.length actual in
  if expected_size <> actual_size then
    Some (Section_size { section_name; expected = expected_size; actual = actual_size })
  else
    match find_first_difference expected actual with
    | None -> None
    | Some byte_offset ->
      let instruction_offset =
        if String.equal section_name "text" || String.equal section_name ".text"
        then Some (align_to_instruction byte_offset)
        else None
      in
      let display_offset = Option.value instruction_offset ~default:byte_offset in
      Some (Section_content {
        section_name;
        byte_offset;
        instruction_offset;
        expected = hex_dump expected display_offset;
        actual = hex_dump actual display_offset;
        expected_size;
        actual_size;
      })

(* Read binary emitter section from .binary-sections directory *)
let read_binary_section binary_sections_dir section_name =
  let filename = Filename.concat binary_sections_dir
    ("section_" ^ section_name ^ ".bin") in
  if Sys.file_exists filename then
    Some (read_file_bytes filename)
  else
    None

(* A binary emitter relocation: offset and symbol name *)
type be_relocation = {
  be_offset : int;
  be_symbol : string;
}

(* Read binary emitter relocations from .relocs file *)
let read_binary_relocations binary_sections_dir section_name =
  let filename = Filename.concat binary_sections_dir
    ("section_" ^ section_name ^ ".relocs") in
  if not (Sys.file_exists filename) then []
  else begin
    let ic = open_in filename in
    let relocs = ref [] in
    (try
      while true do
        let line = input_line ic in
        match String.split_on_char ' ' line with
        | [offset_str; symbol] ->
          let offset = int_of_string offset_str in
          relocs := { be_offset = offset; be_symbol = symbol } :: !relocs
        | _ -> ()
      done
    with End_of_file -> ());
    close_in ic;
    List.rev !relocs
  end

module Owee_buf = Compiler_owee.Owee_buf
module Owee_elf = Compiler_owee.Owee_elf
module Owee_macho = Compiler_owee.Owee_macho

(* Module for Mach-O parsing using owee *)
module Macho = struct
  let find_segment commands seg_name =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SEGMENT_64 seg :: _ when
          String.equal (Lazy.force seg).seg_segname seg_name ->
        Some (Lazy.force seg)
      | _ :: rest -> loop rest
    in
    loop commands

  let find_section segment sect_name =
    let sections = segment.Owee_macho.seg_sections in
    let rec loop i =
      if i >= Array.length sections then None
      else if String.equal sections.(i).sec_sectname sect_name then
        Some sections.(i)
      else loop (i + 1)
    in
    loop 0

  let extract_section buf commands ~seg_name ~sect_name =
    match find_segment commands seg_name with
    | None -> None
    | Some segment ->
      match find_section segment sect_name with
      | None -> None
      | Some section ->
        let body = Owee_macho.section_body buf segment section in
        let cursor = Owee_buf.cursor body in
        let size = Owee_buf.size body in
        Some (Owee_buf.Read.fixed_string cursor size)

  (* Find section by name in any segment (for object files with unnamed segments) *)
  let find_section_any_segment commands sect_name =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SEGMENT_64 seg :: rest ->
        let seg = Lazy.force seg in
        (match find_section seg sect_name with
         | Some sec -> Some (seg, sec)
         | None -> loop rest)
      | _ :: rest -> loop rest
    in
    loop commands

  let extract_section_any buf commands ~sect_name =
    match find_section_any_segment commands sect_name with
    | None -> None
    | Some (segment, section) ->
      let body = Owee_macho.section_body buf segment section in
      let cursor = Owee_buf.cursor body in
      let size = Owee_buf.size body in
      Some (Owee_buf.Read.fixed_string cursor size)

  let extract_sections buf commands =
    (* Try named segment first (__TEXT), then any segment (for .o files) *)
    let text =
      match extract_section buf commands ~seg_name:"__TEXT" ~sect_name:"__text" with
      | Some _ as t -> t
      | None -> extract_section_any buf commands ~sect_name:"__text"
    in
    (* Try __DATA,__data first, then __DATA,__const, then any segment *)
    let data =
      match extract_section buf commands ~seg_name:"__DATA" ~sect_name:"__data" with
      | Some _ as d -> d
      | None ->
        match extract_section buf commands ~seg_name:"__DATA" ~sect_name:"__const" with
        | Some _ as d -> d
        | None -> extract_section_any buf commands ~sect_name:"__data"
    in
    text, data

  (* Get symbol table from load commands *)
  let get_symbol_table commands =
    let rec loop = function
      | [] -> None
      | Owee_macho.LC_SYMTAB syms :: _ -> Some (Lazy.force syms)
      | _ :: rest -> loop rest
    in
    loop commands

  (* Extract relocations from a section, resolving symbol names *)
  let extract_section_relocations symbols section =
    let relocs = section.Owee_macho.sec_relocs in
    Array.to_list relocs |> List.filter_map (fun reloc ->
      match reloc with
      | `Relocation_info ri when ri.Owee_macho.ri_extern ->
        let sym_idx = ri.Owee_macho.ri_symbolnum in
        if sym_idx < Array.length symbols then
          let sym = symbols.(sym_idx) in
          Some { be_offset = ri.Owee_macho.ri_address;
                 be_symbol = sym.Owee_macho.sym_name }
        else None
      | _ -> None)

  (* Extract all relocations for text and data sections *)
  let extract_relocations commands =
    match get_symbol_table commands with
    | None -> [], []
    | Some (symbols, _strtab) ->
      let text_relocs = ref [] in
      let data_relocs = ref [] in
      List.iter (function
        | Owee_macho.LC_SEGMENT_64 seg ->
          let seg = Lazy.force seg in
          Array.iter (fun section ->
            let relocs = extract_section_relocations symbols section in
            if String.equal section.Owee_macho.sec_sectname "__text" then
              text_relocs := relocs @ !text_relocs
            else if String.equal section.Owee_macho.sec_sectname "__data" then
              data_relocs := relocs @ !data_relocs
          ) seg.Owee_macho.seg_sections
        | _ -> ()
      ) commands;
      (* Sort by offset for comparison *)
      let sort_relocs = List.sort (fun a b -> compare a.be_offset b.be_offset) in
      sort_relocs !text_relocs, sort_relocs !data_relocs
end

(* Module for ELF parsing using owee *)
module Elf = struct
  let extract_section buf sections ~section_name =
    match Owee_elf.find_section sections section_name with
    | None -> None
    | Some section ->
      let body = Owee_elf.section_body buf section in
      let cursor = Owee_buf.cursor body in
      let size = Owee_buf.size body in
      Some (Owee_buf.Read.fixed_string cursor size)

  let extract_sections buf sections =
    let text = extract_section buf sections ~section_name:".text" in
    (* Try .data first, then .rodata *)
    let data =
      match extract_section buf sections ~section_name:".data" with
      | Some _ as d -> d
      | None -> extract_section buf sections ~section_name:".rodata"
    in
    text, data
end

(* Extract sections from object file using owee *)
let extract_obj_sections unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  (* Check magic to determine format *)
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\x7FELF" ->
    (* ELF format *)
    let _header, sections = Owee_elf.read_elf buf in
    Elf.extract_sections buf sections
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe"
  | "\xfe\xed\xfa\xce" | "\xce\xfa\xed\xfe" ->
    (* Mach-O format (32 or 64 bit, big or little endian) *)
    let _header, commands = Owee_macho.read buf in
    Macho.extract_sections buf commands
  | _ ->
    None, None

(* Extract relocations from object file using owee *)
let extract_obj_relocations unix obj_file =
  let buf = Owee_buf.map_binary unix obj_file in
  let cursor = Owee_buf.cursor buf in
  let magic = Owee_buf.Read.fixed_string cursor 4 in
  Owee_buf.seek cursor 0;
  match magic with
  | "\xfe\xed\xfa\xcf" | "\xcf\xfa\xed\xfe"
  | "\xfe\xed\xfa\xce" | "\xce\xfa\xed\xfe" ->
    (* Mach-O format *)
    let _header, commands = Owee_macho.read buf in
    Macho.extract_relocations commands
  | "\x7FELF" ->
    (* ELF format - TODO: implement ELF relocation extraction *)
    [], []
  | _ ->
    [], []

(* Group relocations by offset, returning (offset, [symbols]) pairs sorted by
   offset *)
let group_relocations_by_offset relocs =
  let tbl = Hashtbl.create 16 in
  List.iter (fun r ->
    let existing = try Hashtbl.find tbl r.be_offset with Not_found -> [] in
    Hashtbl.replace tbl r.be_offset (r.be_symbol :: existing)
  ) relocs;
  let pairs = Hashtbl.fold (fun offset syms acc -> (offset, syms) :: acc) tbl [] in
  (* Sort by offset, and sort symbols within each group for stable comparison *)
  List.sort (fun (o1, _) (o2, _) -> compare o1 o2) pairs
  |> List.map (fun (offset, syms) -> (offset, List.sort String.compare syms))

(* Compare two lists of relocations, handling paired relocations (multiple
   symbols at the same offset) *)
let compare_relocations ~section_name ~expected ~actual =
  let exp_grouped = group_relocations_by_offset expected in
  let act_grouped = group_relocations_by_offset actual in
  let rec loop exp act =
    match exp, act with
    | [], [] -> None
    | [], (offset, syms) :: _ ->
      Some (Relocation {
        section_name;
        offset;
        expected = "(none)";
        actual = String.concat ", " syms;
      })
    | (offset, syms) :: _, [] ->
      Some (Relocation {
        section_name;
        offset;
        expected = String.concat ", " syms;
        actual = "(none)";
      })
    | (e_off, e_syms) :: erest, (a_off, a_syms) :: arest ->
      if e_off <> a_off then
        Some (Relocation {
          section_name;
          offset = min e_off a_off;
          expected = Printf.sprintf "%s @ 0x%x" (String.concat ", " e_syms) e_off;
          actual = Printf.sprintf "%s @ 0x%x" (String.concat ", " a_syms) a_off;
        })
      else if not (List.equal String.equal e_syms a_syms) then
        Some (Relocation {
          section_name;
          offset = e_off;
          expected = String.concat ", " e_syms;
          actual = String.concat ", " a_syms;
        })
      else
        loop erest arest
  in
  loop exp_grouped act_grouped

let compare unix ~obj_file ~binary_sections_dir =
  (* Check if binary sections directory exists *)
  if not (Sys.file_exists binary_sections_dir) then
    Mismatch (Missing_binary_sections_dir binary_sections_dir)
  else if not (Sys.is_directory binary_sections_dir) then
    Object_file_error (binary_sections_dir ^ " is not a directory")
  else begin
    (* Read binary emitter output *)
    let be_text = read_binary_section binary_sections_dir "text" in
    let be_data = read_binary_section binary_sections_dir "data" in

    (* Extract sections from object file *)
    let asm_text, asm_data =
      try extract_obj_sections unix obj_file
      with exn ->
        let msg = Printf.sprintf "Failed to read %s: %s"
          obj_file (Printexc.to_string exn) in
        raise (Failure msg)
    in

    (* Compare text section *)
    let text_result =
      match be_text, asm_text with
      | None, None -> None
      | Some _, None -> Some (Missing_section ".text (in object file)")
      | None, Some _ -> Some (Missing_section ".text (in binary emitter output)")
      | Some expected, Some actual ->
        compare_section ~section_name:"text" ~expected ~actual
    in

    match text_result with
    | Some mismatch -> Mismatch mismatch
    | None ->
      (* Compare data section *)
      let data_result =
        match be_data, asm_data with
        | None, None -> None
        | Some _, None ->
          (* Data section may be legitimately missing in object file *)
          None
        | None, Some _ ->
          (* Binary emitter didn't produce data, but assembler did - skip *)
          None
        | Some expected, Some actual ->
          compare_section ~section_name:"data" ~expected ~actual
      in

      match data_result with
      | Some mismatch -> Mismatch mismatch
      | None ->
        (* Compare relocations for both text and data sections *)
        let be_text_relocs = read_binary_relocations binary_sections_dir "text" in
        let be_data_relocs = read_binary_relocations binary_sections_dir "data" in
        let asm_text_relocs, asm_data_relocs =
          try extract_obj_relocations unix obj_file
          with _ -> [], []
        in
        let text_reloc_result =
          compare_relocations ~section_name:"text"
            ~expected:be_text_relocs ~actual:asm_text_relocs
        in
        (match text_reloc_result with
        | Some mismatch -> Mismatch mismatch
        | None ->
          let data_reloc_result =
            compare_relocations ~section_name:"data"
              ~expected:be_data_relocs ~actual:asm_data_relocs
          in
          match data_reloc_result with
          | Some mismatch -> Mismatch mismatch
          | None ->
            let text_size = Option.fold ~none:0 ~some:String.length be_text in
            let data_size = Option.fold ~none:0 ~some:String.length be_data in
            Match { text_size; data_size })
  end

let print_result ppf = function
  | Match { text_size; data_size } ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification passed@,\
       text: %d bytes, data: %d bytes@]@."
      text_size data_size
  | Mismatch (Section_content m) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Section: %s@,\
       Assembler size:      %d bytes@,\
       Binary emitter size: %d bytes@,@,\
       First difference at byte offset 0x%x%s:@,@,\
       Assembler bytes:@,  %s@,@,\
       Binary emitter bytes:@,  %s@]@."
      m.section_name
      m.actual_size
      m.expected_size
      m.byte_offset
      (match m.instruction_offset with
       | Some off when off <> m.byte_offset ->
         Printf.sprintf " (instruction at 0x%x)" off
       | _ -> "")
      m.actual
      m.expected
  | Mismatch (Section_size { section_name; expected; actual }) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Section: %s@,\
       Size mismatch: assembler produced %d bytes, \
       binary emitter produced %d bytes@]@."
      section_name actual expected
  | Mismatch (Relocation r) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Relocation mismatch in %s at offset 0x%x:@,\
       Assembler:      %s@,\
       Binary emitter: %s@]@."
      r.section_name r.offset r.actual r.expected
  | Mismatch (Missing_section name) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Missing section: %s@]@."
      name
  | Mismatch (Missing_binary_sections_dir dir) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Binary sections directory not found: %s@,\
       (Did the binary emitter run?)@]@."
      dir
  | Object_file_error msg ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,\
       Error reading object file: %s@]@."
      msg
