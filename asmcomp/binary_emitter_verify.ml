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

(* Binary emitter verification: compare binary emitter output against system
   assembler output according to the selected comparison mode. *)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_object = Compiler_owee.Owee_object

type relocation = Owee_object.relocation

type section_mismatch =
  { section_name : string;
    byte_offset : int;
    instruction_offset : int option;
    expected : string;
    actual : string;
    expected_size : int;
    actual_size : int
  }

type relocation_mismatch =
  { section_name : string;
    offset : int;
    expected : string;
    actual : string
  }

type mismatch =
  | Section_content of section_mismatch
  | Section_size of
      { section_name : string;
        expected : int;
        actual : int
      }
  | Relocation of relocation_mismatch
  | Instruction of
      { section_name : string;
        index : int;
        assembler : string;
        binary_emitter : string
      }
  | Instruction_count of
      { section_name : string;
        assembler : int;
        binary_emitter : int
      }
  | Missing_section of string
  | Missing_binary_sections_dir of string

type result =
  | Match of
      { text_size : int;
        data_size : int
      }
  | Mismatch of mismatch
  | Object_file_error of string
  | Error of string

(* Text sections: either a single .text section or per-function sections *)
type text_sections =
  | No_function_sections of string option
  | Function_sections of (string * string) list

(* Utilities *)

let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let hex_dump ?(max_bytes = 32) s offset =
  let len = min max_bytes (String.length s - offset) in
  if len <= 0
  then "(empty)"
  else
    let buf = Buffer.create (len * 3) in
    for i = 0 to len - 1 do
      if i > 0 && i mod 4 = 0 then Buffer.add_char buf ' ';
      Printf.bprintf buf "%02x" (Char.code s.[offset + i])
    done;
    Buffer.contents buf

let find_first_difference s1 s2 =
  let len = min (String.length s1) (String.length s2) in
  let rec loop i =
    if i >= len
    then if String.length s1 <> String.length s2 then Some len else None
    else if s1.[i] <> s2.[i]
    then Some i
    else loop (i + 1)
  in
  loop 0

let is_text_section name =
  String.equal name "text" || String.starts_with ~prefix:".text" name

let align_to_instruction offset =
  match Target_system.architecture () with
  | AArch64 -> offset / 4 * 4
  | X86_64 | _ -> offset

(* Reading binary emitter output from .binary-sections directory *)

let read_section dir name =
  let path = Filename.concat dir ("section_" ^ name ^ ".bin") in
  if Sys.file_exists path then Some (read_file path) else None

let read_relocations dir name =
  let path = Filename.concat dir ("section_" ^ name ^ ".relocs") in
  if not (Sys.file_exists path)
  then []
  else
    let ic = open_in path in
    let relocs = ref [] in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char ' ' line with
         | [offset_str; symbol] ->
           relocs
             := { Owee_object.r_offset = int_of_string offset_str;
                  r_symbol = symbol;
                  r_addend = 0L
                }
                :: !relocs
         | [offset_str; symbol; addend_str] ->
           relocs
             := { Owee_object.r_offset = int_of_string offset_str;
                  r_symbol = symbol;
                  r_addend = Int64.of_string addend_str
                }
                :: !relocs
         | _ -> ()
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !relocs

let read_text_sections dir =
  if (not (Sys.file_exists dir)) || not (Sys.is_directory dir)
  then No_function_sections None
  else
    let files = Sys.readdir dir in
    let function_sections =
      Array.to_list files
      |> List.filter_map (fun f ->
          if
            String.starts_with ~prefix:"section_text." f
            && String.ends_with ~suffix:".bin" f
            && not (String.equal f "section_text.bin")
          then
            let name = "." ^ String.sub f 8 (String.length f - 12) in
            Some (name, read_file (Filename.concat dir f))
          else None)
    in
    match function_sections with
    | [] -> No_function_sections (read_section dir "text")
    | sections ->
      (* When there are function sections, also include the main .text section
         if it exists (even if empty) to match assembler output *)
      let main_text =
        match read_section dir "text" with
        | Some content -> [".text", content]
        | None -> []
      in
      Function_sections (main_text @ sections)

let read_text_relocations dir ~include_main_text =
  if (not (Sys.file_exists dir)) || not (Sys.is_directory dir)
  then []
  else
    let files = Sys.readdir dir in
    let function_relocs =
      Array.to_list files
      |> List.filter_map (fun f ->
          if
            String.starts_with ~prefix:"section_text." f
            && String.ends_with ~suffix:".relocs" f
            && not (String.equal f "section_text.relocs")
          then
            let name_part = String.sub f 8 (String.length f - 15) in
            Some ("." ^ name_part, read_relocations dir name_part)
          else None)
    in
    if include_main_text
    then (".text", read_relocations dir "text") :: function_relocs
    else function_relocs

(* Section comparison *)

type comparison_mode =
  | Exact
  | Disassembly

(* Disassembly-based comparison: disassemble both binary emitter output and
   assembler output using objdump in raw binary mode, then compare the textual
   instruction representations (mnemonic + operands). Since both byte streams go
   through the same disassembler, any difference that changes the decoded
   instruction text is caught. This is intentionally not a byte-exact check:
   different encodings with the same decoded text are accepted, and details that
   objdump omits (for example branch/call targets when [--no-addresses] is used)
   are not compared. *)
module Disassembly = struct
  type classified_line =
    | Skip
    | Instr of string
    | Parse_error of string

  (* Classify a single line of objdump output. With --no-addresses, instruction
     lines start with a tab followed by the instruction text. The text runs up
     to an optional '#' comment, which we strip so that RIP-relative comment
     targets don't cause spurious mismatches. Expected non-instruction lines
     (file header, section headers, blanks) are skipped. Anything else is a
     parse error; we'd rather fail loudly than silently drop lines and report a
     false match. *)
  let classify_line line =
    let trimmed = String.trim line in
    if
      trimmed = ""
      || String.starts_with ~prefix:"Disassembly of section" trimmed
      || String.starts_with ~prefix:"<" trimmed
      ||
      (* e.g. "foo.bin: file format binary" *)
      Misc.Stdlib.String.is_substring trimmed ~substring:"file format"
    then Skip
    else if String.starts_with ~prefix:"\t" line
    then
      let mnemonic_end =
        match String.index_opt line '#' with
        | Some i -> i
        | None -> String.length line
      in
      (* Skip the leading tab (index 0), take up to the comment or end. *)
      Instr (String.trim (String.sub line 1 (mnemonic_end - 1)))
    else Parse_error line

  let extract_instructions output =
    let rec loop acc = function
      | [] -> Result.Ok (List.rev acc)
      | line :: rest -> (
        match classify_line line with
        | Skip -> loop acc rest
        | Instr s -> loop (s :: acc) rest
        | Parse_error l ->
          Result.Error (Printf.sprintf "unexpected objdump line: %S" l))
    in
    loop [] (String.split_on_char '\n' output)

  type redirect =
    | Null
    | File of string
    | Pipe

  let run ~command ?(stdout = Pipe) ?(stderr = Pipe) args =
    let stdout_str =
      match stdout with
      | Pipe -> ""
      | Null -> " > /dev/null"
      | File path -> Printf.sprintf " > %s" (Filename.quote path)
    in
    let stderr_str =
      match stderr with
      | Pipe -> ""
      | Null -> " 2> /dev/null"
      | File path -> Printf.sprintf " 2> %s" (Filename.quote path)
    in
    let cmd =
      String.concat " " (Filename.quote command :: List.map Filename.quote args)
      ^ stdout_str ^ stderr_str
    in
    Ccomp.command cmd

  let disassemble bin_path =
    let out_file = Filename.temp_file "caml_objdump" ".txt" in
    let machine =
      match Target_system.architecture () with
      | X86_64 -> "i386:x86-64"
      | AArch64 -> "aarch64"
      | _ -> "i386:x86-64"
    in
    let rc =
      run ~command:"objdump" ~stdout:(File out_file)
        [ "-D";
          "-b";
          "binary";
          "-m";
          machine;
          "--no-show-raw-insn";
          "--no-addresses";
          "-z";
          bin_path ]
    in
    if rc <> 0
    then (
      (try Sys.remove out_file with Sys_error _ -> ());
      Result.Error (Printf.sprintf "objdump failed (exit %d)" rc))
    else
      let output = read_file out_file in
      (try Sys.remove out_file with Sys_error _ -> ());
      extract_instructions output

  let compare_instruction_lists ~section_name ~assembler ~binary_emitter =
    let rec loop i asm be =
      match asm, be with
      | [], [] -> None
      | [], _ :: _ ->
        Some
          (Instruction_count
             { section_name;
               assembler = i;
               binary_emitter = i + List.length be
             })
      | _ :: _, [] ->
        Some
          (Instruction_count
             { section_name;
               assembler = i + List.length asm;
               binary_emitter = i
             })
      | a :: asm_rest, b :: be_rest ->
        if String.equal a b
        then loop (i + 1) asm_rest be_rest
        else
          Some
            (Instruction
               { section_name; index = i; assembler = a; binary_emitter = b })
    in
    loop 0 assembler binary_emitter

  let write_temp_bin contents =
    let path = Filename.temp_file "caml_verify" ".bin" in
    let oc = open_out_bin path in
    output_string oc contents;
    close_out oc;
    path

  let disassemble_contents contents =
    (* [objdump -D -b binary] exits with status 1 on a zero-byte file. Empty
       sections are legitimate (e.g. [.note.GNU-stack], or [code_end]/empty
       jump-tables under [-function-sections]), so short-circuit instead of
       invoking objdump. *)
    if String.length contents = 0
    then Result.Ok []
    else
      let bin_path = write_temp_bin contents in
      let result = disassemble bin_path in
      (try Sys.remove bin_path with Sys_error _ -> ());
      result

  let compare_sections ~section_name ~expected ~actual =
    match disassemble_contents expected, disassemble_contents actual with
    | Result.Error e, _ | _, Result.Error e -> Some (Error e)
    | Result.Ok be_instrs, Result.Ok asm_instrs ->
      Option.map
        (fun m -> Mismatch m)
        (compare_instruction_lists ~section_name ~assembler:asm_instrs
           ~binary_emitter:be_instrs)
end

let byte_mismatch ~section_name ~expected ~actual byte_offset =
  let instruction_offset =
    if is_text_section section_name
    then Some (align_to_instruction byte_offset)
    else None
  in
  let display_offset = Option.value instruction_offset ~default:byte_offset in
  Section_content
    { section_name;
      byte_offset;
      instruction_offset;
      expected = hex_dump expected display_offset;
      actual = hex_dump actual display_offset;
      expected_size = String.length expected;
      actual_size = String.length actual
    }

(* Returns [result option]: [None] means match, [Some r] means mismatch or
   error. *)
let compare_section ~comparison_mode ~section_name ~expected ~actual =
  let expected_size = String.length expected in
  let actual_size = String.length actual in
  if expected_size <> actual_size
  then
    Some
      (Mismatch
         (Section_size
            { section_name; expected = expected_size; actual = actual_size }))
  else
    match comparison_mode with
    | Disassembly when is_text_section section_name ->
      Disassembly.compare_sections ~section_name ~expected ~actual
    | Exact | Disassembly -> (
      match find_first_difference expected actual with
      | None -> None
      | Some byte_offset ->
        Some
          (Mismatch (byte_mismatch ~section_name ~expected ~actual byte_offset))
      )

let compare_sections ~comparison_mode ~expected ~actual =
  let actual_map = Hashtbl.create (List.length actual) in
  List.iter (fun (name, content) -> Hashtbl.add actual_map name content) actual;
  let expected_map = Hashtbl.create (List.length expected) in
  List.iter (fun (name, _) -> Hashtbl.add expected_map name ()) expected;
  (* Check all expected sections exist in actual and match *)
  let rec check_expected = function
    | [] -> None
    | (name, expected) :: rest -> (
      match Hashtbl.find_opt actual_map name with
      | None -> Some (Mismatch (Missing_section (name ^ " (in object file)")))
      | Some actual -> (
        match
          compare_section ~comparison_mode ~section_name:name ~expected ~actual
        with
        | Some _ as r -> r
        | None -> check_expected rest))
  in
  match check_expected expected with
  | Some _ as r -> r
  | None -> (
    (* Check for sections in actual that are missing from expected *)
    let extra_in_actual =
      List.find_opt
        (fun (name, _) -> not (Hashtbl.mem expected_map name))
        actual
    in
    match extra_in_actual with
    | Some (name, _) ->
      Some (Mismatch (Missing_section (name ^ " (in binary emitter output)")))
    | None -> None)

(* Relocation comparison *)

let format_reloc (r : relocation) =
  if r.r_addend = 0L
  then r.r_symbol
  else Printf.sprintf "%s+0x%Lx" r.r_symbol r.r_addend

let format_reloc_list relocs = String.concat ", " (List.map format_reloc relocs)

let group_by_offset relocs =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (r : relocation) ->
      let existing = try Hashtbl.find tbl r.r_offset with Not_found -> [] in
      Hashtbl.replace tbl r.r_offset (r :: existing))
    relocs;
  let compare_reloc (a : relocation) (b : relocation) =
    let c = String.compare a.r_symbol b.r_symbol in
    if c <> 0 then c else Int64.compare a.r_addend b.r_addend
  in
  Hashtbl.fold (fun offset rs acc -> (offset, rs) :: acc) tbl []
  |> List.sort (fun (o1, _) (o2, _) -> compare o1 o2)
  |> List.map (fun (offset, rs) -> offset, List.sort compare_reloc rs)

let compare_relocations ~section_name ~expected ~actual =
  let exp_grouped = group_by_offset expected in
  let act_grouped = group_by_offset actual in
  let rec loop exp act =
    match exp, act with
    | [], [] -> None
    | [], (offset, relocs) :: _ ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = "(none)";
             actual = format_reloc_list relocs
           })
    | (offset, relocs) :: _, [] ->
      Some
        (Relocation
           { section_name;
             offset;
             expected = format_reloc_list relocs;
             actual = "(none)"
           })
    | (e_off, e_relocs) :: erest, (a_off, a_relocs) :: arest ->
      if e_off <> a_off
      then
        Some
          (Relocation
             { section_name;
               offset = min e_off a_off;
               expected =
                 Printf.sprintf "%s @ 0x%x" (format_reloc_list e_relocs) e_off;
               actual =
                 Printf.sprintf "%s @ 0x%x" (format_reloc_list a_relocs) a_off
             })
      else if not (List.equal Owee_object.relocs_equal e_relocs a_relocs)
      then
        Some
          (Relocation
             { section_name;
               offset = e_off;
               expected = format_reloc_list e_relocs;
               actual = format_reloc_list a_relocs
             })
      else loop erest arest
  in
  loop exp_grouped act_grouped

let compare_section_relocations ~expected ~actual =
  let actual_map = Hashtbl.create (List.length actual) in
  List.iter (fun (name, relocs) -> Hashtbl.add actual_map name relocs) actual;
  let expected_map = Hashtbl.create (List.length expected) in
  List.iter (fun (name, _) -> Hashtbl.add expected_map name ()) expected;
  (* Check all expected section relocations *)
  let rec check_expected = function
    | [] -> None
    | (name, expected) :: rest -> (
      let actual =
        Option.value (Hashtbl.find_opt actual_map name) ~default:[]
      in
      match compare_relocations ~section_name:name ~expected ~actual with
      | Some mismatch -> Some mismatch
      | None -> check_expected rest)
  in
  match check_expected expected with
  | Some _ as mismatch -> mismatch
  | None -> (
    (* Check for sections with relocations in actual but not in expected *)
    let extra_in_actual =
      List.find_opt
        (fun (name, relocs) ->
          relocs <> [] && not (Hashtbl.mem expected_map name))
        actual
    in
    match extra_in_actual with
    | Some (name, _) ->
      Some
        (Relocation
           { section_name = name;
             offset = 0;
             expected = "(no relocations expected for this section)";
             actual = "(has relocations)"
           })
    | None -> None)

(* Main comparison *)

let compare ~comparison_mode unix ~obj_file ~binary_sections_dir =
  if not (Sys.file_exists binary_sections_dir)
  then Mismatch (Missing_binary_sections_dir binary_sections_dir)
  else if not (Sys.is_directory binary_sections_dir)
  then Object_file_error (binary_sections_dir ^ " is not a directory")
  else
    let buf =
      try Owee_buf.map_binary unix obj_file
      with exn ->
        Misc.fatal_errorf "Failed to read %s: %s" obj_file
          (Printexc.to_string exn)
    in
    let be_text = read_text_sections binary_sections_dir in
    let be_data = read_section binary_sections_dir "data" in
    let asm_data = Owee_object.extract_data_section buf in
    (* Normalize text sections to (name, content) list *)
    let be_text_list, asm_text_list =
      match be_text with
      | Function_sections sections ->
        sections, Owee_object.extract_individual_text_sections buf
      | No_function_sections be_opt -> (
        let asm_opt = Owee_object.extract_text_section buf in
        ( (match be_opt with Some s -> [".text", s] | None -> []),
          match asm_opt with Some s -> [".text", s] | None -> [] ))
    in
    (* Compare text sections *)
    let text_result =
      compare_sections ~comparison_mode ~expected:be_text_list
        ~actual:asm_text_list
    in
    match text_result with
    | Some r -> r
    | None -> (
      (* Compare data section *)
      let data_result =
        match be_data, asm_data with
        | Some expected, Some actual ->
          compare_section ~comparison_mode ~section_name:"data" ~expected
            ~actual
        | None, None -> None
        | Some _, None ->
          Some (Mismatch (Missing_section "data (in object file)"))
        | None, Some _ ->
          Some (Mismatch (Missing_section "data (in binary emitter output)"))
      in
      match data_result with
      | Some r -> r
      | None -> (
        let text_size =
          List.fold_left
            (fun acc (_, c) -> acc + String.length c)
            0 be_text_list
        in
        let data_size = Option.fold ~none:0 ~some:String.length be_data in
        match comparison_mode with
        | Disassembly ->
          (* Disassembly mode is instruction-stream-only for text sections. It
             intentionally skips relocation checks, including data relocations,
             because relocation differences are not represented in raw-binary
             objdump output. *)
          Match { text_size; data_size }
        | Exact -> (
          (* Normalize text relocations to (name, relocs) list *)
          let be_text_relocs, asm_text_relocs =
            match be_text with
            | Function_sections _ ->
              ( read_text_relocations binary_sections_dir ~include_main_text:true,
                Owee_object.extract_individual_text_relocations buf )
            | No_function_sections _ ->
              ( [".text", read_relocations binary_sections_dir "text"],
                [".text", Owee_object.extract_text_relocations buf] )
          in
          (* Compare text relocations *)
          let text_reloc_result =
            compare_section_relocations ~expected:be_text_relocs
              ~actual:asm_text_relocs
          in
          match text_reloc_result with
          | Some m -> Mismatch m
          | None -> (
            (* Compare data relocations *)
            let be_data_relocs =
              [".data", read_relocations binary_sections_dir "data"]
            in
            let asm_data_relocs =
              [".data", Owee_object.extract_data_relocations buf]
            in
            match
              compare_section_relocations ~expected:be_data_relocs
                ~actual:asm_data_relocs
            with
            | Some m -> Mismatch m
            | None -> Match { text_size; data_size }))))

let print_result ppf = function
  | Match { text_size; data_size } ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification passed@,\
       text: %d bytes, data: %d bytes@]@."
      text_size data_size
  | Mismatch (Section_content m) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Assembler size:      %d bytes@,\
       Binary emitter size: %d bytes@,\
       @,\
       First difference at byte offset 0x%x%s:@,\
       @,\
       Assembler bytes:@,\
      \  %s@,\
       @,\
       Binary emitter bytes:@,\
      \  %s@]@."
      m.section_name m.actual_size m.expected_size m.byte_offset
      (match m.instruction_offset with
      | Some off when off <> m.byte_offset ->
        Printf.sprintf " (instruction at 0x%x)" off
      | _ -> "")
      m.actual m.expected
  | Mismatch (Section_size { section_name; expected; actual }) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Size mismatch: assembler produced %d bytes, binary emitter produced %d \
       bytes@]@."
      section_name actual expected
  | Mismatch (Relocation r) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Relocation mismatch in %s at offset 0x%x:@,\
       Assembler:      %s@,\
       Binary emitter: %s@]@."
      r.section_name r.offset r.actual r.expected
  | Mismatch (Instruction m) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Instruction #%d:@,\
      \  Assembler:      %s@,\
      \  Binary emitter: %s@]@."
      m.section_name m.index m.assembler m.binary_emitter
  | Mismatch (Instruction_count m) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Section: %s@,\
       Instruction count mismatch:@,\
      \  Assembler:      %d instructions@,\
      \  Binary emitter: %d instructions@]@."
      m.section_name m.assembler m.binary_emitter
  | Mismatch (Missing_section name) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,Missing section: %s@]@." name
  | Mismatch (Missing_binary_sections_dir dir) ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Binary sections directory not found: %s@,\
       (Did the binary emitter run?)@]@."
      dir
  | Object_file_error msg ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,\
       @,\
       Error reading object file: %s@]@."
      msg
  | Error msg ->
    Format.fprintf ppf
      "@[<v>Binary emitter verification FAILED@,@,Error: %s@]@." msg

module For_testing = struct
  let extract_text_sections buf =
    match Owee_object.extract_individual_text_sections buf with
    | [] -> (
      match Owee_object.extract_text_section buf with
      | Some s -> [".text", s]
      | None -> [])
    | sections -> sections

  let extract_text_relocs buf =
    match Owee_object.extract_individual_text_relocations buf with
    | [] -> [".text", Owee_object.extract_text_relocations buf]
    | relocs -> relocs

  let compare_object_files unix ~expected_pathname ~actual_pathname =
    let map_file pathname =
      try Owee_buf.map_binary unix pathname
      with exn ->
        raise
          (Failure
             (Printf.sprintf "Failed to read %s: %s" pathname
                (Printexc.to_string exn)))
    in
    let expected_buf = map_file expected_pathname in
    let actual_buf = map_file actual_pathname in
    let expected_text_list = extract_text_sections expected_buf in
    let actual_text_list = extract_text_sections actual_buf in
    let expected_data = Owee_object.extract_data_section expected_buf in
    let actual_data = Owee_object.extract_data_section actual_buf in
    let expected_text_relocs = extract_text_relocs expected_buf in
    let actual_text_relocs = extract_text_relocs actual_buf in
    (* Compare text sections *)
    match
      compare_sections ~comparison_mode:Exact ~expected:expected_text_list
        ~actual:actual_text_list
    with
    | Some r -> r
    | None -> (
      (* Compare data section *)
      let data_result =
        match expected_data, actual_data with
        | Some expected, Some actual ->
          compare_section ~comparison_mode:Exact ~section_name:"data" ~expected
            ~actual
        | None, None -> None
        | Some _, None -> Some (Mismatch (Missing_section "data (in actual)"))
        | None, Some _ -> Some (Mismatch (Missing_section "data (in expected)"))
      in
      match data_result with
      | Some r -> r
      | None -> (
        match
          compare_section_relocations ~expected:expected_text_relocs
            ~actual:actual_text_relocs
        with
        | Some m -> Mismatch m
        | None -> (
          let expected_data_relocs =
            [".data", Owee_object.extract_data_relocations expected_buf]
          in
          let actual_data_relocs =
            [".data", Owee_object.extract_data_relocations actual_buf]
          in
          match
            compare_section_relocations ~expected:expected_data_relocs
              ~actual:actual_data_relocs
          with
          | Some m -> Mismatch m
          | None ->
            let text_size =
              List.fold_left
                (fun acc (_, c) -> acc + String.length c)
                0 expected_text_list
            in
            let data_size =
              Option.fold ~none:0 ~some:String.length expected_data
            in
            Match { text_size; data_size })))
end
