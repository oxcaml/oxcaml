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

module String = Misc.Stdlib.String

type error =
  | File_not_found of string
  | Duplicate_file of string

exception Error of error

let report_error ppf = function
  | File_not_found filename ->
    Format.fprintf ppf "Dissector: file not found: %s" filename
  | Duplicate_file filename ->
    Format.fprintf ppf "Dissector: duplicate file in link: %s" filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

type file_origin =
  | OCaml
  | Startup
  | Cached_genfns

let string_of_origin = function
  | OCaml -> "OCaml"
  | Startup -> "Startup"
  | Cached_genfns -> "Cached_genfns"

(* Analyze a single ELF buffer, returning (size, has_probes) *)
let analyze_elf_buf buf =
  let _header, sections = Compiler_owee.Owee_elf.read_elf buf in
  let size, has_probes =
    Array.fold_left
      (fun (acc_size, acc_probes) (section : Compiler_owee.Owee_elf.section) ->
        let new_size =
          if Compiler_owee.Owee_elf.Section_flags.(
               is_alloc (of_u64 section.sh_flags))
          then Int64.add acc_size section.sh_size
          else acc_size
        in
        let new_probes =
          acc_probes || String.equal section.sh_name_str ".probes"
        in
        new_size, new_probes)
      (0L, false) sections
  in
  size, has_probes

(* Check if a string is a linker option (starts with '-') rather than a file *)
let is_linker_option s = String.length s > 0 && String.get s 0 = '-'

(* Check for duplicate files in the input list, ignoring linker options *)
let check_for_duplicates files =
  let seen = String.Tbl.create 256 in
  List.iter
    (fun file ->
      if is_linker_option file
      then ()
      else if String.Tbl.mem seen file
      then raise (Error (Duplicate_file file))
      else String.Tbl.add seen file ())
    files

module File_size = struct
  type t =
    { filename : string;
      size : int64;
      has_probes : bool;
      origin : file_origin
    }

  let filename t = t.filename

  let size t = t.size

  let has_probes t = t.has_probes

  let origin t = t.origin
end

let measure_files (unix : (module Compiler_owee.Unix_intf.S)) ~files =
  (* Check for duplicates in the input list first (extract just filenames) *)
  check_for_duplicates (List.map fst files);
  let module Unix = (val unix) in
  (* Open output file if -ddissector-inputs is set *)
  let out_channel = Option.map open_out !Clflags.ddissector_inputs in
  let log fmt =
    match out_channel with
    | None -> Printf.ifprintf stderr fmt
    | Some oc -> Printf.fprintf oc fmt
  in
  (* Analyze a single .o file, returning (size, has_probes) *)
  let analyze_object_file filename =
    if not (Sys.file_exists filename)
    then raise (Error (File_not_found filename))
    else
      let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
      analyze_elf_buf buf
  in
  (* Analyze an archive (.a) file, returning (size, has_probes, member_names) *)
  let analyze_archive_file filename =
    if not (Sys.file_exists filename)
    then raise (Error (File_not_found filename))
    else
      let buf = Compiler_owee.Owee_buf.map_binary (module Unix) filename in
      let archive, members = Compiler_owee.Owee_archive.read buf in
      let size, has_probes, member_names =
        List.fold_left
          (fun (acc_size, acc_probes, acc_names) member ->
            let name = member.Compiler_owee.Owee_archive.name in
            if Filename.check_suffix name ".o"
            then
              let member_buf =
                Compiler_owee.Owee_archive.member_body archive member
              in
              let size, has_probes = analyze_elf_buf member_buf in
              ( Int64.add acc_size size,
                acc_probes || has_probes,
                name :: acc_names )
            else acc_size, acc_probes, acc_names)
          (0L, false, []) members
      in
      size, has_probes, List.rev member_names
  in
  (* Track which files we've already analyzed to avoid double-counting *)
  let analyzed = String.Tbl.create 256 in
  (* Analyze a single file based on its extension, return list of file_size
     records. Linker options (starting with '-') are ignored. The origin is
     propagated to the resulting entries. *)
  let analyze_one ~indent (filename, origin) =
    if is_linker_option filename
    then (
      log "%sInput: %s (%s) [linker option, skipped]\n" indent filename
        (string_of_origin origin);
      [])
    else if String.Tbl.mem analyzed filename
    then (
      log "%sInput: %s (%s) [already analyzed, skipped]\n" indent filename
        (string_of_origin origin);
      [])
    else (
      String.Tbl.add analyzed filename ();
      if Filename.check_suffix filename ".o"
      then (
        let size, has_probes = analyze_object_file filename in
        log "%sInput: %s (%s)\n" indent filename (string_of_origin origin);
        log "%s  -> %s (%Ld bytes, has_probes=%b)\n" indent filename size
          has_probes;
        [{ File_size.filename; size; has_probes; origin }])
      else if Filename.check_suffix filename ".a"
      then (
        let size, has_probes, member_names = analyze_archive_file filename in
        log "%sInput: %s (%s)\n" indent filename (string_of_origin origin);
        log "%s  -> %s (%Ld bytes, has_probes=%b)\n" indent filename size
          has_probes;
        log "%s  Members: %s\n" indent (String.concat ", " member_names);
        [{ File_size.filename; size; has_probes; origin }])
      else (
        log "%sInput: %s (%s) [unknown extension, skipped]\n" indent filename
          (string_of_origin origin);
        []))
  in
  let result = List.concat_map (analyze_one ~indent:"") files in
  Option.iter close_out out_channel;
  (* Log summary if -ddissector is enabled *)
  if !Clflags.ddissector
  then (
    let count_by_origin = Hashtbl.create 8 in
    List.iter
      (fun entry ->
        let origin = File_size.origin entry in
        let count =
          Hashtbl.find_opt count_by_origin origin |> Option.value ~default:0
        in
        Hashtbl.replace count_by_origin origin (count + 1))
      result;
    Printf.eprintf "Dissector: measured %d file(s) by origin:\n"
      (List.length result);
    Hashtbl.iter
      (fun origin count ->
        Printf.eprintf "  %s: %d\n" (string_of_origin origin) count)
      count_by_origin);
  result
