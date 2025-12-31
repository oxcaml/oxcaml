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

(* Test for Owee_archive: reads an archive file and reports section sizes. *)

let () =
  if Array.length Sys.argv < 2
  then (
    Printf.eprintf "Usage: %s <archive.a>\n" Sys.argv.(0);
    exit 1)

let archive_path = Sys.argv.(1)

let () =
  (* Memory-map the archive *)
  let fd = Unix.openfile archive_path [Unix.O_RDONLY] 0 in
  let len = (Unix.fstat fd).Unix.st_size in
  let buf =
    Unix.map_file fd Bigarray.Int8_unsigned Bigarray.c_layout false [| len |]
    |> Bigarray.array1_of_genarray
  in
  Unix.close fd;
  (* Read the archive *)
  let archive, members = Compiler_owee.Owee_archive.read buf in
  Printf.printf "Archive: %s\n" archive_path;
  Printf.printf "Number of members: %d\n\n" (List.length members);
  (* Process each member *)
  List.iter
    (fun (member : Compiler_owee.Owee_archive.member) ->
      Printf.printf "Member: %s (size=%d)\n" member.name member.size;
      (* Get the member body and try to parse as ELF *)
      let member_buf = Compiler_owee.Owee_archive.member_body archive member in
      try
        let _header, sections = Compiler_owee.Owee_elf.read_elf member_buf in
        (* Report sizes of interesting sections *)
        Array.iter
          (fun (section : Compiler_owee.Owee_elf.section) ->
            let name = section.sh_name_str in
            (* Truncate long section names for consistency across systems. E.g.,
               ".note.gnu.property" vs ".note.gnu.pr[...]" *)
            let name =
              if String.length name > 15 then String.sub name 0 15 else name
            in
            (* Only report non-empty sections with common names *)
            if section.sh_size > 0L && String.starts_with ~prefix:"." name
            then Printf.printf "  %s: size=%Ld\n" name section.sh_size)
          sections;
        print_newline ()
      with Compiler_owee.Owee_buf.Invalid_format msg ->
        Printf.printf "  (not ELF: %s)\n\n" msg)
    members
