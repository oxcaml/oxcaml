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

(* See owee_archive.mli for documentation. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open Owee_buf

(* Archive format constants *)
let archive_magic = "!<arch>\n"

let archive_magic_size = 8

let header_size = 60

let header_terminator = "`\n"

type t = Owee_buf.t

(* Archive member header field sizes (in bytes). The ar format stores these as
   fixed-width ASCII strings in this order. *)
let ar_name_size = 16

let ar_date_size = 12

let ar_uid_size = 6

let ar_gid_size = 6

let ar_mode_size = 8

let ar_size_size = 10

let ar_fmag_size = 2

(* Parsed archive member. Field order matches header field order: name, date
   (mtime), uid, gid, mode, size. The fmag field is not stored (it's just the
   header terminator "`\n"). data_offset is computed from the header position.

   The numeric fields are decimal except mode which is octal. The maximum values
   that fit in these fields (e.g., 999999 for 6-digit uid/gid) fit in OCaml's
   31-bit int on 32-bit platforms (max 1,073,741,823), so this code works on
   both 32-bit and 64-bit architectures. *)
type member =
  { name : string;
    mtime : int;
    uid : int;
    gid : int;
    mode : int;
    size : int;
    data_offset : int
  }

type member_header = member

(* Parse an integer from a fixed-width ASCII field, ignoring trailing spaces.
   Returns 0 if the field is empty or all spaces. The [convert] function
   transforms the trimmed string before parsing (e.g., adding "0o" for
   octal). *)
let parse_int_field ~convert ~field_type s =
  let s = String.trim s in
  if String.length s = 0
  then 0
  else
    match int_of_string_opt (convert s) with
    | Some n -> n
    | None -> invalid_formatf "Invalid %s field: %s" field_type s

let parse_decimal_field s =
  parse_int_field ~convert:Fun.id ~field_type:"decimal" s

let parse_octal_field s =
  parse_int_field ~convert:(fun s -> "0o" ^ s) ~field_type:"octal" s

(* Read the archive magic number and validate it. *)
let read_magic cursor =
  ensure cursor archive_magic_size "Archive too small for magic number";
  let magic = Read.fixed_string cursor archive_magic_size in
  if not (String.equal magic archive_magic)
  then invalid_format "Not an ar archive (missing \"!<arch>\\n\" magic number)"

(* Check if a name indicates a BSD-style symbol table *)
let is_bsd_symtab name =
  String.starts_with ~prefix:"__.SYMDE" (String.trim name)

(* Check if a name indicates a System V symbol table *)
let is_sysv_symtab name = String.equal (String.trim name) "/"

(* Check if a name indicates a System V string table *)
let is_sysv_strtab name = String.equal (String.trim name) "//"

(* Parse an extended name format that uses a prefix followed by a number. Used
   for both BSD "#1/N" and System V "/N" formats. *)
let parse_extended_name ~prefix ar_name =
  let trimmed = String.trim ar_name in
  if String.starts_with ~prefix trimmed
  then
    let prefix_len = String.length prefix in
    let num_str =
      String.sub trimmed prefix_len (String.length trimmed - prefix_len)
    in
    int_of_string_opt (String.trim num_str)
  else None

(* Check if a name uses BSD extended format "#1/N" *)
let parse_bsd_extended_name ar_name = parse_extended_name ~prefix:"#1/" ar_name

(* Check if a name uses System V extended format "/N" (offset into string
   table). Note: we can't use parse_extended_name here because "/" alone is the
   symbol table and "//" is the string table, so we need to check for a
   digit. *)
let parse_sysv_extended_name ar_name =
  let trimmed = String.trim ar_name in
  if String.length trimmed >= 2
     && Char.equal trimmed.[0] '/'
     && match trimmed.[1] with '0' .. '9' -> true | _ -> false
  then
    let offset_str = String.sub trimmed 1 (String.length trimmed - 1) in
    int_of_string_opt (String.trim offset_str)
  else None

(* Read a string from the System V string table at the given offset. Strings are
   terminated by '/' or '\n'. *)
let read_sysv_string_table_entry strtab offset =
  let strtab_cursor = Owee_buf.cursor strtab ~at:offset in
  let rec find_end i =
    if i >= Owee_buf.size strtab
    then i
    else
      let c = Bigarray.Array1.get strtab i in
      if c = Char.code '/' || c = Char.code '\n' || c = 0
      then i
      else find_end (i + 1)
  in
  let end_pos = find_end offset in
  let len = end_pos - offset in
  Read.fixed_string strtab_cursor len

(* Read a single member header. Returns None if at end of archive. For BSD
   extended names, the actual filename is read from the data section and
   data_offset/size are adjusted accordingly. *)
let read_member_header buf cursor ~string_table =
  if at_end cursor
  then None
  else (
    (* Ensure we're on an even byte boundary *)
    if cursor.position mod 2 = 1 then advance cursor 1;
    if at_end cursor
    then None
    else (
      ensure cursor header_size "Truncated member header";
      let header_start = cursor.position in
      let ar_name = Read.fixed_string cursor ar_name_size in
      let ar_date = Read.fixed_string cursor ar_date_size in
      let ar_uid = Read.fixed_string cursor ar_uid_size in
      let ar_gid = Read.fixed_string cursor ar_gid_size in
      let ar_mode = Read.fixed_string cursor ar_mode_size in
      let ar_size = Read.fixed_string cursor ar_size_size in
      let ar_fmag = Read.fixed_string cursor ar_fmag_size in
      if not (String.equal ar_fmag header_terminator)
      then invalid_formatf "Invalid header terminator at offset %d" header_start;
      let total_size = parse_decimal_field ar_size in
      let after_header = cursor.position in
      (* Determine the member name and actual data location/size *)
      let name, data_offset, size =
        match parse_bsd_extended_name ar_name with
        | Some name_len ->
          (* BSD extended filename: name is at start of data section *)
          let name_cursor = Owee_buf.cursor buf ~at:after_header in
          let name = Read.fixed_string name_cursor name_len in
          (* Strip trailing nulls from the name *)
          let name =
            match String.index_opt name '\000' with
            | Some i -> String.sub name 0 i
            | None -> name
          in
          name, after_header + name_len, total_size - name_len
        | None -> (
          match parse_sysv_extended_name ar_name with
          | Some offset ->
            (* System V extended filename: look up in string table *)
            let name =
              match string_table with
              | None ->
                invalid_formatf
                  "Extended filename at offset %d but no string table found"
                  header_start
              | Some strtab -> read_sysv_string_table_entry strtab offset
            in
            name, after_header, total_size
          | None ->
            (* Regular short filename *)
            let trimmed = String.trim ar_name in
            let name =
              (* Strip trailing '/' if present (System V style) *)
              if String.ends_with ~suffix:"/" trimmed
              then String.sub trimmed 0 (String.length trimmed - 1)
              else trimmed
            in
            name, after_header, total_size)
      in
      (* Skip past the member data for the next iteration *)
      advance cursor total_size;
      Some
        { name;
          mtime = parse_decimal_field ar_date;
          uid = parse_decimal_field ar_uid;
          gid = parse_decimal_field ar_gid;
          mode = parse_octal_field ar_mode;
          size;
          data_offset
        }))

(* Check if a member is a special entry that should be skipped *)
let is_special_member member =
  is_sysv_symtab member.name || is_sysv_strtab member.name
  || is_bsd_symtab member.name

let read buf =
  let cur = Owee_buf.cursor buf in
  read_magic cur;
  (* First pass: look for the System V string table "//" if present. BSD
     archives don't have a separate string table. *)
  let string_table = ref None in
  let first_pass_cursor = Owee_buf.cursor buf ~at:archive_magic_size in
  let rec find_string_table () =
    if at_end first_pass_cursor
    then ()
    else (
      if first_pass_cursor.position mod 2 = 1 then advance first_pass_cursor 1;
      if at_end first_pass_cursor
      then ()
      else (
        ensure first_pass_cursor header_size "Truncated header in first pass";
        let ar_name = Read.fixed_string first_pass_cursor ar_name_size in
        let (_ar_date : string) =
          Read.fixed_string first_pass_cursor ar_date_size
        in
        let (_ar_uid : string) =
          Read.fixed_string first_pass_cursor ar_uid_size
        in
        let (_ar_gid : string) =
          Read.fixed_string first_pass_cursor ar_gid_size
        in
        let (_ar_mode : string) =
          Read.fixed_string first_pass_cursor ar_mode_size
        in
        let ar_size = Read.fixed_string first_pass_cursor ar_size_size in
        let (_ar_fmag : string) =
          Read.fixed_string first_pass_cursor ar_fmag_size
        in
        let size = parse_decimal_field ar_size in
        let data_offset = first_pass_cursor.position in
        if is_sysv_strtab ar_name
        then
          (* Found the string table *)
          string_table := Some (Bigarray.Array1.sub buf data_offset size);
        advance first_pass_cursor size;
        (* Only continue searching if we haven't found it and we're still in the
           special entries at the start. Note: if is_sysv_strtab was true, we
           already set string_table above, so !string_table = None is false. *)
        if Option.is_none !string_table && is_sysv_symtab ar_name
        then find_string_table ()))
  in
  find_string_table ();
  (* Second pass: read all members *)
  let main_cursor = Owee_buf.cursor buf ~at:archive_magic_size in
  let rec read_members acc =
    match read_member_header buf main_cursor ~string_table:!string_table with
    | None -> List.rev acc
    | Some member ->
      (* Skip special entries in the returned list *)
      if is_special_member member
      then read_members acc
      else read_members (member :: acc)
  in
  let members = read_members [] in
  buf, members

let member_body buf member =
  Bigarray.Array1.sub buf member.data_offset member.size

let iter_members _archive members ~f = List.iter f members
