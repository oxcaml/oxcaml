(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Config
open Cmj_format

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

let copy_compunit ic oc compunit =
  seek_in ic compunit.cu_pos;
  let new_pos = pos_out oc in
  copy_file_chunk ic oc compunit.cu_codesize;
  { compunit with cu_pos = new_pos }

let lib_ccobjs = ref []
let lib_ccopts = ref []
let lib_dllibs = ref []

let add_ccobjs l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := !lib_ccobjs @ l.lib_ccobjs;
    lib_ccopts := !lib_ccopts @ l.lib_ccopts;
    lib_dllibs := !lib_dllibs @ l.lib_dllibs
  end

let copy_object_file oc name =
  let file_name =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = really_input_string ic (String.length cmj_magic_number) in
    if buffer = cmj_magic_number then begin
      let compunit_pos = pos_out oc in
      seek_in ic 0;
      let file_size = in_channel_length ic in
      seek_in ic (String.length cmj_magic_number);
      let compunit_size = file_size - (String.length cmj_magic_number) in
      copy_file_chunk ic oc compunit_size;
      close_in ic;
      let cu_name = 
        Compilation_unit.create Compilation_unit.Prefix.empty 
          (Compilation_unit.Name.of_string (Filename.remove_extension (Filename.basename name)))
      in
      let compunit =
        { cu_name = cu_name;
          cu_pos = compunit_pos;
          cu_codesize = compunit_size;
          cu_imports = [||] }
      in
      [compunit]
    end else
    if buffer = cmja_magic_number then begin
      let toc_pos = input_binary_int ic in
      seek_in ic toc_pos;
      let toc = (input_value ic : library) in
      add_ccobjs toc;
      let units = List.map (copy_compunit ic oc) toc.lib_units in
      close_in ic;
      units
    end else
      raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

let create_archive file_list lib_name =
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name)
    (fun () ->
       output_string outchan cmja_magic_number;
       let ofs_pos_toc = pos_out outchan in
       output_binary_int outchan 0;
       let units =
         List.flatten(List.map (copy_object_file outchan) file_list) in
       let toc =
         { lib_units = units;
           lib_ccobjs = !Clflags.ccobjs @ !lib_ccobjs;
           lib_ccopts = !Clflags.all_ccopts @ !lib_ccopts;
           lib_dllibs = !Clflags.dllibs @ !lib_dllibs } in
       let pos_toc = pos_out outchan in
       output_value outchan toc;
       seek_out outchan ofs_pos_toc;
       output_binary_int outchan pos_toc
    )

open Format
module Style = Misc.Style

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a JavaScript IR object file"
        (Style.as_inline_code Location.print_filename) name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := []