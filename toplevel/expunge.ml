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

(* "Expunge" a toplevel by removing compiler modules from the global map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Misc

let to_keep = ref Compilation_unit.Set.empty

let negate = Sys.argv.(3) = "-v"

let keep0 cu =
  if negate then not (Compilation_unit.Set.mem cu !to_keep)
  else (Compilation_unit.Set.mem cu !to_keep)

let keep = function
  | Symtable.Global.Glob_predef _ -> true
  | Symtable.Global.Glob_compunit cu -> keep0 cu

let expunge_map tbl =
  Symtable.filter_global_map keep tbl

let expunge_crcs tbl =
  Array.to_list tbl
  |> List.filter (fun import ->
      match Import_info.Intf.view import with
      | Normal (cu, _) | Alias cu -> keep0 cu
      | Parameter (intf, _) ->
        (* Compare parameters by name against the units to keep *)
        let name = Compilation_unit.Intf.to_name intf in
        let mem =
          Compilation_unit.Set.exists
            (fun cu -> Compilation_unit.Name.equal (Compilation_unit.name cu) name)
            !to_keep
        in
        if negate then not mem else mem)
  |> Array.of_list

let main () =
  let input_name = Sys.argv.(1) in
  let output_name = Sys.argv.(2) in
  for i = (if negate then 4 else 3) to Array.length Sys.argv - 1 do
    let modname = Unit_info.modulize Sys.argv.(i) in
    let cu = Compilation_unit.of_string modname in
    to_keep := Compilation_unit.Set.add cu !to_keep
  done;
  let ic = open_in_bin input_name in
  let toc = Bytesections.read_toc ic in
  seek_in ic 0;
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
      output_name in
  let first_pos = Bytesections.pos_first_section toc in
  (* Copy the file up to the first section as is *)
  copy_file_chunk ic oc first_pos;
  (* Copy each section, modifying the symbol section in passing *)
  let toc_writer = Bytesections.init_record oc in
  List.iter
    (fun {Bytesections.name; pos; len} ->
      seek_in ic pos;
      begin match name with
        SYMB ->
          let global_map : Symtable.global_map = input_value ic in
          output_value oc (expunge_map global_map)
      | CRCS ->
          let crcs : Import_info.t array = input_value ic in
          output_value oc (expunge_crcs crcs)
      | _ ->
          copy_file_chunk ic oc len
      end;
      Bytesections.record toc_writer name)
    (Bytesections.all toc);
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer toc_writer;
  (* Done *)
  close_in ic;
  close_out oc

let _ = main (); exit 0
