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

(* Build libraries of .cmjx files *)

open Misc
open Config
open Cmx_format

type error = File_not_found of string | Archiver_error of string

exception Error of error

let read_info name =
  let filename =
    try Load_path.find name
    with Not_found -> raise (Error (File_not_found name))
  in
  let info, crc = Compilenv.read_unit_info filename in
  info.ui_force_link <- info.ui_force_link || !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmjxa file,
     since the compiler will go looking directly for .cmjx files.
     The linker, which is the only one that reads .cmjxa files, does not
     need the approximation. *)
  info.ui_export_info <- None;
  (Filename.chop_suffix filename ".cmjx" ^ ".cmjo", (info, crc))

let create_archive file_list lib_name =
  if not (Filename.check_suffix lib_name ".cmjxa") then
    Location.prerr_warning Location.none
      (Warnings.Preprocessor
         (Printf.sprintf "library output should have .cmjxa extension, got: %s"
            lib_name));
  let archive_name = Filename.remove_extension lib_name ^ ".cmja" in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () ->
      remove_file lib_name;
      remove_file archive_name)
    (fun () ->
      output_string outchan cmxa_magic_number;
      let objfile_list, descr_list =
        List.split (List.map read_info file_list)
      in
      List.iter2
        (fun file_name (unit, crc) ->
          Jslink.check_consistency file_name unit crc)
        file_list descr_list;
      let cmis = Jslink.extract_crc_interfaces () in
      let cmxs = Jslink.extract_crc_implementations () in
      (* CR mshinwell: see comment in compilenv.ml
         let cmxs =
           Compilenv.ensure_sharing_between_cmi_and_cmx_imports cmis cmxs
         in
      *)
      let cmis = Array.of_list cmis in
      let cmxs = Array.of_list cmxs in
      let cmi_index = Compilation_unit.Name.Tbl.create 42 in
      Array.iteri
        (fun i import ->
          Compilation_unit.Name.Tbl.add cmi_index (Import_info.name import) i)
        cmis;
      let cmx_index = Compilation_unit.Tbl.create 42 in
      Array.iteri
        (fun i import ->
          Compilation_unit.Tbl.add cmx_index (Import_info.cu import) i)
        cmxs;
      let genfns = Generic_fns.Tbl.make () in
      let mk_bitmap arr ix entries ~find ~get_name =
        let module B = Misc.Bitmap in
        let b = B.make (Array.length arr) in
        List.iter (fun import -> B.set b (find ix (get_name import))) entries;
        b
      in
      let units =
        List.map
          (fun (unit, crc) ->
            ignore
              (Generic_fns.Tbl.add ~imports:Generic_fns.Partition.Set.empty
                 genfns unit.ui_generic_fns);
            {
              li_name = unit.ui_unit;
              li_crc = crc;
              li_defines = unit.ui_defines;
              li_force_link = unit.ui_force_link;
              li_imports_cmi =
                mk_bitmap cmis cmi_index unit.ui_imports_cmi
                  ~find:Compilation_unit.Name.Tbl.find
                  ~get_name:Import_info.name;
              li_imports_cmx =
                mk_bitmap cmxs cmx_index unit.ui_imports_cmx
                  ~find:Compilation_unit.Tbl.find ~get_name:Import_info.cu;
              li_external_symbols = Array.of_list unit.ui_external_symbols;
            })
          descr_list
      in
      let infos =
        {
          lib_units = units;
          lib_imports_cmi = cmis;
          lib_imports_cmx = cmxs;
          lib_generic_fns = Generic_fns.Tbl.entries genfns;
          lib_ccobjs = !Clflags.ccobjs;
          lib_ccopts = !Clflags.all_ccopts;
        }
      in
      output_value outchan infos;
      (* For JavaScript, create the .cmja archive using js_of_ocaml *)
      if objfile_list <> [] then
        let linkall_flag =
          if !Clflags.link_everything then [ "--linkall" ] else []
        in
        let debug_flag = if !Clflags.debug then [ "--debug-info" ] else [] in
        Jscompile.run_jsoo_exn
          ~args:
            ([ "link"; "-a" ] @ linkall_flag @ debug_flag
           @ [ "-o"; archive_name ] @ objfile_list))

open Format

let report_error ppf = function
  | File_not_found name -> fprintf ppf "Cannot find file %s" name
  | Archiver_error name ->
      fprintf ppf "Error while creating the library %s" name

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let reset () = ()
