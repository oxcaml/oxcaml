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
open Cmx_format
open Compilenv
open Linkenv

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let read_file obj_name =
  let file_name =
    try Load_path.find obj_name
    with Not_found -> raise (Linkenv.Error (Linkenv.File_not_found obj_name))
  in
  if Filename.check_suffix file_name ".cmx"
  then
    (* This is a .cmx file. It must be linked in any case. Read the infos to see
       which modules it requires. *)
    let info, crc = read_unit_info file_name in
    Unit (file_name, info, crc)
  else if Filename.check_suffix file_name ".cmxa"
  then
    let infos =
      try read_library_info file_name
      with Compilenv.Error (Not_a_unit_info _) ->
        raise (Linkenv.Error (Linkenv.Not_an_object_file file_name))
    in
    Library (file_name, infos)
  else raise (Linkenv.Error (Linkenv.Not_an_object_file file_name))

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so no
     module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

let scan_file ~shared genfns file (objfiles, tolink, cached_genfns_imports) =
  match read_file file with
  | Unit (file_name, info, crc) ->
    (* This is a .cmx file. It must be linked in any case. *)
    remove_required info.ui_unit;
    List.iter
      (fun import -> add_required (file_name, None) import)
      info.ui_imports_cmx;
    let dynunit : Cmxs_format.dynunit option =
      if not shared
      then None
      else
        Some
          { dynu_name = info.ui_unit;
            dynu_crc = crc;
            dynu_defines = info.ui_defines;
            dynu_imports_cmi = info.ui_imports_cmi |> Array.of_list;
            dynu_imports_cmx = info.ui_imports_cmx |> Array.of_list
          }
    in
    let unit =
      { name = info.ui_unit;
        crc;
        defines = info.ui_defines;
        file_name;
        dynunit
      }
    in
    let object_file_name = Filename.chop_suffix file_name ".cmx" ^ ext_obj in
    check_consistency ~unit
      (Array.of_list info.ui_imports_cmi)
      (Array.of_list info.ui_imports_cmx);
    let cached_genfns_imports =
      Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
        info.ui_generic_fns
    in
    object_file_name :: objfiles, unit :: tolink, cached_genfns_imports
  | Library (file_name, infos) ->
    (* This is an archive file. Each unit contained in it will be linked in only
       if needed. *)
    add_ccobjs (Filename.dirname file_name) infos;
    let cached_genfns_imports =
      Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
        infos.lib_generic_fns
    in
    check_cmi_consistency file_name infos.lib_imports_cmi;
    check_cmx_consistency file_name infos.lib_imports_cmx;
    let objfiles =
      let obj_file = Filename.chop_suffix file_name ".cmxa" ^ ext_lib in
      (* MSVC doesn't support empty .lib files, and macOS struggles to make them
         (#6550), so there shouldn't be one if the .cmxa contains no units. The
         file_exists check is added to be ultra-defensive for the case where a
         user has manually added things to the .a/.lib file *)
      if infos.lib_units = [] && not (Sys.file_exists obj_file)
      then objfiles
      else obj_file :: objfiles
    in
    ( objfiles,
      List.fold_right
        (fun info reqd ->
          let li_name = CU.name info.li_name in
          if info.li_force_link || !Clflags.link_everything
             || is_required info.li_name
          then (
            remove_required info.li_name;
            let req_by = file_name, Some li_name in
            info.li_imports_cmx
            |> Misc.Bitmap.iter (fun i ->
                   let import = infos.lib_imports_cmx.(i) in
                   add_required req_by import);
            let imports_list tbl bits =
              List.init (Array.length tbl) (fun i ->
                  if Misc.Bitmap.get bits i then Some tbl.(i) else None)
              |> List.filter_map Fun.id
            in
            let dynunit : Cmxs_format.dynunit option =
              if not shared
              then None
              else
                Some
                  { dynu_name = info.li_name;
                    dynu_crc = info.li_crc;
                    dynu_defines = info.li_defines;
                    dynu_imports_cmi =
                      imports_list infos.lib_imports_cmi info.li_imports_cmi
                      |> Array.of_list;
                    dynu_imports_cmx =
                      imports_list infos.lib_imports_cmx info.li_imports_cmx
                      |> Array.of_list
                  }
            in
            let unit =
              { name = info.li_name;
                crc = info.li_crc;
                defines = info.li_defines;
                file_name;
                dynunit
              }
            in
            check_consistency ~unit [||] [||];
            unit :: reqd)
          else reqd)
        infos.lib_units tolink,
      cached_genfns_imports )

let make_globals_map units_list =
  (* The order in which entries appear in the globals map does not matter (see
     the natdynlink code). We can corrupt [interfaces] since it won't be used
     again until the next compilation. *)
  let find_crc name =
    Cmi_consistbl.find crc_interfaces name
    |> Option.map (fun (_unit, crc) -> crc)
  in
  let defined =
    List.map
      (fun unit ->
        let name = CU.name unit.name in
        let intf_crc = find_crc name in
        CU.Name.Tbl.remove interfaces name;
        let syms = List.map Symbol.for_compilation_unit unit.defines in
        unit.name, intf_crc, Some unit.crc, syms)
      units_list
  in
  CU.Name.Tbl.fold
    (fun name () globals_map ->
      let intf_crc = find_crc name in
      (assume_no_prefix name, intf_crc, None, []) :: globals_map)
    interfaces defined

(* Exported version for Asmlibrarian / Asmpackager *)
let check_consistency file_name u crc =
  let unit =
    { file_name; name = u.ui_unit; defines = u.ui_defines; crc; dynunit = None }
  in
  check_consistency ~unit
    (Array.of_list u.ui_imports_cmi)
    (Array.of_list u.ui_imports_cmx)
