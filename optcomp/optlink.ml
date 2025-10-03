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

(* Link a set of native/flambda2 object files and produce an executable *)

type filepath = string
open Format

module type S = sig
  val link: ppf_dump:formatter ->
    string list -> string -> unit

  val link_shared:
    ppf_dump:formatter -> string list -> string -> unit

  val link_partial :  string -> string list -> unit
end

module Make (Backend : Optbackend_intf.S) : S = struct
open Cmx_format
open Compilenv

let link_partial = Backend.link_partial

module String = Misc.Stdlib.String
module CU = Compilation_unit

type unit_link_info = Optbackend_intf.unit_link_info = {
  name: Compilation_unit.t;
  defines: Compilation_unit.t list;
  file_name: string;
  crc: Digest.t;
  (* for shared libs *)
  dynunit : Cmxs_format.dynunit option;
}

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

(* First pass: determine which units are needed *)
open Optlink_common

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name Backend.ext_flambda_obj then begin
    (* This is a cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name Backend.ext_flambda_lib then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file ~shared genfns file (objfiles, tolink, cached_genfns_imports) =
  match read_file file with
  | Unit (file_name,info,crc) ->
      (* This is a cmx file. It must be linked in any case. *)
      remove_required info.ui_unit;
      List.iter (fun import ->
          add_required (file_name, None) import)
        info.ui_imports_cmx;
      let dynunit : Cmxs_format.dynunit option =
        if not shared then None else
          Some { dynu_name = info.ui_unit;
                 dynu_crc = crc;
                 dynu_defines = info.ui_defines;
                 dynu_imports_cmi = info.ui_imports_cmi |> Array.of_list;
                 dynu_imports_cmx = info.ui_imports_cmx |> Array.of_list }
      in
      let unit =
        { name = info.ui_unit;
          crc;
          defines = info.ui_defines;
          file_name;
          dynunit }
      in
      let object_file_name =
        Filename.chop_suffix file_name Backend.ext_flambda_obj
        ^ Backend.ext_obj
      in
      check_consistency' ~unit
        (Array.of_list info.ui_imports_cmi)
        (Array.of_list info.ui_imports_cmx);
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns info.ui_generic_fns
      in
      object_file_name :: objfiles, unit :: tolink, cached_genfns_imports
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports  genfns infos.lib_generic_fns
      in
      check_cmi_consistency file_name infos.lib_imports_cmi;
      check_cmx_consistency file_name infos.lib_imports_cmx;
      let objfiles =
        let obj_file =
          Filename.chop_suffix file_name Backend.ext_flambda_lib
          ^ Backend.ext_lib
        in
        (* MSVC doesn't support empty .lib files, and macOS struggles to
           make them (#6550), so there shouldn't be one if the cmxa
           contains no units. The file_exists check is added to be
           ultra-defensive for the case where a user has manually added
           things to the .a/.lib file *)
        if infos.lib_units = [] && not (Sys.file_exists obj_file)
        then objfiles
        else obj_file :: objfiles
      in
      objfiles,
      List.fold_right
        (fun info reqd ->
           let li_name = CU.name info.li_name in
           if info.li_force_link
           || !Clflags.link_everything
           || is_required info.li_name
           then begin
             remove_required info.li_name;
             let req_by = (file_name, Some li_name) in
             info.li_imports_cmx |> Misc.Bitmap.iter (fun i ->
               let import = infos.lib_imports_cmx.(i) in
               add_required req_by import);
             let imports_list tbl bits =
               List.init (Array.length tbl) (fun i ->
                 if Misc.Bitmap.get bits i then Some tbl.(i) else None)
               |> List.filter_map Fun.id
             in
             let dynunit : Cmxs_format.dynunit option =
               if not shared then None else
                 Some {
                   dynu_name = info.li_name;
                   dynu_crc = info.li_crc;
                   dynu_defines = info.li_defines;
                   dynu_imports_cmi =
                     imports_list infos.lib_imports_cmi info.li_imports_cmi
                     |> Array.of_list;
                   dynu_imports_cmx =
                     imports_list infos.lib_imports_cmx info.li_imports_cmx
                     |> Array.of_list }
             in
             let unit =
               { name = info.li_name;
                 crc = info.li_crc;
                 defines = info.li_defines;
                 file_name;
                 dynunit }
             in
             check_consistency' ~unit [| |] [| |];
             unit :: reqd
           end else
           reqd)
        infos.lib_units tolink, cached_genfns_imports

(* Second pass: generate the startup file and link it with everything else *)

let named_startup_file () =
  !Clflags.keep_startup_file || !Emitaux.binary_backend_available

let make_globals_map units_list =
  (* The order in which entries appear in the globals map does not matter
     (see the natdynlink code).
     We can corrupt [interfaces] since it won't be used again until the next
     compilation. *)
  let find_crc name =
    Cmi_consistbl.find crc_interfaces name
    |> Option.map (fun (_unit, crc) -> crc)
  in
  let defined =
    List.map (fun unit ->
        let name = CU.name unit.name in
        let intf_crc = find_crc name in
        CU.Name.Tbl.remove interfaces name;
        let syms = List.map Symbol.for_compilation_unit unit.defines in
        (unit.name, intf_crc, Some unit.crc, syms))
      units_list
  in
  CU.Name.Tbl.fold (fun name () globals_map ->
      let intf_crc = find_crc name in
      (assume_no_prefix name, intf_crc, None, []) :: globals_map)
    interfaces
    defined

(* The compiler allows [-o /dev/null], which can be used for testing linking.
   In this case, we should not use the DWARF fission workflow during linking. *)
let not_output_to_dev_null output_name =
  not (String.equal output_name "/dev/null")

let link_shared ~ppf_dump objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
    let genfns = Generic_fns.Tbl.make () in
    let ml_objfiles, units_to_link, _ =
      List.fold_right
        (scan_file ~shared:true genfns)
        objfiles
        ([],[], Generic_fns.Partition.Set.empty)
    in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    Backend.link_shared
      ml_objfiles
      output_name
      ~ppf_dump
      ~genfns
      ~units_to_link)

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  CU.Tbl.reset implementations_defined;
  cmx_required := [];
  CU.Name.Tbl.reset interfaces;
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []

(* Main entry point *)

let link ~ppf_dump objfiles output_name =
  let shared = false in
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
    let stdlib = "stdlib" ^ Backend.ext_flambda_lib in
    let stdexit = "std_exit" ^ Backend.ext_flambda_obj in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let genfns = Generic_fns.Tbl.make () in
    let ml_objfiles, units_to_link, cached_genfns_imports =
      List.fold_right
        (scan_file ~shared:false genfns)
        objfiles
        ([],[], Generic_fns.Partition.Set.empty)
    in
    if not shared then
      (match extract_missing_globals() with
      | [] -> ()
      | mg -> raise(Error(Missing_implementations mg)));
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    (* put user's opts first *)
    Backend.link
      ml_objfiles
      output_name
      ~ppf_dump
      ~genfns
      ~units_to_link
      ~cached_genfns_imports
  )
end
