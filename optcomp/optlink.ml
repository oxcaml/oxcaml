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

open Format

module type S = sig
  val link : ppf_dump:formatter -> string list -> string -> unit

  val link_shared : ppf_dump:formatter -> string list -> string -> unit

  val link_partial : string -> string list -> unit

  val check_consistency :
    Linkenv.t -> string -> Cmx_format.unit_infos -> Digest.t -> unit
end

module Make (Backend : Optcomp_intf.Backend) : S = struct
  open Cmx_format
  open Compilenv

  let link_partial = Backend.link_partial

  module String = Misc.Stdlib.String
  module CU = Compilation_unit

  type unit_link_info = Linkenv.unit_link_info =
    { name : Compilation_unit.t;
      defines : Compilation_unit.t list;
      file_name : string;
      crc : Digest.t;
      (* for shared libs *)
      dynunit : Cmxs_format.dynunit option
    }

  (* First pass: determine which units are needed *)

  type file =
    | Unit of string * unit_infos * Digest.t
    | Library of string * library_infos

  (* CR mshinwell: This should not be raising errors from [Linkenv] *)
  let read_file obj_name =
    let file_name =
      try Load_path.find obj_name
      with Not_found -> raise (Linkenv.Error (File_not_found obj_name))
    in
    if Filename.check_suffix file_name Backend.ext_flambda_obj
    then
      (* This is a cmx file. It must be linked in any case. Read the infos to
         see which modules it requires. *)
      let info, crc = read_unit_info file_name in
      Unit (file_name, info, crc)
    else if Filename.check_suffix file_name Backend.ext_flambda_lib
    then
      let infos =
        try read_library_info file_name
        with Compilenv.Error (Not_a_unit_info filename) ->
          raise (Linkenv.Error (Not_an_object_file filename))
      in
      Library (file_name, infos)
    else raise (Linkenv.Error (Not_an_object_file file_name))

  let scan_file linkenv ~shared genfns file
      (full_paths, objfiles, tolink, cached_genfns_imports) =
    match read_file file with
    | Unit (file_name, info, crc) ->
      (* This is a cmx file. It must be linked in any case. *)
      Linkenv.remove_required linkenv info.ui_unit;
      Linkenv.add_quoted_globals linkenv info.ui_quoted_globals;
      List.iter
        (fun import -> Linkenv.add_required linkenv (file_name, None) import)
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
              dynu_imports_cmx = info.ui_imports_cmx |> Array.of_list;
              dynu_quoted_globals = info.ui_quoted_globals |> Array.of_list
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
      let object_file_name =
        Filename.chop_suffix file_name Backend.ext_flambda_obj ^ Backend.ext_obj
      in
      Linkenv.check_consistency linkenv ~unit
        (Array.of_list info.ui_imports_cmi)
        (Array.of_list info.ui_imports_cmx);
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
          info.ui_generic_fns
      in
      ( file_name :: full_paths,
        object_file_name :: objfiles,
        unit :: tolink,
        cached_genfns_imports )
    | Library (file_name, infos) ->
      (* This is an archive file. Each unit contained in it will be linked in
         only if needed. *)
      Linkenv.add_ccobjs linkenv (Filename.dirname file_name) infos;
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
          infos.lib_generic_fns
      in
      Linkenv.check_cmi_consistency linkenv file_name infos.lib_imports_cmi;
      Linkenv.check_cmx_consistency linkenv file_name infos.lib_imports_cmx;
      let objfiles =
        let obj_file =
          Filename.chop_suffix file_name Backend.ext_flambda_lib
          ^ Backend.ext_lib
        in
        (* MSVC doesn't support empty .lib files, and macOS struggles to make
           them (#6550), so there shouldn't be one if the cmxa contains no
           units. The file_exists check is added to be ultra-defensive for the
           case where a user has manually added things to the .a/.lib file *)
        if infos.lib_units = [] && not (Sys.file_exists obj_file)
        then objfiles
        else obj_file :: objfiles
      in
      (* [file_name] is always returned irrespective of the [objfiles]
         calculation above and the units calculation below: the aim is to know
         the full set of files which were provided on the command line. *)
      ( file_name :: full_paths,
        objfiles,
        List.fold_right
          (fun info reqd ->
            let li_name = CU.name info.li_name in
            if info.li_force_link || !Clflags.link_everything
               || Linkenv.is_required linkenv info.li_name
            then (
              Linkenv.remove_required linkenv info.li_name;
              let req_by = file_name, Some li_name in
              info.li_imports_cmx
              |> Misc.Bitmap.iter (fun i ->
                     let import = infos.lib_imports_cmx.(i) in
                     Linkenv.add_required linkenv req_by import);
              let imports_list tbl bits =
                List.init (Array.length tbl) (fun i ->
                    if Misc.Bitmap.get bits i then Some tbl.(i) else None)
                |> List.filter_map Fun.id
              in
              let quoted_globals =
                imports_list infos.lib_quoted_globals info.li_quoted_globals
              in
              Linkenv.add_quoted_globals linkenv quoted_globals;
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
                        |> Array.of_list;
                      dynu_quoted_globals = quoted_globals |> Array.of_list
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
              Linkenv.check_consistency linkenv ~unit [||] [||];
              unit :: reqd)
            else reqd)
          infos.lib_units tolink,
        cached_genfns_imports )

  (* Second pass: generate the startup file and link it with everything else *)

  let named_startup_file () =
    !Clflags.keep_startup_file || !Emitaux.binary_backend_available

  (* The compiler allows [-o /dev/null], which can be used for testing linking.
     In this case, we should not use the DWARF fission workflow during
     linking. *)
  let not_output_to_dev_null output_name =
    not (String.equal output_name "/dev/null")

  let link_shared ~ppf_dump objfiles output_name =
    Profile.(record_call (annotate_file_name output_name)) (fun () ->
        let linkenv = Linkenv.create () in
        let genfns = Generic_fns.Tbl.make () in
        let _full_paths, ml_objfiles, units_tolink, _ =
          List.fold_right
            (scan_file linkenv ~shared:true genfns)
            objfiles
            ([], [], [], Generic_fns.Partition.Set.empty)
        in
        Clflags.ccobjs := !Clflags.ccobjs @ Linkenv.lib_ccobjs linkenv;
        Clflags.all_ccopts := Linkenv.lib_ccopts linkenv @ !Clflags.all_ccopts;
        Backend.link_shared ml_objfiles output_name ~ppf_dump ~genfns
          ~units_tolink)

  (* Main entry point *)

  let link ~ppf_dump objfiles output_name =
    let shared = false in
    Profile.(record_call (annotate_file_name output_name)) (fun () ->
        let linkenv = Linkenv.create () in
        let stdlib = "stdlib" ^ Backend.ext_flambda_lib in
        let stdexit = "std_exit" ^ Backend.ext_flambda_obj in
        let objfiles =
          (* stdlib is added below as part of [early_pervasives], if required *)
          if !Clflags.nopervasives || !Clflags.output_c_object
          then objfiles
          else objfiles @ [stdexit]
        in
        let genfns = Generic_fns.Tbl.make () in
        (* CR mshinwell/xclerc: This tuple should be a record *)
        let full_paths, ml_objfiles, units_tolink, cached_genfns_imports =
          (* This covers all files that the user has requested be linked *)
          List.fold_right
            (scan_file linkenv ~shared:false genfns)
            objfiles
            ([], [], [], Generic_fns.Partition.Set.empty)
        in
        let uses_eval =
          (* This query must come after scan_file has been called on objfiles,
             otherwise is_required will always return false. *)
          Linkenv.is_required linkenv
            (Compilation_unit.of_string "Camlinternaleval")
        in
        let quoted_globals = Linkenv.get_quoted_globals linkenv in
        if uses_eval && not Backend.supports_metaprogramming
        then
          raise
            (Linkenv.Error
               (Metaprogramming_not_supported_by_backend output_name));
        let stdlib_and_support_files_for_eval =
          if !Clflags.nopervasives
          then []
          else
            let for_eval =
              if not uses_eval
              then []
              else
                let deps = Backend.support_files_for_eval () in
                (* Avoid double linking errors in the case where the user has
                   already passed one of the support files on the command line.
                   The equality used here is the full path as resolved by
                   [Load_path] (see also [scan_file], above). *)
                List.filter
                  (fun dep ->
                    (* CR mshinwell: it's unclear that [Load_path] does anything
                       along the lines of [realpath], so this equality might not
                       be as good as we would like *)
                    match Load_path.find dep with
                    | full_path -> not (List.mem full_path full_paths)
                    | exception Not_found ->
                      (* An error will be reported by [scan_file], called below,
                         in this case. (This is likely to be a compiler bug or a
                         corrupted installation.) *)
                      true)
                  deps
            in
            stdlib :: for_eval
        in
        let _full_paths, ml_objfiles, units_tolink, cached_genfns_imports =
          (* This is just for any stdlib and eval support files which are
             needed. *)
          List.fold_right
            (scan_file linkenv ~shared:false genfns)
            stdlib_and_support_files_for_eval
            ([], ml_objfiles, units_tolink, cached_genfns_imports)
        in
        (if not shared
        then
          match Linkenv.extract_missing_globals linkenv with
          | [] -> ()
          | mg -> raise (Linkenv.Error (Missing_implementations mg)));
        Clflags.ccobjs := !Clflags.ccobjs @ Linkenv.lib_ccobjs linkenv;
        Clflags.all_ccopts := Linkenv.lib_ccopts linkenv @ !Clflags.all_ccopts;
        (* put user's opts first *)
        Backend.link linkenv ml_objfiles output_name ~ppf_dump ~genfns
          ~units_tolink ~uses_eval ~quoted_globals ~cached_genfns_imports)

  (* Exported version for Asmlibrarian / Asmpackager *)
  let check_consistency linkenv file_name u crc =
    let unit =
      { file_name;
        name = u.ui_unit;
        defines = u.ui_defines;
        crc;
        dynunit = None
      }
    in
    Linkenv.check_consistency linkenv ~unit
      (Array.of_list u.ui_imports_cmi)
      (Array.of_list u.ui_imports_cmx)
end
