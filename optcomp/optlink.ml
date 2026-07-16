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

  val link_shared :
    ppf_dump:formatter -> Linkenv.t -> string list -> string -> unit

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
      imports_cmx : Import_info.t list;
      (* for shared libs *)
      dynunit : Cmxs_format.dynunit option
    }

  (* First pass: determine which units are needed *)

  type file =
    | Unit of string * unit_infos * Digest.t
    | Library of string * library_infos

  (* CR mshinwell: This should not be raising errors from [Linkenv] *)
  let read_file requested_filename =
    let resolved_pathname =
      try Load_path.find requested_filename
      with Not_found ->
        raise (Linkenv.Error (File_not_found requested_filename))
    in
    (* The kind of a link input is normally determined by the extension of
       [resolved_pathname]. However, when [requested_filename] is resolved
       through a manifest entry (see [-I-manifest]), the resolved path need not
       retain the extension (manifests may map filenames to e.g.
       content-addressed paths), so we also consult the extension of
       [requested_filename]. *)
    let has_suffix ext =
      Filename.check_suffix resolved_pathname ext
      || Filename.check_suffix requested_filename ext
    in
    if has_suffix Backend.ext_flambda_obj
    then
      (* This is a cmx file. It must be linked in any case. Read the infos to
         see which modules it requires. *)
      let info, crc =
        Profile.record_call ~accumulate:true "link/scan/read_cmx" (fun () ->
            read_unit_info resolved_pathname)
      in
      Unit (resolved_pathname, info, crc)
    else if has_suffix Backend.ext_flambda_lib
    then
      let infos =
        try
          Profile.record_call ~accumulate:true "link/scan/read_cmxa" (fun () ->
              read_library_info resolved_pathname)
        with Compilenv.Error (Not_a_unit_info filename) ->
          raise (Linkenv.Error (Not_an_object_file filename))
      in
      Library (resolved_pathname, infos)
    else raise (Linkenv.Error (Not_an_object_file resolved_pathname))

  (* The result of [find_companion_object_file], below. [Adjacent] carries the
     object file's path, obtained by replacing the artifact extension in the
     artifact's resolved path. The [Resolved_via_manifest_*] cases arise when
     the artifact's resolved path does not retain the artifact extension, which
     only happens when the artifact was resolved through a manifest entry (see
     [-I-manifest]); the object file is then resolved through the load path
     itself, yielding its path if found, or the searched-for filename if not. *)
  type companion_object_file =
    | Adjacent of Misc.filepath
    | Resolved_via_manifest_and_found_in_load_path of Misc.filepath
    | Resolved_via_manifest_but_not_found_in_load_path of Misc.filepath

  (* Locate the object file ([.o] or [.a]/[.lib]) that accompanies a compilation
     artifact ([.cmx] or [.cmxa]).

     Normally the object file sits next to the artifact, and we obtain its path
     by replacing the artifact extension in the artifact's resolved path
     [resolved_pathname].

     However, when the artifact was resolved through a manifest entry (see
     [-I-manifest]), its resolved path need not retain the extension, and the
     object file need not sit next to it. In that case we form the object file's
     name from the name the artifact was requested under ([requested_filename],
     which must then carry the artifact extension, since [read_file] dispatched
     on it) and resolve that name through the load path. In other words,
     manifests must list the object files as separate entries. *)
  let find_companion_object_file ~requested_filename ~resolved_pathname
      ~artifact_ext ~object_ext =
    if Filename.check_suffix resolved_pathname artifact_ext
    then
      let object_pathname =
        Filename.chop_suffix resolved_pathname artifact_ext ^ object_ext
      in
      Adjacent object_pathname
    else
      let object_filename =
        Filename.chop_suffix requested_filename artifact_ext ^ object_ext
      in
      match Load_path.find object_filename with
      | object_pathname ->
        Resolved_via_manifest_and_found_in_load_path object_pathname
      | exception Not_found ->
        Resolved_via_manifest_but_not_found_in_load_path object_filename

  let scan_file linkenv ~shared genfns requested_filename
      (full_paths, objfiles, tolink, cached_genfns_imports) =
    match read_file requested_filename with
    | Unit (resolved_pathname, info, crc) ->
      (* This is a cmx file. It must be linked in any case. *)
      Linkenv.remove_required linkenv info.ui_unit;
      Linkenv.add_quoted_cmi linkenv info.ui_quoted_cmi;
      Linkenv.add_quoted_cmx linkenv info.ui_quoted_cmx;
      List.iter
        (fun import ->
          Linkenv.add_required linkenv (resolved_pathname, None) import)
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
              dynu_quoted_cmi = info.ui_quoted_cmi |> Array.of_list;
              dynu_quoted_cmx = info.ui_quoted_cmx |> Array.of_list
            }
      in
      let unit =
        { name = info.ui_unit;
          crc;
          defines = info.ui_defines;
          file_name = resolved_pathname;
          imports_cmx = info.ui_imports_cmx;
          dynunit
        }
      in
      let object_file_name =
        match
          find_companion_object_file ~requested_filename ~resolved_pathname
            ~artifact_ext:Backend.ext_flambda_obj ~object_ext:Backend.ext_obj
        with
        | Adjacent object_file
        | Resolved_via_manifest_and_found_in_load_path object_file ->
          object_file
        | Resolved_via_manifest_but_not_found_in_load_path object_filename ->
          raise
            (Linkenv.Error
               (Companion_object_file_not_found_via_manifest
                  (object_filename, requested_filename)))
      in
      Profile.record_call ~accumulate:true "link/scan/check_consistency"
        (fun () ->
          Linkenv.check_consistency linkenv ~unit
            (Array.of_list info.ui_imports_cmi)
            (Array.of_list info.ui_imports_cmx));
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
          info.ui_generic_fns
      in
      if
        (not shared) && info.ui_requires_metaprogramming
        && not !Clflags.uses_metaprogramming
      then
        raise
          (Linkenv.Error
             (Requires_metaprogramming_without_flag resolved_pathname));
      ( resolved_pathname :: full_paths,
        object_file_name :: objfiles,
        unit :: tolink,
        cached_genfns_imports )
    | Library (resolved_pathname, infos) ->
      (* This is an archive file. Each unit contained in it will be linked in
         only if needed. *)
      Linkenv.add_ccobjs linkenv (Filename.dirname resolved_pathname) infos;
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns
          infos.lib_generic_fns
      in
      Linkenv.check_cmi_consistency linkenv resolved_pathname
        infos.lib_imports_cmi;
      Linkenv.check_cmx_consistency linkenv resolved_pathname
        infos.lib_imports_cmx;
      if
        (not shared) && infos.lib_requires_metaprogramming
        && not !Clflags.uses_metaprogramming
      then
        raise
          (Linkenv.Error
             (Requires_metaprogramming_without_flag resolved_pathname));
      let objfiles =
        match
          find_companion_object_file ~requested_filename ~resolved_pathname
            ~artifact_ext:Backend.ext_flambda_lib ~object_ext:Backend.ext_lib
        with
        | Adjacent obj_file ->
          (* MSVC doesn't support empty .lib files, and macOS struggles to make
             them (#6550), so there shouldn't be one if the cmxa contains no
             units. The file_exists check is added to be ultra-defensive for the
             case where a user has manually added things to the .a/.lib file *)
          if List.is_empty infos.lib_units && not (Sys.file_exists obj_file)
          then objfiles
          else obj_file :: objfiles
        | Resolved_via_manifest_and_found_in_load_path obj_file ->
          obj_file :: objfiles
        | Resolved_via_manifest_but_not_found_in_load_path object_filename ->
          (* The archive was resolved through a manifest with no entry for the
             corresponding [.a]/[.lib] file. This is only legitimate when the
             archive contains no units (see the comment above about empty .lib
             files). *)
          if List.is_empty infos.lib_units
          then objfiles
          else
            raise
              (Linkenv.Error
                 (Companion_object_file_not_found_via_manifest
                    (object_filename, requested_filename)))
      in
      (* [resolved_pathname] is always returned irrespective of the [objfiles]
         calculation above and the units calculation below: the aim is to know
         the full set of files which were provided on the command line. *)
      ( resolved_pathname :: full_paths,
        objfiles,
        List.fold_right
          (fun info reqd ->
            let li_name = CU.name info.li_name in
            if
              info.li_force_link || !Clflags.link_everything
              || Linkenv.is_required linkenv info.li_name
            then (
              Linkenv.remove_required linkenv info.li_name;
              let req_by = resolved_pathname, Some li_name in
              info.li_imports_cmx
              |> Misc.Bitmap.iter (fun i ->
                  let import = infos.lib_imports_cmx.(i) in
                  Linkenv.add_required linkenv req_by import);
              let imports_list tbl bits =
                List.init (Array.length tbl) (fun i ->
                    if Misc.Bitmap.get bits i then Some tbl.(i) else None)
                |> List.filter_map Fun.id
              in
              let quoted_cmi =
                imports_list infos.lib_quoted_cmi info.li_quoted_cmi
              in
              let quoted_cmx =
                imports_list infos.lib_quoted_cmx info.li_quoted_cmx
              in
              Linkenv.add_quoted_cmi linkenv quoted_cmi;
              Linkenv.add_quoted_cmx linkenv quoted_cmx;
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
                      dynu_quoted_cmi = quoted_cmi |> Array.of_list;
                      dynu_quoted_cmx = quoted_cmx |> Array.of_list
                    }
              in
              let imports_cmx =
                imports_list infos.lib_imports_cmx info.li_imports_cmx
              in
              let unit =
                { name = info.li_name;
                  crc = info.li_crc;
                  defines = info.li_defines;
                  file_name = resolved_pathname;
                  imports_cmx;
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

  let link_shared ~ppf_dump linkenv objfiles output_name =
    Profile.(record_call (annotate_file_name output_name)) (fun () ->
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
        let[@inline] scan_user_supplied_files linkenv ~genfns ~objfiles =
          (* This covers all files that the user has requested be linked *)
          List.fold_right
            (scan_file linkenv ~shared:false genfns)
            objfiles
            ([], [], [], Generic_fns.Partition.Set.empty)
        in
        let linkenv = Linkenv.create () in
        let full_paths, ml_objfiles, units_tolink, cached_genfns_imports =
          Profile.record_call "link/scan/user_files_pass1" (fun () ->
              scan_user_supplied_files linkenv ~genfns ~objfiles)
        in
        let uses_eval = !Clflags.uses_metaprogramming in
        if uses_eval && not Backend.supports_metaprogramming
        then
          raise
            (Linkenv.Error
               (Metaprogramming_not_supported_by_backend output_name));
        let eval_support_files = Backend.support_files_for_eval () in
        if uses_eval && not !Clflags.nopervasives
        then Backend.set_load_path_for_eval ();
        let full_paths_of_eval_support_files_already_provided_by_user =
          if not uses_eval
          then []
          else
            (* Avoid double linking errors in the case where the user has
               already passed one of the support files on the command line. The
               equality used here is the full path as resolved by [Load_path]
               (see also [scan_file], above). *)
            List.filter_map
              (fun support_file ->
                (* CR mshinwell: it's unclear that [Load_path] does anything
                   along the lines of [realpath], so this equality might not be
                   as good as we would like *)
                match Load_path.find support_file with
                | full_path ->
                  if List.mem full_path full_paths then Some full_path else None
                | exception Not_found ->
                  (* An error will be reported by [scan_file], called below, in
                     this case. (This is likely to be a compiler bug or a
                     corrupted installation.) *)
                  None)
              eval_support_files
        in
        (* Unfortunately because of the need to determine [for_eval] in order to
           decide whether we need to filter the list of input files, we have to
           rerun the first [scan_file] pass here if we need to remove any
           user-specified libraries. *)
        (* CR mshinwell: another possibility might be to always move the eval
           support files to the start of the command line whether or not they're
           used, but this seems like the sort of thing that might cost someone a
           lot of time one day *)
        let ( linkenv,
              _full_paths,
              ml_objfiles,
              units_tolink,
              cached_genfns_imports,
              genfns ) =
          match full_paths_of_eval_support_files_already_provided_by_user with
          | [] ->
            ( linkenv,
              full_paths,
              ml_objfiles,
              units_tolink,
              cached_genfns_imports,
              genfns )
          | _ :: _ when !Clflags.nopervasives ->
            (* In this case we won't link any eval support files
               automatically *)
            ( linkenv,
              full_paths,
              ml_objfiles,
              units_tolink,
              cached_genfns_imports,
              genfns )
          | _ :: _ ->
            assert uses_eval;
            let linkenv = Linkenv.create () in
            let genfns = Generic_fns.Tbl.make () in
            let objfiles =
              (* Remove user-provided occurrences of support libraries *)
              List.filter
                (fun file ->
                  match Load_path.find file with
                  | full_path ->
                    not
                      (List.mem full_path
                         full_paths_of_eval_support_files_already_provided_by_user)
                  | exception Not_found ->
                    (* Some kind of race has occurred, just ignore it. *)
                    true)
                objfiles
            in
            let _full_paths, ml_objfiles, units_tolink, cached_genfns_imports =
              Profile.record_call "link/scan/user_files_pass2" (fun () ->
                  scan_user_supplied_files linkenv ~genfns ~objfiles)
            in
            ( linkenv,
              _full_paths,
              ml_objfiles,
              units_tolink,
              cached_genfns_imports,
              genfns )
        in
        let quoted_cmi = Linkenv.get_quoted_cmi linkenv in
        let quoted_cmx = Linkenv.get_quoted_cmx linkenv in
        let stdlib_and_support_files_for_eval =
          if !Clflags.nopervasives
          then []
          else
            (* We can safely add all of the support files now without risking
               double linking. *)
            stdlib :: (if uses_eval then eval_support_files else [])
        in
        let _full_paths, ml_objfiles, units_tolink, cached_genfns_imports =
          (* This is just for any stdlib and eval support files which are
             needed. *)
          Profile.record_call "link/scan/stdlib_and_eval_support" (fun () ->
              List.fold_right
                (scan_file linkenv ~shared:false genfns)
                stdlib_and_support_files_for_eval
                ([], ml_objfiles, units_tolink, cached_genfns_imports))
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
          ~units_tolink ~uses_eval ~quoted_cmi ~quoted_cmx
          ~cached_genfns_imports)

  (* Exported version for Asmlibrarian / Asmpackager *)
  let check_consistency linkenv file_name u crc =
    let unit =
      { file_name;
        name = u.ui_unit;
        defines = u.ui_defines;
        crc;
        imports_cmx = u.ui_imports_cmx;
        dynunit = None
      }
    in
    Linkenv.check_consistency linkenv ~unit
      (Array.of_list u.ui_imports_cmi)
      (Array.of_list u.ui_imports_cmx)
end
