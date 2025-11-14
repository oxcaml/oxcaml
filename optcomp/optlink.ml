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
      ui_imports_cmi : Import_info.t list;
      ui_imports_cmx : Import_info.t list;
      ui_quoted_globals : Compilation_unit.Name.t list
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
      let unit =
        { name = info.ui_unit;
          crc;
          defines = info.ui_defines;
          file_name;
          ui_imports_cmi = info.ui_imports_cmi;
          ui_imports_cmx = info.ui_imports_cmx;
          ui_quoted_globals = info.ui_quoted_globals
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
              let ui_imports_cmi =
                imports_list infos.lib_imports_cmi info.li_imports_cmi
              in
              let ui_imports_cmx =
                imports_list infos.lib_imports_cmx info.li_imports_cmx
              in
              let unit =
                { name = info.li_name;
                  crc = info.li_crc;
                  defines = info.li_defines;
                  file_name;
                  ui_imports_cmi;
                  ui_imports_cmx;
                  ui_quoted_globals = quoted_globals
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

        (* Build consolidated import arrays from linkenv *)
        let dynu_imports_cmi =
          Linkenv.extract_crc_interfaces linkenv |> Array.of_list
        in
        let dynu_imports_cmx =
          Linkenv.extract_crc_implementations linkenv |> Array.of_list
        in
        (* Create index tables for bitmap creation *)
        let cmi_index = Compilation_unit.Name.Tbl.create 42 in
        Array.iteri
          (fun i import ->
            Compilation_unit.Name.Tbl.add cmi_index (Import_info.name import) i)
          dynu_imports_cmi;
        let cmx_index = Compilation_unit.Tbl.create 42 in
        Array.iteri
          (fun i import ->
            Compilation_unit.Tbl.add cmx_index (Import_info.cu import) i)
          dynu_imports_cmx;
        (* Helper function to create bitmaps *)
        let mk_bitmap arr ix entries ~find ~get_name =
          let module B = Misc.Bitmap in
          let b = B.make (Array.length arr) in
          List.iter (fun import -> B.set b (find ix (get_name import))) entries;
          b
        in
        (* Create dynunits with bitmaps *)
        let dynu_units =
          List.map
            (fun unit ->
              { Cmxs_format.dynu_name = unit.name;
                dynu_crc = unit.crc;
                dynu_imports_cmi_bitmap =
                  mk_bitmap dynu_imports_cmi cmi_index unit.ui_imports_cmi
                    ~find:Compilation_unit.Name.Tbl.find
                    ~get_name:Import_info.name;
                dynu_imports_cmx_bitmap =
                  mk_bitmap dynu_imports_cmx cmx_index unit.ui_imports_cmx
                    ~find:Compilation_unit.Tbl.find ~get_name:Import_info.cu;
                dynu_quoted_globals = Array.of_list unit.ui_quoted_globals;
                dynu_defines = unit.defines
              })
            units_tolink
        in
        let dynheader : Cmxs_format.dynheader =
          { dynu_magic = Config.cmxs_magic_number;
            dynu_units;
            dynu_imports_cmi;
            dynu_imports_cmx
          }
        in
        Backend.link_shared ml_objfiles output_name ~ppf_dump ~genfns
        ~dynheader)

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
          scan_user_supplied_files linkenv ~genfns ~objfiles
        in
        let uses_eval =
          (* This query must come after scan_file has been called on objfiles,
             otherwise is_required will always return false. *)
          (* CR mshinwell: maybe instead we could have a flag on cmx/cmxa files
             to indicate whether they need metaprogramming? Then we wouldn't
             have to do scan_file first, which in turn would avoid the need to
             snapshot the Linkenv. It also wouldn't capture things like mdx
             which should not receive this special treatment. *)
          Linkenv.is_required linkenv
            (Compilation_unit.of_string "Camlinternaleval")
        in
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
              scan_user_supplied_files linkenv ~genfns ~objfiles
            in
            ( linkenv,
              _full_paths,
              ml_objfiles,
              units_tolink,
              cached_genfns_imports,
              genfns )
        in
        let quoted_globals = Linkenv.get_quoted_globals linkenv in
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
  let check_consistency linkenv file_name (u : Cmx_format.unit_infos) crc =
    let unit : unit_link_info =
      { file_name;
        name = u.ui_unit;
        defines = u.ui_defines;
        crc;
        ui_imports_cmi = u.ui_imports_cmi;
        ui_imports_cmx = u.ui_imports_cmx;
        ui_quoted_globals = u.ui_quoted_globals
      }
    in
    Linkenv.check_consistency linkenv ~unit
      (Array.of_list u.ui_imports_cmi)
      (Array.of_list u.ui_imports_cmx)
end
