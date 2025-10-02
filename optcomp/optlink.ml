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

  val reset : unit -> unit
  val check_consistency: filepath -> Cmx_format.unit_infos -> Digest.t -> unit
  val extract_crc_interfaces: unit -> Import_info.t list
  val extract_crc_implementations: unit -> Import_info.t list
end

module Make (Backend : Optbackend_intf.S) : S = struct
open Cmx_format
open Compilenv

let link_partial = Backend.link_partial

module String = Misc.Stdlib.String
module CU = Compilation_unit

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (CU.t * string list) list
  | Inconsistent_interface of CU.Name.t * filepath * filepath
  | Inconsistent_implementation of CU.t * filepath * filepath
  | Multiple_definition of CU.Name.t * filepath * filepath
  | Missing_cmx of filepath * CU.t

exception Error of error

type unit_link_info = Optbackend_intf.unit_link_info = {
  name: Compilation_unit.t;
  defines: Compilation_unit.t list;
  file_name: string;
  crc: Digest.t;
  (* for shared libs *)
  dynunit : Cmxs_format.dynunit option;
}

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (CU.Name) (Import_info.Intf.Nonalias.Kind)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = CU.Name.Tbl.create 100

module Cmx_consistbl = Consistbl.Make (CU) (Unit)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : CU.t list)
let implementations_defined = CU.Tbl.create 100
let cmx_required = ref ([] : CU.t list)

let check_cmi_consistency file_name cmis =
  try
    Array.iter
      (fun import ->
        let name = Import_info.name import in
        let info = Import_info.Intf.info import in
        CU.Name.Tbl.replace interfaces name ();
        match info with
          None -> ()
        | Some (kind, crc) ->
            Cmi_consistbl.check crc_interfaces name kind crc file_name)
      cmis
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))

let check_cmx_consistency file_name cmxs =
  try
    Array.iter
      (fun import ->
        let name = Import_info.cu import in
        let crco = Import_info.crc import in
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name () crc file_name)
      cmxs
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))

let check_consistency ~unit cmis cmxs =
  check_cmi_consistency unit.file_name cmis;
  check_cmx_consistency unit.file_name cmxs;
  let ui_unit = CU.name unit.name in
  begin try
    let source = CU.Tbl.find implementations_defined unit.name in
    raise (Error(Multiple_definition(ui_unit, unit.file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.name () unit.crc unit.file_name;
  CU.Tbl.replace implementations_defined unit.name unit.file_name;
  if CU.is_packed unit.name then
    cmx_required := unit.name :: !cmx_required

let extract_crc_interfaces () =
  CU.Name.Tbl.fold (fun name () crcs ->
      let crc_with_unit = Cmi_consistbl.find crc_interfaces name in
      Import_info.Intf.create name crc_with_unit :: crcs)
    interfaces
    []

let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations
  |> List.map (fun (cu, crc) ->
       let crc = Option.map (fun ((), crc) -> crc) crc in
       Import_info.create_normal cu ~crc)

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

let missing_globals =
  (Hashtbl.create 17 :
     (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by import =
  let name = Import_info.cu import in
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  let fmt = function
    | file, None -> file
    | file, Some part -> Format.asprintf "%s(%a)" file CU.Name.print part
  in
  Hashtbl.iter (fun md rq -> mg := (md, List.map fmt !rq) :: !mg) missing_globals;
  !mg

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

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so
     no module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

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
      check_consistency ~unit
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
             check_consistency ~unit [| |] [| |];
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
      ~reset
      ~cached_genfns_imports
  )

(* Exported version for Asmlibrarian / Asmpackager *)
let check_consistency file_name u crc =
  let unit =
    { file_name;
      name = u.ui_unit;
      defines = u.ui_defines;
      crc;
      dynunit = None }
  in
  check_consistency ~unit
    (Array.of_list u.ui_imports_cmi) (Array.of_list u.ui_imports_cmx)

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
            CU.print md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %a@]"
       Location.print_filename file1
       Location.print_filename file2
       CU.Name.print intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %a@]"
       Location.print_filename file1
       Location.print_filename file2
       CU.print intf
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %a,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %a.cmx@ is found.@]"
        Location.print_filename filename
        CU.print name
        Location.print_filename filename
        CU.print name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
end
