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

(* Link a set of .cmjx/.cmjo files and produce a JavaScript executable *)

open Misc
open Cmx_format
open Compilenv

module CU = Compilation_unit

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (CU.t * string list) list
  | Inconsistent_interface of CU.Name.t * filepath * filepath
  | Inconsistent_implementation of CU.t * filepath * filepath
  | Linking_error of int
  | Multiple_definition of CU.Name.t * filepath * filepath
  | Missing_cmjx of filepath * CU.t

exception Error of error

type unit_link_info = {
  name: Compilation_unit.t;
  defines: Compilation_unit.t list;
  (* Note: The 'defines' field tracks packed modules and is used for consistency
     checking. Unlike native compilation, JavaScript module initialization is
     handled by js_of_ocaml at the JavaScript level, so we don't use this field
     to generate startup code. We keep it for compatibility with the common
     compilation infrastructure and for proper error reporting about packed modules. *)
  file_name: string;
  crc: Digest.t;
}
[@@warning "-69"]

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (CU.Name) (Import_info.Intf.Nonalias.Kind)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = CU.Name.Tbl.create 100

module Cmx_consistbl = Consistbl.Make (CU) (Unit)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : CU.t list)
let implementations_defined = CU.Tbl.create 100
let cmjx_required = ref ([] : CU.t list)

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
              if List.mem name !cmjx_required then
                raise(Error(Missing_cmjx(file_name, name)))
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
    cmjx_required := unit.name :: !cmjx_required

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

(* Add JS libs and options from a library descriptor *)

let lib_jsobjs = ref []
let lib_jsopts = ref []

let add_jsobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_jsobjs := l.lib_ccobjs @ !lib_jsobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_jsopts := List.map replace_origin l.lib_ccopts @ !lib_jsopts
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
  if Filename.check_suffix file_name ".cmjx" then begin
    (* This is a .cmjx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name, info, crc)
  end
  else if Filename.check_suffix file_name ".cmjxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name, infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file file (objfiles, tolink) =
  match read_file file with
  | Unit (file_name, info, crc) ->
      (* This is a .cmjx file. It must be linked in any case. *)
      remove_required info.ui_unit;
      List.iter (fun import ->
          add_required (file_name, None) import)
        info.ui_imports_cmx;
      let unit =
        { name = info.ui_unit;
          crc;
          defines = info.ui_defines;
          file_name }
      in
      let object_file_name =
        Filename.chop_suffix file_name ".cmjx" ^ ".cmjo" in
      check_consistency ~unit
        (Array.of_list info.ui_imports_cmi)
        (Array.of_list info.ui_imports_cmx);
      object_file_name :: objfiles, unit :: tolink
  | Library (file_name, infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_jsobjs (Filename.dirname file_name) infos;
      check_cmi_consistency file_name infos.lib_imports_cmi;
      check_cmx_consistency file_name infos.lib_imports_cmx;
      let objfiles =
        let obj_file =
          Filename.chop_suffix file_name ".cmjxa" ^ ".cmja" in
        (* .cmja files are JavaScript archives *)
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
             let unit =
               { name = info.li_name;
                 crc = info.li_crc;
                 defines = info.li_defines;
                 file_name }
             in
             check_consistency ~unit [| |] [| |];
             unit :: reqd
           end else
           reqd)
        infos.lib_units tolink

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  CU.Tbl.reset implementations_defined;
  cmjx_required := [];
  CU.Name.Tbl.reset interfaces;
  implementations := [];
  lib_jsobjs := [];
  lib_jsopts := []

(* Main entry point *)

let link ~ppf_dump:(_ : Format.formatter) objfiles output_name =
  Profile.(record_call (annotate_file_name output_name)) (fun () ->
    let stdlib = "stdlib.cmjxa" in
    let stdexit = "std_exit.cmjx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in

    let js_objfiles, _units_tolink =
      List.fold_right scan_file objfiles ([], [])
    in
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_jsobjs;
    Clflags.all_ccopts := !lib_jsopts @ !Clflags.all_ccopts;

    Clflags.ccobjs := ListLabels.map !Clflags.ccobjs ~f:(fun obj_name ->
        try
          Load_path.find obj_name
        with Not_found ->
          raise(Error(File_not_found obj_name))
    );

    (* Build the runtime *)
    let runtime = output_name ^ ".runtime.js" in
    let debug_flag =
      if !Clflags.debug then ["--debug-info"] else []
    in

    (* Extract runtime files from ccobjs *)
    let runtime_files, other_objs =
      ListLabels.partition !Clflags.ccobjs ~f:(fun f -> Filename.check_suffix f ".js")
    in

    (* Always build runtime - it's required for JavaScript execution *)
    Jscompile.run_jsoo_exn
      ~args:([ "build-runtime"; "--enable=effects,with-js-error"; "-o"; runtime ] @ debug_flag @ runtime_files);

    (* Link everything together *)
    let files_to_link =
      runtime ::
      other_objs @ js_objfiles
    in
    let linkall_flag = if !Clflags.link_everything then ["--linkall"] else [] in
    Misc.try_finally
      (fun () ->
        Jscompile.run_jsoo_exn
          ~args:(["link"; "-o"; output_name ] @ linkall_flag @ debug_flag @ files_to_link
                 @ (List.rev !Clflags.all_jsopts)
                ))
      ~always:(fun () ->
        Misc.remove_file runtime)
  )

(* Exported version for Jslibrarian *)
let check_consistency file_name u crc =
  let unit =
    { file_name;
      name = u.ui_unit;
      defines = u.ui_defines;
      crc }
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
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print modname
  | Missing_cmjx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmjx file@ for module %a,@ \
         which was produced by `ocamlj -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %a.cmjx@ is found.@]"
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
