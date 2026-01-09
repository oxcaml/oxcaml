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

open Cmx_format
module CU = Compilation_unit

type filepath = string

type unit_link_info =
  { name : Compilation_unit.t;
    defines : Compilation_unit.t list;
    file_name : string;
    crc : Digest.t;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

module Cmi_consistbl = Consistbl.Make (CU.Name) (Import_info.Intf.Nonalias.Kind)
module Cmx_consistbl = Consistbl.Make (CU) (Unit)

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (Compilation_unit.t * string list) list
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.t * filepath * filepath
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.t
  | Linking_error of int
  | Archiver_error of string
  | Metaprogramming_not_supported_by_backend of filepath

exception Error of error

type t =
  { crc_interfaces : Cmi_consistbl.t;
    crc_implementations : Cmx_consistbl.t;
    mutable implementations : CU.t list;
    mutable cmx_required : CU.t list;
    interfaces : unit CU.Name.Tbl.t;
    implementations_defined : string CU.Tbl.t;
    mutable quoted_globals : CU.Name.Set.t;
    mutable lib_ccobjs : filepath list;
    mutable lib_ccopts : string list;
    missing_globals : (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t
  }

let create () =
  let quoted_globals =
    if !Clflags.nopervasives
    then CU.Name.Set.empty
    else CU.Name.Set.singleton (CU.Name.of_string "Stdlib")
  in
  { crc_interfaces = Cmi_consistbl.create ();
    crc_implementations = Cmx_consistbl.create ();
    implementations = [];
    cmx_required = [];
    interfaces = CU.Name.Tbl.create 100;
    implementations_defined = CU.Tbl.create 100;
    quoted_globals;
    lib_ccobjs = [];
    lib_ccopts = [];
    missing_globals = Hashtbl.create 17
  }

(* Globals for quotations *)

let add_quoted_globals t globals =
  t.quoted_globals
    <- List.fold_left
         (fun globals global -> CU.Name.Set.add global globals)
         t.quoted_globals globals

let get_quoted_globals t = t.quoted_globals

(* Consistency check between interfaces and implementations: *)

let check_cmi_consistency t file_name cmis =
  try
    Array.iter
      (fun import ->
        let name = Import_info.name import in
        let info = Import_info.Intf.info import in
        CU.Name.Tbl.replace t.interfaces name ();
        match info with
        | None -> ()
        | Some (kind, crc) ->
          Cmi_consistbl.check t.crc_interfaces name kind crc file_name)
      cmis
  with
  | Cmi_consistbl.Inconsistency
      { unit_name = name; inconsistent_source = user; original_source = auth }
  ->
    raise (Error (Inconsistent_interface (name, user, auth)))

let check_cmx_consistency t file_name cmxs =
  try
    Array.iter
      (fun import ->
        let name = Import_info.cu import in
        let crco = Import_info.crc import in
        t.implementations <- name :: t.implementations;
        match crco with
        | None ->
          if List.mem name t.cmx_required
          then raise (Error (Missing_cmx (file_name, name)))
        | Some crc ->
          Cmx_consistbl.check t.crc_implementations name () crc file_name)
      cmxs
  with
  | Cmx_consistbl.Inconsistency
      { unit_name = name; inconsistent_source = user; original_source = auth }
  ->
    raise (Error (Inconsistent_implementation (name, user, auth)))

let check_consistency t ~unit cmis cmxs =
  check_cmi_consistency t unit.file_name cmis;
  check_cmx_consistency t unit.file_name cmxs;
  let ui_unit = CU.name unit.name in
  (try
     let source = CU.Tbl.find t.implementations_defined unit.name in
     raise (Error (Multiple_definition (ui_unit, unit.file_name, source)))
   with Not_found -> ());
  t.implementations <- unit.name :: t.implementations;
  Cmx_consistbl.check t.crc_implementations unit.name () unit.crc unit.file_name;
  CU.Tbl.replace t.implementations_defined unit.name unit.file_name;
  if CU.is_packed unit.name then t.cmx_required <- unit.name :: t.cmx_required

let extract_crc_interfaces t =
  CU.Name.Tbl.fold
    (fun name () crcs ->
      let crc_with_unit = Cmi_consistbl.find t.crc_interfaces name in
      Import_info.Intf.create name crc_with_unit :: crcs)
    t.interfaces []

let extract_crc_implementations t =
  Cmx_consistbl.extract t.implementations t.crc_implementations
  |> List.map (fun (cu, crc) ->
      let crc = Option.map (fun ((), crc) -> crc) crc in
      Import_info.create_normal cu ~crc)

(* Add C objects and options and "custom" info from a library descriptor. See
   bytecomp/bytelink.ml for comments on the order of C objects. *)

let add_ccobjs t origin (l : Cmx_format.library_infos) =
  if not !Clflags.no_auto_link
  then (
    t.lib_ccobjs <- l.lib_ccobjs @ t.lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    t.lib_ccopts <- List.map replace_origin l.lib_ccopts @ t.lib_ccopts)

let is_required t name =
  try
    ignore (Hashtbl.find t.missing_globals name);
    true
  with Not_found -> false

let add_required t by import =
  let name = Import_info.cu import in
  try
    let rq = Hashtbl.find t.missing_globals name in
    rq := by :: !rq
  with Not_found -> Hashtbl.add t.missing_globals name (ref [by])

let remove_required t name = Hashtbl.remove t.missing_globals name

let extract_missing_globals t =
  let mg = ref [] in
  let fmt = function
    | file, None -> file
    | file, Some part -> Format.asprintf "%s(%a)" file CU.Name.print part
  in
  Hashtbl.iter
    (fun md rq -> mg := (md, List.map fmt !rq) :: !mg)
    t.missing_globals;
  !mg

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so no
     module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

let make_globals_map t units_list =
  (* The order in which entries appear in the globals map does not matter (see
     the natdynlink code). *)
  let find_crc name =
    Cmi_consistbl.find t.crc_interfaces name
    |> Option.map (fun (_unit, crc) -> crc)
  in
  let interfaces = CU.Name.Tbl.copy t.interfaces in
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

let lib_ccobjs t = t.lib_ccobjs

let lib_ccopts t = t.lib_ccopts

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name -> fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
    fprintf ppf "The file %a is not a compilation unit description"
      Location.print_filename name
  | Missing_implementations l ->
    let print_references ppf = function
      | [] -> ()
      | r1 :: rl ->
        fprintf ppf "%s" r1;
        List.iter (fun r -> fprintf ppf ",@ %s" r) rl
    in
    let print_modules ppf =
      List.iter (fun (md, rq) ->
          fprintf ppf "@ @[<hov 2>%a referenced from %a@]" CU.print md
            print_references rq)
    in
    fprintf ppf
      "@[<v 2>No implementations provided for the following modules:%a@]"
      print_modules l
  | Inconsistent_interface (intf, file1, file2) ->
    fprintf ppf
      "@[<hov>Files %a@ and %a@ make inconsistent assumptions over interface \
       %a@]"
      Location.print_filename file1 Location.print_filename file2 CU.Name.print
      intf
  | Inconsistent_implementation (intf, file1, file2) ->
    fprintf ppf
      "@[<hov>Files %a@ and %a@ make inconsistent assumptions over \
       implementation %a@]"
      Location.print_filename file1 Location.print_filename file2 CU.print intf
  | Multiple_definition (modname, file1, file2) ->
    fprintf ppf "@[<hov>Files %a@ and %a@ both define a module named %a@]"
      Location.print_filename file1 Location.print_filename file2 CU.Name.print
      modname
  | Missing_cmx (filename, name) ->
    fprintf ppf
      "@[<hov>File %a@ was compiled without access@ to the .cmx file@ for \
       module %a,@ which was produced by `ocamlopt -for-pack'.@ Please \
       recompile %a@ with the correct `-I' option@ so that %a.cmx@ is found.@]"
      Location.print_filename filename CU.print name Location.print_filename
      filename CU.print name
  | Linking_error exitcode ->
    fprintf ppf "Error during linking (exit code %d)" exitcode
  | Archiver_error name ->
    fprintf ppf "Error while creating the library %s" name
  | Metaprogramming_not_supported_by_backend filename ->
    fprintf ppf
      "@[<hov>The file %a@ can only be compiled with a backend with support \
       for metaprogramming@]"
      Location.print_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
