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

(* Globals for quotations *)

let quoted_globals = ref CU.Name.Set.empty

let add_quoted_globals globals =
  quoted_globals
    := List.fold_left
         (fun globals global -> CU.Name.Set.add global globals)
         !quoted_globals globals

let get_quoted_globals () =
  if not !Clflags.nopervasives
  then
    quoted_globals
      := CU.Name.Set.add (CU.Name.of_string "Stdlib") !quoted_globals;
  !quoted_globals

(* Consistency check between interfaces and implementations: *)

let crc_interfaces = ref (Cmi_consistbl.create ())

let interfaces = ref (CU.Name.Tbl.create 100)

let crc_implementations = ref (Cmx_consistbl.create ())

let implementations = ref ([] : CU.t list)

let implementations_defined = ref (CU.Tbl.create 100)

let cmx_required = ref ([] : CU.t list)

let check_cmi_consistency file_name cmis =
  try
    Array.iter
      (fun import ->
        let name = Import_info.name import in
        let info = Import_info.Intf.info import in
        CU.Name.Tbl.replace !interfaces name ();
        match info with
        | None -> ()
        | Some (kind, crc) ->
          Cmi_consistbl.check !crc_interfaces name kind crc file_name)
      cmis
  with
  | Cmi_consistbl.Inconsistency
      { unit_name = name; inconsistent_source = user; original_source = auth }
  ->
    raise (Error (Inconsistent_interface (name, user, auth)))

let check_cmx_consistency file_name cmxs =
  try
    Array.iter
      (fun import ->
        let name = Import_info.cu import in
        let crco = Import_info.crc import in
        implementations := name :: !implementations;
        match crco with
        | None ->
          if List.mem name !cmx_required
          then raise (Error (Missing_cmx (file_name, name)))
        | Some crc ->
          Cmx_consistbl.check !crc_implementations name () crc file_name)
      cmxs
  with
  | Cmx_consistbl.Inconsistency
      { unit_name = name; inconsistent_source = user; original_source = auth }
  ->
    raise (Error (Inconsistent_implementation (name, user, auth)))

let check_consistency ~unit cmis cmxs =
  check_cmi_consistency unit.file_name cmis;
  check_cmx_consistency unit.file_name cmxs;
  let ui_unit = CU.name unit.name in
  (try
     let source = CU.Tbl.find !implementations_defined unit.name in
     raise (Error (Multiple_definition (ui_unit, unit.file_name, source)))
   with Not_found -> ());
  implementations := unit.name :: !implementations;
  Cmx_consistbl.check !crc_implementations unit.name () unit.crc unit.file_name;
  CU.Tbl.replace !implementations_defined unit.name unit.file_name;
  if CU.is_packed unit.name then cmx_required := unit.name :: !cmx_required

let extract_crc_interfaces () =
  CU.Name.Tbl.fold
    (fun name () crcs ->
      let crc_with_unit = Cmi_consistbl.find !crc_interfaces name in
      Import_info.Intf.create name crc_with_unit :: crcs)
    !interfaces []

let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations !crc_implementations
  |> List.map (fun (cu, crc) ->
         let crc = Option.map (fun ((), crc) -> crc) crc in
         Import_info.create_normal cu ~crc)

(* Add C objects and options and "custom" info from a library descriptor. See
   bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []

let lib_ccopts = ref []

let missing_globals =
  ref
    (Hashtbl.create 17 : (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t)

let check_consistency = check_consistency

let add_ccobjs origin l =
  if not !Clflags.no_auto_link
  then (
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts)

let is_required name =
  try
    ignore (Hashtbl.find !missing_globals name);
    true
  with Not_found -> false

let add_required by import =
  let name = Import_info.cu import in
  try
    let rq = Hashtbl.find !missing_globals name in
    rq := by :: !rq
  with Not_found -> Hashtbl.add !missing_globals name (ref [by])

let remove_required name = Hashtbl.remove !missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  let fmt = function
    | file, None -> file
    | file, Some part -> Format.asprintf "%s(%a)" file CU.Name.print part
  in
  Hashtbl.iter
    (fun md rq -> mg := (md, List.map fmt !rq) :: !mg)
    !missing_globals;
  !mg

(* CR mshinwell: let's consider removing the global state from this module. The
   following can form a basis for [type t]. *)

type snapshot =
  { missing_globals : (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t;
    crc_interfaces : Cmi_consistbl.t;
    crc_implementations : Cmx_consistbl.t;
    implementations_defined : string CU.Tbl.t;
    cmx_required : CU.t list;
    interfaces : unit CU.Name.Tbl.t;
    implementations : CU.t list;
    lib_ccobjs : string list;
    lib_ccopts : string list;
    quoted_globals : CU.Name.Set.t
  }

let save_snapshot () =
  { missing_globals = Hashtbl.copy !missing_globals;
    crc_interfaces = Cmi_consistbl.copy !crc_interfaces;
    crc_implementations = Cmx_consistbl.copy !crc_implementations;
    implementations_defined = CU.Tbl.copy !implementations_defined;
    cmx_required = !cmx_required;
    interfaces = CU.Name.Tbl.copy !interfaces;
    implementations = !implementations;
    lib_ccobjs = !lib_ccobjs;
    lib_ccopts = !lib_ccopts;
    quoted_globals = !quoted_globals
  }

let restore_snapshot snapshot =
  missing_globals := Hashtbl.copy snapshot.missing_globals;
  crc_interfaces := Cmi_consistbl.copy snapshot.crc_interfaces;
  crc_implementations := Cmx_consistbl.copy snapshot.crc_implementations;
  implementations_defined := CU.Tbl.copy snapshot.implementations_defined;
  cmx_required := snapshot.cmx_required;
  interfaces := CU.Name.Tbl.copy snapshot.interfaces;
  implementations := snapshot.implementations;
  lib_ccobjs := snapshot.lib_ccobjs;
  lib_ccopts := snapshot.lib_ccopts;
  quoted_globals := snapshot.quoted_globals

let reset () =
  (* CR-someday jvanburen: should we [Hashtbl.reset missing_globals;]? *)
  ignore !missing_globals;
  Cmi_consistbl.clear !crc_interfaces;
  Cmx_consistbl.clear !crc_implementations;
  CU.Tbl.reset !implementations_defined;
  cmx_required := [];
  CU.Name.Tbl.reset !interfaces;
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := [];
  quoted_globals := CU.Name.Set.empty

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so no
     module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

let make_globals_map units_list =
  (* The order in which entries appear in the globals map does not matter (see
     the natdynlink code). *)
  let find_crc name =
    Cmi_consistbl.find !crc_interfaces name
    |> Option.map (fun (_unit, crc) -> crc)
  in
  let interfaces_copy = CU.Name.Tbl.copy !interfaces in
  let defined =
    List.map
      (fun unit ->
        let name = CU.name unit.name in
        let intf_crc = find_crc name in
        CU.Name.Tbl.remove interfaces_copy name;
        let syms = List.map Symbol.for_compilation_unit unit.defines in
        unit.name, intf_crc, Some unit.crc, syms)
      units_list
  in
  CU.Name.Tbl.fold
    (fun name () globals_map ->
      let intf_crc = find_crc name in
      (assume_no_prefix name, intf_crc, None, []) :: globals_map)
    interfaces_copy defined

let lib_ccobjs () = !lib_ccobjs

let lib_ccopts () = !lib_ccopts

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
