open Format
open Cmx_format
open Compilenv
module String = Misc.Stdlib.String
module CU = Compilation_unit

type filepath = string

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (CU.t * string list) list
  | Inconsistent_interface of CU.Name.t * filepath * filepath
  | Inconsistent_implementation of CU.t * filepath * filepath
  | Multiple_definition of CU.Name.t * filepath * filepath
  | Missing_cmx of filepath * CU.t
  | Linking_error of int

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

let check_consistency' ~unit cmis cmxs =
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

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  CU.Tbl.reset implementations_defined;
  cmx_required := [];
  CU.Name.Tbl.reset interfaces;
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []

(* Exported version for Asmlibrarian / Asmpackager *)
let check_consistency file_name u crc =
  let unit =
    { file_name;
      name = u.ui_unit;
      defines = u.ui_defines;
      crc;
      dynunit = None }
  in
  check_consistency' ~unit
    (Array.of_list u.ui_imports_cmi) (Array.of_list u.ui_imports_cmx)

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so
     no module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

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
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
