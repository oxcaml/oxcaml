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
    imports_cmx : Import_info.t list;
    codegen_fingerprint : Codegen_variant.fingerprint;
    polled : bool;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

type objfile_to_link =
  { path : string;
    units : Compilation_unit.t list
  }

module Cmi_consistbl = Consistbl.Make (CU.Name) (Import_info.Intf.Nonalias.Kind)
module Cmx_consistbl = Consistbl.Make (CU) (Unit)

type error =
  | File_not_found of filepath
  | Companion_object_file_not_found_via_manifest of
      { object_filename : filepath;
        requested_filename : filepath
      }
  | Not_an_object_file of filepath
  | Missing_implementations of (Compilation_unit.t * string list) list
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.t * filepath * filepath
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.t
  | Linking_error of int
  | Archiver_error of string
  | Metaprogramming_not_supported_by_backend of filepath
  | Requires_metaprogramming_without_flag of filepath
  | Inconsistent_codegen_variant of
      { unit_name : Compilation_unit.t;
        filename : filepath;
        saved : Codegen_variant.fingerprint;
        current : Codegen_variant.fingerprint
      }
  | Codegen_variant_runtime_not_found of
      { filename : filepath;
        variant : string
      }
  | Metaprogramming_not_supported_under_codegen_variant of { variant : string }
  | Cached_generic_functions_not_found of
      { filename : filepath;
        variant : string
      }

exception Error of error

(* What a file says of the codegen variant that it was compiled for: the
   fingerprint of each of its units. *)
type file_codegen = { fingerprints : (CU.t * Codegen_variant.fingerprint) list }

type t =
  { crc_interfaces : Cmi_consistbl.t;
    crc_implementations : Cmx_consistbl.t;
    codegen_of_files : (filepath, file_codegen) Hashtbl.t;
    mutable implementations : CU.Set.t;
    mutable cmx_required : CU.Set.t;
    interfaces : unit CU.Name.Tbl.t;
    implementations_defined : string CU.Tbl.t;
    mutable quoted_cmi : CU.Name.Set.t;
    mutable quoted_cmx : CU.Set.t;
    mutable lib_ccobjs : filepath list;
    mutable lib_ccopts : string list;
    missing_globals : (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t
  }

let create () =
  let quoted_cmi, quoted_cmx =
    if !Clflags.nopervasives
    then CU.Name.Set.empty, CU.Set.empty
    else
      ( CU.Name.Set.singleton (CU.Name.of_string "Stdlib"),
        CU.Set.singleton (CU.of_string "Stdlib") )
  in
  { crc_interfaces = Cmi_consistbl.create ();
    crc_implementations = Cmx_consistbl.create ();
    codegen_of_files = Hashtbl.create 100;
    implementations = CU.Set.empty;
    cmx_required = CU.Set.empty;
    interfaces = CU.Name.Tbl.create 100;
    implementations_defined = CU.Tbl.create 100;
    quoted_cmi;
    quoted_cmx;
    lib_ccobjs = [];
    lib_ccopts = [];
    missing_globals = Hashtbl.create 17
  }

(* Globals for quotations *)

let add_quoted_cmi t cus =
  t.quoted_cmi
    <- List.fold_left (fun cus cu -> CU.Name.Set.add cu cus) t.quoted_cmi cus

let add_quoted_cmx t cus =
  t.quoted_cmx
    <- List.fold_left (fun cus cu -> CU.Set.add cu cus) t.quoted_cmx cus

let get_quoted_cmi t = t.quoted_cmi

let get_quoted_cmx t = t.quoted_cmx

(* Consistency check between interfaces and implementations: *)

(* The libraries of two codegen variants that differ in more than poll insertion
   do not have the same CRCs, so a file that was compiled for a variant with
   another fingerprint than this invocation's disagrees with the other files on
   what it imports. That is what to report then, whichever of the two files of
   an inconsistency it is: the check of each unit's fingerprint comes too late
   for an archive, whose imports are checked before any of its units is, and
   never for a unit that the link does not use. *)
let unit_of_another_variant t ~user ~auth =
  let current = Codegen_variant.current_fingerprint () in
  List.find_map
    (fun file_name ->
      match Hashtbl.find_opt t.codegen_of_files file_name with
      | None -> None
      | Some { fingerprints } ->
        List.find_map
          (fun (unit_name, saved) ->
            if Codegen_variant.equal_fingerprint saved current
            then None
            else
              Some
                (Inconsistent_codegen_variant
                   { unit_name; filename = file_name; saved; current }))
          fingerprints)
    [user; auth]

let interface_inconsistency t name ~user ~auth =
  Error
    (Option.value
       (unit_of_another_variant t ~user ~auth)
       ~default:(Inconsistent_interface (name, user, auth)))

let implementation_inconsistency t name ~user ~auth =
  Error
    (Option.value
       (unit_of_another_variant t ~user ~auth)
       ~default:(Inconsistent_implementation (name, user, auth)))

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
    raise (interface_inconsistency t name ~user ~auth)

let check_implementation_crc t name crc file_name =
  try Cmx_consistbl.check t.crc_implementations name () crc file_name
  with
  | Cmx_consistbl.Inconsistency
      { unit_name = name; inconsistent_source = user; original_source = auth }
  ->
    raise (implementation_inconsistency t name ~user ~auth)

let check_cmx_consistency t file_name cmxs =
  Array.iter
    (fun import ->
      let name = Import_info.cu import in
      let crco = Import_info.crc import in
      t.implementations <- CU.Set.add name t.implementations;
      match crco with
      | None ->
        if CU.Set.mem name t.cmx_required
        then raise (Error (Missing_cmx (file_name, name)))
      | Some crc -> check_implementation_crc t name crc file_name)
    cmxs

let check_codegen_variant_consistency ~unit =
  let current = Codegen_variant.current_fingerprint () in
  if not (Codegen_variant.equal_fingerprint unit.codegen_fingerprint current)
  then
    raise
      (Error
         (Inconsistent_codegen_variant
            { unit_name = unit.name;
              filename = unit.file_name;
              saved = unit.codegen_fingerprint;
              current
            }))

let check_library_consistency t file_name (infos : library_infos) =
  Hashtbl.replace t.codegen_of_files file_name
    { fingerprints =
        List.map
          (fun unit -> unit.li_name, unit.li_codegen_fingerprint)
          infos.lib_units
    };
  check_cmi_consistency t file_name infos.lib_imports_cmi;
  check_cmx_consistency t file_name infos.lib_imports_cmx

let check_consistency t ~unit cmis cmxs =
  check_codegen_variant_consistency ~unit;
  (* The file of a unit of an archive is the archive, which
     [check_library_consistency] has recorded with all its units. *)
  if not (Hashtbl.mem t.codegen_of_files unit.file_name)
  then
    Hashtbl.add t.codegen_of_files unit.file_name
      { fingerprints = [unit.name, unit.codegen_fingerprint] };
  check_cmi_consistency t unit.file_name cmis;
  check_cmx_consistency t unit.file_name cmxs;
  let ui_unit = CU.name unit.name in
  (try
     let source = CU.Tbl.find t.implementations_defined unit.name in
     raise (Error (Multiple_definition (ui_unit, unit.file_name, source)))
   with Not_found -> ());
  t.implementations <- CU.Set.add unit.name t.implementations;
  check_implementation_crc t unit.name unit.crc unit.file_name;
  CU.Tbl.replace t.implementations_defined unit.name unit.file_name;
  if CU.is_packed unit.name
  then t.cmx_required <- CU.Set.add unit.name t.cmx_required

let file_holds_unit t file_name unit_name =
  match Hashtbl.find_opt t.codegen_of_files file_name with
  | None -> false
  | Some { fingerprints } ->
    List.exists (fun (name, _) -> CU.equal name unit_name) fingerprints

let extract_crc_interfaces t =
  CU.Name.Tbl.fold
    (fun name () crcs ->
      let crc_with_unit = Cmi_consistbl.find t.crc_interfaces name in
      Import_info.Intf.create name crc_with_unit :: crcs)
    t.interfaces []

let extract_crc_implementations t =
  Cmx_consistbl.fold_map t.implementations ~init:[]
    ~f:(fun acc cu crc ->
      let crc = Option.map (fun ((), crc) -> crc) crc in
      Import_info.create_normal cu ~crc :: acc)
    t.crc_implementations

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
    | file, Some part -> Format_doc.asprintf "%s(%a)" file CU.Name.print part
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

open Format_doc

let report_error ppf = function
  | File_not_found name ->
    fprintf ppf "Cannot find file %a" Location.Doc.quoted_filename name
  | Companion_object_file_not_found_via_manifest
      { object_filename; requested_filename } ->
    fprintf ppf
      "@[<hov>Cannot find file %a,@ the object file accompanying %a:@ the \
       latter was resolved through a manifest entry (see -I-manifest),@ so its \
       object file must be listed as a separate manifest entry@]"
      Location.Doc.quoted_filename object_filename Location.Doc.quoted_filename
      requested_filename
  | Not_an_object_file name ->
    fprintf ppf "The file %a is not a compilation unit description"
      Location.Doc.quoted_filename name
  | Missing_implementations l ->
    let print_references ppf = function
      | [] -> ()
      | r1 :: rl ->
        fprintf ppf "%s" r1;
        List.iter (fun r -> fprintf ppf ",@ %s" r) rl
    in
    let print_modules ppf =
      List.iter (fun (md, rq) ->
          fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
            CU.print_as_inline_code md print_references rq)
    in
    fprintf ppf
      "@[<v 2>No implementations provided for the following modules:%a@]"
      print_modules l
  | Inconsistent_interface (intf, file1, file2) ->
    fprintf ppf
      "@[<hov>Files %a@ and %a@ make inconsistent assumptions over interface \
       %a@]"
      Location.Doc.quoted_filename file1 Location.Doc.quoted_filename file2
      CU.Name.print_as_inline_code intf
  | Inconsistent_implementation (intf, file1, file2) ->
    fprintf ppf
      "@[<hov>Files %a@ and %a@ make inconsistent assumptions over \
       implementation %a@]"
      Location.Doc.quoted_filename file1 Location.Doc.quoted_filename file2
      CU.print_as_inline_code intf
  | Multiple_definition (modname, file1, file2) ->
    fprintf ppf "@[<hov>Files %a@ and %a@ both define a module named %a@]"
      Location.Doc.quoted_filename file1 Location.Doc.quoted_filename file2
      CU.Name.print_as_inline_code modname
  | Missing_cmx (filename, name) ->
    fprintf ppf
      "@[<hov>File %a@ was compiled without access@ to the .cmx file@ for \
       module %a,@ which was produced by `ocamlopt -for-pack'.@ Please \
       recompile %a@ with the correct `-I' option@ so that %a.cmx@ is found.@]"
      Location.Doc.quoted_filename filename CU.print_as_inline_code name
      Location.Doc.quoted_filename filename CU.print_as_inline_code name
  | Linking_error exitcode ->
    fprintf ppf "Error during linking (exit code %d)" exitcode
  | Archiver_error name ->
    fprintf ppf "Error while creating the library %s" name
  | Metaprogramming_not_supported_by_backend filename ->
    fprintf ppf
      "@[<hov>The file %a@ can only be compiled with a backend with support \
       for metaprogramming@]"
      Location.Doc.quoted_filename filename
  | Requires_metaprogramming_without_flag filename ->
    fprintf ppf
      "@[<hov>The library %a@ requires metaprogramming support@ but \
       -uses-metaprogramming was not passed@]"
      Location.Doc.quoted_filename filename
  | Inconsistent_codegen_variant { unit_name; filename; saved; current } ->
    fprintf ppf
      "@[<hov>File %a,@ which contains unit %a,@ was compiled with codegen \
       configuration@ [%s],@ which is incompatible with this invocation's@ \
       [%s]%t@]"
      Location.Doc.quoted_filename filename CU.print_as_inline_code unit_name
      (Codegen_variant.string_of_fingerprint saved)
      (Codegen_variant.string_of_fingerprint current) (fun ppf ->
        Option.iter (fprintf ppf "@ (%s)")
          (Codegen_variant.missing_root_hint ()))
  | Codegen_variant_runtime_not_found { filename; variant } ->
    fprintf ppf
      "@[<hov>Cannot find the runtime library %a:@ this installation does not \
       provide codegen variant %s%t@]"
      Location.Doc.quoted_filename filename variant (fun ppf ->
        Option.iter (fprintf ppf "@ (%s)")
          (Codegen_variant.missing_root_hint ()))
  | Metaprogramming_not_supported_under_codegen_variant { variant } ->
    fprintf ppf
      "@[<hov>Programs using eval (metaprogramming) are not supported under \
       codegen variant %s:@ installations only provide compiler-libs for the \
       default codegen variant@]"
      variant
  | Cached_generic_functions_not_found { filename; variant } ->
    let generator_flag =
      match Codegen_variant.of_string variant with
      | Some _ -> " -codegen-variant " ^ variant
      | None -> ""
    in
    fprintf ppf
      "@[<hov>Cannot find %a:@ codegen variant '%s' of this installation does \
       not provide a cached-generic-functions archive;@ generate it with@ \
       @[<h>generate_cached_generic_functions.exe%s %s@]@ or drop \
       -use-cached-generic-functions@]"
      Location.Doc.quoted_filename filename variant generator_flag
      (Filename.quote filename)

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
