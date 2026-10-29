(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Cmx_format

module CU = Compilation_unit

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of CU.t * CU.t * string
  | Recorded_cmx_unavailable of
      { unit : CU.t;
        cmx_path : Misc.filepath;
        filename : Misc.filepath option;
      }

exception Error of error

type unit_infos_builder =
  { mutable uib_unit : Compilation_unit.t;
    mutable uib_defines: Compilation_unit.t list;
    mutable uib_imports_cmx: Import_info.t list;
    mutable uib_generic_fns: generic_fns;
    mutable uib_export_info: Flambda2_cmx.Flambda_cmx_format.raw option;
    uib_zero_alloc_info: Zero_alloc_info.t;
    mutable uib_force_link : bool;
    mutable uib_requires_metaprogramming : bool;
    mutable uib_external_symbols : string list;
    mutable uib_lto_info :
      Flambda2_reaper.Lto_info.t File_sections.Idx.t option;
    uib_file_sections : File_sections.Builder.t;
  }

module Infos_table = CU.Tbl

(* What is known of the .cmx file of a unit looked up so far. *)
type cached_unit =
  | Opaque
  | Missing_from_load_path
      (* Not found in the load path, which is only searched without a recorded
         location: a lookup with one retries it. *)
  | Found of unit_infos * Misc.filepath option
      (* The description read, with the location to record on the identifiers
         read from it, if any (see [CU.Found]). *)

let global_infos_table = (Infos_table.create 17 : cached_unit Infos_table.t)

let reset_info_tables () =
  Infos_table.reset global_infos_table

module String = Misc.Stdlib.String

let cached_zero_alloc_info = Zero_alloc_info.create ()

let cache_zero_alloc_info c = Zero_alloc_info.merge c ~into:cached_zero_alloc_info

let current_unit =
  { uib_unit = CU.dummy;
    uib_defines = [];
    uib_imports_cmx = [];
    uib_generic_fns = { curry_fun = []; apply_fun = []; send_fun = [] };
    uib_export_info = None;
    uib_zero_alloc_info = Zero_alloc_info.create ();
    uib_force_link = false;
    uib_requires_metaprogramming = false;
    uib_external_symbols = [];
    uib_lto_info = None;
    uib_file_sections = File_sections.Builder.create 0;
  }

let current_zero_alloc_info () = current_unit.uib_zero_alloc_info

let current_generic_fns () = current_unit.uib_generic_fns

let current_sections () = current_unit.uib_file_sections

let reset unit_info =
  let compilation_unit = Unit_info.modname unit_info in
  Infos_table.clear global_infos_table;
  Zero_alloc_info.reset cached_zero_alloc_info;
  Env.set_current_unit unit_info;
  current_unit.uib_unit <- compilation_unit;
  current_unit.uib_defines <- [compilation_unit];
  current_unit.uib_imports_cmx <- [];
  current_unit.uib_generic_fns <-
    { curry_fun = []; apply_fun = []; send_fun = [] };
  current_unit.uib_export_info <- None;
  Zero_alloc_info.reset current_unit.uib_zero_alloc_info;
  current_unit.uib_force_link <- !Clflags.link_everything;
  current_unit.uib_requires_metaprogramming <-
    !Clflags.requires_metaprogramming;
  current_unit.uib_external_symbols <- [];
  current_unit.uib_lto_info <- None;
  File_sections.Builder.clear current_unit.uib_file_sections

let record_external_symbols () =
  current_unit.uib_external_symbols <- (List.filter_map (fun prim ->
      if not (Primitive.native_name_is_external prim) then None
      else Some (Primitive.native_name prim))
      !Translmod.primitive_declarations)

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let uir = (input_value ic : unit_infos_raw) in
    let first_section_offset = pos_in ic in
    seek_in ic (first_section_offset + uir.uir_sections_length);
    let crc = Digest.input ic in
    (* This consumes the channel *)
    let sections = File_sections.create uir.uir_section_toc filename ic ~first_section_offset in
    let ui = {
      ui_unit = uir.uir_unit;
      ui_defines = uir.uir_defines;
      ui_format = uir.uir_format;
      ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
      ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
      ui_quoted_cmi = uir.uir_quoted_cmi |> Array.to_list;
      ui_quoted_cmx = uir.uir_quoted_cmx |> Array.to_list;
      ui_generic_fns = uir.uir_generic_fns;
      ui_export_info = uir.uir_export_info;
      ui_zero_alloc_info = Zero_alloc_info.of_raw uir.uir_zero_alloc_info;
      ui_force_link = uir.uir_force_link;
      ui_requires_metaprogramming = uir.uir_requires_metaprogramming;
      ui_external_symbols = uir.uir_external_symbols |> Array.to_list;
      ui_static_data = uir.uir_static_data;
      ui_lto_info = uir.uir_lto_info;
      ui_file_sections = sections;
    }
    in
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmxa_magic_number) in
    if buffer <> cmxa_magic_number then
      raise(Error(Not_a_unit_info filename));
    let infos = (input_value ic : library_infos) in
    close_in ic;
    infos
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))


(* Read and cache info on global identifiers *)

let equal_args arg1 arg2 =
  let ({ param = name1; value = value1 } : CU.argument) = arg1 in
  let ({ param = name2; value = value2 } : CU.argument) = arg2 in
  CU.Name.equal name1 name2 && CU.equal value1 value2

let equal_up_to_pack_prefix cu1 cu2 =
  CU.Name.equal (CU.name cu1) (CU.name cu2)
  && List.equal equal_args (CU.instance_arguments cu1) (CU.instance_arguments cu2)

let export_info_of_unit infos ~cmx_path =
  Option.map
    (fun export_info ->
      Profile.record_call ~accumulate:true "cmx_from_raw" (fun () ->
      Flambda2_cmx.Flambda_cmx_format.from_raw
        ~sections:infos.ui_file_sections
        ~cmx_path
        export_info))
    infos.ui_export_info

let get_export_info infos =
  export_info_of_unit infos ~cmx_path:None

let recorded_cmx_path ~cmx_path =
  if !Clflags.use_persistent_deps then cmx_path else None

(* The file to read the .cmx of [comp_unit] from, with its location relative to
   the manifest files root when known. A recorded location is authoritative:
   the .cmx is loaded from there without consulting the load path, and failing
   that is an error rather than a reason to search the load path. *)
let locate_cmx comp_unit ~cmx_path ~extension =
  match recorded_cmx_path ~cmx_path with
  | Some cmx_path ->
    let filename = Load_path.resolve_manifest_location cmx_path in
    begin match filename with
    | Some filename when Sys.file_exists filename ->
      Some (filename, Some cmx_path)
    | Some _ | None ->
      raise (Error (Recorded_cmx_unavailable
                      { unit = comp_unit; cmx_path; filename }))
    end
  | None ->
    let basename = CU.base_filename comp_unit ^ "." ^ extension in
    match Load_path.find_normalized_with_origin basename with
    | { path; manifest_location; include_flags = _ } ->
      Some (path, manifest_location)
    | exception Not_found -> None

let get_unit comp_unit ~cmx_path =
  (* If this fails, it likely means that someone didn't call
     [CU.which_cmx_file]. *)
  assert (CU.can_access_cmx_file comp_unit ~accessed_by:current_unit.uib_unit);
  (* CR lmaurer: Surely this should just compare [comp_unit] to
     [current_unit.ui_unit], but doing so seems to break Closure. We should fix
     that. *)
  if equal_up_to_pack_prefix comp_unit current_unit.uib_unit
  then
    Misc.fatal_error
      "get_unit: unable to get unit_info for current unit";
  match Infos_table.find global_infos_table comp_unit with
  | Found (ui, cmx_path) -> Some (ui, cmx_path)
  | Opaque -> None
  | Missing_from_load_path when Option.is_none (recorded_cmx_path ~cmx_path) ->
    None
  | Missing_from_load_path | exception Not_found ->
    let (cached, crc) =
      if Env.is_imported_opaque (CU.name comp_unit) then (Opaque, None)
      else begin
        let missing_extension =
          match Clflags.jsir_target () with
          | None -> "cmx"
          | Some Js -> "cmx-js"
          | Some Wasm -> "cmx-wasm"
        in
        match locate_cmx comp_unit ~cmx_path ~extension:missing_extension with
        | Some (filename, manifest_location) ->
          let (ui, crc) = read_unit_info filename in
          if not (CU.equal ui.ui_unit comp_unit) then
            raise(Error(Illegal_renaming(comp_unit, ui.ui_unit, filename)));
          cache_zero_alloc_info ui.ui_zero_alloc_info;
          (* Under -persistent-deps, where the .cmx was found is recorded on
             the identifiers read from it, so that the users of the current
             unit's .cmx load it from there. *)
          let cmx_path =
            if !Clflags.persistent_deps then manifest_location else None
          in
          (Found (ui, cmx_path), Some crc)
        | None ->
          let warn =
            Warnings.No_cmx_file
              { missing_extension
              ; module_name = CU.full_path_as_string comp_unit }
          in
          Location.prerr_warning Location.none warn;
          (Missing_from_load_path, None)
        end
    in
    let import = Import_info.create_normal comp_unit ~crc in
    current_unit.uib_imports_cmx <-
      (* A retry replaces the entry added by the miss it retries. *)
      import
      :: List.filter
           (fun old -> not (CU.equal (Import_info.cu old) comp_unit))
           current_unit.uib_imports_cmx;
    Infos_table.replace global_infos_table comp_unit cached;
    match cached with
    | Found (ui, cmx_path) -> Some (ui, cmx_path)
    | Opaque | Missing_from_load_path -> None

let get_unit_export_info found =
  Option.bind
    (get_unit (CU.Found.unit found) ~cmx_path:(CU.Found.cmx_path found))
    (fun (infos, cmx_path) -> export_info_of_unit infos ~cmx_path)

let get_static_data comp_unit =
  Option.map
    (fun (ui, _cmx_path) ->
      Slambda.CU_data.read ui.ui_static_data ~sections:ui.ui_file_sections)
    (get_unit comp_unit ~cmx_path:None)

let which_cmx_file found =
  CU.Found.which_cmx_file found ~accessed_by:(Current_unit.get_cu_exn ())

let get_global_export_info found =
  get_unit_export_info (which_cmx_file found)

let cache_unit_info ui =
  cache_zero_alloc_info ui.ui_zero_alloc_info;
  Infos_table.add global_infos_table ui.ui_unit
    (Found (ui, None))

(* Exporting cross-module information *)

let set_export_info export_info =
  current_unit.uib_export_info <- Some export_info

let set_lto_info lto_info =
  current_unit.uib_lto_info <- Some
    (File_sections.Builder.add current_unit.uib_file_sections lto_info)

(* Record that a currying function or application function is needed *)

let need_curry_fun kind arity result =
  let fns = current_unit.uib_generic_fns in
  if not (List.mem (kind, arity, result) fns.curry_fun) then
    current_unit.uib_generic_fns <-
      { fns with curry_fun = (kind, arity, result) :: fns.curry_fun }

let need_apply_fun arity result mode =
  assert(List.compare_length_with arity 0 > 0);
  let fns = current_unit.uib_generic_fns in
  if not (List.mem (arity, result, mode) fns.apply_fun) then
    current_unit.uib_generic_fns <-
      { fns with apply_fun = (arity, result, mode) :: fns.apply_fun }

let need_send_fun arity result mode =
  let fns = current_unit.uib_generic_fns in
  if not (List.mem (arity, result, mode) fns.send_fun) then
    current_unit.uib_generic_fns <-
      { fns with send_fun = (arity, result, mode) :: fns.send_fun }

(* Write the description of the current unit *)

(* CR mshinwell: let's think about this later, quadratic algorithm

let ensure_sharing_between_cmi_and_cmx_imports cmi_imports cmx_imports =
  (* If a [CU.t] in the .cmx imports also occurs in the .cmi imports, use
     the one in the .cmi imports, to increase sharing.  (Such a [CU.t] in
     the .cmi imports may already have part of its value shared with the
     first [CU.Name.t] component in the .cmi imports, c.f.
     [Persistent_env.ensure_crc_sharing], so it's best to pick this [CU.t].) *)
  List.map (fun ((comp_unit, crc) as import) ->
      match
        List.find_map (function
            | _, None -> None
            | _, Some (comp_unit', _) ->
              if CU.equal comp_unit comp_unit' then Some comp_unit'
              else None)
          cmi_imports
      with
      | None -> import
      | Some comp_unit -> comp_unit, crc)
    cmx_imports
*)

let write_unit_info info filename =
  let serialized_sections, toc, total_length =
    File_sections.serialize info.ui_file_sections
  in
  let raw_info = {
    uir_unit = info.ui_unit;
    uir_defines = info.ui_defines;
    uir_imports_cmi = Array.of_list info.ui_imports_cmi;
    uir_imports_cmx = Array.of_list info.ui_imports_cmx;
    uir_quoted_cmi = Array.of_list info.ui_quoted_cmi;
    uir_quoted_cmx = Array.of_list info.ui_quoted_cmx;
    uir_format = info.ui_format;
    uir_generic_fns = info.ui_generic_fns;
    uir_export_info = info.ui_export_info;
    uir_zero_alloc_info = Zero_alloc_info.to_raw info.ui_zero_alloc_info;
    uir_force_link = info.ui_force_link;
    uir_requires_metaprogramming = info.ui_requires_metaprogramming;
    uir_section_toc = toc;
    uir_external_symbols = Array.of_list info.ui_external_symbols;
    uir_static_data = info.ui_static_data;
    uir_lto_info = info.ui_lto_info;
    uir_sections_length = total_length;
  } in
  Misc.protect_output_to_file filename (fun oc ->
  output_string oc cmx_magic_number;
  output_value oc raw_info;
  Array.iter (output_string oc) serialized_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc)

let build_unit_info ~main_module_block_format ~static_data =
  let quoted_intfs = Env.quoted_intfs () in
  let quoted_intfs_and_deps = Env.loaded_transitive_dependencies quoted_intfs in
  let static_data =
    Slambda.CU_data.write
      ~sections:current_unit.uib_file_sections
      static_data
  in
  (* We could have [set_main_module_block_format] instead of passing it in as
     an argument but, unlike most of the state that this module keeps track
     of, it's not a value that gets accumulated over time, it just gets
     computed once. (Arguably we should remove [set_export_info] by the same
     reasoning.) *)
  { ui_unit = current_unit.uib_unit;
    ui_defines = current_unit.uib_defines;
    ui_imports_cmi = Env.imports();
    ui_imports_cmx = current_unit.uib_imports_cmx;
    ui_quoted_cmi = CU.Name.Set.to_list quoted_intfs_and_deps;
    ui_quoted_cmx = CU.Set.to_list (Env.quoted_impls ());
    ui_format = main_module_block_format;
    ui_generic_fns = current_unit.uib_generic_fns;
    ui_export_info = current_unit.uib_export_info;
    ui_zero_alloc_info = current_unit.uib_zero_alloc_info;
    ui_force_link = current_unit.uib_force_link;
    ui_requires_metaprogramming = current_unit.uib_requires_metaprogramming;
    ui_static_data = static_data;
    ui_external_symbols = current_unit.uib_external_symbols;
    ui_lto_info = current_unit.uib_lto_info;
    ui_file_sections =
      File_sections.Builder.build current_unit.uib_file_sections;
  }

let save_unit_info filename ~main_module_block_format ~static_data =
  let current_unit =
    build_unit_info ~main_module_block_format ~static_data
  in
  write_unit_info current_unit filename

let new_const_symbol () =
  Current_unit.symbol_for_new_const ()
  |> Symbol.linkage_name
  |> Linkage_name.to_string

let require_global comp_unit =
  if equal_up_to_pack_prefix comp_unit current_unit.uib_unit
  then ()
  else
    ignore
      (get_unit
         (CU.which_cmx_file comp_unit ~accessed_by:(Current_unit.get_cu_exn ()))
         ~cmx_path:None
       : (unit_infos * Misc.filepath option) option)

let load_global global_ident =
  if equal_up_to_pack_prefix (CU.Found.unit global_ident) current_unit.uib_unit
  then ()
  else
    ignore
      (get_global_export_info global_ident
       : Flambda2_cmx.Flambda_cmx_format.t option)

(* Error report *)

open Format_doc

let report_error_doc ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.Doc.quoted_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
       Location.Doc.quoted_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %a when %a was expected"
        Location.Doc.quoted_filename filename
        CU.print_as_inline_code name
        CU.print_as_inline_code modname
  | Recorded_cmx_unavailable { unit; cmx_path; filename } ->
      fprintf ppf
        "@[<hov>The compilation unit description for %a@ was recorded at %a@ \
         relative to the manifest files root,@ but %t.@]"
        CU.print_as_inline_code unit
        Location.Doc.quoted_filename cmx_path
        (fun ppf ->
           match filename with
           | None ->
               fprintf ppf "no manifest files root is set@ (%a)"
                 Misc.Style.inline_code "MANIFEST_FILES_ROOT"
           | Some filename ->
               fprintf ppf "%a@ does not exist"
                 Location.Doc.quoted_filename filename)

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
