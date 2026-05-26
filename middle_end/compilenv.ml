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

module File_sections = Oxcaml_utils.File_sections

module CU = Compilation_unit

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of CU.t * CU.t * string

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
    uib_file_sections : File_sections.Builder.t;
  }

module Infos_table = Global_module.Name.Tbl

let global_infos_table =
  (Infos_table.create 17 : unit_infos option Infos_table.t)

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
    uib_file_sections = File_sections.Builder.create 0;
  }

let current_zero_alloc_info () = current_unit.uib_zero_alloc_info

let current_generic_fns () = current_unit.uib_generic_fns

let current_sections () = current_unit.uib_file_sections

let reset unit_info =
  let compilation_unit = Unit_info.modname unit_info in
  Infos_table.clear global_infos_table;
  Zero_alloc_info.reset cached_zero_alloc_info;
  Env.set_unit_name (Some unit_info);
  current_unit.uib_unit <- compilation_unit;
  current_unit.uib_defines <- [compilation_unit];
  current_unit.uib_force_link <- !Clflags.link_everything;
  current_unit.uib_requires_metaprogramming <-
    !Clflags.requires_metaprogramming;
  Zero_alloc_info.reset current_unit.uib_zero_alloc_info;
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
      ui_arg_descr = uir.uir_arg_descr;
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
      ui_file_sections = sections;
    }
    in
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let equal_args arg1 arg2 =
  let ({ param = name1; value = value1 } : CU.argument) = arg1 in
  let ({ param = name2; value = value2 } : CU.argument) = arg2 in
  CU.Name.equal name1 name2 && CU.equal value1 value2

let equal_up_to_pack_prefix cu1 cu2 =
  CU.Name.equal (CU.name cu1) (CU.name cu2)
  && List.equal equal_args (CU.instance_arguments cu1) (CU.instance_arguments cu2)

let get_unit_export_info comp_unit =
  (* If this fails, it likely means that someone didn't call
     [CU.which_cmx_file]. *)
  assert (CU.can_access_cmx_file comp_unit ~accessed_by:current_unit.uib_unit);
  let of_infos infos =
    Option.map
      (fun export_info ->
        Flambda2_cmx.Flambda_cmx_format.from_raw
          ~sections:infos.ui_file_sections
          export_info)
      infos.ui_export_info
  in
  (* CR lmaurer: Surely this should just compare [comp_unit] to
     [current_unit.ui_unit], but doing so seems to break Closure. We should fix
     that. *)
  if equal_up_to_pack_prefix comp_unit current_unit.uib_unit
  then
    Misc.fatal_error
      "get_unit_export_info: unable to get unit_info for current unit"
  else begin
    let name = CU.to_global_name_without_prefix comp_unit in
    try
      let ui = Infos_table.find global_infos_table name in
      Option.bind ui of_infos
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque (CU.name comp_unit) then (None, None)
        else begin
          let missing_extension =
            match !Clflags.jsir with
            | false -> "cmx"
            | true -> "cmjx"
          in
          try
            let filename =
              Load_path.find_normalized
                (CU.base_filename comp_unit ^ "." ^ missing_extension) in
            let (ui, crc) = read_unit_info filename in
            if not (CU.equal ui.ui_unit comp_unit) then
              raise(Error(Illegal_renaming(comp_unit, ui.ui_unit, filename)));
            cache_zero_alloc_info ui.ui_zero_alloc_info;
            (Some ui, Some crc)
          with Not_found ->
            let warn =
              Warnings.No_cmx_file
                { missing_extension
                ; module_name = Global_module.Name.to_string name }
            in
            Location.prerr_warning Location.none warn;
            (None, None)
          end
      in
      let import = Import_info.create_normal comp_unit ~crc in
      current_unit.uib_imports_cmx <- import :: current_unit.uib_imports_cmx;
      Infos_table.add global_infos_table name infos;
      Option.bind infos of_infos
  end

let which_cmx_file comp_unit =
  CU.which_cmx_file comp_unit ~accessed_by:(CU.get_current_exn ())

let get_global_export_info comp_unit =
  get_unit_export_info (which_cmx_file comp_unit)

let cache_unit_info ui =
  cache_zero_alloc_info ui.ui_zero_alloc_info;
  Infos_table.add global_infos_table
    (ui.ui_unit |> CU.to_global_name_without_prefix) (Some ui)

(* Exporting cross-module information *)

let set_export_info export_info =
  current_unit.uib_export_info <- Some export_info

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
    uir_arg_descr = info.ui_arg_descr;
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
    uir_sections_length = total_length;
    uir_external_symbols = Array.of_list info.ui_external_symbols;
  } in
  Misc.protect_output_to_file filename (fun oc ->
  output_string oc cmx_magic_number;
  output_value oc raw_info;
  Array.iter (output_string oc) serialized_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc)

let save_unit_info filename ~main_module_block_format ~arg_descr =
  let quoted_intfs = Env.quoted_intfs () in
  let quoted_intfs_and_deps = Env.loaded_transitive_dependencies quoted_intfs in
  (* We could have [set_main_module_block_format] and [set_arg_descr] instead
     of passing these in as arguments but, unlike most of the state that this
     module keeps track of, they're not values that get accumulated over time,
     they just get computed once. (Arguably we should remove [set_export_info]
     by the same reasoning.) *)
  let current_unit =
    { ui_unit = current_unit.uib_unit;
      ui_defines = current_unit.uib_defines;
      ui_arg_descr = arg_descr;
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
      ui_file_sections =
        File_sections.Builder.build current_unit.uib_file_sections;
      ui_external_symbols = current_unit.uib_external_symbols;
    }
  in
  write_unit_info current_unit filename

let new_const_symbol () =
  Symbol.for_new_const_in_current_unit ()
  |> Symbol.linkage_name
  |> Linkage_name.to_string

let require_global global_ident =
  if equal_up_to_pack_prefix global_ident current_unit.uib_unit
  then ()
  else ignore
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

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
