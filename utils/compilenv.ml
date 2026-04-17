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

exception Error of error

module Infos_table = Global_module.Name.Tbl

let global_infos_table =
  (Infos_table.create 17
   : (Lambda.main_module_block_format, Obj.t option) unit_infos_gen
     option Infos_table.t)

let reset_info_tables () =
  Infos_table.reset global_infos_table

module String = Misc.Stdlib.String

let exported_constants = Hashtbl.create 17

let cached_zero_alloc_info = Zero_alloc_info.create ()

let cache_zero_alloc_info c =
  Zero_alloc_info.merge c ~into:cached_zero_alloc_info

let current_unit  =
  { ui_unit = CU.dummy;
    ui_defines = [];
    ui_arg_descr = None;
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_quoted_globals = [];
    ui_format = ();
    ui_generic_fns = { curry_fun = []; apply_fun = []; send_fun = [] };
    ui_force_link = false;
    ui_requires_metaprogramming = false;
    ui_zero_alloc_info = Zero_alloc_info.create ();
    ui_export_info = None;
    ui_external_symbols = [];
    ui_static_data = Missing, (Slambda_types.Templates.empty_templates ());
  }

let reset unit_info =
  let compilation_unit = Unit_info.modname unit_info in
  Infos_table.clear global_infos_table;
  Zero_alloc_info.reset cached_zero_alloc_info;
  Env.set_unit_name (Some unit_info);
  current_unit.ui_unit <- compilation_unit;
  current_unit.ui_defines <- [compilation_unit];
  current_unit.ui_arg_descr <- None;
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_quoted_globals <- [];
  current_unit.ui_format <- ();
  current_unit.ui_generic_fns <-
    { curry_fun = []; apply_fun = []; send_fun = [] };
  current_unit.ui_force_link <- !Clflags.link_everything;
  current_unit.ui_requires_metaprogramming <-
    !Clflags.requires_metaprogramming;
  Zero_alloc_info.reset current_unit.ui_zero_alloc_info;
  Hashtbl.clear exported_constants;
  current_unit.ui_export_info <- None;
  current_unit.ui_external_symbols <- []

let record_external_symbols () =
  current_unit.ui_external_symbols <- (List.filter_map (fun prim ->
      if not (Primitive.native_name_is_external prim) then None
      else Some (Primitive.native_name prim))
      !Translmod.primitive_declarations)

let current_unit_infos () =
  current_unit

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let uir = (input_value ic : Obj.t option unit_infos_raw) in
    let first_section_offset = pos_in ic in
    seek_in ic (first_section_offset + uir.uir_sections_length);
    let crc = Digest.input ic in
    (* This consumes the channel *)
    let sections =
      File_sections.create uir.uir_section_toc filename ic ~first_section_offset
    in
    let ui = {
      ui_unit = uir.uir_unit;
      ui_defines = uir.uir_defines;
      ui_format = uir.uir_format;
      ui_arg_descr = uir.uir_arg_descr;
      ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
      ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
      ui_quoted_globals = uir.uir_quoted_globals |> Array.to_list;
      ui_generic_fns = uir.uir_generic_fns;
      ui_export_info = uir.uir_export_info;
      ui_zero_alloc_info = Zero_alloc_info.of_raw uir.uir_zero_alloc_info;
      ui_force_link = uir.uir_force_link;
      ui_requires_metaprogramming = uir.uir_requires_metaprogramming;
      ui_external_symbols = uir.uir_external_symbols |> Array.to_list;
      ui_static_data = uir.uir_static_data;
    }
    in
    (ui, sections, crc)
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
  && List.equal equal_args
      (CU.instance_arguments cu1)
      (CU.instance_arguments cu2)

let load_unit_infos ~warn_on_missing comp_unit =
  assert (CU.can_access_cmx_file comp_unit ~accessed_by:current_unit.ui_unit);
  if equal_up_to_pack_prefix comp_unit current_unit.ui_unit
  then
    ()
  else begin
    let name = CU.to_global_name_without_prefix comp_unit in
    if not (Infos_table.mem global_infos_table name) then begin
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
            let (ui, sections, crc) = read_unit_info filename in
            if not (CU.equal ui.ui_unit comp_unit) then
              raise(Error(Illegal_renaming(comp_unit, ui.ui_unit, filename)));
            cache_zero_alloc_info ui.ui_zero_alloc_info;
            (* Pair raw export info with its sections so the
               middle-end can interpret them later. *)
            let ui = { ui with ui_export_info =
              Option.map
                (fun raw_ei -> Obj.repr (raw_ei, sections))
                ui.ui_export_info }
            in
            (Some ui, Some crc)
          with Not_found ->
            if warn_on_missing then begin
              let warn =
                Warnings.No_cmx_file
                  { missing_extension
                  ; module_name = Global_module.Name.to_string name }
              in
              Location.prerr_warning Location.none warn
            end;
            (None, None)
          end
      in
      let import = Import_info.create_normal comp_unit ~crc in
      current_unit.ui_imports_cmx <- import :: current_unit.ui_imports_cmx;
      Infos_table.add global_infos_table name infos
    end
  end

let ensure_unit_loaded comp_unit =
  load_unit_infos ~warn_on_missing:true comp_unit

let try_load_unit comp_unit =
  load_unit_infos ~warn_on_missing:false comp_unit

let get_cached_static_data comp_unit =
  if equal_up_to_pack_prefix comp_unit current_unit.ui_unit
  then
    current_unit.ui_static_data
  else begin
    let name = CU.to_global_name_without_prefix comp_unit in
    match Infos_table.find global_infos_table name with
    | Some ui -> ui.ui_static_data
    | None | exception Not_found ->
      Slambda_types.Or_missing.Missing,
        (Slambda_types.Templates.empty_templates ())
  end

let get_cached_export_info comp_unit =
  if equal_up_to_pack_prefix comp_unit current_unit.ui_unit
  then
    current_unit.ui_export_info
  else begin
    let name = CU.to_global_name_without_prefix comp_unit in
    try
      let ui = Infos_table.find global_infos_table name in
      Option.bind ui (fun ui -> ui.ui_export_info)
    with Not_found -> None
  end

let which_cmx_file comp_unit =
  CU.which_cmx_file comp_unit ~accessed_by:(CU.get_current_exn ())

let cache_unit_info ui =
  cache_zero_alloc_info ui.ui_zero_alloc_info;
  Infos_table.add global_infos_table
    (ui.ui_unit |> CU.to_global_name_without_prefix) (Some ui)

(* Record that a currying function or application function is needed *)

let need_curry_fun kind arity result =
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (kind, arity, result) fns.curry_fun) then
    current_unit.ui_generic_fns <-
      { fns with curry_fun = (kind, arity, result) :: fns.curry_fun }

let need_apply_fun arity result mode =
  assert(List.compare_length_with arity 0 > 0);
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (arity, result, mode) fns.apply_fun) then
    current_unit.ui_generic_fns <-
      { fns with apply_fun = (arity, result, mode) :: fns.apply_fun }

let need_send_fun arity result mode =
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (arity, result, mode) fns.send_fun) then
    current_unit.ui_generic_fns <-
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

let write_unit_info info ~export_info_sections filename =
  let serialized_sections, toc, total_length =
    File_sections.serialize export_info_sections
  in
  let raw_info = {
    uir_unit = info.ui_unit;
    uir_defines = info.ui_defines;
    uir_arg_descr = info.ui_arg_descr;
    uir_imports_cmi = Array.of_list info.ui_imports_cmi;
    uir_imports_cmx = Array.of_list info.ui_imports_cmx;
    uir_quoted_globals = Array.of_list info.ui_quoted_globals;
    uir_format = info.ui_format;
    uir_generic_fns = info.ui_generic_fns;
    uir_export_info = info.ui_export_info;
    uir_zero_alloc_info = Zero_alloc_info.to_raw info.ui_zero_alloc_info;
    uir_force_link = info.ui_force_link;
    uir_requires_metaprogramming = info.ui_requires_metaprogramming;
    uir_section_toc = toc;
    uir_sections_length = total_length;
    uir_external_symbols = Array.of_list info.ui_external_symbols;
    uir_static_data = info.ui_static_data;
  } in
  Misc.protect_output_to_file filename (fun oc ->
  output_string oc cmx_magic_number;
  output_value oc raw_info;
  Array.iter (output_string oc) serialized_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc)

let save_unit_info filename ~main_module_block_format ~arg_descr ~static_data =
  current_unit.ui_imports_cmi <- Env.imports();
  current_unit.ui_quoted_globals <- Env.quoted_globals();
  current_unit.ui_arg_descr <- arg_descr;
  current_unit.ui_static_data <- static_data;
  let export_info, export_info_sections =
    match current_unit.ui_export_info with
    | None -> None, File_sections.empty
    | Some packed ->
      let (raw_ei, sections) =
        (Obj.obj packed : Obj.t * File_sections.t)
      in
      Some raw_ei, sections
  in
  write_unit_info
    { current_unit with
      ui_format = main_module_block_format;
      ui_export_info = export_info }
    ~export_info_sections
    filename

let new_const_symbol () =
  Symbol.for_new_const_in_current_unit ()
  |> Symbol.linkage_name
  |> Linkage_name.to_string

let require_global global_ident =
  ensure_unit_loaded (which_cmx_file global_ident)

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
