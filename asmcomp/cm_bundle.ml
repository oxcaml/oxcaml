(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* CR mshinwell: This file has not been code reviewed *)

module CU = Compilation_unit

type error =
  | Missing_intf_for_quote of CU.Name.t
  | Missing_impl_for_quote of CU.Name.t

exception Error of error

let no_such_module = CU.Name.of_string "No_such_module"

let cmi_bundle ~quoted_globals =
  let rec loop cmis missing_globals =
    match missing_globals with
    | [] -> cmis
    | global :: missing_globals ->
      if CU.Name.Map.mem global cmis || CU.Name.equal global no_such_module
      then loop cmis missing_globals
      else
        let path =
          try Load_path.find_normalized (CU.Name.to_string global ^ ".cmi")
          with Not_found -> raise (Error (Missing_intf_for_quote global))
        in
        let cmi = Cmi_format.read_cmi path in
        let missing_globals =
          Array.fold_left
            (fun missing_globals import ->
              Import_info.name import :: missing_globals)
            missing_globals cmi.cmi_crcs
        in
        let new_cmis = CU.Name.Map.add global cmi cmis in
        loop new_cmis missing_globals
  in
  loop CU.Name.Map.empty (CU.Name.Set.elements quoted_globals)

(* We can't populate this during scan_file because a cmxa contains less
   information than a cmx. *)
let cmx_bundle ~quoted_globals =
  let rec loop cmxs missing_globals =
    match missing_globals with
    | [] -> cmxs
    | global :: missing_globals ->
      if CU.Name.Map.mem global cmxs || CU.Name.equal global no_such_module
      then loop cmxs missing_globals
      else
        let path =
          try Load_path.find_normalized (CU.Name.to_string global ^ ".cmx")
          with Not_found -> raise (Error (Missing_impl_for_quote global))
        in
        let unit_info, _crc = Compilenv.read_unit_info path in
        let missing_globals =
          List.fold_left
            (fun missing_globals import ->
              Import_info.name import :: missing_globals)
            missing_globals unit_info.ui_imports_cmx
        in
        let new_cmxs = CU.Name.Map.add global unit_info cmxs in
        loop new_cmxs missing_globals
  in
  let unit_infos =
    loop CU.Name.Map.empty (CU.Name.Set.elements quoted_globals)
  in
  ListLabels.map (CU.Name.Map.data unit_infos)
    ~f:(fun (info : Cmx_format.unit_infos) ->
      let raw_export_info, sections =
        match info.ui_export_info with
        | None -> None, Oxcaml_utils.File_sections.empty
        | Some info ->
          let info, sections = Flambda2_cmx.Flambda_cmx_format.to_raw info in
          Some info, sections
      in
      let serialized_sections, toc, total_length =
        Oxcaml_utils.File_sections.serialize sections
      in
      let raw_info : Cmx_format.unit_infos_raw =
        { uir_unit = info.ui_unit;
          uir_defines = info.ui_defines;
          uir_arg_descr = info.ui_arg_descr;
          uir_imports_cmi = Array.of_list info.ui_imports_cmi;
          uir_imports_cmx = Array.of_list info.ui_imports_cmx;
          uir_quoted_globals = Array.of_list info.ui_quoted_globals;
          uir_format = info.ui_format;
          uir_generic_fns = info.ui_generic_fns;
          uir_export_info = raw_export_info;
          uir_zero_alloc_info = Zero_alloc_info.to_raw info.ui_zero_alloc_info;
          uir_force_link = info.ui_force_link;
          uir_section_toc = toc;
          uir_sections_length = total_length;
          uir_external_symbols = Array.of_list info.ui_external_symbols
        }
      in
      raw_info, serialized_sections)

let make_bundled_cm_file unix ~ppf_dump ~quoted_globals ~output_name
    ~named_startup_file =
  let bundled_cm =
    if named_startup_file
    then output_name ^ ".bundled_cm" ^ Config.ext_asm
    else Filename.temp_file "bundled_cm" Config.ext_asm
  in
  let sourcefile_for_dwarf =
    Some (if named_startup_file then bundled_cm else ".bundled_cm")
  in
  let bundled_cm_obj = Filename.temp_file "bundled_cm" ".cmx" in
  Asmgen.compile_unit ~output_prefix:output_name ~asm_filename:bundled_cm
    ~keep_asm:true (* TODO *)
    ~obj_filename:bundled_cm_obj ~may_reduce_heap:true ~ppf_dump (fun () ->
      Location.input_name := "caml_bundled_cm";
      let bundle_comp_unit =
        CU.create CU.Prefix.empty (CU.Name.of_string "_bundled_cm")
      in
      let bundle_unit_info =
        Unit_info.make_dummy ~input_name:"caml_bundled_cm" bundle_comp_unit
      in
      Compilenv.reset bundle_unit_info;
      Emitaux.Dwarf_helpers.init ~ppf_dump
        ~disable_dwarf:(not !Dwarf_flags.dwarf_for_startup_file)
        ~sourcefile:sourcefile_for_dwarf;
      Emit.begin_assembly unix;
      let bundle_cm name value cont =
        let symbol = { Cmm.sym_name = name; sym_global = Global } in
        let string = Marshal.to_string value [] in
        Cmm_helpers.emit_string_constant symbol string cont
      in
      let cont =
        bundle_cm "caml_bundled_cmis" (cmi_bundle ~quoted_globals) []
      in
      let cont =
        bundle_cm "caml_bundled_cmxs" (cmx_bundle ~quoted_globals) cont
      in
      Asmgen.compile_phrase ~ppf_dump (Cmm.Cdata cont);
      Emit.end_assembly ());
  bundled_cm_obj
