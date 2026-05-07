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
  | Missing_impl_for_quote of CU.t
(* This is currently unused as we can't ensure the cmx files are always
   available, specifically compiler-libs doesn't ship its cmx files. We should
   emit this again once all of the compiler artifacts do. *)

exception Error of error

let cmi_bundle ~quoted_cmi =
  CU.Name.Map.of_set
    (fun name ->
      let path =
        try Load_path.find_normalized (CU.Name.to_string name ^ ".cmi")
        with Not_found -> raise (Error (Missing_intf_for_quote name))
      in
      Cmi_format.read_cmi path)
    quoted_cmi

(* We can't populate this during scan_file because a cmxa contains less
   information than a cmx. *)
let cmx_bundle ~quoted_cmx =
  let cmxes = CU.Tbl.create 100 in
  let visited = CU.Tbl.create 100 in
  let rec find_all_cmxes cu =
    if CU.Tbl.mem visited cu
    then ()
    else begin
      CU.Tbl.add visited cu ();
      match Load_path.find_normalized (CU.base_filename cu ^ ".cmx") with
      | exception Not_found -> ()
      | path -> begin
        let unit_info, _crc = Compilenv.read_unit_info path in
        CU.Tbl.add cmxes cu unit_info;
        List.iter
          (fun import -> find_all_cmxes (Import_info.cu import))
          unit_info.ui_imports_cmx
        end
    end
  in
  CU.Set.iter find_all_cmxes quoted_cmx;
  ListLabels.map (CU.Tbl.to_list cmxes)
    ~f:(fun ((_, info) : CU.t * Cmx_format.unit_infos) ->
      let serialized_sections, toc, total_length =
        Oxcaml_utils.File_sections.serialize info.ui_file_sections
      in
      let raw_info : Cmx_format.unit_infos_raw =
        { uir_unit = info.ui_unit;
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
          uir_static_data = info.ui_static_data
        }
      in
      raw_info, serialized_sections)

let make_bundled_cm_file unix ~ppf_dump ~quoted_cmi ~quoted_cmx ~output_name
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
  Asmgen.compile_unit unix ~output_prefix:output_name ~asm_filename:bundled_cm
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
      let cont = bundle_cm "caml_bundled_cmis" (cmi_bundle ~quoted_cmi) [] in
      let cont = bundle_cm "caml_bundled_cmxs" (cmx_bundle ~quoted_cmx) cont in
      Asmgen.compile_phrase ~ppf_dump (Cmm.Cdata cont);
      Emit.end_assembly ());
  bundled_cm_obj
