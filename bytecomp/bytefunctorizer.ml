(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2024 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

module CU = Compilation_unit

let read_unit_info_of_cmo file : Functorizer.unit_info =
  let open Cmo_format in
  let ic = open_in_bin file in
  try
    let buffer =
      really_input_string ic (String.length Config.cmo_magic_number)
    in
    if buffer <> Config.cmo_magic_number
    then Misc.fatal_errorf "%s is not a bytecode object file" file;
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let cmo = (input_value ic : compilation_unit_descr) in
    close_in ic;
    { Functorizer.ui_unit = cmo.cu_name; ui_format = cmo.cu_format }
  with x ->
    close_in ic;
    raise x

let find_unit_info_by_name_cmi name : Functorizer.unit_info =
  let filename =
    Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmi")
  in
  Functorizer.read_unit_info_of_cmi filename

let find_unit_info_by_name_cmo name : Functorizer.unit_info =
  let filename =
    Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmo")
  in
  read_unit_info_of_cmo filename

let make_compilation_unit target =
  let output_basename = Filename.basename (Filename.remove_extension target) in
  CU.of_string (String.capitalize_ascii output_basename)

(* Interface mode: input is .cmi files. Analogous to compiling a .mli.
   Outputs .cmi + .cmti + .cmsi. *)
let functorize_intf initial_env files target =
  let src_infos = List.map Functorizer.read_unit_info_of_cmi files in
  let all_modules =
    Functorizer.collect_all_modules
      ~find_unit_info_by_name:find_unit_info_by_name_cmi src_infos
  in
  let all_params = Functorizer.collect_all_params all_modules in
  let compilation_unit = make_compilation_unit target in
  let for_pack_prefix = CU.Prefix.from_clflags () in
  let target_artifact =
    Unit_info.Artifact.from_filename ~for_pack_prefix target
  in
  let unit_info =
    Unit_info.of_artifact Intf target_artifact ~dummy_source_file:target
  in
  Env.set_unit_name (Some unit_info);
  let modules_cu =
    List.map (fun (ui : Functorizer.unit_info) -> ui.ui_unit) all_modules
  in
  Misc.try_finally
    (fun () ->
      Typemod.functorize_interface initial_env ~all_params ~modules:modules_cu
        unit_info compilation_unit)
    ~exceptionally:(fun () -> Misc.remove_file target)

(* Implementation mode: input is .cmo files. Analogous to compiling a .ml.
   Reads individual CMIs via [Typemod.functorize_implementation] to populate
   [Env.imports ()] with constituent CRCs, then outputs .cmo + .cmi + .cmt + .cms.
   Respects -stop-after typing. *)
let functorize_impl initial_env files target =
  let src_infos = List.map read_unit_info_of_cmo files in
  let all_modules =
    Functorizer.collect_all_modules
      ~find_unit_info_by_name:find_unit_info_by_name_cmo src_infos
  in
  let all_params = Functorizer.collect_all_params all_modules in
  let output_prefix = Filename.remove_extension target in
  let compilation_unit = make_compilation_unit target in
  let for_pack_prefix = CU.Prefix.from_clflags () in
  let target_artifact =
    Unit_info.Artifact.from_filename ~for_pack_prefix target
  in
  let unit_info =
    Unit_info.of_artifact Impl target_artifact ~dummy_source_file:target
  in
  Env.set_unit_name (Some unit_info);
  let modules_cu =
    List.map (fun (ui : Functorizer.unit_info) -> ui.ui_unit) all_modules
  in
  let modules =
    List.map
      (fun (ui : Functorizer.unit_info) -> ui.ui_unit, ui.ui_format)
      all_modules
  in
  let source_file = List.hd files in
  Misc.try_finally
    (fun () ->
      Compile.functorize ~source_file ~output_prefix ~compilation_unit
        ~initial_env ~all_params ~modules_cu ~modules)
    ~exceptionally:(fun () ->
      Misc.remove_file target;
      Misc.remove_file (Unit_info.Artifact.filename (Unit_info.cmi unit_info)))

let functorize_files ~ppf_dump:_ initial_env files target =
  let files =
    List.map
      (fun f ->
        if Sys.file_exists f
        then f
        else Compenv.fatal (Printf.sprintf "File %s not found" f))
      files
  in
  match files with
  | [] -> Misc.fatal_error "No input files for -functorize"
  | first :: _ when Filename.check_suffix first ".cmi" ->
    functorize_intf initial_env files target
  | _ -> functorize_impl initial_env files target
