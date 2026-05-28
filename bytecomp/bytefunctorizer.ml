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

let read_unit_info_of_cmo file : Functorizer.impl_unit_info =
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
    Functorizer.impl_unit_info_with_cmi_data ~ui_unit:cmo.cu_name
      ~ui_format:cmo.cu_format
  with x ->
    close_in ic;
    raise x

let find_unit_info_by_name_cmo name : Functorizer.impl_unit_info =
  let filename =
    Load_path.find_normalized (String.uncapitalize_ascii name ^ ".cmo")
  in
  read_unit_info_of_cmo filename

(* Implementation mode: input is .cmo files. Analogous to compiling a .ml.
   Outputs .cmo + .cmi + .cmt + .cms.  Respects -stop-after typing.  Input
   files are read inside [Compile.with_info]'s callback so each input's cmi is
   registered as an import of the bundle. *)
let functorize_impl initial_env files target =
  let output_prefix = Filename.remove_extension target in
  let compilation_unit = Functorizer.make_compilation_unit target in
  let source_file = List.hd files in
  Compile.with_info ~source_file ~output_prefix
    ~compilation_unit:(Compile_common.Exactly compilation_unit) ~kind:Impl
    ~dump_ext:"cmo"
  @@ fun info ->
  Misc.try_finally
    (fun () ->
      let compile_program info program =
        let bytecode =
          Compile.tlambda_to_bytecode info program ~as_arg_for:None
        in
        if not (Clflags.should_stop_after Clflags.Compiler_pass.Lambda)
        then Compile.emit_bytecode info bytecode
      in
      Functorizer.functorize_impl_with ~initial_env ~info ~input_files:files
        ~read_unit_info_of_input:read_unit_info_of_cmo
        ~find_intf_unit_info_by_name:Functorizer.find_unit_info_by_name_cmi
        ~find_impl_unit_info_by_name:find_unit_info_by_name_cmo ~compile_program)
    ~exceptionally:(fun () ->
      Misc.remove_file target;
      Misc.remove_file (Unit_info.Artifact.filename (Unit_info.cmi info.target)))

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
    Functorizer.functorize_intf initial_env files target
  | _ -> functorize_impl initial_env files target
