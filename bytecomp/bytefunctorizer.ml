(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
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

let find_impl_by_name_cmo cu : Lambda.main_module_block_format =
  let filename =
    Load_path.find_normalized (Compilation_unit.base_filename cu ^ ".cmo")
  in
  (Cmo_io.read_cmo filename).cu_format

(* [with_info] must wrap the whole call: [Compile_common.with_info]'s
   [Compmisc.init_path] resets the persistent_env, so any imports registered
   beforehand would be wiped — leaving the emitted [.cmo] with no interface
   CRCs, masking link-time mismatches. *)
let functorize input_module_names targetcmo =
  if Filename.check_suffix targetcmo ".cmi"
  then Functorizer.interface input_module_names targetcmo
  else
    let output_prefix = Filename.remove_extension targetcmo in
    let unit_info =
      Compile_common.unit_info_from_cu_or_output_prefix ~source_file:targetcmo
        Unit_info.Impl ~output_prefix
        ~compilation_unit:Inferred_from_output_prefix
    in
    Compile_common.with_info ~backend:Byte ~tool_name:"ocamlc" ~dump_ext:"cmo"
      unit_info
    @@ fun info ->
    Misc.try_finally
      (fun () ->
        Functorizer.implementation input_module_names
          ~find_impl_by_name:find_impl_by_name_cmo
          ~compile_program:Compile.emit_lambda_program info)
      ~exceptionally:(fun () ->
        Misc.remove_file targetcmo;
        Misc.remove_file
          (Unit_info.Artifact.filename (Unit_info.cmi info.target)))
