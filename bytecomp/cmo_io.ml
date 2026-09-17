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

type error =
  | Not_an_object_file of Misc.filepath
  | Illegal_renaming of
      { expected : Compilation_unit.t;
        found : Compilation_unit.t;
        file : Misc.filepath }

exception Error of error

let with_cmo file f =
  let ic = open_in_bin file in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let buffer = really_input_string ic (String.length Config.cmo_magic_number) in
  if buffer <> Config.cmo_magic_number
  then raise (Error (Not_an_object_file file));
  let compunit_pos = input_binary_int ic in
  seek_in ic compunit_pos;
  let compunit = (input_value ic : Cmo_format.compilation_unit_descr) in
  f ic compunit

let read_cmo file = with_cmo file (fun _ic compunit -> compunit)

let read_static_data cu =
  let cmo = Compilation_unit.base_filename cu ^ ".cmo" in
  match Load_path.find_normalized cmo with
  | exception Not_found ->
    Location.prerr_warning Location.none
      (Warnings.No_cmx_file
         { missing_extension = "cmo";
           module_name = Compilation_unit.full_path_as_string cu });
    None
  | file ->
    with_cmo file (fun ic compunit ->
      if not (Compilation_unit.equal compunit.cu_name cu) then
        raise (Error (Illegal_renaming
                        { expected = cu; found = compunit.cu_name; file }));
      if compunit.cu_static_data = 0 then None
      else begin
        seek_in ic compunit.cu_static_data;
        Some (input_value ic : Slambdaeval.CU_data.t)
      end)

open Format_doc

let report_error ppf = function
  | Not_an_object_file file ->
    fprintf ppf "%a is not a bytecode object file" Location.Doc.filename file
  | Illegal_renaming { expected; found; file } ->
    fprintf ppf "Wrong file naming: %a@ contains the compiled code for@ \
                 %a when %a was expected"
      Location.Doc.filename file
      Compilation_unit.print found
      Compilation_unit.print expected

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
