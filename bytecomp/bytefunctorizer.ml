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

let read_unit_info_of_cmo file : Functorizer.unit_info =
  let open Cmo_format in
  let ic = open_in_bin file in
  (try
    let buffer =
      really_input_string ic (String.length Config.cmo_magic_number)
    in
    if buffer <> Config.cmo_magic_number then
      Misc.fatal_errorf "%s is not a bytecode object file" file;
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let cmo = (input_value ic : compilation_unit_descr) in
    close_in ic;
    { Functorizer.ui_unit = cmo.cu_name; ui_format = cmo.cu_format }
  with x ->
    close_in ic;
    raise x)

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

let functorize_intf ~srcs target =
  Functorizer.functorize_intf ~srcs target
    ~read_unit_info:Functorizer.read_unit_info_of_cmi
    ~find_unit_info_by_name:find_unit_info_by_name_cmi

let functorize_impl ~srcs target =
  Functorizer.functorize_impl ~srcs target
    ~read_unit_info:read_unit_info_of_cmo
    ~find_unit_info_by_name:find_unit_info_by_name_cmo
    ~compile:Compile.functorize
