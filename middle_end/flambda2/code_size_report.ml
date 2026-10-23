(******************************************************************************
 *                                  OxCaml                                    *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(* The text fields are quoted, with any double quotes doubled, as in RFC 4180:
   the symbols of anonymous functions contain commas. *)
let csv_field s =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""

let dump ~prefixname all_code =
  let current_unit = Current_unit.get_cu_exn () in
  Misc.protect_output_to_file (prefixname ^ ".code_sizes.csv") (fun out ->
      output_string out "symbol,debuginfo,v1\n";
      (* [all_code] also holds the metadata of the code imported from other
         compilation units; the bodies of the pieces of code of this unit that
         cannot be inlined are absent too, but their metadata is there. *)
      Exported_code.iter_code_metadata all_code ~f:(fun metadata ->
          let code_id = Code_metadata.code_id metadata in
          if
            Flambda2_identifiers.Code_id.in_compilation_unit code_id
              current_unit
          then
            let cost_metrics = Code_metadata.cost_metrics metadata in
            (* The code of the closures defined in the function is measured on
               its own. *)
            let size =
              Code_size.( - )
                (Cost_metrics.size cost_metrics)
                (Cost_metrics.size_of_nested_code cost_metrics)
            in
            Printf.fprintf out "%s,%s,%d\n"
              (csv_field
                 (Linkage_name.to_string
                    (Flambda2_identifiers.Code_id.linkage_name code_id)))
              (csv_field
                 (Format.asprintf "%a" Debuginfo.print_compact
                    (Code_metadata.dbg metadata)))
              (Code_size.to_int size)))
