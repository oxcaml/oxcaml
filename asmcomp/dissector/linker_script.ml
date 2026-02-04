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

(* CR mshinwell: This file needs to be code reviewed *)

let sections =
  [".text"; ".rodata"; ".data"; ".bss"; ".eh_frame"; ".data.igot"; ".text.iplt"]

let generate ~existing_script ~partitions =
  let buf = Buffer.create 1024 in
  (match existing_script with
  | None -> ()
  | Some path ->
    Buffer.add_string buf
      (Printf.sprintf "/* BEGIN include existing linker script: %s */\n" path);
    let ic = open_in path in
    (try
       while true do
         Buffer.add_string buf (input_line ic);
         Buffer.add_char buf '\n'
       done
     with End_of_file -> close_in ic);
    Buffer.add_string buf
      (Printf.sprintf "/* END include existing linker script: %s */\n\n" path));
  Buffer.add_string buf "SECTIONS {\n";
  List.iter
    (fun linked ->
      let kind = Partition.kind (Partition.Linked.partition linked) in
      match kind with
      | Main -> ()
      | Large_code _ ->
        let prefix = Partition.section_prefix kind in
        List.iter
          (fun section ->
            let name = prefix ^ section in
            Buffer.add_string buf
              (Printf.sprintf "%s : { *(%s) *(%s.*) }\n" name name name))
          sections)
    partitions;
  Buffer.add_string buf "} INSERT AFTER .bss\n";
  Buffer.contents buf

let write ~output_file ~existing_script ~partitions =
  let contents = generate ~existing_script ~partitions in
  let oc = open_out output_file in
  output_string oc contents;
  close_out oc
