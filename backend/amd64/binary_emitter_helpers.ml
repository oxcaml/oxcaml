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

(* CR sspies: More of this module could be shared with the arm64 counterpart
   (e.g. the section-name normalization and the on-disk layout used when saving
   binary sections for verification). The two backends currently differ because
   the x86 emitter assembles sections post-hoc via [X86_proc.iter_sections]
   while the arm64 emitter is wired in via callbacks during emission. *)

let should_use_binary_emitter () = false

let begin_emission () = fun _ -> ()

let should_save_binary_sections () =
  !Oxcaml_flags.verify_binary_emitter && not !Oxcaml_flags.internal_assembler

let save_binary_sections () =
  let dir = !Emitaux.output_prefix ^ ".binary-sections" in
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  X86_proc.iter_sections (fun name instructions ->
      let buf =
        X86_binary_emitter.assemble_section X64
          { X86_binary_emitter.sec_name = name;
            sec_instrs = Oxcaml_utils.Doubly_linked_list.to_array instructions
          }
      in
      let sec_name = X86_proc.Section_name.to_string name in
      let bare_name =
        if String.length sec_name > 0 && Char.equal (String.get sec_name 0) '.'
        then String.sub sec_name 1 (String.length sec_name - 1)
        else sec_name
      in
      let safe_name = "section_" ^ bare_name in
      let bin_path = Filename.concat dir (safe_name ^ ".bin") in
      let oc = open_out_bin bin_path in
      output_string oc (X86_binary_emitter.contents buf);
      close_out oc)

let end_emission () =
  if should_save_binary_sections () then save_binary_sections ()
