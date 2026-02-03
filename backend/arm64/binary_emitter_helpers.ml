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

(** CR mshinwell: Adapt the binary sections saving to x86 in due course. *)

module BE = Arm64_binary_emitter.Binary_emitter

(* Binary emitter for JIT mode *)
let jit_emitter : BE.t option ref = ref None

let should_use_binary_emitter () =
  Option.is_some (BE.For_jit.Internal_assembler.get ())
  || !Oxcaml_flags.verify_binary_emitter

let should_save_binary_sections () = !Oxcaml_flags.verify_binary_emitter

let begin_emission () =
  if should_use_binary_emitter ()
  then (
    (* When saving binary sections (for verification), emit relocations for all
       symbol references to match assembler behavior *)
    if should_save_binary_sections ()
    then BE.emit_relocs_for_all_symbol_refs := true;
    let emitter = BE.create () in
    jit_emitter := Some emitter;
    Arm64_ast.Ast.DSL.Acc.set_emit_instruction
      ~emit_instruction:(BE.add_instruction emitter);
    fun d -> BE.add_directive emitter d)
  else fun _ -> ()

(* Aggregate all .text.* sections into a single .text section for JIT *)
let aggregate_text_sections sections =
  let is_text_section sec_name =
    let len = String.length sec_name in
    len >= 5 && String.equal (String.sub sec_name 0 5) ".text"
  in
  let text_sections, other_sections =
    List.partition (fun (sec_name, _) -> is_text_section sec_name) sections
  in
  match text_sections with
  | [] -> sections
  | [(sec_name, _)] when String.equal sec_name ".text" -> sections
  | _ ->
    (* Aggregate all text sections into one .text section. Sort by name for
       deterministic ordering. *)
    let sorted_text =
      List.sort (fun (a, _) (b, _) -> String.compare a b) text_sections
    in
    let module SS = BE.Section_state in
    let aggregated = SS.create () in
    List.iter
      (fun (_name, state) ->
        let content = SS.contents state in
        let base_offset = Buffer.length (SS.buffer aggregated) in
        Buffer.add_string (SS.buffer aggregated) content;
        (* Copy relocations with adjusted offsets *)
        List.iter
          (fun (reloc : Arm64_binary_emitter.Relocation.t) ->
            let adjusted =
              { reloc with
                offset_from_section_beginning =
                  reloc.offset_from_section_beginning + base_offset
              }
            in
            SS.add_relocation aggregated adjusted)
          (SS.relocations state))
      sorted_text;
    (".text", aggregated) :: other_sections

(* Convert section table to list of (name, section) pairs *)
let sections_to_list section_tbl =
  let from_standard =
    Arm64_binary_emitter.All_section_states.fold section_tbl ~init:[]
      ~f:(fun section state acc ->
        let name = Asm_targets.Asm_section.to_string section in
        (name, state) :: acc)
  in
  let from_individual =
    Arm64_binary_emitter.All_section_states.fold_individual section_tbl ~init:[]
      ~f:(fun name state acc -> (name, state) :: acc)
  in
  from_standard @ from_individual

(* Save sections to files for verification *)
let save_sections_to_files sections section_tbl =
  let dir = !Emitaux.output_prefix ^ ".binary-sections" in
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  List.iter
    (fun (name, state) ->
      (* Convert section name like ".text" to "section_text" *)
      let safe_name =
        if String.length name > 0 && Char.equal (String.get name 0) '.'
        then "section_" ^ String.sub name 1 (String.length name - 1)
        else "section_" ^ name
      in
      (* Save binary content *)
      let bin_filename = Filename.concat dir (safe_name ^ ".bin") in
      let oc = open_out_bin bin_filename in
      output_string oc (BE.Section_state.contents state);
      close_out oc;
      (* Save relocations if any *)
      let relocs = BE.Section_state.relocations state in
      match relocs with
      | [] -> ()
      | _ ->
        let reloc_filename = Filename.concat dir (safe_name ^ ".relocs") in
        let oc = open_out reloc_filename in
        let module R = Arm64_binary_emitter.Relocation in
        let module ED = BE.Encode_directive in
        List.iter
          (fun reloc ->
            (* For paired relocations (SUBTRACTOR + UNSIGNED), write both
               symbols as separate lines at the same offset. On RELA platforms
               (Linux), include addends for proper verification. Also convert
               local/file- scope symbols to section+offset. *)
            let offset = R.offset_from_section_beginning reloc in
            List.iter
              (fun (target, addend) ->
                (* For verification output, convert local/file-scope symbols to
                   section+offset format to match assembler behavior *)
                let resolved_sym, resolved_addend =
                  ED.resolve_local_label_for_elf ~all_sections:section_tbl
                    ~target ~sym_offset:addend
                in
                if resolved_addend = 0
                then Printf.fprintf oc "%d %s\n" offset resolved_sym
                else
                  Printf.fprintf oc "%d %s %d\n" offset resolved_sym
                    resolved_addend)
              (R.all_targets_with_addends reloc))
          relocs;
        close_out oc)
    sections

let end_emission () =
  match !jit_emitter with
  | None -> ()
  | Some emitter -> (
    (* Clear the instruction emission callback *)
    Arm64_ast.Ast.DSL.Acc.clear_emit_instruction ();
    jit_emitter := None;
    (* Dump instructions if JIT debug is enabled *)
    (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
    | Some ("true" | "1") -> BE.dump_instructions emitter
    | _ -> ());
    (* Get assembled sections. for_jit is true only when there's a JIT hook
       registered (actual JIT compilation). When we're just saving binary
       sections for verification, we're generating an object file and should use
       for_jit:false to match ELF relocation format. *)
    let for_jit = Option.is_some (BE.For_jit.Internal_assembler.get ()) in
    let section_tbl = BE.emit ~for_jit emitter in
    (* Convert to list of (name, section) pairs. Include both standard sections
       (by Asm_section.t) and individual sections (by name string, for function
       sections like .text.caml.<funcname>). *)
    let sections = sections_to_list section_tbl in
    (* For JIT, we need to aggregate all .text.* sections into a single .text
       section because the JIT loader expects exactly one .text section. *)
    let sections_for_jit = aggregate_text_sections sections in
    (* Save sections to files if needed for verification or explicit saving *)
    if should_save_binary_sections ()
    then save_sections_to_files sections section_tbl;
    (* Call the JIT hook if registered *)
    match BE.For_jit.Internal_assembler.get () with
    | None -> ()
    | Some hook ->
      (* The hook expects (string * assembled_section) list and returns a file
         writer function. We ignore the file writer for JIT. Use the aggregated
         sections where all .text.* are merged into .text. *)
      let _file_writer = hook sections_for_jit in
      ())
