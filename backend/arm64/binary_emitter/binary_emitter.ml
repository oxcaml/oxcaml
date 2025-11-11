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

open Arm64_ast.Ast
module Asm_section = Asm_targets.Asm_section
module D = Asm_targets.Asm_directives

(* Re-export modules for the library interface *)
module All_section_states = All_section_states
module Section_state = Section_state

type instruction_or_directive =
  | Instruction of Instruction.t
  | Directive of D.Directive.t

type t = { mutable enqueued_rev : instruction_or_directive list }

let create () = { enqueued_rev = [] }

let enqueue t insn_or_directive =
  t.enqueued_rev <- insn_or_directive :: t.enqueued_rev

let enqueued t = List.rev t.enqueued_rev

let dump_instructions t =
  Format.printf "=== ARM64 Binary Emitter Instructions ===@.";
  let offset = ref 0 in
  List.iter
    (fun insn_or_directive ->
      match insn_or_directive with
      | Instruction i ->
        Format.printf "%04x: %a@." !offset Instruction.print i;
        offset := !offset + 4
      | Directive d ->
        let buf = Buffer.create 256 in
        D.Directive.print buf d;
        Format.printf "      [directive: %s]@." (Buffer.contents buf))
    (enqueued t);
  Format.printf "=== End Instructions ===@."

let add_instruction t i = enqueue t (Instruction i)

let add_directive t d = enqueue t (Directive d)

let iter emitter ~all_sections ~on_insn ~on_directive =
  let current_state =
    ref (All_section_states.get_or_create all_sections Asm_section.Text)
  in
  List.iter
    (fun insn_or_directive ->
      match insn_or_directive with
      | Instruction i ->
        on_insn !current_state i;
        let offset = Section_state.offset_in_bytes !current_state in
        Section_state.set_offset_in_bytes !current_state (offset + 4)
      | Directive d ->
        (match[@warning "-4"] d with
        | Section (section, _) ->
          current_state := All_section_states.get_or_create all_sections section
        | _ -> ());
        on_directive !current_state d;
        let offset_in_bytes = Section_state.offset_in_bytes !current_state in
        let new_offset =
          D.Directive.increment_offset_in_bytes d ~offset_in_bytes
        in
        Section_state.set_offset_in_bytes !current_state new_offset)
    (enqueued emitter)

(* First pass: compute offsets of local symbol and label definitions *)
let compute_label_offsets emitter ~all_sections =
  iter emitter ~all_sections
    ~on_insn:(fun _state _insn -> ())
    ~on_directive:(fun state directive ->
      match directive with
      | New_label (Label lbl, _) -> Section_state.define_label state lbl
      | New_label (Symbol sym, _) -> Section_state.define_symbol state sym
      | Global sym ->
        Section_state.define_symbol state sym;
        Section_state.mark_global state sym
      (* Weak symbols also have symbol table entries on ELF, so track them
         too *)
      | Weak sym ->
        Section_state.define_symbol state sym;
        Section_state.mark_global state sym
      | Direct_assignment (name, expr) ->
        All_section_states.add_direct_assignment all_sections name expr
      (* Directives that don't define labels or symbols *)
      | Align _ | Bytes _ | Cfi_adjust_cfa_offset _ | Cfi_def_cfa_offset _
      | Cfi_endproc | Cfi_offset _ | Cfi_startproc | Cfi_remember_state
      | Cfi_restore_state | Cfi_def_cfa_register _ | Comment _ | Const _
      | File _ | Indirect_symbol _ | Loc _ | New_line | Private_extern _
      | Section _ | Size _ | Sleb128 _ | Space _ | Type _ | Uleb128 _
      | Protected _ | Hidden _ | External _ | Reloc _ ->
        ())

(* Second pass: emit machine code and data *)
let emit_code_and_data emitter ~all_sections =
  let current_section = ref Asm_section.Text in
  iter emitter ~all_sections
    ~on_insn:(fun state (Instruction.I { name; operands }) ->
      let encoded =
        Encode_instruction.encode_instruction ~all_sections state name operands
      in
      let buf = Section_state.buffer state in
      (* Emit as little-endian 32-bit *)
      let emit shift =
        Int32.to_int (Int32.shift_right_logical encoded shift) land 0xff
        |> Char.chr |> Buffer.add_char buf
      in
      emit 0;
      emit 8;
      emit 16;
      emit 24)
    ~on_directive:
      (Encode_directive.emit_directive ~current_section ~all_sections)

let emit ?(for_jit = false) emitter =
  let all_sections = All_section_states.create ~for_jit in
  compute_label_offsets emitter ~all_sections;
  All_section_states.reset_offsets all_sections;
  emit_code_and_data emitter ~all_sections;
  all_sections

module For_jit = For_jit
module Encode_directive = Encode_directive

(* Re-export ref to control relocation emission behavior for verification *)
let emit_relocs_for_all_symbol_refs =
  Encode_directive.emit_relocs_for_all_symbol_refs
