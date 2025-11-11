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
module Section_state = Section_state

type t

val create : unit -> t

val add_instruction : t -> Instruction.t -> unit

val add_directive : t -> Asm_targets.Asm_directives.Directive.t -> unit

val emit : ?for_jit:bool -> t -> All_section_states.t

val dump_instructions : t -> unit

(** Module implementing Binary_emitter_intf.S for use by ocaml-jit *)
module For_jit :
  Binary_emitter_intf.S
    with type Assembled_section.t = Section_state.t
     and type Relocation.t = Relocation.t

module Encode_directive : sig
  (** On Linux ELF, local labels (starting with .L) don't have symbol table
      entries. The assembler converts them to section symbol + offset. This
      function performs that conversion. Returns (symbol_name, addend) where
      symbol_name is either the original label or the section name, and addend
      includes the offset within the section plus any original offset.
      On macOS, returns the original label name and offset unchanged. *)
  val resolve_local_label_for_elf :
    all_sections:All_section_states.t ->
    target:Symbol.target ->
    sym_offset:int ->
    string * int
end

(** When true, emit relocations for ALL 8-byte symbol references (matching
    assembler behavior). When false, only emit relocations for cross-section
    references and resolve same-section refs at emit time. Set to true for
    verification against the assembler. *)
val emit_relocs_for_all_symbol_refs : bool ref
