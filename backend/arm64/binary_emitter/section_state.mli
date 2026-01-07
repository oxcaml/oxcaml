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

module Asm_label = Asm_targets.Asm_label
module Asm_section = Asm_targets.Asm_section
module Asm_symbol = Asm_targets.Asm_symbol
module Symbol = Arm64_ast.Ast.Symbol

type patch_size =
  | P8
  | P16
  | P32
  | P64

type t

val create : unit -> t

val buffer : t -> Buffer.t

val offset_in_bytes : t -> int

val set_offset_in_bytes : t -> int -> unit

val add_relocation_at_current_offset : t -> reloc_kind:Relocation.Kind.t -> unit

val add_relocation : t -> Relocation.t -> unit

(** Define a symbol at the current offset. *)
val define_symbol : t -> Asm_symbol.t -> unit

(** Define a label at the current offset. *)
val define_label : t -> Asm_label.t -> unit

(** Mark a symbol as global (called for Global and Weak directives).
    Global symbols get symbol table entries in ELF. *)
val mark_global : t -> Asm_symbol.t -> unit

(** Check if a symbol is explicitly global (has Global or Weak directive).
    File-scope symbols defined only via New_label return false. *)
val is_global : t -> Asm_symbol.t -> bool

(** Find the offset of a symbol. *)
val find_symbol_offset_in_bytes : t -> Asm_symbol.t -> int option

(** Find the offset of a label. *)
val find_label_offset_in_bytes : t -> Asm_label.t -> int option

(** Find the nearest global symbol strictly before a given offset.
    Returns the symbol and its offset. *)
val find_nearest_symbol_before : t -> int -> (Asm_symbol.t * int) option

(** Look up a target (symbol or label). *)
val find_target_offset_in_bytes : t -> Symbol.target -> int option

val relocations : t -> Relocation.t list

(** Iterate over all defined symbols with their offsets. *)
val iter_symbols : t -> f:(Asm_symbol.t -> int -> unit) -> unit

(** Iterate over all defined labels with their offsets. *)
val iter_labels : t -> f:(Asm_label.t -> int -> unit) -> unit

val add_patch : t -> offset:int -> size:patch_size -> data:int64 -> unit

val contents_mut : t -> bytes

val contents : t -> string
