(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

type need_reloc

type relocated

type _ t

val name : string
(** Name of the text section: [".text"] *)

val from_binary_section : X86_binary_emitter.buffer -> need_reloc t
(** Creates a text section with empty GOT and PLT. These will be filled
    along with relocations applied by [relocate].
    The returned section can be used to determine the size of the final text section
    to properly allocate memory pages *)

val in_memory_size : _ t -> int
(** Returns the size (in bytes) the section + GOT and PLT will take up in the memory *)

val relocate :
  symbols:Symbols.t ->
  need_reloc t addressed ->
  (relocated t addressed, string list) result
(** Apply relocations to the following section and fills the GOT and PLT.
    The returned section can be written to memory and run. *)

val content : relocated t -> string
(** Return the text section along with the GOT and PLT tables, in binary form as a string *)

val symbols : _ t addressed -> Symbols.t
(** Return a mapping from symbols to absolute address for the symbols defined in the given
    text section. *)
