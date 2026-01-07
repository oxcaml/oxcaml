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

type empty

type filled

type _ t

val from_binary_section :
  (module Binary_emitter_intf.S
     with type Assembled_section.t = 'a
      and type Relocation.t = 'r) ->
  name:string ->
  entry_size:int ->
  is_relevant_reloc:('r -> bool) ->
  write_entry:(Buffer.t -> Address.t -> unit) ->
  'a ->
  empty t
(** Creates a table with all the entries needed for the given binary section.
    The table will have the right size, i.e. one entry per corresponding
    relocation matching [is_relevant_reloc] in the given section. You need
    to use [fill] to write the actual addresses of the pointed symbols. *)

val fill : Symbols.t -> empty t -> filled t
(** Fills the table with the absolute addresses of the symbols it holds *)

val in_memory_size : _ t -> int
(** Returns the size (in bytes) the table will take up in the memory *)

val content : filled t -> string
(** Returns the table in binary form, as it should be written at the end of
    the text section *)

val symbol_address : _ t addressed -> string -> Address.t option
(** [symbol_address table symbol] returns the absolute address at which the
    address or jump for [symbol] is stored within the given table. *)
