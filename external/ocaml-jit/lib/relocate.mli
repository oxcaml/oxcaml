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

(** Type for looking up symbol addresses in GOT/PLT tables *)
type table_lookup = string -> Address.t option

(** Apply all relocations to the given binary section.
    Uses the unified Binary_emitter interface. *)
val all :
  (module Binary_emitter_intf.S
     with type Assembled_section.t = 'a
      and type Relocation.t = 'r) ->
  symbols:Symbols.t ->
  got_lookup:table_lookup option ->
  plt_lookup:table_lookup option ->
  section_name:string ->
  'a addressed ->
  (unit, string list) result
