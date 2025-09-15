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

open! Import

module Target : sig
  (** Type describing relocation targets *)
  type t = Direct of string | Got of string | Plt of string

  val from_label : string -> t option
  (** Returns the type of relative relocation based on the relocation target label:
      - ["<label>@GOTPCREL"] is interpreted as [Got "<label>"]
      - ["<label>@PLT"] is interpreted as [Plt "<label>"]
      - A reloc target with no ['@'] is interpreted as [Direct "<target>"]
      - Anything else is interpreted as [None]. *)
end

module Size : sig
  type t = S64 | S32

  val to_data_size : t -> X86_binary_emitter.data_size

  val to_int : t -> int
  (** Returns the size in bytes corresponding to the given relocation size *)
end

module Kind : sig
  type t = Absolute | Relative
end

type t = {
  offset_from_section_beginning : int;
  size : Size.t;
  kind : Kind.t;
  target : Target.t;
  addend : int64;
}

val from_x86_relocation : X86_binary_emitter.Relocation.t -> t option
(** Returns the type of relative relocation is the given relocation is relative, returns
    [None] otherwise or if the target label cannot be parsed properly by [from_label]. *)

val from_x86_relocation_err :
  X86_binary_emitter.Relocation.t -> (t, string) result
(** Same as [from_x86_relocation] but returns an error instead of [None]. *)
