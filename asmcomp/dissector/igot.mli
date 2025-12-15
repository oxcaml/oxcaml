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

(** Intermediate GOT (Global Offset Table) for the dissector.

    The intermediate GOT provides local GOT entries that are within range of
    PC-relative addressing from the code in a partition. Each entry holds the
    absolute address of an external symbol, filled in by an R_X86_64_64
    relocation at final link time. *)

(** Size of each IGOT entry in bytes. *)
val entry_size : int

(** An entry in the intermediate GOT. *)
module Entry : sig
  type t

  (** Returns the index of this entry (0-based). *)
  val index : t -> int

  (** Returns the original external symbol this GOT entry references. *)
  val original_symbol : t -> string

  (** Returns the synthetic symbol for this GOT entry. *)
  val igot_symbol : t -> string

  (** Returns the byte offset of the entry within the IGOT section. *)
  val offset : t -> int
end

(** A built intermediate GOT section. *)
type t

(** [build ~prefix symbols] builds an intermediate GOT from a list of symbols
    that need GOT entries.

    @param prefix A unique prefix for this partition (e.g., "0", "1")
    @param symbols List of original symbol names needing GOT entries *)
val build : prefix:string -> symbols:string list -> t

(** Returns the list of entries in the IGOT. *)
val entries : t -> Entry.t list

(** Returns the section data (zero-initialized). *)
val section_data : t -> bytes

(** Returns the size of the section in bytes. *)
val section_size : t -> int

(** [find_entry t ~symbol] returns the entry for [symbol], or [None] if not
    found. *)
val find_entry : t -> symbol:string -> Entry.t option

(** [igot_symbol_name ~prefix ~symbol] returns the IGOT symbol name for the
    given original symbol. *)
val igot_symbol_name : prefix:string -> symbol:string -> string

(** A relocation for an IGOT entry. *)
module Relocation : sig
  type t

  (** Returns the offset within the IGOT section. *)
  val offset : t -> int

  (** Returns the original external symbol to relocate to. *)
  val symbol : t -> string

  (** Returns the relocation addend (always 0 for IGOT). *)
  val addend : t -> int64
end

(** [relocations t] returns the list of R_X86_64_64 relocations needed to
    fill the IGOT entries with the addresses of the original symbols. *)
val relocations : t -> Relocation.t list
