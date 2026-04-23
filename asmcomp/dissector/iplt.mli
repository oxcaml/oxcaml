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

(* CR mshinwell: This file needs to be code reviewed *)

(** Intermediate PLT (Procedure Linkage Table) for the dissector.

    The intermediate PLT provides local PLT entries that are within range of
    PC-relative call/jump instructions from the code in a partition. Each entry
    contains a jump instruction through the corresponding IGOT entry.

    On x86-64, each PLT entry is 8 bytes:
    - ff 25 XX XX XX XX : jmp [rip + displacement] (6 bytes)
    - 90 90 : 2-byte nop padding
    - One R_X86_64_PC32 relocation at offset +2

    On AArch64, each PLT entry is 16 bytes:
    - adrp x16, \#0 : load page of IGOT entry (4 bytes)
    - ldr x17, [x16, \#0] : load address from IGOT (4 bytes)
    - br x17 : branch to target (4 bytes)
    - nop : padding (4 bytes)
    - Two relocations: R_AARCH64_ADR_PREL_PG_HI21 at +0 and
      R_AARCH64_LDST64_ABS_LO12_NC at +4 *)

(** Size of each IPLT entry in bytes. *)
val entry_size : int

(** An entry in the intermediate PLT. *)
module Entry : sig
  type t

  (** Returns the index of this entry (0-based). *)
  val index : t -> int

  (** Returns the original external symbol this PLT entry calls. *)
  val original_symbol : t -> string

  (** Returns the synthetic symbol for this PLT entry. *)
  val iplt_symbol : t -> string

  (** Returns the IGOT symbol this entry jumps through. *)
  val igot_symbol : t -> string

  (** Returns the byte offset of the entry within the IPLT section. *)
  val offset : t -> int
end

(** A built intermediate PLT section. *)
type t

(** [build ~prefix ~igot symbols] builds an intermediate PLT from a list of
    symbols that need PLT entries.

    Each PLT entry requires a corresponding IGOT entry, which must exist in the
    provided [igot].

    @param prefix A unique prefix for this partition (e.g., "0", "1")
    @param igot The intermediate GOT (must contain entries for all symbols)
    @param symbols List of original symbol names needing PLT entries *)
val build : prefix:string -> igot:Igot.t -> symbols:string list -> t

(** Returns the list of entries in the IPLT. *)
val entries : t -> Entry.t list

(** Returns the section data (machine code). *)
val section_data : t -> bytes

(** Returns the size of the section in bytes. *)
val section_size : t -> int

(** [find_entry t ~symbol] returns the entry for [symbol], or [None] if not
    found. *)
val find_entry : t -> symbol:string -> Entry.t option

(** [iplt_symbol_name ~prefix ~symbol] returns the IPLT symbol name for the
    given original symbol. *)
val iplt_symbol_name : prefix:string -> symbol:string -> string

(** A relocation for an IPLT entry. *)
module Relocation : sig
  type t

  (** Returns the offset within the IPLT section. *)
  val offset : t -> int

  (** Returns the IGOT symbol to relocate to. *)
  val symbol : t -> string

  (** Returns the relocation type. *)
  val reloc_type : t -> Compiler_owee.Owee_elf_relocation.Reloc_type.t

  (** Returns the relocation addend. *)
  val addend : t -> int64
end

(** [relocations t] returns the list of relocations needed to fill the IPLT
    entries with references to their IGOT entries. On x86-64 this is one
    R_X86_64_PC32 per entry; on AArch64 it is two per entry
    (R_AARCH64_ADR_PREL_PG_HI21 and R_AARCH64_LDST64_ABS_LO12_NC). *)
val relocations : t -> Relocation.t list
