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

(** Build intermediate GOT and PLT sections from extracted relocations.

    This module constructs synthetic IGOT and IPLT sections that allow
    code within a partition to reach external symbols via local, in-range
    entries rather than relying on the linker's default GOT/PLT placement.

    The transformation converts:

    - R_X86_64_PLT32 relocations to external symbols -> R_X86_64_PC32 to IPLT

    - R_X86_64_REX_GOTPCRELX relocations to external symbols -> R_X86_64_PC32
      to IGOT

    Every PLT entry requires a corresponding GOT entry (since the PLT stub
    jumps through the GOT), so the IGOT contains entries for both PLT and
    GOT symbols. *)

(** The result of building IGOT and IPLT sections. *)
type t

(** Returns the built IGOT. *)
val igot : t -> Igot.t

(** Returns the built IPLT. *)
val iplt : t -> Iplt.t

(** Returns the original symbols that had PLT32 relocations (need IPLT
    entries). *)
val plt_symbols : t -> string list

(** Returns the original symbols that had GOTPCRELX relocations (need IGOT
    entries, but not IPLT entries). These are in addition to the PLT symbols
    which also get IGOT entries. *)
val got_symbols : t -> string list

(** [build ~prefix relocations] builds IGOT and IPLT sections from the
    extracted relocations.

    @param prefix A unique prefix for this partition (e.g., "0", "1")
    @param relocations The relocations extracted from the partition's object
      files *)
val build : prefix:string -> Extract_relocations.t -> t

(** [igot_symbol_for_plt_reloc t reloc] returns the IGOT symbol name that
    should replace the original PLT32 relocation target, or [None] if the
    relocation doesn't need conversion.

    Note: PLT32 relocations are redirected to IPLT entries, not directly to
    IGOT. Use [iplt_symbol_for_plt_reloc] instead. *)
val igot_symbol_for_got_reloc :
  t -> Extract_relocations.Relocation_entry.t -> string option

(** [iplt_symbol_for_plt_reloc t reloc] returns the IPLT symbol name that
    should replace the original PLT32 relocation target, or [None] if the
    relocation doesn't need conversion. *)
val iplt_symbol_for_plt_reloc :
  t -> Extract_relocations.Relocation_entry.t -> string option
