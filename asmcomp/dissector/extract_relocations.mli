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

(** Extract relocations from partially-linked object files.

    This module reads ELF object files and extracts the symbols whose
    relocations need to be converted to use an intermediate PLT or GOT when
    linking with the dissector code model. *)

(** A partition's partially-linked object file. *)
module Mapped_object_file : sig
  type t

  (** Map the ELF object file at [filename] and parse its header, section table,
      symbol table and .rela.text* sections. Fatal error if the file has no
      symbol table. *)
  val read : (module Compiler_owee.Unix_intf.S) -> filename:string -> t

  (** The filename passed to [read]. *)
  val filename : t -> string

  (** The whole mapped file. *)
  val buf : t -> Compiler_owee.Owee_buf.t

  (** The ELF header. *)
  val header : t -> Compiler_owee.Owee_elf.header

  (** The section table. *)
  val sections : t -> Compiler_owee.Owee_elf.section array

  (** The symbol table, indexed by symbol index. *)
  val symbols : t -> Compiler_owee.Owee_elf.symbol array

  (** All .rela.text* sections with their bodies, in section table order.
      Handles both a traditional single .rela.text section and function
      sections: .rela.text.foo, .rela.text.bar, etc. *)
  val rela_text_sections :
    t -> (Compiler_owee.Owee_elf.section * Compiler_owee.Owee_buf.t) list
end

(** The result of extracting relocations from object files. *)
type t

(** Returns the symbol name of each relocation with type R_X86_64_PLT32 that
    needs a PLT entry. The names are unique, in order of first appearance; this
    order determines IPLT entry order in [Build_igot_and_iplt]. *)
val plt_symbols : t -> Relocatable_symbol_name.t list

(** Returns the symbol name of each relocation with type R_X86_64_REX_GOTPCRELX
    that needs a GOT entry. The names are unique, in order of first appearance;
    they may overlap with [plt_symbols], and together the two lists determine
    IGOT entry order in [Build_igot_and_iplt]. *)
val got_symbols : t -> Relocatable_symbol_name.t list

(** Returns the number of PLT relocation sites (not deduplicated; O(1)). *)
val num_plt : t -> int

(** Returns the number of GOT relocation sites (not deduplicated; O(1)). *)
val num_got : t -> int

(** [extract input] scans the .rela.text* sections of [input] for relocations
    that need to be converted for the medium code model.

    Returns the symbol names of the PLT32 and REX_GOTPCRELX relocations found.
*)
val extract : Mapped_object_file.t -> t
