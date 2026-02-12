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

    This module reads ELF object files and extracts relocations that need to be
    converted to use an intermediate PLT or GOT when linking with the dissector
    code model. *)

(** Information about a single relocation that needs conversion. *)
module Relocation_entry : sig
  type t

  (** Returns the symbol name for the relocation. *)
  val symbol_name : t -> string

  (** Returns the offset of the relocation within the section. *)
  val offset : t -> int64
end

(** The result of extracting relocations from object files. *)
type t

(** Returns relocations with type R_X86_64_PLT32 that need PLT entries. *)
val convert_to_plt : t -> Relocation_entry.t list

(** Returns relocations with type R_X86_64_REX_GOTPCRELX that need GOT entries.
*)
val convert_to_got : t -> Relocation_entry.t list

(** [extract unix ~filename] reads the ELF object file at [filename] and
    extracts relocations from the .rela.text section that need to be converted
    for the medium code model.

    Returns the lists of PLT32 and REX_GOTPCRELX relocations found. *)
val extract : (module Compiler_owee.Unix_intf.S) -> filename:string -> t

(** [extract_from_linked_partitions unix linked_partitions] extracts relocations
    from all the partially-linked object files.

    Returns combined relocation information from all partitions. *)
val extract_from_linked_partitions :
  (module Compiler_owee.Unix_intf.S) -> Partition.Linked.t list -> t
