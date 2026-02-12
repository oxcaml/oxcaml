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

(** Partition types for the dissector.

    A partition groups object files together for partial linking. The first
    partition (Main) contains code that must be addressable within the small
    code model's 2GB limit. Additional partitions (Large_code) are placed at
    higher addresses and use an intermediate GOT/PLT to bridge calls. *)

(** The kind of partition, determining how sections are named and placed. *)
type kind =
  | Main
      (** The main partition. Sections keep their original names (.text, .data,
          etc.) and are placed by default linker rules at low addresses. *)
  | Large_code of int
      (** A large code partition with the given index (1, 2, ...). Sections are
          renamed with a prefix (e.g., .caml.p1.text) and placed after .bss by
          the linker script. *)

(** Returns the symbol prefix for IGOT/IPLT symbols in this partition. *)
val symbol_prefix : kind -> string

(** Returns the section prefix for this partition. Empty for Main, ".caml.pN"
    for Large_code N. *)
val section_prefix : kind -> string

(** A partition before partial linking, containing files with their sizes. *)
type t

(** Returns the kind of this partition. *)
val kind : t -> kind

(** Returns the files in this partition. *)
val files : t -> Measure_object_files.File_size.t list

(** Returns the total size of allocated sections across all files. *)
val total_size : t -> int64

(** Create a partition from a list of files with the given kind. *)
val create : kind:kind -> Measure_object_files.File_size.t list -> t

(** A partition after partial linking, with the path to the partially linked
    object file. *)
module Linked : sig
  type partition := t

  type t

  (** Returns the original partition. *)
  val partition : t -> partition

  (** Returns the path to the partially-linked object file. *)
  val linked_object : t -> string

  (** Create a linked partition. *)
  val create : partition:partition -> linked_object:string -> t
end
