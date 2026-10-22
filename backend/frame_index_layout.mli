(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(** Layout of the post-link frame-descriptor index stored in the
    [caml_frame_index] section of an executable.

    The section is reserved (zero-filled) by the startup object at link time
    and filled in place by [Frame_index] once the executable has been linked.
    The runtime ([runtime/frame_descriptors.c]) reads it; the constants here
    must agree with those in [runtime/caml/frame_descriptors.h].

    The section holds a 64-byte header followed by two 64-byte-aligned
    regions: the bucket table ([u32 bucket[num_granules + 1]]) and the
    entries, each the return-address offset of a descriptor within its
    granule next to the descriptor's offset within the frametables section
    ([{u32 pc_off; u32 descr_off} entry[num_entries]]), so that a lookup
    finds both in the same cache line. The regions are sized from the
    reservation recorded in the header (fields [reserved_entries] and
    [bucket_budget]), not from the final entry counts, so their offsets can be
    computed before the index is built. *)

[@@@ocaml.warning "+a-40-41-42"]

(** The symbol at the start of the section. It cannot share the section's
    name: the assembler defines a section symbol of that name. *)
val symbol_name : string

val magic : int64

val version : int

val header_size : int

(** Alignment of the section start and of each region. *)
val region_align : int

(** Entries reserved beyond the pre-link estimate. *)
val slack : int

(** Bounds on [log2] of the granule size in bytes. *)
val min_shift : int

val max_shift : int

(** Byte offsets of the header fields. *)

val magic_offset : int

val version_offset : int

val shift_offset : int

val text_lo_offset : int

val num_granules_offset : int

val num_entries_offset : int

val ft_lo_offset : int

val ft_hi_offset : int

val reserved_entries_offset : int

val bucket_budget_offset : int

(** A reservation: how many entries and buckets the section has room for. *)
type t = private
  { reserved_entries : int;
    bucket_budget : int
  }

(** The header-only reservation emitted when the index is disabled. *)
val empty : t

val is_empty : t -> bool

(** The reservation for an estimated number of static frame descriptors. *)
val of_estimate : int -> t

(** Reconstruct a reservation from the header fields of a reserved section. *)
val of_header : reserved_entries:int -> bucket_budget:int -> t

val round_up : int -> int -> int

(** Size in bytes of an entry of the entries region. *)
val entry_size : int

(** Byte offsets of the two regions from the start of the section, and the
    total size of the section. *)

val bucket_offset : int

val entries_offset : t -> int

val total_bytes : t -> int
