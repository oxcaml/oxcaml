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

[@@@ocaml.warning "+a-40-41-42"]

(* Keep these constants in sync with runtime/caml/frame_descriptors.h. *)

let symbol_name = "caml_frame_index_data"

let magic = 0x0FEF4F5849445801L

let version = 1

let header_size = 64

let region_align = 64

let slack = 64

let min_shift = 9

let max_shift = 20

let bucket_word_size = 4

let entry_size = 8

let magic_offset = 0

let version_offset = 8

let shift_offset = 12

let text_lo_offset = 16

let num_granules_offset = 24

let num_entries_offset = 32

let ft_lo_offset = 40

let ft_hi_offset = 48

let reserved_entries_offset = 56

let bucket_budget_offset = 60

type t =
  { reserved_entries : int;
    bucket_budget : int
  }

let empty = { reserved_entries = 0; bucket_budget = 0 }

let of_estimate estimate =
  let reserved_entries = estimate + slack in
  { reserved_entries; bucket_budget = max (reserved_entries / 2) 1024 }

let of_header ~reserved_entries ~bucket_budget =
  { reserved_entries; bucket_budget }

let round_up n align = (n + align - 1) / align * align

let is_empty t = t.reserved_entries = 0

let bucket_region t =
  if is_empty t
  then 0
  else round_up (bucket_word_size * (t.bucket_budget + 1)) region_align

let entries_region t =
  if is_empty t
  then 0
  else round_up (entry_size * t.reserved_entries) region_align

let bucket_offset = header_size

let entries_offset t = bucket_offset + bucket_region t

let total_bytes t = header_size + bucket_region t + entries_region t
