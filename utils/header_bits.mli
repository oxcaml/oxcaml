(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nick Barnes, Jane Street                          *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The interleaved-block header field layout. The implementation is
    generated from runtime/caml/header_bits.h (the single source).
    Widths and shifts are in bits within the 64-bit header word; the
    scan mask covers words 0-15 (bit i = word i, 1 = unscannable) and
    the repeat field gives the number of trailing mask bits tiled past
    the mask. *)

val tag_bits : int

val color_bits : int

val wosize_bits : int

val format_bits : int

val repeat_bits : int

val scan_mask_bits : int

(** Total reserved header bits (format + repeat + scan mask); equals
    Config.reserved_header_bits. *)
val reserved_bits : int

val color_shift : int

val wosize_shift : int

val scan_mask_shift : int

val repeat_shift : int

val format_shift : int

(** Header formats (the top two bits of the header). *)
val format_legacy : int

val format_inline : int

val format_extended : int

val format_external : int

(** The shared layout-word format: 58 mask bits below a 6-bit repeat
    count; repeat values above [layout_word_repeat_max] are
    reserved. *)
val layout_word_mask_bits : int

val layout_word_repeat_bits : int

val layout_word_repeat_shift : int

val layout_word_repeat_max : int
