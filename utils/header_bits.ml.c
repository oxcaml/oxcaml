/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Nick Barnes, Jane Street                          */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Processed by the C preprocessor against runtime/caml/header_bits.h
   to produce header_bits.ml: the compiler's view of the
   interleaved-block header field layout. */

#include "header_bits.h"

let tag_bits = HEADER_TAG_BITS
let color_bits = HEADER_COLOR_BITS
let wosize_bits = HEADER_WOSIZE_BITS
let format_bits = HEADER_FORMAT_BITS
let repeat_bits = HEADER_REPEAT_BITS
let scan_mask_bits = HEADER_SCAN_MASK_BITS
let reserved_bits =
  HEADER_FORMAT_BITS + HEADER_REPEAT_BITS + HEADER_SCAN_MASK_BITS

let color_shift = HEADER_COLOR_SHIFT
let wosize_shift = HEADER_WOSIZE_SHIFT
let scan_mask_shift = HEADER_SCAN_MASK_SHIFT
let repeat_shift = HEADER_REPEAT_SHIFT
let format_shift = HEADER_FORMAT_SHIFT

let format_legacy = HEADER_FORMAT_LEGACY
let format_inline = HEADER_FORMAT_INLINE
let format_extended = HEADER_FORMAT_EXTENDED
let format_external = HEADER_FORMAT_EXTERNAL

let layout_word_mask_bits = LAYOUT_WORD_MASK_BITS
let layout_word_repeat_bits = LAYOUT_WORD_REPEAT_BITS
let layout_word_repeat_shift = LAYOUT_WORD_REPEAT_SHIFT
let layout_word_repeat_max = LAYOUT_WORD_REPEAT_MAX
