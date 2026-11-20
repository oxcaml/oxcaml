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

#ifndef CAML_HEADER_BITS_H
#define CAML_HEADER_BITS_H

/* Field widths and positions for the interleaved-block header layout
   on 64-bit platforms:

      63 62  61...58  57......42  41......10  9   8 7...0
     +-----+--------+-----------+------------+-----+-----+
     | fmt | repeat | scan mask |   wosize   | col | tag |
     +-----+--------+-----------+------------+-----+-----+

   and for the shared "layout word" format used by header format 2,
   layout blocks, and the GC mark stack:

      63......58  57........................0
     +----------+----------------------------+
     |  repeat  |    scan mask (58 bits)     |
     +----------+----------------------------+

   Scan mask polarity is 1 = unscannable, everywhere.

   This is the single source for these values: the runtime reads them
   from here (via mlvalues.h), and the compiler reads them through the
   generated utils/header_bits.ml.  This file must therefore remain
   plain pre-processor macros -- no C constructs -- because it is
   preprocessed into OCaml source.

HEADER_RESERVED_BITS ==
    HEADER_SCAN_MASK_BITS + HEADER_REPEAT_BITS + HEADER_FORMAT_BITS

 */

#define HEADER_TAG_BITS 8
#define HEADER_COLOR_BITS 2
#define HEADER_WOSIZE_BITS 32
#define HEADER_SCAN_MASK_BITS 16
#define HEADER_REPEAT_BITS 4
#define HEADER_FORMAT_BITS 2

#define HEADER_COLOR_SHIFT     HEADER_TAG_BITS
#define HEADER_WOSIZE_SHIFT    (HEADER_COLOR_SHIFT     + HEADER_COLOR_BITS)
#define HEADER_SCAN_MASK_SHIFT (HEADER_WOSIZE_SHIFT    + HEADER_WOSIZE_BITS)
#define HEADER_REPEAT_SHIFT    (HEADER_SCAN_MASK_SHIFT + HEADER_SCAN_MASK_BITS)
#define HEADER_FORMAT_SHIFT    (HEADER_REPEAT_SHIFT    + HEADER_REPEAT_BITS)

/* Header formats (bits 63-62). */
#define HEADER_FORMAT_LEGACY   0
#define HEADER_FORMAT_INLINE   1
#define HEADER_FORMAT_EXTENDED 2
#define HEADER_FORMAT_EXTERNAL 3

/* The layout word: 58 scan mask bits below a 6-bit repeat count.
   Repeat values above LAYOUT_WORD_REPEAT_MAX are reserved (invalid). */
#define LAYOUT_WORD_MASK_BITS    58
#define LAYOUT_WORD_REPEAT_BITS  6
#define LAYOUT_WORD_REPEAT_SHIFT LAYOUT_WORD_MASK_BITS
#define LAYOUT_WORD_REPEAT_MAX   LAYOUT_WORD_MASK_BITS

#endif /* CAML_HEADER_BITS_H */
