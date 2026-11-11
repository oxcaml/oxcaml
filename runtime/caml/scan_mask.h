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

#ifndef CAML_SCAN_MASK_H
#define CAML_SCAN_MASK_H

#ifdef CAML_INTERNALS

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include "mlvalues.h"

#ifdef ARCH_SIXTYFOUR

/* Iteration over the scannable words of a block.

   See header_bits.h for format description. A block's scannability is
   described by one or more "scan words" in a mask+repeat
   format. Words beyond a scan word's mask take its last [repeat] mask
   bits, tiled.

   A scanner initializes an iterator with [caml_block_scan_init] and
   calls [caml_block_scan_next] until it returns false; each call
   yields the block index of the next scannable word. Closures and
   No_scan or special tags are the caller's business. */

/* TODO: when the rest of a block is entirely scannable (the repeat
   pattern is all zero), callers could be given a plain index range
   rather than one word per call. */

/* The most mask bits a window or refill can hold: one fewer than a
   word, so that the top bit is always padding. The padding above the
   valid bits is all ones (unscannable), and consuming the window with
   an arithmetic shift keeps it so. */
#define CAML_SCAN_WINDOW_MAX ((uint32_t)(CHAR_BIT * sizeof(uintnat) - 1))

typedef struct caml_block_scan {
  uintnat window;        /* unconsumed mask bits; bit 0 covers [pos] */
  uintnat refill;        /* the repeat pattern, replicated on demand */
  uint32_t left;         /* valid bits in window; higher bits are 1 */
  uint32_t refill_len;   /* valid bits in refill; higher bits are 1 */
  mlsize_t pos;          /* block index of the next word to consider */
  mlsize_t end;          /* the block's size in words */
  const value* more;     /* further scan words, if any */
  const value* more_end;
} caml_block_scan;

/* Start consuming a mask of [bits] bits, to be followed by its last
   [repeat] bits, tiled. [mask] has no bits at or above [bits]. The
   refill starts as one copy of the repeat pattern: most blocks end
   within the mask, and never need more. With no repeat the refill is
   all scannable. */
Caml_inline void caml_block_scan_load(caml_block_scan* s, uintnat mask,
                                      uint32_t bits, uint32_t repeat)
{
  CAMLassert(bits <= CAML_SCAN_WINDOW_MAX);
  CAMLassert((mask >> bits) == 0);
  s->window = mask | ~Low_bits_mask(bits); /* set all top bits */
  s->left = bits;
  if (repeat == 0) { /* rest is scannable */
    s->refill = ~Low_bits_mask(CAML_SCAN_WINDOW_MAX); /* set all top bits */
    s->refill_len = CAML_SCAN_WINDOW_MAX;
  } else {
    CAMLassert(repeat <= bits);
    s->refill = (mask >> (bits - repeat))
                | ~Low_bits_mask(repeat); /* set all top bits */
    s->refill_len = repeat;
  }
}

/* Start consuming scan word [w]. */
Caml_inline void caml_block_scan_load_word(caml_block_scan* s, uintnat w)
{
  caml_block_scan_load(s, Layout_scan_mask(w), LAYOUT_WORD_MASK_BITS,
                       Layout_repeat(w));
}

/* The window is exhausted: continue with the next scan word, or
   else with the repeat pattern. Note a format constraint: if there
   are more layout words, there cannot be a current refill. */
Caml_inline void caml_block_scan_reload(caml_block_scan* s)
{
  if (s->more != s->more_end) {
    caml_block_scan_load_word(s, (uintnat)*s->more);
    s->more++;
  } else {
    /* The first use of the refill replicates the pattern, by doubling,
       to a whole number of copies: reloading it then preserves phase,
       and the iterator does no per-word division. */
    while (2 * s->refill_len <= CAML_SCAN_WINDOW_MAX) {
      /* The shifted copy brings the top bits with it. */
      s->refill = (s->refill << s->refill_len)
                  | Low_bits_of(s->refill, s->refill_len);
      s->refill_len *= 2;
    }
    s->window = s->refill;
    s->left = s->refill_len;
  }
}

Caml_inline void caml_block_scan_init(caml_block_scan* s, value v,
                                      header_t hd)
{
  s->end = Wosize_hd(hd);
  s->pos = 0;
  s->more = NULL;
  s->more_end = NULL;
  switch (Format_hd(hd)) {
  case HEADER_FORMAT_LEGACY:
    /* No mask: all scannable. */
    caml_block_scan_load(s, 0, HEADER_SCAN_MASK_BITS, 0);
    break;
  case HEADER_FORMAT_INLINE:
    caml_block_scan_load(s, Scan_mask_hd(hd), HEADER_SCAN_MASK_BITS,
                         Repeat_hd(hd));
    break;
  case HEADER_FORMAT_EXTENDED:
    /* Word 0 is the layout word, a raw scan word, so start after it.
       The header mask covers user words 0-15; the layout word
       continues from user word 16. */
    s->pos = 1;
    caml_block_scan_load(s, Scan_mask_hd(hd), HEADER_SCAN_MASK_BITS, 0);
    s->more = Op_val(v);
    s->more_end = Op_val(v) + 1;
    break;
  default: { /* HEADER_FORMAT_EXTERNAL */
    /* Word 0 is the layout word, a pointer to the layout block, so
       it is itself scannable: start with a one-bit window saying so.
       The layout block's scan words then cover the user words. */
    value layout_block = Field(v, 0);
    CAMLassert(Tag_val(layout_block) == Abstract_tag);
    caml_block_scan_load(s, 0, 1, 0);
    s->more = Op_val(layout_block);
    s->more_end = Op_val(layout_block) + Wosize_val(layout_block);
    break;
  }
  }
}

/* Set [*i_p] to the block index of the next scannable word and return
   true, or return false if there are no more. */
Caml_inline bool caml_block_scan_next(caml_block_scan* s, mlsize_t* i_p)
{
  while (s->pos < s->end) {
    if (s->window == ~(uintnat)0) {
      /* No scannable word in this window, which may be exhausted. If
         only the repeat pattern remains and it has none either, nor
         has the rest of the block. */
      s->pos += s->left;
      if (s->pos >= s->end) break;
      if (s->more == s->more_end && s->refill == ~(uintnat)0) break;
      caml_block_scan_reload(s);
      continue;
    }
    uint32_t skip = caml_ctz(~s->window);
    s->pos += skip;
    if (s->pos >= s->end) break;
    *i_p = s->pos;
    ++ s->pos;
    /* An arithmetic shift, bringing ones in at the top. */
    s->window = (uintnat)((intnat)s->window >> (skip + 1));
    s->left -= skip + 1;
    return true;
  }
  return false;
}

#endif /* ARCH_SIXTYFOUR */
#endif /* CAML_INTERNALS */

#endif /* CAML_SCAN_MASK_H */
