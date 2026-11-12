#define CAML_INTERNALS

#include <stdint.h>
#include <stdio.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/scan_mask.h>

/* A fixed pseudo-random sequence (xorshift), so failures reproduce. */
static uint64_t rng = UINT64_C(88172645463325252);

static uint64_t rnd(void)
{
  rng ^= rng << 13;
  rng ^= rng >> 7;
  rng ^= rng << 17;
  return rng;
}

/* A random mask of [bits] bits. Most are built from alternating runs
   of zeros and ones, with run lengths up to a limit chosen per mask,
   so they range from a single run through a few transitions to
   short-run noise. Uniform and fully random masks are mixed in. */
static uintnat rmask(uint32_t bits)
{
  switch (rnd() % 8) {
  case 0: return 0;
  case 1: return Low_bits_mask(bits);
  case 2: return Low_bits_of(rnd(), bits);
  default: {
    uint32_t max_run = 1 + rnd() % bits;
    uintnat bit = rnd() & 1;
    uintnat mask = 0;
    for (uint32_t pos = 0; pos < bits; bit ^= 1) {
      uint32_t run = 1 + rnd() % max_run;
      for (; run > 0 && pos < bits; run--, pos++)
        mask |= bit << pos;
    }
    return mask;
  }
  }
}

static header_t make_header(uintnat format, uintnat repeat, uintnat mask,
                            mlsize_t wosize)
{
  return (format << HEADER_FORMAT_SHIFT) | (repeat << HEADER_REPEAT_SHIFT)
         | (mask << HEADER_SCAN_MASK_SHIFT)
         | ((header_t)wosize << HEADER_WOSIZE_SHIFT);
}

/* Reference: whether user word [i] is unscannable, given [n] scan
   words, of which only the last has a repeat. */
static int ref_scan_words(const uintnat *w, mlsize_t n, mlsize_t i)
{
  mlsize_t bits = LAYOUT_WORD_MASK_BITS * n;
  if (i < bits)
    return (w[i / LAYOUT_WORD_MASK_BITS] >> (i % LAYOUT_WORD_MASK_BITS)) & 1;
  uintnat repeat = w[n - 1] >> LAYOUT_WORD_REPEAT_SHIFT;
  if (repeat == 0) return 0;
  return (w[n - 1]
          >> (LAYOUT_WORD_MASK_BITS - repeat + (i - bits) % repeat)) & 1;
}

/* Reference: the same for an inline header mask and repeat. */
static int ref_inline(uintnat mask, uintnat repeat, mlsize_t i)
{
  if (i < HEADER_SCAN_MASK_BITS) return (mask >> i) & 1;
  if (repeat == 0) return 0;
  return (mask >> (HEADER_SCAN_MASK_BITS - repeat
                   + (i - HEADER_SCAN_MASK_BITS) % repeat)) & 1;
}

#define MAX_WOSIZE 600

/* Half the blocks are no bigger than this, to exercise the sizes at
   which the header mask, a layout word, and their repeats run out. */
#define SMALL_WOSIZE 80

static char message[100];

/* Run the iterator over block [v] from word [from], and compare its
   classification of every word from there on with [unscannable]. */
static void check_from(value v, const char *format,
                       const unsigned char *unscannable, mlsize_t from)
{
  header_t hd = Hd_val(v);
  mlsize_t wosize = Wosize_hd(hd);
  mlsize_t expected = from; /* the next word not yet accounted for */
  mlsize_t i;
  caml_block_scan bs;
  caml_block_scan_init(&bs, v, hd);
  if (from > 0) caml_block_scan_skip_to(&bs, from);
  while (caml_block_scan_next(&bs, &i)) {
    if (i < expected || i >= wosize) goto fail;
    for (; expected < i; expected++)
      if (!unscannable[expected]) goto fail;
    if (unscannable[i]) goto fail;
    expected = i + 1;
  }
  for (; expected < wosize; expected++)
    if (!unscannable[expected]) goto fail;
  return;
 fail:
  snprintf(message, sizeof message,
           "scan_mask_test: format %s, wosize %lu, from %lu, word %lu",
           format, (unsigned long)wosize, (unsigned long)from,
           (unsigned long)expected);
  caml_failwith(message);
}

/* Check block [v] from the start, and from a few random words. */
static void check(value v, const char *format,
                  const unsigned char *unscannable)
{
  mlsize_t wosize = Wosize_val(v);
  check_from(v, format, unscannable, 0);
  for (int k = 0; k < 3; k++)
    check_from(v, format, unscannable, rnd() % (wosize + 1));
}

value scan_mask_test(value unit)
{
  static uintnat block[MAX_WOSIZE + 2];
  static uintnat layout[5];
  static unsigned char unscannable[MAX_WOSIZE + 2];
  value v = (value)&block[1];

  for (int trial = 0; trial < 20000; trial++) {
    mlsize_t limit = (rnd() & 1) ? SMALL_WOSIZE : MAX_WOSIZE - 2;
    mlsize_t user = rnd() % limit; /* user words */
    switch (rnd() % 3) {
    case 0: { /* format 01 */
      uintnat mask = rmask(HEADER_SCAN_MASK_BITS);
      uintnat repeat = rnd() % HEADER_SCAN_MASK_BITS;
      block[0] = make_header(HEADER_FORMAT_INLINE, repeat, mask, user);
      for (mlsize_t i = 0; i < user; i++)
        unscannable[i] = ref_inline(mask, repeat, i);
      check(v, "01", unscannable);
      break;
    }
    case 1: { /* format 10: word 0 is the layout word, never scanned */
      uintnat mask = rmask(HEADER_SCAN_MASK_BITS);
      uintnat repeat = rnd() % (LAYOUT_WORD_REPEAT_MAX + 1);
      uintnat layout_word = rmask(LAYOUT_WORD_MASK_BITS)
                            | (repeat << LAYOUT_WORD_REPEAT_SHIFT);
      block[0] = make_header(HEADER_FORMAT_EXTENDED, 0, mask, user + 1);
      block[1] = layout_word;
      unscannable[0] = 1;
      for (mlsize_t i = 0; i < user; i++)
        unscannable[i + 1] =
          i < HEADER_SCAN_MASK_BITS
          ? ref_inline(mask, 0, i)
          : ref_scan_words(&layout_word, 1, i - HEADER_SCAN_MASK_BITS);
      check(v, "10", unscannable);
      break;
    }
    default: { /* format 11: word 0 points to the layout block */
      mlsize_t n = 1 + rnd() % 4;
      uintnat repeat = rnd() % (LAYOUT_WORD_REPEAT_MAX + 1);
      layout[0] = make_header(HEADER_FORMAT_LEGACY, 0, 0, n) | Abstract_tag;
      for (mlsize_t j = 0; j < n; j++)
        layout[1 + j] = rmask(LAYOUT_WORD_MASK_BITS);
      layout[n] |= repeat << LAYOUT_WORD_REPEAT_SHIFT;
      block[0] = make_header(HEADER_FORMAT_EXTERNAL, 0, 0, user + 1);
      block[1] = (uintnat)&layout[1];
      unscannable[0] = 0;
      for (mlsize_t i = 0; i < user; i++)
        unscannable[i + 1] = ref_scan_words(&layout[1], n, i);
      check(v, "11", unscannable);
      break;
    }
    }
  }

  /* Format 00: every word is scannable. */
  block[0] = make_header(HEADER_FORMAT_LEGACY, 0, 0, 300);
  for (mlsize_t i = 0; i < 300; i++) unscannable[i] = 0;
  check(v, "00", unscannable);

  return Val_unit;
}
