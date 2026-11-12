#define CAML_INTERNALS

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/gc.h>
#include <caml/minor_gc.h>
#include <caml/mlvalues.h>
#include <caml/address_class.h>
#include <caml/shared_heap.h>

/* A fixed pseudo-random sequence (xorshift), so failures reproduce. */
static uint64_t rng;

static uint64_t rnd(void)
{
  rng ^= rng << 13;
  rng ^= rng >> 7;
  rng ^= rng << 17;
  return rng;
}

/* A random mask of [bits] bits: see scan_mask_stubs.c. */
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

#define SLOTS 32          /* batches alive at once */
#define BATCH 4           /* blocks in a batch */
#define MAX_WOSIZE 4096
#define MAX_LAYOUT 12     /* scan words in a layout block */

/* What a word of a block holds. */
enum kind {
  RAW,        /* unscannable: must survive bit for bit */
  IMMEDIATE,
  CHILD,      /* a block of its own, young when built */
  SHARED,     /* the batch's shared young block */
  STATIC,     /* a pointer outside the heap */
  EARLIER     /* an earlier block of the batch */
};

static struct expect {
  mlsize_t wosize;
  tag_t tag;
  reserved_t reserved;
  int format;
  unsigned char kind[MAX_WOSIZE];
  uintnat word[MAX_WOSIZE]; /* RAW, IMMEDIATE, STATIC: the word;
                               CHILD: its tag; EARLIER: its index */
} expect[SLOTS][BATCH];

static int batch_len[SLOTS];

/* Layout blocks for format 11: a header, then scan words. They are
   outside the heap, and never freed or reused: the collector may
   still scan a block after the test has dropped it. */
static uintnat *layout[SLOTS][BATCH];

/* Half the blocks are also global roots, so that the collector comes
   to some directly and to others through the tuple that holds them:
   it treats the two cases differently. */
static value roots[SLOTS][BATCH];
static int roots_registered;

/* A static block which nothing scannable points to. Some raw words
   hold its address: a collector which wrongly treats one as a
   pointer marks the decoy, changing its header. */
static uintnat decoy[3];
static uintnat decoy_header;

static void decoy_arm(void)
{
  decoy_header = Make_header(2, 0, caml_global_heap_state.UNMARKED);
  decoy[0] = decoy_header;
  decoy[1] = decoy[2] = (uintnat)Val_unit;
}

/* Choose the format, size and scan mask of block [b] of slot [s],
   setting each word's kind to RAW or (provisionally) IMMEDIATE. The
   result obeys the rules for each format: a repeat only where the
   mask doesn't cover the block, and ones in any mask bits beyond the
   end of the block. */
static void shape(int s, int b)
{
  struct expect *e = &expect[s][b];
  static unsigned char unscannable[MAX_WOSIZE];
  mlsize_t limit = (rnd() % 8 == 0) ? MAX_WOSIZE - 1
                   : (rnd() & 1) ? 40 : Max_young_wosize - 1;
  mlsize_t user = 1 + rnd() % limit;
  uintnat hmask = 0, hrepeat = 0;
  e->tag = rnd() % 200;
  e->format = 1 + rnd() % 3;
  if (e->format == HEADER_FORMAT_EXTENDED && user < 17) user += 17;
  switch (e->format) {
  case HEADER_FORMAT_INLINE:
    hmask = rmask(HEADER_SCAN_MASK_BITS);
    if (user < HEADER_SCAN_MASK_BITS) {
      hmask |= Low_bits_mask(HEADER_SCAN_MASK_BITS) & ~Low_bits_mask(user);
    } else {
      if (hmask == 0) hmask = (uintnat)1 << (rnd() % HEADER_SCAN_MASK_BITS);
      if (user > HEADER_SCAN_MASK_BITS)
        hrepeat = 1 + rnd() % (HEADER_SCAN_MASK_BITS - 1);
    }
    e->wosize = user;
    for (mlsize_t i = 0; i < user; i++)
      unscannable[i] = ref_inline(hmask, hrepeat, i);
    break;
  case HEADER_FORMAT_EXTENDED: {
    mlsize_t in_word = user - HEADER_SCAN_MASK_BITS;
    uintnat layout_word = rmask(LAYOUT_WORD_MASK_BITS);
    if (in_word <= LAYOUT_WORD_MASK_BITS) {
      layout_word |=
        Low_bits_mask(LAYOUT_WORD_MASK_BITS) & ~Low_bits_mask(in_word);
    } else {
      uintnat repeat = 1 + rnd() % LAYOUT_WORD_REPEAT_MAX;
      layout_word |= repeat << LAYOUT_WORD_REPEAT_SHIFT;
    }
    hmask = rmask(HEADER_SCAN_MASK_BITS);
    e->wosize = user + 1;
    unscannable[0] = 1;
    e->word[0] = layout_word;
    for (mlsize_t i = 0; i < user; i++)
      unscannable[i + 1] =
        i < HEADER_SCAN_MASK_BITS
        ? ref_inline(hmask, 0, i)
        : ref_scan_words(&layout_word, 1, i - HEADER_SCAN_MASK_BITS);
    break;
  }
  default: { /* HEADER_FORMAT_EXTERNAL */
    /* Either an exact bitmap, or a shorter one with a repeat. */
    mlsize_t exact = (user + LAYOUT_WORD_MASK_BITS - 1) / LAYOUT_WORD_MASK_BITS;
    mlsize_t n = exact;
    uintnat repeat = 0;
    if (exact > MAX_LAYOUT || (exact > 1 && (rnd() & 1))) {
      mlsize_t most = exact - 1 < MAX_LAYOUT ? exact - 1 : MAX_LAYOUT;
      n = 1 + rnd() % most;
      repeat = 1 + rnd() % LAYOUT_WORD_REPEAT_MAX;
    }
    uintnat *l = malloc((1 + n) * sizeof(uintnat));
    if (l == NULL) caml_failwith("gc_masked_build: out of memory");
    layout[s][b] = l;
    l[0] = Make_header(n, Abstract_tag, NOT_MARKABLE);
    for (mlsize_t j = 0; j < n; j++)
      l[1 + j] = rmask(LAYOUT_WORD_MASK_BITS);
    if (repeat == 0) {
      mlsize_t in_last = user - (n - 1) * LAYOUT_WORD_MASK_BITS;
      l[n] |= Low_bits_mask(LAYOUT_WORD_MASK_BITS) & ~Low_bits_mask(in_last);
    }
    l[n] |= repeat << LAYOUT_WORD_REPEAT_SHIFT;
    e->wosize = user + 1;
    unscannable[0] = 0;
    for (mlsize_t i = 0; i < user; i++)
      unscannable[i + 1] = ref_scan_words(&l[1], n, i);
    break;
  }
  }
  e->reserved =
    ((uintnat)e->format << (HEADER_FORMAT_SHIFT - HEADER_RESERVED_SHIFT))
    | (hrepeat << (HEADER_REPEAT_SHIFT - HEADER_RESERVED_SHIFT))
    | (hmask << (HEADER_SCAN_MASK_SHIFT - HEADER_RESERVED_SHIFT));
  for (mlsize_t i = 0; i < e->wosize; i++)
    e->kind[i] = unscannable[i] ? RAW : IMMEDIATE;
}

/* Build a batch of blocks with scan masks in slot [slot]: small ones
   in the minor heap, large ones in the major heap. Returns a young
   tuple holding them. */
value gc_masked_build(value seed, value slot)
{
  int s = Int_val(slot);
  static value child[MAX_WOSIZE];
  value blocks[BATCH];
  rng = (uint64_t)Long_val(seed) * UINT64_C(0x9E3779B97F4A7C15) + 1;
  /* Empty the minor heap, so that nothing below can cause a GC while
     blocks are half built. */
  caml_minor_collection();
  decoy_arm();
  if (!roots_registered) {
    for (int i = 0; i < SLOTS; i++) {
      for (int b = 0; b < BATCH; b++) {
        roots[i][b] = Val_unit;
        caml_register_generational_global_root(&roots[i][b]);
      }
    }
    roots_registered = 1;
  }
  batch_len[s] = 1 + rnd() % BATCH;
  value shared = caml_alloc_small(1, 7);
  Field(shared, 0) = Val_long(-7);
  /* Dead after the next minor GC: raw words may hold its address. */
  value tempting = caml_alloc_small(1, 0);
  Field(tempting, 0) = Val_long(0);

  for (int b = 0; b < batch_len[s]; b++) {
    struct expect *e = &expect[s][b];
    shape(s, b);
    /* Some blocks start with a run of immediates, which the marker
       treats specially. */
    mlsize_t immediates = (rnd() % 4 == 0) ? rnd() % 12 : 0;
    /* Children first, so the block is never half built across an
       allocation. */
    for (mlsize_t i = 0; i < e->wosize; i++) {
      if (e->kind[i] == RAW) {
        if (!(e->format == HEADER_FORMAT_EXTENDED && i == 0)) {
          switch (rnd() % 4) {
          case 0: e->word[i] = (uintnat)tempting; break;
          case 1: e->word[i] = (uintnat)&decoy[1]; break;
          default: e->word[i] = rnd(); break;
          }
        }
        child[i] = (value)e->word[i];
      } else if (e->format == HEADER_FORMAT_EXTERNAL && i == 0) {
        e->kind[i] = STATIC;
        e->word[i] = (uintnat)&layout[s][b][1];
        child[i] = (value)e->word[i];
      } else if (immediates > 0) {
        -- immediates;
        e->kind[i] = IMMEDIATE;
        e->word[i] = (uintnat)Val_long(rnd() >> 2);
        child[i] = (value)e->word[i];
      } else {
        switch (rnd() % 5) {
        case 0:
          e->kind[i] = IMMEDIATE;
          e->word[i] = (uintnat)Val_long(rnd() >> 2);
          child[i] = (value)e->word[i];
          break;
        case 1:
          e->kind[i] = CHILD;
          e->word[i] = 1 + rnd() % 100; /* tag */
          child[i] = caml_alloc_small(1, e->word[i]);
          Field(child[i], 0) = Val_long(i);
          break;
        case 2:
          e->kind[i] = SHARED;
          child[i] = shared;
          break;
        case 3:
          e->kind[i] = STATIC;
          e->word[i] = (uintnat)Atom(0);
          child[i] = Atom(0);
          break;
        default:
          if (b == 0) {
            e->kind[i] = IMMEDIATE;
            e->word[i] = (uintnat)Val_long(b);
            child[i] = (value)e->word[i];
          } else {
            e->kind[i] = EARLIER;
            e->word[i] = rnd() % b;
            child[i] = blocks[e->word[i]];
          }
          break;
        }
      }
    }
    if (e->wosize <= Max_young_wosize) {
      blocks[b] = caml_alloc_small_with_reserved(e->wosize, e->tag,
                                                 e->reserved);
      for (mlsize_t i = 0; i < e->wosize; i++)
        Field(blocks[b], i) = child[i];
    } else {
      blocks[b] = caml_alloc_shr_reserved(e->wosize, e->tag, e->reserved);
      for (mlsize_t i = 0; i < e->wosize; i++) {
        if (e->kind[i] == RAW) Field(blocks[b], i) = child[i];
        else caml_initialize(&Field(blocks[b], i), child[i]);
      }
    }
  }
  value holder = caml_alloc_small(BATCH + 1, 0);
  for (int b = 0; b < BATCH; b++) {
    value v = b < batch_len[s] ? blocks[b] : Val_unit;
    Field(holder, b) = v;
    if ((s + b) % 2 == 0)
      caml_modify_generational_global_root(&roots[s][b], v);
  }
  Field(holder, BATCH) = shared;
  if (!Is_young(holder) || !Is_young(shared))
    caml_failwith("gc_masked_build: a GC happened during the build");
  return holder;
}

static char message[200];

/* Check the batch in slot [slot], held by [holder], after any amount
   of garbage collection. */
value gc_masked_check(value holder, value slot)
{
  int s = Int_val(slot);
  value shared = Field(holder, BATCH);
  int b = 0;
  mlsize_t i = 0;
  const char *what = "decoy was marked";
  if (decoy[0] != decoy_header) goto fail;
  what = "holder is young";
  if (Is_young(holder)) goto fail;
  what = "shared";
  if (Is_young(shared) || Tag_val(shared) != 7
      || Field(shared, 0) != Val_long(-7)) goto fail;
  for (b = 0; b < batch_len[s]; b++) {
    struct expect *e = &expect[s][b];
    value v = Field(holder, b);
    i = 0;
    what = "block is young";
    if (!Is_block(v) || Is_young(v)) goto fail;
    what = "header";
    if (Wosize_val(v) != e->wosize || Tag_val(v) != e->tag
        || Reserved_val(v) != e->reserved) goto fail;
    for (i = 0; i < e->wosize; i++) {
      value f = Field(v, i);
      switch (e->kind[i]) {
      case RAW:
        what = "raw word changed";
        if ((uintnat)f != e->word[i]) goto fail;
        break;
      case IMMEDIATE: case STATIC:
        what = "immediate or static word changed";
        if ((uintnat)f != e->word[i]) goto fail;
        break;
      case CHILD:
        what = "child lost";
        if (!Is_block(f) || Is_young(f) || Wosize_val(f) != 1
            || Tag_val(f) != e->word[i] || Field(f, 0) != Val_long(i))
          goto fail;
        break;
      case SHARED:
        what = "shared child";
        if (f != shared) goto fail;
        break;
      case EARLIER:
        what = "pointer to earlier block";
        if (f != Field(holder, e->word[i])) goto fail;
        break;
      }
    }
  }
  return Val_unit;
 fail:
  snprintf(message, sizeof message,
           "%s: block %d of %d, format %d, wosize %lu, word %lu",
           what, b, batch_len[s],
           b < batch_len[s] ? expect[s][b].format : -1,
           b < batch_len[s] ? (unsigned long)expect[s][b].wosize : 0,
           (unsigned long)i);
  caml_failwith(message);
}
