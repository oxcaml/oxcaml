/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <pthread.h>
#include <stdatomic.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/resource.h> /* for the prewarm thread's priority */
#include <sys/syscall.h>
#endif

#include "caml/backtrace_prim.h"
#include "caml/camlatomic.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/frame_descriptors.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"

/* Mapping return addresses to frame descriptors.

   ocamlopt emits, per compilation unit, a "frametable" describing every
   stack frame of the unit's code: for each call site, the frame's size
   and the locations of the GC roots in it (the format is described in
   caml/frame_descriptors.h). The GC, backtrace, statmemprof and signal
   code all ask the same question: given a return address, find its
   descriptor (caml_find_frame_descr), or NULL if it has none.

   To answer that question, we use compact per-unit indexes (about 5.6
   bytes per descriptor), mostly built lazily, with a small per-domain
   cache in front.

   1. Glossary and data design

   unit: one frametable registered before the program runs: one
   caml_frametable[] entry (with LINK_ORDER_FRAMETABLES, one
   [begin, end) range), or one -manual-module-init table. At startup,
   units_init records {first return address, table, count} per unit,
   sorted; no descriptor is decoded. Unit i is assumed to span
   [first_i, first_i+1) -- an assumption, see section 3.

   segment: up to SEGMENT_MAX_COUNT consecutive descriptors of one unit
   with increasing return addresses, spanning < 64 KiB of code and
   < 64 KiB of table: {start, first descriptor body, side, count}.

   side entry: one uint32 per descriptor:
   (return address - segment start) | (body - first body) << 16.

   A lookup scans at most SEGMENT_MAX_COUNT side entries -- one or two
   cache lines -- and a match yields the descriptor body directly.

   unit index: a unit's segments + side entries, built in one decode
   walk of the unit by the first lookup landing in it
   (unit_index_build), allocated from a reserved arena, CAS-published.

   buckets: unit_buckets[] (filled at startup) maps a pc to its unit;
   segment_buckets[] (filled as each unit is built) maps a pc to a
   starting segment; both one uint32 per 4 KiB of code, finished off by
   short forward scans.

   segment_index: the same segments + side entries, plus its own bucket
   table, built eagerly over any set of tables. Each late-registered
   table (natdynlink, a JIT, caml_copy_and_register_*) gets its own --
   an "extra" index, checked first -- and the "fallback" index covers
   every unit (section 3).

   2. Build policies

   Lazy (the default): startup records units only, and each unit's
   index is built by its first lookup: one decode walk, so that lookup
   stalls by up to a few hundred microseconds (possibly inside a GC
   stack scan). Eager: late registrations, always; the whole program,
   under OCAMLRUNPARAM=Xframe_index_eager=1 (startup pays the full
   build, lookups never stall). Xframe_index_prewarm=1 instead builds
   every unit on a background thread started after startup.

   3. The fallback rule: lookups never wrongly return NULL

   A unit's code need not all lie below the next unit's first return
   address (for example, -function-sections plus a linker ordering file
   can interleave units). The lazy indexes are therefore never trusted
   for a miss: a pc not found in the unit it lands in is answered by an
   eager index over every unit, built at most once and CAS-published
   (fallback_get); once it exists, all lookups use it alone. In the
   common contiguous layout, only pcs in no frametable at all reach it.
   Unregistering a unit's table also switches to a (rebuilt) fallback
   index: the lazy structures are append-only and never shrink.

   4. Registration and the generation counter

   Registration runs on the final domain of a STW section
   (stw_register_frametables). Lookups take no locks: the structures
   only change when no other domain runs OCaml code, and systhreads
   only look descriptors up while holding their domain's lock.
   Unregistration (allowed at any time, under [mutex]) only moves
   tables to a zombie list; the next registration reclaims them, so an
   unregistered table remains findable until then. Re-registering an
   already-indexed table changes nothing: in particular the
   -manual-module-init tables, pre-indexed at startup (sound because an
   uninitialised unit's code cannot be on any stack), register for
   free. [index_generation] is bumped by any change; each domain's
   cache empties itself when it next sees a new generation, so a cached
   descriptor pointer never outlives its frametable.

   5. Concurrency and signals on the lookup path

   Lookups are lock-free and malloc-free: they run during GC and,
   prospectively, from signal handlers. A unit is built by at most one
   thread, the first to CAS its pid into [builder] (a pid, not a thread
   id, so a claim inherited across fork() is re-taken in the child). A
   lookup finding the unit claimed but unpublished does a linear decode
   walk of the unit instead of waiting: that costs about as much as the
   build, and waiting risks priority inversion. Builds allocate from
   [arena], reserved at startup and bump-allocated with atomics; if it
   is ever exhausted (it is sized at twice the worst case), lookups in
   unbuilt units degrade to linear walks. The fallback build maps fresh
   memory: no malloc. [index_building] flags that this thread is
   mid-build, so a lookup re-entered from a signal handler walks tables
   linearly rather than touching a build in progress. Cache entries are
   written with signal fences in an order that a same-thread signal
   handler can never misread (cache_write).

   6. The per-domain cache

   In front of everything sits a direct-mapped pc -> descriptor cache
   per domain (Caml_state->frame_descr_cache, created with the domain;
   see CACHE_BITS). It is keyed on the raw return address and
   invalidated by generation (section 4). Threads without a domain
   (and domains whose cache allocation failed) look up uncached.

   7. Diagnostics

   OCAMLRUNPARAM=Xframe_index_check=1 rebuilds the replaced hash table
   at every registration and checks exhaustive lookups against it
   (check_run; fatal on any mismatch). Xmeasure_frametables=1 prints
   the frametable statistics report plus a summary of the index. */

/* Diagnostics, settable as OCAMLRUNPARAM "X" GC tweaks. All four are
   defined in gc_ctrl.c, which both runtimes link. */
extern uintnat caml_frame_index_check;   /* Xframe_index_check=1 */
extern uintnat caml_frame_index_eager;   /* Xframe_index_eager=1 */
extern uintnat caml_frame_index_prewarm; /* Xframe_index_prewarm=1 */
extern uintnat caml_measure_frametables; /* Xmeasure_frametables=1 */

/**** Decoding descriptors ****/

/* Decode one descriptor (short or escaped) into [out], filling in its
   flags, sizes, and the pointers needed to locate its live offsets,
   allocation sizes, and debug words. [d] points at the descriptor body:
   the byte after its LEB128 delta (short), or the escape byte itself
   (medium/long). */
void caml_decode_frame_descr(frame_descr *d, struct frame_descr_decoded *out)
{
  memset(out, 0, sizeof(*out));
  if (frame_is_short(d)) {
    const unsigned char *p = (const unsigned char *)d;
    unsigned char sf = *p++; /* size+flags byte */
    out->is_short = true;
    out->has_allocs = (sf & FRAME_DESCRIPTOR_ALLOC) != 0;
    out->has_debug = (sf & FRAME_DESCRIPTOR_DEBUG) != 0;
    out->frame_size = ((uint32_t)(sf >> 2)) * 16;
    if (out->has_allocs) {
      out->short_reg_bitmap = *p++;
      out->num_allocs = *p++;
      out->short_allocs = p;
      p += (out->num_allocs + 1) / 2; /* 4-bit alloc sizes */
    }
    out->short_live_bytes =
      (uint8_t)((out->frame_size / sizeof(value) + 7) / 8);
    out->short_live = p;
    p += out->short_live_bytes;
    out->end_of_live = p;
    out->num_debuginfo = out->has_allocs ? out->num_allocs : 1;
    if (out->has_debug) {
      p += sizeof(uint32_t) * out->num_debuginfo;
    }
    out->end = p;
    return;
  }

  /* Escaped descriptor: normal or long format. */
  if (frame_return_to_C(d)) {
    out->return_to_C = true;
    /* Top of an ML stack chunk: an empty descriptor. */
    CAMLassert(caml_read_unaligned_uint16(d + Frame_num_live_ofs) == 0);
    out->end_of_live = out->end = d + Frame_live_ofs;
    return;
  }
  out->is_long = frame_is_long(d);
  out->has_allocs = frame_has_allocs(d);
  out->has_debug = frame_has_debug(d);
  out->frame_size = frame_size(d);
  const unsigned char *p;
  if (out->is_long) {
    out->num_live = caml_read_unaligned_uint32(d + Frame_long_num_live_ofs);
    p = d + Frame_long_live_ofs + (uintnat)out->num_live * sizeof(uint32_t);
  } else {
    out->num_live = caml_read_unaligned_uint16(d + Frame_num_live_ofs);
    p = d + Frame_live_ofs + (uintnat)out->num_live * sizeof(uint16_t);
  }
  out->end_of_live = p;
  out->num_debuginfo = 1;
  if (out->has_allocs) {
    out->num_allocs = *p;
    out->num_debuginfo = *p;
    p += (uintnat)(*p) + 1; /* num_allocs byte + one byte per alloc */
  }
  if (out->has_debug) {
    p += sizeof(uint32_t) * out->num_debuginfo;
  }
  out->end = p;
}

/* One past the body of a short descriptor: a fast path for the walks
   below, which decode every descriptor of a unit just to step over it.
   Must agree with caml_decode_frame_descr (Xframe_index_check verifies
   that it does, for every registered short descriptor). */
Caml_inline const unsigned char *short_end(const unsigned char *d)
{
  unsigned char sf = d[0];
  const unsigned char *p = d + 1;
  unsigned num_debug = 1;
  if (sf & FRAME_DESCRIPTOR_ALLOC) {
    p++; /* register bitmap */
    unsigned num_allocs = *p++;
    p += (num_allocs + 1) / 2;
    num_debug = num_allocs;
  }
  uint32_t frame_size = ((uint32_t)(sf >> 2)) * 16;
  p += (frame_size / sizeof(value) + 7) / 8;
  if (sf & FRAME_DESCRIPTOR_DEBUG)
    p += sizeof(uint32_t) * num_debug;
  return p;
}

/* One past the body of any descriptor. */
Caml_inline const unsigned char *descr_end(frame_descr *d)
{
  if (frame_is_short(d))
    return short_end(d);
  struct frame_descr_decoded dec;
  caml_decode_frame_descr(d, &dec);
  return dec.end;
}

/* Unsigned LEB128 (a short descriptor's return-address delta; >= 1, so
   it never starts with a 0 byte). */
Caml_inline const unsigned char *read_uleb128(const unsigned char *p,
                                              uintnat *out)
{
  uintnat delta = 0;
  int shift = 0;
  unsigned char byte;
  do {
    byte = *p++;
    delta |= (uintnat)(byte & 0x7f) << shift;
    shift += 7;
  } while (byte & 0x80);
  *out = delta;
  return p;
}

/**** Frametables and iteration ****/

/* Note: [cur] is bound by this macro */
#define iter_list(list,cur) \
  for (caml_frametable_list *cur = list; cur != NULL; cur = cur->next)

/* A frametable in either layout: count-prefixed ([end] == NULL: the
   count word is at [tbl] and the descriptors follow it) or, with
   LINK_ORDER_FRAMETABLES, the descriptor range [tbl, end) whose count
   word at [end] is only an upper bound (the compile-time count, before
   the linker dropped dead functions' pieces). */
typedef struct {
  intnat *tbl;
  const unsigned char *end;
} table_ref;

Caml_inline const unsigned char *table_first(table_ref t)
{
  return t.end != NULL ? (const unsigned char *)t.tbl
                       : (const unsigned char *)(t.tbl + 1);
}

/* The count word: exact for a count-prefixed table, an upper bound for
   a range. */
Caml_inline intnat table_count(table_ref t)
{
  const void *count = t.end != NULL ? (const void *)t.end
                                    : (const void *)t.tbl;
  return (intnat)caml_read_unaligned_uintnat(count);
}

/* Whether a walk of [t] that has yielded [seen] of [count] descriptors
   and stands at [p] has any left. */
Caml_inline bool table_more(table_ref t, const unsigned char *p,
                            intnat seen, intnat count)
{
  return t.end != NULL ? p < t.end : seen < count;
}

Caml_inline table_ref list_ref(const caml_frametable_list *cell)
{
  return (table_ref){ cell->frametable, cell->end };
}

static int table_compare(const void *a, const void *b)
{
  uintnat x = (uintnat)((const table_ref *)a)->tbl;
  uintnat y = (uintnat)((const table_ref *)b)->tbl;
  return x < y ? -1 : x > y ? 1 : 0;
}

/* Iterate over the descriptors of one frametable, reconstructing the
   absolute return address of each descriptor by walking the delta
   chain. An escaped descriptor carries an absolute return address; a
   short descriptor's address is the running address plus its delta. */
typedef struct {
  const unsigned char *next; /* points at the next descriptor's delta byte */
  const unsigned char *end;  /* range: one past the last descriptor;
                                NULL for a count-prefixed table */
  uintnat retaddr;           /* running absolute return address */
  intnat remaining;          /* count-prefixed: descriptors left to yield */
} frametable_iter;

static void frametable_iter_start(frametable_iter *it,
                                  const caml_frametable_list *tbl)
{
  it->end = tbl->end;
  if (tbl->end == NULL) {
    it->remaining = (intnat)caml_read_unaligned_uintnat(tbl->frametable);
    it->next = (const unsigned char *)(tbl->frametable + 1);
  } else {
    it->remaining = 0;
    it->next = (const unsigned char *)tbl->frametable;
  }
  it->retaddr = 0;
}

static void table_iter_start(frametable_iter *it, table_ref t)
{
  caml_frametable_list cell = { t.tbl, t.end, NULL };
  frametable_iter_start(it, &cell);
}

Caml_inline bool frametable_iter_more(const frametable_iter *it)
{
  return it->end != NULL ? it->next < it->end : it->remaining > 0;
}

/* Yield the next descriptor body and its absolute return address. Must
   only be called while [frametable_iter_more(it)]. */
static frame_descr *frametable_iter_next(frametable_iter *it,
                                         uintnat *retaddr_out)
{
  const unsigned char *p = it->next;
  frame_descr *d;
  if (*p == FRAME_DELTA_ESCAPE) {
    d = (frame_descr *)p;
    it->retaddr = Retaddr_frame(d);
  } else {
    uintnat delta;
    p = read_uleb128(p, &delta);
    it->retaddr += delta;
    d = (frame_descr *)p;
  }
  it->next = descr_end(d);
  it->remaining--;
  *retaddr_out = it->retaddr;
  return d;
}

static intnat count_descriptors(caml_frametable_list *list) {
  intnat num_descr = 0;
  iter_list(list,cur) {
    num_descr += table_count(list_ref(cur));
  }
  return num_descr;
}

static caml_frametable_list* frametables_list_tail(caml_frametable_list *list) {
  caml_frametable_list *tail = NULL;
  iter_list(list,cur) {
    tail = cur;
  }
  return tail;
}

/**** Tuning constants ****/

/* A lookup scans at most one segment's side entries: one or two cache
   lines. Longer segments save little index space and lengthen the
   scan. */
#define SEGMENT_MAX_COUNT 16

/* Side entries pack two 16-bit offsets (also used as masks below): a
   segment spans < 64 KiB of code and < 64 KiB of descriptor bytes. */
#define SEGMENT_MAX_SPAN 0xFFFF
#define SEGMENT_MAX_BODY 0xFFFF
#define SIDE_BODY_SHIFT 16

/* One 32-bit bucket per 4 KiB of code (0.1% of text size) starts each
   lookup within a short forward scan of its target. */
#define BUCKET_SHIFT 12

/* Tables far apart in the address space (dynlinked code) would need a
   huge bucket array: widen the buckets until there are at most this
   many per segment (or per unit, for the unit buckets). */
#define BUCKETS_PER_SEGMENT_MAX 4

/* Units at least this big are built with a single decode walk into a
   worst-case-sized allocation (see unit_index_build); smaller units,
   where decoding twice is cheap, take an exact two-pass build so they
   don't consume worst-case arena space. */
#define UNIT_ONE_PASS_MIN 256

/* Segment buckets extend this many buckets (64 MiB of code) past the
   last unit's first return address; pcs beyond that scan from the
   unit's first segment. */
#define SEGMENT_BUCKETS_SLACK ((intnat)1 << 14)

/* Arena allocation granularity. */
#define ARENA_ALIGN 16

/**** The segment index ****/

typedef struct {
  uintnat start;      /* return address of the first descriptor */
  frame_descr *first; /* body of the first descriptor */
  uint32_t side;      /* index of this segment's first side entry */
  uint32_t count;     /* number of descriptors, 1..SEGMENT_MAX_COUNT */
} segment;

/* An eagerly built index over a set of frametables: the fallback index
   and the extra indexes. (Lazily built units use the same segments and
   side entries but share bucket tables: see unit_index.) */
typedef struct {
  intnat ndescr;
  intnat nsegs;
  segment *segs;      /* sorted by [start] */
  uint32_t *side;     /* side entries, indexed by segment.side */
  intnat nside;
  uintnat lo;         /* lowest return address */
  uintnat hi;         /* highest return address */
  /* buckets[b] is the index of the last segment starting at or before
     lo + (b << bucket_shift), for b in [0, nbuckets]. */
  int bucket_shift;
  intnat nbuckets;
  uint32_t *buckets;
  /* Non-NULL iff segments from different tables interleave: maxend[i]
     is the highest end address among segs[0..i], bounding how far left
     of its bucket a lookup must search. */
  uintnat *maxend;
  size_t maxend_len;
  void *region;       /* the mapping holding segs, side and buckets */
  size_t region_len;
} segment_index;

/* Anonymous mappings for index arrays, named in /proc/pid/smaps so
   their resident size can be read off directly, and sized exactly, so
   no build-time temporary lands in malloc free lists. No huge pages:
   the arena and segment buckets rely on sparse residency. */
static void *index_map_noexc(size_t len, const char *name)
{
  return caml_mem_map(caml_mem_round_up_mapping_size(len),
                      CAML_MAP_NO_HUGETLB, name);
}

/* Used only at startup and in STW registration, where the stock
   hashtable raised Out_of_memory too. */
static void *index_map(size_t len, const char *name)
{
  void *p = index_map_noexc(len, name);
  if (p == NULL) caml_raise_out_of_memory();
  return p;
}

static void index_unmap(void *p, size_t len)
{
  if (p != NULL) caml_mem_unmap(p, caml_mem_round_up_mapping_size(len));
}

/* Walk [tables] (an array of [n]), cutting their descriptors into
   segments. With [segs] == NULL this is a sizing pass that only counts;
   otherwise it also fills [segs] and [side], which must have room for
   the counts a sizing pass returned. Returns the number of segments,
   side entries and descriptors, and the lowest and highest return
   address. */
static void segments_cut(const table_ref *tables, intnat n,
                         segment *segs, uint32_t *side,
                         intnat *nsegs_out, intnat *nside_out,
                         intnat *ndescr_out,
                         uintnat *lo_out, uintnat *hi_out)
{
  intnat nsegs = 0, nside = 0, ndescr = 0;
  uintnat lo = (uintnat)-1, hi = 0;
  for (intnat i = 0; i < n; i++) {
    table_ref t = tables[i];
    intnat count = table_count(t);
    intnat seen = 0;
    const unsigned char *p = table_first(t);
    uintnat ret = 0, last_ret = 0, seg_start = 0;
    frame_descr *seg_first = NULL;
    uint32_t seg_count = 0;
    bool in_seg = false;
    while (table_more(t, p, seen, count)) {
      frame_descr *d;
      if (*p == FRAME_DELTA_ESCAPE) {
        d = (frame_descr *)p;
        ret = Retaddr_frame(d);
      } else {
        uintnat delta;
        p = read_uleb128(p, &delta);
        ret += delta;
        d = (frame_descr *)p;
      }
      seen++;
      ndescr++;
      if (!in_seg || seg_count == SEGMENT_MAX_COUNT || ret <= last_ret
          || ret - seg_start > SEGMENT_MAX_SPAN
          || (uintnat)(d - seg_first) > SEGMENT_MAX_BODY) {
        if (segs != NULL) {
          segs[nsegs].start = ret;
          segs[nsegs].first = d;
          segs[nsegs].side = (uint32_t)nside;
          segs[nsegs].count = 0;
        }
        nsegs++;
        in_seg = true;
        seg_start = ret;
        seg_first = d;
        seg_count = 0;
        if (ret < lo) lo = ret;
      }
      if (segs != NULL) {
        side[nside] = (uint32_t)(ret - seg_start)
          | ((uint32_t)(d - seg_first) << SIDE_BODY_SHIFT);
        segs[nsegs - 1].count++;
      }
      nside++;
      seg_count++;
      if (ret > hi) hi = ret;
      last_ret = ret;
      p = descr_end(d);
    }
  }
  *nsegs_out = nsegs;
  *nside_out = nside;
  *ndescr_out = ndescr;
  *lo_out = nsegs > 0 ? lo : 0;
  *hi_out = hi;
}

/* In-place heapsort of segments by [start]: qsort may allocate, and
   unit builds run on the lookup path. */
static void segments_sift_down(segment *a, intnat root, intnat n)
{
  while (2 * root + 1 < n) {
    intnat child = 2 * root + 1;
    if (child + 1 < n && a[child + 1].start > a[child].start) child++;
    if (a[root].start >= a[child].start) return;
    segment t = a[root]; a[root] = a[child]; a[child] = t;
    root = child;
  }
}

static void segments_sort(segment *a, intnat n)
{
  for (intnat i = n / 2 - 1; i >= 0; i--) segments_sift_down(a, i, n);
  for (intnat end = n - 1; end > 0; end--) {
    segment t = a[0]; a[0] = a[end]; a[end] = t;
    segments_sift_down(a, 0, end);
  }
}

static bool segments_sorted(const segment *a, intnat n)
{
  for (intnat i = 1; i < n; i++)
    if (a[i].start < a[i - 1].start) return false;
  return true;
}

/* The highest return address in [s] ([side] is its index's side
   array). */
Caml_inline uintnat segment_end(const segment *s, const uint32_t *side)
{
  return s->start + (side[s->side + s->count - 1] & SEGMENT_MAX_SPAN);
}

/* Scan [s]'s side entries for a descriptor at exactly [pc]. Entries
   are sorted by return address, so stop at the first that reaches it. */
Caml_inline frame_descr *segment_scan(const segment *s,
                                      const uint32_t *side, uintnat pc)
{
  uintnat off = pc - s->start;
  if (off > SEGMENT_MAX_SPAN) return NULL;
  const uint32_t *e = side + s->side;
  for (uint32_t k = 0; k < s->count; k++) {
    uint32_t r = e[k] & SEGMENT_MAX_SPAN;
    if (r >= off)
      return r == off ? s->first + (e[k] >> SIDE_BODY_SHIFT) : NULL;
  }
  return NULL;
}

static int bucket_shift_for(uintnat span, intnat nsegs)
{
  int shift = BUCKET_SHIFT;
  while (shift < 63 &&
         (span >> shift) + 1
           > (uintnat)(BUCKETS_PER_SEGMENT_MAX * (nsegs > 0 ? nsegs : 1)))
    shift++;
  return shift;
}

static void segment_index_free(segment_index *ix)
{
  index_unmap(ix->region, ix->region_len);
  index_unmap(ix->maxend, ix->maxend_len);
  memset(ix, 0, sizeof(*ix));
}

/* Build [ix] over [tables] (an array of [n]), replacing its previous
   contents. With [noexc], returns false and leaves [ix] empty if a
   mapping fails; otherwise raises Out_of_memory. */
static bool segment_index_build(segment_index *ix, const table_ref *tables,
                                intnat n, bool noexc)
{
  /* Pass 1: exact sizes. Pass 2: fill one exactly-sized mapping. */
  intnat nsegs, nside, ndescr;
  uintnat lo, hi;
  segments_cut(tables, n, NULL, NULL, &nsegs, &nside, &ndescr, &lo, &hi);
  uintnat span = nsegs > 0 ? hi - lo : 0;
  int shift = bucket_shift_for(span, nsegs);
  intnat nbuckets = (intnat)(span >> shift) + 1;
  size_t segs_len = (size_t)(nsegs > 0 ? nsegs : 1) * sizeof(segment);
  size_t side_len = (size_t)(nside > 0 ? nside : 1) * sizeof(uint32_t);
  size_t buckets_len = (size_t)(nbuckets + 1) * sizeof(uint32_t);
  size_t len = segs_len + side_len + buckets_len;
  char *region = noexc ? index_map_noexc(len, "frame_descr index")
                       : index_map(len, "frame_descr index");
  if (region == NULL) { segment_index_free(ix); return false; }
  segment *segs = (segment *)region;
  uint32_t *side = (uint32_t *)(region + segs_len);
  uint32_t *buckets = (uint32_t *)(region + segs_len + side_len);
  intnat nsegs2, nside2, ndescr2;
  uintnat lo2, hi2;
  segments_cut(tables, n, segs, side, &nsegs2, &nside2, &ndescr2,
               &lo2, &hi2);
  CAMLassert(nsegs2 == nsegs && nside2 == nside);
  segments_sort(segs, nsegs);

  segment_index_free(ix);
  ix->region = region;
  ix->region_len = len;
  ix->segs = segs;
  ix->side = side;
  ix->nside = nside;
  ix->nsegs = nsegs;
  ix->ndescr = ndescr;

  intnat overlaps = 0;
  uintnat running_max = 0;
  for (intnat i = 0; i < nsegs; i++) {
    if (i > 0 && segs[i].start <= running_max) overlaps++;
    uintnat end = segment_end(&segs[i], side);
    if (end > running_max) running_max = end;
  }
  if (overlaps > 0) {
    ix->maxend_len = (size_t)nsegs * sizeof(uintnat);
    ix->maxend = noexc ? index_map_noexc(ix->maxend_len, "frame_descr index")
                       : index_map(ix->maxend_len, "frame_descr index");
    if (ix->maxend == NULL) { segment_index_free(ix); return false; }
    running_max = 0;
    for (intnat i = 0; i < nsegs; i++) {
      uintnat end = segment_end(&segs[i], side);
      if (end > running_max) running_max = end;
      ix->maxend[i] = running_max;
    }
  }
  intnat j = 0;
  for (intnat b = 0; b <= nbuckets; b++) {
    uintnat bstart = lo + ((uintnat)b << shift);
    while (j + 1 < nsegs && segs[j + 1].start <= bstart) j++;
    buckets[b] = (uint32_t)j;
  }
  ix->lo = lo;
  ix->hi = hi;
  ix->bucket_shift = shift;
  ix->nbuckets = nbuckets;
  ix->buckets = buckets;
  return true;
}

static frame_descr *segment_index_lookup(const segment_index *ix, uintnat pc)
{
  if (ix->nsegs == 0 || pc < ix->lo) return NULL;
  const segment *segs = ix->segs;
  uintnat b = (pc - ix->lo) >> ix->bucket_shift;
  if (b > (uintnat)ix->nbuckets) b = ix->nbuckets;
  intnat i = ix->buckets[b];
  while (i + 1 < ix->nsegs && segs[i + 1].start <= pc) i++;
  if (segs[i].start > pc) return NULL;
  if (ix->maxend == NULL) return segment_scan(&segs[i], ix->side, pc);
  /* Segments from different tables interleave: keep going left while
     some earlier segment still ends at or after pc. */
  for (intnat j = i; j >= 0 && ix->maxend[j] >= pc; j--) {
    frame_descr *d = segment_scan(&segs[j], ix->side, pc);
    if (d != NULL) return d;
  }
  return NULL;
}

/* The index's memory footprint, for the Xmeasure_frametables report. */
static size_t segment_index_bytes(const segment_index *ix)
{
  if (ix->segs == NULL) return 0;
  return (size_t)ix->nsegs * sizeof(segment)
    + (size_t)ix->nside * sizeof(uint32_t)
    + (ix->maxend != NULL ? (size_t)ix->nsegs * sizeof(uintnat) : 0)
    + (size_t)(ix->nbuckets + 1) * sizeof(uint32_t);
}

/**** The registered frametables ****/

typedef struct extra_index extra_index;

struct caml_frame_descrs {
  int num_descr;      /* sum of the registered tables' count words */
  /* The fallback index storage, used when it is (re)built in a STW
     section: under Xframe_index_eager, and after a main table is
     unregistered. [fallback_index] points here in those cases; the
     lazily built fallback lives in its own mapping instead. */
  segment_index fallback;
  /* One eager index per late-registered table, sorted by lowest return
     address; extras_maxhi[i] is the highest [hi] among extras[0..i]
     (their address ranges may interleave). */
  extra_index *extras;
  intnat nextras, extras_cap;
  uintnat *extras_maxhi;
  /* The tables indexed as units: those registered by
     caml_init_frame_descriptors, plus the -manual-module-init tables of
     caml_unit_deps_table. Sorted by table address; main_removed[i] is
     set once main_tables[i] has been unregistered, which also sets
     [main_dirty] to force the fallback rebuild at the next
     registration. */
  table_ref *main_tables;
  intnat nmain_tables;
  unsigned char *main_removed;
  bool main_dirty;
  /* caml_unit_deps_table entries indexed at startup but not (yet)
     registered; the Xframe_index_check reference includes them. */
  caml_frametable_list *preindexed;
  caml_frametable_list *frametables;
  caml_frametable_list *zombies;
  caml_plat_mutex mutex;
};

/* Modified only at startup and in STW sections; [zombies] under
   [mutex] (see design comment section 4). */
static caml_frame_descrs current_frame_descrs = {
  .mutex = CAML_PLAT_MUTEX_INITIALIZER,
};

/* Bumped by every registration or reclamation that changes the indexes
   (all in STW sections; release so that the new tables are visible to
   any thread that loads it). A domain whose cache was filled under an
   older generation empties it (cache_sync_gen), so a cached descriptor
   pointer never outlives its frametable. Starts at 1: a freshly zeroed
   cache (generation 0) is always out of date. */
static atomic_uintnat index_generation = 1;

/* The position of [tbl] in [t->main_tables], or -1. */
static intnat main_table_index(caml_frame_descrs *t, intnat *tbl)
{
  intnat lo = 0, hi = t->nmain_tables;
  while (lo < hi) {
    intnat mid = lo + (hi - lo) / 2;
    if ((uintnat)t->main_tables[mid].tbl < (uintnat)tbl) lo = mid + 1;
    else hi = mid;
  }
  return (lo < t->nmain_tables && t->main_tables[lo].tbl == tbl) ? lo : -1;
}

static bool is_active_main(caml_frame_descrs *t, intnat *tbl)
{
  intnat mi = main_table_index(t, tbl);
  return mi >= 0 && !t->main_removed[mi];
}

/* The main tables not yet unregistered, as a fresh array the caller
   frees. */
static table_ref *active_tables(caml_frame_descrs *t, intnat *n_out)
{
  table_ref *tables = caml_stat_alloc(
    (size_t)(t->nmain_tables > 0 ? t->nmain_tables : 1) * sizeof(table_ref));
  intnat k = 0;
  for (intnat i = 0; i < t->nmain_tables; i++)
    if (!t->main_removed[i]) tables[k++] = t->main_tables[i];
  *n_out = k;
  return tables;
}

/**** Units: lazily indexed startup frametables ****/

/* A unit's index: the same segments and side entries as a
   segment_index, but bucketed by the shared [segment_buckets]. */
typedef struct {
  intnat nsegs;
  bool overlaps;  /* this unit's own segments overlap (the unit has
                     runs of code out of address order) */
  segment *segs;  /* sorted by [start] */
  uint32_t *side;
} unit_index;

typedef struct {
  uintnat first;  /* return address of the unit's first descriptor */
  table_ref table;
  intnat ndescr;  /* the count word: an upper bound for a range table */
  unit_index *_Atomic index; /* CAS-published by its builder, once */
  _Atomic int32_t builder;   /* pid of the claiming process, or 0 */
} unit;

static unit *units = NULL; /* sorted by [first]; fixed after units_init */
static intnat units_count = 0;
/* unit_buckets[b] is the index of the last unit whose first return
   address is at or before units_lo + (b << unit_bucket_shift). Filled
   at startup. */
static uint32_t *unit_buckets = NULL;
static intnat unit_buckets_count = 0;
static uintnat units_lo = 0;
static int unit_bucket_shift = BUCKET_SHIFT;
/* segment_buckets[b] is the index of the containing unit's last segment
   starting at or before units_lo + (b << BUCKET_SHIFT). Filled by each
   unit's builder as the unit is built, so resident only near built
   units. Duplicate builders (possible only after fork) store identical
   values, and readers race with them: relaxed atomics, and publication
   order comes from the index CAS. */
static _Atomic uint32_t *segment_buckets = NULL;
static intnat segment_buckets_count = 0;
/* The arena holding every unit index: reserved at startup at twice the
   worst case (every descriptor its own segment), so a resident page
   costs only a touched page. [arena_used] is the bump pointer; the
   allocation protocol is arena_alloc and the give-back CAS in
   unit_index_build. Exhaustion (which 2x makes unreachable in practice)
   only costs linear walks of unbuilt units. */
static char *arena = NULL;
static size_t arena_len = 0;
static _Atomic size_t arena_used = 0;
/* Non-zero while this thread builds an index. A lookup re-entered from
   a signal handler on this thread must not touch the half-made build or
   claim further builds; it walks the tables linearly instead. */
static CAMLthread_local int index_building = 0;

Caml_inline size_t arena_round(size_t bytes)
{
  return (bytes + ARENA_ALIGN - 1) & ~(size_t)(ARENA_ALIGN - 1);
}

/* Bump-allocate [bytes] from the arena; NULL when full. */
static void *arena_alloc(size_t bytes)
{
  bytes = arena_round(bytes);
  if (atomic_load_relaxed(&arena_used) + bytes > arena_len) return NULL;
  size_t off =
    atomic_fetch_add_explicit(&arena_used, bytes, memory_order_relaxed);
  if (off + bytes > arena_len) return NULL;
  return arena + off;
}

/* In-place heapsort of units by [first] (see segments_sort). */
static void units_sift_down(unit *a, intnat root, intnat n)
{
  while (2 * root + 1 < n) {
    intnat child = 2 * root + 1;
    if (child + 1 < n && a[child + 1].first > a[child].first) child++;
    if (a[root].first >= a[child].first) return;
    unit t = a[root]; a[root] = a[child]; a[child] = t;
    root = child;
  }
}

static void units_sort(unit *a, intnat n)
{
  for (intnat i = n / 2 - 1; i >= 0; i--) units_sift_down(a, i, n);
  for (intnat end = n - 1; end > 0; end--) {
    unit t = a[0]; a[0] = a[end]; a[end] = t;
    units_sift_down(a, 0, end);
  }
}

/* Record [tables] (an array of [n]) as units and build the unit
   buckets; decode no descriptor. Runs once, at the first registration.
   Raises Out_of_memory if a mapping fails, as the stock table fill
   did. */
static void units_init(const table_ref *tables, intnat n)
{
  units = index_map((size_t)(n > 0 ? n : 1) * sizeof(unit),
                    "frame_descr units");
  intnat k = 0;
  size_t reserve = 0;
  for (intnat i = 0; i < n; i++) {
    intnat nd = table_count(tables[i]);
    frame_descr *d = (frame_descr *)table_first(tables[i]);
    /* Skip empty tables, and ranges whose every piece the linker
       dropped. */
    if (nd <= 0 || (tables[i].end != NULL
                    && (const unsigned char *)d >= tables[i].end))
      continue;
    /* A frametable's first descriptor is always escaped, so this is an
       absolute address: no delta chain to walk. */
    CAMLassert(*d == FRAME_DELTA_ESCAPE);
    units[k].first = Retaddr_frame(d);
    units[k].table = tables[i];
    units[k].ndescr = nd;
    atomic_store_relaxed(&units[k].index, NULL);
    atomic_store_relaxed(&units[k].builder, 0);
    /* Worst case: every descriptor its own segment, plus alignment. */
    reserve += sizeof(unit_index)
      + (size_t)nd * (sizeof(segment) + sizeof(uint32_t)) + 4 * ARENA_ALIGN;
    k++;
  }
  units_count = k;
  units_sort(units, k);
  if (k == 0) return;
  units_lo = units[0].first;
  uintnat span = units[k - 1].first - units_lo;
  unit_bucket_shift = bucket_shift_for(span, k);
  unit_buckets_count = (intnat)(span >> unit_bucket_shift) + 1;
  unit_buckets =
    index_map((size_t)(unit_buckets_count + 1) * sizeof(uint32_t),
              "frame_descr unit buckets");
  intnat j = 0;
  for (intnat b = 0; b <= unit_buckets_count; b++) {
    uintnat bstart = units_lo + ((uintnat)b << unit_bucket_shift);
    while (j + 1 < k && units[j + 1].first <= bstart) j++;
    unit_buckets[b] = (uint32_t)j;
  }
  segment_buckets_count = (intnat)(span >> BUCKET_SHIFT)
    + SEGMENT_BUCKETS_SLACK;
  segment_buckets =
    index_map((size_t)segment_buckets_count * sizeof(uint32_t),
              "frame_descr segment buckets");
  arena_len = caml_mem_round_up_mapping_size(2 * reserve);
  arena = index_map(arena_len, "frame_descr index arena");
}

/* The unit whose assumed range contains [pc], or NULL if [pc] is below
   every unit. */
static unit *unit_find(uintnat pc)
{
  if (units_count == 0 || pc < units_lo) return NULL;
  uintnat b = (pc - units_lo) >> unit_bucket_shift;
  if (b > (uintnat)unit_buckets_count) b = unit_buckets_count;
  intnat i = unit_buckets[b];
  while (i + 1 < units_count && units[i + 1].first <= pc) i++;
  return units[i].first <= pc ? &units[i] : NULL;
}

/* Build [u]'s index in the arena and publish it. Returns the published
   index (ours, or exceptionally a winner's: see the final CAS), or NULL
   if the arena is exhausted. Neither allocates nor locks: only the
   arena and atomics, so it can run inside a GC stack scan or a signal
   handler. */
static unit_index *unit_index_build(unit *u)
{
  index_building++;
  table_ref tables[1] = { u->table };
  intnat nsegs = 0, nside = 0, ndescr = 0;
  uintnat lo, hi;
  size_t header = arena_round(sizeof(unit_index));
  char *mem;
  unit_index *ix = NULL;
  if (u->ndescr >= UNIT_ONE_PASS_MIN) {
    /* One decode walk into a worst-case-sized allocation: the side
       array has at most one entry per descriptor ([ndescr] is an upper
       bound for range tables), and the segments at most as many. The
       segments are then moved down to just after the side entries
       actually written, and the unused tail is handed back if no later
       allocation follows (if one does, the tail is left as a gap --
       untouched, hence not resident). */
    size_t side_bytes = arena_round((size_t)u->ndescr * sizeof(uint32_t));
    size_t worst = header + side_bytes + (size_t)u->ndescr * sizeof(segment);
    mem = arena_alloc(worst);
    if (mem != NULL) {
      ix = (unit_index *)mem;
      ix->side = (uint32_t *)(mem + header);
      ix->segs = (segment *)(mem + header + side_bytes);
      segments_cut(tables, 1, ix->segs, ix->side,
                   &nsegs, &nside, &ndescr, &lo, &hi);
      ix->nsegs = nsegs;
      size_t used_side = arena_round((size_t)nside * sizeof(uint32_t));
      if (used_side < side_bytes) {
        segment *dst = (segment *)(mem + header + used_side);
        memmove(dst, ix->segs, (size_t)nsegs * sizeof(segment));
        ix->segs = dst;
      }
      size_t bytes =
        arena_round(header + used_side + (size_t)nsegs * sizeof(segment));
      size_t off = (size_t)(mem - arena);
      size_t expected = off + arena_round(worst);
      atomic_compare_exchange_strong_explicit(
        &arena_used, &expected, off + bytes,
        memory_order_relaxed, memory_order_relaxed);
      /* Contiguous layouts come out already sorted. */
      if (!segments_sorted(ix->segs, nsegs)) segments_sort(ix->segs, nsegs);
    }
  } else {
    /* Exact two-pass build: count, allocate, fill. */
    segments_cut(tables, 1, NULL, NULL, &nsegs, &nside, &ndescr, &lo, &hi);
    size_t bytes = header + (size_t)nsegs * sizeof(segment)
      + (size_t)nside * sizeof(uint32_t);
    mem = arena_alloc(bytes);
    if (mem != NULL) {
      ix = (unit_index *)mem;
      ix->segs = (segment *)(mem + header);
      ix->side = (uint32_t *)(ix->segs + nsegs);
      ix->nsegs = nsegs;
      intnat nsegs2, nside2, ndescr2;
      segments_cut(tables, 1, ix->segs, ix->side,
                   &nsegs2, &nside2, &ndescr2, &lo, &hi);
      CAMLassert(nsegs2 == nsegs && nside2 == nside);
      if (!segments_sorted(ix->segs, nsegs)) segments_sort(ix->segs, nsegs);
    }
  }
  if (ix != NULL) {
    CAMLassert(nsegs > 0);
    uintnat running_max = 0;
    ix->overlaps = false;
    for (intnat i = 0; i < nsegs; i++) {
      uintnat end = segment_end(&ix->segs[i], ix->side);
      if (i > 0 && ix->segs[i].start <= running_max) ix->overlaps = true;
      if (end > running_max) running_max = end;
    }
    /* Fill the unit's segment buckets: those whose start address lies
       inside the unit's assumed range. The bucket holding [u->first]
       belongs to the previous unit (unit_index_lookup skips it). */
    uintnat uend = (u + 1 < units + units_count) ? (u + 1)->first
                                                 : running_max + 1;
    uintnat b0 = ((u->first - units_lo) >> BUCKET_SHIFT) + 1;
    uintnat b1 = (uend - 1 - units_lo) >> BUCKET_SHIFT;
    if (b1 >= (uintnat)segment_buckets_count)
      b1 = segment_buckets_count - 1;
    intnat j = 0;
    for (uintnat b = b0; b <= b1; b++) {
      uintnat bstart = units_lo + (b << BUCKET_SHIFT);
      while (j + 1 < nsegs && ix->segs[j + 1].start <= bstart) j++;
      atomic_store_relaxed(&segment_buckets[b], (uint32_t)j);
    }
    /* Publish: release pairs with the acquire loads of [u->index].
       Losing is only possible against a pre-fork parent's build; adopt
       the winner and leave ours as a gap in the arena. */
    unit_index *expected = NULL;
    if (!atomic_compare_exchange_strong_explicit(
          &u->index, &expected, ix,
          memory_order_release, memory_order_acquire))
      ix = expected;
  }
  index_building--;
  return ix;
}

/* Whether this thread now owns building [u]. Claims are per-pid:
   another thread of this process never steals a claim, but a child
   process takes over a claim inherited across fork() (the builder
   died in the fork; nothing else would ever build the unit). Relaxed:
   the claim only serialises builders, the data is published by the
   index CAS. */
static bool unit_claim(unit *u)
{
  int32_t self = (int32_t)getpid();
  int32_t cur = atomic_load_relaxed(&u->builder);
  return cur != self
    && atomic_compare_exchange_strong_explicit(
         &u->builder, &cur, self,
         memory_order_relaxed, memory_order_relaxed);
}

/* [u]'s index, building it if this thread can claim that. NULL if
   another thread holds the claim and has not yet published, or the
   arena is exhausted: the caller walks the unit linearly. */
static unit_index *unit_index_get(unit *u)
{
  if (unit_claim(u)) return unit_index_build(u);
  return atomic_load_acquire(&u->index);
}

/* Linear decode walk of one unit, or of all units ([u] == NULL): the
   build-free, allocation-free fallback of last resort. */
static frame_descr *units_walk(unit *u, uintnat pc)
{
  intnat from = u != NULL ? u - units : 0;
  intnat to = u != NULL ? from + 1 : units_count;
  for (intnat i = from; i < to; i++) {
    frametable_iter it;
    table_iter_start(&it, units[i].table);
    while (frametable_iter_more(&it)) {
      uintnat ret;
      frame_descr *d = frametable_iter_next(&it, &ret);
      if (ret == pc) return d;
    }
  }
  return NULL;
}

/* Look [pc] up in [u]'s built index [ix]. */
static frame_descr *unit_index_lookup(const unit_index *ix, const unit *u,
                                      uintnat pc)
{
  CAMLassert(ix->nsegs > 0);
  intnat i = 0;
  uintnat b = (pc - units_lo) >> BUCKET_SHIFT;
  if (b < (uintnat)segment_buckets_count
      && b != ((u->first - units_lo) >> BUCKET_SHIFT))
    i = atomic_load_relaxed(&segment_buckets[b]);
  while (i + 1 < ix->nsegs && ix->segs[i + 1].start <= pc) i++;
  if (ix->segs[i].start > pc) return NULL;
  frame_descr *d = segment_scan(&ix->segs[i], ix->side, pc);
  if (d != NULL || !ix->overlaps) return d;
  /* The unit's runs are out of address order: scan earlier segments. */
  for (intnat j = i - 1; j >= 0; j--) {
    d = segment_scan(&ix->segs[j], ix->side, pc);
    if (d != NULL) return d;
  }
  return NULL;
}

/**** The fallback index ****/

/* The index over every unit (design comment section 3). NULL until the
   first lookup that the per-unit indexes cannot answer builds it
   (fallback_get, CAS-published); eager indexing and unregistration
   rebuilds instead point it at current_frame_descrs.fallback, from a
   STW section. */
static segment_index *_Atomic fallback_index = NULL;

/* The fallback index, built if needed. On the lookup path, so nothing
   here raises, allocates or locks: NULL if a mapping fails, and the
   caller walks the tables. */
static segment_index *fallback_get(void)
{
  segment_index *g = atomic_load_acquire(&fallback_index);
  if (g != NULL) return g;
  index_building++;
  size_t tables_len =
    (size_t)(units_count > 0 ? units_count : 1) * sizeof(table_ref);
  segment_index *ix =
    index_map_noexc(sizeof(segment_index), "frame_descr fallback");
  table_ref *tables = index_map_noexc(tables_len, "frame_descr fallback");
  bool ok = ix != NULL && tables != NULL;
  if (ok) {
    for (intnat i = 0; i < units_count; i++) tables[i] = units[i].table;
    ok = segment_index_build(ix, tables, units_count, true);
  }
  index_unmap(tables, tables_len);
  if (!ok) {
    index_unmap(ix, sizeof(segment_index));
    index_building--;
    return NULL;
  }
  segment_index *expected = NULL;
  if (atomic_compare_exchange_strong_explicit(
        &fallback_index, &expected, ix,
        memory_order_release, memory_order_acquire)) {
    g = ix;
  } else {
    /* A racing build won: use it, free ours. */
    segment_index_free(ix);
    index_unmap(ix, sizeof(segment_index));
    g = expected;
  }
  index_building--;
  return g;
}

/* Look [pc] up through the per-unit indexes. Per the fallback rule, a
   miss is never final: it is answered by the fallback index (or, while
   re-entered from a signal handler or out of arena, by linear walks). */
static frame_descr *units_lookup(uintnat pc)
{
  segment_index *g = atomic_load_acquire(&fallback_index);
  if (g != NULL) return segment_index_lookup(g, pc);
  unit *u = unit_find(pc);
  if (u != NULL) {
    unit_index *ix = atomic_load_acquire(&u->index);
    if (ix == NULL) {
      if (index_building) {
        /* Re-entered from a signal handler during this thread's own
           build: leave the arena alone. */
        frame_descr *d = units_walk(u, pc);
        return d != NULL ? d : units_walk(NULL, pc);
      }
      ix = unit_index_get(u);
    }
    frame_descr *d = ix != NULL ? unit_index_lookup(ix, u, pc)
                                : units_walk(u, pc);
    if (d != NULL) return d;
  }
  if (index_building) return units_walk(NULL, pc);
  segment_index *fallback = fallback_get();
  return fallback != NULL ? segment_index_lookup(fallback, pc)
                          : units_walk(NULL, pc);
}

/**** Prewarming (Xframe_index_prewarm) ****/

#define PREWARM_STACK_BYTES (256 * 1024)

static void *prewarm_thread(void *unused)
{
  (void)unused;
#ifdef __linux__
  /* Lowest priority: prewarming must not compete with real work. */
  setpriority(PRIO_PROCESS, (id_t)syscall(SYS_gettid), 19);
#endif
  for (intnat i = 0; i < units_count; i++) {
    if (atomic_load_acquire(&fallback_index) != NULL) break;
    if (atomic_load_acquire(&units[i].index) == NULL
        && unit_claim(&units[i]))
      unit_index_build(&units[i]);
  }
  return NULL;
}

static void prewarm_start(void)
{
  pthread_attr_t attr;
  pthread_t th;
  int err = pthread_attr_init(&attr);
  if (err == 0) {
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    /* A modest stack: this thread only decodes frametables. (Not too
       small: glibc carves each thread's static TLS out of its stack.) */
    pthread_attr_setstacksize(&attr, PREWARM_STACK_BYTES);
    err = pthread_create(&th, &attr, prewarm_thread, NULL);
    pthread_attr_destroy(&attr);
  }
  if (err != 0)
    fprintf(stderr, "[ocaml] Xframe_index_prewarm: cannot start thread: %s\n",
            strerror(err));
}

/**** Extra indexes: late-registered frametables ****/

/* One eager index per table registered after startup (natdynlink, a
   JIT, caml_copy_and_register_*). A registration indexes only its new
   tables, so registering n tables one at a time costs O(their
   descriptors) plus a sorted-array insertion, not a rebuild over all
   extras. Modified only in STW registration. */
struct extra_index {
  uintnat lo, hi; /* lowest and highest return address in the table */
  intnat *tbl;    /* identity: the registered table pointer */
  segment_index ix;
};

static intnat extras_find(caml_frame_descrs *t, intnat *tbl)
{
  for (intnat i = 0; i < t->nextras; i++)
    if (t->extras[i].tbl == tbl) return i;
  return -1;
}

static void extras_fix_maxhi(caml_frame_descrs *t, intnat from)
{
  for (intnat i = from; i < t->nextras; i++) {
    uintnat prev = i > 0 ? t->extras_maxhi[i - 1] : 0;
    t->extras_maxhi[i] = t->extras[i].hi > prev ? t->extras[i].hi : prev;
  }
}

static void extras_add(caml_frame_descrs *t, table_ref tab)
{
  if (extras_find(t, tab.tbl) >= 0) return; /* registered twice */
  extra_index x;
  memset(&x, 0, sizeof(x));
  x.tbl = tab.tbl;
  /* not noexc: raises on failure rather than returning false */
  (void)segment_index_build(&x.ix, &tab, 1, false);
  if (x.ix.nsegs == 0) { segment_index_free(&x.ix); return; }
  x.lo = x.ix.lo;
  x.hi = x.ix.hi;
  if (t->nextras == t->extras_cap) {
    intnat cap = t->extras_cap > 0 ? 2 * t->extras_cap : 16;
    extra_index *e =
      caml_stat_resize_noexc(t->extras, (size_t)cap * sizeof(extra_index));
    if (e == NULL) caml_raise_out_of_memory();
    t->extras = e;
    uintnat *m =
      caml_stat_resize_noexc(t->extras_maxhi, (size_t)cap * sizeof(uintnat));
    if (m == NULL) caml_raise_out_of_memory();
    t->extras_maxhi = m;
    t->extras_cap = cap;
  }
  intnat pos = t->nextras;
  while (pos > 0 && t->extras[pos - 1].lo > x.lo) pos--;
  memmove(&t->extras[pos + 1], &t->extras[pos],
          (size_t)(t->nextras - pos) * sizeof(extra_index));
  t->extras[pos] = x;
  t->nextras++;
  extras_fix_maxhi(t, pos);
}

/* Returns whether [tbl] had an extra index. */
static bool extras_remove(caml_frame_descrs *t, intnat *tbl)
{
  intnat i = extras_find(t, tbl);
  if (i < 0) return false;
  segment_index_free(&t->extras[i].ix);
  memmove(&t->extras[i], &t->extras[i + 1],
          (size_t)(t->nextras - i - 1) * sizeof(extra_index));
  t->nextras--;
  extras_fix_maxhi(t, i);
  return true;
}

static frame_descr *extras_lookup(caml_frame_descrs *t, uintnat pc)
{
  intnat lo = 0, hi = t->nextras;
  while (lo < hi) { /* first extra whose lo > pc */
    intnat mid = lo + (hi - lo) / 2;
    if (t->extras[mid].lo <= pc) lo = mid + 1; else hi = mid;
  }
  for (intnat i = lo - 1; i >= 0 && t->extras_maxhi[i] >= pc; i--) {
    if (pc > t->extras[i].hi) continue;
    frame_descr *d = segment_index_lookup(&t->extras[i].ix, pc);
    if (d != NULL) return d;
  }
  return NULL;
}

/**** Lookup and the per-domain cache ****/

/* Index lookup, with no cache in front. */
static frame_descr *lookup_nocache(caml_frame_descrs *fds, uintnat pc)
{
  /* Late-registered tables are indexed eagerly: look there first, so
     that a pc in one never triggers the fallback build. */
  if (fds->nextras > 0) {
    frame_descr *d = extras_lookup(fds, pc);
    if (d != NULL) return d;
  }
  return units_lookup(pc);
}

/* Cache geometry: direct-mapped, 2^CACHE_BITS 16-byte entries (64 KiB
   per domain, resident only as touched). On a mixed GC + raise
   workload, 4096 entries hit ~95% and 2048 ~88%; 2-way associativity
   recovered too few misses to pay for its longer hit path. */
#define CACHE_BITS 12

typedef struct {
  /* Written only by cache_write below. 0 means empty or mid-write. */
  _Atomic uintnat retaddr;
  frame_descr *_Atomic fd;
} cache_entry;

/* A domain's cache is written only under its domain lock, and read
   under it or from a signal handler on the same thread; the fences in
   cache_write and caml_find_frame_descr make that last case safe. */
struct caml_frame_descr_cache {
  uintnat generation; /* the index_generation the entries belong to */
  cache_entry entries[(uintnat)1 << CACHE_BITS];
};

struct caml_frame_descr_cache *caml_frame_descr_cache_create(void)
{
  /* Zeroed: every entry empty, generation 0 (always stale). NULL is
     allowed; that domain's lookups are simply uncached. */
  return caml_stat_calloc_noexc(1, sizeof(struct caml_frame_descr_cache));
}

Caml_inline struct caml_frame_descr_cache *domain_cache(void)
{
  caml_domain_state *st = Caml_state_opt;
  return st != NULL ? st->frame_descr_cache : NULL;
}

/* Slot hash: xorshift, Fibonacci multiply, top bits. The return
   addresses of evenly spaced functions collide badly under a plain
   multiply (and worse under Hash_retaddr's low product bits); mixing
   in pc >> 15 first took the hit rate on a mixed GC + raise workload
   from 18% to over 90%. */
Caml_inline uintnat cache_slot(uintnat pc)
{
  uint64_t h = ((uint64_t)pc ^ ((uint64_t)pc >> 15))
    * UINT64_C(0x9E3779B97F4A7C15);
  return (uintnat)(h >> (64 - CACHE_BITS));
}

/* Write [e] so that a signal handler interrupting us never sees a torn
   entry: empty it, set [fd], then set [retaddr], with compiler fences
   keeping the order. (No handler looks descriptors up today; this
   keeps the cache safe for ones that will.) */
Caml_inline void cache_write(cache_entry *e, uintnat pc, frame_descr *d)
{
  atomic_store_relaxed(&e->retaddr, 0);
  atomic_signal_fence(memory_order_seq_cst);
  atomic_store_relaxed(&e->fd, d);
  atomic_signal_fence(memory_order_seq_cst);
  atomic_store_relaxed(&e->retaddr, pc);
}

/* Make [c] valid for generation [gen], emptying it if it was filled
   under an older one. A signal handler arriving mid-memset re-enters
   here, clears the cache again and completes its own lookup before the
   interrupted memset resumes: it can erase a fresh entry, never see a
   torn one (its clear precedes its reads). */
Caml_inline void cache_sync_gen(struct caml_frame_descr_cache *c,
                                uintnat gen)
{
  if (c->generation != gen) {
    /* A fresh cache is already zeroed: only clear used ones. */
    if (c->generation != 0) memset(c->entries, 0, sizeof(c->entries));
    c->generation = gen;
  }
}

/* Cache misses (and generation changes) leave the hot path here, so
   that caml_find_frame_descr needs no callee-saved registers. */
static Caml_noinline frame_descr *cache_miss(caml_frame_descrs *fds,
                                             uintnat pc,
                                             struct caml_frame_descr_cache *c,
                                             cache_entry *e,
                                             uintnat gen)
{
  cache_sync_gen(c, gen);
  frame_descr *d = lookup_nocache(fds, pc);
  if (d != NULL && pc != 0) cache_write(e, pc, d);
  return d;
}

/* The hot path: the GC and backtrace walkers come through here for
   every frame of every stack they scan. */
frame_descr *caml_find_frame_descr(caml_frame_descrs *fds, uintnat pc)
{
  struct caml_frame_descr_cache *c = domain_cache();
  if (CAMLunlikely(c == NULL)) return lookup_nocache(fds, pc);
  uintnat gen = atomic_load_acquire(&index_generation);
  cache_entry *e = &c->entries[cache_slot(pc)];
  /* Load [fd] before [retaddr] -- the reverse of cache_write -- so if a
     same-thread signal handler refilled the entry in between, [retaddr]
     no longer matches [pc]. [pc] == 0 could match a mid-write entry,
     and a stale generation could match a freed descriptor: both take
     the miss path. */
  frame_descr *d = atomic_load_relaxed(&e->fd);
  atomic_signal_fence(memory_order_seq_cst);
  if (CAMLlikely(atomic_load_relaxed(&e->retaddr) == pc
                 && c->generation == gen && pc != 0))
    return d;
  return cache_miss(fds, pc, c, e, gen);
}

/**** Frametable measurement ****/

/* As preparation for designing a more memory-efficient frametable
 * format, this code measures various things about the frametables of
 * an executable. It decodes both short and escaped ("medium"/"long")
 * descriptors compatibly, via caml_decode_frame_descr.
 *
 * Settable via GC tweak OCAMLRUNPARAM=Xmeasure_frametables (the
 * frametables registered in this batch). */

#define MAX_LOG 32
#define SMALL_FRAMES 32
#define REGS 64
#define MAX_REG (REGS - 1) /* Largest register index recorded in reg tables */
#define MAX_REG_IN_SMALL_MAP 12

/* Why an escaped descriptor could not use the short format. [ESC_FITS] means
   its content is short-compatible, so it escaped only for a positional reason
   (a function / text-section boundary). Values are declared in reporting
   order: cases we expect on real binaries first (cold register last of
   those), then cases we expect never to see. */
enum escape_reason {
  ESC_BADSIZE,      /* frame size 0, > 1008, or not a multiple of 16 */
  ESC_BIGALLOC,     /* an allocation size that does not fit a 4-bit nibble */
  ESC_FT_FIRST,     /* first in its frametable: always escapes; positional,
                       so never returned by classify_escape */
  ESC_FITS,         /* short-compatible: escaped only for a boundary */
  ESC_COLDREG,      /* a live register outside the 8 hot registers */
  ESC_LONG,         /* long (32-bit) descriptor */
  ESC_BADSLOT,      /* a live stack slot is not word-aligned */
  ESC_SLOTBEYOND,   /* a live stack slot outside the frame (stack argument) */
  ESC_NOALLOC_REGS, /* non-allocation descriptor with live registers */
  ESC_MANYALLOCS,   /* more than 255 allocations (comballoc) */
  NUM_ESCAPE_REASONS
};

static const char *const escape_names[NUM_ESCAPE_REASONS] = {
  [ESC_BADSIZE]      = "bad frame size",
  [ESC_BIGALLOC]     = "alloc too large",
  [ESC_FT_FIRST]     = "first in frametable",
  [ESC_FITS]         = "fits (section boundary)",
  [ESC_COLDREG]      = "cold register",
  [ESC_LONG]         = "long descriptor",
  [ESC_BADSLOT]      = "unaligned stack slot",
  [ESC_SLOTBEYOND]   = "stack slot outside frame",
  [ESC_NOALLOC_REGS] = "non-alloc with live registers",
  [ESC_MANYALLOCS]   = "> 255 allocations",
};

struct frametable_stats {
  /* A few per-frametable items */
  unsigned char *last_retaddr;
  unsigned char *min_retaddr;
  unsigned char *max_retaddr;
  unsigned char *min_descr;
  unsigned char *max_descr;
  size_t descrs;

  /* Everything else is accumulated over all frametables */
  size_t frametables;
  size_t total_codesize;
  size_t total_ft_size;
  size_t total_debuginfo_size;
  size_t total_descrs;
  size_t total_debuginfo;

  /* Are frametables in memory before or after the code they describe? */
  size_t ft_before_code;
  size_t ft_after_code;

  /* Descriptor kinds */
  size_t return_to_C;
  size_t with_debug;
  size_t with_alloc;
  size_t short_descrs;
  size_t medium_descrs;
  size_t long_descrs;

  /* Why escaped (non-short) descriptors could not use the short format,
     indexed by enum escape_reason. [ESC_FITS] entries are short-compatible
     and so escaped only for a function / text-section boundary -- the
     descriptors a cross-function delta chain could reclaim. */
  size_t escape_reason[NUM_ESCAPE_REASONS];

  /* Frame sizes */
  size_t small_frames[SMALL_FRAMES];
  size_t log_framesize[MAX_LOG];
  size_t max_framesize;

  /* Relative return addresses */
  size_t log_pos_retaddr_rel[MAX_LOG];
  size_t max_pos_retaddr_rel;
  size_t log_neg_retaddr_rel[MAX_LOG];
  size_t max_neg_retaddr_rel;

  /* Delta from one retaddr to the next */
  size_t log_pos_delta[MAX_LOG];
  size_t max_pos_delta;
  size_t log_neg_delta[MAX_LOG];
  size_t max_neg_delta;

  /* Counts of "live" values (GC regs + slots) */
  size_t small_lives[SMALL_FRAMES];
  size_t log_lives[MAX_LOG];
  size_t max_lives; /* Overall maximum count of lives */

  /* GCable registers */
  size_t reg[REGS]; /* # descriptors using each register */
  size_t reg_count[REGS]; /* Count GCable registers */
  size_t max_regs;
  size_t max_reg[REGS];  /* Count max GCable register index */
  size_t big_reg_descrs; /* Descrs with a register > MAX_REG_IN_SMALL_MAP */
  size_t big_reg_entries; /* live register entries with number > MAX_REG */
  size_t noalloc_with_regs; /* Non-alloc descrs that have registers (anomaly) */

  /* Count of GCable stack slots */
  size_t small_slots[SMALL_FRAMES];
  size_t log_slots[MAX_LOG];
  size_t max_slots;

  /* maximum GCable stack slot offset */
  size_t small_max_slot[SMALL_FRAMES];
  size_t log_max_slot[MAX_LOG];
  size_t max_slot;

  /* Comballoc allocation counts */
  size_t log_comballocs[MAX_LOG];
  size_t small_comballocs[SMALL_FRAMES];
  size_t max_comballocs;

  /* allocation sizes */
  size_t alloc_sizes;
  size_t log_alloc_sizes[MAX_LOG];
  size_t small_alloc_sizes[SMALL_FRAMES];
  size_t max_alloc_size;
};

static void clear_stats(struct frametable_stats *stats)
{
  caml_debuginfo_reset();
  memset(stats, 0, sizeof(*stats));
}

static void clear_per_frametable_stats(struct frametable_stats *stats)
{
  stats->min_retaddr = stats->max_retaddr = stats->last_retaddr =
    stats->min_descr = stats->max_descr = NULL;
  stats->descrs = 0;
  caml_debuginfo_reset();
}

/* Turn `val` into a string representing that number of bytes */

static void report_bytes(char *buf, size_t space, size_t val)
{
  if (val < 1024) {
    snprintf(buf, space, "%zu", val);
  } else {
    char suffix[] = " kMGTE";
    double scaled = val;
    char *p = suffix;
    while(scaled > 1000 && *p) {
      scaled /= 1024.0;
      ++p;
    }
    snprintf(buf, space, "%.2f %ciB", scaled, *p);
  }
}

/* Write text showing an array `vals` of `count` size_t values,
 * without trailing zero values, into `buf` (size `space` bytes). */

/* This works for regs and logs and smalls */
#define TABLE_BUF_SIZE (REGS * 16)

static void report_table(char *fmt, size_t *vals, size_t count)
{
  char buf[TABLE_BUF_SIZE];
  size_t space = TABLE_BUF_SIZE;

  size_t max = count - 1;
  while(max && vals[max] == 0) {
    --max;
  }
  char *p = buf;
  for (size_t i = 0; i <= max; ++i) {
    int len = snprintf(p, space, "%zu ", vals[i]);
    if (len > space) { /* truncated, replace with ... */
      p[space-4] = p[space-3] = p[space-2] = '.';
      p[space-1] = '\0';
      return;
    }
    space -= len;
    p += len;
  }
  printf(fmt, buf);
}

/* At the end of a single frametable, deduce per-frametable sizes and
 * add them to global stats. */

static void accumulate_frametable_stats(intnat *frametable,
                                        struct frametable_stats *stats)
{
  char *debuginfo_low, *debuginfo_high;
  size_t debuginfo_count;
  caml_debuginfo_measurements(&debuginfo_count,
                              &debuginfo_low,
                              &debuginfo_high);
  /* round debuginfo_high up to word boundary */
  debuginfo_high = Align_to(debuginfo_high, uintnat);
  stats->total_codesize += (stats->max_retaddr - stats->min_retaddr);
  stats->total_ft_size += (stats->max_descr - stats->min_descr);
  /* Debuginfo bytes = the span bracketing the record and jump words and the
     name_info/name_and_loc_info structs (counting record words would multiply
     out suffix sharing). The deduped filename/defname strings live in a
     separate section and are not attributed to a frametable here. */
  stats->total_debuginfo_size += (debuginfo_high - debuginfo_low);
  stats->total_descrs += stats->descrs;
  stats->total_debuginfo += debuginfo_count;
  if (stats->min_retaddr) {
    if (stats->min_retaddr > stats->min_descr)
      ++ stats->ft_before_code;
    else
      ++ stats->ft_after_code;
  }
}

/* Is [reg] one of the eight hot registers the short format can encode? */

static bool reg_is_hot(size_t reg)
{
  for (int i = 0; i < FRAME_NUM_HOT_REGS; ++i) {
    if (caml_frame_hot_regs[i] == reg) {
      return true;
    }
  }
  return false;
}

/* Report all accumulated frametable stats to stdout. */

static void report_stats(struct frametable_stats *stats)
{
  char table_buf[TABLE_BUF_SIZE];

  size_t total_size =
    stats->total_codesize
    + stats->total_ft_size
    + stats->total_debuginfo_size;
  printf("Summary of %zu frametables.\n", stats->frametables);
  report_bytes(table_buf, TABLE_BUF_SIZE, stats->total_codesize);
  printf("%s code (%5.2f%%)\n", table_buf,
         (double)stats->total_codesize/total_size * 100.0);
  report_bytes(table_buf, TABLE_BUF_SIZE, stats->total_ft_size);
  printf("%s descriptors (%5.2f%%; %zu descrs, %f bytes each)\n",
         table_buf,
         (double)stats->total_ft_size/total_size * 100.0,
         stats->total_descrs,
         (double)stats->total_ft_size / stats->total_descrs);
  report_bytes(table_buf, TABLE_BUF_SIZE, stats->total_debuginfo_size);
  printf("%s Debuginfo (%5.2f%% %zu entries)\n",
         table_buf,
         (double)stats->total_debuginfo_size/total_size * 100.0,
         stats->total_debuginfo);
  printf("%zu frametables before code, %zu after code.\n",
         stats->ft_before_code, stats->ft_after_code);
  printf("short %zu medium %zu long %zu return to C %zu alloc %zu debug %zu\n",
         stats->short_descrs, stats->medium_descrs, stats->long_descrs,
         stats->return_to_C, stats->with_alloc, stats->with_debug);

  {
    size_t total_escapes = 0;
    for (int i = 0; i < NUM_ESCAPE_REASONS; ++i) {
      total_escapes += stats->escape_reason[i];
    }
    printf("Escaped descriptors\n");
    for (int i = 0; i < NUM_ESCAPE_REASONS; ++i) {
      printf("  %-33s %9zu (%4.1f%%)\n", escape_names[i],
             stats->escape_reason[i],
             total_escapes
               ? 100.0 * stats->escape_reason[i] / total_escapes : 0.0);
    }
    printf("%35s %9zu (%.1f%%)\n", "Total:", total_escapes,
           stats->total_descrs
             ? 100.0 * total_escapes / stats->total_descrs : 0.0);
  }

  printf("Max pos retaddr offset %zu\n", stats->max_pos_retaddr_rel);
  report_table("  (logs %s)\n", stats->log_pos_retaddr_rel, MAX_LOG);
  printf("Max neg retaddr offset %zu\n", stats->max_neg_retaddr_rel);
  report_table("  (logs %s)\n", stats->log_neg_retaddr_rel, MAX_LOG);
  printf("Max retaddr pos delta %zu\n", stats->max_pos_delta);
  report_table("  (logs %s)\n", stats->log_pos_delta, MAX_LOG);
  printf("Max retaddr neg delta %zu\n", stats->max_neg_delta);
  report_table("  (logs %s)\n", stats->log_neg_delta, MAX_LOG);

  printf("Frame sizes (max %zu)\n", stats->max_framesize);
  report_table("  (small %s)\n", stats->small_frames, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_framesize, MAX_LOG);

  printf("Comballoc entries (max %zu)\n",
         stats->max_comballocs);
  report_table("  (small %s)\n", stats->small_comballocs, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_comballocs, MAX_LOG);

  printf("Allocation sizes (wosize-1) (%zu allocs, max %zu)\n",
         stats->alloc_sizes, stats->max_alloc_size);
  report_table("  (small %s)\n", stats->small_alloc_sizes, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_alloc_sizes, MAX_LOG);

  printf("GC live values (max %zu)\n", stats->max_lives);
  report_table("  (small %s)\n", stats->small_lives, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_lives, MAX_LOG);

  printf("GCable stack slots (max %zu)\n", stats->max_slots);
  report_table("  (small %s)\n", stats->small_slots, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_slots, MAX_LOG);

  printf("Max GCable stack slot (max %zu)\n", stats->max_slot);
  report_table("  (small %s)\n", stats->small_max_slot, SMALL_FRAMES);
  report_table("  (logs %s)\n", stats->log_max_slot, MAX_LOG);

  printf("GCable registers (max %zu)\n", stats->max_regs);
  report_table("  (counts %s)\n", stats->reg_count, REGS);
  report_table("  (each reg %s)\n", stats->reg, REGS);
  report_table("  (max %s)\n", stats->max_reg, REGS);

  printf("Descriptors with a register > %d: %zu\n",
         MAX_REG_IN_SMALL_MAP, stats->big_reg_descrs);
  printf("live register entries with number > %d: %zu\n",
         MAX_REG, stats->big_reg_entries);
  printf("Non-allocation descriptors with registers: %zu\n",
         stats->noalloc_with_regs);

  {
    /* The FRAME_NUM_HOT_REGS most-used registers (ties kept in the current
       hot set), as lines to paste into runtime/caml/frame_descriptors.h and
       backend/<arch>/arch.ml -- or a note that no change is warranted. */
    bool chosen[REGS] = { false };
    bool same = true;
    const char *sep;
    for (int k = 0; k < FRAME_NUM_HOT_REGS; ++k) {
      int best = -1;
      for (int r = 0; r < REGS; ++r) {
        if (chosen[r]) continue;
        if (best < 0 || stats->reg[r] > stats->reg[best]
            || (stats->reg[r] == stats->reg[best]
                && reg_is_hot(r) && !reg_is_hot(best)))
          best = r;
      }
      chosen[best] = true;
    }
    for (int r = 0; r < REGS; ++r) {
      if (chosen[r] != reg_is_hot(r)) { same = false; break; }
    }
    if (same) {
      printf("Existing hot register set is optimal.\n");
    } else {
      printf("Hot registers (frame_descriptors.h): {");
      sep = " ";
      for (int r = 0; r < REGS; ++r) {
        if (chosen[r]) { printf("%s%d", sep, r); sep = ", "; }
      }
      printf(" };\n");
      printf("Hot registers (arch.ml): let frame_hot_regs = [|");
      sep = " ";
      for (int r = 0; r < REGS; ++r) {
        if (chosen[r]) { printf("%s%d", sep, r); sep = "; "; }
      }
      printf(" |]\n");
    }
  }
}

/* Actually floor(log_2(x))+1, clamped at MAX_LOG-1.
   mylog(0) = 0
   mylog(1) = 1
   mylog(2) = 2
   mylog(3) = 2
   mylog(4) = 3
   ...
   mylog(255) = 8
   mylog(256) = 9
   etc
  */
Caml_inline size_t mylog(size_t v)
{
  size_t log = 0;
  while(v) {
    v /= 2;
    ++ log;
  }
  /* Clamp so the result is always a valid index into a MAX_LOG table */
  if (log >= MAX_LOG) {
    log = MAX_LOG - 1;
  }
  return log;
}

/* Common code for recording an item in:
   - an optional max value;
   - an optional table of small values (less than SMALL_FRAMES)
   - an optional table of log values.
 */

Caml_inline void count_item(size_t item, size_t *max, size_t *smalls,
                            size_t *logs)
{
  if (max && item > *max) {
    *max = item;
  }
  if (smalls && item < SMALL_FRAMES) {
    ++ smalls[item];
  }
  if (logs) {
    ++ logs[mylog(item)];
  }
}

/* Record a single GCable register: register numbers are normally small
   (we have never observed one larger than MAX_REG_IN_SMALL_MAP), but the
   format permits large ones (e.g. Valx2 values held in SIMD registers),
   so we guard the fixed-size tables and count the outliers separately. */

static void add_reg(size_t reg, size_t *regs, size_t *max_reg,
                    bool *has_big_reg, struct frametable_stats *stats)
{
  ++ *regs;
  if (reg < REGS) {
    ++ stats->reg[reg];
  }
  if (reg > *max_reg) {
    *max_reg = reg;
  }
  if (reg > MAX_REG_IN_SMALL_MAP) {
    *has_big_reg = true;
  }
  if (reg > MAX_REG) {
    ++ stats->big_reg_entries;
  }
}

/* Record a single GCable stack slot, given its byte offset. */

static void add_slot(size_t byte_ofs, size_t *slots, size_t *max_slot)
{
  size_t slot = byte_ofs / sizeof(value);
  ++ *slots;
  if (slot > *max_slot) {
    *max_slot = slot;
  }
}

/* For an escaped (non-short, non-return-to-C, non-first-in-frametable)
   descriptor, determine why it could not be encoded in the short format,
   mirroring [short_encoding] in backend/emitaux.ml (same checks, same
   order). Returns [ESC_FITS] if the content is short-compatible, meaning
   the descriptor escaped only for a function/text-section boundary. */

static enum escape_reason classify_escape(frame_descr *d,
                                          struct frame_descr_decoded *dec)
{
  if (dec->is_long) return ESC_LONG;
  uint32_t size = dec->frame_size; /* in bytes, flag bits already masked off */
  if (size == 0 || size > 1008 || (size & 15) != 0) return ESC_BADSIZE;

  size_t nregs = 0;
  size_t frame_words = size / sizeof(value);
  bool cold_reg = false;
  const uint16_t *ofp = (const uint16_t *)(d + Frame_live_ofs);
  for (uint32_t n = dec->num_live; n > 0; n--, ofp++) {
    uint16_t v = caml_read_unaligned_uint16(ofp);
    if (v & 1) {
      ++ nregs;
      if (!reg_is_hot(v >> 1)) cold_reg = true;
    } else {
      if ((v & (sizeof(value) - 1)) != 0) return ESC_BADSLOT; /* unaligned */
      if ((size_t)(v / sizeof(value)) >= frame_words) return ESC_SLOTBEYOND;
    }
  }
  if (!dec->has_allocs && nregs > 0) return ESC_NOALLOC_REGS;
  if (cold_reg) return ESC_COLDREG;

  if (dec->has_allocs) {
    size_t num_allocs = dec->num_allocs;
    if (num_allocs > 255) return ESC_MANYALLOCS;
    /* Escaped alloc sizes are one byte each (same wosize-1 value the short
       format would store in a nibble), just past the num_allocs byte. */
    const unsigned char *sizes = dec->end_of_live + 1;
    for (size_t i = 0; i < num_allocs; ++i) {
      if (sizes[i] > 15) return ESC_BIGALLOC;
    }
  }
  return ESC_FITS;
}

/* Record stats for a single descriptor, given its decoded form, its
   descriptor body pointer, and its absolute return address (as
   reconstructed by the frametable iterator). */

static void add_descriptor_to_stats(frame_descr *d,
                                    struct frame_descr_decoded *dec,
                                    uintnat retaddr,
                                    struct frametable_stats *stats)
{
  bool first_in_ft = (stats->descrs == 0);
  ++ stats->descrs;
  if (!stats->min_descr || ((unsigned char*)d < stats->min_descr)) {
    stats->min_descr = (unsigned char*)d;
  }

  /* Relative return address: the (signed) byte offset from the
     descriptor to its return address. For short descriptors this is
     implicit (encoded as a delta); we compute it from the
     reconstructed absolute address. */
  intnat retaddr_rel = (intnat)((uintnat)retaddr - (uintnat)d);
  if (retaddr_rel < 0) {
    count_item(-retaddr_rel, &stats->max_neg_retaddr_rel, NULL,
               stats->log_neg_retaddr_rel);
  } else {
    count_item(retaddr_rel, &stats->max_pos_retaddr_rel, NULL,
               stats->log_pos_retaddr_rel);
  }

  unsigned char *retaddr_p = (unsigned char *)retaddr;
  if (stats->last_retaddr) {
    if (retaddr_p < stats->min_retaddr) {
      stats->min_retaddr = retaddr_p;
    }
    if (retaddr_p > stats->max_retaddr) {
      stats->max_retaddr = retaddr_p;
    }
    intnat delta = (uintnat)retaddr_p - (uintnat)stats->last_retaddr;
    if (delta < 0) {
      count_item(-delta, &stats->max_neg_delta, NULL, stats->log_neg_delta);
    } else {
      count_item(delta, &stats->max_pos_delta, NULL, stats->log_pos_delta);
    }
  } else {
    stats->min_retaddr = stats->max_retaddr = retaddr_p;
  }
  stats->last_retaddr = retaddr_p;

  if (dec->return_to_C) {
    ++ stats->return_to_C;
  } else {
    if (dec->is_short) ++ stats->short_descrs;
    else if (dec->is_long) ++ stats->long_descrs;
    else ++ stats->medium_descrs;

    /* For escaped descriptors, record why they could not go short. The first
       descriptor of a frametable always escapes; the rest are classified by
       whether their content fits (a section-boundary escape) or not. */
    if (!dec->is_short) {
      ++ stats->escape_reason[first_in_ft ? ESC_FT_FIRST
                                          : classify_escape(d, dec)];
    }

    uint32_t sz = dec->frame_size; /* in bytes */
    sz /= sizeof(uintnat);
    count_item(sz, &stats->max_framesize, stats->small_frames,
               stats->log_framesize);

    size_t regs = 0; /* number of live registers */
    size_t max_reg = 0; /* max live register number */
    size_t slots = 0; /* number of live stack slots */
    size_t max_slot = 0; /* max live stack slot offset */
    bool has_big_reg = false; /* saw a register too big for the small map */

    /* Extract the normalized (registers, stack slots) live set,
       decoding short and escaped descriptors compatibly. */
    if (dec->is_short) {
      /* Short: live registers come from the hot-register bitmap (set
         only for allocation descriptors); live stack slots from the
         frame's slot bitmap. */
      for (int i = 0; i < FRAME_NUM_HOT_REGS; ++i) {
        if (dec->short_reg_bitmap & (1u << i)) {
          add_reg(caml_frame_hot_regs[i], &regs, &max_reg, &has_big_reg,
                  stats);
        }
      }
      for (uint32_t byte = 0; byte < dec->short_live_bytes; ++byte) {
        unsigned char bits = dec->short_live[byte];
        for (int i = 0; bits != 0; ++i, bits >>= 1) {
          if (bits & 1) {
            size_t byte_ofs = ((size_t)byte * 8 + i) * sizeof(value);
            add_slot(byte_ofs, &slots, &max_slot);
          }
        }
      }
    } else if (dec->is_long) {
      const uint32_t *ofp = (const uint32_t *)(d + Frame_long_live_ofs);
      for (uint32_t n = dec->num_live; n > 0; n--, ofp++) {
        uint32_t v = caml_read_unaligned_uint32(ofp);
        if (v & 1) {
          add_reg(v >> 1, &regs, &max_reg, &has_big_reg, stats);
        } else {
          add_slot(v, &slots, &max_slot);
        }
      }
    } else {
      const uint16_t *ofp = (const uint16_t *)(d + Frame_live_ofs);
      for (uint32_t n = dec->num_live; n > 0; n--, ofp++) {
        uint16_t v = caml_read_unaligned_uint16(ofp);
        if (v & 1) {
          add_reg(v >> 1, &regs, &max_reg, &has_big_reg, stats);
        } else {
          add_slot(v, &slots, &max_slot);
        }
      }
    }

    /* GC live values = live registers + live stack slots. */
    count_item(regs + slots, &stats->max_lives, stats->small_lives,
               stats->log_lives);

    /* Register counts, recorded only for allocation descriptors.
       Non-allocation descriptors are not expected to keep GC roots in
       registers, so count any that do as an anomaly rather than folding
       them into the register statistics. */
    if (dec->has_allocs) {
      if (regs < REGS) {
        ++ stats->reg_count[regs];
      }
      if (max_reg < REGS) {
        ++ stats->max_reg[max_reg];
      }
      if (regs > stats->max_regs) {
        stats->max_regs = regs;
      }
      if (has_big_reg) {
        ++ stats->big_reg_descrs;
      }
    } else if (regs > 0) {
      ++ stats->noalloc_with_regs;
    }

    /* Slot counts */
    count_item(slots, &stats->max_slots, stats->small_slots,
               stats->log_slots);

    /* Maximum slot offset */
    count_item(max_slot, &stats->max_slot, stats->small_max_slot,
               stats->log_max_slot);

    /* Allocation sizes and comballoc counts. The alloc sizes store
       wosize-1 in both encodings (short: 4-bit nibbles; escaped: whole
       bytes after end_of_live), so they are directly comparable. */
    if (dec->has_allocs) {
      ++ stats->with_alloc;
      size_t num_allocs = dec->num_allocs;
      stats->alloc_sizes += num_allocs;
      count_item(num_allocs, &stats->max_comballocs, stats->small_comballocs,
                 stats->log_comballocs);
      if (dec->is_short) {
        for (size_t idx = 0; idx < num_allocs; ++idx) {
          unsigned char byte = dec->short_allocs[idx / 2];
          unsigned char sz = (idx & 1) ? (byte >> 4) : (byte & 0x0f);
          count_item(sz, &stats->max_alloc_size, stats->small_alloc_sizes,
                     stats->log_alloc_sizes);
        }
      } else {
        /* escaped: [num_allocs byte][num_allocs size bytes] */
        const unsigned char *sizes = dec->end_of_live + 1;
        for (size_t idx = 0; idx < num_allocs; ++idx) {
          count_item(sizes[idx], &stats->max_alloc_size,
                     stats->small_alloc_sizes, stats->log_alloc_sizes);
        }
      }
    }

    /* Count debug info if present */
    if (dec->has_debug) {
      ++ stats->with_debug;
      const unsigned char *p = dec->end_of_live;
      if (!dec->is_short && dec->has_allocs) {
        /* escaped: skip num_allocs byte + alloc bytes */
        p += (uintnat)(*p) + 1;
      }
      for (uint32_t i = 0; i < dec->num_debuginfo; ++i) {
        uint32_t offset = caml_read_unaligned_uint32(p);
        if (offset) { /* there may be invalid debuginfo slots */
          caml_debuginfo_measure((debuginfo)(p + offset));
        }
        p += sizeof(uint32_t);
      }
    }
  }

  if (!stats->max_descr || (dec->end > stats->max_descr)) {
    stats->max_descr = (unsigned char *)dec->end;
  }
}

/* Record stats for all descriptors from a frametable */

static void add_frametable_to_stats(caml_frametable_list *cell,
                                    struct frametable_stats *stats)
{
  unsigned char *begin = (unsigned char *)cell->frametable;
  unsigned char *past = cell->end != NULL
    ? (unsigned char *)cell->end
    : (unsigned char *)(cell->frametable + 1);
  ++ stats->frametables;
  if (!stats->min_descr || begin < stats->min_descr) {
    stats->min_descr = begin;
  }
  if (!stats->max_descr || past > stats->max_descr) {
    stats->max_descr = past;
  }
  frametable_iter it;
  frametable_iter_start(&it, cell);
  while (frametable_iter_more(&it)) {
    uintnat retaddr;
    frame_descr *d = frametable_iter_next(&it, &retaddr);
    struct frame_descr_decoded dec;
    caml_decode_frame_descr(d, &dec);
    add_descriptor_to_stats(d, &dec, retaddr, stats);
  }
}

/* Record and report stats for all frametables on a list */

static void report_frametables_stats(caml_frametable_list *new_frametables)
{
  struct frametable_stats stats;
  clear_stats(&stats);
  iter_list(new_frametables, cur) {
    add_frametable_to_stats(cur, &stats);
    accumulate_frametable_stats(cur->frametable, &stats);
    clear_per_frametable_stats(&stats);
  }
  report_stats(&stats);
}

/**** Differential checking (Xframe_index_check) ****/

/* Under OCAMLRUNPARAM=Xframe_index_check=1, every registration
   rebuilds the hash table this file replaced and compares answers:
   each registered descriptor's return address (and its neighbours
   +-1), two million random pcs around the program text and a hundred
   thousand arbitrary ones must give the same descriptor pointer from
   the reference table, from the index (lookup_nocache) and through
   the cache (caml_find_frame_descr); and each short descriptor's
   short_end must agree with caml_decode_frame_descr. Any mismatch is
   fatal. */

typedef struct {
  uintnat retaddr;
  frame_descr *fd;
} check_entry;

typedef struct {
  uintnat mask;
  check_entry *slots;
} check_table;

/* Fill [r] with every descriptor the index should answer for: the
   registered tables plus the preindexed -manual-module-init tables,
   minus unregistered main tables (which keep their preindexed cells
   but lose their index). */
static void check_table_build(caml_frame_descrs *t, check_table *r)
{
  intnat n = count_descriptors(t->frametables)
    + count_descriptors(t->preindexed);
  uintnat size = 4;
  while (size < 2 * (uintnat)n) size *= 2;
  r->mask = size - 1;
  r->slots = caml_stat_calloc_noexc(size, sizeof(check_entry));
  if (r->slots == NULL) caml_raise_out_of_memory();
  for (caml_frametable_list *lists[2] = { t->frametables, t->preindexed },
         **l = lists; l < lists + 2; l++) {
    iter_list(*l, cur) {
      if (l == lists + 1
          && t->main_removed[main_table_index(t, cur->frametable)])
        continue;
      frametable_iter it;
      frametable_iter_start(&it, cur);
      while (frametable_iter_more(&it)) {
        uintnat retaddr;
        frame_descr *d = frametable_iter_next(&it, &retaddr);
        uintnat h = Hash_retaddr(retaddr, r->mask);
        while (r->slots[h].fd != NULL) h = (h + 1) & r->mask;
        r->slots[h].retaddr = retaddr;
        r->slots[h].fd = d;
      }
    }
  }
}

static frame_descr *check_table_find(const check_table *r, uintnat pc)
{
  uintnat h = Hash_retaddr(pc, r->mask);
  while (true) {
    check_entry e = r->slots[h];
    if (e.fd == NULL) return NULL;
    if (e.retaddr == pc) return e.fd;
    h = (h + 1) & r->mask;
  }
}

/* The splitmix64 finalizer: reproducible random pcs. */
static uint64_t splitmix64(uint64_t *s)
{
  uint64_t z = (*s += UINT64_C(0x9e3779b97f4a7c15));
  z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb);
  return z ^ (z >> 31);
}

static intnat check_mismatches = 0;
/* Report the first few mismatches in full, then just count. */
#define CHECK_MISMATCHES_SHOWN 10

static void check_pc(caml_frame_descrs *t, check_table *r, uintnat pc,
                     const char *what)
{
  frame_descr *want = check_table_find(r, pc);
  frame_descr *a = lookup_nocache(t, pc);
  frame_descr *b = caml_find_frame_descr(t, pc); /* cache miss or hit */
  frame_descr *c = caml_find_frame_descr(t, pc); /* cache hit */
  if (a != want || b != want || c != want) {
    if (check_mismatches < CHECK_MISMATCHES_SHOWN)
      fprintf(stderr,
              "frame_index_check MISMATCH (%s) pc=%p reference=%p "
              "index=%p cached=%p/%p\n",
              what, (void *)pc, (void *)want, (void *)a, (void *)b,
              (void *)c);
    check_mismatches++;
  }
}

#define CHECK_RANDOM_PCS 2000000
/* How far the random pcs stray beyond the program text, each side. */
#define CHECK_RANDOM_SLACK ((uintnat)1 << 19)
#define CHECK_ARBITRARY_PCS 100000

static void check_run(caml_frame_descrs *t)
{
  check_table r;
  check_table_build(t, &r);
  intnat n = 0, duplicates = 0, end_mismatches = 0;
  uintnat lo = (uintnat)-1, hi = 0;
  check_mismatches = 0;
  for (caml_frametable_list *lists[2] = { t->frametables, t->preindexed },
         **l = lists; l < lists + 2; l++) {
    iter_list(*l, cur) {
      if (l == lists + 1
          && t->main_removed[main_table_index(t, cur->frametable)])
        continue;
      frametable_iter it;
      frametable_iter_start(&it, cur);
      while (frametable_iter_more(&it)) {
        uintnat ret;
        frame_descr *d = frametable_iter_next(&it, &ret);
        n++;
        if (ret < lo) lo = ret;
        if (ret > hi) hi = ret;
        /* Two descriptors with one return address: the reference and
           the index may legitimately answer different copies. */
        if (check_table_find(&r, ret) != d) duplicates++;
        if (frame_is_short(d)) {
          struct frame_descr_decoded dec;
          caml_decode_frame_descr(d, &dec);
          if (dec.end != short_end(d)) end_mismatches++;
        }
        check_pc(t, &r, ret, "retaddr");
      }
    }
  }
  iter_list(t->frametables, cur) {
    frametable_iter it;
    frametable_iter_start(&it, cur);
    while (frametable_iter_more(&it)) {
      uintnat ret;
      frametable_iter_next(&it, &ret);
      check_pc(t, &r, ret - 1, "retaddr-1");
      check_pc(t, &r, ret + 1, "retaddr+1");
    }
  }
  uint64_t seed = 42;
  uintnat span = hi - lo + 2 * CHECK_RANDOM_SLACK;
  for (intnat i = 0; i < CHECK_RANDOM_PCS; i++) {
    uintnat pc = lo - CHECK_RANDOM_SLACK + (uintnat)(splitmix64(&seed) % span);
    check_pc(t, &r, pc, "random-in-text");
  }
  for (intnat i = 0; i < CHECK_ARBITRARY_PCS; i++)
    check_pc(t, &r, (uintnat)splitmix64(&seed), "random-any");
  caml_stat_free(r.slots);
  fprintf(stderr,
          "frame_index_check: %s: %ld descriptors (each also +-1), "
          "%d random pcs; %ld mismatches, %ld duplicates, "
          "%ld short-end mismatches\n",
          (check_mismatches > 0 || end_mismatches > 0) ? "FAILED" : "ok",
          (long)n, CHECK_RANDOM_PCS + CHECK_ARBITRARY_PCS,
          (long)check_mismatches, (long)duplicates, (long)end_mismatches);
  if (check_mismatches > 0 || end_mismatches > 0)
    caml_fatal_error("Xframe_index_check failed");
}

/**** Registration ****/

/* The -manual-module-init unit table (runtime/startup_nat.c): the
   compiler always emits it, empty unless -manual-module-init. Each
   unit's frametable is registered when the unit is first initialised.
   Must match the definition in startup_nat.c. */
struct caml_unit_deps_entry {
  const char *unit_name;
  void *entry_fn;
  value *gc_roots;
  intnat *frametable;
  intnat num_deps;
  const intnat *dep_indices;
  int init_state; /* enum init_state in startup_nat.c */
  value raised_exn;
};
struct caml_unit_deps_table {
  intnat num_units;
  struct caml_unit_deps_entry entries[];
};
extern struct caml_unit_deps_table caml_unit_deps_table;

Caml_inline table_ref unit_deps_ref(const struct caml_unit_deps_entry *e)
{
  return (table_ref){ e->frametable, NULL };
}

/* A short summary of the index structures, appended to the
   Xmeasure_frametables report. */
static void measure_report_index(caml_frame_descrs *t)
{
  size_t extras_bytes = 0;
  intnat extras_segs = 0;
  for (intnat i = 0; i < t->nextras; i++) {
    extras_bytes += segment_index_bytes(&t->extras[i].ix);
    extras_segs += t->extras[i].ix.nsegs;
  }
  intnat built = 0;
  for (intnat i = 0; i < units_count; i++)
    if (atomic_load_acquire(&units[i].index) != NULL) built++;
  const segment_index *g = atomic_load_acquire(&fallback_index);
  /* What the replaced hash table would hold: 16-byte slots in a power
     of two kept at most half full. */
  size_t slots = 4;
  while (slots < 2 * (size_t)t->num_descr) slots *= 2;
  printf("Frame-descriptor index: %ld units (%ld built), "
         "arena %zu of %zu bytes used,\n",
         (long)units_count, (long)built,
         atomic_load_relaxed(&arena_used), arena_len);
  printf("  %ld extra tables (%ld segments, %zu bytes), fallback ",
         (long)t->nextras, (long)extras_segs, extras_bytes);
  if (g != NULL)
    printf("%ld segments (%zu bytes).\n", (long)g->nsegs,
           segment_index_bytes(g));
  else
    printf("not built.\n");
  printf("Replaced hash table: %zu bytes.\n", slots * sizeof(check_entry));
}

/* Register [new_frametables], prepending them to [table]'s list and
   indexing them. Called at startup (when table->main_tables == NULL)
   and from STW registration; may raise Out_of_memory, as filling the
   replaced hash table could. */
static void add_frame_descriptors(caml_frame_descrs *table,
                                  caml_frametable_list *new_frametables)
{
  CAMLassert(new_frametables != NULL);
  bool first = (table->main_tables == NULL);

  caml_frametable_list *tail = frametables_list_tail(new_frametables);
  tail->next = table->frametables;
  table->frametables = new_frametables;
  table->num_descr = (int)count_descriptors(table->frametables);

  if (!first && !table->main_dirty) {
    /* A registration whose tables are all indexed already (a
       -manual-module-init unit pre-indexed at startup, or a second
       registration of a table) changes no index: keep just the list
       update, as the replaced code did. */
    bool all_indexed = true;
    for (caml_frametable_list *cur = new_frametables;
         all_indexed && cur != tail->next; cur = cur->next) {
      all_indexed = is_active_main(table, cur->frametable)
        || extras_find(table, cur->frametable) >= 0;
      if (cur == tail) break;
    }
    if (all_indexed) return;
  }

  if (first) {
    /* The main tables: everything registered now, plus the frametables
       of caml_unit_deps_table. The latter are only registered when
       their unit is first initialised, but a return address in an
       uninitialised unit cannot be on any stack, so indexing them
       early is unobservable -- and makes their later registration
       free. */
    intnat m = 0, nud = caml_unit_deps_table.num_units;
    iter_list(table->frametables, cur) m++;
    table->main_tables =
      caml_stat_alloc((size_t)(m + nud + 1) * sizeof(table_ref));
    m = 0;
    iter_list(table->frametables, cur)
      table->main_tables[m++] = list_ref(cur);
    for (intnat i = 0; i < nud; i++) {
      table_ref t = unit_deps_ref(&caml_unit_deps_table.entries[i]);
      if (t.tbl != NULL) table->main_tables[m++] = t;
    }
    qsort(table->main_tables, (size_t)m, sizeof(table_ref), table_compare);
    intnat u = 0; /* drop duplicates: a table can be registered twice */
    for (intnat i = 0; i < m; i++)
      if (u == 0 || table->main_tables[u - 1].tbl != table->main_tables[i].tbl)
        table->main_tables[u++] = table->main_tables[i];
    table->nmain_tables = u;
    table->main_removed = caml_stat_calloc_noexc((size_t)u + 1, 1);
    if (table->main_removed == NULL) caml_raise_out_of_memory();
    for (intnat i = 0; i < nud; i++) {
      table_ref t = unit_deps_ref(&caml_unit_deps_table.entries[i]);
      if (t.tbl == NULL) continue;
      bool listed = false;
      iter_list(table->frametables, cur)
        if (cur->frametable == t.tbl) listed = true;
      if (!listed) {
        caml_frametable_list *c =
          caml_stat_alloc(sizeof(caml_frametable_list));
        c->frametable = t.tbl;
        c->end = t.end;
        c->next = table->preindexed;
        table->preindexed = c;
      }
    }
    intnat n;
    table_ref *tables = active_tables(table, &n);
    if (caml_frame_index_eager) {
      /* Eager: pay the whole build at startup; no units, every lookup
         goes straight to the fallback index. Not noexc: raises on
         failure rather than returning false. */
      (void)segment_index_build(&table->fallback, tables, n, false);
      atomic_store_release(&fallback_index, &table->fallback);
    } else {
      units_init(tables, n);
      if (caml_frame_index_prewarm) prewarm_start();
    }
    caml_stat_free(tables);
  } else if (table->main_dirty) {
    /* A main table was unregistered: the per-unit indexes may still
       hold it, so build the fallback index over what is left and
       answer everything from it from now on. */
    intnat n;
    table_ref *tables = active_tables(table, &n);
    segment_index *old = atomic_load_acquire(&fallback_index);
    /* not noexc: raises on failure rather than returning false */
    (void)segment_index_build(&table->fallback, tables, n, false);
    atomic_store_release(&fallback_index, &table->fallback);
    if (old != NULL && old != &table->fallback) {
      /* A lazily built fallback lives in its own mapping; no lookups
         run during a STW section, so it can go now. */
      segment_index_free(old);
      index_unmap(old, sizeof(segment_index));
    }
    caml_stat_free(tables);
    table->main_dirty = false;
  }

  /* Index each new table that is not a main table, on its own. */
  for (caml_frametable_list *cur = new_frametables; cur != tail->next;
       cur = cur->next) {
    if (!is_active_main(table, cur->frametable))
      extras_add(table, list_ref(cur));
    if (cur == tail) break;
  }
  atomic_fetch_add_explicit(&index_generation, 1, memory_order_release);

  if (caml_measure_frametables) {
    report_frametables_stats(new_frametables);
    measure_report_index(table);
  }
  if (caml_frame_index_check) check_run(table);
}

/* Reclaim the zombie list (in the STW registration that always
   follows an unregistration). A zombie that no remaining registration
   refers to loses its extra index; one that was a main table sets
   [main_dirty], forcing the fallback rebuild in add_frame_descriptors. */
static void clean_frame_descriptors(caml_frame_descrs *table)
{
  caml_frametable_list *cur = table->zombies, *rem;
  bool changed = false;
  while (cur != NULL) {
    rem = cur;
    cur = cur->next;
    bool listed = false;
    iter_list(table->frametables, c)
      if (c->frametable == rem->frametable) listed = true;
    if (!listed) {
      intnat mi = main_table_index(table, rem->frametable);
      if (mi >= 0 && !table->main_removed[mi]) {
        /* A main table stops being indexed once no registration of it
           is left. */
        table->main_removed[mi] = 1;
        table->main_dirty = true;
      } else if (extras_remove(table, rem->frametable)) {
        changed = true;
      }
    }
    caml_stat_free(rem);
  }
  table->zombies = NULL;
  if (changed)
    atomic_fetch_add_explicit(&index_generation, 1, memory_order_release);
}

/**** Initialisation and the exported entry points ****/

/* Defined in code generated by ocamlopt. */
#ifdef LINK_ORDER_FRAMETABLES
extern void *caml_frametable_ranges[];
#else
extern intnat *caml_frametable[];
#endif

static caml_frametable_list *cons(void *frametable, const void *end,
                                  caml_frametable_list *tl)
{
  caml_frametable_list *li = caml_stat_alloc(sizeof(caml_frametable_list));
  li->frametable = frametable;
  li->end = end;
  li->next = tl;
  return li;
}

/* Allocates the cons cell and the copy of the frametable in a single
   block, so that freeing the cell on unregistration frees the copy
   too. */
static caml_frametable_list *copy_cons(
  intnat **frametable, intnat size, caml_frametable_list *tl)
{
  caml_frametable_list *li =
    caml_stat_alloc(sizeof(caml_frametable_list) + size);
  intnat *frametable_copy = (intnat *)(li + 1);
  memcpy(frametable_copy, *frametable, size);
  *frametable = frametable_copy;
  li->frametable = frametable_copy;
  li->end = NULL;
  li->next = tl;
  return li;
}

void caml_init_frame_descriptors(void)
{
  caml_frametable_list *frametables = NULL;
#ifdef LINK_ORDER_FRAMETABLES
  for (int i = 0; caml_frametable_ranges[i] != NULL; i += 2)
    frametables = cons(caml_frametable_ranges[i],
                       caml_frametable_ranges[i + 1], frametables);
#else
  for (int i = 0; caml_frametable[i] != 0; i++)
    frametables = cons(caml_frametable[i], NULL, frametables);
#endif

  /* [caml_init_frame_descriptors] is called from [init_gc], before
     any mutator can run: [current_frame_descrs] can be mutated
     freely. */
  add_frame_descriptors(&current_frame_descrs, frametables);
}

static void register_frametables_from_stw_single(
  caml_frametable_list *new_frametables)
{
  clean_frame_descriptors(&current_frame_descrs);
  add_frame_descriptors(&current_frame_descrs, new_frametables);
}

static void stw_register_frametables(
    caml_domain_state *domain,
    void *frametables,
    int participating_count,
    caml_domain_state **participating)
{
  Caml_global_barrier_if_final(participating_count) {
    register_frametables_from_stw_single(
      (caml_frametable_list *)frametables);
  }
}

static void register_frametable_list(caml_frametable_list *new_frametables)
{
  do {} while (!caml_try_run_on_all_domains(
                 &stw_register_frametables, new_frametables, 0));
}

void caml_register_frametables(void **tables, int ntables)
{
  caml_frametable_list *new_frametables = NULL;
  for (int i = 0; i < ntables; i++)
    new_frametables = cons(tables[i], NULL, new_frametables);
  register_frametable_list(new_frametables);
}

#ifdef LINK_ORDER_FRAMETABLES
void caml_register_frametable_ranges(void **begins, void **ends,
                                     int ntables)
{
  caml_frametable_list *new_frametables = NULL;
  for (int i = 0; i < ntables; i++)
    new_frametables = cons(begins[i], ends[i], new_frametables);
  register_frametable_list(new_frametables);
}

void caml_register_frametable_range(void *begin, void *end)
{
  caml_register_frametable_ranges(&begin, &end, 1);
}
#endif

void caml_copy_and_register_frametables(void **table, int *sizes,
                                        int ntables)
{
  caml_frametable_list *new_frametables = NULL;
  for (int i = 0; i < ntables; i++)
    new_frametables = copy_cons((intnat **)(table + i),
                                sizes[i], new_frametables);
  register_frametable_list(new_frametables);
}

static void remove_frame_descriptors(
  caml_frame_descrs *table, void **frametables, int ntables)
{
  void *frametable;
  caml_frametable_list **previous;

  /* cannot release the domain lock here (e.g. custom block finaliser) */
  caml_plat_lock_blocking(&table->mutex);

  previous = &table->frametables;

  iter_list(table->frametables, current) {
  resume:
    for (int i = 0; i < ntables; i++) {
      if (current->frametable == frametables[i]) {
        *previous = current->next;
        current->next = table->zombies;
        table->zombies = current;
        ntables--;
        if (ntables == 0) goto release;
        current = *previous;
        frametable = frametables[i];
        frametables[i] = frametables[ntables];
        frametables[ntables] = frametable;
        goto resume;
      }
    }
    previous = &current->next;
  }

 release:
  caml_plat_unlock(&table->mutex);
}

void caml_unregister_frametables(void **frametables, int ntables)
{
  remove_frame_descriptors(&current_frame_descrs, frametables, ntables);
}

void caml_register_frametable(void *frametables)
{
  caml_register_frametables(&frametables, 1);
}

void *caml_copy_and_register_frametable(void *frametable, int size)
{
  caml_copy_and_register_frametables(&frametable, &size, 1);
  return frametable;
}

void caml_unregister_frametable(void *frametables)
{
  caml_unregister_frametables(&frametables, 1);
}

caml_frame_descrs *caml_get_frame_descrs(void)
{
  return &current_frame_descrs;
}
