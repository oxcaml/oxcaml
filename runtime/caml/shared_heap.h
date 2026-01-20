/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2015 Indian Institute of Technology, Madras                */
/*   Copyright 2015 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#ifndef CAML_SHARED_HEAP_H
#define CAML_SHARED_HEAP_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "roots.h"
#include "domain.h"
#include "misc.h"
#include "gc_stats.h"
#include "major_gc.h"
#include "sizeclasses.h"

CAMLextern atomic_uintnat caml_compactions_count;

struct caml_heap_state;
struct pool;

struct caml_heap_state* caml_init_shared_heap(void);
void caml_teardown_shared_heap(struct caml_heap_state* heap);

value* caml_shared_try_alloc(struct caml_heap_state*,
                             mlsize_t, tag_t, reserved_t);

/* If we were to grow the shared heap, how much would we grow it? */
uintnat caml_shared_heap_grow_bsize(void);

/* Update the dependent_bytes field of the heap stats. */
void caml_add_dependent_bytes (struct caml_heap_state *local, intnat n);

/* Copy the domain-local heap stats into a heap stats sample. */
void caml_collect_heap_stats_sample(
  struct caml_heap_state* local,
  struct heap_stats *sample);

/* Add the global orphaned heap stats into an accumulator. */
void caml_accum_orphan_heap_stats(struct heap_stats *acc);

uintnat caml_heap_size(struct caml_heap_state*);
uintnat caml_top_heap_words(struct caml_heap_state*);
uintnat caml_heap_blocks(struct caml_heap_state*);

void caml_get_global_heap_stats(struct global_heap_stats *stats);

void caml_compact_heap(caml_domain_state* domain_state,
                         int participating_count,
                         caml_domain_state** participants);

void caml_shared_unpin(value v);

/* always readable by all threads
   written only by a single thread during STW periods */
typedef uintnat status;
struct global_heap_state {
  status MARKED, UNMARKED, GARBAGE;
};
extern struct global_heap_state caml_global_heap_state;

/* CR mshinwell: ensure this matches [Emitaux] */
enum {NOT_MARKABLE = 3 << HEADER_COLOR_SHIFT};

Caml_inline int Has_status_hd(header_t hd, status s) {
  return Color_hd(hd) == s;
}

Caml_inline int Has_status_val(value v, status s) {
  return Has_status_hd(Hd_val(v), s);
}

Caml_inline header_t With_status_hd(header_t hd, status s) {
  return Hd_with_color(hd, s);
}

Caml_inline int is_garbage(value v) {
  return Has_status_val(v, caml_global_heap_state.GARBAGE);
}

Caml_inline int is_unmarked(value v) {
  return Has_status_val(v, caml_global_heap_state.UNMARKED);
}

Caml_inline int is_marked(value v) {
  return Has_status_val(v, caml_global_heap_state.MARKED);
}

Caml_inline int is_not_markable(value v) {
  return Has_status_val(v, NOT_MARKABLE);
}

Caml_inline status caml_allocation_status(void) {
  return
    caml_marking_started()
    ? caml_global_heap_state.MARKED
    : caml_global_heap_state.UNMARKED;
}

/* Notionally-opaque type to support fast inline allocation on a shared heap */

typedef struct shared_heap_fast_data_s *shared_heap_fast_data_p;

/* Get the fast-allocation structure for the given heap. */

shared_heap_fast_data_p caml_shared_fast_data(struct caml_heap_state *);

/* Call back into the shared-heap code from the inline fast allocation
 * code (below), when the "fast allocation data" is somehow
 * exhausted. Restores invariant.
 */

void caml_shared_fast_data_refill(struct caml_heap_state *,
                                  sizeclass_t);

/* Expose implementation of this opaque type to allow the inline
 * function below to access the contents. Clients should not rely on
 * the contents of this data structure. */
struct shared_heap_fast_data_s {
  value **lists[NUM_SIZECLASSES];
  /* Invariant: `(lists[sz] == NULL || *lists[sz] != NULL)`, that is,
   *  we don't point to any empty free lists. */
};

/* Allocates a block of at least `whsize` words, using `fast_data`,
 * for `domain`, returning a pointer to the header word. Requires
 * `whsize <= SIZECLASS_MAX`. If we can't do a fast allocation, return
 * NULL (meaning "use caml_shared_try_alloc instead"). Does not
 * accumulate shared-heap stats, so the caller should accumulate those
 * and later call `caml_shared_add_pool_stats`.
 */

Caml_inline void *caml_shared_fast_alloc (mlsize_t whsize,
                                          shared_heap_fast_data_p fast_data,
                                          caml_domain_state *domain)
{
  CAMLassert(whsize <= SIZECLASS_MAX);

  sizeclass_t sz = sizeclass_whsize[whsize];
  value **free_p = fast_data->lists[sz];
  if (!free_p) {
    return NULL;
  }
  value *block = *free_p;
  CAMLassert(block != NULL);
  value *next = (value*)(block[1]);
  caml_prefetchw(next);
  caml_prefetchw(next+3); /* will coalesce, or prefetch the next cache line */
  *free_p = next;
  if (!next) caml_shared_fast_data_refill(domain->shared_heap, sz);
  return (void*)block;
}

/* Update shared-heap stats at the end of a minor GC, to account for
 * some number of free-list allocations. */

void caml_shared_add_pool_stats(struct caml_heap_state *,
                                uintnat /* pool_live_blocks */,
                                uintnat /* pool_live_words */,
                                uintnat /* pool_frag_words */);

void caml_redarken_pool(struct pool*, scanning_action, void*);

intnat caml_sweep(struct caml_heap_state*, intnat);

void caml_cycle_heap_from_stw_single(void);

/* must be called on each domain
   (after caml_cycle_heap_from_stw_single) */
void caml_cycle_heap(struct caml_heap_state*);

/* Heap invariant verification (for debugging) */
void caml_verify_heap_from_stw(caml_domain_state *domain);

#ifdef DEBUG
/* [is_garbage(v)] returns true if [v] is a garbage value */
int is_garbage (value);
#endif

#endif /* CAML_INTERNALS */

#endif /* CAML_SHARED_HEAP_H */
