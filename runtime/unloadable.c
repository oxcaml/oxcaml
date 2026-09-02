/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                       Mark Shinwell, Jane Street                       */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "caml/unloadable.h"
#include "caml/codefrag.h"
#include "caml/frame_descriptors.h"
#include "caml/globroots.h"
#include "caml/memory.h"
#include "caml/platform.h"
#include "caml/shared_heap.h"

/* Debug tracing: when [OCAML_UNLOADABLE_DEBUG] is set in the environment,
 * trace each unit registration, activation and unload to stderr. Latched on
 * first call; subsequent calls reuse the cached value. */
static int unloadable_debug_init = 0;
static int unloadable_debug_enabled = 0;

static int unloadable_debug(void) {
  if (!unloadable_debug_init) {
    const char *s = getenv("OCAML_UNLOADABLE_DEBUG");
    unloadable_debug_enabled = (s != NULL && s[0] != '\0' && s[0] != '0');
    unloadable_debug_init = 1;
  }
  return unloadable_debug_enabled;
}

/* Linked list of all currently-registered unloadable units. Protected by
 * [units_mutex]. The extent free callback (running from GC sweeping on
 * whichever domain owns the extent) uses it to map an extent base address
 * back to its unit. The mutex is statically initialised so concurrent
 * first-time registrations from multiple domains do not race on its
 * initialisation. */
static struct caml_unloadable_unit *units_head = NULL;
static caml_plat_mutex units_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* Units whose extents the GC has reclaimed, awaiting the actual unload work
   (code-fragment removal, frame-table unregistration, buffer free). The
   extent free callback runs from sweeping, but frame-table removal must
   happen under the STW barrier (see
   [caml_unregister_frametable_from_stw_single]) and the buffer cannot be
   freed before the frame table is removed, so the callback only queues the
   unit here; [caml_unloadable_process_pending_unloads] drains the queue at
   the start of the next major cycle. Protected by [units_mutex]; the [next]
   field is reused for this list (the unit is off [units_head] by then). */
static struct caml_unloadable_unit *pending_unloads = NULL;

/* Cumulative counters for observability. Updated under [units_mutex]. */
static uintnat units_registered_total = 0;
static uintnat units_unloaded_total = 0;

/* Number of currently-registered (registered minus unloaded) units. Read
 * from the major mark path on every closure to short-circuit
 * [caml_darken_unloadable_code_blocks_in_closure] when there are no
 * unloadable units at all. Accessed with relaxed atomics: stale reads are
 * harmless — a false positive walks the closure (cheap), and a false
 * negative is impossible because mutators only mark after registrations
 * are visible. */
CAMLexport atomic_uintnat caml_unloadable_units_live_count = 0;

void caml_register_unloadable_unit(struct caml_unloadable_unit *u) {
  CAMLassert(u != NULL);

  /* Register each text range as a code fragment so
   * [caml_find_code_fragment_by_pc] returns the fragment for any PC in the
   * unit's text. Digests are not used for unloadable units (DIGEST_IGNORE);
   * uniqueness is by-pc rather than by-content.
   *
   * Immediately after registration, tag the fragment with this unit so the
   * stack-scan hot path (F.3 in [caml_visit_frame_code_ptr_slots]) can
   * derive "is this PC in unloadable code?" from the same skiplist lookup
   * it already does. The fragment is allocated by
   * [caml_register_code_fragment] with [owner_unloadable_unit = NULL], and
   * the mark phase cannot find a PC inside the unit's text until the
   * unit's text has executed at least once, which only happens after this
   * function returns and the JIT loader calls the entry. */
  for (uintnat i = 0; i < u->num_text_ranges; i++) {
    char *start = u->text_ranges[2 * i];
    char *end = u->text_ranges[2 * i + 1];
    int fragnum =
        caml_register_code_fragment(start, end, DIGEST_IGNORE, NULL);
    u->text_range_fragnums[i] = fragnum;
    struct code_fragment *cf = caml_find_code_fragment_by_num(fragnum);
    /* [caml_register_code_fragment] just inserted this fragment; the lookup
     * cannot miss. */
    CAMLassert(cf != NULL);
    cf->owner_unloadable_unit = u;
  }

  /* Register the frame table so stack walks can find frame descriptors
   * for the unit's return addresses. The table inside the unit's buffer is
   * registered directly: it cannot be copied, because frame descriptors
   * encode their return addresses as 32-bit self-relative offsets
   * ([retaddr_rel]) and a copy is not guaranteed to land within 2GB of the
   * unit's code. Unregistration therefore happens under the STW barrier
   * (via [caml_unregister_frametable_from_stw_single]) immediately before
   * the buffer is freed. */
  if (u->frametable != NULL) {
    void *tbl = (void *)u->frametable;
    caml_register_frametables(&tbl, 1);
  }

  /* Register the unit's [gc_roots] as dyn-globals, exactly as for a
   * non-unloadable JIT/Dynlink unit. This is what keeps heap values stored
   * into the unit's static data alive while the unit's initialiser runs:
   * during that window the static blocks still carry their NOT_MARKABLE
   * emission headers, so the GC does not scan them itself.
   * [caml_activate_unloadable_unit] removes this registration when it
   * donates the blocks to the heap (at which point the ordinary mark scan
   * takes over, and a permanent root registration would pin the unit
   * forever). */
  if (u->gc_roots != NULL) {
    caml_register_dyn_globals(&u->gc_roots, 1);
  }

  caml_plat_lock_blocking(&units_mutex);
  u->next = units_head;
  units_head = u;
  units_registered_total++;
  uintnat seq = units_registered_total;
  caml_plat_unlock(&units_mutex);

  atomic_fetch_add(&caml_unloadable_units_live_count, 1);

  if (unloadable_debug()) {
    fprintf(stderr,
        "[unloadable] register #%lu: unit=%p blocks=[%p,+%lu) "
        "text_ranges=%lu gc_roots=%p frametable=%p\n",
        (unsigned long)seq, (void *)u, u->blocks_base,
        (unsigned long)u->blocks_size, (unsigned long)u->num_text_ranges,
        u->gc_roots, (void *)u->frametable);
  }
}

/* Extent free callback: the GC has determined that every static block of
 * some unit is dead. Runs from sweeping (or from heap teardown), with
 * finaliser-like restrictions. Unlinks the unit and queues it for
 * unloading at the next STW point; the counters are updated here so that
 * observers (e.g. tests polling [Eval.unloadable_units_unloaded_total])
 * see the reclamation as soon as the GC has decided it.
 *
 * No live reference to the unit's text or data can exist any more: any
 * closure, return address or in-flight code pointer would have darkened
 * one of the unit's [Code_block]s during the previous mark phase,
 * contradicting the extent being fully dead. The unit's frame table and
 * code fragments remain registered until the queue is drained, which is
 * harmless: no stack walk can encounter the unit's PCs. */
static void unloadable_extent_freed(void *base, size_t size) {
  caml_plat_lock_blocking(&units_mutex);
  struct caml_unloadable_unit *u = NULL;
  struct caml_unloadable_unit **link = &units_head;
  while (*link != NULL) {
    if ((*link)->blocks_base == base) {
      u = *link;
      *link = u->next;
      break;
    }
    link = &(*link)->next;
  }
  if (u != NULL) {
    units_unloaded_total++;
    u->next = pending_unloads;
    pending_unloads = u;
  }
  caml_plat_unlock(&units_mutex);

  if (u == NULL) {
    /* An extent we did not create; nothing to do. (Cannot happen for
     * extents added by [caml_activate_unloadable_unit].) */
    return;
  }
  CAMLassert(u->blocks_size == size);

  atomic_fetch_sub(&caml_unloadable_units_live_count, 1);

  if (unloadable_debug()) {
    fprintf(stderr,
        "[unloadable] extent dead: unit=%p blocks=[%p,+%lu) "
        "text_ranges=%lu\n",
        (void *)u, base, (unsigned long)size,
        (unsigned long)u->num_text_ranges);
  }
}

void caml_unloadable_process_pending_unloads(void) {
  /* Caller holds the STW barrier (single-domain section of
   * [cycle_major_heap_from_stw_single]). */
  caml_plat_lock_blocking(&units_mutex);
  struct caml_unloadable_unit *to_unload = pending_unloads;
  pending_unloads = NULL;
  caml_plat_unlock(&units_mutex);

  while (to_unload != NULL) {
    struct caml_unloadable_unit *u = to_unload;
    to_unload = u->next;

    if (unloadable_debug()) {
      fprintf(stderr,
          "[unloadable] unload: unit=%p blocks=[%p,+%lu) text_ranges=%lu\n",
          (void *)u, u->blocks_base, (unsigned long)u->blocks_size,
          (unsigned long)u->num_text_ranges);
    }

    for (uintnat i = 0; i < u->num_text_ranges; i++) {
      struct code_fragment *cf =
          caml_find_code_fragment_by_num(u->text_range_fragnums[i]);
      if (cf != NULL) caml_remove_code_fragment(cf);
    }

    /* Remove the frame table's descriptors immediately (we are in STW, and
     * the table memory — inside the unit's buffer — is about to be
     * freed). */
    if (u->frametable != NULL) {
      caml_unregister_frametable_from_stw_single(u->frametable);
    }

    if (u->on_unload != NULL) u->on_unload(u);
  }
}

void caml_activate_unloadable_unit(struct caml_unloadable_unit *u) {
  CAMLassert(u != NULL);

  if (unloadable_debug()) {
    fprintf(stderr, "[unloadable] activate: unit=%p blocks=[%p,+%lu)\n",
        (void *)u, u->blocks_base, (unsigned long)u->blocks_size);
  }

  /* Drop the dyn-globals registration and donate the unit's static blocks
   * to the major heap. No GC safe point separates the two steps (plain C
   * calls), so there is no window in which a heap value referenced only
   * from the unit's static data is unprotected: before, the dyn-globals
   * scan covers it; after, the blocks are ordinary markable heap blocks
   * and the mark phase scans their fields directly.
   *
   * [caml_add_blocks_to_heap] forces every block to the current allocation
   * colour (MARKED if marking is in progress), so the unit survives the
   * current major cycle by construction; from the next cycle onwards its
   * blocks live and die by ordinary marking. */
  if (u->gc_roots != NULL) {
    caml_unregister_dyn_global(u->gc_roots);
    u->gc_roots = NULL;
  }
  caml_add_blocks_to_heap(u->blocks_base, u->blocks_size,
                          &unloadable_extent_freed);
}

uintnat caml_unloadable_units_registered_total(void) {
  caml_plat_lock_blocking(&units_mutex);
  uintnat r = units_registered_total;
  caml_plat_unlock(&units_mutex);
  return r;
}

uintnat caml_unloadable_units_unloaded_total(void) {
  caml_plat_lock_blocking(&units_mutex);
  uintnat r = units_unloaded_total;
  caml_plat_unlock(&units_mutex);
  return r;
}
