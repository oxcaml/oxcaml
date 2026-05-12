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
 * trace each unit registration and unload to stderr. Latched on first call;
 * subsequent calls reuse the cached value. */
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
 * [units_mutex]. Iteration during the GC's end-of-cycle pass holds the
 * mutex (and runs from a stop-the-world section). The mutex is statically
 * initialised so concurrent first-time registrations from multiple domains
 * do not race on its initialisation. */
static struct caml_unloadable_unit *units_head = NULL;
static caml_plat_mutex units_mutex = CAML_PLAT_MUTEX_INITIALIZER;

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

/* Normalize a static block's header color bits to the current cycle's
 * allocation status. When marking has not started, the block becomes
 * UNMARKED so the next mark scan can darken it. When marking is in
 * progress (caml_marking_started()), the block becomes MARKED — i.e.
 * "born alive in this cycle". This matches the convention applied by the
 * shared-heap allocator (caml_allocation_status) and is required for
 * correctness: the curry-stub closure (or any other heap block) created
 * after this unit is registered may be allocated MARKED if marking is in
 * progress, in which case the marker will not scan its fields. If our
 * static block were UNMARKED in such a cycle, no path would darken it
 * (the curry stub holding the only pointer is allocated MARKED and
 * therefore skipped by the mark scan). Born-marked here means the unit
 * survives the current cycle; the post-rotation cycle reverts it to
 * UNMARKED, after which standard marking takes over. */
static void normalize_block_color(value v) {
  header_t hd = Hd_val(v);
  /* Use the atomic-header store path for consistency with concurrent
   * marking; even though this call site runs before the unit is linked
   * into [units_head] (and so is not yet visible to other domains), the
   * runtime convention is that any write through the header word goes
   * via [Hp_atomic_val]. */
  atomic_store_relaxed(Hp_atomic_val(v),
                       With_status_hd(hd, caml_allocation_status()));
}

void caml_register_unloadable_unit(struct caml_unloadable_unit *u) {
  CAMLassert(u != NULL);

  /* Patch block headers to current UNMARKED. See [normalize_block_color]. */
  for (uintnat i = 0; i < u->num_code_blocks; i++) {
    normalize_block_color(u->code_blocks[i]);
  }
  for (uintnat i = 0; i < u->num_data_blocks; i++) {
    normalize_block_color(u->data_blocks[i]);
  }

  /* Register each text range as a code fragment so
   * [caml_find_code_fragment_by_pc] returns true for any PC in the unit's
   * text. Digests are not used for unloadable units (DIGEST_IGNORE);
   * uniqueness is by-pc rather than by-content. */
  for (uintnat i = 0; i < u->num_text_ranges; i++) {
    char *start = u->text_ranges[2 * i];
    char *end = u->text_ranges[2 * i + 1];
    u->text_range_fragnums[i] =
        caml_register_code_fragment(start, end, DIGEST_IGNORE, NULL);
  }

  /* Register the frame table so stack walks can find frame descriptors for
   * the unit's return addresses. */
  if (u->frametable != NULL) {
    void *tbl = (void *)u->frametable;
    caml_register_frametables(&tbl, 1);
  }

  /* gc_roots semantics, important to unloadability:
   * [scan_native_globals] iterates each registered glob_block and applies
   * the scanning action to its FIELDS only — not to the glob_block
   * itself. So the gc_roots scan does NOT keep the unit's static data
   * blocks alive; it only preserves heap values stored INTO those
   * blocks (e.g. a heap pointer stuck into a static ref cell). The
   * unit's static data blocks are marked by ordinary heap-side
   * [caml_darken] following Symbol references from the module block
   * and from Code_block dep fields.
   *
   * Consequence: unloadability of a CU hinges on the gc_roots scan
   * walking the block's fields, not the block itself. A future
   * "improvement" that darkens the glob_block before scanning its
   * fields would silently make every registered unit immortal. If
   * that ever changes, the unit's static data blocks would all stay
   * MARKED forever and the end-of-cycle unload pass would never
   * select them for reclamation. */
  if (u->gc_roots != NULL) {
    caml_register_dyn_globals(&u->gc_roots, 1);
  }

  /* Registration order matters:
   *   1. patch headers (born-marked normalisation)
   *   2. register code fragments       (now visible to F.2 / F.3)
   *   3. register frametable           (now visible to stack walks)
   *   4. register dyn_globals          (now visible to global-root scan)
   *   5. link into units_head          (now visible to the end-of-cycle
   *                                     unload pass — done below)
   * Steps 2-4 publish the unit to *globally visible* tables before
   * step 5 makes it visible on our own list. A major-GC cycle that
   * starts on another domain in this window will scan via gc_roots
   * and the frametable, see the unit's blocks, and mark them; the
   * end-of-cycle unload pass will not visit this unit (it is not
   * yet on units_head) but that is correct — it has just been
   * registered as born-marked. */
  caml_plat_lock_blocking(&units_mutex);
  u->next = units_head;
  units_head = u;
  units_registered_total++;
  uintnat seq = units_registered_total;
  caml_plat_unlock(&units_mutex);

  atomic_fetch_add(&caml_unloadable_units_live_count, 1);

  if (unloadable_debug()) {
    fprintf(stderr,
        "[unloadable] register #%lu: unit=%p code_blocks=%lu data_blocks=%lu "
        "text_ranges=%lu gc_roots=%p frametable=%p\n",
        (unsigned long)seq, (void *)u, (unsigned long)u->num_code_blocks,
        (unsigned long)u->num_data_blocks, (unsigned long)u->num_text_ranges,
        u->gc_roots, (void *)u->frametable);
  }
}

void caml_iter_unloadable_units(
    void (*f)(struct caml_unloadable_unit *, void *), void *user_data) {
  caml_plat_lock_blocking(&units_mutex);
  for (struct caml_unloadable_unit *u = units_head; u != NULL; u = u->next) {
    f(u, user_data);
  }
  caml_plat_unlock(&units_mutex);
}

void caml_unloadable_check_and_unload_dead(void) {
  /* The caller (cycle_major_heap_from_stw_single) holds the STW barrier and
   * has not yet rotated [caml_global_heap_state]; MARKED bits below are the
   * cycle-N values, which the imminent rotation will reinterpret as cycle-
   * N+1's UNMARKED. */
  status marked = caml_global_heap_state.MARKED;

  caml_plat_lock_blocking(&units_mutex);

  struct caml_unloadable_unit *to_unload = NULL;
  struct caml_unloadable_unit **link = &units_head;
  while (*link != NULL) {
    struct caml_unloadable_unit *u = *link;
    int live = 0;
    for (uintnat i = 0; i < u->num_code_blocks && !live; i++) {
      if (Has_status_val(u->code_blocks[i], marked)) live = 1;
    }
    for (uintnat i = 0; i < u->num_data_blocks && !live; i++) {
      if (Has_status_val(u->data_blocks[i], marked)) live = 1;
    }
    if (unloadable_debug()) {
      fprintf(stderr, "[unloadable] check unit=%p live=%d\n",
              (void*)u, live);
    }
    if (!live) {
      *link = u->next;
      u->next = to_unload;
      to_unload = u;
      units_unloaded_total++;
      atomic_fetch_sub(&caml_unloadable_units_live_count, 1);
    } else {
      /* Survives: rewrite all blocks to MARKED bits so the imminent
       * rotation maps them uniformly to UNMARKED in cycle N+1. Blocks
       * that were already MARKED need no update; this is the simplest
       * uniform write. */
      for (uintnat i = 0; i < u->num_code_blocks; i++) {
        value v = u->code_blocks[i];
        header_t hd = Hd_val(v);
        *Hp_val(v) = With_status_hd(hd, marked);
      }
      for (uintnat i = 0; i < u->num_data_blocks; i++) {
        value v = u->data_blocks[i];
        header_t hd = Hd_val(v);
        *Hp_val(v) = With_status_hd(hd, marked);
      }
      link = &u->next;
    }
  }

  caml_plat_unlock(&units_mutex);

  /* Outside the lock so [caml_remove_code_fragment] (which uses its own
   * skiplist mutexes) and the loader callback do not nest. */
  while (to_unload != NULL) {
    struct caml_unloadable_unit *u = to_unload;
    to_unload = u->next;
    if (unloadable_debug()) {
      fprintf(stderr,
          "[unloadable] unload: unit=%p code_blocks=%lu data_blocks=%lu "
          "text_ranges=%lu\n",
          (void *)u, (unsigned long)u->num_code_blocks,
          (unsigned long)u->num_data_blocks,
          (unsigned long)u->num_text_ranges);
    }
    for (uintnat i = 0; i < u->num_text_ranges; i++) {
      struct code_fragment *cf =
          caml_find_code_fragment_by_num(u->text_range_fragnums[i]);
      if (cf != NULL) caml_remove_code_fragment(cf);
    }
    /* Unregister the unit's frame table. We're inside STW (called from
     * [cycle_major_heap_from_stw_single]) so the dedicated single-domain
     * variant is appropriate. */
    if (u->frametable != NULL) {
      caml_unregister_frametable_from_stw_single(u->frametable);
    }
    if (u->gc_roots != NULL) {
      caml_unregister_dyn_global(u->gc_roots);
    }
    if (u->on_unload != NULL) u->on_unload(u);
  }
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

struct caml_unloadable_unit *caml_find_unloadable_unit_by_pc(char *pc) {
  caml_plat_lock_blocking(&units_mutex);
  struct caml_unloadable_unit *result = NULL;
  for (struct caml_unloadable_unit *u = units_head; u != NULL; u = u->next) {
    for (uintnat i = 0; i < u->num_text_ranges; i++) {
      char *start = u->text_ranges[2 * i];
      char *end = u->text_ranges[2 * i + 1];
      if (start <= pc && pc < end) {
        result = u;
        goto done;
      }
    }
  }
done:
  caml_plat_unlock(&units_mutex);
  return result;
}
