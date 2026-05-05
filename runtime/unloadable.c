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
 * mutex (and runs from a stop-the-world section). */
static struct caml_unloadable_unit *units_head = NULL;
static caml_plat_mutex units_mutex;
static int units_mutex_initialized = 0;

/* Cumulative counters for observability. Updated under [units_mutex]. */
static uintnat units_registered_total = 0;
static uintnat units_unloaded_total = 0;

static void ensure_mutex_initialized(void) {
  /* Lazy initialization: the runtime may register a unit before any
   * explicit [caml_init_unloadable] hook runs. The first registration
   * initializes; subsequent calls are no-ops. */
  if (!units_mutex_initialized) {
    /* REVIEW(codex): This is not thread-safe: concurrent registrations can race and
       initialize the mutex multiple times (or observe a half-initialized
       mutex). Prefer a static initializer (CAML_PLAT_MUTEX_INITIALIZER) or
       an atomic once-style init. */
    caml_plat_mutex_init(&units_mutex);
    units_mutex_initialized = 1;
  }
}

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
  /* REVIEW(codex): This writes the header non-atomically. During concurrent marking,
     other domains update/read headers via [Hp_atomic_val]; consider using
     [atomic_store_relaxed(Hp_atomic_val(v), ...)] for consistency. */
  *Hp_val(v) = With_status_hd(hd, caml_allocation_status());
}

void caml_register_unloadable_unit(struct caml_unloadable_unit *u) {
  CAMLassert(u != NULL);
  ensure_mutex_initialized();

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

  /* Register the unit's gc_roots so the major GC's global-root scan walks
   * the unit's static blocks. The runtime would otherwise leave them
   * unmarked (they are emitted with white headers per B.1). */
  if (u->gc_roots != NULL) {
    /* REVIEW(codex): Registering [gc_roots] here means global-root scanning will walk
       the unit every major cycle, regardless of whether any heap root reaches
       the unit. Please confirm this cannot keep an otherwise-dead unit live
       (e.g. via the module block pointing at other markable unit blocks), and
       that scanning via reachability (closures/stack/code_ptr slots) would be
       insufficient. */
    caml_register_dyn_globals(&u->gc_roots, 1);
  }

  caml_plat_lock_blocking(&units_mutex);
  u->next = units_head;
  units_head = u;
  units_registered_total++;
  uintnat seq = units_registered_total;
  caml_plat_unlock(&units_mutex);

  if (unloadable_debug()) {
    fprintf(stderr,
        "[unloadable] register #%lu: unit=%p code_blocks=%lu data_blocks=%lu "
        "text_ranges=%lu gc_roots=%p frametable=%p\n",
        (unsigned long)seq, (void *)u, (unsigned long)u->num_code_blocks,
        (unsigned long)u->num_data_blocks, (unsigned long)u->num_text_ranges,
        u->gc_roots, (void *)u->frametable);
  }
}

void caml_unregister_unloadable_unit(struct caml_unloadable_unit *u) {
  CAMLassert(u != NULL);
  ensure_mutex_initialized();

  caml_plat_lock_blocking(&units_mutex);
  struct caml_unloadable_unit **link = &units_head;
  while (*link != NULL && *link != u) link = &(*link)->next;
  if (*link == u) *link = u->next;
  caml_plat_unlock(&units_mutex);

  /* Remove each text range's code fragment. The fragment objects are placed
   * on the codefrag garbage list and freed by
   * [caml_code_fragment_cleanup_from_stw_single]. */
  for (uintnat i = 0; i < u->num_text_ranges; i++) {
    struct code_fragment *cf =
        caml_find_code_fragment_by_num(u->text_range_fragnums[i]);
    if (cf != NULL) caml_remove_code_fragment(cf);
  }

  /* Frame-table removal: must be performed under STW because it mutates the
   * shared [current_frame_descrs] hashtable in place. The unload pass (G),
   * which is the primary unloader, calls
   * [caml_unregister_frametable_from_stw_single] directly. The explicit
   * [caml_unregister_unloadable_unit] entry point assumes its caller is
   * also STW-safe. */
  if (u->frametable != NULL) {
    caml_unregister_frametable_from_stw_single(u->frametable);
  }

  if (u->gc_roots != NULL) {
    caml_unregister_dyn_global(u->gc_roots);
  }
}

void caml_iter_unloadable_units(
    void (*f)(struct caml_unloadable_unit *, void *), void *user_data) {
  ensure_mutex_initialized();
  caml_plat_lock_blocking(&units_mutex);
  for (struct caml_unloadable_unit *u = units_head; u != NULL; u = u->next) {
    f(u, user_data);
  }
  caml_plat_unlock(&units_mutex);
}

void caml_unloadable_check_and_unload_dead(void) {
  ensure_mutex_initialized();

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
  ensure_mutex_initialized();
  caml_plat_lock_blocking(&units_mutex);
  uintnat r = units_registered_total;
  caml_plat_unlock(&units_mutex);
  return r;
}

uintnat caml_unloadable_units_unloaded_total(void) {
  ensure_mutex_initialized();
  caml_plat_lock_blocking(&units_mutex);
  uintnat r = units_unloaded_total;
  caml_plat_unlock(&units_mutex);
  return r;
}

struct caml_unloadable_unit *caml_find_unloadable_unit_by_pc(char *pc) {
  ensure_mutex_initialized();
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
