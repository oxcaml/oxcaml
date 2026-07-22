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

/* Runtime registration for unloadable compilation units. An unloadable unit
 * is a JIT-emitted (or Dynlink-loaded with opt-in) compilation unit whose
 * code and static data may be reclaimed by the GC when unreferenced.
 *
 * The unit's static data blocks (including the per-function [Code_block]s)
 * occupy one contiguous region which, once the unit's initialiser has run,
 * is donated to the major heap as a heap extent
 * ([caml_add_blocks_to_heap]). From that point the GC marks and sweeps
 * those blocks like any other major-heap blocks; when every block in the
 * extent has died, the extent's free callback fires and the unit is
 * unloaded (code fragments removed, frame table unregistered, buffers
 * freed via the loader's [on_unload] callback).
 *
 * The unit's code is kept alive through the [Code_block] dependency graph:
 *   - Every function in the unit has a [Code_block] (tag [Code_block_tag])
 *     in the extent whose fields are the function's direct dependencies
 *     (other functions' [Code_block]s plus same-CU static data blocks).
 *   - The word immediately before each function entry in [.text] holds the
 *     address of that function's [Code_block] (the "back-pointer").
 *   - The mark phase darkens [Code_block]s via three paths: the closure
 *     function-slot walk (closinfo unloadable bit), the stack
 *     return-address scan (FRAME_DESCRIPTOR_UNLOADABLE), and the stack
 *     code-pointer-slot scan (FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS).
 *
 * Lifecycle:
 *   1. [caml_register_unloadable_unit] (before the unit's initialiser
 *      runs): registers code fragments and the frame table, and registers
 *      the unit's [gc_roots] as dyn-globals. At this stage the unit's
 *      static blocks still have their NOT_MARKABLE emission headers, so
 *      the GC ignores them entirely, exactly as for AOT static data; the
 *      dyn-globals scan keeps heap values stored into them alive.
 *   2. [caml_activate_unloadable_unit] (after the initialiser has
 *      returned, or raised): unregisters the dyn-globals and donates the
 *      static-block region to the major heap as an extent. The extent
 *      machinery forces every block to the current allocation colour, so
 *      the unit survives the in-progress major cycle by construction.
 *   3. When the GC finds the whole extent dead, the free callback runs
 *      (from sweeping, with the usual finaliser-like restrictions): the
 *      unit is unlinked from the registration list, the observability
 *      counters are updated, and the unit is queued for unloading.
 *   4. At the start of the next major cycle (under the STW barrier,
 *      [caml_unloadable_process_pending_unloads]) the queued units'
 *      code fragments are removed, their frame tables unregistered, and
 *      the loader's [on_unload] callback frees the underlying buffers. */

#ifndef CAML_UNLOADABLE_H
#define CAML_UNLOADABLE_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"

struct caml_unloadable_unit {
  struct caml_unloadable_unit *next;

  /* Contiguous region holding every static data block of the unit
   * (including [Code_block]s), delimited by the CU's
   * [unloadable_blocks_start] / [unloadable_blocks_end] symbols. Donated
   * to the major heap by [caml_activate_unloadable_unit]. */
  void *blocks_base;
  size_t blocks_size; /* in bytes */

  /* (text_start, text_end) pairs covering the unit's [.text] regions. Each
   * range is registered with the runtime code-fragment table; the fragnums
   * are stored here so the unload path can remove them. */
  char **text_ranges; /* Flat: [s0, e0, s1, e1, ...]; length = 2 * num_text_ranges. */
  int *text_range_fragnums;
  uintnat num_text_ranges;

  /* The unit's frame table (intnat[1+num]; first slot = entry count),
   * inside the unit's buffer, or NULL if none. Registered directly with
   * [caml_register_frametables] at registration time — it cannot be
   * copied, because frame descriptors encode their return addresses as
   * 32-bit self-relative offsets. Unregistered under the STW barrier
   * (via [caml_unregister_frametable_from_stw_single]) immediately before
   * the buffer is freed. */
  intnat *frametable;

  /* Dyn-globals (gc_roots) pointer, registered at registration time and
   * removed again by [caml_activate_unloadable_unit]. Needed only while
   * the unit's initialiser runs: during that window the unit's static
   * blocks are not yet part of the heap, so heap values stored into them
   * are kept alive by the dyn-globals field scan (as for AOT data). May
   * be NULL. */
  void *gc_roots;

  /* Optional callback invoked once the GC has reclaimed the unit's extent
   * and its runtime registrations have been removed. The loader uses this
   * to free the text/data buffer and the unit structure itself. Runs under
   * the STW barrier (from [caml_unloadable_process_pending_unloads]); must
   * not allocate on the OCaml heap or call back into OCaml. */
  void (*on_unload)(struct caml_unloadable_unit *);

  /* Loader-private data (e.g. buffer base addresses). The runtime never
   * inspects this field. */
  void *loader_data;
};

/* Register an unloadable compilation unit with the runtime. Called by the
 * loader (e.g. ocaml-jit) after the unit's text and data buffers have been
 * mapped and relocations applied, but BEFORE the unit's initialiser runs.
 *
 * The runtime takes ownership of the [unit] structure and links it into the
 * global registration list.
 *
 * Side effects:
 *   - Registers each [text_ranges[i]] pair as a code fragment, tagging it
 *     with [owner_unloadable_unit] so the stack scans can identify
 *     unloadable PCs.
 *   - Registers [frametable] via [caml_register_frametables].
 *   - Registers [gc_roots] as dyn-globals (if non-NULL). */
CAMLextern void caml_register_unloadable_unit(struct caml_unloadable_unit *u);

/* Activate an unloadable unit: unregister its [gc_roots] dyn-globals and
 * donate its static-block region to the major heap as a heap extent.
 * Must be called exactly once, after the unit's initialiser has finished
 * (normally or with an exception). From this point the GC may reclaim the
 * unit once no references to its code or data remain.
 *
 * The two steps happen without any intervening GC safe point (plain C
 * code), which matters: between dropping the roots and donating the
 * blocks, heap values referenced only from the unit's static data would
 * otherwise be unprotected. */
CAMLextern void caml_activate_unloadable_unit(struct caml_unloadable_unit *u);

/* Drain the queue of units whose extents the GC has reclaimed: remove
 * their code fragments, unregister their frame tables, and invoke their
 * [on_unload] callbacks (which free the underlying buffers). Called by
 * [major_gc.c] from the STW single-domain portion of
 * [cycle_major_heap_from_stw_single]; must be called under the STW
 * barrier because frame-table removal mutates the global descriptor
 * hashtable in place. */
void caml_unloadable_process_pending_unloads(void);

/* Cumulative counters since process start. [registered] counts every
 * successful [caml_register_unloadable_unit] call; [unloaded] counts every
 * unit reclaimed by the GC. The number of currently-live units equals the
 * difference. Used by tests to confirm unloading is firing. */
CAMLextern uintnat caml_unloadable_units_registered_total(void);
CAMLextern uintnat caml_unloadable_units_unloaded_total(void);

/* Live-unit counter: registered minus unloaded. Read with relaxed atomics
 * from the major mark path to short-circuit
 * [caml_darken_unloadable_code_blocks_in_closure] when there are no
 * unloadable units. Defined in [unloadable.c]. */
CAMLextern atomic_uintnat caml_unloadable_units_live_count;

/* Darken the [Code_block] associated with an unloadable function entry. The
 * back-pointer convention places the [Code_block] address in the word
 * immediately before the entry [PC]. This helper reads that word and feeds
 * it to [caml_darken], so the standard mark scan recursively darkens the
 * Code_block's dep code-blocks and dep data blocks. Used by the closure
 * scan (F.1) where we know we are in the mark phase.
 *
 * Before the unit is activated its blocks are NOT_MARKABLE, so this is a
 * harmless no-op in that window.
 *
 * Callers are responsible for ensuring [entry] does point into unloadable
 * code (closinfo bit / FRAME_DESCRIPTOR_UNLOADABLE / code-fragment lookup);
 * the back-pointer word is undefined for non-unloadable entries. */
#include "major_gc.h"
Caml_inline void caml_darken_code_block_for_entry(void *state, value entry) {
  value code_block = *((value *)entry - 1);
  caml_darken(state, code_block, NULL);
}

/* Walk every function-slot in a closure and darken the [Code_block] of any
 * function whose closinfo has the unloadable bit set. Handles both
 * single- and multi-function closures.
 *
 * Closure prefix layout (per [Slot_offsets.Layout]):
 *   [F_0] ([infix] [F_k])* (...env...)
 * where each function slot [F_k] is two words (size 2: code, closinfo) for
 * "Full_application_only" or three words (size 3: curry_ptr, closinfo, code)
 * for "Full_and_partial_application". The closinfo is always at slot offset
 * 1; the actual function entry is at slot offset 0 (size 2) or slot offset 2
 * (size 3). The curry_ptr in a size-3 slot points to a non-unloadable
 * runtime stub and never needs back-pointer darkening.
 *
 * Slot size is decided by [Flambda 2]'s [closure_code_pointers]:
 * [Full_application_only] (size 2) for [Curried] with 0 or 1 param;
 * [Full_and_partial_application] (size 3) otherwise (curried with >= 2
 * params, or tupled). The closinfo arity field encodes this:
 * [closure_info'] writes [List.length params] for curried (a non-negative
 * int), and [-List.length params] for tupled (negative). We read
 * [Arity_closinfo] (signed) to disambiguate: arity in [{0, 1}] => size 2;
 * arity > 1 or arity < 0 => size 3. We cannot rely on a following infix
 * header because a single-function closure with a non-scannable env (e.g.
 * a captured int) has no infix header after [F_0], yet has
 * [env_start - slot_start > 2] because the non-scannable env words are
 * part of the prefix. */
Caml_inline void caml_darken_unloadable_code_blocks_in_closure(
    void *state, value closure) {
  /* Fast path: if no unloadable units are currently registered, no closure
   * can contain an unloadable function slot, so skip the prefix walk
   * entirely. Relaxed load is sufficient: the registration paths use
   * release semantics implicitly via the units_mutex, and a stale 0 here
   * is impossible because mutator code in a unit only runs after its
   * registration is complete. */
  if (atomic_load_explicit(&caml_unloadable_units_live_count,
                           memory_order_relaxed) == 0) {
    return;
  }
  value closinfo_0 = Closinfo_val(closure);
  uintnat env_start = Start_env_closinfo(closinfo_0);
  uintnat slot_start = 0;
  while (slot_start + 2 <= env_start) {
    value closinfo = Field(closure, slot_start + 1);
    intnat arity = Arity_closinfo(closinfo);
    uintnat slot_size = (arity > 1 || arity < 0) ? 3 : 2;
    if (slot_start + slot_size > env_start) break;
    if (Unloadable_closinfo(closinfo)) {
      uintnat code_offset =
        (slot_size == 2) ? slot_start : slot_start + 2;
      caml_darken_code_block_for_entry(state, Field(closure, code_offset));
    }
    if (Is_last_closinfo(closinfo)) break;
    /* In a multi-function closure, [Slot_offsets.Layout] emits an
       [Infix_tag] header between adjacent function slots. The word at
       [slot_start + slot_size] is that header; assert this in DEBUG so a
       layout change does not silently miscount. */
    CAMLassert(
      Tag_hd((header_t)Field(closure, slot_start + slot_size)) == Infix_tag);
    slot_start += slot_size + 1; /* skip past the infix header */
  }
}

/* Like [caml_darken_code_block_for_entry], but invokes a generic
 * [scanning_action]. Used by F.2 / F.3 in the stack-walker, which is shared
 * across mark, oldify, and compactor scans. For non-mark scans the action's
 * effect on a [Code_block] is benign (oldify is a no-op for non-young
 * values; the compactor does not move extent blocks).
 *
 * The slot pointer is to a local because [oldify_one] writes [*p = v]
 * unconditionally for non-young values; passing NULL would crash a minor
 * GC that walks an unloadable frame. */
#include "roots.h"
Caml_inline void caml_visit_code_block_for_entry(
    scanning_action f, void *fdata, value entry) {
  value code_block = *((value *)entry - 1);
  f(fdata, code_block, &code_block);
}

/* Walk the parallel [code_ptr_live_ofs] array of a frame descriptor (F.3),
 * darkening the Code_block of each slot whose target is in unloadable code.
 * Caller must have verified [frame_has_code_ptr_slots(d)]. Slot encoding
 * matches [live_ofs[]]: bit 0 set means register index, clear means stack
 * offset.
 *
 * Defined inline here so callers (fiber.c, signals_nat.c) avoid duplicating
 * the offset-decoding logic; the function takes [scanning_action] so it
 * works in mark, oldify, and compactor scans uniformly. */
#include "frame_descriptors.h"
#include "codefrag.h"
/* Correctness depends on every value that ever reaches a
 * [Code_pointer]-typed slot being a function-entry PC: only then is
 * [*((value*)cp - 1)] the back-pointer to the unit's [Code_block]. The
 * compiler enforces this only by convention — [Code_pointer] machtype is
 * not arithmetic-safe and the only in-tree producers are closure Field-0
 * loads and static [Csymbol_address] references of entry symbols. The
 * DEBUG assertion below catches any future divergence: if a non-entry
 * PC ever reaches a [Code_pointer] slot, [cf->code_start != cp] and the
 * assertion fires before we dereference [cp - 1]. */
Caml_inline void caml_visit_frame_code_ptr_slots(
    scanning_action f, void *fdata, frame_descr *d, char *sp, value *regs) {
  unsigned char *p = frame_end_of_live_ofs(d);
  uint8_t num_allocs = 0;
  if (frame_has_allocs(d)) {
    num_allocs = *p;
    p += num_allocs + 1;
  }
  if (frame_has_debug(d)) {
    p = Align_to(p, uint32_t);
    p += sizeof(uint32_t) * (frame_has_allocs(d) ? num_allocs : 1);
  }
  if (frame_is_long(d)) {
    p = Align_to(p, uint32_t);
    uint32_t n = *(uint32_t *)p;
    uint32_t *q = (uint32_t *)p + 1;
    for (uint32_t k = 0; k < n; k++) {
      uint32_t ofs = q[k];
      value cp = (ofs & 1) ? regs[ofs >> 1] : *(value *)(sp + ofs);
      /* Single O(log n) check: the code-fragment skiplist returns the
       * fragment (if any) for this PC, and the fragment's
       * [owner_unloadable_unit] field tells us whether it belongs to an
       * unloadable CU (non-NULL) or to non-unloadable registered code
       * (NULL — main program, Dynlink). Dereferencing the [entry - 1]
       * back-pointer word is only valid for unloadable entries, so the
       * owner check is the authoritative gate. */
      struct code_fragment *cf = caml_find_code_fragment_by_pc((char *)cp);
      if (cf != NULL && cf->owner_unloadable_unit != NULL) {
        CAMLassert(cf->code_start == (char *)cp);
        caml_visit_code_block_for_entry(f, fdata, cp);
      }
    }
  } else {
    p = Align_to(p, uint16_t);
    uint16_t n = *(uint16_t *)p;
    uint16_t *q = (uint16_t *)p + 1;
    for (uint16_t k = 0; k < n; k++) {
      uint16_t ofs = q[k];
      value cp = (ofs & 1) ? regs[ofs >> 1] : *(value *)(sp + ofs);
      /* See long-format case above. */
      struct code_fragment *cf = caml_find_code_fragment_by_pc((char *)cp);
      if (cf != NULL && cf->owner_unloadable_unit != NULL) {
        CAMLassert(cf->code_start == (char *)cp);
        caml_visit_code_block_for_entry(f, fdata, cp);
      }
    }
  }
}

#endif /* CAML_INTERNALS */

#endif /* CAML_UNLOADABLE_H */
