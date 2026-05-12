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
 * code and static data may be reclaimed by the GC at end of major cycle when
 * unreferenced.
 *
 * Each unit holds:
 *   - A list of [Code_block] addresses (one per function defined in the
 *     unit), each a heap-shaped block tagged [Code_block_tag] holding
 *     pointers to dependency [Code_block]s and data blocks.
 *   - A list of static-data block addresses (white-headed; unmarked at
 *     emission time per B.1).
 *   - A list of (text_start, text_end) ranges covering the JIT-emitted code
 *     for the unit. Each range is registered with the standard code-fragment
 *     table so [caml_find_code_fragment_by_pc] returns the fragment when a
 *     return address lands in unloadable code.
 *   - A pointer to the unit's frame table.
 *
 * The runtime structure owns no per-symbol mark bits and no dependency
 * arrays — those live in the heap-shaped [Code_block]s themselves. */

#ifndef CAML_UNLOADABLE_H
#define CAML_UNLOADABLE_H

#ifdef CAML_INTERNALS

#include "config.h"
#include "mlvalues.h"

struct caml_unloadable_unit {
  struct caml_unloadable_unit *next;

  /* Code_block heap-shaped objects (tag = Code_block_tag) for each function
   * defined in the unit. The mark phase darkens these via the closure /
   * frame back-pointer paths (F.1 / F.2). */
  value *code_blocks;
  uintnat num_code_blocks;

  /* Static-data block addresses for the unit. Headers are unmarked at
   * emission time (B.1) so the standard mark scan can darken them when
   * referenced; at end of cycle, surviving blocks are reset back to
   * UNMARKED. */
  value *data_blocks;
  uintnat num_data_blocks;

  /* (text_start, text_end) pairs covering the unit's [.text] regions. Each
   * range is registered with the runtime code-fragment table; the fragnums
   * are stored here so the end-of-major-cycle unload pass can remove
   * them during unload. */
  char **text_ranges; /* Flat: [s0, e0, s1, e1, ...]; length = 2 * num_text_ranges. */
  int *text_range_fragnums;
  uintnat num_text_ranges;

  /* Frame table pointer (intnat[1+num]; first slot = entry count). The frame
   * table is registered with [caml_register_frametables] at registration
   * time. */
  intnat *frametable;

  /* Dyn-globals (gc_roots) pointer. Address of the unit's null-terminated
   * [gc_roots] array; the major GC's global-root scan walks each entry to
   * mark the unit's static blocks (per B.1 they have white headers, so this
   * scan is the path that marks them when they are reachable). Registered
   * with [caml_register_dyn_globals] at unit registration; removed via
   * [caml_unregister_dyn_global] on unload. May be [NULL] if the unit has
   * no gc_roots table (degenerate case). */
  void *gc_roots;

  /* Optional callback invoked from STW when the unit has been determined
   * unreachable and its registration has been removed. The loader uses this
   * to [munmap] the text and data buffers and free the unit structure
   * itself. May be [NULL] for transient test-only registrations. */
  void (*on_unload)(struct caml_unloadable_unit *);

  /* Loader-private data (e.g. mmap base addresses, allocator handles). The
   * runtime never inspects this field. */
  void *loader_data;
};

/* Register an unloadable compilation unit with the runtime. Called by the
 * loader (e.g. ocaml-jit) after the unit's text and data buffers have been
 * mapped and relocations applied.
 *
 * The runtime takes ownership of the [unit] structure and links it into the
 * global registration list. The arrays referenced by the structure must
 * remain valid until the end-of-major-cycle unload pass
 * ([caml_unloadable_check_and_unload_dead]) reclaims the unit and calls its
 * [on_unload] callback.
 *
 * Side effects:
 *   - Registers each [text_ranges[i]] pair as a code fragment.
 *   - Registers [frametable] via [caml_register_frametables].
 *
 * Must be called from a non-GC context (or holding the appropriate mutex);
 * the runtime takes the unloadable-units lock internally. */
CAMLextern void caml_register_unloadable_unit(struct caml_unloadable_unit *u);

/* Iterate over all currently-registered unloadable units, calling [f] on
 * each. Used by the end-of-major-cycle pass (G) to find units that have
 * become unreachable and to reset surviving units' mark bits.
 *
 * Must be called from a stop-the-world section. */
void caml_iter_unloadable_units(
    void (*f)(struct caml_unloadable_unit *, void *), void *user_data);

/* End-of-major-cycle hook (G). Called by [major_gc.c] from the STW
 * single-leader portion of [cycle_major_heap_from_stw_single], BEFORE
 * [caml_cycle_heap_from_stw_single] rotates the global heap state.
 *
 * For each registered unloadable unit:
 *   - If no Code_block or data block in the unit has the current
 *     [caml_global_heap_state.MARKED] bits, the unit is unreachable. It is
 *     unlinked from the registration list, its code fragments are removed,
 *     and its [on_unload] callback (if any) is invoked. The loader is
 *     responsible for [munmap]ping the buffers in [on_unload].
 *   - Otherwise, the unit's blocks are uniformly rewritten to MARKED bits
 *     so that the rotation maps them to UNMARKED in the new cycle. This
 *     keeps surviving units' headers consistent with subsequent cycles.
 *
 * Caller must be in STW; this function takes the unloadable-units lock
 * internally. */
void caml_unloadable_check_and_unload_dead(void);

/* Cumulative counters since process start. [registered] counts every
 * successful [caml_register_unloadable_unit] call; [unloaded] counts every
 * unit unlinked from the registration list by [caml_unloadable_check_and_
 * unload_dead]. The number of currently-live units equals the difference.
 * Used by tests to confirm unloading is firing. */
CAMLextern uintnat caml_unloadable_units_registered_total(void);
CAMLextern uintnat caml_unloadable_units_unloaded_total(void);

/* Live-unit counter: registered minus unloaded. Read with relaxed atomics
 * from the major mark path to short-circuit
 * [caml_darken_unloadable_code_blocks_in_closure] when there are no
 * unloadable units. Defined in [unloadable.c]. */
CAMLextern atomic_uintnat caml_unloadable_units_live_count;

/* Look up the unloadable unit (if any) whose [.text] range contains [pc].
 * Returns NULL when [pc] lies in non-unloadable code. Used by F.2 (stack
 * return-address scan) and F.3 (stack code-pointer slot scan).
 *
 * Reasonably fast: walks the registered units list. The runtime
 * code-fragment table is the primary lookup; this helper exists for the
 * additional unit-level information.
 *
 * Note: callers may prefer [caml_find_code_fragment_by_pc] when they need
 * only to know whether [pc] is in unloadable code (cheap negative answer);
 * use this when the unit pointer itself is needed. */
/* REVIEW(codex): [caml_find_code_fragment_by_pc] is not a reliable "unloadable code"
 * predicate as written: the main program's .text is also registered as a code
 * fragment (startup_nat.c), and Dynlink/meta fragments may be too. Any use of
 * code-fragment presence to gate dereferencing the [entry - 1] back-pointer
 * must additionally establish that [pc] lies in an unloadable unit to avoid
 * reading arbitrary words from non-unloadable code. For performance, consider
 * making this check O(log n) by tagging code fragments as unloadable (or
 * pointing at their owning unit) rather than doing an O(units) scan. */
struct caml_unloadable_unit *caml_find_unloadable_unit_by_pc(char *pc);

/* Darken the [Code_block] associated with an unloadable function entry. The
 * back-pointer convention (D) places the [Code_block] address in the word
 * immediately before the entry [PC]. This helper reads that word and feeds
 * it to [caml_darken], so the standard mark scan recursively darkens the
 * Code_block's dep code-blocks and dep data blocks. Used by F.1 (closure
 * scan) where we know we are in the mark phase.
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
 * effect on a static [Code_block] is benign (oldify is a no-op for
 * out-of-heap blocks; the compactor does not move static data).
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
      /* The code fragment table contains non-unloadable code too (main
       * program, Dynlink). Dereferencing the [entry - 1] back-pointer word
       * is only valid for unloadable entries, so require membership in an
       * unloadable unit. */
      struct code_fragment *cf = caml_find_code_fragment_by_pc((char *)cp);
      if (cf != NULL
          && caml_find_unloadable_unit_by_pc((char *)cp) != NULL) {
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
      if (cf != NULL
          && caml_find_unloadable_unit_by_pc((char *)cp) != NULL) {
        CAMLassert(cf->code_start == (char *)cp);
        caml_visit_code_block_for_entry(f, fdata, cp);
      }
    }
  }
}

#endif /* CAML_INTERNALS */

#endif /* CAML_UNLOADABLE_H */
