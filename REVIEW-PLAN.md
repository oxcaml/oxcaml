# Review resolution plan — unloading of metaprogramming compilation units

Base revision: `565b72a8b2` ("Unloading of metaprogramming compilation
units").

Inputs:

- `REVIEW-CLAUDE.md` (numbered items #1–#18 + "doc gaps")
- `REVIEW-CODEX.md`
- All inline `REVIEW(claude)` / `REVIEW(codex)` markers still in source

The base review already contains two committed fixes that stay:

- `backend/emitaux.ml` — `flags = 3` → `flags land 3 = 3`
  (REVIEW-CLAUDE bug #1, frame-descriptor parse drift under
  `-g + Eval.eval + allocations`). Commit `354b75c1`.
- `runtime/caml/unloadable.h` — F.3 code-pointer-slot gating now
  requires `caml_find_unloadable_unit_by_pc(cp) != NULL` *in addition
  to* `caml_find_code_fragment_by_pc(cp) != NULL`
  (REVIEW-CODEX correctness issue #1, F.3 dereferencing of `entry - 1`
  for non-unloadable code fragments). Commit `90055d16`.

Side effect of the F.3 fix: `caml_find_unloadable_unit_by_pc` is no
longer dead. The inline `REVIEW(claude)` note in `runtime/unloadable.c`
that calls it dead (item P.21 below) is now stale and should simply be
deleted.

This document is the punch list for the remaining inline REVIEW
markers — one entry per marker, in source-tree order. Each entry
lists:

- **Where** — file:line as the marker currently appears
- **What** — one-sentence summary
- **Disposition** — chosen action: `fix`, `assert`, `refactor`,
  `delete-comment`, `doc-only`, or `defer`
- **Notes** — context, cross-refs to REVIEW-CLAUDE / REVIEW-CODEX,
  test coverage that already exists

Severity ordering (top-of-file in case we triage):

1. Latent correctness traps that current invariants paper over —
   P.4 (LLVM silently drops `Unloadable`), P.18 (F.3 cp-1 trust),
   P.13 (closure-prefix +1 infix step), P.7 (Mach-O alias dup),
   P.16 (Make_closinfo bit aliasing).
2. Thread-safety / multi-domain — P.19 (mutex lazy init),
   P.20 (non-atomic header store), P.10 (registration window race).
3. Fragility / contracts — P.2 (file-global per-CU state),
   P.6 / P.9 (suffix-based JIT symbol detection), P.14 (cross-CU
   inlining of unloadable IR).
4. Dead / misleading API — P.15 (`caml_unregister_unloadable_unit`),
   P.21 (`caml_find_unloadable_unit_by_pc` stale comment).
5. Performance — P.22 / P.23 (closure scan on every closure).
6. Documentation gaps — P.1, P.5, P.8, P.11, P.12, P.17, P.24, P.25,
   P.26 (registration order, gc_roots semantics, frame-descriptor
   layout cross-refs, text-range / back-pointer boundary, entry
   Code_block dep fields).

---

## P.1 — `backend/amd64/proc.ml:133` (REVIEW(codex))

**What.** amd64 returns physical regs with the underlying `Int` (or
caller-passed) machtype unless the LLVM flag is set; arm64 builds an
alias reg with the requested `Code_pointer` type.

**Disposition.** doc-only (no behavioural change).

**Notes.** No path today emits a `code_ptr_live_ofs` slot for a
physical reg whose live-time machtype was lost; the asymmetry with
arm64 is real but pre-dates this PR. Action: convert the REVIEW
comment into a permanent design comment explaining why amd64 keeps
the Int-typed phys_reg, with a pointer to the LLVM branch.

---

## P.2 — `backend/cmm_helpers.ml:4280` (REVIEW(claude))

**What.** `unloadable_data_block_symbols` (per-CU accumulator) and
`suppress_unloadable_data_block_tracking` (one-deep stack) are
file-globals; `Clflags.unit_is_unloadable` is read globally. Implicit
serial-single-CU contract.

**Disposition.** assert.

**Action.** Add a one-line assertion in
`flush_unloadable_data_block_symbols` that the accumulator is empty
on entry to each CU (caller already calls it once per unit); leave
the file-globals in place. Document the contract in the existing
comment block.

---

## P.3 — `backend/emitaux.ml:88` (REVIEW(codex))

**What.** `FRAME_DESCRIPTOR_FLAGS = 0xF` consumes 4 low bits; emitter
only asserts `frame_size land 3 = 0`.

**Disposition.** assert.

**Action.** Tighten the assertion at the top of
`record_frame_descr` to `assert (frame_size land 0xF = 0)` and
mirror the invariant in a comment near `FRAME_DESCRIPTOR_FLAGS`.
This pairs with P.24 below.

**Test.** Existing `unload_frametable_layout_sanity.ml` would catch
runtime misalignment but not the emitter-side invariant; a
unit-style assertion is cheapest.

---

## P.4 — `backend/llvm/llvmize.ml:1475` (REVIEW(claude))

**What.** `Cfg.Unloadable` is matched in `fun_attrs` alongside other
ignored codegen options and silently returns `[]`. Front-end has
already set the closinfo unloadable bit and `fun_unloadable`. If LLVM
is ever selected for an unloadable CU, F.1 will dereference garbage
at `entry - 1` in LLVM-compiled text → UB at next major GC.

**Disposition.** fix (defensive `Misc.fatal_error`).

**Action.** Split `Unloadable` out of the catch-all and call
`Misc.fatal_error "LLVM backend does not implement unloadable
codegen"`. The `Eval.eval` path never selects LLVM today so this is
defensive — but cheap, and turns a silent UB into a clear error if
the combination ever appears.

---

## P.5 — `external/ocaml-jit/lib/jit.ml:209` (REVIEW(claude))

**What.** Code_block discovery walks the JIT symbol table looking for
the `_code_block` suffix. Any user function whose linkage name ends
in `_code_block` would be misclassified.

**Disposition.** refactor.

**Action.** Have `to_cmm` emit a sentinel symbol — e.g.
`<unit>__unloadable_code_blocks` — whose data is the address list of
the unit's `Code_block` symbols (parallel to the existing
`<unit>__unloadable_data_blocks` table). The JIT loader then reads
the sentinel and stops doing name-pattern matching. Same change
addresses P.9 (`__unloadable_data_blocks` suffix walk) and reduces
the three full passes over the symbol table to one.

**Notes.** Cross-references REVIEW-CLAUDE #5.

---

## P.6 — `external/ocaml-jit/lib/jit.ml:240` (REVIEW(claude))

**What.** After `Array.sort Nativeint.compare entries`, `entries[i]`
no longer corresponds to `code_blocks[i]`.

**Disposition.** delete-comment + rename.

**Action.** Rename `entries` to `entries_sorted` at the binding site
and document that index-correspondence with `code_blocks` is
intentionally dropped (the C side only needs sorted text ranges).

---

## P.7 — `external/ocaml-jit/lib/jit.ml:247` (REVIEW(codex))

**What.** Mach-O may expose both a global and a local alias symbol
for the same function entry → duplicate `entries` (and possibly
`code_blocks`).

**Disposition.** fix.

**Action.** After the sort, deduplicate `entries_sorted` by address
and assert strict monotonic increase. If duplicates ever appear in
`code_blocks_rev`, drop them too (they would correspond to the same
back-pointer slot).

**Notes.** Latent on Mach-O; deduping is cheap.

---

## P.8 — `external/ocaml-jit/lib/jit.ml:354` (REVIEW(claude))

**What.** Three full passes over the JIT symbol table per load (one
for `unloadable_metadata`, one for the data-blocks fold, plus the
unit's own entry-point lookup).

**Disposition.** subsumed by P.5.

**Action.** Sentinel-symbol approach in P.5 produces both pieces of
metadata in O(1) lookups; remove this comment when P.5 lands.

---

## P.9 — same as P.5 (`__unloadable_data_blocks` suffix walk).

Action covered by P.5.

---

## P.10 — `external/ocaml-jit/lib/jit.ml:394` (REVIEW(claude))

**What.** Window between `Externals.register_unloadable_unit`
(publishes text range / frametable / dyn_globals) and `jit_run`
(invokes entry). A major GC on another domain in this window could
visit stale state.

**Disposition.** doc-only.

**Action.** Add a paragraph in the design doc + an in-source comment
arguing why this is safe: born-marked normalisation makes all
freshly-registered blocks survive the current cycle regardless of
mark order; the chained `gc_roots` scan operates on glob_block
*fields* (see P.17), and no field reachable via the unit's gc_roots
holds the entry closure until after `jit_run` writes the module
block.

**Notes.** REVIEW-CLAUDE #18. A targeted multi-domain test would be
valuable but is non-deterministic to write; the safety argument is
the deliverable.

---

## P.11 — `external/ocaml-jit/lib/jit_stubs.c:404` (REVIEW(claude))

**What.** `mprotect(buffer_base, buffer_size, ...)` where
`buffer_size` is the raw byte count summed by `alloc_all`, not
necessarily page-aligned. Linux rounds length up to a page; protect
may extend past the buffer.

**Disposition.** fix.

**Action.** When storing `buffer_size` in `jit_unit_loader_data`,
round up to `sysconf(_SC_PAGESIZE)` (or whatever
`aligned_alloc` was given as alignment, which is already the page
size). Single arithmetic change at the store site.

---

## P.12 — `external/ocaml-jit/lib/jit_stubs.c:485` (REVIEW(claude))

**What.** Five sequential `caml_stat_alloc_noexc` calls each
`caml_raise_out_of_memory()` on failure, longjmping out and leaking
everything allocated above.

**Disposition.** fix (low-impact but pattern-fix worth doing).

**Action.** Replace the cascade with a single allocation of
`(n_code + n_funcs * 4 + ...) * sizeof(value)` and slice it. Free
on the single failure path. If a single allocation is awkward
because of alignment requirements, fall back to a cleanup list of
already-allocated pointers and free them all before raising.

---

## P.13 — `external/ocaml-jit/lib/jit_stubs.c:509` (REVIEW(claude))

**What.** Each text range starts at the function entry, not at
`entry - 1`. PC-based fragment lookup for `entry - 1` misses; this
is deliberate but non-obvious.

**Disposition.** doc-only.

**Action.** Convert the REVIEW comment into a permanent comment at
the same site stating the invariant ("back-pointer at `entry - 1` is
*outside* the registered text range — do not expand the range to
cover it") and pointing at the consumers in `unloadable.h`.

---

## P.14 — `middle_end/flambda2/from_lambda/closure_conversion.ml:2889` (REVIEW(claude))

**What.** `is_unloadable` is sourced from `!Clflags.unit_is_unloadable`
at three sites here plus once in `simplify_apply_expr.ml`. If
cross-CU inlining of unloadable IR ever lands a function in a
non-unloadable host CU, the closinfo bit and `fun_unloadable` survive
but `to_cmm` won't emit a matching `Code_block` → back-pointer
relocation points at a non-existent symbol.

**Disposition.** assert (now) + refactor (follow-up).

**Action now.** Add an assertion at each of the four read sites that
`Compilation_unit.is_current
  (Code_id.get_compilation_unit code_id)` — i.e. the flag we are
about to stamp into the function's metadata applies to the CU that
defined it. This catches any future cross-CU inlining at the latest
point before the bit gets baked in.

**Follow-up.** Thread `is_unloadable` through `Code_metadata.create`
from a per-CU input so the global flag can be deleted from these
sites.

---

## P.15 — `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:28` (REVIEW(codex))

**What.** Comment claims Code_block symbols are exported because
"back-pointers in foreign CUs may reference them via direct calls
into our text" — but the back-pointer is in this function's own
.text and points at this function's Code_block, so the foreign-CU
case is unclear.

**Disposition.** doc-only.

**Action.** Rewrite the comment: the true reason for `Global` is so
the JIT loader can find these symbols by name when populating the
unit's `code_blocks` table. Once P.5 lands (sentinel-symbol
discovery), revisit whether `Global` is needed at all — if the
sentinel encodes the addresses directly, these can be `Local`.

---

## P.16 — `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:51` (REVIEW(claude))

**What.** `symbol_deps` (unfiltered) becomes fields of every
unloadable Code_block; cross-CU externs (caml_int_ops, stdlib lifted
constants) and same-CU `Local` symbols are NOT_MARKABLE no-op
darkens. Sound but bloats every Code_block.

**Disposition.** fix.

**Action.** Filter `symbol_deps` to symbols whose defining CU is the
current (unloadable) CU AND whose defining symbol has a markable
header — i.e. emitted via `emit_block` and tracked in
`unloadable_data_block_symbols`. Other symbols can be skipped.

**Notes.** Same comment also flags a separate concern about
`Symbol.is_local` vs the `sym_global` field — defer; current pipeline
doesn't produce a disagreement.

---

## P.17 — `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:71` (REVIEW(codex))

**What.** `Symbol` occurrences are assumed to denote value-like
static blocks; a non-value symbol would be scanned as a heap pointer
and crash.

**Disposition.** assert.

**Action.** Add a debug-only assertion in `emit_code_block_for`
that each symbol in `symbol_deps` has been seen by `emit_block` (i.e.
it's in `unloadable_data_block_symbols` for the current CU). Once
P.16 lands the filter establishes the same property by construction;
keep the assertion as belt-and-braces.

---

## P.18 — `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:103` (REVIEW(claude))

**What.** The entry function's Code_block has zero dependency fields.
Correct because the entry is only on-stack during initialisation,
but worth a sanity test.

**Disposition.** test (low priority).

**Action.** Add `unload_signal_gc`-style test with extreme recursion
inside the eval'd initialiser, to confirm a major GC fired during
entry execution still marks everything reachable via the stack. If
adding the test is heavy, defer — the reasoning is documented here.

---

## P.19 — `otherlibs/eval/eval.ml:231` (REVIEW(codex))

**What.** After extracting the result via `Obj.field struct_obj 0`,
the field is left in place. If the unit's `gc_roots` table is scanned
every major cycle, that field can pin the returned value (and thus
the whole unit).

**Notes.** The existing test `run_heap_value_returned_directly` was
added to confirm unload still happens for `Eval.eval <[ ref 0 ]>`
and the reference output shows `unloaded=5 live=0`, so reachability
is correctly cut today.

**Disposition.** doc-only.

**Action.** Convert the REVIEW into a comment explaining *why*
unloading still works: `scan_native_globals` walks
glob_block *fields*, but the field is reached via the result
reference, and once the caller drops the result, the dyn-globals
scan finds nothing live. Cross-reference P.20 / P.25.

---

## P.20 — `runtime/caml/frame_descriptors.h:66` (REVIEW(codex))

**What.** `FRAME_DESCRIPTOR_FLAGS = 0xF` but the comment above still
says "bottom two bits".

**Disposition.** doc-only.

**Action.** Update the comment to "bottom four bits" and document the
16-byte alignment invariant on `frame_size` (paired with P.3
emitter-side assertion).

---

## P.21 — `runtime/caml/mlvalues.h:485` (REVIEW(claude))

**What.** New `is_unloadable` bit is at position 54. `Make_closinfo`
(C macro, in-tree callers pass tiny deltas) does not mask `delta`
against the new bit; a delta with bit 53 set would alias.

**Disposition.** fix (defensive).

**Action.** Either delete `Make_closinfo` in favour of always passing
`is_unloadable` explicitly, OR mask `delta` with
`((uintnat)1 << 53) - 1` inside the macro. Prefer the latter for
diff-size; it's one extra `&`.

---

## P.22 — `runtime/caml/unloadable.h:116` (REVIEW(claude))

**What.** `caml_unregister_unloadable_unit` is public CAMLextern, no
in-tree callers (`caml_unloadable_check_and_unload_dead` inlines its
steps). Comment claims "STW required" with no enforcement.

**Disposition.** delete (with one caveat).

**Action.** *However* — the new test
`unload_units_mutex_init_race_.c` calls it. Either keep the function
and tighten its contract (assert we are in STW), or replace the test
caller with direct inlining of the steps and then delete the public
entry point. Recommend: keep, add an STW assertion using whatever
mechanism the runtime exposes (search for `caml_state->all_domains_lock`
or `Caml_state` flags).

---

## P.23 — `runtime/caml/unloadable.h:168` (REVIEW(codex))

**What.** `caml_find_code_fragment_by_pc` is not a reliable
"unloadable code" predicate, because main program and Dynlink also
register fragments. Suggests tagging code fragments with their
owning unit so the check is O(log n) instead of O(units).

**Disposition.** doc-only (now) + perf follow-up.

**Action now.** The F.3 fix (commit `90055d16`) addresses the
correctness bug by chaining the unloadable-unit check. This REVIEW
note is now a performance concern, not a correctness concern.
Convert to a permanent comment recording the perf trade-off; add a
follow-up issue to push owner-CU info into the code-fragment record.

---

## P.24 — `runtime/caml/unloadable.h:221` (REVIEW(codex))

**What.** Closure-prefix slot walker infers slot size from
`Arity_closinfo` and assumes an infix header between functions —
fragile if the layout evolves.

**Disposition.** see P.25 (these comment about the same code).

---

## P.25 — `runtime/caml/unloadable.h:241` (REVIEW(claude))

**What.** Same code as P.24. `slot_start += slot_size + 1` assumes
an `Infix_tag` header between adjacent function slots.

**Disposition.** assert.

**Action.** In DEBUG, before stepping, assert that the word at
`Field(closure, slot_start + slot_size) - sizeof(value)` (i.e. the
word just before the next function slot) holds an `Infix_tag`
header. Holds for the current `Slot_offsets.Layout`; would catch any
future divergence.

**Test.** Existing
`unload_closure_prefix_mixed_arity.ml` exercises the walker over
mixed slot sizes; the DEBUG assertion would fire there if violated.

---

## P.26 — `runtime/caml/unloadable.h:281` (REVIEW(claude))

**What.** F.3 walker reads `*((value*)cp - 1)` to recover the
`Code_block` back-pointer, assuming `cp` is exactly a function entry.
Only the closure Field-0 load and static `Csymbol_address` of entry
symbols produce values into `Code_pointer` slots today — no compiler
assertion.

**Disposition.** assert.

**Action.** In DEBUG, after the (already-present) unit-membership
check, assert `caml_find_code_fragment_by_pc((char *)cp)->code_start
== (char *)cp`. If `Code_pointer` ever flows through non-entry PCs,
this fires before the deref of `cp - 1`.

---

## P.27 — `runtime/major_gc.c:950` and `:1159–1160` (REVIEW(codex), REVIEW(claude))

**What.** `caml_darken_unloadable_code_blocks_in_closure` fires for
every `Closure_tag` block scan, even when no unloadable units exist.
Inner loop bails fast, but the call is on the hot mark path.

**Disposition.** fix (cheap gate).

**Action.** Replace both call sites with:

```c
if (atomic_load_relaxed(&caml_unloadable_units_live_count) > 0)
  caml_darken_unloadable_code_blocks_in_closure(Caml_state, block);
```

backed by a new atomic counter incremented in
`caml_register_unloadable_unit` and decremented in the unload pass.
Keep the per-closure check inside the walker for callers that
bypass the gate.

---

## P.28 — `runtime/unloadable.c:55` (REVIEW(claude)) and `:70` (REVIEW(codex))

**What.** `ensure_mutex_initialized` is broken DCL — two domains can
both observe `!units_mutex_initialized` and both run
`caml_plat_mutex_init`.

**Disposition.** fix.

**Action.** Either (a) replace with a static initializer
`CAML_PLAT_MUTEX_INITIALIZER` at the declaration of `units_mutex`
and delete `ensure_mutex_initialized` entirely, or (b) use a
`pthread_once_t` (or the runtime's equivalent if one exists — grep
for `pthread_once` in `runtime/` to see what idiom is already
established).

**Notes.** Stress test `unload_units_mutex_init_race.ml` exercises
this; non-deterministic, but with TSAN it would flag the data race
today.

---

## P.29 — `runtime/unloadable.c:95` (REVIEW(codex))

**What.** `normalize_block_color` writes the header via `*Hp_val(v)`
non-atomically. During concurrent marking, other domains
update/read headers via `Hp_atomic_val`.

**Disposition.** fix.

**Action.** Use `atomic_store_relaxed(Hp_atomic_val(v), ...)` to
match the convention elsewhere in the runtime. The store happens
during registration (before the unit is visible on `units_head`),
so the atomic ordering is conservative-but-correct.

---

## P.30 — `runtime/unloadable.c:131` (REVIEW(claude)) — gc_roots interaction

**What.** The existing comment is misleading: `scan_native_globals`
walks each glob_block's *fields*, not the glob_block itself. So
`gc_roots` does NOT mark the unit's static blocks; heap-side
`Csymbol_address` references via `caml_darken` are what do.
Unloadability depends on this.

**Disposition.** doc-only.

**Action.** Promote this to a top-level paragraph in the design doc
(`metaprog_unloading_design.md`) and rewrite the in-source comment
to point to it. Add a runtime assertion in DEBUG that surviving
units' `unloadable_data_block_symbols` blocks are reached via heap
pointers, not via the gc_roots field walk.

---

## P.31 — `runtime/unloadable.c:155` (REVIEW(codex)) — gc_roots pinning concern

**What.** Concern that registering gc_roots could pin
otherwise-dead units.

**Disposition.** doc-only.

**Action.** Subsumed by P.30 — the design-doc paragraph explains
why this doesn't happen.

---

## P.32 — `runtime/unloadable.c:164` (REVIEW(claude)) — registration order

**What.** Order is: patch headers → register code fragments →
register frametable → register dyn_globals → link into `units_head`.
Steps 2–4 publish the unit globally before step 5; matters for the
born-marked invariant. Today implicit.

**Disposition.** doc-only.

**Action.** Convert the REVIEW into a permanent comment block at the
top of `caml_register_unloadable_unit` explaining the order and
which interleavings each step protects against. Cross-reference
P.10 (registration-window race) and P.30 (gc_roots semantics).

---

## P.33 — `runtime/unloadable.c:338` (REVIEW(claude)) — stale dead-function note

**What.** Comment says `caml_find_unloadable_unit_by_pc` is dead.

**Disposition.** delete-comment.

**Action.** Delete the REVIEW(claude) block — the F.3 gating fix
(commit `90055d16`) now uses this function at two sites in
`runtime/caml/unloadable.h`.

---

## Things this plan deliberately does NOT change

- The two committed fixes (`flags land 3 = 3` in emitaux, F.3
  unloadable-unit gating in unloadable.h).
- The closinfo bit layout / Start_env_closinfo / Unloadable_closinfo
  math — checked OK in REVIEW-CLAUDE "Things judged OK".
- Frame-descriptor on-disk layout — three consumers agree on the
  five-section walk; layout comments at each of the three sites
  cross-reference the others.
- Born-marked normalisation in `normalize_block_color` — correct use
  of `caml_allocation_status()`. (Only the *atomicity* of the store
  changes — see P.29.)
- Test coverage. The base PR already covers smoke, multi-cycle,
  mutual recursion, partial application, tupled/unboxed nums/floats,
  value slots, exceptions, returned data, cross-unit, signal GC,
  large static, C callback, reachability — plus the new tests
  contributed by this review.

## Recommended landing order

1. P.4, P.21 (defensive errors / masking — small, isolated).
2. P.28, P.29 (multi-domain correctness — small, isolated).
3. P.27 (cheap perf gate — small, isolated).
4. P.5 + P.6 + P.7 + P.8 + P.9 (sentinel-symbol refactor — one PR
   covering all JIT symbol-table cleanups).
5. P.3 + P.20 (frame-descriptor 16-byte invariant + comment).
6. P.16 + P.17 (Code_block dep-field filter + assertion).
7. P.25 + P.26 (DEBUG asserts in the closure-prefix and F.3 walkers).
8. P.11, P.12 (jit_stubs hygiene).
9. P.22 (`caml_unregister_unloadable_unit` decision).
10. P.30 + P.31 + P.32 + P.13 + P.15 + P.19 + P.1 (doc-only sweep).
11. P.33 (delete stale comment — fold into the doc-only sweep).
12. P.2 + P.14 (per-CU contract assertions).
13. P.10, P.18, P.23 (deferred / follow-up).
