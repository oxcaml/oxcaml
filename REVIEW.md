# Review: Unloading of metaprogramming compilation units

PR: commit `565b72a8b2` ("Unloading of metaprogramming compilation units")

Reviewed against the design at [`metaprog_unloading_design.md`](./metaprog_unloading_design.md).

All inline review notes are tagged `REVIEW(claude)` in the source. This
document organises them by severity and gives reviewers the context they
need to triage / land fixes.

## TL;DR

- One reproducible **bug** (frame-descriptor parse drift under
  `-g + Eval.eval + allocations`).
- Two **latent correctness traps** (LLVM backend silently drops
  `Unloadable`; F.3 dereferences arbitrary `Code_pointer` slots as if
  they were function entries).
- A handful of **fragility / design** concerns (file-global mutable
  compilation state, suffix-based JIT symbol detection, dead public
  entry points, hot-path overhead in the closure mark scan).
- Several **documentation gaps** where the code is correct but the
  reasoning is non-obvious (how `gc_roots` interacts with unloadability,
  the closinfo layout, the frame-descriptor on-disk layout shared
  between three files).
- One new test (`unload_alloc_with_debug.ml`) reproduces the bug.

## Bugs

### 1. `emitaux.ml` — `flags = 3` should be `flags land 3 = 3`

**Files**:
- `backend/emitaux.ml:261` (the buggy check)
- `runtime/frame_descriptors.c:next_frame_descr` (the consumer that
  trips on the absent labels)

**Severity**: bug. Crashes / table corruption under `-g`.

**Detail**: When emitting a frame descriptor for an alloc-safe-point
inside an unloadable function compiled with `Clflags.debug = true`,
`get_flags` returns `3` (DEBUG | ALLOC). The new code then ORs in `4`
(UNLOADABLE), giving `flags = 7`. The check at line 261 — `if flags = 3
then ... emit per-alloc debuginfo labels ...` — is now false, so the
labels are not emitted. The runtime parser
(`runtime/frame_descriptors.c next_frame_descr`) still sees DEBUG and
ALLOC bits set and reads `4 * num_allocs` bytes that aren't there. Every
subsequent `next_frame_descr` call lands at the wrong offset, corrupting
the frame-descriptor hashtable. Symptoms: `CAMLassert(Retaddr_frame(d)
>= 4096)` failure during registration, or wrong descriptors causing
corrupt stack scans / segfaults during the next major GC walking the
unit's stack frames.

**Repro**: `testsuite/tests/quotation/eval/unloading/unload_alloc_with_debug.ml`

**Fix**: change `if flags = 3` to `if flags land 3 = 3`.

## Latent correctness traps

### 2. F.3 dereferences `cp - 1` for any `Code_pointer`-typed slot

**File**: `runtime/caml/unloadable.h:267` (and the implementation just
below in `caml_visit_frame_code_ptr_slots`)

**Severity**: latent — correct under current compiler invariants;
silently UB if any future code path produces a non-entry PC into a
`Code_pointer` slot.

**Detail**: The walker reads `*((value*)cp - 1)` to recover the
`Code_block` back-pointer, assuming `cp` is exactly a function entry.
Today this is guaranteed only by convention — the only producers are
closure-Field-0 loads in `cmm_helpers.indirect_full_call` and static
`Csymbol_address` references to entry symbols. There is no compiler
assertion; a future optimization that lets a `Code_pointer` value flow
through arithmetic, a phi node, or a different load chunk would silently
break this with no test coverage to catch it.

**Suggested fix**: use `caml_find_code_fragment_by_pc(cp)->code_start - 1`
as the back-pointer slot (we already do the fragment lookup in the
guard); or at minimum `CAMLassert(cf->code_start == (char *)cp)` in
DEBUG.

### 3. LLVM backend silently drops `Cmm.Unloadable`

**File**: `backend/llvm/llvmize.ml:1475`

**Severity**: latent — only matters if LLVM is selected for an
unloadable CU; the `Eval.eval` path doesn't currently use LLVM.

**Detail**: `Cfg.Unloadable` is matched alongside other ignored
codegen options (returns `[]` attributes). But the front-end has still
set:
- the closinfo `is_unloadable` bit on every closure of this CU
- `Code.is_unloadable` in `Code_metadata`
- `fundecl.fun_unloadable` (would set frame-descriptor bits if respected)

If LLVM is ever selected for an unloadable CU, no back-pointer is
emitted at `entry - 1`, no `FRAME_DESCRIPTOR_UNLOADABLE` /
`HAS_CODE_PTR_SLOTS` bits are set, and no `code_ptr_live_ofs` slots are
populated. The closinfo bit is still set, so `caml_darken_unloadable_
code_blocks_in_closure` will dereference `entry - 1` in LLVM-compiled
text and read garbage — UB at the next major GC.

**Suggested fix**: `Misc.fatal_error` if `Unloadable` appears in the
LLVM codegen path, or wire LLVM up to honour the bit.

## Fragility / design concerns

### 4. `ensure_mutex_initialized` is not thread-safe

**File**: `runtime/unloadable.c:55`

**Detail**: Classic broken double-checked-locking. Two domains observing
`!units_mutex_initialized` both call `caml_plat_mutex_init`. The mutex
itself can't serialise this — it's the thing being initialised. Today
single-threaded via `Eval.eval`; `caml_register_unloadable_unit` is
public CAMLextern with no such restriction documented.

**Suggested fix**: one-shot init at runtime startup, or `pthread_once`.

### 5. JIT loader symbol detection is suffix-only

**File**: `external/ocaml-jit/lib/jit.ml:209` and `:345`

**Detail**: Both `unloadable_metadata` (matches `_code_block`) and the
data-blocks lookup (matches `__unloadable_data_blocks`) walk the symbol
table by suffix. Hard to trigger from user OCaml today (linkage names
get stamps), but fragile and three full passes over the symbol table
per JIT load.

**Suggested fix**: emit a sentinel symbol from `to_cmm.ml` enumerating
the unit's `Code_block` symbols (mirroring the existing
`unloadable_data_blocks` table). One known symbol, no walk needed.

### 6. `mprotect(buffer_base, buffer_size, ...)` with unaligned size

**File**: `external/ocaml-jit/lib/jit_stubs.c:404`

**Detail**: `buffer_size` is the raw byte count summed in
`alloc_all`, not necessarily page-aligned. Linux's `mprotect` rounds
length up to a page, potentially changing protection on memory just
past the buffer. Works in practice with glibc / Mach-O `aligned_alloc`
because they `mmap` whole pages, but is implementation-defined.

**Suggested fix**: round `buffer_size` up to page size when storing it
in `jit_unit_loader_data`.

### 7. `Make_closinfo` C macro doesn't zero new bit 54

**File**: `runtime/caml/mlvalues.h:485`

**Detail**: With the new is_unloadable bit at position 54, a `delta`
with bit 53 set would alias into is_unloadable. All in-tree callers
pass tiny constants (0, 2, envofs), so this is not exploitable today;
the macro encodes "non-unloadable" by accident.

**Suggested fix**: either delete `Make_closinfo` in favour of always
passing `is_unloadable`, or explicitly mask.

### 8. F.1 closure scan fires on every closure

**File**: `runtime/major_gc.c:1154`

**Detail**: `caml_darken_unloadable_code_blocks_in_closure` is invoked
for every `Closure_tag` block scan, even when no unloadable units
exist. Inner loop bails fast (one closinfo read), but the call itself
is on the hot mark path.

**Suggested fix**: gate on a cheap predicate (e.g. a global non-zero
counter of registered unloadable units), or on the closure's own
closinfo unloadable bit (within a closure all slots are from the same
unit).

### 9. Per-CU compilation state held in file-global mutables

**File**: `backend/cmm_helpers.ml:4279`

**Detail**: `unloadable_data_block_symbols` (per-CU accumulator) and
`suppress_unloadable_data_block_tracking` (a stack of one) are file
globals. With `Clflags.unit_is_unloadable` they implicitly assume
serial single-CU compilation. Concurrent compilation of multiple CUs
would corrupt these silently. Not broken today; relies on an
undocumented contract.

**Suggested fix**: thread compilation context through to_cmm explicitly,
or at minimum add a CU-id assertion.

### 10. `caml_unregister_unloadable_unit` is dead and misleading

**File**: `runtime/caml/unloadable.h:116` and `runtime/unloadable.c:138`

**Detail**: Public CAMLextern, no in-tree callers
(`caml_unloadable_check_and_unload_dead` inlines its steps). Comment
says "STW required" but no enforcement; would race the unload pass if
used.

**Suggested fix**: delete, or add an assertion + restrict visibility.

### 11. `caml_find_unloadable_unit_by_pc` is dead

**File**: `runtime/unloadable.c:325`

**Detail**: Header documents F.2/F.3 use; those paths actually use
`caml_find_code_fragment_by_pc`. No callers.

**Suggested fix**: delete.

### 12. `symbol_deps` in Code_block fields is unfiltered

**File**: `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:46`

**Detail**: `code_id_deps` is filtered by `dep_is_unloadable`, but
`symbol_deps` includes every referenced symbol (including cross-CU
extern data, same-CU `Local` symbols which are NOT_MARKABLE / no-op
darkens). Sound; just bloats every Code_block by symbol-reference
fan-out.

**Suggested fix**: filter to same-CU Globals, or to symbols whose
defining CU is also unloadable.

### 13. OOM cascade leaks in `jit_register_unloadable_unit_native`

**File**: `external/ocaml-jit/lib/jit_stubs.c:485`

**Detail**: The five sequential `caml_stat_alloc_noexc` allocations
each `caml_raise_out_of_memory()` on failure, longjmping out and
leaking everything allocated above. OOM is fatal-ish so the leak is
small, but the pattern is incorrect.

**Suggested fix**: either compute total bytes once and a single
allocator call, or stash partial state on a cleanup list before each
fallible alloc.

### 14. `function_entries` index drift after sort

**File**: `external/ocaml-jit/lib/jit.ml:240`

**Detail**: After `Array.sort Nativeint.compare entries`, `entries[i]`
no longer corresponds to `code_blocks[i]`. The C side doesn't rely on
the alignment, but the asymmetry is non-obvious.

**Suggested fix**: rename to `entries_sorted` and document the
intentional drop of correspondence.

### 15. Cross-CU inlining of unloadable code into non-unloadable host

**File**: `middle_end/flambda2/from_lambda/closure_conversion.ml:2889`

**Detail**: `is_unloadable` is set from a global mutable
(`!Clflags.unit_is_unloadable`) at three call sites in this file plus
once in `simplify_apply_expr.ml`. If the IR for an unloadable function
ever reaches a non-unloadable host CU (cross-CU inlining), the closinfo
unloadable bit and `fun_unloadable` flag survive but the host's
to_cmm will not emit a matching `Code_block`. The back-pointer
relocation then points to a non-existent symbol. Doesn't happen with
current pipelines; no invariant prevents it.

**Suggested fix**: thread an explicit per-CU input through
`Code_metadata.create`, or add an invariant assertion.

### 16. F.1 closure-walker `+1` infix-step assumption

**File**: `runtime/caml/unloadable.h:227`

**Detail**: `slot_start += slot_size + 1` assumes every multi-function
closure has an `Infix_tag` header between adjacent function slots. Holds
for the current `Slot_offsets.Layout` emission; would silently miscount
under any change to the prefix layout.

**Suggested fix**: in DEBUG, assert that the word at `slot_start - 1`
holds an `Infix_tag` header before stepping.

### 17. Entry Code_block has zero dep fields

**File**: `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:92`

**Detail**: The module-initialiser ("entry") function emits a
`Code_block` with no scannable fields. Correct because the entry is
only on-stack during initialisation, but worth a sanity-checking test
(e.g. extreme recursion depth from inside the entry).

### 18. Registration window between unit-register and `jit_run`

**File**: `external/ocaml-jit/lib/jit.ml:385`

**Detail**: Between `Externals.register_unloadable_unit` (which
publishes the text range and frametable) and `jit_run` (which actually
invokes `caml_callback` on the entry), a major GC on another domain
could in principle visit a stale state. On further analysis the
chained `gc_roots` scan is safe because `compute_index_for_global_root_
scan` skips closure-prefix function slots — but this is non-obvious and
worth an explicit safety argument or a test for "Eval.eval racing with
GC on another domain".

## Documentation gaps (code correct, reasoning non-obvious)

- **`runtime/unloadable.c:124`** — gc_roots interaction. The current
  comment is misleading: gc_roots scan does NOT mark the unit's static
  blocks; it scans their *fields*. Heap-side `Csymbol_address`
  references are what darken them. Unloadability depends on this
  detail; a future "fix" that darkens the glob_block itself would
  silently make every unit immortal. Worth promoting to a top-level
  comment in the design doc with a runtime assertion.
- **`backend/cmm_helpers.ml:380`** — closinfo word layout
  (`pack_closure_info`). The `pos_arity_in_closinfo - 3` in the
  assertion was previously `-2`; the layout cross-reference and the `-3`
  derivation are now in a comment block above `pack_closure_info`.
- **`backend/emitaux.ml:197`** and **`runtime/frame_descriptors.c:37`**
  and **`runtime/caml/unloadable.h:265`** — the on-disk frame-descriptor
  layout now has THREE consumers/producers that must agree. Each has a
  layout comment cross-referencing the other two.
- **`runtime/unloadable.c:151`** — registration order
  (header-patch → code-fragments → frametable → dyn_globals → list link)
  matters for the born-marked invariant; previously implicit.
- **`external/ocaml-jit/lib/jit_stubs.c:509`** — text_range starts at
  the function entry, NOT at `entry - 1`; back-pointer is intentionally
  outside the registered fragment range.

## Tests added

`testsuite/tests/quotation/eval/unloading/unload_alloc_with_debug.ml`
reproduces bug #1. Sets `Clflags.debug := true`, `Eval.eval`s a
function with multiple allocations and an in-call `Gc.compact`. With
the bug present the test crashes (or asserts) during JIT registration
or the in-call major GC. With the bug fixed, prints `result = 128 / ok`.

Why only one test:
- Bug #4 (`ensure_mutex_initialized` race) — non-deterministic, depends
  on platform timing.
- Bugs #6 / #13 / #7 — implementation-defined behaviour or physical
  impossibility (`delta >= 2^53`).
- Latent traps #2 / #3 — current compiler invariants prevent reaching
  the buggy paths from `Eval.eval` today.
- Fragility issues #5 / #15 / #16 — would require adversarial inputs
  the front-end doesn't currently produce.
- Documentation gaps — not "bugs that exhibit visible failures".

## Things judged OK after careful review

These were checked deliberately and are believed correct:

- **Closinfo bit-stealing math** (`pack_closure_info`,
  `Start_env_closinfo`, `Unloadable_closinfo`). Layout: arity (8) |
  is_last (1) | is_unloadable (1) | startenv (53) | tag (1). Assertion
  bound `< 1 lsl (pos_arity_in_closinfo - 3)` is correct.
- **Frame-descriptor on-disk layout**. `emit_frame` (producer),
  `next_frame_descr` (table-construction consumer), and
  `caml_visit_frame_code_ptr_slots` (mark-time consumer) all walk the
  five sections (header / live_ofs / alloc_lens / debuginfo /
  code_ptrs / pad) in the same order with the same alignment rules.
  See cross-referenced comments.
- **Born-marked normalisation** in `normalize_block_color`. Correctly
  uses `caml_allocation_status()` to match the shared-heap allocator's
  convention so blocks born during in-progress marking survive the
  current cycle.
- **Text-range / code-fragment integration**. All functions in a JIT'd
  unit are unloadable (no mixed-loadability inside one CU by
  construction), so `[entry_i .. entry_{i+1})` ranges don't cover
  non-unloadable code.
- **`gc_roots` registration**. Sound only because
  `scan_native_globals` scans fields-of-block, not block-itself. See
  documentation gap above.
- **Test coverage**. Existing test set covers smoke, multi-cycle,
  mutual recursion, partial application, tupled / unboxed nums /
  floats, value slots, exceptions, returned data, cross-unit, signal
  GC, large static, C callback, reachability. Notable remaining gaps:
  no `-g` debug-mode test would have caught bug #1 (now added); no
  targeted F.3 test that spills a `Code_pointer` across a safepoint
  with an in-call GC; no multi-domain stress test for the registration
  race (#4).

## Suggested follow-ups (not in this PR)

1. Fix bug #1 and confirm the new test passes.
2. Add a DEBUG assertion in F.3 (#2) that
   `cf->code_start == (char *)cp`.
3. Make LLVM (#3) error out on `Cmm.Unloadable` with `Misc.fatal_error`
   until properly supported.
4. Fix the mutex-init race (#4) via one-shot init.
5. Replace the `_code_block` / `__unloadable_data_blocks` suffix walk
   with a sentinel-symbol enumeration (#5).
6. Page-align `buffer_size` in `jit_unit_loader_data` (#6).
7. Delete dead entry points (#10, #11) or wire them up.
8. Promote the gc_roots-scan-skips-block-itself invariant into the
   design doc and add a runtime assertion that surviving units' tracked
   blocks really are reached via heap pointers, not gc_roots.
