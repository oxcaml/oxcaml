# PR Review: Metaprogram Unloading (OxCaml)

This is an external-style review of the squashed PR (single commit).

Primary inputs:
- `PR.md`
- `metaprog_unloading_design.md`

The implementation spans the runtime, backend frame descriptors, Flambda2
codegen to Cmm, and ocaml-jit integration.

## High-level summary

The PR introduces a mechanism to register "unloadable compilation units"
(CUs) produced by the JIT, keep them alive when reachable during GC, and
unregister/unmap them at the end of a major GC cycle when unreachable.

Reachability is detected via multiple paths (per the design doc):
- F.1: closure scanning (function slots) to find unloadable entries
- F.2: stack return-address scanning (frame descriptor bit)
- F.3: stack code-pointer slot scanning (parallel per-frame metadata)
- G: end-of-major-cycle unload of unreachable units

The design is reasonable and fits runtime5 constraints (STW boundaries,
frame-table lookup, code fragment registration). The key concern is that
some gating logic currently treats "registered code fragment" as synonymous
with "unloadable code", which is not true and can cause crashes.

## Most important correctness issues

### 1) F.3 gating can crash (or mis-mark)

In `runtime/caml/unloadable.h:267`, F.3 walks code-pointer-typed slots and
currently gates by `caml_find_code_fragment_by_pc(pc) != NULL`. However,
the code fragment table includes non-unloadable code (main program `.text`,
dynlink, etc.). For those PCs, the "entry - 1 back-pointer word" convention
does not hold; dereferencing it can read arbitrary memory and crash.

Files:
- `runtime/caml/unloadable.h:267` (code-pointer slot scan)
- `runtime/caml/unloadable.h:184` (dereference of `entry - 1`)

Impact:
- potential segfault during stack scanning (major, minor, compactor)
- potential incorrect marking by treating non-unloadable code as unloadable

Recommendation:
- do not use code-fragment membership as an "unloadable code" predicate
- gate by unloadable-unit membership (or tag code fragments with owner CU)

Test reproducer:
- `testsuite/tests/quotation/eval/unloading/unload_code_ptr_slot_gating.ml:1`
  (currently fails until the gating is fixed)

### 2) Thread-safety hazards in unloadable registry

`runtime/unloadable.c` lazily initializes `units_mutex` via a non-atomic
flag. Concurrent registrations can race and observe a partially initialized
mutex. Additionally, block header writes use `*Hp_val(v) = ...` rather than
the atomic header paths used elsewhere during concurrent marking.

Files:
- `runtime/unloadable.c:55` (lazy mutex init)
- `runtime/unloadable.c:83` (non-atomic header update)

Impact:
- undefined behavior under multicore registration / GC concurrency

Recommendation:
- prefer a static initializer or once-style init for `units_mutex`
- use the same atomic header store conventions as concurrent marking code

Stress test:
- `testsuite/tests/quotation/eval/unloading/unload_units_mutex_init_race.ml:1`
  (best-effort stress; may not deterministically fail even if buggy)

### 3) Frame-descriptor flags vs. frame size alignment

The PR uses the low 4 bits of `frame_data` for flags (`0xF`), but the
emitter currently only asserts 4-byte alignment for `frame_size`. If any
frame size is not 16-byte aligned, flags can collide with size bits and
break stack walking.

Files:
- `runtime/caml/frame_descriptors.h:57`
- `backend/emitaux.ml:86`

Impact:
- corrupted stack walking / GC root scanning on affected frames

Recommendation:
- assert or enforce 16-byte alignment for emitted `frame_size`
- document the invariant in the emitter and/or runtime

Sanity test:
- `testsuite/tests/quotation/eval/unloading/`
  `unload_frametable_layout_sanity.ml:1` (validates that code-ptr-slot
  metadata is parseable and in-bounds)

## Other notable review points

### Closure-prefix scanning subtlety (F.1)

The closure function-slot walker in `runtime/caml/unloadable.h` infers slot
size from `Arity_closinfo` and assumes an infix header between functions.
This is coupled to the current closure-prefix layout and can go out of sync
if the layout evolves.

File:
- `runtime/caml/unloadable.h:214`

Test coverage:
- `testsuite/tests/quotation/eval/unloading/`
  `unload_closure_prefix_mixed_arity.ml:1` (mixed arities + immediate
  captured env to stress scanning logic)

### `gc_roots` registration may pin units / results

`runtime/unloadable.c` registers each unit's `gc_roots` via dyn-globals.
Since dyn-globals are scanned every major cycle, this can keep the unit
reachable in ways that are easy to miss (e.g. module-block field retains
the eval result).

Files:
- `runtime/unloadable.c:121`
- `otherlibs/eval/eval.ml:231`

Regression coverage:
- `testsuite/tests/quotation/eval/unloading/eval_test_unload.ml:27`
  includes a direct heap value: `Eval.eval <[ ref 0 ]>`

### Code_block dependency scanning assumes "value-like" symbols

Flambda2 emits Code_blocks with dependency fields containing symbol
addresses. If any of those symbols are not valid OCaml values (static
blocks), scanning them as heap pointers can crash.

File:
- `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml:40`

Recommendation:
- filter symbol dependencies to value-like symbols only (if possible)
- or add debug-only validation in the JIT loader / registration path

### ocaml-jit metadata robustness (duplicates)

On Mach-O and some toolchains, symbol aliasing can yield duplicate function
entry records. If duplicates lead to repeated fragment registrations or
conflicting ranges, lookups become unpredictable.

File:
- `external/ocaml-jit/lib/jit.ml:233`

Recommendation:
- deduplicate entries by address/range before registration

### Performance: closure scanning on every closure in major GC

The runtime currently calls the unloadable-closure scan helper for every
closure encountered during marking. If unloadable closures are rare, this
may be a measurable overhead.

File:
- `runtime/major_gc.c:935`

Recommendation:
- gate the slow path (e.g. by a global "any unloadable registered" flag)
- or reduce work when `unit_is_unloadable` is known absent

## Tests added / updated by this review

Updated:
- `testsuite/tests/quotation/eval/unloading/eval_test_unload.ml:1`
  Adds a case that returns a heap value directly (`ref 0`).

New:
- `testsuite/tests/quotation/eval/unloading/unload_code_ptr_slot_gating.ml:1`
  Deterministic repro of the F.3 gating issue (currently fails).
- `testsuite/tests/quotation/eval/unloading/`
  `unload_closure_prefix_mixed_arity.ml:1` Stresses closure-prefix scanning
  across mixed slot sizes.
- `testsuite/tests/quotation/eval/unloading/`
  `unload_frametable_layout_sanity.ml:1` Validates parsing of per-frame code
  pointer slot metadata.
- `testsuite/tests/quotation/eval/unloading/unload_units_mutex_init_race.ml:1`
  Best-effort stress for concurrent init/registration safety.

## How to run the relevant tests

Single tests:
- `make -s test-one TEST=quotation/eval/unloading/eval_test_unload.ml`
- `make -s test-one TEST=quotation/eval/unloading/`
  `unload_code_ptr_slot_gating.ml`
- `make -s test-one TEST=quotation/eval/unloading/`
  `unload_closure_prefix_mixed_arity.ml`
- `make -s test-one TEST=quotation/eval/unloading/`
  `unload_frametable_layout_sanity.ml`
- `make -s test-one TEST=quotation/eval/unloading/`
  `unload_units_mutex_init_race.ml`

Note: on this machine, I needed `CCACHE_TEMPDIR=$PWD/_build/ccache-tmp`
to avoid ccache tempdir permission failures during testsuite builds.
