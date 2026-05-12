# Review: Unloading of Metaprogramming Compilation Units

Diff under review: `72c5b63b80..HEAD`

## Design doc (`metaprog_unloading_design.md`)

The design is **reasonable and well-considered**. Key strengths:

1. **Elegant core mechanism.** Putting the dependency graph into the heap
   (`Code_block`s with standard headers) lets the existing major-GC mark phase
   do most of the work. The back-pointer at `entry - 1` in `.text` keeps
   `.text` RX while still giving the GC a path from any function entry to its
   Code_block.
2. **Closure walk in F.1** correctly uses arity (signed) — not infix-tag
   probing — to determine slot size; this is necessary because single-function
   closures with non-scannable env have no infix header.
3. **White/black header split for Global/Local symbols** is well-justified:
   Mach-O cross-section relocation prevents Local symbols in the tracked
   array; the same uniform code path on ELF is correct by a clean argument
   (Local sub-blocks reachable only via Global ancestors, NOT_MARKABLE is
   transitively safe).
4. **"Born-marked" units** registered during in-progress marking are correctly
   handled — necessary because curry-stub closures allocated mid-cycle are
   stamped MARKED and skipped by the marker.
5. **F.3's two-gate check** (registered fragment AND unloadable unit) is
   necessary because the code-fragment skiplist also contains non-unloadable
   text; the `unload_code_ptr_slot_gating` test exercises this directly.
6. **Entry-function zero-dep Code_block** is sound: while running, the entry's
   frame keeps its Code_block alive via F.2; every same-CU function it has
   called is also on stack above (F.2 keeps theirs alive); after `Eval.eval`
   returns nothing reaches the entry's Code_block.

## Implementation vs. doc

**Faithfulness.** The diff implements every numbered mechanism in the doc:

- A.0–A.5 (closinfo bit, frame descriptor bits, Code_pointer machtype) —
  `runtime/caml/mlvalues.h:454-490` (`Unloadable_closinfo`,
  `Make_closinfo_unloadable`, mask of `delta` to 53 bits);
  `runtime/caml/frame_descriptors.h:44-58` (flag bits 4/8, `0xF` mask);
  `backend/cmm.ml` (`Code_pointer`, `Word_code_pointer`);
  `backend/cmm_helpers.ml` (`pack_closure_info`, `Make_closinfo_unloadable`
  bit positions match).
- B.1–B.3 sentinel emission, white/black header split —
  `backend/cmm_helpers.ml:emit_unit_block`, `unit_block_header`;
  `middle_end/flambda2/to_cmm/to_cmm.ml` (both sentinels emitted
  unconditionally for unloadable CUs, with explicit `count=0` allowed).
- C: Code_block emission, dep-list filtering (same-CU symbols, unloadable
  code IDs), entry's zero-dep Code_block —
  `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml`.
- D: back-pointer at `entry - 1` — `backend/amd64/emit.ml:2719-2729`
  (8 bytes pad + 8 bytes back-pointer = 16, keeping entry 16-byte aligned);
  `backend/arm64/emit.ml:2104-2112` (8-byte align + 8-byte back-pointer).
- E: registration ordering — `runtime/unloadable.c:90-171` matches doc
  step-for-step including the "publish before list" ordering.
- F.1–F.3 — `runtime/major_gc.c` (closure scan injection in both fast-path
  and `mark_stack_push_block`), `runtime/fiber.c:caml_scan_stack`
  (F.2 + F.3).
- G: end-of-cycle pass — `runtime/unloadable.c:182-262`, called from
  `cycle_major_heap_from_stw_single` **before** rotation as required.

**Tag reservation.** `Code_block_tag = 243` is added consistently across
`runtime/caml/mlvalues.h`, `utils/runtimetags.ml(i)`, `stdlib/obj.ml(i)`;
`last_non_constant_constructor_tag` reduced from 243 to 242.

**Closinfo bit layout.** Verified end-to-end:

- C: arity << 56, is_last << 55, is_unloadable << 54, delta masked to 53 bits
  then << 1, low bit = 1.
- OCaml `pack_closure_info` uses `pos_arity_in_closinfo (= 56) - 2 = 54` for
  is_unloadable; startenv assertion changed from `< 1 lsl (56-2)` to
  `< 1 lsl (56-3) = 1 lsl 53`. Matches the C-side mask.
- `Start_env_closinfo` shifted from `<< 9 >> 10` to `<< 10 >> 11` to skip the
  new bit.
- `Unloadable_closinfo` = `<< 9 >> 63` (extracts bit 54).

**Frame-descriptor layout.** `frame_size land 0xF = 0` is asserted in
`record_frame_descr`; the existing AAPCS / SysV ABI guarantee that frame
sizes are 16-byte aligned makes this safe. The `flags land 3 = 3` mask fix
in `emit_frame` is correct — without it, an unloadable frame with `Dbg_alloc`
under `-g` would set bit 2 or 3 in `flags`, breaking the original `flags = 3`
check for emitting per-alloc debuginfo labels.

**JIT integration.** `external/ocaml-jit/lib/jit.ml:264-327` correctly:

- Looks up sentinels by exact name (no scan-by-suffix).
- Strips frametable/gc_roots/code_begin/code_end from `entry_points` after
  registering with the unloadable path (avoids duplicate registration; the
  comment about `caml_register_dyn_global` raising on duplicate is real).
- `jit_supports_unloading` correctly gates on ASan/musl/TCMalloc; fallback
  path compiles non-unloadable and leaks.
- `jit_unit_on_unload` correctly mprotects RW before `free` (since parts
  were RX/RO).
- Entry deduplication via `qsort` + uniq handles Mach-O alias symbols.

## Concerns

1. **F.3 mutex contention.** `caml_find_unloadable_unit_by_pc` is O(units)
   and takes `units_mutex` per code-pointer slot during stack scans. Under
   multi-domain marking, this is a serialization point. The doc acknowledges
   this in Open Issue #2 (suggest tagging code fragments with their owning
   unit for O(log n)). Not blocking, but should be measured.

2. **End-of-cycle header rewrites use non-atomic stores.**
   `caml_unloadable_check_and_unload_dead` writes
   `*Hp_val(v) = With_status_hd(hd, marked)` (non-atomic), while
   `normalize_block_color` uses `atomic_store_relaxed` on `Hp_atomic_val`.
   STW execution makes the non-atomic store visible to all domains before
   they resume, so it's correct — but inconsistent style. Worth a comment.

3. **`cfg_selectgen.ml` emit_stores arm
   `Val | Addr | Int | Code_pointer -> Word_val`.** Treats a Code_pointer-typed
   register as Word_val when stored to a heap block. This is dead code in
   practice (Code_pointer is supposed to be transient register/stack only,
   never stored to heap blocks), but if it ever fires it would push a raw PC
   through `caml_initialize`. Either it should be `Word_code_pointer`
   (consistent) or `Misc.fatal_error "Code_pointer cannot be stored to a heap
   block"` (defensive). Currently it would silently miscompile.

4. **`lub_component` asserts false on Code_pointer + others.** If Cmm IR
   ever joins a `Code_pointer` with a `Val` or `Int` at a control-flow
   merge, the compiler crashes. The same is true of `Float`, so this
   follows precedent — but `Code_pointer` originates from a more localized
   site (closure Field 0 load), so the constraint is tight.

5. **`unloadable_bit_for_code_id` assertion** in `closure_conversion.ml`
   only fires when the code id's CU is the current one. The comment correctly
   identifies that cross-CU inlining of unloadable IR would break this. A
   future cross-CU inlining feature would need to thread the bit through
   `Code_metadata`, not read `!Clflags.unit_is_unloadable`.

6. **File-global state in `cmm_helpers.ml`** (`unloadable_data_block_symbols`,
   `unloadable_code_block_entries`, `suppress_unloadable_data_block_tracking`).
   Documented as fine because compilation is serial, but a CR for concurrent
   CU compilation.

7. **AMD64/ARM64 alignment asymmetry.** AMD64 uses 16-byte alignment + 8-byte
   pad + 8-byte back-pointer (total 16). ARM64 uses 8-byte alignment + 8-byte
   back-pointer (total 8). Both correct, but the design doc says "one machine
   word at entry - 1" without specifying the leading alignment; the ARM64
   pre-alignment of 8 (not 16) is fine because functions only need
   instruction alignment, but worth noting.

8. **`to_cmm_result.ml` fallback** uses `unit_block_header 0 0` for an empty
   module symbol. In unloadable mode this emits a white-headered size-0 block
   AND tries to register it for tracking — registering a 0-field block is
   harmless but unusual. The case only fires when the unit has no actual
   content; probably dead in practice.

9. **`caml_visit_frame_code_ptr_slots` DEBUG assertion**
   `cf->code_start == cp`. If any future codegen ever produces a non-entry PC
   in a `Code_pointer` slot (e.g., a code-pointer arithmetic expression),
   DEBUG builds catch it; release builds would dereference into adjacent text
   and read garbage. The assertion is the only safety net — worth keeping
   invariant-checked tests.

10. **`unit_is_unloadable` flag is global mutable state**. The save/restore
    in `jit_load_lambda` is correct, but any exception in
    `compile_implementation` that doesn't run the explicit `restore ()` would
    leave the flag stale. The `try ... | exception exn -> restore (); raise
    exn` pattern handles this — verified.

## Test coverage

Coverage is genuinely thorough:

- Smoke and reachability via closures.
- All closure shapes (single/multi-function, sized 2/3, mutual rec up to
  4-way, partial app, tupled, infix-tag stress, mixed-arity prefix).
- Env shapes (constants, lifted, mixed scannable/non-scannable, boxed/unboxed
  floats, int64#).
- Cross-CU heap pointer chains.
- Stress (many cycles, concurrent units, large static).
- C-level gating tests (`unload_code_ptr_slot_gating`,
  `unload_frametable_layout_sanity`) directly exercise the runtime predicates.
- `unload_entry_gc_during_init` stresses C.2's zero-dep entry invariant under
  forced mid-init GC.
- Alloc-with-debug stresses the `flags land 3 = 3` mask fix.
- Musl is correctly excluded in CI; all tests carry `runtime5;
  no-address-sanitizer;`.

## Verdict

**The design is sound and the implementation is faithful to it.** The
trickiest parts (registration ordering, born-marked invariant, F.1
closure-walk arity heuristic, F.3 dual-gate check, entry's zero-dep
Code_block, gc_roots semantics) are handled correctly with detailed
documentation in both the runtime headers and the design doc. The test suite
genuinely exercises the corner cases that matter.

Most concerns are non-blocking quality items: O(units) F.3 lookup
(acknowledged), the `Word_val` mapping for Code_pointer in `cfg_selectgen.ml`
(dead code today but a footgun), and the non-atomic header rewrite in the
end-of-cycle pass (correct under STW, but inconsistent).

I'd accept this with the suggestion to:

1. Either change the `cfg_selectgen.ml` Code_pointer arm to
   `Misc.fatal_error` or `Word_code_pointer` rather than silently aliasing to
   `Word_val`.
2. Add a comment to `caml_unloadable_check_and_unload_dead` explaining why
   the header rewrite uses non-atomic stores (STW invariant).
3. Track Open Issue #2 (O(log n) `caml_find_unloadable_unit_by_pc`) as a real
   performance follow-up — it's on the hot stack-scan path.
