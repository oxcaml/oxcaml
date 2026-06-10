# Compilation Unit Unloading

This document describes how the runtime reclaims JIT-emitted (or
otherwise opt-in) compilation units that have become unreachable. It is
a description of the implementation as it stands, not a forward-looking
design.

## Background

`Eval.eval` (in `otherlibs/eval/eval.ml`) compiles a quoted expression
to a fresh native compilation unit, allocates a buffer for the unit's
text and data, applies relocations, and runs the unit's initialiser
in-process. Without unloading, those buffers would leak for the
remainder of the process — they cannot be `free`d while any live
reference (closure, return address, code-pointer slot, captured
static-data pointer) reaches into them.

The mechanism here lets the major GC determine when an entire
compilation unit (text + data + frame table) has become unreachable,
and reclaim it as a unit at end of major cycle. The mechanism is
general; the only consumer today is the metaprogramming JIT
(`Eval.eval`), but a future Dynlink opt-in would slot in via the same
runtime registration.

## Strategy

Each unloadable CU is a "scannable region with explicit dependency
edges." The standard mark phase keeps the unit's code and data alive
transitively. At end of major cycle, if no `Code_block` and no data
block in the unit has the current `MARKED` status, the unit is
unloaded: its frame table is removed, its code fragments are dropped,
its loader callback is invoked (which `munmap`s / `free`s the
underlying buffer), and its registration is freed.

Core mechanisms:

1. **Code blocks as heap-shaped objects.** For every function in an
   unloadable CU the compiler emits a `Code_block` — a regular OCaml
   block (tag `Code_block_tag = 243`, standard heap header) in the
   `.data` section. Its scannable fields are the function's direct
   **unloadable** dependencies: pointers to other unloadable
   `Code_block`s and to static data blocks. The mark bit lives in the
   standard heap color bits.

2. **Back-pointer in `.text` for unloadable functions.** Immediately
   before each labeled entry of an unloadable function, the compiler
   emits one machine word holding the function's `Code_block` address.
   Set at link/load time, never written again — `.text` stays RX.
   Non-unloadable functions emit nothing.

3. **A closinfo flag bit** identifies closures whose code lives in an
   unloadable CU. The major-GC closure-scan, on a closure with the bit
   set, walks each function slot in the prefix and darkens the
   `Code_block` of every slot whose own closinfo carries the bit, via
   the back-pointer at `entry - 1`.

4. **A frame-descriptor flag bit** identifies stack frames whose return
   address points into unloadable code. The stack walker maps the RA
   to its function entry via the registered code fragment, reads the
   back-pointer, and darkens the `Code_block`.

5. **A `Code_pointer` Cmm machtype** identifies stack/register slots
   that transiently hold a code pointer (e.g. between loading Field 0
   of a closure and issuing an indirect call, when the value is
   spilled or held across a safepoint). Frame descriptors record these
   slots in a parallel `code_ptr_live_ofs[]` array. The stack walker
   reads each such slot, and if the target PC is in a registered
   unloadable text region, darkens the `Code_block` via the
   back-pointer.

6. **Static data in unloadable CUs is emitted in the standard `.data`
   section with non-default headers.** Global symbols get a *white*
   (UNMARKED) header so the standard mark scan can darken them via
   heap references; Local (CU-private) symbols get a *black*
   (NOT_MARKABLE) header and rely transitively on a Global ancestor.
   The unit's `gc_roots` table is registered with the runtime's
   global-root scan: that scan walks each registered block's **fields**
   (not the block itself), preserving any heap values stored into the
   unit's static data while leaving the static blocks themselves
   reclaimable when no heap reference reaches them. This subtlety —
   that `scan_native_globals` darkens fields-of-block, not block-itself
   — is what allows the unit to become unreachable in the first
   place. A future "improvement" that darkens the registered block
   before scanning its fields would silently make every unloadable
   unit immortal.

7. **End-of-cycle unload pass.** Before the heap state rotation at the
   end of every major cycle: for each registered unloadable CU, if no
   block in the unit is `MARKED`, unload it. Otherwise rewrite all
   surviving blocks to `MARKED` so the imminent rotation maps them
   uniformly to `UNMARKED` for the next cycle.

8. **"Born-marked" units registered during in-progress marking.**
   When `Eval.eval` runs while the concurrent marker is mid-cycle, the
   freshly registered unit's blocks are stamped with the current
   allocation status (`MARKED`), not `UNMARKED`. This matches the
   shared-heap allocator's convention and is necessary because heap
   blocks allocated during marking (e.g. a curry-stub closure created
   right after registration) are also stamped `MARKED` and the marker
   skips them — without this, the static eval'd block reachable only
   through such a heap closure would never be darkened.

## A. Per-CU `is_unloadable` flag plumbing

### A.0 Trigger flag

`Clflags.unit_is_unloadable : bool ref` (in `utils/clflags.ml(.mli)`),
default `false`. Not user-CLI-exposed. Set programmatically in
`external/ocaml-jit/lib/jit.ml` around the JIT compilation call:

```
let saved_unit_is_unloadable = !Clflags.unit_is_unloadable in
Clflags.unit_is_unloadable := Externals.supports_unloading ();
... compile ...
Clflags.unit_is_unloadable := saved_unit_is_unloadable
```

`Externals.supports_unloading` returns `false` on three Linux
configurations where `jit_unit_on_unload`'s `free` would corrupt
process state (see C stub `jit_supports_unloading`):

- musl libc: `jit_memalign` falls back to a static `.bss` arena
  because musl's malloc mixes `sbrk` and `mmap`, breaking relocations.
- AddressSanitizer (Linux): `jit_memalign` uses `sbrk` (ASan's
  intercepted `aligned_alloc` returns out-of-relocation-range high
  addresses); ASan reports the resulting `free` as a SEGV.
- TCMalloc (Linux): same `sbrk` path; TCMalloc's `free` does not know
  about pointers returned from `sbrk`.

On macOS, ASan-instrumented `aligned_alloc` is paired with an
ASan-aware `free`, so unloading remains supported. When unloading is
disabled, `Eval.eval` still works: the unit is compiled non-unloadable
(black-headered data, no `Code_block` scaffolding) and its buffer
leaks for the life of the process.

### A.1 `Code_metadata` field

Single source of truth: `Code_metadata.t.is_unloadable : bool`
(`middle_end/flambda2/terms/code_metadata.ml`). Set during closure
conversion from `Clflags.unit_is_unloadable` (snapshot at the start of
compilation) and threaded through to the to_cmm pass that emits
closures, code blocks, function prologues, and frame descriptors.

The flag fans out to four emit-time consumers (A.2–A.5).

### A.2 Closinfo bit (closures)

- Bit **54** of the closinfo word (steals 1 bit from the 54-bit
  `start_env` delta — still covers ~128 PB per closure).
- Macros in `runtime/caml/mlvalues.h`: `Unloadable_closinfo(info)`,
  `Make_closinfo_unloadable(arity, delta, is_last, is_unloadable)`.
- Emitted by `pack_closure_info` in `backend/cmm_helpers.ml`. The
  curry-stub allocation path (`intermediate_curry_functions`) sets
  `is_unloadable = false` on the heap stub closure it allocates: the
  stub itself is in shared, non-unloadable runtime code; the
  underlying unloadable closure is captured as a value-slot env field
  and is reached via the regular field scan.

The bytecode interpreter (`runtime/interp.c`) is not touched —
unloadable units are native-only.

### A.3 Frame descriptor bit (stack frames)

`backend/emitaux.ml` `frame_descr` record carries `fd_unloadable :
bool` (passed by `record_frame_descr`). Callers in
`backend/{amd64,arm64}/emit.ml` thread it through. In `emit_frames`
the bit is OR'd into bit 2 of the `frame_data` word.

`runtime/caml/frame_descriptors.h`:

```
#define FRAME_DESCRIPTOR_DEBUG               1
#define FRAME_DESCRIPTOR_ALLOC               2
#define FRAME_DESCRIPTOR_UNLOADABLE          4
#define FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS  8
#define FRAME_DESCRIPTOR_FLAGS              0xF
```

`frame_size()` masks `~FRAME_DESCRIPTOR_FLAGS`; the bit-2 / bit-3
predicates are `frame_is_unloadable` and `frame_has_code_ptr_slots`.

### A.4 Static data section + header color

See section B.

### A.5 `Code_pointer` Cmm machtype

`Cmm.machtype_component` has a `Code_pointer` constructor (one word;
not a heap value). Used at sites that materialize a code pointer:
closure code-pointer load (Field 0 of a closure for indirect-call
setup) and static code-symbol references.

The register allocator preserves the machtype through copies, moves,
and spills (machtype is a property of pseudoregs). At safepoints, the
backend exposes which slots are `Code_pointer`-typed so frame
descriptor emission can populate the parallel `code_ptr_live_ofs[]`
array.

`code_ptr_live_ofs[]` uses the same encoding as `live_ofs[]` — stack
offsets even, register entries `(reg << 1) | 1` — and is gated by
`FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS` (independent of
`FRAME_DESCRIPTOR_UNLOADABLE`: a non-unloadable frame can still spill
an unloadable code pointer mid-flight to an indirect call). The frame
table parser (`runtime/frame_descriptors.c` `next_frame_descr`) skips
this section.

`code_ptr_live_ofs[]` slots are universal, not unloadable-only: any
load of a closure's Field 0 for indirect call should produce a
`Code_pointer`. For non-unloadable targets, the F.3 lookup is a fast
no-op (the registered-fragment check misses), so the only cost is the
parallel array.

`Code_pointer` values are register/stack-only by construction: the
only in-tree producers are closure Field-0 loads (via
`Word_code_pointer`) and static `Csymbol_address` references of
function entries, and the only consumer is `Capply` (indirect call).
There is no path through to_cmm that would store a `Code_pointer`-
typed value into a heap block, and the `cfg_selectgen.ml`
`emit_stores` arm for `Code_pointer` is `Misc.fatal_error` as a
guard against accidentally introducing such a path in the future:
silently routing the raw PC through `caml_initialize` (which is what
mapping to `Word_val` would do) would create a non-`value` word in a
scannable heap-block field, and silently mapping to `Word_int` would
skip GC tracking but still leave the field unscannable to later
mark scans.

## B. Static data for unloadable CUs

### B.1 Header color

`backend/cmm_helpers.ml` `emit_unit_block`:

```
emit_unit_block sym white_header cont =
  if Clflags.unit_is_unloadable then
    match sym.sym_global with
    | Global -> register sym for tracking; emit white_header
    | Local  -> emit (white_header | caml_black)
  else
    emit (white_header | caml_black)
```

Why the split. Tracked Global blocks are marked at end of every
surviving cycle so the rotation maps them uniformly to UNMARKED in
the next cycle. A white-headered block that is *not* tracked would
have its bits left at zero, which the next cycle interprets as
GARBAGE — any heap pointer reaching it would then trip
`!Has_status_hd(hd, GARBAGE)` in the debug runtime (or read evicted
bits in release).

Local blocks therefore must either be tracked (white) or shielded
from the marker entirely (black, NOT_MARKABLE). On Mach-O they are
*forced* to be black: the assembler cannot relocate a
`Csymbol_address` entry of the per-unit `unloadable_data_blocks`
array to a Local symbol cross-section, so the JIT loader cannot
collect Local addresses. On ELF (Linux x86-64 / arm64) the
constraint does not apply — Local symbols can be relocated
cross-section — but the same code path is used uniformly because the
black-Local choice is correct on all platforms anyway:

- The compiler never emits a heap-allocated value whose scannable
  field points directly to a Local sub-block of the unit. Curry-stub
  closures capture `(arg, clos)` where `clos` is a Global closure
  symbol (`closure_symbol_for_updates` always uses `Global` in
  `to_cmm_set_of_closures.ml`); regular closure value-slot envs hold
  free-variable values, which resolve to Global symbols (other
  top-level closures, ref cells, lifted shared constants) or to
  inlined immediates; `Eval.eval`'s return value is a field of the
  *Global* module block.
- Within the unit, Local sub-blocks are reachable only via a Global
  ancestor. NOT_MARKABLE skips work transitively: a Local block
  whose Global ancestor is marked stays alive (it's part of the
  unit's data section, freed when the unit unloads); a Local block
  whose Global ancestor is *not* marked could in principle be live
  via a stale heap pointer, but the analysis above shows that
  scenario does not arise from the compiler.

So on both platforms the unit's liveness signal is carried entirely
by its Globals.

### B.2 Section

No new section. Cmm data items emit into the standard `.data`
section, which the JIT loader maps writable (only `.text` is RX, and
only `.rodata`-prefixed sections are RO; see
`external/ocaml-jit/lib/jit.ml`). White-headered static data and
`Code_block`s land there and get the right protection automatically.

### B.3 The `unloadable_data_blocks` and `unloadable_code_blocks` sentinels

The compiler emits two static arrays per unloadable unit, each looked
up by the JIT loader using its exact linkage name — no symbol-table
scan by suffix:

- `<unit-prefix>__unloadable_data_blocks`: every tracked Global static
  block's address. Layout `[count; addr_1; ...; addr_count]`.
- `<unit-prefix>__unloadable_code_blocks`: every unloadable function
  in the unit, as `(entry_address, code_block_address)` pairs.
  Layout `[count; entry_1; code_block_1; ...; entry_count;
  code_block_count]`.

The JIT loader hands both sentinel addresses to the runtime stub
(`jit_register_unloadable_unit_native`), which derives the unit's
`code_blocks` array (mark-time darken targets), per-function text
ranges (sorted, deduplicated to handle Mach-O alias symbols), and
`data_blocks` array directly from these arrays. Both sentinels are
emitted unconditionally for every unloadable CU (count may be 0),
so the loader can rely on their presence by name without a fallback
path.

## C. Code blocks

For each function in an unloadable CU the to_cmm pass emits a
`Code_block` — an ordinary Cmm data item (lands in `.data`):

- Header: `Code_block_tag = 243`, `wosize = N`, color UNMARKED.
- `N` value-typed fields, one per direct dependency:
  - For a code dependency: pointer to the dep function's `Code_block`.
  - For a data dependency: pointer to the dep static block.
- All fields are regular Vals; standard mark scan recursively darkens
  them. `Code_block_tag` exists for safety assertions and for
  unload-pass identification; `caml_darken` does not have a special
  arm for it.

The `Code_block` symbol name is `<entry>_code_block` (see
`Cmm_helpers.code_block_symbol_name`). The JIT loader discovers them
via the `unloadable_code_blocks` sentinel array (see B.3); the
`_code_block` suffix is internal-only and never consulted by the
loader.

### C.1 Dep-list filtering

When computing the dep list:

- **Code IDs are filtered to those whose `Code_metadata.is_unloadable`
  is true.** Non-unloadable callees are always live
  (statically-linked code that can never be unloaded), so listing them
  is redundant — it would just bloat the `Code_block` and add no-op
  darken calls.
- **Symbol dependencies are filtered to those whose defining
  compilation unit is the current (unloadable) CU.** Cross-CU
  symbols (e.g. `caml_int_ops`, stdlib lifted constants, predefined
  exceptions) have NOT_MARKABLE headers, so `caml_darken` is a no-op
  on them — including them only bloats every `Code_block` and adds
  work to the mark scan. Same-CU `Local` symbols are kept because
  the unit's data section may reference them and they have markable
  headers per B.1.

Cross-CU direct references go through closures (carrying the
closinfo flag); the `Code_block` dep list captures only same-CU
direct edges.

### C.2 Entry function's Code_block has zero dependency fields

The module-initialiser ("entry") function in an unloadable CU also
gets a `Code_block`, but with **zero scannable fields**, even though
its body typically calls top-level same-CU functions and references
the unit's static data.

The justification: the entry is only on-stack during initialisation
(F.2 keeps its `Code_block` alive while running), and once
`Eval.eval` returns nothing reaches the entry's `Code_block`. If a
major GC fires during eval'd initialisation and walks the running
entry:

- F.2 darkens the entry's `Code_block` (zero deps — no recursion).
- Every same-CU function the entry has called is also on the stack
  above it (the call stack), so F.2 darkens *their* `Code_block`s
  through *their* RAs.
- Any captured/in-flight values the entry references are reachable
  via the regular stack scan or via the unit's `gc_roots` table
  (which the runtime registers for the unit's static data; see
  strategy point 6).

Together these paths reach every block the entry needs, so the
entry's `Code_block` not carrying explicit dep fields is sound.

## D. Back-pointer convention (unloadable functions only)

For each labeled entry of an unloadable function — closure Field 0
entries, infix entries, partial-app trampolines — the function
prologue in `backend/{amd64,arm64}/emit.ml` emits one machine word at
`entry - 1` holding the function's `Code_block` address, gated on
`fundecl.fun_unloadable`. All entries of a single function share the
same `Code_block` target. Non-unloadable functions emit nothing —
`.text` is unchanged for them.

The back-pointer is consulted only by the GC paths in F.1, F.2, and
F.3, all of which already know they are dealing with an unloadable
target (closinfo bit / frame `UNLOADABLE` bit / fragment lookup hit).
The runtime never dereferences `*(entry - 1)` for a non-unloadable
function.

## E. Runtime registration

`runtime/caml/unloadable.h` `struct caml_unloadable_unit`:

- `code_blocks` / `num_code_blocks` — `Code_block` heap-shape
  addresses for each function in the unit.
- `data_blocks` / `num_data_blocks` — tracked Global static block
  addresses (per B.3).
- `text_ranges` / `num_text_ranges` — flat `[s0, e0, s1, e1, ...]`
  per-function text ranges; `text_range_fragnums` holds the matching
  code-fragment registration handles.
- `frametable` — pointer to the unit's frame table (or NULL).
- `gc_roots` — pointer to the unit's `gc_roots` table (registered via
  `caml_register_dyn_globals` so the global-root scan walks the unit's
  static blocks; see B.1).
- `on_unload` — callback invoked under STW after the unit has been
  removed from the registration list. The JIT loader uses this to
  `free` the buffer and the registration struct.
- `loader_data` — opaque to the runtime.

`caml_register_unloadable_unit` (called from
`jit_register_unloadable_unit_native` in
`external/ocaml-jit/lib/jit_stubs.c`):

1. Normalises every block header in `code_blocks` and `data_blocks`
   to `caml_allocation_status()` (`MARKED` if marking is in progress,
   `UNMARKED` otherwise — see the "born-marked" invariant in
   strategy point 8). The header store is atomic
   (`atomic_store_relaxed` on `Hp_atomic_val`) for consistency with
   the concurrent marker's conventions.
2. Registers each `text_range[i]` as a code fragment so
   `caml_find_code_fragment_by_pc` returns true for any PC in the
   unit's text. Digests are unused (`DIGEST_IGNORE`). Immediately
   after registration, the per-fragment field
   `cf->owner_unloadable_unit` is set to this unit, so subsequent
   skiplist lookups for any PC in the unit's text return a fragment
   whose owner pointer is the unit itself. This avoids a second
   O(units) linear scan on the F.3 hot path; see F.3 below.
3. Registers `frametable` via `caml_register_frametables`.
4. Registers `gc_roots` via `caml_register_dyn_globals`.
5. Links the unit into the global registration list and bumps
   `caml_unloadable_units_live_count` (an atomic counter read on the
   major mark hot path to fast-exit `F.1` when no units are live).

Step ordering matters: steps 2–4 publish the unit to globally
visible tables before step 5 makes it visible on the units list. A
major-GC cycle starting on another domain in this window scans via
`gc_roots` and the frametable, sees the unit's blocks, and marks
them; the end-of-cycle unload pass does not visit this unit (it is
not yet on `units_head`) but that is correct — it has just been
registered as born-marked and survives the current cycle by
construction.

The window between `caml_register_unloadable_unit` returning and
the JIT loader's `caml_callback` into the entry is safe because
the unit's static blocks are born-marked, the entry's `Code_block`
has no dep fields (so a scan in the window cannot dereference a
not-yet-populated field), and no thread has yet entered the unit's
text so F.2 cannot land in the new range.

`units_mutex` is statically initialised
(`CAML_PLAT_MUTEX_INITIALIZER`); the previous lazy init-on-first-use
pattern had a multi-domain race that is no longer possible.

## F. Mark phase changes

Three injections, all routed through helpers in `runtime/caml/unloadable.h`:

```
caml_darken_code_block_for_entry(state, entry):
    code_block = *((value*)entry - 1)
    caml_darken(state, code_block, NULL)

caml_visit_code_block_for_entry(f, fdata, entry):
    code_block = *((value*)entry - 1)
    f(fdata, code_block, &code_block)
```

The slot pointer is `&code_block` (a stack local), not `NULL`: the
`scanning_action` family includes `oldify_one` which writes
`*p = v` unconditionally for non-young values, so a NULL slot would
crash a minor GC that walks an unloadable frame. The static back-pointer
at `entry - 1` lives in `.text` and cannot serve as the slot. The
no-op write through the local is the correct way to satisfy the
contract: `caml_darken` ignores `p`, `oldify_one` writes `v` back
into the local (which is then discarded), and the compactor does not
move static data.

Standard `caml_darken` then scans the `Code_block`'s fields,
recursively darkening dep `Code_block`s and dep data blocks.

### F.1 Closure scan injection

In `runtime/major_gc.c` (the `Closure_tag` arm of both the
fast-path block-pop loop and `mark_stack_push_block`), after computing
`env_offset` from `Start_env_closinfo`:

```
caml_darken_unloadable_code_blocks_in_closure(Caml_state, block);
```

The helper fast-exits via a relaxed atomic load on
`caml_unloadable_units_live_count` when no unloadable units are
registered, so the cost on the hot mark path for non-unloadable
programs is one cache-resident load and a conditional branch.

When at least one unit is live, the helper walks each function slot
in the closure prefix:

```
slot_start = 0
while slot_start + 2 <= env_start:
    closinfo = Field(closure, slot_start + 1)
    arity = Arity_closinfo(closinfo)            // signed
    slot_size = (arity > 1 || arity < 0) ? 3 : 2
    // ^ Curried with 0 or 1 param  => Full_application_only (size 2);
    //   curried with >= 2 params or tupled (negative arity)
    //                              => Full_and_partial_application (size 3).
    if slot_start + slot_size > env_start: break
    if Unloadable_closinfo(closinfo):
        code_offset = (slot_size == 2) ? slot_start : slot_start + 2
        caml_darken_code_block_for_entry(Caml_state,
                                         Field(closure, code_offset))
    if Is_last_closinfo(closinfo): break
    slot_start += slot_size + 1   // skip past the infix header
```

Slot size is read from `Arity_closinfo`, **not** by probing for an
infix header at `slot_start + 2/3`. A single-function closure with a
non-scannable env (e.g. a captured `int` or `float#`) has no infix
header following the slot, yet the prefix extends past `slot_start +
2` because non-scannable env words sit between the slot and the
scannable env at `env_start`.

Minor GC has **no** equivalent injection: static blocks aren't in the
minor heap, so the minor scan never reaches them.

### F.2 Stack return-address scan

In `caml_scan_stack` (`runtime/fiber.c`), for each frame:

```
if frame_is_unloadable(d):
    cf = caml_find_code_fragment_by_pc(retaddr)
    if cf != NULL:
        caml_visit_code_block_for_entry(f, fdata, (value)cf->code_start)
```

Each function in an unloadable unit has its own code fragment whose
`code_start` is the function entry, so the back-pointer at
`code_start - 1` is the function's `Code_block`. This fires before
the regular `live_ofs[]` scan so the `Code_block` is reached even if
the frame holds no other refs.

There is no separate F.2 site in `signals_nat.c`; stack walking is
centralized in `caml_scan_stack`.

### F.3 Stack code-pointer slot scan

After the regular `live_ofs[]` scan, also in `caml_scan_stack`:

```
if frame_has_code_ptr_slots(d):
    caml_visit_frame_code_ptr_slots(f, fdata, d, sp, regs)
```

`caml_visit_frame_code_ptr_slots` (in `runtime/caml/unloadable.h`)
walks the parallel `code_ptr_live_ofs[]` array, reads each slot, and
for any whose target lies in a registered code fragment whose owning
CU is unloadable, darkens the `Code_block` at `*((value*)cp - 1)`.
The check is a single O(log n) skiplist lookup
(`caml_find_code_fragment_by_pc`) followed by a field read
(`cf->owner_unloadable_unit`): a NULL fragment means non-registered
text; a non-NULL fragment with a NULL owner means non-unloadable
registered text (main program, Dynlink), for which dereferencing the
back-pointer at `entry - 1` would read arbitrary memory; a non-NULL
fragment with a non-NULL owner is unloadable text and is safe to
deref. `caml_find_unloadable_unit_by_pc` is a thin wrapper on the
same lookup, retained for callers (e.g. future stack-walk paths)
that do not already have the fragment pointer in hand.

A DEBUG `CAMLassert` checks `cf->code_start == cp` before
dereferencing: the only in-tree producers of `Code_pointer`-typed
values are closure Field-0 loads and static `Csymbol_address`
references to function entries, so a `Code_pointer` slot should
always hold a function-entry PC. If a future code path ever
produces a non-entry PC into a `Code_pointer` slot (e.g. via
arithmetic on a code pointer), this assertion fires before the
unsafe deref.

### F.4 Mark propagation semantics

There are no special mark domains. Marking a `Code_block` darkens its
fields via the standard scan — pointers to other `Code_block`s and to
data blocks. All recursion is the standard mark loop.

## G. End-of-cycle pass

`caml_unloadable_check_and_unload_dead` is called from
`cycle_major_heap_from_stw_single` (`runtime/major_gc.c`), before
`caml_cycle_heap_from_stw_single` rotates the heap state. Caller is
in STW; the function takes the unloadable-units lock internally:

```
marked = caml_global_heap_state.MARKED   // cycle-N's MARKED bits
for each registered unit u:
    live = any code_blocks[i] or data_blocks[i] has status `marked`
    if !live:
        unlink u from the list
        defer to the to_unload list
    else:
        rewrite every block in u to MARKED  // rotation will map it
                                             // to UNMARKED in cycle N+1
        // Already-MARKED blocks need no update; this uniform write is
        // the simplest way to leave the unit consistent.
```

The surviving-unit header rewrite uses `atomic_store_relaxed` on
`Hp_atomic_val` for consistency with `normalize_block_color` and with
the rest of the runtime's header-write conventions. The caller is in
STW (no other domain is running mutator or marker code), so a plain
non-atomic store would also be visible to all domains once the STW
ends — but matching the atomic-header-store convention avoids a
footgun if this code is ever called from a non-STW context. The
relaxed ordering is sufficient: STW provides the happens-before edge
with respect to the imminent cycle rotation.

After releasing the units lock (so code-fragment skiplist mutexes and
the loader callback do not nest with it), each deferred unit is
finalised under STW:

- Remove its code fragments via `caml_remove_code_fragment` (the
  fragment objects go on the codefrag garbage list and are freed by
  `caml_code_fragment_cleanup_from_stw_single`).
- Unregister its frame table via
  `caml_unregister_frametable_from_stw_single`.
- Unregister its `gc_roots` via `caml_unregister_dyn_global`.
- Invoke `u->on_unload(u)` (the JIT loader's hook frees the buffer
  and the unit struct).

`caml_iter_unloadable_units` is also exposed for read-only iteration
(under STW + the units mutex).

### G.1 Compactor robustness pair

The compactor evacuates pool blocks, sets their old headers to
`caml_global_heap_state.MARKED`, writes `Field(v, 0)` to a forwarding
pointer at the new location, and then walks every heap pointer
(`compact_update_value` in `runtime/shared_heap.c`) updating
references via that forwarding pointer:

```
if (Has_status_val(v, NOT_MARKABLE))     return;        // safe
if (Whsize_val(v) <= SIZECLASS_MAX) {
  if (Has_status_val(v, caml_global_heap_state.MARKED)) {
    *p = Field(v, 0) + infix_offset;                    // forward
  }
}
```

JIT static blocks live in the JIT-allocated buffer, not in any
pool, so the evacuator never visits them. But the `Has_status_val(v,
MARKED)` test fires on any block — pool or not — whose color bits
match the new cycle's MARKED. If a JIT static block were ever to
appear with that color at compaction time, the compactor would
silently read its first data word as a forwarding pointer and
corrupt every heap pointer to it.

Today the post-rotation status of every surviving unloadable block
is UNMARKED (the end-of-cycle pass writes survivors to the pre-
rotation MARKED bits; the rotation maps those bits to UNMARKED), so
the MARKED check in `compact_update_value` misses them. The safety
argument is correct but load-bearing on the precise interleaving of
the end-of-cycle pass, the cycle rotation, and the compactor —
any reordering would silently re-introduce the corruption.

To make the compactor's behaviour toward JIT static blocks robust
*by construction*, the runtime flips registered unloadable static
blocks to `NOT_MARKABLE` for the duration of `caml_compact_heap`
and restores them to `UNMARKED` after:

```
if (compacting) {
  Caml_global_barrier_if_final(participating_count) {
    caml_unloadable_pre_compact();   // UNMARKED  -> NOT_MARKABLE
  }
  caml_compact_heap(...);            // takes NOT_MARKABLE early-out
  Caml_global_barrier_if_final(participating_count) {
    caml_unloadable_post_compact();  // NOT_MARKABLE -> UNMARKED
  }
}
```

Both `caml_unloadable_pre_compact` and `caml_unloadable_post_compact`
walk `units_head` under `units_mutex`, applying
`atomic_store_relaxed(Hp_atomic_val(v), With_status_hd(hd, status))`
to each unit's `code_blocks` and `data_blocks`. Code_blocks are
strictly speaking not reachable via standard heap walks
(`Code_block`s are only reached via the back-pointer at `entry - 1`
in `.text`, not via value-typed fields), so flipping them is
defensive; data blocks are the load-bearing target. The cost is
O(total unloadable symbols) per compaction, bounded and
infrequent.

Calls are inserted in `cycle_all_domains_callback`
(`runtime/major_gc.c`), gated on the `compacting` decision returned
by `should_compact_from_stw_single`. The placement is *after* the
heap-state rotation in `cycle_major_heap_from_stw_single` and
*after* `caml_verify_heap_from_stw` (so verify still sees blocks in
UNMARKED state and its `Has_status_val(v, UNMARKED)` assertion
holds). Each flip is wrapped in `Caml_global_barrier_if_final` so it
runs once across the participating domains; the barriers also
provide the happens-before edges needed before and after
`caml_compact_heap`.

The post-flip restores blocks to `caml_global_heap_state.UNMARKED`
(the current — post-rotation — UNMARKED), so the next major mark
cycle finds them ready for the standard `caml_darken` path. This
matches what surviving unloadable blocks would have had absent the
pair.

## H. Open issues

1. **Reset cost**: walking every block in every unloadable unit at
   end of cycle to rewrite marks is O(total unloadable symbols).
   Bounded and infrequent (major GC). Measure if it shows up.
2. **Dynlink applicability**: same machinery should serve Dynlink
   units that opt in to unloadability. The `is_unloadable` flag in
   `Code_metadata` is the natural opt-in; the runtime registration
   path would mirror `caml_register_unloadable_unit`.
3. **LLVM backend does not implement unloadable codegen**. The
   `Cmm.Unloadable` codegen option is recognised but raises
   `Misc.fatal_error` in `backend/llvm/llvmize.ml`. Wiring LLVM up
   to honour the bit (back-pointer at `entry - 1`,
   `FRAME_DESCRIPTOR_UNLOADABLE` /
   `FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS` flags, and
   `code_ptr_live_ofs` slot enumeration) is future work.

## Testing

Tests live in `testsuite/tests/quotation/eval/`. They use
`Eval.eval`'s native JIT path directly (no separate harness). Every
test header carries `runtime5;` (concurrent marker required) and
`no-address-sanitizer;` (the `Eval.eval` JIT relies on
`jit_supports_unloading` which is false under ASan on Linux).

The CI configuration `.github/workflows/build.yml` lists every
unloading test under `disable_testcases:` for the musl matrix entry
(unloading is unsupported on musl per A.0).

Test-only observability is exposed by `Eval`:
- `Eval.unloadable_units_registered_total : unit -> int`
- `Eval.unloadable_units_unloaded_total : unit -> int`

These wrap the runtime counters maintained in
`runtime/unloadable.c`.

The runtime also honours an `OCAML_UNLOADABLE_DEBUG` environment
variable: when set non-empty / non-zero, each registration, every
end-of-cycle check, and every unload prints a one-line summary to
stderr. Useful for diagnosing test failures.

Coverage axes currently exercised:

- Smoke (`unload_smoke`, `eval_test_unload`): compile, drop, GC,
  assert unload.
- Reachability via closure (`unload_reachability`).
- Multiple closures in one unit / mutual recursion
  (`unload_mutual_rec`, `unload_letrec`, `unload_letrec_three`,
  `unload_letrec_infix_stress`): infix-tag pointers held alone, mixed
  slot sizes, 2-/3-/4-way mutual recursion, `let rec` with mixed env.
- Closure shapes (`unload_multi_arg`, `unload_partial_app`,
  `unload_tupled`, `unload_value_slots`, `unload_self_rec`,
  `unload_nested_closures`, `unload_closure_in_env`).
- Captured env (`unload_static_constants`, `unload_static_let`,
  `unload_mixed_env`, `unload_floats`, `unload_unboxed_nums`):
  constant lifted data, non-constant `let x = … in …`, mixed
  scannable + non-scannable env, boxed `float`, unboxed `float#` /
  `int64#`.
- Cross-unit (`unload_inter_unit`, `unload_letrec_inter_unit`):
  closure from unit U_A captured in a curry stub closure from unit
  U_B; transitive reachability through heap.
- Stress / many cycles (`unload_many_cycles`, `unload_concurrent_units`,
  `unload_large_static`).
- Exceptions and returned data (`unload_exceptions`,
  `unload_returned_data`).
- Allocation safe-points under `-g`
  (`unload_alloc_with_debug`): exercises the
  `flags land 3 = 3` mask in `emit_frame` for frame descriptors
  carrying both `Dbg_alloc` and the new `UNLOADABLE` /
  `HAS_CODE_PTR_SLOTS` flags.
- Mixed-arity closure prefixes
  (`unload_closure_prefix_mixed_arity`): stresses F.1's
  arity-driven slot-size inference and the Infix_tag DEBUG
  assertion.
- F.3 gating on non-unloadable code fragments
  (`unload_code_ptr_slot_gating`, C-side): confirms that
  `caml_visit_frame_code_ptr_slots` does not deref `entry - 1` for
  PCs that lie in non-unloadable registered fragments.
- Frame-table layout sanity
  (`unload_frametable_layout_sanity`, C-side): walks every emitted
  frame descriptor and validates the `code_ptr_live_ofs` section
  parses in bounds.
- Major GC during eval'd module initialisation
  (`unload_entry_gc_during_init`): exercises C.2's zero-dep entry
  Code_block invariant by forcing GCs while a recursive same-CU
  helper is on the stack.

## Reference: key source locations

| Concern | File |
|---|---|
| Closinfo layout & macros | `runtime/caml/mlvalues.h` |
| `Code_block_tag` reservation | `runtime/caml/mlvalues.h` |
| `Code_metadata.is_unloadable` | `middle_end/flambda2/terms/code_metadata.ml` |
| `pack_closure_info`, `closure_info'`, `unit_block_header`, `emit_unit_block`, `code_block_symbol_name`, `unloadable_data_blocks_symbol_basename`, `unloadable_code_blocks_symbol_basename`, `register_unloadable_code_block_entry`, `fail_if_called_indirectly_*` | `backend/cmm_helpers.ml` |
| Set-of-closures emit (closinfo + curry stub address) | `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml` |
| `Code_block` emission, same-CU symbol filter, entry Code_block | `middle_end/flambda2/to_cmm/to_cmm_code_blocks.ml` |
| `unloadable_data_blocks` and `unloadable_code_blocks` sentinel emission | `middle_end/flambda2/to_cmm/to_cmm.ml` |
| Frame descriptor flags + accessors | `runtime/caml/frame_descriptors.h` |
| Frame descriptor parser (skip code-ptr-slot section) | `runtime/frame_descriptors.c` |
| Frame descriptor emit (record + bits) | `backend/emitaux.ml` |
| Function prologue back-pointer emit | `backend/{amd64,arm64}/emit.ml` |
| `Code_pointer` machtype | `backend/cmm.ml` |
| `Clflags.unit_is_unloadable` | `utils/clflags.ml(.mli)` |
| `Eval.eval` entry point + observability externals | `otherlibs/eval/eval.ml(.mli)` |
| `jit_supports_unloading`, `jit_memalign`, `jit_register_unloadable_unit_native` (reads both sentinels, dedupes entries), `jit_unit_on_unload` (page-aligned `mprotect`) | `external/ocaml-jit/lib/jit_stubs.c` |
| JIT load path + exact-name lookup of `unloadable_code_blocks` and `unloadable_data_blocks` sentinels + `Clflags.unit_is_unloadable` toggle | `external/ocaml-jit/lib/jit.ml` |
| `Externals.supports_unloading` | `external/ocaml-jit/lib/externals.ml(.mli)` |
| Major-GC closure scan F.1 injection | `runtime/major_gc.c` |
| Stack scan F.2 / F.3 injections | `runtime/fiber.c` (`caml_scan_stack`) |
| Closure slot walker, code-block back-pointer helpers, code-ptr-slot walker | `runtime/caml/unloadable.h` |
| Registration, end-of-cycle pass, "born-marked" normalisation, debug tracing, `caml_unloadable_units_live_count` | `runtime/unloadable.c` |
| `cf->owner_unloadable_unit` field, initialisation to NULL on registration | `runtime/caml/codefrag.h`, `runtime/codefrag.c` |
| End-of-cycle pass call site (before rotation) | `runtime/major_gc.c` `cycle_major_heap_from_stw_single` |
| Compactor robustness flip (`caml_unloadable_pre_compact` / `caml_unloadable_post_compact`) | `runtime/unloadable.c`, call site `runtime/major_gc.c` around `caml_compact_heap` |
| Allocation status convention | `runtime/caml/shared_heap.h` `caml_allocation_status` |
| Heap colors | `runtime/caml/shared_heap.h` |
| Test predicate `no-address-sanitizer` + musl `disable_testcases` | `.github/workflows/build.yml` |
| Existing quotation tests | `testsuite/tests/quotation/eval/` |
