# Compilation-unit unloading for JIT-emitted metaprogramming code

## What

`Eval.eval` (in `otherlibs/eval/`) compiles a quoted OCaml expression to a
fresh native compilation unit at runtime, allocates a buffer for the unit's
text and data, applies relocations, and runs the unit's initialiser. Without
this change those buffers would leak for the lifetime of the process — they
cannot be `free`d while any closure, return address, or other reference
reaches into the unit's text or data.

This PR adds a runtime mechanism that lets the major GC determine when an
entire JIT-emitted CU has become unreachable and reclaim it. The mechanism
is general; the only in-tree consumer today is the metaprogramming JIT, but
a future Dynlink opt-in would slot in through the same runtime registration
path.

## Why

Long-running metaprogramming workloads compile thousands of small
quotations. Without reclamation, every compilation grows the resident
process size monotonically. Existing metaprogramming benchmarks already
show this as a noticeable footprint problem; production use of `Eval.eval`
would make it unbounded.

## How — overview

The GC-facing half of the design is built on **heap extensions**
(`caml_add_blocks_to_heap` in `runtime/shared_heap.c`): a region of
caller-owned memory, pre-filled with well-formed OCaml blocks, donated to
the major heap. The GC marks and sweeps those blocks exactly like other
major-heap blocks (dead blocks are individually freed in place and
coalesced), compaction updates their fields without moving them, and once
*every* block in the region is dead the region's free callback fires.

Each unloadable CU's static data — ordinary constants, closures, the
module block, and the `Code_block` dependency blocks described below — is
emitted as one contiguous run of blocks, bracketed by two symbols
(`unloadable_blocks_start` / `unloadable_blocks_end`). The unit's lifecycle
has three stages:

1. **Register** (before the unit's initialiser runs). The loader registers
   the unit's code fragments (tagged so stack scans can recognise
   unloadable PCs), a *copy* of its frame table, and its `gc_roots` as
   dyn-globals. The static blocks still carry their black (NOT_MARKABLE)
   emission headers, so the GC ignores them entirely — exactly as for AOT
   static data — and the dyn-globals scan keeps heap values stored into
   them alive during initialisation.

2. **Activate** (immediately after the initialiser returns or raises;
   `caml_activate_unloadable_unit`). The dyn-globals registration is
   dropped and the bracketed region is donated to the major heap as a heap
   extension. The extension machinery forces every block to the current
   allocation colour, so the unit survives the in-progress major cycle by
   construction; from the next cycle onwards its blocks live and die by
   ordinary marking. The two steps share no GC safe point, so there is no
   window in which init-stored heap values are unprotected.

3. **Unload** (when the whole extent is dead). The extent free callback —
   running from GC sweeping — unlinks the unit, updates the observability
   counters, and queues it; at the start of the next major cycle, under
   the STW barrier, the runtime removes the unit's code fragments,
   unregisters its frame table, and invokes the loader's `on_unload`
   callback, which frees the text/data buffer. (See "Frame tables and
   deferred unloading" below for why the split is necessary.)

Keeping the unit's *code* alive is what requires new GC work. Each function
in an unloadable CU has a **`Code_block`** — a regular OCaml block (new tag
243) in the bracketed data region whose scannable fields are the function's
direct unloadable dependencies (other functions' `Code_block`s, plus
same-CU static data blocks). Marking a `Code_block` darkens its dep fields
via the standard mark loop, so transitive reachability falls out of
existing GC mechanics. Each function's `Code_block` address is stored in
**one machine word in `.text`, immediately before the function's entry
label** — set at link/load time, never written again, so `.text` stays RX.

The mark phase darkens `Code_block`s via three paths:

1. A short walk over each closure's function slots, looking for slots whose
   code lives in an unloadable CU (one new closinfo bit identifies these).
2. A check on each stack frame whose return address is in unloadable code
   (one new frame-descriptor bit identifies these).
3. A scan over a small number of "transient code pointer" slots in each
   frame — values like `closure.code` that are in flight from a closure
   Field-0 load to an indirect call.

Consequently: if any closure, frame, or in-flight code pointer can reach a
function of the unit, that function's `Code_block` is marked every cycle
and the extent — and hence the buffer, including `.text` — stays alive.
When nothing reaches the unit any more, all of its blocks go unmarked, the
extent dies, and the free callback reclaims the whole unit. Individual
blocks that become unreachable while the unit as a whole is still live
(e.g. a helper closure only used during initialisation) are reclaimed
in place by the ordinary extent sweep — a small bonus over whole-unit
reclamation.

## Tricky cases

### Keeping a running unit alive when no heap reference reaches it

The hardest case: a `<[ let x = Buffer.create 16 in fun () -> x ]>` quote
returns a `unit -> Buffer.t` closure. While the eval'd initialiser is
running, no closure for it exists yet — every reference to the unit's text
lives only on the call stack.

Deferring activation until after the initialiser has finished makes this
window a non-problem: during initialisation the unit's blocks are not part
of the heap at all (black headers, gc_roots registered), so the GC can
neither reclaim them nor prematurely sweep blocks that the initialiser has
not yet referenced. Once activation donates the blocks, each unloadable
frame's return address darkens the running function's `Code_block` via the
back-pointer at `entry - 1`, covering code that is on the stack (e.g. a
function that re-enters unit code via a callback).

### The window between activation and the caller taking its reference

After activation, the unit is kept alive only by GC-visible references —
but the caller has not yet looked up the module block (it does so by
symbol name, invisible to the GC). The JIT pins the unit's module block in
an OCaml global (`Globals.unloadable_pin`) immediately before activation;
`Eval.eval` clears the pin once it holds the module block as an ordinary
value on its own frame.

### Indirect calls through unloadable code pointers

For an indirect call, the compiler loads `closure.code` (Field 0 of the
closure) into a register, then jumps through that register. Between the
load and the call, the value is a raw PC — not a `value`, not a heap
pointer, and not visible to either the closure-prefix scan (the closure
might already be dead by the time the spilled PC matters) or the
return-address scan (the call hasn't happened yet).

A new `Code_pointer` Cmm machtype propagates through the register allocator
on these slots. At each safepoint, the backend emits a parallel
`code_ptr_live_ofs[]` array in the frame descriptor listing which
stack/register slots hold `Code_pointer` values. The GC stack walker reads
each such slot, checks whether the target PC lies in an unloadable code
fragment, and if so darkens the `Code_block` via the back-pointer.

This is needed even for non-unloadable frames: a function in non-unloadable
code can still spill an unloadable code pointer mid-flight to an indirect
call.

The `Code_pointer` machtype is by construction register/stack-only — there
is no in-tree path that would store a `Code_pointer` value into a heap
block, and the relevant codegen arm raises `Misc.fatal_error` as a guard
against accidentally introducing such a path.

### Single-function closures with non-scannable env

The closure-prefix walker decides each function slot's size (2 or 3 words)
from the closinfo arity field, not by probing for an infix-tag header
between slots. A single-function closure with a non-scannable env (e.g. a
captured `int` or `float#`) has no infix header after the slot, yet the
prefix extends past `slot_start + 2` because the non-scannable env words
sit before the scannable env. Probing the next word for an infix tag
would misclassify these. Reading the arity from the closinfo is the
authoritative signal.

### Closures crossing CU boundaries

When a closure from unloadable unit A is captured in a curry-stub closure
allocated from unit B's code, the curry-stub closure itself has no
unloadable bit set (the stub lives in shared runtime code) but holds the
underlying unloadable closure as a value-slot env field. The standard env
scan reaches the underlying closure, and its closinfo carries the
unloadable bit, so the major-GC closure-scan darkens A's `Code_block`
through it.

### Frame tables and deferred unloading

Frame descriptors encode their return addresses as 32-bit *self-relative*
offsets (`retaddr_rel`), so a unit's frame table cannot be copied out of
its buffer (a copy is not guaranteed to land within 2GB of the code); the
table inside the buffer is registered directly. Consequently the buffer
cannot be freed until the table's descriptors have been removed from the
global hashtable, and that removal mutates shared state, so it must happen
under the STW barrier — but the extent free callback runs from concurrent
sweeping. The callback therefore only unlinks the unit and queues it
(updating the observability counters immediately); the actual unload —
code-fragment removal, `caml_unregister_frametable_from_stw_single`,
buffer free — runs at the start of the next major cycle, in the STW
single-domain section (`caml_unloadable_process_pending_unloads`). In the
interim the stale registrations are harmless: no stack can hold a PC into
a unit whose extent was found fully dead.

### Zero-size blocks

Unloadable units contain zero-wosize static blocks: empty arrays,
dependency-free `Code_block`s (including the entry function's), and
occasionally an empty module block. The heap-extension machinery supports
these — its free-block headers carry the block size, so sweeping and
consolidation are unaffected — with one caveat: a zero-wosize block's
*value* points one word past its header, so such a block must never be the
last block of the donated region (otherwise address-range classification
of the value, e.g. `Is_young` inside `caml_darken`, could misattribute it
to another memory region). The compiler therefore ends the bracketed
region with an anonymous one-field padding block; the GC frees it in
place after the first cycle, which is harmless.

One debug-runtime consequence: in cycles where the UNMARKED status
encoding is 0, a live zero-wosize tag-0 block has an all-zero header, so
the heap verifier's "header must be non-zero" assertion (which exists to
catch references to freed pool slots) had to be dropped; the adjacent
UNMARKED-status assertion still provides that coverage in the other
cycles.

## Scope and limitations

- **Native code only.** The bytecode interpreter is not touched.
- **Flambda 2 only.** The LLVM backend recognises the new `Cmm.Unloadable`
  codegen option but raises `Misc.fatal_error`; honouring it requires
  emitting the back-pointer at `entry - 1`, setting the two new
  frame-descriptor flag bits, and enumerating `code_ptr_live_ofs` slots —
  future work.
- **Dynlink is not wired up.** The `is_unloadable` flag in
  `Code_metadata` is the natural opt-in; the runtime registration path
  would mirror the JIT's.
- **Three build configurations disable unloading.** On Linux under
  musl, AddressSanitizer, or TCMalloc, the JIT's buffer allocator returns
  memory that cannot be passed to `free` (musl: static `.bss` arena;
  ASan/TCMalloc: `sbrk`). In these configurations `Eval.eval` still works
  — the unit is compiled non-unloadable and the buffer leaks. On macOS
  ASan-instrumented `aligned_alloc` is paired with an ASan-aware `free`,
  so unloading remains supported.

## Performance

The hot paths are designed to be effectively free for non-unloadable
programs:

- Closure scan: one relaxed atomic load on a process-global counter; if no
  unloadable units are registered, the prefix walk is skipped entirely.
- Stack-RA scan: only fires on frames with the `UNLOADABLE` bit set.
- Code-pointer-slot scan: only fires on frames with the
  `HAS_CODE_PTR_SLOTS` bit set (i.e. any frame holding a closure-Field-0
  load across a safepoint, in any program).
- Membership check ("is this PC in an unloadable unit?") is a single
  O(log n) skiplist lookup on the code-fragment table, plus one pointer
  read — both via existing runtime infrastructure.

Steady-state GC cost is the ordinary extent sweep from the heap-extension
machinery: proportional to the unit's block count, incremental, and
integrated with the existing sweep budget. There is no separate
end-of-cycle pass and no compaction special-casing.

## Configuration

- `Clflags.unit_is_unloadable : bool ref` — internal compiler flag, not
  CLI-exposed. `Eval.eval` saves, sets, and restores it around its
  `compile_implementation` call. Drives `.is_unloadable` propagation
  through Flambda 2's `Code_metadata` and from there to every emit-time
  consumer.
- `OCAML_UNLOADABLE_DEBUG=1` — runtime env var, prints one line per
  registration, activation, and unload to stderr. Useful for diagnosing
  test failures.

## Observability

Two cumulative counters are exposed via the `Eval` module for test
assertions and for production telemetry hooks:

```
val Eval.unloadable_units_registered_total : unit -> int
val Eval.unloadable_units_unloaded_total   : unit -> int
```

The live count is the difference.

## Testing

`testsuite/tests/heap_extent/` covers the heap-extension machinery in
isolation (basic liveness, compaction, debug runtime, many extents,
multi-domain).

`testsuite/tests/quotation/eval/unloading/` adds 23 tests exercising the
unloading mechanism end-to-end. Coverage axes:

- Smoke and reachability via closures.
- Every closure shape (single/multi-function, sizes 2 and 3, mutual rec up
  to 4-way, infix-tag stress, partial-app, tupled, mixed-arity prefix).
- Env shapes including boxed/unboxed floats and int64s.
- Cross-CU heap-pointer chains.
- Stress (many cycles, multiple concurrent units, large static data).
- Exceptions and returned heap data.
- Allocation safe-points under `-g`.
- C-side gating tests that directly call the runtime predicates with
  synthetic frame descriptors covering both unloadable and non-unloadable
  PCs.
- Major GC during eval'd module initialisation, with a same-CU recursive
  helper deeply on the stack.

All tests carry `runtime5; no-address-sanitizer;`. The musl CI matrix
entry disables the directory entirely (unloading is unsupported on musl
per above).
