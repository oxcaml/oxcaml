# Compilation-unit unloading for JIT-emitted metaprogramming code

## What

`Eval.eval` (in `otherlibs/eval/`) compiles a quoted OCaml expression to a
fresh native compilation unit at runtime, allocates a buffer for the unit's
text and data, applies relocations, and runs the unit's initialiser. Without
this change those buffers would leak for the lifetime of the process — they
cannot be `free`d while any closure, return address, or other reference
reaches into the unit's text or data.

This PR adds a runtime mechanism that lets the major GC determine when an
entire JIT-emitted CU has become unreachable and reclaim it as a single
unit at the end of a major cycle. The mechanism is general; the only
in-tree consumer today is the metaprogramming JIT, but a future Dynlink
opt-in would slot in through the same runtime registration path.

## Why

Long-running metaprogramming workloads compile thousands of small
quotations. Without reclamation, every compilation grows the resident
process size monotonically. Existing metaprogramming benchmarks already
show this as a noticeable footprint problem; production use of `Eval.eval`
would make it unbounded.

## How — overview

Each unloadable CU becomes a **scannable region with explicit dependency
edges**. The standard mark phase keeps the unit's code and data alive
transitively; nothing in the GC needed a new mark domain or a new pass over
the heap. The only new GC work is:

1. A short walk over each closure's function slots, looking for slots whose
   code lives in an unloadable CU (one new closinfo bit identifies these).
2. A check on each stack frame whose return address is in unloadable code
   (one new frame-descriptor bit identifies these).
3. A scan over a small number of "transient code pointer" slots in each
   frame — values like `closure.code` that are in flight from a closure
   Field-0 load to an indirect call.
4. A new end-of-cycle pass that asks, for each registered unit, "did
   anything in the unit get marked this cycle?", and either resets the
   unit's marks for the next cycle or removes the unit and invokes its
   loader's free callback.

The dependency graph itself lives in the **heap, in standard heap-shaped
blocks**. For every function in an unloadable CU the compiler emits a
`Code_block` — a regular OCaml block (new tag 243) in the unit's `.data`
section, whose scannable fields are the function's direct unloadable
dependencies (other functions' `Code_block`s, plus same-CU static data
blocks). Marking a `Code_block` darkens its dep fields via the standard
mark loop, so transitive reachability falls out of existing GC mechanics.

Each function's `Code_block` address is stored in **one machine word in
`.text`, immediately before the function's entry label**. Set at link/load
time, never written again — `.text` stays RX. The GC paths above read this
back-pointer to map "function entry I'm looking at" to "Code_block I need
to darken".

Static data emitted by an unloadable CU uses white (UNMARKED) headers for
Global symbols (which the GC's standard mark scan can then darken via heap
references), and black (NOT_MARKABLE) headers for Local symbols (which are
only reachable transitively from a Global ancestor in the same unit). The
unit's `gc_roots` table is registered with the runtime's global-root scan;
this scan walks the **fields** of each registered block, not the block
itself — which is the subtlety that lets the unit become unreachable in
the first place.

## Tricky cases

### Keeping a running unit alive when no heap reference reaches it

The hardest case: a `<[ let x = Buffer.create 16 in fun () -> x ]>` quote
returns a `unit -> Buffer.t` closure. While the eval'd initialiser is
running, no closure for it exists yet — every reference to the unit's text
lives only on the call stack.

This works because each unloadable frame's return address darkens the
running function's `Code_block` via the back-pointer at `entry - 1`. Every
function transitively called from the entry is on the stack above the
entry, so every active function's `Code_block` is darkened by the frame at
its return PC.

The entry function (module initialiser) itself is on the bottom of the
stack while the unit is running. Its `Code_block` exists with **zero
dependency fields** — the design choice that lets it not have to enumerate
all top-level functions and static data of the unit. While the entry is
running, the same stack-walk argument darkens every function it has called.
Once the entry has returned, nothing reaches the entry's `Code_block` —
which is correct: the entry is no longer needed.

### Born-marked units registered mid-cycle

The concurrent major marker on another domain can be in the middle of a
mark cycle when `Eval.eval` registers a new unit. The shared-heap allocator
already handles this for ordinary heap blocks by stamping mid-cycle
allocations as `MARKED` (so the marker skips them and they survive the
cycle by construction). The same convention is applied at registration to
every block in the new unit. Without this, a curry-stub closure allocated
right after registration would be MARKED-by-allocator while the static
block it references would be UNMARKED-by-emission, and the closure-Field-0
darken path would never run because the closure itself is skipped by the
marker.

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

End-of-cycle cost is O(total unloadable symbols) per major GC for the
mark-reset pass. Bounded and infrequent. Worth measuring under real
workloads, but no obvious scaling cliff.

## Configuration

- `Clflags.unit_is_unloadable : bool ref` — internal compiler flag, not
  CLI-exposed. `Eval.eval` saves, sets, and restores it around its
  `compile_implementation` call. Drives `.is_unloadable` propagation
  through Flambda 2's `Code_metadata` and from there to every emit-time
  consumer.
- `OCAML_UNLOADABLE_DEBUG=1` — runtime env var, prints one line per
  registration, end-of-cycle check, and unload to stderr. Useful for
  diagnosing test failures.

## Observability

Two cumulative counters are exposed via the `Eval` module for test
assertions and for production telemetry hooks:

```
val Eval.unloadable_units_registered_total : unit -> int
val Eval.unloadable_units_unloaded_total   : unit -> int
```

The live count is the difference.

## Testing

A new directory `testsuite/tests/quotation/eval/unloading/` adds 23 tests
exercising the mechanism end-to-end. Coverage axes:

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
  helper deeply on the stack, exercising the zero-dep entry `Code_block`
  invariant.

All tests carry `runtime5; no-address-sanitizer;`. The musl CI matrix
entry disables the directory entirely (unloading is unsupported on musl
per above).
