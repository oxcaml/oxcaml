# Callee-Register Savings Estimate

## Background

OCaml's current calling convention treats every register as caller-saved: before
a direct call, the allocator must assume that the callee clobbers every register
in each class, so any value that is live across the call must be spilled.  In
reality, leaf functions (those that never call another OCaml function) often
clobber only a small subset of registers.

The work recorded here adds infrastructure to measure how much register-save
traffic could be eliminated if the allocator knew — and exploited — the actual
clobber set of each leaf callee.

## Infrastructure Added

### Callee clobber sets in `.cmx` files (`backend/callee_regs_collector.ml`)

After register allocation, `Callee_regs_collector.cfg` computes the set of
physical registers written by a function and stores it in `Compilenv` under the
function's name, from where it is serialised into the `.cmx` file.  The
information is stored as a per-register-class bitmask.

A function qualifies as a *leaf* if its CFG contains no `Call` or
`Tailcall_func` terminators (i.e. it never transfers control to another OCaml
function).  Functions that allocate, poll, make external C calls, or contain
probes still qualify; their register effects are captured by
`Proc.destroyed_at_basic` / `Proc.destroyed_at_terminator` and therefore appear
in the bitmask.

### Profile counters (`asmcomp/asmgen.ml`)

Two new `whole_cfg` counters are recorded for every compilation unit:

- **`callee_regs_live`** — total number of virtual (pre-allocation) registers
  live across all direct call sites that target a leaf callee with known clobber
  information.  Only registers with `loc = Unknown` are counted; pinned physical
  registers are not subject to the allocator's placement choices.

- **`callee_regs_savings`** — estimated number of those live registers that the
  allocator could keep in non-clobbered physical registers (and therefore avoid
  spilling), given the callee's actual clobber set.

Both counters are computed **before register allocation**, using the pre-allocation
liveness information from `Cfg_with_infos.liveness`.  Using post-allocation
liveness would undercount, because spilled values are already absent from
post-allocation live sets.

### Estimation formula

For each direct call site targeting a leaf callee with known clobber set, and
for each register class `c`:

| Symbol | Meaning |
|--------|---------|
| `L_c`  | number of virtual registers of class `c` live across this call |
| `R_c`  | number of registers available to the allocator in class `c` |
| `K_c`  | number of those registers actually clobbered by the callee |

With the **current** convention the allocator must treat all `R_c` registers as
clobbered, so any of the `L_c` live values may need saving.  With the callee's
actual clobber set, up to `R_c - K_c` registers are safe havens; the allocator
need only spill `max(0, L_c - (R_c - K_c))` values.

```
savings_c = L_c - max(0, L_c - (R_c - K_c))
          = min(L_c, max(0, R_c - K_c))
```

The per-call-site contribution to `callee_regs_savings` is the sum of
`savings_c` over all classes; `callee_regs_live` accumulates the corresponding
sum of `L_c`.

### Extraction script (`extract_callee_regs_counters.py`)

A Python 3 script at the repository root reads all `*.csv` profile files in a
directory and prints, for each file and in total, the accumulated values of both
counters together with their ratio.  It filters to rows whose pass name ends
with `/save_cfg` (emitted exactly once per function) to avoid double-counting
across passes.

## Results on the OxCaml Compiler Distribution

Running the compiler with profiling enabled on its own sources (1,234
compilation units) and aggregating the output with `extract_callee_regs_counters.py`
yields:

| Metric | Value |
|--------|-------|
| Compilation units | 1,234 |
| `callee_regs_live` (total) | 49,539 |
| `callee_regs_savings` (total) | 34,801 |
| Savings ratio | **70.2 %** |

In other words, roughly 70 % of the registers that are live across direct calls
to leaf functions could, in principle, be kept in non-clobbered registers rather
than being spilled — provided the allocator were given and exploited the callee's
actual clobber set.  This is a conservative lower bound on the potential benefit,
since it counts only calls to leaf callees for which clobber information is
available.

## Effect of Higher Optimisation (`O3`)

The same experiment was repeated with `OCAMLPARAM=_,O3=1` and
`BUILD_OCAMLPARAM=_,O3=1` (still 1,234 compilation units):

| Metric | Default | O3 |
|--------|---------|----|
| `callee_regs_live` (total) | 49,539 | 38,914 |
| `callee_regs_savings` (total) | 34,801 | 15,585 |
| Savings ratio | **70.2 %** | **40.0 %** |

Two effects combine here.  First, `callee_regs_live` falls from 49,539 to
38,914: O3 inlines more aggressively, eliminating a significant number of direct
call sites altogether and therefore reducing the pool of live-across-call
registers that the measurement even considers.  Second, and more striking, the
savings ratio drops from 70 % to 40 %: the call sites that survive inlining tend
to be the harder cases — callees that clobber a larger fraction of the available
registers, leaving fewer safe havens for live values.

The combined result is that the absolute savings estimate falls from 34,801 to
15,585, a reduction of roughly 55 %.  This suggests that a meaningful share of
the potential benefit identified at the default optimisation level is already
addressed implicitly by inlining at O3, but a substantial residual opportunity
(~15,000 register saves per compiler self-build) remains even at the higher
level.
