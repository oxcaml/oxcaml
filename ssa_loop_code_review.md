# Code review: backend/ssa/loop/

Full correctness-focused review of the SSA loop analyses and transformations,
carried out after the refactoring described in `ssa_loop_refactoring.md`. All
findings pre-date that refactoring (checked against the pre-refactor code
where it matters); the new pure modules make the arithmetic fixes local.

## Verdict

The architecture is sound and most of the subtle arguments check out under
adversarial reading. Verified end-to-end:

- The Fourier-Motzkin core: exact rational elimination, correct overflow
  guards (`add_ovf`/`mul_ovf`/`neg_ovf`), conservative directions all
  pointing the right way (`feasible` overflows to `true`, `entails` to
  `false`), and the integer-tightened negation `goal <= -1` in `entails`.
- LFTR's overflow proof, including the `ratio = 1` rejection via
  `Int64.to_int` truncation and the mod-2^64 cancellation identity: the IV
  range argument `i in [i_init, bound + step)` holds exactly at 64 bits
  because `bound <= max_int64 / ratio` and the step is a machine int, so
  `ratio * (i - bound)` stays in signed range at every test evaluation.
- The `guards_at` edge-dominance criterion (successor dominates target and
  has the branch source as sole predecessor) and its per-iteration validity
  for loop-variant atoms.
- `dead_induction_var`'s whole-graph use approval — airtight given that
  `Tuple` never appears in a well-formed graph (confirmed in
  `ssa_intf.ml`), so `args_of_instr`'s empty result for it is a dead case.
- Fusion's `clonable`: leaves must bottom out at `head` or constants;
  `Alloc` is impure so boxed intermediates are rejected; loads can only be
  over `head`-derived addresses, and chain-wide store-freedom makes cloning
  them time-shift-safe. The odd-parity chain algebra (a chain of k
  reversing maps is a single reversing map of the composition iff k is odd)
  is correct, as is the exit re-targeting.
- Strength reduction's transform is exact mod 2^64 (the new parameter is an
  identity-preserving recurrence regardless of overflow) — except for
  finding 1 below.
- The in-place `set_terminator` staleness in `bounds_check_elim` is benign:
  edge removal only strengthens dominance, and stale predecessor supersets
  make `edge_dominates` more conservative, never less.

## Findings (ranked)

### 1. Coefficient arithmetic wraps at OCaml-int width while modeling 64-bit machine values — unsound, compile-time reachable

`Affine.add`/`scale` (used by `Affine_expr.to_affine` during fact
construction) and `Affine_expr.coeff_of_atom` compute coefficients in OCaml
`int` (63-bit wrap), but the SSA values they model wrap at 64 bits. The two
widths disagree for true coefficients in `[2^62, 2^64)`, and both passes can
be driven into that zone by chained shifts/adds even though each single
shift is gated (`k < 16` for linearize, `k < 62` for strength reduction):

- Bounds-check elimination: for
  `v = ((((x lsl 15) lsl 15) lsl 15) lsl 15) lsl 3` the true machine value
  is `x * 2^63 mod 2^64` but the recorded affine form is `0 * x`, i.e. the
  model believes `v = 0`. A dominating runtime-true guard `v <= -1` (true
  when `x` is odd, since `2^63` is negative as a signed 64-bit value) then
  contributes the fact `-1 >= 0` — a contradictory fact set entails
  everything, so any bounds check in scope gets eliminated. No long runtime
  needed.
- Strength reduction: a derived value with true coefficient `2^63 + 4`
  (e.g. `((i lsl 61) + i) lsl 2`) records `c = 4`, passes the
  `abs c >= 2` gate, and the replacement IV increments by `4 * step`
  instead of `(2^63 + 4) * step` — the derived value diverges in the sign
  bit from the first iteration.

Fix: do coefficient arithmetic with the checked operations (they already
exist in `Fourier_motzkin.Affine`) or in `int64`/`nativeint`, and on
overflow atomize (linearize) or reject the candidate (strength reduction).
Since the refactoring this is confined to `affine_expr.ml` plus the two
`Affine` entry points — a small patch.

### 2. `continue_terminates` ignores wrap-around, so empty-loop deletion can erase genuinely infinite loops

Of the accepted (direction, comparison) pairs, only `(Up, Clt, step 1)`,
`(Down, Cgt, step -1)`, and `Ceq` are wrap-proof. Counterexamples for the
rest: `while i <= n` with `n = max_int` and step 1 (continue at
`i = max_int`, `i + 1` wraps to `min_int`, loops forever — and this is the
OCaml semantics of that source program); `Up, Clt` with step >= 2 and
`n = max_int`. `Termination.analyze` reports `Terminates`, and
`delete_empty_loops` then removes a divergent loop, changing program
semantics from hanging to terminating.

Compiler-generated `for`-loops are immune (int64 counters bounded by
tagged-int stops cannot reach the 64-bit wrap point), so this needs a
`while`-loop and ~2^62 iterations to witness — real in principle,
unobservable in practice.

Fix options: restrict to the wrap-proof triples; or discharge
`bound <= max - step` via Fourier-Motzkin the way LFTR's
`scaled_no_overflow` already does; or record deliberately (with a comment)
that deleting divergent pure loops is accepted policy.

### 3. `iv_lower_fact` is false under 63-bit wrap (same class, bounds-check side)

The invariant "increasing IV implies `param >= min(inits)`" fails once the
IV wraps. Concrete (2^62-iteration) scenario: `i` from 0 step 2, header
guard `i < n` with opaque `n = max_int`, body does
`if i < len then a.(i)`; both entailments succeed, the check is removed,
and after wrap `i = min_int` reaches the load where the original program
raised `Invalid_argument`. Notably the upper fact survives wrap; only the
lower one lies.

LFTR proves its overflow side conditions from guards; the bounds-check IV
facts assert theirs for free — an asymmetry in proof discipline worth
closing the same way (`u <= max_int - step` as a side obligation) or
documenting alongside the existing 63-bit-operand comment in
`try_eliminate`.

### 4. Fusion's `classify` never inspects addressing offsets

`head`, `tail`, `car`, and the cdr store are identified purely by
value-identity heuristics: `head` is "the single cursor load that isn't
`tail`", `tail` is "the cursor load passed as next cursor", car is "the
unique non-accu non-const stored value" — no `Arch` addressing mode is
ever examined. A body whose single non-tail cursor load is the header
word, or whose "tail" load actually reads the car field (a car-walking
loop), satisfies every check yet breaks the rev_map interpretation the
fusion relies on. No such shape arises from real `List.rev_map` lowerings
as far as I can tell, but nothing in the pass excludes it.

Fix: check offsets explicitly (head at the field the producer's car store
writes, tail at the cdr offset, header word excluded) — an
arch-parameterized helper in the spirit of `specific_operation_as_affine`.

### 5. Fusion checks purity of the body block but not the header block

`classify` runs `body_side_effect_free` on the body block only; the
two-block loop's header instructions are never checked
(`delete_empty_loops` checks every block in the loop — the asymmetry is
visible side by side). An effectful op scheduled into a later loop's
header would be silently discarded by fusion. One-line fix.

### 6. Validator gaps for invariants these passes rely on

`ssa_validate` checks neither (a) Goto arity against the target's param
count, nor (b) that `Branch`/`Switch` targets have no live params. Both
invariants are load-bearing here: `bounds_check_elim` and
`delete_empty_loops` rewrite `Branch` -> `Goto ... args [||]` (sound only
if the former Branch target is parameterless), and strength reduction's
latch rewrite assumes every back edge is a `Goto` (guaranteed only because
a `Branch` cannot bind a header's params). Adding both checks would
convert silent miscompiles into validator failures under `-ssa-validate`.

### 7. `iv_upper_fact.verify` depends on right-to-left argument evaluation

`entails (guards_at ... @ !side) (Affine.sub u (linearize ctx side iarg))`
— the goal's linearization pushes side facts that the facts argument reads
via `!side`. OCaml's de facto right-to-left evaluation makes the side
facts land in time; a well-meaning refactor that let-binds the facts first
would silently weaken the analysis (conservative, but invisible). Bind the
goal explicitly before constructing the fact list, as `try_eliminate` and
`scaled_no_overflow` already do.

### 8. Minor / hygiene

- `is_loop_invariant` returns `true` for `Tuple` / `Push_trap` /
  `Pop_trap` / `Stack_check` / `Name_for_debugger`. All unreachable as
  argument values, but `true` is the dangerous default for a soundness
  predicate; prefer `false` (`bounds_check_elim.atom_invariant` already
  chose the conservative side, and the comment there records why the two
  must not be merged).
- `analyze_loop` indexes predecessor `args.(index)` without a length guard
  while `iv_upper_fact.arg_to_header` checks `Array.length args > k` —
  the validator implies safety, but the inconsistency invites confusion;
  pick one style.
- `Loop_chains.chains`/`extend` and `consumes`'s walk terminate only
  because reachable graphs cannot contain single-predecessor cycles; a
  visited-set (or fuel) in `extend` is cheap insurance against a malformed
  graph hanging the compiler rather than crashing it.
- Fourier-Motzkin has no size caps; elimination is worst-case doubly
  exponential, and a guard-heavy function could blow up compile time. A
  constraint-count budget with conservative bail-out (`true`/`false` per
  the existing asymmetry) would bound it.
- `fits_int`'s truncate-and-compare idiom relies on `Nativeint.to_int`
  wrapping; fine, but a one-line comment would help.

## Suggested fix order

1. Finding 1 (small, and the only one reachable at human timescales).
2. Findings 6, 5, 7 (cheap hardening).
3. Finding 4 (needs a small arch hook).
4. Findings 2 and 3 after a policy decision: whether to buy soundness at
   the 2^62-iteration horizon with extra Fourier-Motzkin obligations, or
   to document the wrap caveat instead.
