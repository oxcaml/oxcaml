# region-pair-atomic: Begin_region and all its End_regions live or die together

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/region_pair_atomic.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/region_pair_atomic.ml)
(reference: `formalism/region_pair_atomic.simplify.reference`) — run with
`make -s test-one-no-rebuild TEST=flambda2/examples/formalism/region_pair_atomic.ml`.

MATCH. Exercises `INV.Simplify.RegionPairAtomic`
([`../13-soundness.md`](../13-soundness.md)) and, through it,
`S.Rewrite.Let.DeadRegion` ([`../10-simplify-rewrites.md`](../10-simplify-rewrites.md))
and `INV.Simplify.EffectfulDeletionInventory`. A single function with two
`exclave_` exits gives a `Begin_region` with **two** `End_region`s, so it witnesses
the *multi-End* atomicity the rule claims.

## The claim under test

For an ordinary region, either `ρ ∈ required_names` and the `Begin_region` binding
and ALL its `End_region`s survive, or `ρ ∉ required_names` and ALL `End_region`s
are deleted (and then the `Begin_region`, now used zero times, is deleted too) —
never a mixed outcome. `ρ ∈ required_names` iff some live non-End use of `ρ` exists,
because `flow_acc.record_let_binding` does not count `End_region`'s use of `ρ`.

## Source (Hume `region_multi_end.ml`)

```ocaml
type box = { mutable v : int }
let[@inline never] use (local_ t) = t.v

(* dead local alloc, exclave on both branches: predict ALL Ends + Begin deleted *)
let[@inline never] f b x =
  let t = local_ { v = x } in
  let _ = t in
  if b then exclave_ { v = x + 1 } else exclave_ { v = x + 2 }

(* live local alloc, exclave on both branches: predict Begin + BOTH Ends kept *)
let[@inline never] g b x =
  let t = local_ { v = x } in
  let y = use t in
  if b then exclave_ { v = y + 1 } else exclave_ { v = y + 2 }
```

## Prediction (before running)

In `f` the local `t` is dead (`let _ = t`), so its region `ρ_f` has no live non-End
use: `ρ_f ∉ required_names`, and both `End_region`s (one per `exclave_` branch) plus
the `Begin_region` are deleted. In `g`, `use t` is a live use of `t` inside the
region, so `ρ_g ∈ required_names`: the `Begin_region` and BOTH `End_region`s
survive. Never a Begin without both Ends, or an End without its Begin.

## Actual IR (verified, `regionM_simp.out`)

- `f_1_1`: no `%begin_region`, no `%end_region` anywhere; the two `exclave_`
  makeblocks allocate directly into the caller's `my_region`. Begin + both Ends
  gone — matching `ρ_f ∉ required_names`.
- `g_2_1`: `let region/377 = %begin_region (…)`, and BOTH exit continuations carry
  `let unit/… = %end_region (region/377)` (`k/126` and `k/127`). One Begin, two
  Ends, all kept — matching `ρ_g ∈ required_names` (`use t` recorded a live use).

Neither function ever shows a Begin without both matching Ends.

## Verdict

MATCH. The multi-End atomicity holds in both directions: the dead-region function
drops one Begin and two Ends together; the live-region function keeps one Begin and
two Ends together. The coinductive cut (End uses of `ρ` not counted as uses,
`flow_acc.ml:360-362`) and the result-type pinning of `simplify_end_region` (which
makes the unconditional `will_delete_binding` sound, `S.Rewrite.Let.DeadRegion`) are
what make this work. `End_try_region` is excluded from the deletion path and is not
exercised here (no `try`).
