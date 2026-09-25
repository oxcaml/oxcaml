# removed_operations_of_switch

Source: `testsuite/tests/flambda2/removed_operations_of_switch.ml`
Reference: `removed_operations_of_switch.simplify.reference`
Flags: `-flambda2-inline-small-function-size 26 -flambda2-inline-threshold 26.99
-flambda2-inline-large-function-size 100`

## Input

```ocaml
let f b x y = if b then (x, y) else (y, x)
let g b x y = f b x y
```

## Prediction (written before reading the reference)

- `f` is a two-arm switch on `b` whose arms feed **different** destinations
  (each builds a distinct block: `(x,y)` vs `(y,x)`). The `S.Rewrite.Switch.*`
  merge/identity/boolean-not rewrites all require a common destination, so none
  fire. `f` stays a switch: `untag b`, then `switch { 0 → k2; 1 → k3 }` with the
  two block-building arms. Predicted size 27.
- `g` calls `f` directly. `f`'s size (27) is below `large_size` (100) but above
  `small_size` (26), so `S.Inline.DeclDecision` classifies it
  Speculatively_inlinable (could). The heuristic reaches **`S.Inline.Speculative`**,
  which inlines-and-simplifies to measure cost: with `x,y` of unknown type the
  block is still built and nothing is removed, so `evaluated_to ≈ size ≈ 27 >
  threshold 26.99` → Speculatively_not_inline → keep. `g` retains
  `Apply direct(f)`.

## Actual

```
... f_0_1 (b : imm tagged, x, y) ... size(27) ... =
  (let untagged = %untag_imm (b) in switch untagged | 0 -> k2 | 1 -> k3)
    where k3 = let Pmakeblock = %block.[`0`] (x, y) in cont k (Pmakeblock)
    where k2 = let Pmakeblock = %block.[`0`] (y, x) in cont k (Pmakeblock)
... g_1_1 (b, x, y) ... =
  apply direct(f_0_1) (...) (b, x, y) -> k * k1
```

`f` retains the two-arm switch to distinct block-building destinations, `size(27)`;
`g` retains the un-inlined direct call.

## Verdict

MATCH. The switch does not simplify (arms feed different destinations), its size
is exactly 27, and speculative inlining declines because the threshold (26.99)
sits just below the un-improved size (27) — the whole point of the test.
