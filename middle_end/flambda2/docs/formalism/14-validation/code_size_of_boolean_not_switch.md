# code_size_of_boolean_not_switch

Source: `testsuite/tests/flambda2/code_size_of_boolean_not_switch.ml`
Reference: `code_size_of_boolean_not_switch.simplify.reference`
Flags: `-flambda2-inline-small-function-size 1 -flambda2-inline-large-function-size 1`

## Input

```ocaml
let f b = if b then 9 - 9 else 8 - 7
let g b = f b
```

## Prediction (written before reading the reference)

- The arithmetic `9 - 9` and `8 - 7` const-fold (`S.Rewrite.Prim.ConstFold`) to
  `0` and `1`, so `f`'s body is the switch `{ 0 → k [1]; 1 → k [0] }` (false
  arm returns the else value `1`, true arm returns the then value `0`).
- That is exactly the boolean-not pattern, so **`S.Rewrite.Switch.BooleanNot`**
  fires, replacing the switch by `Boolean_not` on the tagged scrutinee. The
  scrutinee is produced by untagging `b`, so the `tag_imm` reintroduced by the
  rule cancels the untag via **`S.Rewrite.Prim.UntagTag`**, leaving
  `%boolean_not(b)` directly. Predicted code size 2 (boolean_not: 1,
  apply_cont: 1).
- In `g`, the call to `f` is direct. `f`'s size (2) is `≥ large_size` (1), so
  **`S.Inline.DeclDecision`** classifies it Body_too_large (cannot inline), and
  **`S.Inline.Decision`** returns keep. `g` retains `Apply direct(f) b`.

## Actual

```
let code ... f_0_1 (b : imm tagged) ... size(2) ... =
  let not_scrutinee = %boolean_not (b) in
  cont k (not_scrutinee)
...
let code ... g_1_1 (b : imm tagged) ... size(4) ... =
  apply direct(f_0_1) (...f...) (b) -> k * k1
```

`f` is exactly `%boolean_not(b)` with `size(2)`; `g` retains the direct
un-inlined call.

## Verdict

MATCH. The targeted phenomenon (BooleanNot rewrite, untag/tag cancellation,
code size 2, inlining declined because size exceeds the function-size band) is
predicted correctly, including the size annotation.
