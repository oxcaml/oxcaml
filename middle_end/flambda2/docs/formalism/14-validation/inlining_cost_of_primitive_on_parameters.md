# inlining_cost_of_primitive_on_parameters

Source: `testsuite/tests/flambda2/inlining_cost_of_primitive_on_parameters.ml`
Reference: `inlining_cost_of_primitive_on_parameters.simplify.reference`
Flags: `-flambda2-inline-small-function-size 0 -flambda2-inline-threshold 7.99`

## Input

```ocaml
external string_length : string -> int = "%string_length"
let f x = string_length x
let g x = f x
```

## Prediction (written before reading the reference)

- `f`'s body is `let prim = %string_length x in let r = tag_imm prim in
  cont k [r]`. Per the cost model (chapter 11 §2.4): `string_length` costs 5,
  `Tag_immediate` costs 2, `apply_cont` costs 1 → **code size 8**.
- `g` calls `f` directly. `small_function_size = 0`, so `f` (size 8) is not
  must-inline; it is below the (default) large size, so
  `S.Inline.DeclDecision` classifies it Speculatively_inlinable. The heuristic
  reaches **`S.Inline.Speculative`**: inlining `string_length x` with `x` of
  unknown type removes no operations, so `evaluated_to ≈ size ≈ 8 > threshold
  7.99` → Speculatively_not_inline → keep. `g` retains `Apply direct(f) x`.

## Actual

```
... f_0_1 (x : val) ... size(8) ... =
  let prim = %string_length (x) in
  let Pstringlength = %tag_imm (prim) in
  cont k (Pstringlength)
... g_1_1 (x : val) ... =
  apply direct(f_0_1) (...) (x) -> k * k1
```

`f` has `size(8)`; `g` retains the un-inlined direct call.

## Verdict

MATCH. `f`'s code size is exactly 8, and speculative inlining declines because
the threshold (7.99) sits just below that size with no removed operations to
offset it — the intended behaviour. Confirms the cost-model constants for
`%string_length` (5) and `Tag_immediate` (2) documented in chapter 11 §2.4.
