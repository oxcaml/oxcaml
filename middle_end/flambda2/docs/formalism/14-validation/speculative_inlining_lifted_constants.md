# speculative_inlining_lifted_constants

Source: `testsuite/tests/flambda2/speculative_inlining_lifted_constants.ml`
Reference: `speculative_inlining_lifted_constants.simplify.reference`
Flags: `-flambda2-inline-small-function-size 0 -flambda2-inline-threshold 0
-flambda2-speculative-inlining-track-lifted-constants
-no-flambda2-speculative-inlining-only-if-arguments-useful` (`[@@@ocaml.flambda_o3]`)

## Input

```ocaml
let f x = (x, x)
let zero = Sys.opaque_identity 0
let v0 = f zero
let one = Sys.opaque_identity 1
let v1 () = f one
```

The test guards a fixed bug (oxcaml#5917): speculative inlining used to ignore
the size of lifted constants, computing a size of `1` (just loading the lifted
pair) for each call to `f`, which slipped under the threshold and wrongly caused
inlining. With `-flambda2-speculative-inlining-track-lifted-constants`, the
lifted constant's size is counted, so the calls should **not** be inlined.

## Prediction (written before reading the reference)

- `f x = (x, x)` builds a block (size ≈ 7 for the block + 1 apply_cont = 8).
- With `small_function_size = 0`, `f` is not must-inline; it is
  Speculatively_inlinable. At the calls `f zero` / `f one`, the argument is a
  known constant, so inlining would let the pair `(x,x)` be lifted to a static
  constant. **`S.Inline.Speculative`** performs the speculative
  inline-and-simplify and measures cost: with lifted-constant tracking on, the
  measured cost includes the lifted pair, so `evaluated_to > threshold (0)` →
  Speculatively_not_inline → keep.
- Predicted output: `f` remains a separate closure; `v0` and `v1` both retain a
  direct `apply` of `f`, un-inlined.

## Actual

```
... f_0_1 (x) ... size(8) ... = let Pmakeblock = %block.[`0`] (x, x) in cont k (Pmakeblock)
...
apply direct(f_0_1) (...) (zero) -> k * error       (* v0: not inlined *)
...
  v1_1_1 (param) ... = ... apply direct(f_0_1) (...) (one_1) -> k * k1   (* v1: not inlined *)
```

`f` stays a separate function with `size(8)`; both `v0` and `v1` retain direct,
un-inlined calls to `f`.

## Verdict

MATCH. Neither call to `f` is inlined: speculative inlining counts the lifted
pair constant, keeping the estimated cost above the zero threshold — exactly the
regression the test guards, and consistent with `S.Inline.Speculative`'s
"inline-and-simplify to measure cost" formulation (chapter 11 §2.3).
