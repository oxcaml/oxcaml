# n_way_join_null

Source: `testsuite/tests/flambda2/n_way_join_null.ml`
Reference: `n_way_join_null.simplify.reference`
Flags: `-flambda2-join-points -flambda2-join-algorithm=n-way`

## Input

```ocaml
let main x =
  let[@local] ret y = y in
  match x with
  | This _ -> ret 0
  | Null   -> ret 1
```

`x : _ or_null`. The `[@local]` continuation forces an actual join at `ret`
(function returns are otherwise not joined), and the two predecessors refine `x`
to `This _` (non-null) and `Null` respectively. The test checks that joining
`This _` with `Null` does not crash.

## Prediction (written before reading the reference)

- The match on an `or_null` scrutinee lowers to a switch on `%is_null x` with
  `0 → This-arm`, `1 → Null-arm`. The `This` arm calls `ret 0`, the `Null` arm
  calls `ret 1`; `ret` is local and returns its argument, so it inlines to give
  the switch `{ 0 → k [0]; 1 → k [1] }`.
- The n-way join at the merge joins the environment across the two arms,
  including `x : This _` ⊔ `x : Null`. Per `T.Join.Sound` and the nullability
  product of `T.Gamma.Value.Nullability`, this is well-defined (result is a
  `Maybe_null` value with non-null part `= (This _)`), so it must **not crash**.
- The surviving switch `{ 0 → k [0]; 1 → k [1] }` passes each discriminant,
  tagged, to one continuation — the identity pattern — so
  **`S.Rewrite.Switch.Identity`** collapses it to `let t = tag_imm(scrutinee)`.
  The scrutinee is `%is_null x`, so `main` should become
  `tag_imm(is_null x)`.

## Actual

```
... main_0_1 (x) ... size(4) ... : imm tagged =
  let prim = %is_null (x) in
  let tagged_scrutinee = %tag_imm (prim) in
  cont k (tagged_scrutinee)
```

`main = tag_imm(is_null x)`. No crash; the compile succeeds and the switch has
collapsed to the tagged is_null test.

## Verdict

MATCH. The join of `This _` with `Null` is computed without crashing, and the
identity switch over the is_null discriminant collapses to `tag_imm(is_null x)`
exactly as predicted.
