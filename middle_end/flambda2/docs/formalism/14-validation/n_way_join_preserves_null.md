# n_way_join_preserves_null

Source: `testsuite/tests/flambda2/n_way_join_preserves_null.ml`
Reference: `n_way_join_preserves_null.simplify.reference`
Flags: `-flambda2-join-points -flambda2-join-algorithm=n-way`

## Input

```ocaml
let f bit bit' =
  let c = if bit' < 0 then false else (bit > bit') in
  if c then bit <> 0 else false
```

The comment states the intent: `c` is a boolean, and if the join at `c`'s
definition loses the fact that `c` is `Not_null`, unboxing of `c` is prevented.

## Prediction (written before reading the reference)

- `c` is defined by a join of two branches: `bit' < 0` gives the constant
  `false` (`0`), the else branch gives `bit > bit'` (an `Int_comp` result). Both
  are booleans and hence `Not_null` tagged immediates.
- Per `T.Join.Sound` / `T.Join.Head` over the nullability product
  (`T.Gamma.Value.Nullability`), joining two `Not_null` immediate types yields a
  `Not_null` type (nullability is not spuriously widened to `Maybe_null`). This
  preserved `Not_null` is the precondition that lets continuation-parameter
  unboxing (`S.Unbox.ContParam.*`) unbox the join point's parameter.
- So the merge continuation carrying `c` should have a **naked immediate**
  parameter (unboxed), not a tagged `Value`. The subsequent `if c` becomes a
  switch on that naked immediate.

## Actual

```
... f_0_1 (bit : imm tagged, bit' : imm tagged) ... =
  (let prim = %int_comp.lt (bit', 0) in
   switch prim | 0 -> k3 | 1 -> k2 (0i)
     where k3 = let prim_1 = %int_comp.gt (bit, bit') in cont k2 (prim_1))
    where k2 (join_param : imm) =
      (switch join_param | 0 -> k (0) | 1 -> k2
         where k2 = let prim = %phys_ne (bit, 0) in
                    let noteq = %tag_imm (prim) in cont k (noteq))
```

The join continuation is `k2 (join_param : imm)` — the parameter is a **naked
immediate** (`imm`), and the two predecessors pass `0i` and the untagged
comparison result. `if c` is `switch join_param`.

## Verdict

MATCH. The join preserves `Not_null`, so the join-point parameter carrying `c`
is unboxed to a naked immediate (`join_param : imm`) rather than a tagged value —
exactly the information-preservation the test guards. Had the not-null fact been
lost, the parameter would remain a tagged `Value` and no unboxing would appear.
