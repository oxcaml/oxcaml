# issue5721

Source: `testsuite/tests/flambda2/issue5721.ml`
Reference: `issue5721.simplify.reference`
Flags: `-flambda2-join-points -flambda2-join-algorithm=n-way`

## Input

```ocaml
type 'a t = Left_input | Right_input | New_result of 'a
let main ~f x =
  let _ = (match x with Left_input -> () | Right_input -> f () | New_result _ -> ()) in
  match x with
  | Left_input -> false
  | Right_input -> false
  | New_result _ -> true
```

The test asserts: the `%is_int` on `x` in the *second* match should be
eliminated, because its value is already known from the first match.

## Prediction (written before reading the reference)

- `Left_input`/`Right_input` are constant constructors (immediates `0`/`1`);
  `New_result _` is a block. The first match lowers to `is_int x` (constant vs
  non-constant) then, for the immediate case, an inner switch on the untagged
  value.
- `is_int` is CSE-eligible (`unary_primitive_eligible_for_cse: Is_int _ ->
  true`), and the n-way join across the first match's arms preserves the
  relational `is_int`/discriminant information on `x`
  (`T.Grammar.NakedImmediate.Relational`, `T.Meet.Relational`,
  `S.Rewrite.CSE.Replace`). So the second match's `is_int x` should be reused,
  not recomputed: only **one** `%is_int (x)` should appear.
- Semantically the second match returns `true` iff `x` is a block
  (`New_result`), i.e. `not (is_int x)`. Predicted result:
  `boolean_not(tag_imm(is_int x))` at the merge, with a single `is_int`.

## Actual

```
... main_0_1 (f : val, x : [ 0 |1 | 0 of val ]) ... =
  (let prim = %is_int (x) in
   switch prim | 0 -> k2 (0i) | 1 -> k3
     where k3 = ((let untagged = %untag_imm (x) in
                  switch untagged | 0 -> k2 (1i) | 1 -> k3
                    where k3 = (apply f (0) -> k3 * k1
                                  where k3 (param) = cont k2 (1i)))))
    where k2 (join_param : imm) =
      let Pisint = %tag_imm (join_param) in
      let Pnot = %boolean_not (Pisint) in
      cont k (Pnot)
```

There is exactly **one** `%is_int (x)`. The first match's control flow merges at
`k2 (join_param : imm)`, carrying the discriminant, and the second match's result
is computed as `boolean_not(tag_imm(join_param))` from that single is_int — the
redundant second `is_int` is gone.

## Verdict

MATCH. The second `%is_int (x)` is eliminated; only one is_int remains and the
result is `not(is_int x)` at the merge, exactly as predicted. The n-way join
preserves the discriminant so the two matches fuse rather than re-testing `x`.

## Diagnosis

Not a mismatch. Minor prediction refinements confirmed by the actual output: the
two matches do not merely share `is_int` — they *fuse* into a single control tree
whose merge continuation `k2 (join_param : imm)` carries the (unboxed) is_int
discriminant, and the `Right_input` arm still performs the side-effecting `f ()`
before joining. This depends on the n-way join preserving relational info across
the intervening effectful match, which the flags (`-flambda2-join-points
-flambda2-join-algorithm=n-way`) enable.
