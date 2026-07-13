# new-01: integer constant-folding chain

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/constfold.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/constfold.ml) (reference: `formalism/constfold.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/constfold.ml`.

Targets `S.Rewrite.Prim.ConstFold` (ch. 10): a nested arithmetic expression on
constants folds to a single constant.

## Source

`t01.ml`:

```ocaml
let f () = (3 + 4) * 2
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 01.raw.fl -dfexpr-to 01.final.fl t01.ml`
(the outcome is identical at default optimization level; folding is not
opt-level gated).

## Raw IR (body of `f`, after CPS)

```
let int_add/28 = %int_barith.add (3, 4) in
let int_mul/29 = %int_barith.mul (int_add/28, 2) in
cont k/11 (int_mul/29)
```

Two separate primitives, each with at least one constant argument; the second
consumes the result of the first.

## Prediction (before running)

1. `%int_barith.add (3, 4)`: both arguments are proven constants, `⟦add⟧(3,4) = 7`
   is defined, so `S.Rewrite.Prim.ConstFold` rewrites the binding to `let
   int_add = 7`, with `int_add : {7}`.
2. Canonicalization (`S.Rewrite.Alias.Canonicalize`) then makes the second
   primitive `%int_barith.mul (7, 2)`; both arguments constant, `⟦mul⟧(7,2) = 14`,
   so `S.Rewrite.Prim.ConstFold` fires again giving `let int_mul = 14`.
3. The intermediate bindings are dead (`S.Rewrite.Let.DeadBinding`).

Expected final body: `cont k (14)`.

## Actual IR (final)

```
cont k/23 (14)
```

## Verdict

MATCH. The chain collapses exactly as predicted; the folded value 14 equals
`⟦mul⟧(⟦add⟧(3,4),2)`, the soundness coupling `S.Rewrite.Prim.ConstFold`
asserts to chapters 05–06.

## Diagnosis

No discrepancy. The two-step fold with an intervening canonicalization is the
textbook application of `S.Rewrite.Prim.ConstFold` plus
`S.Rewrite.Alias.Canonicalize`.
