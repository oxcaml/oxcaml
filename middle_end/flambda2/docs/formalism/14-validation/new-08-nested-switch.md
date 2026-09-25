# new-08: unreachable arm pruned via refined scrutinee type

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/nested_switch.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/nested_switch.ml) (reference: `formalism/nested_switch.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/nested_switch.ml`.

Targets `S.Rewrite.Switch.ArmPrune` and `S.Rewrite.Invalid.Propagate` (ch. 10):
a match on a value already known (from an enclosing match) to be a specific
constructor prunes the impossible inner arm.

## Source

`t08.ml`:

```ocaml
type t = A | B
let f (x : t) =
  match x with
  | A -> (match x with A -> 1 | B -> 2)
  | B -> 0
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 08.raw.fl -dfexpr-to 08.final.fl t08.ml`.

## Raw IR

An outer `switch x` with arms `0 (A)` and `1 (B)`. The `A` arm contains an inner
`switch x` with arms `0 → k[1]` and `1 → k[2]`.

## Prediction (before running)

Inside the outer `A` arm, the scrutinee type of `x` is refined to `{0}` (the
switch-arm meet). The inner `switch x` therefore sees `x : {0}`: arm `1 (B)`
meets to `⊥` and is pruned (`S.Rewrite.Switch.ArmPrune`); the surviving arm `0`
is emitted (`S.Rewrite.Switch.Merge`), so the inner match yields the constant
`1`. The outer match then reads `A → 1, B → 0`.

Expected: at minimum, the inner switch collapses to `1`. I predicted the outer
would remain as a two-arm `switch x { A → 1; B → 0 }`.

## Actual IR (final, body of `f`)

```
let not_scrutinee/51 = %boolean_not (x/50) in
cont k/31 (not_scrutinee/51)
```

## Verdict

MATCH, and stronger than predicted. The inner arm was pruned exactly as
predicted (the inner match resolved to the constant `1`, confirming the refined
scrutinee type). The *outer* switch — now `{ 0 (A) → k[1]; 1 (B) → k[0] }`, i.e.
`0 → k[1]; 1 → k[0]` on discriminants `{0,1}` — additionally matched the
boolean-negation pattern and was rewritten by `S.Rewrite.Switch.BooleanNot` to
`%boolean_not (x)`.

## Diagnosis

No discrepancy. The result validates two rules at once:

- `S.Rewrite.Switch.ArmPrune`: the inner `B` arm is unreachable because the
  enclosing arm pinned `x` to `A`, so `{0} ⊓ {1} = ⊥`. (This is the descriptive
  mechanism by which "GADT-style" dead arms disappear, without needing an actual
  GADT.)
- `S.Rewrite.Switch.BooleanNot`: the resulting `{0 → 1, 1 → 0}` two-arm switch is
  recognized as negation of the (tagged) scrutinee. This is the expected outcome
  the `S.Rewrite.Switch.BooleanNot` notes describe, and semantically correct:
  `boolean_not(0) = 1 (A→1)`, `boolean_not(1) = 0 (B→0)`.

Observation worth adding to the formalism later: chaining ArmPrune with
BooleanNot means a "refine-then-negate" pattern lands on straight-line
arithmetic even when the source looks like a nested match.
