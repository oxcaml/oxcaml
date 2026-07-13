# new-02: switch on a known constant-constructor scrutinee

Targets `S.Rewrite.Switch.ArmPrune` + `S.Rewrite.Switch.Merge` (ch. 10): a match
whose scrutinee is a statically-known constructor keeps no `switch`.

## Source

`t02.ml`:

```ocaml
type t = A | B | C
let f () = match A with A -> 1 | B -> 2 | C -> 3
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 02.raw.fl -dfexpr-to 02.final.fl t02.ml`.

## Raw IR

The scrutinee `A` is the tagged immediate `0`; the match becomes a `switch` on
`0` with arms `0 → k[1]`, `1 → k[2]`, `2 → k[3]` (constant constructors are
numbered in declaration order).

## Prediction (before running)

The scrutinee's type is the singleton `{0}`. For arms `1` and `2`, `{0} ⊓ {1} =
⊥` and `{0} ⊓ {2} = ⊥`, so `S.Rewrite.Switch.ArmPrune` removes both. One arm
(`0 → k[1]`) survives, and `S.Rewrite.Switch.Merge` emits it directly.

Expected final body: `cont k (1)`, with no `switch`.

## Actual IR (final)

```
cont k/23 (1)
```

## Verdict

MATCH. No `switch` survives; the single reachable arm's continuation call is
emitted with the constant `1`.

## Diagnosis

No discrepancy. This is the "known scrutinee" special case called out in the
`S.Rewrite.Switch.ArmPrune` notes: every arm but one prunes and the survivor is
emitted by `S.Rewrite.Switch.Merge`.
