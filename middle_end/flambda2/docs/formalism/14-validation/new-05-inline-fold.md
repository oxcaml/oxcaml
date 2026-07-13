# new-05: function inlining then constant folding

Targets `S.Inline.Substitute` (ch. 11) + `S.Rewrite.Prim.ConstFold` (ch. 10): a
small function applied to a constant is inlined and the body folds.

## Source

`t05.ml`:

```ocaml
let f x = x + 3
let g () = f 4
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 05.raw.fl -dfexpr-to 05.final.fl t05.ml`.
(Identical result at default optimization level: `f` is below the small-function
size band, so `S.Inline.DeclDecision` classifies it must-inline regardless.)

## Raw IR

`g`'s body is a direct full application `apply f_0 (4) -> k`, and `f`'s body is
`let next_depth = ... in %int_barith.add (x, 3); cont ...`.

## Prediction (before running)

The call in `g` is a direct, full application of `f` whose code is available and
small, so `S.Inline.Substitute` replaces it with `let x = 4 in <body of f>` (plus
the depth/closure lets). Canonicalizing `x ↦ 4` makes the body's primitive
`%int_barith.add (4, 3)`, both operands constant, so `S.Rewrite.Prim.ConstFold`
folds it to `7`.

Expected: `g`'s final body is `cont k (7)`; `f` remains separately with body
`x + 3` (its own `x` is unknown, so it cannot fold in isolation).

## Actual IR (final)

`g`:

```
cont k/39 (7)
```

`f` (retained):

```
let int_add/85 = %int_barith.add (x/84, 3) in
cont k/37 (int_add/85)
```

## Verdict

MATCH. `f` was inlined into `g` and `4 + 3` folded to `7`; `f` itself survives
unfolded because its parameter is unknown.

## Diagnosis

No discrepancy. This is the canonical `S.Inline.Substitute` → resimplify →
`S.Rewrite.Prim.ConstFold` pipeline described in ch. 11 §4 ("what inlining
enables"): the argument's singleton type flows into the parameter, exposing the
fold.
