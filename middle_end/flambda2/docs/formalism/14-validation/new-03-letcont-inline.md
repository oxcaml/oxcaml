# new-03: single-use continuation inlining

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/letcont_inline.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/letcont_inline.ml) (reference: `formalism/letcont_inline.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/letcont_inline.ml`.

Targets `S.Rewrite.LetCont.Inline` (ch. 10): a non-recursive continuation used
exactly once in tail position is inlined and its binder disappears.

## Source

`t03.ml`:

```ocaml
let f x =
  let y = if true then x else x + 1 in
  y + 100
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 03.raw.fl -dfexpr-to 03.final.fl t03.ml`.

## Raw IR (body of `f`)

The frontend already folds `if true then x else x + 1` to `x`, so the join
continuation `k/15` (the code after the conditional) is left with a single tail
call:

```
cont k/15 (x/27)
  where k/15 (y/29 : imm tagged) =
    let int_add/30 = %int_barith.add (y/29, 100) in
    cont k/14 (int_add/30)
```

`k/15` has exactly one use, in tail position, with no trap action, and is not a
return/exn continuation.

## Prediction (before running)

`S.Rewrite.LetCont.Inline` applies: the one call `cont k/15 (x)` is replaced by
`let y = x in <handler>`, and the `let_cont k/15` binder is dropped
(`S.Rewrite.LetCont.DeadHandler`). After canonicalization `y ↦ x`, the body is
`let int_add = %int_barith.add (x, 100) in cont k (int_add)`.

Expected final body: no `let_cont`; `let int_add = %int_barith.add (x, 100) in
cont k (int_add)`.

## Actual IR (final)

```
let int_add/51 = %int_barith.add (x/50, 100) in
cont k/28 (int_add/51)
```

## Verdict

MATCH. The `let_cont k/15` is gone and its handler body is inlined at the single
call site with `y` bound to (and canonicalized to) `x`.

## Diagnosis

No discrepancy. The "exactly one use" side condition of
`S.Rewrite.LetCont.Inline` is met because the constant-condition `if` folded
away before Simplify saw the continuation, leaving a linearly-used join point.
