# naked_immediates_many_relations

Source: `testsuite/tests/flambda2/simplify/naked_immediates_many_relations.ml`
Reference: inline `[%%expect_fexpr Simplify{| … |}]` block in the source
Flags: `[@@@ocaml.flambda_o3]`

Covers rules not exercised by the main-directory tests:
`T.Grammar.NakedImmediate.Relational`, `T.Meet.Relational`, `T.Meet.Variant`,
`S.Rewrite.Apply.IndirectToDirect`, `S.Inline.Substitute`.

## Input

```ocaml
type t = A of (int -> int) | B of (int -> int)
let r =
  let f x = x in let g x = x in
  let k x y =
    match x, y with
    | A _, A _ | B _, B _ ->
      (match x, y with
       | A ax, A ay -> ax (ay 0)
       | B bx, B by -> by (bx 0)
       | _ -> assert false)
    | _ -> assert false in
  let x = if Sys.opaque_identity true then A f else B g in
  let y = if Sys.opaque_identity true then A g else B f in
  (k[@inlined]) x y
```

The comment: with only a *single* relation between a naked immediate and a
variant recordable, re-simplifying `k` forgets its relation with either `x` or
`y`, leaving one of the two calls per branch as an indirect call. The fix records
*multiple* relations.

## Prediction (written before reading the expected block)

- `x`/`y` are each an opaque-guarded choice between two variant constructors, so
  their tags are naked-immediate discriminants (`0 = A`, `1 = B`) tied to which
  function each holds (`T.Grammar.NakedImmediate.Relational` — a naked immediate
  relating to a variant via `Get_tag`/`Is_int`).
- With multiple relations recorded, inside a reachable branch (both `A` or both
  `B`) the environment knows *both* `x`'s and `y`'s constructor, hence both
  captured functions (`ax`,`ay` are `f`,`g`; `bx`,`by` are `g`,`f`). So both
  applications `ax (ay 0)` / `by (bx 0)` resolve `S.Rewrite.Apply.IndirectToDirect`
  and then inline (`S.Inline.Substitute`). `f` and `g` are the identity, so each
  branch computes `0`.
- Predicted output: **no residual `apply`** (neither indirect nor direct); the
  two opaque switches for `x`'s and `y`'s tags remain; reachable branches yield
  `0`; the mismatched `(A,B)`/`(B,A)` branches become `assert false`
  (`Assert_failure` raise). `r = Block 0 (0)`.

## Actual

The expected fexpr shows exactly this: two opaque `switch untagged` blocks
producing `tag` and `tag_1`; a nested switch on `tag`/`tag_1` where the matched
`(A,A)`/`(B,B)` branches go to `k` (which builds `Block 0 (0)`) and the
`(A,B)`/`(B,A)` branches go to `error pop(regular error)
($camlTOP3__Pmakeblock59)` (the `Assert_failure`). There is **no `apply`
node anywhere** in the output.

## Verdict

MATCH. Both function calls in each reachable branch are eliminated (resolved to
direct and inlined to the identity), leaving no indirect — nor any — `apply`.
This is precisely what recording multiple naked-immediate/variant relations
enables, and it confirms `T.Grammar.NakedImmediate.Relational` /
`T.Meet.Relational` support more than one relation per naked immediate, feeding
`S.Rewrite.Apply.IndirectToDirect` for both `x` and `y` simultaneously.
