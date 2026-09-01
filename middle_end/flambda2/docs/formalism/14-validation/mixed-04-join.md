# mixed-04: joining two mixed blocks of equal shape

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/mixed_join.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_join.ml) (reference: `formalism/mixed_join.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/mixed_join.ml`.

Targets `T.Meet.BlockShape` (ch. 08): two `if`/`else` branches build mixed
blocks of the *same* shape; at the join the block type keeps that shape (equal
shapes ⟹ keep σ), which in turn lets the flat-suffix field be unboxed across the
join.

## Source

`m4.ml`:

```ocaml
type t = { a : string; b : float# }

external opaque : 'a -> 'a = "%opaque"

let choose cond (s1 : string) (f1 : float#) (s2 : string) (f2 : float#) =
  let r = if cond then { a = s1; b = f1 } else { a = s2; b = f2 } in
  let _ : t = opaque r in
  r.b
```

`opaque r` forces `r` to be a real block that survives the join (otherwise the
projection would sink into the branches and the block would vanish). Flags:
`ocamlopt.opt -O3 -c -drawfexpr-to m4.raw.fl -dfexpr-to m4.final.fl m4.ml`.

## Raw IR

```
switch untagged/53 | 0 -> k/25 | 1 -> k/26
  where k/23 = let Pmakeblock/51 = %block.mixed.`0`.`1`.[`float`] (s1/43, f1/44) in cont k/22 (Pmakeblock/51)
  where k/24 = let Pmakeblock/52 = %block.mixed.`0`.`1`.[`float`] (s2/45, f2/46) in cont k/22 (Pmakeblock/52)
  where k/22 (r/48 : [ 0 of val * float ]) =
    let Popaque/49 = %opaque (r/48) in
    let Pmixedfield/50 = %block_load.mixed.`float`.`1`.[`float`].[`1`] (r/48) in
    cont k/21 (Pmixedfield/50)
```

Both branches build shape ⟨1, [Naked_float]⟩; the join continuation `k/22`
binds `r : [ 0 of val * float ]` and the body loads `r.b`.

## Prediction (before running)

Both branches produce a mixed block with the *same* shape σ = ⟨1, [Naked_float]⟩
at tag 0. Joining the two block types is `join_row_like_for_blocks`:
`join_shape σ σ = Known σ` because `Block_shape.equal σ σ` (`T.Meet.BlockShape`).
The `a` and `b` fields differ across branches, so each joins to Unknown and is
filled with `unknown_from_shape(σ, i)` — a top `Value` for field 0 and a top
naked `float` for field 1 (`join_int_indexed_product`). The joined `r` therefore
keeps shape ⟨1, [float]⟩ with field 1 a top naked float, so the post-join load
`r.b` cannot fold to a constant and survives as a `Block_load Mixed` of element
kind `float`.

(A genuine *different*-shape join at the same tag is not constructible from
well-typed source — see Diagnosis.)

## Actual IR (final)

```
choose_0_1 (cond, s1, f1, s2, f2) ... : float =
  switch untagged/102 | 0 -> k/48 | 1 -> k/49
    where k/49 = let Pmakeblock/101 = %block.mixed.`0`.`1`.[`float`] (s1, f1) in
                 cont k/47 (Pmakeblock/101, f1/94)
    where k/48 = let Pmakeblock/100 = %block.mixed.`0`.`1`.[`float`] (s2, f2) in
                 cont k/47 (Pmakeblock/100, f2/96)
    where k/47 (r/97 : [ 0 of val * float ], unboxed_mixed_field_1/98 : float) =
      let Popaque/99 = %opaque (r/97) in
      cont k/46 (unboxed_mixed_field_1/98)
```

## Verdict

MATCH (richer than predicted). The join continuation parameter `r/97` keeps
type `[ 0 of val * float ]` — tag 0, shape ⟨value-prefix `val`, flat-suffix
`float`⟩ — confirming `T.Meet.BlockShape`'s equal-shape clause: both branches
share σ = ⟨1, [float]⟩ so `join_shape` returns `Known σ` and the shape is
preserved across the join. The block `r` survives (kept alive by `%opaque`) with
that shape.

The post-join load `r.b` was *not* left as a `Block_load` (my prediction), but
eliminated by continuation-parameter unboxing: `k/47` gains an extra parameter
`unboxed_mixed_field_1/98 : float`, each branch passes its own `f1`/`f2` on the
edge, and the body returns that naked float directly. This is strictly *enabled*
by the shape being preserved: the field kind `Naked_float` is known at the join
only because `join_shape` kept σ rather than dropping to Unknown, so the field is
unboxable (ch. 12 continuation-parameter unboxing on a mixed-block field). Had
the shapes differed and the join dropped to `Unknown` shape, this unboxing could
not fire.

## Diagnosis

Prediction under-called the optimization: I predicted a surviving post-join
`Block_load Mixed`, but a known joined shape lets the flat-suffix field be
threaded as an unboxed continuation argument, so the load disappears while the
block itself is retained at shape ⟨1, [float]⟩. This confirms rather than
contradicts `T.Meet.BlockShape` (equal shapes ⟹ keep σ; the kept σ is what makes
the field-kind known and hence unboxable).

The *different*-shape branch of `T.Meet.BlockShape` (`¬ Block_shape.equal ⟹
Unknown` on join, `⊥` on meet) could not be exercised from surface source: in
well-typed OxCaml the type system assigns exactly one `Mixed_record` shape per
record type, and distinct record types cannot share one join variable, so two
*different* mixed shapes never meet at the *same* tag. That clause is a defensive
path in `join_row_like_for_blocks` / `meet_row_like_for_blocks` (and its n-way
mirror `n_way_join_row_like_for_blocks`), verified by reading the code but not
reachable by a clean witness.
