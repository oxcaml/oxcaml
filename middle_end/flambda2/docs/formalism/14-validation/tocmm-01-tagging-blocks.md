# tocmm-01: tagging, block build/load, static closures

Targets the `to_cmm` representation relation `R.*` (ch. [17](../17-representation.md))
and data lowering `TC.Prim.*` (ch. [18](../18-to-cmm-data.md)): integer tagging,
`Make_block`, `Block_load`, header encoding, and the static-closure layout.

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/tagging_blocks.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/tagging_blocks.ml) (reference: `tocmm/tagging_blocks.compilers.reference`, capturing `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/tagging_blocks.ml`.

## Source

```ocaml
let[@inline never] pair a b = (a, b)
let f x = x + 1
let g p = fst p + snd p
```

## Prediction (before reading `-dcmm`)

1. `f x = x + 1`: `x` is a tagged `int` (`R.Val.Imm`: `x ≈ 2a+1`), `1` is
   `tagged_imm 1 ≈ 3`. Adding a tagged int and a tagged constant is
   `x_tagged + 2` (`= 2(a+1)+1`), so `TC.Prim.TagUntag`/tagged arithmetic should
   give `(+ x 2)`.
2. `pair a b = (a, b)`: `Make_block` of a 2-field value block, tag 0
   (`TC.Prim.MakeBlock`, `R.Obj.Block`), header `hdr(0, 2, white, 0) = 2≪10 =
   2048` (`R.Header`) → `(alloc 2048 a b)`.
3. `g p = fst p + snd p`: two `Block_load`s (`TC.Prim.BlockLoad`, `R.Obj.Block`):
   field 0 at `p` (`load val p`), field 1 at `Cadda(p, 8)` (stride 8,
   `R.Val.Pointer`) → `load val (+a p 8)`; then tagged addition
   `(2a+1)+(2b+1)-1` → `(+ (+ … …) -1)`.
4. Each toplevel function is a static closure block (`R.Obj.Closures`): header
   `hdr(closure_tag=247, size 2, black) = (2≪10)|(3≪8)|247 = 3063`, a code
   pointer, and a `pack_closure_info` word with arity 1 (top 8 bits) and the
   `is_last` bit set.

## Actual `-dcmm -dcanonical-ids` (module `Tagging_blocks`; ids normalized to `/0`)

```
(data int 3840 global "camlTagging_blocks":
 addr G:"camlTagging_blocks__pair_3"
 addr G:"camlTagging_blocks__f_4"
 addr G:"camlTagging_blocks__g_5")
(data int 3063 global "camlTagging_blocks__f_4":
 addr G:"camlTagging_blocks__f_1_4_code" int 108086391056891909)
(function camlTagging_blocks__pair_0_3_code (a/0: val b/0: val) : val
 (alloc 2048 a/0 b/0) )
(function camlTagging_blocks__f_1_4_code (x/0: int) : int (+ x/0 2) )
(function camlTagging_blocks__g_2_5_code (p/0: val) : int
 (+ (+ (load val p/0) (load val (+a p/0 8))) -1) )
```

## Verdict

MATCH.

- `(+ x 2)` — tagged arithmetic, `R.Val.Imm` (`1 ↦ 3`, `+1 ↦ +2`).
- `(alloc 2048 a b)` — header `2048 = 2≪10`, `R.Header` + `TC.Prim.MakeBlock` +
  `R.Obj.Block`.
- `(load val p)` / `(load val (+a p 8))` — field 0 and field 1 at stride 8,
  `TC.Prim.BlockLoad` + `R.Val.Pointer` + `R.Obj.Block`; the `+…-1` is the
  tagged-int addition identity.
- Static closure header `3063 = (2≪10)|(3≪8)|247` — `R.Obj.Closures` header with
  black color (`R.Header`); closinfo `108086391056891909` has arity `1` in its top
  8 bits (`>>56 = 1`) and `is_last` set (`pack_closure_info`). The module block
  header `3840 = (3≪10)|(3≪8)` is a size-3 black block (`R.Header`).

## Diagnosis

No discrepancy. Every bit of the header/tag arithmetic (`R.Header`,
`pack_closure_info`) and every field offset (`R.Obj.Block` stride 8) matches the
emitted Cmm exactly, confirming the concrete-layout half of `TC.Prim.Sound`.
