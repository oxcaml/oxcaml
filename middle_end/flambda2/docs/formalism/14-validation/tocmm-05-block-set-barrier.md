# tocmm-05: Block_set — immediate store vs. caml_modify barrier

Targets `TC.Prim.BlockSet` (ch. [18](../18-to-cmm-data.md)) and the `CM.Store`
note (ch. [15](../15-cmm.md)): a store of an *immediate* is a plain `Cstore`,
whereas a store of a *pointer* to the heap goes through the `caml_modify` GC write
barrier. Also witnesses `array_indexing` (`TC.Prim.ArrayAccess`) via a
dense-match data table.

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/block_set_barrier.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/block_set_barrier.ml) (reference: `tocmm/block_set_barrier.compilers.reference`, capturing `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/block_set_barrier.ml`.

## Source

```ocaml
let[@inline never] setfst (r : int ref) v = r := v          (* immediate store *)
let[@inline never] setref (r : string ref) s = r := s        (* value store *)
let[@inline never] shape n =
  match n with 0 -> "zero" | 1 -> "one" | 2 -> "two" | _ -> "many"
```

## Prediction (before reading `-dcmm`)

1. `setfst`: `r := v` where `v : int`. A `Block_set` of an *immediate* field needs
   no GC barrier (`TC.Prim.BlockSet` / `setfield_computed`), so expect a plain
   `(store int r v)`.
2. `setref`: `r := s` where `s : string` (a heap pointer). A `Block_set` of a
   *value* to the heap needs the barrier, so expect `(extcall "caml_modify" r s)`.
3. `shape`: a dense `int → string` match compiles to a data table indexed by the
   scrutinee; `array_indexing` on the *tagged* index scales by the word size and
   folds the untag (`TC.Prim.ArrayAccess` / `R.Obj.Array`): expect
   `base + (n≪2) − 4` (for tagged `n = 2m+1`, `= base + 8m`), plus a range check.

## Actual `-dcmm -dcanonical-ids` (module `Block_set_barrier`)

```
(function camlBlock_set_barrier__setfst_0_3_code (r/0: val v/0: int) : int
 (let Psetfield/0 (seq (store int r/0 v/0) 1) 1) )

(function camlBlock_set_barrier__setref_1_4_code (r/1: val s/0: val) : int
 (let Psetfield/1 (seq (extcall "caml_modify" r/1 s/0 ->unit) 1) 1) )

(function camlBlock_set_barrier__shape_2_5_code (n/0: int) : val
 (if (<u 5 n/0) L:"camlBlock_set_barrier__immstring26"
   (load_mut val
     (+a (+a L:"camlBlock_set_barrier__switch_block65" (<< n/0 2)) -4))) )
```

The string data confirms `R.Obj.Bytes`: e.g. `immstring26` is
`(data int 2044 … string "many" skip 3 byte 3)` — header
`2044 = (1≪10)|(3≪8)|252` (size 1 word, black, `string_tag = 252`), payload plus a
trailing padding byte holding the pad count.

## Verdict

MATCH.

- `(store int r v)` — immediate `Block_set` is a plain `Cstore(Word_int)`, no
  barrier (`TC.Prim.BlockSet`).
- `(extcall "caml_modify" r s)` — value `Block_set` to the heap uses the
  `caml_modify` write barrier (`TC.Prim.BlockSet`, `CM.Store` note). The two
  contrast exactly on the immediate-vs-pointer distinction `setfield_computed`
  makes.
- `(+a (+a switch_block (<< n 2)) -4)` — `array_indexing` on the tagged index:
  `(2m+1)≪2 = 8m+4`, `+ base − 4 = base + 8m`, a word-stride (8-byte) element
  address (`TC.Prim.ArrayAccess`, `R.Obj.Array`); the `(<u 5 n)` is the
  out-of-range guard.

## Diagnosis

No discrepancy. The GC-barrier decision (`caml_modify` for a value store, bare
`Cstore` for an immediate) is the load-bearing part of `TC.Prim.BlockSet`, and it
matches. The `shape` table access confirms the tagged-index scaling arithmetic of
`array_indexing` that `TC.Prim.ArrayAccess` and `R.Obj.Array` describe.
