# tocmm-04: closure allocation and value-slot projection

Targets `R.Obj.Closures` (ch. [17](../17-representation.md)),
`TC.Prim.ProjectValueSlot`, and `TC.Let.SetOfClosures`
(ch. [18](../18-to-cmm-data.md)): allocating a closure that captures a variable,
and reading that captured variable back from the closure environment.

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/closure_capture.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/closure_capture.ml) (reference: `tocmm/closure_capture.compilers.reference`, capturing `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/closure_capture.ml`.

## Source

```ocaml
let[@inline never] adder x = let f y = x + y in f
```

## Prediction (before reading `-dcmm`)

1. `adder` allocates a closure for `f` capturing `x` (`TC.Let.SetOfClosures`,
   `R.Obj.Closures`): a `closure_tag` block holding a code pointer, a
   `pack_closure_info` word (arity 1, `is_last`), and the captured `x` as one
   value slot. Expect `(alloc H code closinfo x)`.
2. Inside `f`, `x` is read from the closure environment
   (`TC.Prim.ProjectValueSlot`): `f` receives `my_closure`, and `x` is a value
   slot loaded at `my_closure + 8·off` for the slot's offset `off`.
3. `x + y` is tagged addition `(+ (+ x y) -1)`.

## Actual `-dcmm -dcanonical-ids`

```
(function camlClosure_capture__adder_0_2_code (x/0: int) : val
 (alloc 3319
   L:"camlClosure_capture__fn[closure_capture.ml:...]_1_3_code"
   108086391056891911 x/0) )
(function camlClosure_capture__fn[closure_capture.ml:...]_1_3_code
     (y/0: int my_closure/0: val) : int
 (+ (+ (load val (+a my_closure/0 16)) y/0) -1) )
```

## Verdict

MATCH.

- `(alloc 3319 code closinfo x)` — a closure block: header
  `3319 = (3≪10)|247` (`closure_tag = 247`, size 3 = code + closinfo + one captured
  value; `R.Header`/`R.Obj.Closures`), a code pointer, a `pack_closure_info` word
  (`108086391056891911`: arity `1` in the top 8 bits, `is_last` set), and the
  captured `x` — `TC.Let.SetOfClosures`.
- `(load val (+a my_closure 16))` — the captured `x` read from the closure at
  `my_closure + 16` (word offset 2), i.e. `TC.Prim.ProjectValueSlot` /
  `R.Obj.Closures` value-slot load; `f` takes `my_closure` as its trailing
  argument as `R.Val.Clos` / the closure calling convention predicts.
- `(+ (+ … y) -1)` — tagged addition.

## Diagnosis

No discrepancy. The closure header size/tag (`R.Header`, `R.Obj.Closures`), the
`pack_closure_info` arity/`is_last` fields, and the value-slot load offset all
match. (The `startenv` field of the closinfo word — the low bits — encodes where
the scannable environment begins; only its `pack_closure_info` structure, not a
particular numeric value, is asserted here.)
