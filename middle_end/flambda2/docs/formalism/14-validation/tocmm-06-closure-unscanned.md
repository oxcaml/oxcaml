# tocmm-06: unscanned value slot sits below `startenv`

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/closure_unscanned.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/closure_unscanned.ml) (reference: `tocmm/closure_unscanned.compilers.reference`, `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/closure_unscanned.ml`.

Adversarial probe of `R.Obj.Closures` (ch. [17](../17-representation.md)): a
mutually-recursive set of closures capturing an **immediate** (`int`). Immediate /
naked-number captures are *unscanned* value slots (`Slot_offsets.Layout`
`Value_slot {is_scanned = false}`) and are laid out **below** `startenv`, not at
`off ≥ startenv`. This case **drove a correction** to the rule.

## Source

```ocaml
let[@inline never] make n =
  let rec even x = if x = 0 then n = 0 else odd (x - 1)
  and odd x = if x = 0 then n <> 0 else even (x - 1) in
  (even, odd)
```

## Prediction (before reading `-dcmm`)

Original rule text: "for each value slot w at offset `off(w) ≥ startenv`,
`M[a+8·off(w)] = env(w)`." Predicted layout of the set-of-closures block:
`[odd_code; odd_closinfo; infix; even_code; even_closinfo; …]`, with the captured
`n` in the *scanned environment* at some offset `≥ startenv`.

## Actual `-dcmm -dcanonical-ids`

```
(function camlClosure_unscanned__make_0_3_code (n/0: int) : val
 (let *set_of_closures*/0
     (alloc 6391 L:"…__odd_2_5_code" 72057594037927949 3321
       L:"…__even_1_4_code" 108086391056891911 n/0)
   (alloc 2048 (+v *set_of_closures*/0 24) *set_of_closures*/0)) )
(function camlClosure_unscanned__even_1_4_code (x/0: int my_closure/0: val) : int
 (if (== x/0 1) (+ (<< (== (load val (+a my_closure/0 16)) 1) 1) 1)
   (app L:"…__odd_2_5_code" (+ x/0 -2) (+v my_closure/0 -24) int)) )
(function camlClosure_unscanned__odd_2_5_code (x/1: int my_closure/1: val) : int
 (if (== x/1 1) (+ (<< (!= (load val (+a my_closure/1 40)) 1) 1) 1)
   (app L:"…__even_1_4_code" (+ x/1 -2) (+v my_closure/1 24) int)) )
```

Block layout (header `6391 = (6≪10)|247`, `closure_tag`, size 6 words):

| word | 0 | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|---|
| | odd code | odd closinfo | infix(3)=3321 | even code | even closinfo | **n** |

`pack_closure_info` decode: odd (off 0) `72057594037927949` → arity 1, is_last 0,
stored startenv `6`; even (off 3) `108086391056891911` → arity 1, is_last 1, stored
startenv `3` (relative), i.e. absolute `startenv = 3 + 3 = 6`. The captured `n` is at
**word 5 < startenv 6** — read as `(load val (+a my_closure 16))` from `even`
(slot 3 → +2 words = word 5) and `(+a my_closure 40)` from `odd` (slot 0 → +5 words =
word 5).

## Verdict

MISMATCH (drove a formalism fix).

The old "all value slots at `off ≥ startenv`" clause is FALSE: `n` (an unscanned
`int` capture) sits at word 5, below `startenv = 6`. `R.Obj.Closures` was corrected
to split the value-slot clause on `is_scanned` — scanned slots at `off ≥ startenv`
(the GC-scanned environment), unscanned slots at `off < startenv` (in the region the
GC skips, among/after the function slots).

## Diagnosis

`startenv` marks where the **GC-scanned** environment begins, not where captured
values begin. `int`/naked-number captures need no scanning, so they are packed below
`startenv` with the function-slot region (this is why a closure needs no mixed-block
header — the GC skips everything below `startenv`). Corollary confirmations, all
now witnessed against a real dump for the first time: `infix_header(3) = (3≪10)|249 =
3321`; the relative `startenv − off(f)` closinfo encoding; and a sibling projection
`Move_within_set_of_closures` as a signed `Caddv` (`(+v my_closure 24)` /
`(+v my_closure -24)`), confirming `TC.Prim.ProjectFunctionSlot` including the
negative offset when moving to an earlier slot.
