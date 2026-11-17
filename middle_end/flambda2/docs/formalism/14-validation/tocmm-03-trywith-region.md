# tocmm-03: try/with — exn handler, trap push/pop, raise, region

Targets the control chapter's exception machinery: `TC.LetCont.Exn` /
`TC.ApplyCont.Raise` / `TC.ApplyCont.Jump` (ch. [16](../16-to-cmm-control.md)),
`CM.Catch.Exn` / `CM.Exit.Trap` / `CM.Raise` (ch. [15](../15-cmm.md)), and the
region rules `CM.Region.Begin` / `CM.Region.End` (ch. [19](../19-cmm-memory-gc.md)).

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/trywith_region.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/trywith_region.ml) (reference: `tocmm/trywith_region.compilers.reference`, capturing `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/trywith_region.ml`.

## Source

```ocaml
let[@inline never] tw g x = try g x with Not_found -> -1
```

## Prediction (before reading `-dcmm`)

1. The `try` body is entered under a pushed exception handler
   (`CM.Catch.Exn`/`TC.LetCont.Exn`): expect a `catch … with(H exn)` whose entry
   pushes handler `H` (`exit<push(H)>`, `CM.Exit.Trap`).
2. The non-exceptional exit pops the handler (`exit<pop(H)>`,
   `TC.ApplyCont.Jump` with a `Pop` trap action).
3. The handler tests the exception (`Not_found`) and, if it doesn't match,
   reraises (`CM.Raise` / `raise`).
4. `g x` is an indirect call: load the code pointer from the closure and apply,
   passing the closure (`CM.Apply`).
5. Stack allocation being enabled, the try is wrapped in a local region
   (`beginregion`/`endregion`; `CM.Region.Begin`/`End`).

## Actual `-dcmm -dcanonical-ids`

```
(function camlTrywith_region__tw_0_1_code (g/0: val x/0: val) : int
 (let try_region/0 (beginregion)
   (catch exn
     (catch (exit<push(6)> 7) with(7)
       (let body_result/0
           (app (load_mut int g/0) x/0 g/0 int)
         (exit<pop(6)> *return* body_result/0)))
   with(6 exn/0: val)
     (let unit/0 (seq (endregion try_region/0) 1)
       (if (== exn/0 G:"caml_exn_Not_found") -1 (raise_notrace exn/0))))) )
```

## Verdict

MATCH.

- `(beginregion)` / `(endregion try_region)` — `CM.Region.Begin` / `CM.Region.End`.
- `catch … with(14 exn/336: val)` — the exception-handler catch, `CM.Catch.Exn` /
  `TC.LetCont.Exn` (handler receives the exception value).
- `exit<push(14)>` / `exit<pop(14)>` — trap push on entering the try body and trap
  pop on the normal exit, `CM.Exit.Trap` / `TC.ApplyCont.Jump` trap actions,
  keeping the trap stack balanced (`INV.ToCmm.Control`).
- `raise_notrace exn/336` — reraise when the exception doesn't match, `CM.Raise` /
  `TC.ApplyCont.Raise`.
- `(app (load_mut int g) x g int)` — indirect call: code pointer loaded from the
  closure `g`, applied to `x` with `g` passed as the closure (`CM.Apply`).

## Diagnosis

No discrepancy. This is the sharpest control-flow witness: the trap
`push`/`pop`/`raise` triple, the exn-handler `catch…with`, and the surrounding
region all appear exactly where `TC.LetCont.Exn` / `TC.ApplyCont.*` and the
`CM.*` control rules predict, confirming the target-independent
`INV.ToCmm.Control` lemma end to end.
