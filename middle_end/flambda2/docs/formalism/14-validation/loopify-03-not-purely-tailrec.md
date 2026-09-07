# loopify-03: not purely tail-recursive ⇒ not loopified, original survives

Targets `S.Rewrite.Loopify.Attribute` and `S.Rewrite.Loopify.AttributeUpdate`
from the negative side, bounding the loopification rules: a function with a
non-tail self-call — even one that *also* has a self tail call — is not marked
for loopification, and its `Apply`-recursive form survives Simplify.

## Sources

`c.ml` (self-call not in tail position):

```ocaml
let rec fact n = if n = 0 then 1 else n * fact (n - 1)
```

`e2.ml` (one tail and one non-tail self-call):

```ocaml
let rec go n = if n > 100 then go (n - 100) else if n = 0 then 0 else n + go (n - 1)
```

Flags: `ocamlopt.opt -c -O3 -dflambda <file>`.

## Prediction (before running)

In both files every function is a single-function `let rec`, but
`is_purely_tailrec` is falsified by the non-tail occurrence of `my_closure`
(the `fact (n-1)` under `*`; the `go (n-1)` under `+`), so
`S.Rewrite.Loopify.Attribute` assigns `Default_loopify_and_not_tailrec`.
`should_loopify` is false: no wrapping, no redirect. Expected output for both:
the body contains direct self-`Apply`(s) — including, in `e2.ml`, the self
*tail* call `go (n - 100)`, which stays an `Apply` because
`S.Rewrite.Loopify.SelfTailCall` only fires under `loopify_state = Loopify k` —
metadata `recursive Recursive`, and `loopify Never_loopify` by
`S.Rewrite.Loopify.AttributeUpdate`.

## Actual IR (final, key parts)

`c.ml`:

```
camlC__fact_0_1_code =
  ((code_metadata (… (recursive Recursive) …
     (inlining_decision Recursive) (loopify Never_loopify)))
   (λ〈k14〉《k13》⟅⟆ ⟅⟆ (n/21UV …) (my_closure/20N ∷ 𝕍) my_depth/19N .
    …
     (apply
      (((Some (coerce C.camlC__fact_1 (depth my_depth/19N -> (succ my_depth/19N))))〈k19〉《k13》(int_sub/27N))
       … (call_kind (Function (function_call (Direct camlC__fact_0_1_code)))) …))
      …))
```

`e2.ml`: same shape — `recursive Recursive`, `loopify Never_loopify`, and
*both* self-calls (the tail `go (n - 100)` and the non-tail `go (n - 1)`)
remain `Apply`s with `Direct camlE2__go_0_1_code` call kinds.

## Verdict

MATCH (both files).

## Diagnosis

No discrepancy. The no-original-survival consequence of the loopification
rules is scoped to `Default_loopify_and_tailrec`: a *mixed* tail/non-tail recursive
function keeps even its self tail calls as `Apply`s, because the loopify
attribute is all-or-nothing per function and is decided purely by
`is_purely_tailrec` at closure conversion.
