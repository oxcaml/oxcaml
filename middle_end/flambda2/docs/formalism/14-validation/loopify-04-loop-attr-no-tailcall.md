# loopify-04: `[@loop]` with no self tail call — the wrapper collapses back

Targets `S.Rewrite.LetCont.Demote` (with `S.Rewrite.LetCont.Inline`) and the
boundary of the loopification rules: when loopification fires on a function
with **no** self tail call, the self continuation gets no recursive use, is
demoted to non-recursive, and is inlined away — the one way a `should_loopify`
function's body can emerge without the wrapper, and the original body survives
(which is why the no-original-survival consequence holds only for
`Default_loopify_and_tailrec`, where a redirected self tail call always
populates the loop).

## Source

`d.ml`:

```ocaml
let[@loop] rec fact n = if n = 0 then 1 else n * fact (n - 1)
```

Flags: `ocamlopt.opt -c -O3 -dflambda d.ml`.

## Prediction (before running)

`[@loop]` forces `Always_loopify` (`S.Rewrite.Loopify.Attribute`), so
`S.Rewrite.Loopify.Body` wraps the body despite the absence of any self tail
call. `S.Rewrite.Loopify.SelfTailCall` never fires (the one self-call has a
non-return continuation, being under `*`), so the `self` continuation's only
use is the entry `apply_cont`. `sort_handlers` classifies it `No_loop`
(`S.Rewrite.LetCont.Demote`), the demoted handler has a single inlinable use,
and `S.Rewrite.LetCont.Inline` substitutes it back: expected final body =
the original body of `fact` (self-`Apply` intact, possibly with a leftover
parameter-renaming `let` from the collapsed `apply_cont`), metadata
`recursive Recursive`, `loopify Always_loopify` (unchanged by
`S.Rewrite.Loopify.AttributeUpdate`).

## Actual IR (final, body of `fact`)

```
camlD__fact_0_1_code =
  ((code_metadata (… (recursive Recursive) …
     (inlining_decision Recursive) (loopify Always_loopify)))
   (λ〈k14〉《k13》⟅⟆ ⟅⟆ (n/21UV …) (my_closure/20N ∷ 𝕍) my_depth/19N .
    n/22UV = n/21UV
    prim/25N = (((Phys_equal Eq) n/22UV 0) …)
    ((switch prim/25N …
     | 0 ↦ goto k19
     | 1 ↦ return k14 1)
     k19 #One:
     (int_sub/29N = ((-Tagged_immediate n/22UV 1) …)
      (apply
       (((Some (coerce D.camlD__fact_1 (depth my_depth/19N -> (succ my_depth/19N))))〈k20〉《k13》(int_sub/29N))
        … (call_kind (Function (function_call (Direct camlD__fact_0_1_code)))) …))
      …)))
```

## Verdict

MATCH. No `self` continuation survives; the body is the original recursive one.
The `n/22UV = n/21UV` renaming is the trace of the collapse: it is the
`let p = a` binding introduced when `S.Rewrite.LetCont.Inline` substituted the
handler for the single entry-`apply_cont` use.

## Diagnosis

No discrepancy — but this case is exactly why the no-original-survival
consequence requires `Default_loopify_and_tailrec` rather than `should_loopify`: under
`Always_loopify` the wrap is still unconditional, yet with nothing redirected
into the loop the wrapper is undone by generic continuation rules, and the
original `Apply`-recursive body is re-emitted.
