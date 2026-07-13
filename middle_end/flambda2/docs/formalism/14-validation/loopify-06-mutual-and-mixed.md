# loopify-06: mutual recursion is never loopified; `[@loop]` redirects only self tail calls

Targets `S.Rewrite.Loopify.Attribute` (the single-function-group premise) and
`S.Rewrite.Loopify.SelfTailCall` (its premises select exactly the self tail
calls, leaving other applies alone).

## Sources

`g.ml` (mutual recursion, all calls in tail position):

```ocaml
let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)
```

`e.ml` (`[@loop]` on a mixed tail/non-tail self-recursive function):

```ocaml
let[@loop] rec go n = if n > 100 then go (n - 100) else if n = 0 then 0 else n + go (n - 1)
```

Flags: `ocamlopt.opt -c -O3 -dflambda <file>`.

## Prediction (before running)

`g.ml`: `even` and `odd` are a two-function recursive group, so
`is_purely_tailrec` starts (and stays) false for both:
`S.Rewrite.Loopify.Attribute` assigns `Default_loopify_and_not_tailrec`. No
loopification; the cross-calls remain (tail) `Apply`s; both come out
`recursive Recursive`, `loopify Never_loopify`.

`e.ml`: `Always_loopify` forces the wrap (`S.Rewrite.Loopify.Body`). Of the two
self-calls, only `go (n - 100)` satisfies `S.Rewrite.Loopify.SelfTailCall`'s
premises (return continuation = the function's); `go (n - 1)` returns to the
continuation of the `+`, so it stays an `Apply`. Expected body: entry
`apply_cont self …`, a `self (rec)` handler whose `> 100` arm ends in
`apply_cont self …` and whose other arm contains a direct self-`Apply`;
metadata `recursive Recursive` (the surviving self-`Apply`'s rec_info coercion
mentions `my_depth` — `S.Rewrite.Code.RecursiveRecompute`), `loopify
Always_loopify` (unchanged).

## Actual IR (final, key parts)

`g.ml`: both codes emitted with `(recursive Recursive)`,
`(loopify Never_loopify)`; `even`'s body ends its recursive arm in
`apply … 〈k17〉《k16》 … (Direct camlG__odd_1_3_code)` (a tail `Apply` of `odd`),
and symmetrically for `odd`. No `self` continuation anywhere.

`e.ml`:

```
camlE__go_0_1_code =
  ((code_metadata (… (recursive Recursive) … (loopify Always_loopify)))
   (λ〈k18〉《k17》⟅⟆ ⟅⟆ (n/25UV …) (my_closure/24N ∷ 𝕍) my_depth/23N .
    (apply_cont self/19 n/25UV
     self/19 (rec) (n/26UV …) #⊤:
     …
      k25 #One:
      int_sub/41N = ((-Tagged_immediate n/26UV 100) …)
      apply_cont self/19 int_sub/41N …
      k24 #One:
      …
       (apply
        (((Some (coerce E.camlE__go_1 (depth my_depth/23N -> (succ my_depth/23N))))〈k28〉《k17》(int_sub/36N))
         … (call_kind (Function (function_call (Direct camlE__go_0_1_code)))) …))
       …)))
```

## Verdict

MATCH (both files). In `e.ml` the tail self-call became `apply_cont self/19`
while the non-tail self-call survived as a direct `Apply` inside the same
loopified body.

## Diagnosis

No discrepancy. Together with loopify-03 this pins the scope of loopification:
the attribute is per-function and all-or-nothing (`Attribute`), the wrap is
attribute-driven (`Body`), and the redirect is per-`Apply` with purely local
premises (`SelfTailCall`) — so mixed functions under `[@loop]` get a *partial*
loop and keep `Recursive` metadata, and mutual recursion is untouched however
tail-ish its calls are.
