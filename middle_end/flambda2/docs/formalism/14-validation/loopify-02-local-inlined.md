# loopify-02: local loopified function is inlined away entirely

Targets the downstream consequence of the loopification rules: loopification
makes a self-tail-recursive function `Non_recursive`
(`S.Rewrite.Code.RecursiveRecompute`), which restores its eligibility for
`Small_function` inlining ([§11](../11-inlining.md)); a local function with all
call sites inlined and no escape then vanishes — code, closure and all.

## Source

`t2.ml`:

```ocaml
let f x =
  let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n) in
  sum x 0
```

Flags: `ocamlopt.opt -c -O3 -dflambda t2.ml`.

## Prediction (before running)

`sum` is marked `Default_loopify_and_tailrec` (`S.Rewrite.Loopify.Attribute`)
and loopified (`S.Rewrite.Loopify.Body` + `.SelfTailCall`), coming out
`Non_recursive` with small cost metrics → `Small_function` inlining decision.
Its single call site `sum x 0` in `f` is inlined, placing the whole
continuation loop inside `f`'s body. With no remaining reference, `sum`'s
closure and code are dead and dropped. Expected final unit: no code binding for
`sum` at all; `f`'s body starts the loop directly.

## Actual IR (final)

The only surviving code is `f`'s, and its body *is* the loop:

```
camlT2__f_0_2_code =
  ((code_metadata (… (recursive Non_recursive) …))
   (λ〈k15〉《k14》⟅⟆ ⟅⟆ (x/26UV …) (my_closure/25N ∷ 𝕍) my_depth/24N .
    (apply_cont self/27 x/26UV 0 …
     self/27 (rec) (n/56UV …) (acc/57UV …) #⊤:
     …
      apply_cont self/27 int_sub/62N int_add/61N …)))
 T2.camlT2__f_2 ↤ (f/0 ∷ 𝕍) =
  (set_of_closures ({((f/0 ∷ 𝕍) camlT2__f_0_2_code)}))
```

No code binding, closure or `Deleted` tombstone for `sum` appears anywhere in
the final term.

## Verdict

MATCH. `sum` vanished entirely; the inlined copy of its (loopified) body — the
`self/27` recursive continuation — is `f`'s loop now.

## Diagnosis

No discrepancy. The disappearance is the intended consequence of the rule
chain: loopify converts "self-recursive function" into "non-recursive function
containing a loop", and ordinary inlining plus dead-code elimination
(`S.Rewrite.Let.DeadBinding`, kept-only-if-used lifted constants) do the rest.
Nothing ever explicitly deletes the function.
