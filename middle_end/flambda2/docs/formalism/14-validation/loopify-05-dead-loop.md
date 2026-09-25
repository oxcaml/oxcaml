# loopify-05: loopified function whose loop is provably dead

Targets the composition of loopification with constant folding and dead-code
rules in a degenerate case: even when the recursive branch is eliminated
entirely, what survives is a *non-recursive* function — never the original
`Apply`-recursive one.

## Source

`f.ml`:

```ocaml
let rec f n = if n = n then n else f (n - 1)
```

Flags: `ocamlopt.opt -c -O3 -dflambda f.ml`.

## Prediction (before running)

Closure conversion sees a single-function `let rec` whose one self-reference is
a tail call (in the syntactic `else` branch), so `f` is marked
`Default_loopify_and_tailrec` and loopified (`S.Rewrite.Loopify.Attribute`,
`.Body`, `.SelfTailCall`). During simplification of the wrapped body,
`Phys_equal n n` folds to true (`S.Rewrite.Prim.PhysEqual`), the recursive arm
is pruned (`S.Rewrite.Switch.ArmPrune`), and the redirected
`apply_cont self …` disappears with it. The `self` continuation is left with
only its entry use → demoted (`S.Rewrite.LetCont.Demote`) and inlined
(`S.Rewrite.LetCont.Inline`). Expected survivor: a trivial non-recursive
`f n = n` — `recursive Non_recursive`, `loopify Already_loopified`
(`S.Rewrite.Code.RecursiveRecompute`, `S.Rewrite.Loopify.AttributeUpdate`).

## Actual IR (final, body of `f`)

```
camlF__f_0_1_code =
  ((code_metadata (… (recursive Non_recursive)
     (cost_metrics size: 0 removed: {… prim: 6 branch: 1 …}) …
     (inlining_decision (Small_function (size 0) (small_function_size 100)))
     (loopify Already_loopified)))
   (λ〈k13〉《k12》⟅⟆ ⟅⟆ (n/19UV …) (my_closure/18N ∷ 𝕍) my_depth/17N .
    n/20UV = n/19UV
    return k13 n/20UV))
```

## Verdict

MATCH. Size-0 body `return n`; `Non_recursive`; `Already_loopified`; the
`n/20UV = n/19UV` renaming again betrays the collapsed entry `apply_cont`.

## Diagnosis

No discrepancy. A source-recursive function whose recursion Simplify can prove
dead exits the pass with no trace of recursion in either its body or its
metadata; the `Already_loopified` attribute is the only reminder that the
loopify machinery ran.
