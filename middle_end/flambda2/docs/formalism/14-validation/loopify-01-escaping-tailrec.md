# loopify-01: escaping purely-tail-recursive function survives only as a wrapper

Targets `S.Rewrite.Loopify.Body`, `S.Rewrite.Loopify.SelfTailCall`,
`S.Rewrite.Loopify.AttributeUpdate`, `S.Rewrite.Code.RecursiveRecompute` and
`S.Struct.Loopify`: a purely self-tail-recursive function that escapes
(exported from the module, so its closure and code must survive) is
nevertheless never emitted in its original `Apply`-recursive form.

## Source

`a.ml`:

```ocaml
let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)
```

Flags: `ocamlopt.opt -c -O3 -dflambda a.ml` (the `-dflambda` "After simplify"
dump shows the code metadata, which `-dfexpr` does not).

## Prediction (before running)

`sum` is the sole function of its `let rec` and its only self-reference is a
tail call, so closure conversion marks it `Default_loopify_and_tailrec`
(`S.Rewrite.Loopify.Attribute`). `should_loopify` holds, so Simplify
unconditionally wraps the body in a recursive continuation
(`S.Rewrite.Loopify.Body`) and redirects the self tail call to it
(`S.Rewrite.Loopify.SelfTailCall`). Because the closure escapes into the module
block, the code binding must survive — but only as the loopified wrapper:

- body = `apply_cont self n acc` entering a `self (rec)` handler containing the
  loop, with the recursive branch ending in `apply_cont self …`, and **no**
  self-`Apply` anywhere;
- metadata: `recursive Non_recursive` (`S.Rewrite.Code.RecursiveRecompute`: no
  self-`Apply` means no `my_depth` occurrence) and `loopify Already_loopified`
  (`S.Rewrite.Loopify.AttributeUpdate`).

## Actual IR (final, body and key metadata of `sum`'s code)

```
camlA__sum_0_1_code =
  ((code_metadata
    (… (recursive Non_recursive) …
     (inlining_decision (Small_function (size 18) (small_function_size 100)))
     (loopify Already_loopified)))
   (λ〈k13〉《k12》⟅⟆ ⟅⟆ (n/21UV …) (acc/22UV …) (my_closure/20N ∷ 𝕍) my_depth/19N .
    (apply_cont self/14 n/21UV acc/22UV
     self/14 (rec) (n/23UV …) (acc/24UV …) #⊤:
     prim/28N = (((Phys_equal Eq) n/23UV 0) …)
     ((switch prim/28N …
      | 0 ↦ goto k18
      | 1 ↦ return k13 acc/24UV)
      k18 #One:
      int_add/31N = ((+Tagged_immediate acc/24UV n/23UV) …)
      int_sub/32N = ((-Tagged_immediate n/23UV 1) …)
      apply_cont self/14 int_sub/32N int_add/31N …))))
```

## Verdict

MATCH. The emitted code is exactly the wrapper: entry `apply_cont self/14`, a
`self/14 (rec)` handler, recursive branch `apply_cont self/14`. There is no
`Apply` in the body at all (also visible: `is_my_closure_used false`), the
metadata is `Non_recursive` / `Already_loopified`, and the now-non-recursive
size-18 function gets a `Small_function` inlining decision that the recursive
original could not have had.

## Diagnosis

No discrepancy. Note the original body shape survives nowhere — the escape only
forces *a* code binding to exist as the out-of-line entry point, and that
binding is the loopified form.
