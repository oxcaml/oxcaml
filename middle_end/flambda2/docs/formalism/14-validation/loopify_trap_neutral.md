# loopify-trap-neutral: loopified self-loops are trap-depth-neutral (Pop, never Push)

MATCH (with a restatement). Exercises `INV.Loopify.TrapNeutral`
([`../13-soundness.md`](../13-soundness.md)) and its interaction with
`S.Rewrite.Loopify.SelfTailCall`, `S.Rewrite.LetCont.Shortcut`, and
`S.Rewrite.LetCont.DemoteExn` ([`../10-simplify-rewrites.md`](../10-simplify-rewrites.md)).
Case-study-only: the witnesses are `.fl`/`-dcmm`/`-g`-differential runs from the
campaign, not a fixed-flag `formalism/` fexpr example.

## The claim under test

For a loopified function with self continuation `k`: (a) the entry jump and every
`SelfTailCall` redirect are trap-action-FREE, at entry trap depth; (b) every
`apply_cont` to `k`, including shortcut-retargeted jumps, is trap-DEPTH-neutral; (c)
in Cmm the loop is a recursive `Ccatch` whose every `Cexit` is depth-matched — a
`Cexit` to the loop label MAY carry a `Pop` (a shortcut-retargeted try-exit) but
NEVER a `Push`; (d) self calls in a `with`-branch are loopifiable, self calls inside
a `try` body are not.

## Prediction (before running)

```ocaml
let[@loop] rec f x = try ... with Not_found -> f (x - 1)   (* with-branch call *)
```
The `with`-branch runs after the `Pop`, at entry depth, so `f (x-1)` there is a self
tail call satisfying `SelfTailCall`'s exn-continuation premise: it is redirected to
`cont self`, with no trap action. A self call inside the `try` BODY fails that
premise (a trap handler pushed since entry is still live) and is NOT redirected.

## Actual (verified, campaign repros)

- `berk4.fl` (`[@loop] rec f` with the self call in the `with`-branch): the
  with-branch call IS redirected — `cont self/39`, no trap action, at entry depth.
- `berk4b.fl` (self call inside the `try` body): NOT redirected — a direct `apply`
  survives and the code is printed `rec`.
- Schopenhauer's counterexample
  `let rec f g x = if x = 0 then 0 else ((try ignore (g ()) with _ -> ()); f g x)`
  at `-O3`: the try's normal-exit continuation collapses onto the loop by Shortcut,
  yielding `pop_trap then goto self` in fexpr and `(exit<pop(7)> 6)` in Cmm — a
  Pop-carrying `Cexit` to the recursive catch. This refuted the original "every
  apply_cont to k carries no trap action" and drove the restatement to legs
  (a)/(b)/(c): action-freeness for loopify-CREATED jumps, depth-neutrality for all,
  Pop-but-never-Push in Cmm.
- `berk4c.cmm` / `berk4_trap.cmm`: `exit<push(...)>` targets only the try-body
  label, never the loop label — confirming leg (c)'s never-Push half. The only
  `Push` creation site in the pipeline is the trywith translation at CPS conversion
  (`lambda_to_flambda.ml`), which targets the fresh try-body continuation; self
  continuations do not exist until Simplify, which never creates `Push` actions, and
  `Shortcut_to` rejects trap-carrying handlers.

## Verdict

MATCH under the restatement. Loopify-created jumps are trap-action-free; all jumps
to `k` are depth-neutral; Cmm `Cexit`s to the loop label carry at most a `Pop`,
never a `Push`; `with`-branch self calls loopify, `try`-body self calls do not. The
`-g` sensitivity (a local `try…with` costs a runtime trap frame only with debug
info) is the `S.Rewrite.LetCont.DemoteExn` differential, witnessed by
`berk4.fl`/`berk4g.fl` (0 trap actions without `-g`; `push`/`pop` and a surviving
handler with `-g`) and `berk10_nog.dump`/`berk10_g.dump`; in the `-g` version the
redirected `cont self` back-edge still carries no trap action and sits at entry
depth, after the handler's pop — reconfirming `INV.Loopify.TrapNeutral`.

Repros: `Schopenhauer-the-Skeptic/_schop-exp/berk4*.fl`, `berk4c.cmm`,
`berk4_trap.*`, `berk10_{g,nog}.dump`.
