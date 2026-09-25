# exn-demotion: all-or-nothing exception-handler demotion

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/exn_demotion.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/exn_demotion.ml)
(reference: `formalism/exn_demotion.simplify.reference`) — run with
`make -s test-one-no-rebuild TEST=flambda2/examples/formalism/exn_demotion.ml`.

MATCH. Exercises the merged `S.Rewrite.LetCont.DemoteExn`
([`../10-simplify-rewrites.md`](../10-simplify-rewrites.md)) — the escaping gate,
all-or-nothing trap-action erasure, and the bucket-pinning fact.

## The claim under test

An exception handler `k` with no *escaping* use (no surviving `Apply` carries `k`
as its exn continuation) is demoted to an ordinary continuation: its `is_exn_handler`
becomes false and every `Push`/`Pop` trap action naming `k` is erased — all or
nothing. A handler with ANY escaping use keeps its trap frame. Independently, for a
KEPT handler the exn bucket param is unconditionally marked, pinning every raise's
value into `required_names`.

## Source (Hume `exn_demotion.ml`)

```ocaml
exception E of int

(* (a) body has no function calls: handler DEMOTED, push/pop gone, raise a jump *)
let[@inline never] fa b x =
  try (if b then raise (E x) else x + 1) with E n -> n * 2

let[@inline never] callee x = x + 1

(* (b) mixed: one Apply (escaping exn use) + one direct raise -> NOT demoted *)
let[@inline never] fc b x =
  try (if b then raise (E (x * 41 + 1)) else callee x) with _ -> 0
```

## Prediction (before running)

`fa`'s try body contains no `Apply`, so the handler has no escaping use: it is
DEMOTED. Its `Push`/`Pop` disappear, the `raise (E x)` becomes a plain jump into the
handler, and the handler body (`n * 2`) is reached by ordinary control flow. `fc`'s
try body contains `callee x` — an `Apply` whose exn continuation is `k`, recorded
escaping — so `fc`'s handler is NOT demoted: its `Push`/`Pop` survive and the raise
keeps its `Pop`.

## Actual IR (verified, `exnd_simp.out`)

- `fa_0_1`: the handler is dissolved — no `push`/`pop` anywhere; the two switch arms
  jump straight to `k/86` (one computing `x + 1`, one computing `x * 2`). The
  `begin_try_region`/`end_try_region` pair survives (a small missed optimization,
  consistent with `End_try_region` never being deleted).
- `fc_2_1`: `cont k/95 push(k/93)` and, in the raising arm, `cont k/93 pop(regular
  k/93) (Pmakeblock/210)` — the trap frame is intact and the raise carries its
  `Pop`. NOT demoted.

## Verdict

MATCH, both directions. `fa`'s call-free handler is demoted and its traps erased
wholesale; `fc`'s mixed handler (one escaping `Apply` use) keeps every trap — the
all-or-nothing property. Note the bucket-pinning corollary: in `fc` the raise's
value `Pmakeblock/210` (the `E (x*41+1)` allocation) SURVIVES even though the
handler `with _ -> 0` ignores the bucket, because `flow_acc.enter_continuation`
unconditionally marks the exn bucket param of the kept handler. Consequently
`patch_unused_exn_bucket` never fires (dead code) — recorded in
`consolidation-code-hygiene.md`. The `-g` sensitivity of the escaping gate (a
`Regular` raise is escaping only with debug info) is documented in the rule and was
witnessed by the campaign's `berk4.fl`/`berk4g.fl` differential.
