# new-07: float accumulator unboxed across a loop

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/float_unbox.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/float_unbox.ml) (reference: `formalism/float_unbox.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/float_unbox.ml`.

Targets `S.Unbox.ContParam.*` and `S.Unbox.Mutable.*` (ch. 12): a `float ref`
accumulator loop drives the recursive loop continuation to a naked-`float`
parameter, with `Box_number` only on the loop-exit edge.

## Source

`t07.ml`:

```ocaml
let f () =
  let r = ref 0. in
  for i = 1 to 1000 do r := !r +. float i done;
  !r
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 07.raw.fl -dfexpr-to 07.final.fl t07.ml`.
(Loop-parameter unboxing to `int64` + `float` also occurs at default optimization
level.)

## Raw IR

After CPS the loop is a recursive continuation whose parameters are the boxed
loop counter and the (boxed) `float ref` cell; a boxed float is threaded around
the loop.

## Prediction (before running)

Two unboxings apply, per ch. 12 §8:

- `S.Unbox.Mutable.Candidate` / `.Rewrite`: `r` is a locally-created `float ref`
  that never escapes (only `Block_load`/`Block_set` touch it), so it is dissolved
  into a threaded field — no `ref`/`Block` survives.
- `S.Unbox.ContParam.*` (`Optimistic.Number` → `Beneficial` →
  `ContParam.Rewrite`): the loop continuation's accumulator parameter is a boxed
  float of known shape, replaced by a naked-`float` parameter; the back-edge
  passes the naked float as an `Already_in_scope` extra arg. The counter is
  likewise unboxed to naked `int64`.

Expected: recursive loop continuation has parameters `(… : int64, … : float)`;
arithmetic inside runs on naked floats (`%bfloat_arith.add`); a single
`%box_num.float` produces the boxed result on the loop-exit edge.

## Actual IR (final, loop continuation `k/55`)

```
where rec k/55 (for_counter_naked/187 : int64, unboxed_float/188 : float) =
  let prim/189 = %num_conv.[int64].[imm] (for_counter_naked/187) in
  let prim/190 = %num_conv.[imm].[float] (prim/189) in
  let prim/191 = %bfloat_arith.add (unboxed_float/188, prim/190) in
  let float_add/192 = %box_num.float (prim/191) in
  let for_next_naked/193 = %int_barith.int64.add (for_counter_naked/187, 1L) in
  let prim/194 = %int_comp.int64.le (for_next_naked/193, 1000L) in
  switch prim/194
    | 0 -> k/54 (float_add/192)               (* exit: boxed result *)
    | 1 -> k/55 (for_next_naked/193, prim/191) (* back-edge: naked float *)
```

## Verdict

MATCH (with one placement note). The loop continuation takes an unboxed
`int64` counter and a naked-`float` accumulator; the back-edge passes the naked
float directly; the only surviving box is the `%box_num.float` reaching the exit
continuation `k/54`. The `ref` cell and all `!r` / `r := …` accesses are gone.

## Diagnosis

Descriptive-heuristic placement detail, not a rule violation: the
`%box_num.float` is hoisted *above* the switch (computed on every iteration) and
then consumed only on arm 0, rather than being sunk into the exit arm as in the
ch. 12 §8 worked example. In that example an intervening `if i mod 2` branch
blocked the hoist; here the straight-line body lets the box float above the
switch. Because `Box_number` has only generative effects and is used on the exit
arm, it is neither dead (`S.Rewrite.Let.DeadBinding` does not fire) nor forced
into the arm; this is code placement, and consistent with the unboxing rules —
the accumulator parameter itself is unboxed exactly as `S.Unbox.ContParam.Rewrite`
predicts. Worth recording as an observation that box sinking into switch arms is
not guaranteed.
