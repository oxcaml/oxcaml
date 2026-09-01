# dead-value-slot-coherence: a captured variable dropped when the slot is never projected

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/dead_value_slot_coherence.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/dead_value_slot_coherence.ml)
(reference: `formalism/dead_value_slot_coherence.simplify.reference`) — run with
`make -s test-one-no-rebuild TEST=flambda2/examples/formalism/dead_value_slot_coherence.ml`.

MATCH. Exercises the Simplify side of the merged whole-unit invariant
`INV.Simplify.DeadValueSlotCoherence` ([`../13-soundness.md`](../13-soundness.md)),
whose to_cmm face is `INV.ToCmm.SlotLiveness`
([`../20-to-cmm-soundness.md`](../20-to-cmm-soundness.md)) — the two are faces of one
pruning event over the single `used_value_slots` set.

## The claim under test

A value slot `w` never projected in the WHOLE unit leaves `dacc.used_value_slots`.
The flow-graph capture edge `closure → captured_contents(w)` is then gated off
(`is_value_slot_used`), so a variable captured ONLY in `w` leaves `required_names`
and its binding is deleted — even when the closure itself is live/escaping/exported.
The fate of a binding at the top of the unit depends on the absence of projections
at the bottom.

## Source (Hume `dead_slot.ml`)

```ocaml
let[@inline never] mk () =
  let x = (1, 2) in
  fun () -> x

let f = mk ()
let r = (opaque f) ()
```

## Prediction (before running)

`x = (1, 2)` is a constant pair, reified and lifted to a symbol. The inner closure
`fun () -> x` captures `x` in a value slot `w`, and its body projects `w` to return
`x`. But the projection folds to the symbol directly (`S.Rewrite.Prim.Projection`,
`Known_result`), which deliberately does NOT record `w` into `used_value_slots`. So
`w ∉ used_value_slots`: the slot is dropped from the closure layout and the capture
edge is gated off — the emitted closure carries no value slot, and its body returns
the symbol.

## Actual IR (verified, `dslot_simp.out`)

```
let $camlDead_slot__const_block9 = Block 0 (1, 2) in
...
let code … `fn[dead_slot.ml:7,2--13]_1_1` (param/98 : imm tagged)
        my_closure/97 _region _ghost_region my_depth/96 … =
  cont k/45 ($camlDead_slot__const_block9)          (* returns the symbol directly *)
in
let $`camlDead_slot__fn…_3` = closure `fn…_1_1` @`fn…`   (* closure: no value slot *)
```

The closure `fn…_1_1` has no value slot in its layout; its body is
`cont k/45 ($camlDead_slot__const_block9)` — the symbol, not a `Project_value_slot`.
The capture of `x` was dropped even though the closure `f` is live and escapes
(`(opaque f) ()`).

## Verdict

MATCH. The slot is dropped and the closure references the lifted symbol directly,
exactly as `INV.Simplify.DeadValueSlotCoherence` predicts. The load-bearing detail
is that the fast-path projection (folded to `Known_result`) never records the slot,
which is precisely the elimination mechanism: the recording caller
`DA.add_use_of_value_slot` fires only on the `Need_meet` branch of
`simplify_project_value_slot`. Hume's adversarial `slot_incoherence.ml` (mixed
benign + existential uses, dead-branch-only in-code projection) also compiled clean,
confirming the survival ⇒ recorded alignment that closes the unboxing recording gap
(the FRAGILITY noted in the rule — a defensive `add_use_of_value_slot` in `unbox_arg`
would make it structural; see `consolidation-code-hygiene.md`).
