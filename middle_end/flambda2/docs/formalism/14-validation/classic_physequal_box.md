# classic-physequal-box: `-Oclassic` `Box_number` duplication vs deterministic `PhysEqual` (FORMALISM BUG)

Like [`float32_double_round`](float32_double_round.md), this is a **MISMATCH**, not
a MATCH — but here the defect is in the **formalism**, not the compiler. The
composed rules of chapters 06, 18 and 20 contradict an observable run of a program
that has **no undefined behaviour**. It is the headline finding of the
believers/skeptics campaign (Emerson found it; Sextus independently reproduced it).

Targets `INV.ToCmm.Simulates` ([`../20-to-cmm-soundness.md`](../20-to-cmm-soundness.md)
§2, §5.6), `P.Effects.DelayDuplicable` ([`../06-primitives-memory.md`](../06-primitives-memory.md)),
`INV.ToCmm.EffectLinear` ([`../20-to-cmm-soundness.md`](../20-to-cmm-soundness.md)),
and `P.Binary.PhysEqual` ([`../06-primitives-memory.md`](../06-primitives-memory.md)).

## The claim under test

`INV.ToCmm.Simulates` says `to_cmm` preserves observable behaviour up to `≈`,
modulo undefined behaviour and resource exhaustion. Physical equality of two
already-existing values is, in the abstract semantics, deterministic:
`P.Binary.PhysEqual(ptr ℓ, ptr ℓ) = 1` and `Opaque_identity` is the identity
([`../04-opsem.md`](../04-opsem.md)/[`../06-primitives-memory.md`](../06-primitives-memory.md)).
So a program that boxes once and compares the box with itself must observe `true`
in both the abstract run and the Cmm run.

## Source (Emerson `scratch/physeq2.ml`, Sextus `sextus-exp/sx_physeq.ml`)

```ocaml
let[@inline never] test x =
  let b = x +. 1.0 in
  (Sys.opaque_identity b == b, b)
```

## Prediction (before running)

`b = x +. 1.0` binds one boxed float. `Sys.opaque_identity b` is the identity, so
the two operands of `==` are the same value `ptr ℓ`; `P.Binary.PhysEqual` folds to
`1`, so `test` returns `(true, b)`. Since `INV.ToCmm.Simulates` claims the Cmm run
observes the same result, we predict **`true` in every mode**.

## Actual (verified, campaign repros)

In **classic** mode a heap `Box_number` for `b` is placement `Delay`,
`Only_generative_effects Immutable`, `No_coeffects`
(`flambda_primitive.ml#effects_and_coeffects`; `P.Effects.DelayDuplicable`). Classic
mode SKIPS Simplify (`flambda2.ml:163-71` routes raw_flambda straight to `to_cmm`),
so `b` still has `More_than_one` occurrence when `to_cmm`'s `classify_let_binding`
sees it — and `Delay` + `More_than_one` ⇒ `Must_inline_and_duplicate`
(`to_cmm_effects.ml`; `INV.ToCmm.EffectLinear`). The `-Oclassic -dcmm` for
`test` (Emerson `physeq2.cmm`) shows the ONE Flambda box as **THREE** separate
`alloc 1277` sites:

```
(let
  (to_cmm_split_3/447 (+f (load float64 x/442) 1.)
   Popaque/448 (opaque (alloc 1277 to_cmm_split_3/447)))       ; box #1 (opaque arg)
  (alloc 2048
    (+ (<< (== Popaque/448 (alloc 1277 to_cmm_split_3/447)) 1) 1)  ; box #2 (== rhs)
    (alloc 1277 to_cmm_split_3/447)))                          ; box #3 (tuple field)
```

The `==` now compares two **distinct** freshly-allocated boxes, so it yields `0`.
Runtime A/B (both `.exe` captured): `-Oclassic` prints **false**, default mode
prints **true**. Sextus confirmed the classic path abstractly: `-drawfexpr` shows
ONE `%box_num` with 3 uses, so the abstract machine (which never duplicates) binds
`b ↦ ptr ℓ` once and `PhysEqual(ptr ℓ, ptr ℓ) = 1` ⇒ **true**; the Cmm run gives
**false**; the two traces differ.

## Verdict

MISMATCH — a **formalism** bug (over-specified `PhysEqual`), not a compiler bug and
not an excluded case. `(==)` on immutable values is implementation-defined (the
OCaml manual), so the compiler is free to duplicate; the abstract semantics is what
over-commits by giving `P.Binary.PhysEqual(ptr ℓ, ptr ℓ)` a deterministic answer.
`INV.ToCmm.Simulates` as stated is therefore violated by a UB-free program.

## Diagnosis and fix (formalism, not code)

The gap is in `to_cmm`'s own machinery (the Delay-duplication license of
`INV.ToCmm.EffectLinear` / `P.Effects.DelayDuplicable`) colliding with the
deterministic `P.Binary.PhysEqual`. Two admissible repairs (see
[`../20-to-cmm-soundness.md`](../20-to-cmm-soundness.md) §5.6):

1. **Weaken `P.Binary.PhysEqual`** ([`../06-primitives-memory.md`](../06-primitives-memory.md))
   to a *nondeterministic* result whenever an argument points to an immutable
   block, making duplication/dropping of `Only_generative_effects Immutable`
   bindings observationally sound. The weakened rule must still license the
   `simplify_phys_equal` folds (`S.Rewrite.Prim.PhysEqual`) under a "same evaluation
   of the same binding" side condition, or constant folding of `==` breaks.
2. **Add a "modulo immutable-block identity" clause** to `INV.ToCmm.Simulates`,
   parallel to its resource-exhaustion clause.

The `INV.ToCmm.EffectLinear` zero-occurrence **Drop** arm needs the same license,
with `Gc.stat` / minor-words allocation counters as the weaker observable there —
so `R.Observe` must carve out allocation counters.

**RESOLUTION ADOPTED (2026-07-22, jointly with
[`../13-soundness.md`](../13-soundness.md) §4 item 8):** repair 1 in
strengthened form (`P.Binary.PhysEqual` loose on all immutable heap objects,
result 0 derivable even for identical pointers — covering duplication AND
dropping) together with repair 2's general observational analogue (13 §1's
immutable-identity folding and refinement reading, rather than a
Simulates-local clause). This study's mismatch is thereby reclassified:
`-Oclassic`'s **false** is a derivable observation of the abstract run.

Repros: `Emerson-the-Believer/scratch/physeq2.*`,
`Sextus-the-Skeptic/sextus-exp/sx_physeq.ml`, `sx_classic.cmm`, `sx_default.cmm`.
Not added as a `formalism/` fexpr test: classic mode skips Simplify, so the
`dump-simplify` harness would show nothing; the witness is a `-dcmm` / runtime A/B.
