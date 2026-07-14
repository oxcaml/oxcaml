# to_cmm soundness: the cross-language simulation

*Part of the Flambda 2 formalism; see [README.md](README.md).*

Chapters [`15`](15-cmm.md)–[`19`](19-cmm-memory-gc.md) defined the Cmm target
machine, the `to_cmm` translation `⤳` (control [`16`](16-to-cmm-control.md), data
[`18`](18-to-cmm-data.md)), the representation relation `≈`
([`17`](17-representation.md)), and the concrete memory/GC model. This chapter
states the property tying them together: **`to_cmm` preserves observable
behaviour, through `≈`**. Where Simplify's soundness ([`13`](13-soundness.md)) is
an *equivalence within one machine*, `to_cmm`'s is a **forward simulation between
two machines** carrying `≈` — the standard shape of a compiler-pass correctness
statement. As with [`13`](13-soundness.md), the claim is not proved; it is the
design intent of every rule in [`15`](15-cmm.md)–[`19`](19-cmm-memory-gc.md) and
is empirically validated by the case studies in
[`14-validation/`](14-validation/) (`tocmm-*`).

## 1. Setup: related configurations

`to_cmm` compiles a unit `U` to a Cmm program `P = ⟦U⟧` (a set of `Cfunction`s —
one per `Code` plus the module initialiser — and `Cdata` for static constants;
[`18`](18-to-cmm-data.md) `TC.Let.Static`, [`15`](15-cmm.md) §10). Relate a
Flambda configuration to a Cmm one by combining `≈` on the heap with the
component correspondences already established:

```
⟨e, ρ, K, H, T, R⟩  ≈cfg  ⟨e_c, ce, χ, M, TT, RR⟩    iff
  e_c is the translation of e up to the Cmm micro-steps already taken
    -- an evaluation-context refinement of Θ ⊢ e ⤳ · (16/18): initially e_c is the
    -- syntactic image of e; after a Cmm-only stutter (a CM.Alloc.GC step, or the
    -- materialization of a delayed binding, 18 TC.Let.Subst) that leaves e unchanged,
    -- e_c is a partially-evaluated form of that image (NOT its verbatim syntactic image)
  H ≈_L M                              -- heaps related (R.Heap)
  ρ ≈ ce   : for each x, ρ(x) ≈ᵥ (value of V(x) in ce, M)      (R.Val.*, TC.Simple)
  K ≈ χ    : via Φ (Jump↦CHandler@label, Inline↦spliced, Return↦boundary, Exn↦Exn_handler; 16 §8)
  T ≈ TT   : same handler identities, same depth (16, INV.ToCmm.Control)
  R ≈ RR   : same region handles in order (19 §2)
```

## 2. The simulation claim

```rule
RULE INV.ToCmm.Simulates
STATUS conjectured
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
---
Let P = ⟦U⟧ and let the initial Cmm state of P's module initialiser be
initc(P), with initial(U) ≈cfg initc(P) (OS.Unit.Init ≈ the entry of the module
initialiser; H₀ ≈ M₀ on predefined symbols).
If the Flambda run of U does not exhibit undefined behaviour (as 13:
INV.Simplify.Preserves), then the Cmm run of P either
  • terminates normally with a memory M and the Flambda run terminates normally with
    heap H, and H(sym_mod) ≈ (module image in M) with equal C-call traces (R.Observe); or
  • both terminate by the same uncaught exception (values related by ≈ᵥ); or
  • both diverge; or
  • the Cmm run additionally exhausts resources (CM.Alloc.Exhaustion) where the
    Flambda run diverges or terminates.
In every outcome except resource exhaustion the two runs perform the SAME C-call
effect trace — the step-wise simulation preserves the trace incrementally (so a run
that does I/O and then raises, or diverges while doing I/O, has its trace preserved
in the exception and divergence outcomes too), not only at normal termination.
--------------------------------------------------
to_cmm preserves observable behaviour up to ≈, modulo undefined behaviour and
resource exhaustion.
NOTES: STATUS conjectured — claimed and empirically validated (§4), not proved. The
proof obligation is a forward simulation: initial(U) ≈cfg initc(P), and every
Flambda step is matched by ≥0 Cmm steps re-establishing ≈cfg, where GC steps
(CM.Alloc.GC) and delayed-binding materialization (18, TC.Let.Subst) are Cmm
stutters that preserve ≈cfg with no Flambda step. Decomposes into: the control
lemma INV.ToCmm.Control (16 §8, target-independent), and the per-primitive
data obligation TC.Prim.Sound (18 §1, where ≈ is committed). The "modulo UB"
clause is inherited verbatim from 13; the resource-exhaustion clause is the one
outcome Cmm adds (19 §5).
```

## 3. The end-to-end story, and Invalid

Composing with Simplify's soundness gives the whole middle-end pipeline
`source Flambda → Simplify → to_cmm → Cmm`.

```rule
RULE INV.ToCmm.EndToEnd
STATUS conjectured
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
---
Let U₀ be a well-formed raw Flambda unit, U′ = Simplify(U₀) (13,
INV.Simplify.Preserves), and P = ⟦U′⟧ (INV.ToCmm.Simulates). If U₀ has no
undefined behaviour then P's observable behaviour refines U₀'s: same C-call trace
and termination outcome, with the module value related through ≈, modulo resource
exhaustion AND the known int→float32 constant-fold unsoundness (§5.1, 13 §4.7).
--------------------------------------------------
By transitivity of INV.Simplify.Preserves (equivalence on Flambda observations)
and INV.ToCmm.Simulates (≈-refinement of Flambda by Cmm).
NOTES: STATUS conjectured. This is the composed correctness of the two formalized
passes, and it INHERITS INV.Simplify.Preserves as a premise — a premise with a known
counterexample. So EndToEnd as stated is FALSE for any U₀ containing a compile-time
int→float32 conversion: Simplify's constant fold double-rounds (13 §4.7,
float32_double_round), so P computes a different float32 than U₀'s semantics — not
undefined behaviour and not resource exhaustion, just a wrong result. The gap is
ENTIRELY in Simplify: INV.ToCmm.Simulates takes U′ (Simplify's already-double-rounded
output) and translates it faithfully (§5.1: TC.Prim.NumConv emits a single-rounding
cast). Until the Simplify fold is fixed, read EndToEnd modulo §5.1. `to_cmm` requires
its input to be Simplify's output only inasmuch as it relies on Simplify having done
all unboxing (to_cmm introduces box/unbox solely as forced by kinds, 01/12); the
simulation itself holds for any well-formed optimized Flambda.
```

```rule
RULE INV.ToCmm.InvalidUnreached
STATUS conjectured
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
CODE backend/cmm.mli#Cinvalid
---
If a Flambda configuration is unreachable-when-Invalid in U′ (13: a correct
Simplify never steps a reachable config to OS.Invalid), then the ≈cfg-related Cmm
configuration never reaches CM.Invalid (Cinvalid) either.
--------------------------------------------------
Reachable Cmm states never hit Cinvalid, because TC.Invalid maps exactly Flambda
Invalid to Cmm Cinvalid and the unreachable-switch-case Cinvalid sits at
discriminants OS.Switch.Undef already calls undefined (16, TC.Switch; CM.Switch).
NOTES: STATUS conjectured. The Cmm `Cinvalid` (which lowers to caml_flambda2_invalid,
aborting) is reached only from states the Flambda machine already calls undefined,
so it does not widen the set of undefined behaviours. Inherits the discharge of
"reachable ⇏ Invalid" from 13.
```

## 4. Validation summary

The `to_cmm` chapters are validated by the same prediction-first protocol as
[`13`](13-soundness.md) §5, predicting the **`-dcmm` output** (and checking it
against `≈`) *before* reading it, plus runtime A/B checks where cheap. The
`tocmm-*` case studies in [`14-validation/`](14-validation/) cover: tag/untag
round-trip (`R.Val.Imm`, `TC.Prim.TagUntag`); block build + immutable load
(`R.Obj.Block`, `TC.Prim.MakeBlock`/`BlockLoad`); a mutable set via `caml_modify`
(`TC.Prim.BlockSet`); closure build with function-slot (`Caddv`) and value-slot
projection (`R.Obj.Closures`, `TC.Prim.ProjectFunctionSlot`/`ProjectValueSlot`);
a multi-arm switch (`CM.Switch`, `TC.Switch`) and a two-arm if-then-else;
`try/with` traps (`CM.Catch.Exn`/`CM.Raise`, `TC.LetCont.Exn`/`TC.ApplyCont.Raise`);
single-use continuation inlining vs a `Ccatch` (`TC.LetCont.Inline`/`Jump`);
local allocation with `Cendregion` reclaim (`CM.Alloc.Local`/`CM.Region.End`);
and int→float32 emitting a single cast (`TC.Prim.NumConv`).

## 5. Known discrepancies and modelling choices

Parallel to [`13`](13-soundness.md) §4. Grep here when a `to_cmm` claim seems too
clean.

1. **int→float32 double-rounding is in Simplify, not `to_cmm`.** `to_cmm` emits a
   single `Cstatic_cast (Float_of_int Float32)` for `Num_conv` into `Naked_float32`
   ([`18`](18-to-cmm-data.md) `TC.Prim.NumConv`; `cmm_helpers.ml#float32_of_int`),
   which single-rounds. The double rounding of the open soundness bug
   ([`13`](13-soundness.md) §4.7, [`float32_double_round`](14-validation/float32_double_round.md))
   is committed earlier, by Simplify's constant fold (`number_adjuncts` computing
   int → `float64` → `float32`). So `TC.Prim.NumConv` is sound and the Stage-2
   model *localizes* the bug to `S.Rewrite.Prim.ConstFold`. This is a positive
   finding, recorded so the boundary is unambiguous.

2. **Evaluation order of `Cop` arguments is a modelling choice.** Cmm fixes no
   order for `Cop` operands (Selectgen chooses; [`15`](15-cmm.md) `CM.Context`); the
   semantics picks left-to-right for determinism. `INV.ToCmm.Simulates` is
   independent of the choice because `to_cmm`'s let-substitution only reorders
   across the boundary when the effects/coeffects quadruple proves it sound
   ([`18`](18-to-cmm-data.md) `TC.Let.Subst`). This is a modelling decision, not a
   discrepancy — but a reviewer should know the order is not observable for
   well-formed output.

3. **The GC is axiomatized, not modelled.** `CM.Alloc.GC`
   ([`19`](19-cmm-memory-gc.md)) asserts the collector is `≈`-preserving rather
   than modelling collection. Its soundness rests on the machtype root discipline
   and `CM.Addr.NoSurvive`; a bug in `to_cmm`'s `Addr` handling (holding a `Cadda`
   result across an allocation) would violate the assumption and is *not* caught by
   this model — it is an unmodelled trust boundary.

4. **The effects/coeffects model is imprecise for local blocks.** An immutable load
   of a locally-allocated block is classified pure and could be reordered past an
   `End_region`; `to_cmm` works around this with a forced flush at `End_region`
   ([`18`](18-to-cmm-data.md) `TC.Let.Subst` NOTES; `to_cmm_expr.ml#let_expr0`
   hack). The formalism records the workaround rather than a more precise effect
   algebra.

5. **64-bit little-endian only.** All of `≈` ([`17`](17-representation.md)) and the
   machine ([`15`](15-cmm.md)) fix a 64-bit little-endian target — faithful to the
   code, which `fatal_error`s on 32-bit and asserts 64-bit mixed blocks. The
   endianness- and `size_float`-dependent branches the compiler genuinely has are
   noted where they occur ([`17`](17-representation.md) §6; the int32 box, mixed
   flat access) but only the little-endian side is modelled. 32-bit (`to_jsir`
   consistency; PR #685) and big-endian are out of scope
   ([`01`](01-overview.md)).

## 6. Summary of rules

`INV.ToCmm.Simulates`, `INV.ToCmm.EndToEnd`, `INV.ToCmm.InvalidUnreached`
(and the target-independent control lemma `INV.ToCmm.Control`,
[`16`](16-to-cmm-control.md) §8).
