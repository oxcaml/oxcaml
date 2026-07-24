# to_cmm soundness: the cross-language simulation

*Part of the Flambda 2 formalism; see [README.md](README.md).*

Chapters [`15`](15-cmm.md)–[`19`](19-cmm-memory-gc.md) defined the Cmm target
machine, the `to_cmm` translation `⤳` (control [`16`](16-to-cmm-control.md), data
[`18`](18-to-cmm-data.md)), the representation relation `≈`
([`17`](17-representation.md)), and the concrete memory/GC model. This chapter
states the property tying them together: **the Cmm run refines the Flambda
run, through `≈`**. Where Simplify's soundness ([`13`](13-soundness.md)) is a
*refinement within one machine* (up to immutable-identity folding, 13 §1),
`to_cmm`'s is a **refinement between two machines** carrying `≈`, established
by forward-simulation machinery plus determinacy of the target modulo its
oracles (§2 NOTES) — the standard shape of a compiler-pass correctness
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
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
CAVEAT disclosure: refinement holds only up to ≈ and 13 §1's immutable-identity folding, modulo undefined behaviour (inherited verbatim from 13) and resource exhaustion (the one outcome Cmm adds).
CAVEAT disclosure: Cmm fixes no Cop operand order (Selectgen chooses); the model's left-to-right pick is a modelling choice; INV.ToCmm.Simulates is claimed independent of it.
---
Let P = ⟦U⟧ and let the initial Cmm state of P's module initialiser be
initc(P), with initial(U) ≈cfg initc(P) (OS.Unit.Init ≈ the entry of the module
initialiser; H₀ ≈ M₀ on predefined symbols).
If the Flambda run of U does not exhibit undefined behaviour (as 13:
INV.Simplify.Preserves), then EVERY outcome of the Cmm run of P is matched by
a behaviour of the (relational) Flambda run of U — ∀ Cmm outcome ∃ Flambda
behaviour, "≈-refinement of Flambda by Cmm":
  • if the Cmm run terminates normally with memory M, some Flambda behaviour
    terminates normally with a heap H, and H(sym_mod) ≈ (module image in M) —
    ≈ read up to 13 §1's immutable-identity folding — with equal C-call
    traces (R.Observe); or
  • if the Cmm run terminates by an uncaught exception, some Flambda behaviour
    terminates by the related uncaught exception (≈ᵥ, up to the folding); or
  • if the Cmm run diverges, some Flambda behaviour diverges; or
  • if the Cmm run exhausts resources (CM.Alloc.Exhaustion), some Flambda
    behaviour diverges or terminates.
In every outcome except resource exhaustion the matching Flambda behaviour
performs the SAME C-call effect trace (event-carried heap snapshots compared up
to the folding) — the step-wise simulation preserves the trace incrementally
(so a run that does I/O and then raises, or diverges while doing I/O, has its
trace preserved in the exception and divergence outcomes too), not only at
normal termination.
--------------------------------------------------
to_cmm refines observable behaviour up to ≈ and 13 §1's folding, modulo
undefined behaviour and resource exhaustion.
NOTES: Claimed and empirically validated (§4), not proved.
REPOSED (KF-056, 2026-07-22): the top-level claim is the BACKWARD ("Cmm
refines Flambda") direction — the former forward direction (∀ Flambda
behaviour ∃ Cmm outcome) is refutable under the revised P.Binary.PhysEqual: an
abstract run may resolve phys_equal(ptr ℓ, ptr ℓ) → 0 on an ι-operand and
branch where the emitted word-equality code never goes. The
proof machinery remains a forward simulation: initial(U) ≈cfg initc(P), and every
Flambda step is matched by ≥0 Cmm steps re-establishing ≈cfg, where GC steps
(CM.Alloc.GC) and delayed-binding materialization (18, TC.Let.Subst) are Cmm
stutters that preserve ≈cfg with no Flambda step — stated over abstract runs
whose identity resolutions match the emitted code's; the backward top-level
claim follows by determinacy of the Cmm run modulo its oracles (GC relocation,
extern answers, allocation addresses): given the oracles' choices the emitted
run is unique, and the forward simulation of the code-matching abstract
behaviour supplies its witness. Decomposes into: the control
lemma INV.ToCmm.Control (16 §8, target-independent), and the per-primitive
data obligation TC.Prim.Sound (18 §1, where ≈ is committed). The "modulo UB"
clause is inherited verbatim from 13; the resource-exhaustion clause is the one
outcome Cmm adds (19 §5). FORMER KNOWN VIOLATION, resolved 2026-07-22 (13 §4
item 8): `-Oclassic` `Box_number` duplication falsified this rule against the
then-deterministic ch06 `P.Binary.PhysEqual` (§5.6). Under the revised
semantics — `P.Binary.PhysEqual` loose on immutable heap objects, observations
compared up to immutable-identity folding (13 §1) — the duplication is one of
the abstract run's derivable observations and the "modulo immutable-block
identity" reading instruction is superseded by the definitions themselves,
with the statement now re-posed as above.
```

## 3. The end-to-end story, and Invalid

Composing with Simplify's soundness gives the whole middle-end pipeline
`source Flambda → Simplify → to_cmm → Cmm`.

```rule
RULE INV.ToCmm.EndToEnd
CLAIM normative
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
CAVEAT known-false: false for any U₀ with a compile-time int→float32 conversion (float32_double_round, 13 §4.7); the gap is entirely in Simplify's constant fold — read EndToEnd modulo that family until the fix lands.
---
Let U₀ be a well-formed raw Flambda unit, U′ = Simplify(U₀) (13,
INV.Simplify.Preserves), and P = ⟦U′⟧ (INV.ToCmm.Simulates). If U₀ has no
undefined behaviour then P's observable behaviour refines U₀'s: same C-call trace
and termination outcome, with the module value related through ≈, modulo resource
exhaustion AND the known int→float32 constant-fold unsoundness (§5.1, 13 §4.7).
--------------------------------------------------
By transitivity of INV.Simplify.Preserves (refinement on Flambda observations)
and INV.ToCmm.Simulates (≈-refinement of Flambda by Cmm).
NOTES: This is the composed correctness of the two formalized
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
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#nullary_primitive
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
CODE backend/cmm.mli#Cinvalid
CAVEAT disclosure: leans on the Slot_offsets slot-liveness premise, stated but not discharged in this rule — the dead-slot arms' discharge lives only in INV.ToCmm.SlotLiveness.
---
If a Flambda configuration is unreachable-when-Invalid in U′ (13: a correct
Simplify never steps a reachable config to OS.Invalid), and Simplify/Slot_offsets
slot-liveness holds (a live Project_function_slot/Project_value_slot never targets
a slot dropped from the closure layout), then the ≈cfg-related Cmm configuration
never reaches CM.Invalid (Cinvalid) either.
--------------------------------------------------
Reachable Cmm states never hit Cinvalid. `to_cmm` emits Cmm Cinvalid from four
site-classes, each discharged: (a) the Flambda Invalid *expression* (TC.Invalid,
to_cmm_expr#invalid); (b) an unreachable switch case, sitting at discriminants
OS.Switch.Undef already calls undefined (16, TC.Switch; CM.Switch); (c) the nullary
Invalid *primitive* (to_cmm_primitive#nullary_primitive), folded into the Flambda
Invalid ⇒ Cmm Cinvalid clause and discharged by the same reachable-never-Invalid
argument as OS.Invalid (05 §60, cross-ref); and (d) the dead closure-slot
projections (to_cmm_primitive#unary_primitive, the `Dead_function_slot` /
`Dead_value_slot` arms), unreachable by slot-liveness: a live projection never
targets a slot dropped from the layout, so those arms are never taken.
NOTES: The Cmm `Cinvalid` (which lowers to caml_flambda2_invalid,
aborting) is reached only from states the Flambda machine already calls undefined,
so it does not widen the set of undefined behaviours. Inherits the discharge of
"reachable ⇏ Invalid" from 13, plus the Slot_offsets slot-liveness invariant: the
offset *layout* appears in 17/18, but the *liveness* discharge for the dead-slot
arms is stated only here.
```

### Discharging invariants: liveness, layout, totality, linkage

The following whole-unit (and cross-compilation-unit) invariants discharge the
premises the simulation of §2/§3 leans on: that lowering is total (no fatal fires
on well-formed Simplify output), that the GC scans exactly the right words, that
Reaper-consumer facts (dead slots, deleted code, symbol locality) are coherent, and
that the delayed-binding machinery preserves the effect subtrace. Several are
Reaper-consumer obligations invisible to `INV.ToCmm.Simulates` (a single-unit
statement) that manifest only when another unit links.

```rule
RULE INV.ToCmm.SlotLiveness
CLAIM normative
CODE middle_end/flambda2/flambda2.ml#build_run_result
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#finalize
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#mark_slot_as_removed
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot
CODE middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
CODE middle_end/flambda2/cmx/exported_code.ml#prepare_for_export
CODE middle_end/flambda2/types/env/cached_level.ml#remove_unused_value_slots_and_shortcut_aliases
CAVEAT disclosure: holds only under undischarged premises — (P1) survival⇒recorded, an ACCIDENTAL, non-structural alignment whose breakage means reachable Cinvalid at runtime (watch W-41, 13).
---
[MANDATORY MERGE with INV.Simplify.DeadValueSlotCoherence ([§13](13-soundness.md)):
these are the to_cmm and Simplify faces of ONE pruning event. Three DISTINCT
value-slot liveness accumulators exist, not one: (1) build_run_result's used_slots =
the FINAL term's free_names (NO.*_in_normal_projections / NO.all_*_at_normal_mode) —
feeds Slot_offsets.finalize AND the cmx type/code pruning; (2) a SEPARATE DOWNWARDS
accumulator DA.add_use_of_value_slot (recorded at the Need_meet branch of
simplify_project_value_slot) consumed by expr_builder.remove_unused_value_slots,
which prunes STATIC set-of-closures captures only, with imported slots exempt
(value_slot_is_used_or_imported); (3) the UPWARDS pass, which CANNOT record
(UA.used_value_slots is a creation-dacc snapshot — a barrier).]
Let U′ be the simplified unit handed to to_cmm and exported_offsets =
Slot_offsets.finalize on accumulator (1). This rule holds under two explicit,
undischarged-by-reading premises:
  (P1) SURVIVAL ⇒ RECORDED (Hume's lemma, ACCIDENTAL not structural): every value
       slot projected by a reachable non-phantom binding of the final term is in
       accumulator (1). This is currently a NON-LOCAL ACCIDENT because
       unboxing-materialized projections (unboxing_epa#unbox_arg) never call
       add_use_of_value_slot and the upwards pass cannot record; coherence holds only
       by the survival ⇒ extra-param-Used ⇒ in-handler-projection-folded ⇒ recorded
       alignment. If that alignment ever breaks, the failure mode is a REACHABLE
       projection getting a Dead offset entry ⇒ Cinvalid EXECUTED at runtime.
  (P2) EXPORTED-CODE SYNC: Exported_code inlinable bodies ⊆ the final term's code
       bindings (a body exported for inlining but absent from the compiled term would
       escape both the occurrence count and the code pruning).
--------------------------------------------------
Under (P1) and (P2):
(a) TOTALITY: every Project_function_slot / Project_value_slot / Set_of_closures
    to_cmm translates looks up a slot with an entry in exported_offsets — the
    "Missing offset" fatal in to_cmm_primitive is unreachable;
(b) DEAD ⟹ UNREACHABLE: a Dead_function_slot / Dead_value_slot entry (the Cinvalid
    arms of unary_primitive, site-class (d) of INV.ToCmm.InvalidUnreached) is never
    evaluated by any reachable configuration of any program linking U′.
The dead-slot Cinvalid arms and missing-offset fatals never fire, discharging the
slot-liveness premise INV.ToCmm.InvalidUnreached states but discharges nowhere.
NOTES: FIVE subsystems are pruned from accumulator (1) in the one finalize event:
offsets, final-term occurrences, exported TYPES (cached_level
remove_unused_value_slots_and_shortcut_aliases), exported CODE
(Exported_code.prepare_for_export, flambda_cmx.ml:190-194), and Cached_level
pruning. Since the totality sets are *at_normal_mode*, (a) also needs to_cmm to
reach a slot lookup only from normal-mode occurrences (INV.NameMode.Coherent + the
Phantom skip). A Dead entry arises two ways: live_slots (w projected but in no
non-phantom set — dynamically unreachable; create_*_slot fatal on an imported Dead
entry blocks any other unit) and mark_slot_as_removed (w in a set but no normal
projection — layout drops w; LATER units foreclosed by the type+code cmx scrub). P1
is the fragility Hume recommends closing with a defensive DA.add_use_of_value_slot in
unbox_arg to make coherence structural (see consolidation-code-hygiene.md); keep the
two-unit test as standing validation for P2. Composes:
INV.Simplify.DeadValueSlotCoherence, INV.ToCmm.InvalidUnreached, INV.NameMode.Coherent.
```

```rule
RULE INV.ToCmm.ClosureScanBoundary
CLAIM normative
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#update_set_for_slot
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#layout_aux
CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
CODE backend/cmm_helpers.ml#pack_closure_info
CAVEAT disclosure: rests on the subkind-soundness premise (Tagged_immediate captures really immediate at runtime), a 07/13 typing fact outside the kind grammar; a wrong bit is silent heap corruption.
---
For every closure block emitted by to_cmm (dynamic TC.Let.SetOfClosures or static
TC.Let.Static), there is a SINGLE word boundary startenv such that:
(i)   every word at offset ≥ startenv is a scanned value slot holding a Word_val;
      SUBKIND-SOUNDNESS PREMISE: a kind-VALUE slot is placed UNSCANNED (below
      startenv) when Value_slot.is_always_immediate (from the subkind
      Tagged_immediate at closure conversion), so soundness of (i)/(ii) also needs a
      Tagged_immediate-subkinded capture to really be an immediate at runtime — a
      07/13 typing fact, not derivable from the kind grammar; a wrong bit is silent
      GC corruption;
(ii)  every word at offset < startenv (code pointers, closinfo words, infix headers,
      unscanned captures) is never a value the GC may interpret;
(iii) the closinfo word of EVERY function slot f encodes startenv − off(f) with
      respect to the SAME absolute startenv;
(iv)  offset 0 is a function slot, so the closinfo word the GC unconditionally reads
      at field 1 yields that startenv.
--------------------------------------------------
The GC's scan set for a closure block is exactly its scanned value slots — the ONE
object kind where "which fields are Val" is not derivable from the header alone.
This discharges CM.Alloc.GC premise (ii) for closure blocks.
NOTES: (iii)+(iv) are per-set-local (fill_slot computes one startenv per set,
writing pack_closure_info(arity, startenv − slot_offset)). (i)+(ii) reduce to the
THREE-ZONE ORDERING (function slots < unboxed/unscanned slots < scannable value
slots), which is NOT local: offsets are assigned once per slot by a greedy global
algorithm (slots are SHARED across sets), and cross-unit slots arrive pre-assigned.
PRIMARY enforcement is the mutable-bound bookkeeping + fatal in update_set_for_slot
("slot ordering is broken") at every (slot, set) incidence including shared and
imported (the non-local leg); Layout.layout_aux's asserts are a secondary re-check
and do NOT by themselves enforce fn < unboxed. The subkind-soundness premise is
SHARED with INV.ToCmm.StaticUpdateBarrier's Immediate arm. Composes: R.Obj.Closures,
R.Val.Clos, CM.Alloc.GC, TC.Let.SetOfClosures.
```

```rule
RULE INV.ToCmm.AddrConfined
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#machtype_of_kind
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
CODE middle_end/flambda2/kinds/flambda_kind.ml#t
CODE backend/cmm.mli#machtype_component
CAVEAT disclosure: deliberately narrowed to ⤳-translated phrases of Θ(U′); generic_fns send_function Clet-binds an Addr (cache_ptr) — safe, cited as a deliberate counterexample marking the boundary.
---
[SCOPE: covers the ⤳-TRANSLATED phrases (images of Flambda expressions), NOT every
phrase in the emitted unit — the generic-functions machinery emits a genuine Addr
Clet (send_function's cache_ptr), hand-written runtime support outside ⤳.]
In the ⤳-translated Cmm, every Addr-typed value (a Cadda / field_address result) is
an anonymous subexpression created and consumed inside the Cmm image of a single
TC.Prim.* emission. No binder the TRANSLATION creates carries machtype Addr:
(i)  every declared machtype (function params, Ccatch handler params, Capply return
     types) comes from machtype_of_kind / extended_machtype_of_kind, total from
     Flambda kinds into {Val, Int, Float, Float32, Vec*} — there is NO typ_addr arm,
     because Flambda's kind grammar ([§03](03-kinds.md)) has no address kind;
(ii) every Clet emitted by flush_delayed_lets binds the Cmm image of a Flambda-kinded
     binding, whose ROOT operator yields that kind's non-Addr machtype (Cadda occurs
     only in interior operand positions; Project_function_slot emits Caddv, not Cadda).
--------------------------------------------------
No Addr value is let-bound, passed to a join point, or held live across any Calloc /
Capply / alloc=true Cextcall — CM.Addr.NoSurvive ([§19](19-cmm-memory-gc.md)) holds
for to_cmm output BY KIND PRESERVATION, with zero runtime or translation-time checks.
NOTES: The delayed-binding machinery moves/duplicates only WHOLE images of Flambda
lets; a moved emission recomputes its interior Cadda at the use site from a Val base,
which remains a GC root across any intervening allocation. So the cmm.mli Addr
contract is inherited from the SOURCE kind system: Flambda cannot express a
derived-pointer binding, so to_cmm cannot be induced to emit one. REFINEMENT: scope
to Θ(U′) to_cmm-translated phrases — backend/generic_fns send_function DOES Clet-bind
an Addr (cache_ptr), safe (no alloc across it) but shows the boundary; cite as a
deliberate counterexample. Composes: WF.* ([§03](03-kinds.md)), TC.Let.Subst,
CM.Addr.NoSurvive, CM.Alloc.GC.
```

```rule
RULE INV.ToCmm.EffectLinear
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr
CAVEAT disclosure: occurrence counts from Simplify's upwards accumulation are consumed with no revalidation (Not_found ⇒ Regular is the conservative default).
CAVEAT disclosure: the zero-occurrence Drop arm needs the same immutable-identity license as CSE sharing; the Gc.stat/minor-words counter concern is answered in the NOTES — counters are outside the observation.
---
[RESTATED: the original "an unused effectful binding is never dropped" is FALSE —
can_be_removed holds for ALL Only_generative_effects (Mutable / Immutable /
Immutable_unique), and flush_bindings deletes such bindings when unused. Only
Arbitrary_effects is unconditionally kept.]
Along every Cmm execution path of ⟦U′⟧, the sequence of ARBITRARY-EFFECTS operations
evaluated equals that of the simulated Flambda path (the C-call / write subtrace is
preserved exactly), and coeffect-only operations keep their order relative to
effects. For each Let x = p entering the delayed set D:
  Zero occurrences + Arbitrary_effects ⟹ emitted EXACTLY once at the next flush;
  Zero + generative effects (ANY mutability) / Pure / Coeffect_only ⟹ droppable —
    to_cmm's dead-read and DEAD-ALLOCATION elimination, covering even MUTABLE allocs
    when unused (sound: the identity and contents of a never-referenced block are
    unobservable — R.Observe);
  One ⟹ May/Must_inline_once (evaluated at most once);
  More_than_one + Delay ⟹ Must_inline_and_duplicate (re-evaluated per use, licensed
    by P.Effects.DelayDuplicable — duplication confined to pure/generative-IMMUTABLE;
    the DROP license is broader than the DUPLICATE license);
  More_than_one + Strict ⟹ Regular.
--------------------------------------------------
The effect subtrace of INV.ToCmm.Simulates is preserved by the delayed-binding
machinery: no Arbitrary-effects op is dropped, duplicated, or reordered past another
effect. The missing quantitative half of TC.Let.Subst, closed under the whole
flush/splice control structure (including apply_expr Case 3, where the CALL ITSELF
enters D as a delayed binding for a single-param inlined return continuation, relying
on Arbitrary_effects never being dropped).
NOTES: Non-local premise (P1): occurrence counts come from Simplify's upwards
free-names accumulation ([§09](09-simplify-structure.md)) and are consumed here with
no revalidation; a use under a loop (recursive continuation) MUST be counted
More_than_one (the classify_let_binding convention), and Not_found ⟹ Regular is the
conservative default. Dead-allocation dropping and duplication both require the
semantics to treat immutable-block identity as unobservable — two distinct licenses
(drop covers UNREACHABLE blocks of ANY mutability; duplicate needs only
immutable-block identity). This is exactly the license the revised ch06
P.Binary.PhysEqual denotation and 13 §1's immutable-identity folding now GRANT
(13 §4 item 8, adopted 2026-07-22; formerly withheld — the classic-mode
discrepancy §5.6 was the witness). The Drop arm's `Gc.stat`/allocation-counter
concern needs no `R.Observe` carve-out in the model: counters are not a modeled
observable, and event heap snapshots compare reachable structure up to folding,
which a dropped (unreachable) allocation never enters; real-world counter
divergence remains a manual-level caveat. Composes:
TC.Let.Subst, P.Effects.DelayDuplicable, R.Observe, INV.ToCmm.Simulates.
```

```rule
RULE INV.ToCmm.CallConvCoherent
CLAIM normative
CODE middle_end/flambda2/terms/code_metadata.ml#function_slot_size
CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
CODE backend/cmm_helpers.ml#curry_function_sym
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#create_function_slot
CAVEAT disclosure: leg (2) (null-code slots never applied) is UNDISCHARGED in 15-20 — its discharge lives in Reaper's deletion criterion, currently excluded from the scope ledger.
---
For every function slot f in every emitted closure block:
(1) LIVE code cid: the slot's size = 2 iff (num_params ≤ 1 ∧ not tupled) and 3
    otherwise — even when the size was fixed by a DIFFERENT compilation unit
    (imported offsets), because both sides derive from the same Code_metadata
    (function_slot_size and get_func_decl_params_arity's classification are the same
    predicate); fill_slot re-checks with fatal errors. The closinfo word's arity field
    equals the code's PARAMETER count (List.length, negated for Tupled — NOT the
    unarized component count), so caml_applyN reaches exactly code(cid) on a
    full-arity indirect call, agreeing with the direct call TC.Apply.Call emits.
    Unboxed-product generic application hides behind the CM.Apply axiom;
(2) DELETED code: fill_slot fills the slot with a NULL code pointer (0n) and a
    FABRICATED arity (1 for size 2, 2 for size 3). Safe iff no reachable configuration
    ever APPLIES a closure value pointing at f (projecting, moving within the set, and
    GC-scanning remain fine).
--------------------------------------------------
Direct and indirect application agree per slot, and null-code slots are never
applied. Leg (2) is a liveness invariant of the same family as INV.ToCmm.SlotLiveness
but for CODE deletion (Reaper / "function never called"), stated NOWHERE else in
15-20: R.Obj.Closures does not mention the Deleted arm, yet a violation jumps to
address 0.
NOTES: Non-local three ways: cross-unit (slot sizes re-derived per unit from cmx'd
Code_metadata); cross-component (the closinfo arity/startenv must match the runtime's
caml_applyN/caml_curryN/GC conventions, axiomatized as CM.Apply); cross-pass (leg
(2)'s discharge lives in Reaper's deletion criterion — a code_id is Deleted only if
never applied through any alias, in this unit or any importer — which the scope
ledger currently excludes). only_full_applications swaps the curry entry for
fail_if_called_indirectly_sym, a runtime trap witnessing that partial application of
such closures must be impossible. The obligation (leg 2) is real and UNDISCHARGED in
15-20. Composes: R.Obj.Closures, R.Val.Clos, TC.Apply.Call, CM.Apply,
INV.ToCmm.SlotLiveness.
```

```rule
RULE INV.ToCmm.StaticUpdateBarrier
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update
CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#update_field
CODE backend/cmm_helpers.ml#setfield
CAVEAT disclosure: a header bug would surface as GC-scheduling-dependent corruption invisible to byte-layout validation — no case study CAN check caml_initialize placement, a structural gap in the validation method.
---
Every deferred symbol-field update (the Or_variable "holes" of TC.Let.Static, filled
at module-init time) is emitted as follows, and this classification is COMPLETE:
  Pointer kind   → setfield … Root_initialization (the caml_initialize barrier),
                   never a plain store ("the GC must see static field updates");
  Immediate kind → plain store, sound because storing a non-pointer cannot create a
                   static→minor-heap edge — exercised only from static-closure value
                   slots and resting on the SAME is_always_immediate subkind premise
                   as INV.ToCmm.ClosureScanBoundary (i);
  Naked_* kinds  → plain store, sound because the target field lies in a region the
                   GC never scans (a mixed block's flat suffix, an unboxed/untagged
                   array payload, or a custom-block payload, per R.Header /
                   R.Obj.MixedBlock / R.Obj.Array / R.Obj.Boxed).
--------------------------------------------------
The GC's knowledge of static→heap pointers is exactly the set of updates routed
through caml_initialize; hence CM.Alloc.GC premise (ii) holds on the STATIC segment —
the static-data parallel of what INV.ToCmm.ClosureScanBoundary establishes for
closure blocks.
NOTES: Non-local: the license to SKIP the barrier for Naked_* fields is the
header-encoding rules of [§17](17-representation.md), not local to make_update. A
header bug would manifest not as a layout mismatch but as a missed remembered-set
entry — GC-scheduling-dependent corruption INVISIBLE to byte-layout validation (all
tocmm-* case studies check layout, none check barrier placement). Static blocks are
black (R.Header), so their pointer fields stay correct across GC only if (a)
link-time constants, or (b) the runtime registered the field via caml_initialize —
that dichotomy is this rule. The Immediate arm shares the subkind-immediacy premise
with INV.ToCmm.ClosureScanBoundary; a wrong bit = barrier-free pointer store into a
static closure = missed root. Composes: TC.Let.Static, R.Header, CM.Alloc.GC,
TC.Prim.BlockSet, INV.ToCmm.ClosureScanBoundary.
```

```rule
RULE INV.ToCmm.LoweringTotal
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#inline_variable
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#get_continuation
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#param_machtype_of_kinded_parameter
CAVEAT disclosure: the fexprc fuzz matrix is the standing validation for the 109 fatal sites; the fatal-localizes-upstream corollary is NARROWED to the sampled classes.
---
On any unit U′ that is (a) well-formed (WF.*, [§03](03-kinds.md)), (b) name-mode
coherent (INV.NameMode.Coherent, [§13](13-soundness.md)), (c) trap-disciplined
([§04](04-opsem.md) §1.7), and (d) equipped with total slot/code metadata
(INV.ToCmm.SlotLiveness leg (a); get_code_metadata defined on all referenced code
IDs), the translation Θ ⊢ U′ ⤳ · is TOTAL: none of the ~109 Misc.fatal_error sites
across to_cmm/*.ml is reachable. Each fatal class is discharged by a specific
upstream invariant:
  variable-not-found / cannot-rebind      ⟸ scoping + INV.NameMode.Coherent;
  continuation-not-found / inline-vs-jump ⟸ continuation scoping + Φ fixed at binder;
  Cexit/return arity & type mismatches    ⟸ WF continuation arities; kind-driven Skip_param;
  exn-handler-no-args / Push-on-return    ⟸ trap discipline + exn first-param convention;
  missing-offset / dead-slot-in-import    ⟸ INV.ToCmm.SlotLiveness;
  slot-size vs arity mismatches           ⟸ INV.ToCmm.CallConvCoherent leg (1);
  Rec_info-kind-where-value-expected      ⟸ WF kinding;
  to_cmm_env internal-machinery fatals    ⟸ [class (e)] the 17 fatal sites in
                                            to_cmm_env's own bookkeeping are discharged
                                            by to_cmm's OWN construction, NOT an
                                            upstream premise.
--------------------------------------------------
Name-mode coherence (plus WF + trap discipline + metadata totality) implies to_cmm
lowering totality: compilation of Simplify output cannot abort inside to_cmm — the
umbrella that makes INV.ToCmm.Simulates non-vacuous (a simulation presumes a
translation EXISTS).
NOTES: The discharging invariants live in four chapters (03/04/13/20) and none is
checked by to_cmm — the fatal sites ARE the checks. The fuzzing corollary is NARROWED:
a to_cmm fatal on real Simplify output localizes a bug to one of the four upstream
premises ONLY for classes (a)-(d); a class-(e) fatal indicts to_cmm's OWN machinery
(delayed-binding/symbol-init bookkeeping). Census: exactly 109 fatal sites across 9
files; sampled class discharges hold; keep the fexprc fuzz matrix as standing
validation (not all 109 proven). to_cmm_result's fatals: local-vs-global double
declaration ⟸ INV.ToCmm.SymbolLocality; module-symbol-twice ⟸ WF unit; the
data-list/set_data fatals ⟸ the archive discipline in to_cmm_static (class (e)).
Composes: INV.NameMode.Coherent, INV.ToCmm.SlotLiveness, INV.ToCmm.CallConvCoherent,
WF.*.
```

```rule
RULE INV.ToCmm.SymbolInitPlacement
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#place_symbol_inits
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_bindings
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update
CAVEAT watch(W-44): SOFT SPOT — leftover inits at a recursive-handler boundary are eprintf'd and DROPPED (to_cmm_expr.ml:983-988), guarded only by the unchecked top-level-symbol convention.
---
When a static constant's Or_variable hole is filled from a variable v whose
translation is a bare Cvar (make_update's Cvar arm), the init store is REGISTERED
against v (add_symbol_init) rather than emitted inline. Then:
(a) EXACTLY-ONCE: the store is emitted exactly once — at v's Clet during
    flush_bindings, or at the fundecl / Ccatch-handler entry when v is a parameter
    (place_symbol_inits); a binding with pending inits is exempted from removal (the
    is_empty inits conjunct in flush_bindings' removal guard), so the init is never
    dropped even if v is dead;
(b) ORDER: the store precedes, in Cmm program order, every read of that static
    field — reads within the pre-flush region are delayed bindings created after the
    Let_static (later in the order_map fold, placed inside v's Clet+init wrapper) or
    substituted into the body; control cannot leave the region without a flush;
(c) the field holds its Cdata placeholder until the store runs, and no ≈-obligation
    is placed on it before then (R.Heap on the module image is only claimed at
    OS.Unit.Final / CM.Unit.Final).
--------------------------------------------------
Deferred symbol initialization is linear and correctly ordered; TC.Let.Static's
"deferred stores sequenced before the body" survives the delayed-binding machinery
that can move v's materialization arbitrarily far from the Let_static site.
NOTES: Machinery formalized nowhere else. Readers can only SINK relative to the
binder, never hoist, and the binder's Clet is outermost via the descending-order map;
the reified-Block_load attack cannot fire (delayed bindings only sink; v is
Do_not_inline). SOFT SPOT: leftover inits at a RECURSIVE handler boundary are
eprintf'd and DROPPED (to_cmm_expr.ml:983-988), guarded only by the unchecked
convention that symbols are bound at top-level (never under a recursive continuation)
— a latent hazard if lifting ever changes. Composes: TC.Let.Static, TC.Let.Subst,
INV.ToCmm.StaticUpdateBarrier, R.Observe.
```

```rule
RULE INV.ToCmm.SymbolLocality
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol
CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol_of_code_id
CODE middle_end/flambda2/to_cmm/to_cmm_result.ml#raw_symbol
CODE middle_end/flambda2/flambda2.ml#build_run_result
CAVEAT disclosure: link soundness (reachable_names over-approximation) is invisible to the single-unit statements INV.Simplify.Preserves/INV.ToCmm.Simulates and manifests only at link time.
---
to_cmm emits a data/function symbol with Cmm.Local linkage iff it belongs to the
current unit AND is absent from reachable_names (computed alongside the cmx by
Flambda_cmx.prepare_cmx_file_contents; build_run_result). Soundness requires
reachable_names to OVER-approximate the names any other unit can reference: every
symbol/code ID occurring in the exported cmx (types, exported code bodies, offsets)
or otherwise externally addressable is in reachable_names.
  (a) COHERENCE: locality is a deterministic function of (compilation unit,
      reachable_names), so raw_symbol's "declared as both local and global" fatal is
      unreachable from the symbol / symbol_of_code_id paths;
  (b) LINK SOUNDNESS: under-approximating reachable_names yields an undefined-symbol
      link error in an importing unit; over-approximation costs only symbol-table size.
--------------------------------------------------
Symbol locality is the THIRD Reaper-consumer invariant (with dead slots and null-code
deleted slots): Reaper's reachability output feeds offsets, closure-slot filling, and
LINKAGE. Its soundness obligation is invisible to both INV.Simplify.Preserves and
INV.ToCmm.Simulates (single-unit statements) and manifests only when another unit
links.
NOTES: The consumer is the system linker; R.Observe has no notion of linkage, so
15-20 cannot even STATE this as they stand. (a) is SIMPLER than argued: Local/Global
paths never touch the string registry, so the "both local and global" fatal is
trivially unreachable from them; locality is a pure function of (unit,
reachable_names), deterministic. (b) over-approximation discharged STRUCTURALLY:
cmx-visible ⊆ reachable_names by construction (TE and EC both pruned by that same set;
offsets carry slots not symbols). -opaque collapses reachable_names to {module_symbol},
cmx=None ⇒ everything Local, sound. The classic-mode wrinkle (symbol_of_code_id calls
Compilenv.require_global for inlined bodies) shows cross-unit code identity loads
lazily. Composes: INV.ToCmm.SlotLiveness, INV.ToCmm.CallConvCoherent, TC.Let.Static.
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

6. **OPEN formalism bug: `-Oclassic` `Box_number` duplication violates
   `INV.ToCmm.Simulates` via an over-specified `PhysEqual`.** This is a genuine
   *formalism* bug — the composed rules contradict — not a compiler bug and not an
   excluded case. Chain: in classic mode a heap `Box_number` is placement `Delay`,
   `Only_generative_effects Immutable`, `No_coeffects`
   (`flambda_primitive.ml#effects_and_coeffects`; [§06](06-primitives-memory.md)
   `P.Effects.DelayDuplicable`), so `to_cmm`'s `classify_let_binding` maps a
   `Delay` binding with `More_than_one` occurrence to `Must_inline_and_duplicate`
   ([§18](18-to-cmm-data.md) `TC.Let.Subst`; `INV.ToCmm.EffectLinear`). Witness:
   ```ocaml
   let[@inline never] test x = let b = x +. 1.0 in (Sys.opaque_identity b == b, b)
   ```
   `-Oclassic -dcmm` shows the ONE Flambda `Box_number` for `b` as THREE separate
   `alloc` sites; `-Oclassic` prints **false**, default mode prints **true**. The
   abstract run ([§04](04-opsem.md)/[§06](06-primitives-memory.md)) binds `b ↦ ptr ℓ`
   once, `Opaque_identity` is the identity, and `P.Binary.PhysEqual(ptr ℓ, ptr ℓ) =
   1` ⇒ observable **true**; the Cmm run gives **false**. The two runs are not
   ≈ᵥ-related, so `INV.ToCmm.Simulates` as stated is violated by a classic-mode
   program with NO undefined behaviour. Why it is a formalism bug, not a compiler
   bug: `(==)` on immutable values is implementation-defined (the OCaml manual), and
   classic mode SKIPS Simplify (`flambda2.ml:163-71` sends raw_flambda straight to
   `to_cmm`), so the abstract semantics — which gives `P.Binary.PhysEqual` a
   *deterministic* result on `(ptr ℓ, ptr ℓ)` — over-specifies. FIX (either): (1)
   weaken `P.Binary.PhysEqual` ([§06](06-primitives-memory.md)) to a
   nondeterministic result whenever an argument points to an immutable block, so
   duplication/dropping of `Only_generative_effects Immutable` bindings is
   observationally sound — but the weakened rule must still license the
   `simplify_phys_equal` folds (`S.Rewrite.Prim.PhysEqual`) under a "same evaluation
   of the same binding" side condition; or (2) add a "modulo immutable-block
   identity" clause to `INV.ToCmm.Simulates` parallel to its resource-exhaustion
   clause. The `INV.ToCmm.EffectLinear` zero-occurrence **Drop** arm needs the same
   license, with `Gc.stat` / minor-words allocation counters as the weaker
   observable (they must be carved out of `R.Observe`). See
   [`14-validation/classic_physequal_box.md`](14-validation/classic_physequal_box.md).
   RESOLVED (2026-07-22, jointly with 13 §4 item 8): fix (1) adopted in
   strengthened form — `P.Binary.PhysEqual` is loose on immutable heap
   objects, with result 0 derivable even on `(ptr ℓ, ptr ℓ)`, licensing both
   duplication and dropping — together with fix (2)'s observational analogue
   (13 §1's immutable-identity folding + the refinement reading). The
   witness's Cmm observation **false** is now one of the abstract run's
   derivable observations, so the as-stated violation dissolves; the
   `INV.ToCmm.Simulates` header records this and the statement is to be
   re-posed against the revised 13 §1 relation. The Drop-arm counter concern
   is answered at the `INV.ToCmm.EffectLinear` NOTES (no model-side
   `R.Observe` carve-out needed; counters are not a modeled observable).

## 6. Summary of rules

`INV.ToCmm.Simulates`, `INV.ToCmm.EndToEnd`, `INV.ToCmm.InvalidUnreached`
(and the target-independent control lemma `INV.ToCmm.Control`,
[`16`](16-to-cmm-control.md) §8). Discharging invariants (§3): `INV.ToCmm.SlotLiveness`
(to_cmm side of the one pruning event with `INV.Simplify.DeadValueSlotCoherence`),
`INV.ToCmm.ClosureScanBoundary`, `INV.ToCmm.AddrConfined`, `INV.ToCmm.EffectLinear`,
`INV.ToCmm.CallConvCoherent`, `INV.ToCmm.StaticUpdateBarrier`,
`INV.ToCmm.LoweringTotal`, `INV.ToCmm.SymbolInitPlacement`,
`INV.ToCmm.SymbolLocality`.
