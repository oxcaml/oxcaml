# Validation case studies

Each file here is one case study validating the formalism by *prediction*: an
input program, a predicted Simplify (or CPS) output with the rule IDs that should
fire written down **before** reading the actual output, the actual output, and a
verdict with diagnosis. See [`../13-soundness.md` §5](../13-soundness.md) for how
this fits the soundness argument, and [`../README.md`](../README.md) ("Validation")
for the tooling.

**Protocol.** (1) Pick an input (an existing `testsuite/tests/flambda2` test or a
synthesized program); (2) predict the output and cite rules before looking;
(3) compare against the real `.reference` / `-dfexpr` dump / expect block;
(4) record MATCH / PARTIAL / MISMATCH plus a diagnosis. A PARTIAL or MISMATCH is
resolved by **fixing the formalism, never by editing the prediction** — the one
MISMATCH below drove a correction to `S.Rewrite.CSE.Eligible`.

**Verdicts:** 45 case studies — 40 MATCH, 1 PARTIAL, 4 MISMATCH (2 drove formalism
fixes — the `S.Rewrite.CSE.Eligible` correction and the `R.Obj.Closures`
scanned/unscanned split; [`float32_double_round`](float32_double_round.md) witnesses
an open **compiler soundness bug**: int→float32 constant folding double-rounds; and
[`classic_physequal_box`](classic_physequal_box.md) witnesses an open **formalism
bug**: `-Oclassic` `Box_number` duplication vs a deterministic `PhysEqual`). The 6
`to_cmm` case studies (chapters 15–20) validate the emitted
Cmm against the representation relation using real `-dcmm` output; the adversarial
sweep of chapters 15–20 (see below) found the `R.Obj.Closures`, `R.Obj.Array`, and
`CM.Addr.NoSurvive` imprecisions now corrected.

**Synthesized tests in the testsuite.** The 12 synthesized case studies (the
"Synthesized" and "Mixed blocks" tables below) have their programs checked into
the `testsuite/tests/flambda2/examples/formalism/` subdirectory, each paired
with a `.simplify.reference` capturing the Simplify fexpr dump. CI runs them
automatically (the examples directory is picked up with no test-list entry), so
the predicted behavior each case study witnesses is pinned against regressions.
Run one with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/<topic>.ml`
and update its reference with the matching `promote-one-no-rebuild`. Each case
study links to its test at the top; the inline Source sections remain as the
historical prediction record.

## From existing testsuite tests (14)

| Case study | Source test | Verdict | Primary rules |
|---|---|---|---|
| [`array_element_kind_meet`](array_element_kind_meet.md) | `flambda2/array_element_kind_meet.ml` | MATCH | `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.Let.DeadBinding` |
| [`code_size_of_boolean_not_switch`](code_size_of_boolean_not_switch.md) | `flambda2/code_size_of_boolean_not_switch.ml` | MATCH | `S.Rewrite.Switch.BooleanNot`, `S.Rewrite.Prim.UntagTag` |
| [`code_size_of_single_arg_switch`](code_size_of_single_arg_switch.md) | `flambda2/code_size_of_single_arg_switch.ml` | MATCH (descriptive) | `S.Inline.DeclDecision` (switch→lookup/affine are descriptive, no rule ID) |
| [`cse_immutable_array_load`](cse_immutable_array_load.md) | `flambda2/cse_immutable_array_load.ml` | PARTIAL | `S.Rewrite.CSE.Replace`, `S.Rewrite.CSE.Eligible` |
| [`cse_immutable_array_load_var_index`](cse_immutable_array_load_var_index.md) | `flambda2/cse_immutable_array_load_var_index.ml` | MISMATCH (fixed) | `S.Rewrite.CSE.Eligible`, `S.Rewrite.CSE.Replace` |
| [`gadt_simplified_switch`](gadt_simplified_switch.md) | `flambda2/gadt_simplified_switch.ml` (raw dump) | MATCH | `WF.Syntax.SwitchMinArms` |
| [`inlining_cost_of_primitive_on_parameters`](inlining_cost_of_primitive_on_parameters.md) | `flambda2/inlining_cost_of_primitive_on_parameters.ml` | MATCH | `S.Inline.Speculative` |
| [`issue5721`](issue5721.md) | `flambda2/issue5721.ml` | MATCH | `T.Grammar.NakedImmediate.Relational`, `S.Rewrite.CSE.Replace` |
| [`missing_code`](missing_code.md) | `flambda2/simplify/missing_code.ml` | MATCH | `S.Inline.Decision` (step 2, Missing_code) |
| [`naked_immediates_many_relations`](naked_immediates_many_relations.md) | `flambda2/simplify/naked_immediates_many_relations.ml` | MATCH | `T.Grammar.NakedImmediate.Relational`, `S.Rewrite.Apply.IndirectToDirect` |
| [`n_way_join_null`](n_way_join_null.md) | `flambda2/n_way_join_null.ml` | MATCH | `S.Rewrite.Switch.Identity`, `T.Join.Sound` |
| [`n_way_join_preserves_null`](n_way_join_preserves_null.md) | `flambda2/n_way_join_preserves_null.ml` | MATCH | `T.Join.Sound`, `T.Gamma.Value.Nullability` |
| [`removed_operations_of_switch`](removed_operations_of_switch.md) | `flambda2/removed_operations_of_switch.ml` | MATCH | `S.Inline.Speculative` |
| [`speculative_inlining_lifted_constants`](speculative_inlining_lifted_constants.md) | `flambda2/speculative_inlining_lifted_constants.ml` | MATCH | `S.Inline.Speculative` |

## Synthesized (8)

These case studies use synthesized programs; each is checked into the testsuite
(see "Synthesized tests in the testsuite" below).

| Case study | Target | Verdict | Testsuite test | Primary rules |
|---|---|---|---|---|
| [`new-01-constfold`](new-01-constfold.md) | integer constant-folding chain | MATCH | [`formalism/constfold`](../../../../../testsuite/tests/flambda2/examples/formalism/constfold.ml) | `S.Rewrite.Prim.ConstFold` |
| [`new-02-known-switch`](new-02-known-switch.md) | switch on a known constructor | MATCH | [`formalism/known_switch`](../../../../../testsuite/tests/flambda2/examples/formalism/known_switch.ml) | `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.Switch.Merge` |
| [`new-03-letcont-inline`](new-03-letcont-inline.md) | single-use continuation inlining | MATCH | [`formalism/letcont_inline`](../../../../../testsuite/tests/flambda2/examples/formalism/letcont_inline.ml) | `S.Rewrite.LetCont.Inline` |
| [`new-04-cse`](new-04-cse.md) | CSE of a repeated pure primitive | MATCH | [`formalism/cse`](../../../../../testsuite/tests/flambda2/examples/formalism/cse.ml) | `S.Rewrite.CSE.Replace`, `S.Rewrite.CSE.Extend` |
| [`new-05-inline-fold`](new-05-inline-fold.md) | inline then constant-fold | MATCH | [`formalism/inline_fold`](../../../../../testsuite/tests/flambda2/examples/formalism/inline_fold.ml) | `S.Inline.Substitute`, `S.Rewrite.Prim.ConstFold` |
| [`new-06-trap`](new-06-trap.md) | trap actions preserved around opaque call | MATCH | [`formalism/trap`](../../../../../testsuite/tests/flambda2/examples/formalism/trap.ml) | `OS.ApplyCont.TrapPush`, `OS.ApplyCont.TrapPop` |
| [`new-07-float-unbox`](new-07-float-unbox.md) | float accumulator unboxed across a loop | MATCH | [`formalism/float_unbox`](../../../../../testsuite/tests/flambda2/examples/formalism/float_unbox.ml) | `S.Unbox.ContParam.Rewrite`, `S.Unbox.Mutable.Rewrite`, `S.Unbox.Optimistic.Number` |
| [`new-08-nested-switch`](new-08-nested-switch.md) | unreachable arm pruned via refined type | MATCH | [`formalism/nested_switch`](../../../../../testsuite/tests/flambda2/examples/formalism/nested_switch.ml) | `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.Switch.BooleanNot` |

## Mixed blocks (4)

These case studies use synthesized programs; each is checked into the testsuite
(see "Synthesized tests in the testsuite" below).

| Case study | Target | Verdict | Testsuite test | Primary rules |
|---|---|---|---|---|
| [`mixed-01-record`](mixed-01-record.md) | build and read a mixed record | MATCH | [`formalism/mixed_record`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_record.ml) | `P.Variadic.MakeBlock.Mixed`, `P.Unary.BlockLoad.Mixed`, `P.MixedShape.FieldKinds`, `WF.Prim.MakeBlockMixed` |
| [`mixed-02-static`](mixed-02-static.md) | statically-allocated mixed record | MATCH | [`formalism/mixed_static`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_static.ml) | `P.Static.MixedBlock` |
| [`mixed-03-mutable-set`](mixed-03-mutable-set.md) | mutable mixed field, write then read | MATCH | [`formalism/mixed_mutable_set`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_mutable_set.ml) | `P.Binary.BlockSet.Mixed`, `P.Unary.BlockLoad.Mixed`, `P.Effects.ReadingFromBlock` |
| [`mixed-04-join`](mixed-04-join.md) | join two mixed blocks of equal shape | MATCH | [`formalism/mixed_join`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_join.ml) | `T.Meet.BlockShape` |

## Soundness bug witness (1)

| Case study | Target | Verdict | Testsuite test | Primary rules |
|---|---|---|---|---|
| [`float32_double_round`](float32_double_round.md) | int→float32 const-fold double-rounds vs backend `cvtsi2ss` | MISMATCH (compiler bug) | [`formalism/float32_double_round`](../../../../../testsuite/tests/flambda2/examples/formalism/float32_double_round.ml) | `P.Unary.NumConv`, `S.Rewrite.Prim.ConstFold`, `INV.Rewrite.Local` |

## Loopification (6)

| Case study | Target | Verdict | Primary rules |
|---|---|---|---|
| [`loopify-01-escaping-tailrec`](loopify-01-escaping-tailrec.md) | escaping purely-tailrec function survives only as wrapper | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Loopify.SelfTailCall`, `S.Rewrite.Code.RecursiveRecompute` |
| [`loopify-02-local-inlined`](loopify-02-local-inlined.md) | local loopified function inlined away entirely | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Code.RecursiveRecompute`, `S.Inline.DeclDecision` |
| [`loopify-03-not-purely-tailrec`](loopify-03-not-purely-tailrec.md) | non-tail self-recursion is not loopified | MATCH | `S.Rewrite.Loopify.Attribute`, `S.Rewrite.Loopify.AttributeUpdate` |
| [`loopify-04-loop-attr-no-tailcall`](loopify-04-loop-attr-no-tailcall.md) | `[@loop]` with no self tail call: wrapper collapses back | MATCH | `S.Rewrite.LetCont.Demote`, `S.Rewrite.LetCont.Inline` |
| [`loopify-05-dead-loop`](loopify-05-dead-loop.md) | loopified loop proven dead leaves non-recursive residue | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.LetCont.Demote` |
| [`loopify-06-mutual-and-mixed`](loopify-06-mutual-and-mixed.md) | mutual recursion untouched; `[@loop]` redirects only self tail calls | MATCH | `S.Rewrite.Loopify.Attribute`, `S.Rewrite.Loopify.SelfTailCall` |

## to_cmm lowering (6)

These validate the `to_cmm` chapters (15–20) by prediction against real `-dcmm`
output, checking the emitted Cmm against the representation relation `≈` (ch. 17)
and the translation rules (ch. 16, 18). Each program is checked into
`testsuite/tests/flambda2/examples/tocmm/`, paired with a `.compilers.reference`
capturing `-dcmm -dcanonical-ids` (canonical ids keep the reference stable); CI
runs them automatically. Run one with
`make -s test-one TEST=flambda2/examples/tocmm/<name>.ml`.

| Case study | Target | Verdict | Primary rules |
|---|---|---|---|
| [`tocmm-01-tagging-blocks`](tocmm-01-tagging-blocks.md) | tagging, block build/load, static closure layout | MATCH | `R.Val.Imm`, `R.Header`, `R.Obj.Block`, `R.Obj.Closures`, `TC.Prim.MakeBlock`, `TC.Prim.BlockLoad` |
| [`tocmm-02-switch`](tocmm-02-switch.md) | multi-arm switch → `Cswitch`, untagged scrutinee, range guard | MATCH | `TC.Switch`, `CM.Switch`, `TC.Prim.TagUntag` |
| [`tocmm-03-trywith-region`](tocmm-03-trywith-region.md) | try/with: exn handler, trap push/pop, raise, region | MATCH | `TC.LetCont.Exn`, `TC.ApplyCont.Raise`, `CM.Catch.Exn`, `CM.Exit.Trap`, `CM.Raise`, `CM.Region.Begin`, `INV.ToCmm.Control` |
| [`tocmm-04-closure-capture`](tocmm-04-closure-capture.md) | closure alloc + value-slot projection | MATCH | `R.Obj.Closures`, `TC.Prim.ProjectValueSlot`, `TC.Let.SetOfClosures` |
| [`tocmm-05-block-set-barrier`](tocmm-05-block-set-barrier.md) | immediate store vs. `caml_modify` barrier; array indexing | MATCH | `TC.Prim.BlockSet`, `TC.Prim.ArrayAccess`, `CM.Store` |
| [`tocmm-06-closure-unscanned`](tocmm-06-closure-unscanned.md) | unscanned (`int`) capture sits below `startenv` | MISMATCH (drove `R.Obj.Closures` fix) | `R.Obj.Closures`, `R.Val.Clos`, `TC.Prim.ProjectFunctionSlot` |

## Adversarial consolidation (6)

These case studies land the highest-value confirmed results of the
believers/skeptics campaign (see the verdict ledger). The first is a second
soundness-relevant MISMATCH — an over-specified formalism rule; the rest are MATCH
witnesses of whole-body/whole-unit invariants. Three carry a checked-in testsuite
program under `testsuite/tests/flambda2/examples/formalism/` (empty `.reference`
until promoted); the other three are exercised through `meet_test.ml` or
`-dcmm`/`-g`-differential runs rather than the fexpr-dump harness.

| Case study | Target | Verdict | Testsuite test | Primary rules |
|---|---|---|---|---|
| [`classic_physequal_box`](classic_physequal_box.md) | `-Oclassic` `Box_number` duplication vs deterministic `PhysEqual` | MISMATCH (formalism bug) | — (`-dcmm`/runtime; classic skips Simplify) | `INV.ToCmm.Simulates`, `P.Effects.DelayDuplicable`, `INV.ToCmm.EffectLinear`, `P.Binary.PhysEqual` |
| [`region_pair_atomic`](region_pair_atomic.md) | Begin_region + all End_regions live/die atomically (2 exits) | MATCH | [`formalism/region_pair_atomic`](../../../../../testsuite/tests/flambda2/examples/formalism/region_pair_atomic.ml) | `INV.Simplify.RegionPairAtomic`, `S.Rewrite.Let.DeadRegion`, `INV.Simplify.EffectfulDeletionInventory` |
| [`dead_value_slot_coherence`](dead_value_slot_coherence.md) | captured var dropped when its slot is never projected | MATCH | [`formalism/dead_value_slot_coherence`](../../../../../testsuite/tests/flambda2/examples/formalism/dead_value_slot_coherence.ml) | `INV.Simplify.DeadValueSlotCoherence`, `S.Rewrite.Prim.Projection`, `INV.ToCmm.SlotLiveness` |
| [`exn_demotion`](exn_demotion.md) | all-or-nothing exception-handler demotion | MATCH | [`formalism/exn_demotion`](../../../../../testsuite/tests/flambda2/examples/formalism/exn_demotion.ml) | `S.Rewrite.LetCont.DemoteExn`, `INV.Simplify.EffectfulDeletionInventory`, `S.Struct.Flow.ExnFirstParam` |
| [`const_persistence_join_points`](const_persistence_join_points.md) | constant knowledge survives merges only when `join_points` on | MATCH | — (`meet_test.ml` witnesses) | `T.Join.ConstAgreement`, `T.Env.ConstCanonicalPersists`, `S.Struct.JoinParams.AnalysisExtraParams` |
| [`loopify_trap_neutral`](loopify_trap_neutral.md) | loopified self-loops trap-depth-neutral (Pop, never Push) | MATCH | — (`.fl`/`-dcmm`/`-g` differential) | `INV.Loopify.TrapNeutral`, `S.Rewrite.Loopify.SelfTailCall`, `S.Rewrite.LetCont.DemoteExn` |
