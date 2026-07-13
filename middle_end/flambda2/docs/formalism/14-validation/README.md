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

**Verdicts:** 32 case studies — 30 MATCH, 1 PARTIAL, 1 MISMATCH (now fixed).

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

| Case study | Target | Verdict | Primary rules |
|---|---|---|---|
| [`new-01-constfold`](new-01-constfold.md) | integer constant-folding chain | MATCH | `S.Rewrite.Prim.ConstFold` |
| [`new-02-known-switch`](new-02-known-switch.md) | switch on a known constructor | MATCH | `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.Switch.Merge` |
| [`new-03-letcont-inline`](new-03-letcont-inline.md) | single-use continuation inlining | MATCH | `S.Rewrite.LetCont.Inline` |
| [`new-04-cse`](new-04-cse.md) | CSE of a repeated pure primitive | MATCH | `S.Rewrite.CSE.Replace`, `S.Rewrite.CSE.Extend` |
| [`new-05-inline-fold`](new-05-inline-fold.md) | inline then constant-fold | MATCH | `S.Inline.Substitute`, `S.Rewrite.Prim.ConstFold` |
| [`new-06-trap`](new-06-trap.md) | trap actions preserved around opaque call | MATCH | `OS.ApplyCont.TrapPush`, `OS.ApplyCont.TrapPop` |
| [`new-07-float-unbox`](new-07-float-unbox.md) | float accumulator unboxed across a loop | MATCH | `S.Unbox.ContParam.Rewrite`, `S.Unbox.Mutable.Rewrite`, `S.Unbox.Optimistic.Number` |
| [`new-08-nested-switch`](new-08-nested-switch.md) | unreachable arm pruned via refined type | MATCH | `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.Switch.BooleanNot` |

## Mixed blocks (4)

| Case study | Target | Verdict | Primary rules |
|---|---|---|---|
| [`mixed-01-record`](mixed-01-record.md) | build and read a mixed record | MATCH | `P.Variadic.MakeBlock.Mixed`, `P.Unary.BlockLoad.Mixed`, `P.MixedShape.FieldKinds`, `WF.Prim.MakeBlockMixed` |
| [`mixed-02-static`](mixed-02-static.md) | statically-allocated mixed record | MATCH | `P.Static.MixedBlock` |
| [`mixed-03-mutable-set`](mixed-03-mutable-set.md) | mutable mixed field, write then read | MATCH | `P.Binary.BlockSet.Mixed`, `P.Unary.BlockLoad.Mixed`, `P.Effects.ReadingFromBlock` |
| [`mixed-04-join`](mixed-04-join.md) | join two mixed blocks of equal shape | MATCH | `T.Meet.BlockShape` |

## Loopification (6)

| Case study | Target | Verdict | Primary rules |
|---|---|---|---|
| [`loopify-01-escaping-tailrec`](loopify-01-escaping-tailrec.md) | escaping purely-tailrec function survives only as wrapper | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Loopify.SelfTailCall`, `S.Rewrite.Code.RecursiveRecompute` |
| [`loopify-02-local-inlined`](loopify-02-local-inlined.md) | local loopified function inlined away entirely | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Code.RecursiveRecompute`, `S.Inline.DeclDecision` |
| [`loopify-03-not-purely-tailrec`](loopify-03-not-purely-tailrec.md) | non-tail self-recursion is not loopified | MATCH | `S.Rewrite.Loopify.Attribute`, `S.Rewrite.Loopify.AttributeUpdate` |
| [`loopify-04-loop-attr-no-tailcall`](loopify-04-loop-attr-no-tailcall.md) | `[@loop]` with no self tail call: wrapper collapses back | MATCH | `S.Rewrite.LetCont.Demote`, `S.Rewrite.LetCont.Inline` |
| [`loopify-05-dead-loop`](loopify-05-dead-loop.md) | loopified loop proven dead leaves non-recursive residue | MATCH | `S.Rewrite.Loopify.Body`, `S.Rewrite.Switch.ArmPrune`, `S.Rewrite.LetCont.Demote` |
| [`loopify-06-mutual-and-mixed`](loopify-06-mutual-and-mixed.md) | mutual recursion untouched; `[@loop]` redirects only self tail calls | MATCH | `S.Rewrite.Loopify.Attribute`, `S.Rewrite.Loopify.SelfTailCall` |
