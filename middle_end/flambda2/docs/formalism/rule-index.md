# Rule index

Machine-greppable index of every formal rule in the Flambda 2 formalism
(chapters 02-20; chapter 01 has none). Each
row corresponds to one ```` ```rule ```` block in a chapter. See
[`README.md`](README.md) ("Rule blocks" and "Rule ID namespaces") for the
block format and the meaning of each namespace prefix.

**_Agents: weigh a rule by its ROLE × derived grade × CAVEAT lines, never by role alone — see [§ How agents should use this index](#how-agents-should-use-this-index)._**

## Solidity scoreboard

_Every number below is derived by the generator at each run — never
hand-maintained (record 76)._

**453 rules** across chapters 02-20, validated against
**46 case studies** in `14-validation/`.

### Grades

```
A          38  █████
B          18  ██
C         300  ████████████████████████████████████████
D          97  ████████████
DISPUTED    0
```

- **By role:** specification 365 (spec), implementation 75 (impl), definition 13 (def)
- **DISPUTED:** none. DISPUTED is the live under-investigation
  state — an open FIDELITY.md finding naming a rule suspends its
  letter; zero means no rule is currently under an open finding.

### Trust signals

- **Caveats by kind** (urgency order): known-false 4, compiler-bug 2, pending-upstream 3, watch 9, disclosure 104
- **Validation events:** 58 `VERIFIED` lines (58 commit-keyed) across 46 rules; 10 `CHECKED`.
- **Watched provisos** (pending-upstream; the reverse-proviso check
  watches these keys for their lift):
  - `P.Binary.StringOrBigstringLoad` — `flambda2-string-load-fold` @ 9712d270eb
  - `S.Rewrite.Prim.ConstFold` — `fix-float32-double-rounding` @ 26daff7cee
  - `S.Rewrite.Share.StaticDynamicSplit` — `flambda2-string-load-fold` @ 9712d270eb
- **HYBRID rules lacking clause inventories (warned, by design):** 4

### By chapter

- 02-syntax.md — 13 rules · A 1 · B 2 · C 9 · D 1 · 4 caveats
- 03-kinds.md — 27 rules · A 1 · C 24 · D 2 · 4 caveats
- 04-opsem.md — 29 rules · A 2 · C 26 · D 1 · 3 caveats
- 05-primitives-scalar.md — 31 rules · B 1 · C 29 · D 1 · 6 caveats
- 06-primitives-memory.md — 69 rules · A 8 · B 1 · C 52 · D 8 · 8 caveats
- 07-types-domain.md — 30 rules · A 2 · B 3 · C 23 · D 2 · 8 caveats
- 08-meet-join.md — 30 rules · A 2 · B 4 · C 15 · D 9 · 14 caveats
- 09-simplify-structure.md — 37 rules · B 1 · C 5 · D 31 · 13 caveats
- 10-simplify-rewrites.md — 54 rules · A 17 · B 1 · C 27 · D 9 · 7 caveats
- 11-inlining.md — 10 rules · A 1 · B 3 · C 3 · D 3 · 1 caveat
- 12-unboxing.md — 18 rules · A 1 · B 2 · C 2 · D 13 · 3 caveats
- 13-soundness.md — 12 rules · C 4 · D 8 · 5 caveats
- 15-cmm.md — 21 rules · C 21 · 8 caveats
- 16-to-cmm-control.md — 16 rules · C 16 · 4 caveats
- 17-representation.md — 16 rules · A 1 · C 15 · 9 caveats
- 18-to-cmm-data.md — 21 rules · A 2 · C 19 · 5 caveats
- 19-cmm-memory-gc.md — 7 rules · C 7 · 6 caveats
- 20-to-cmm-soundness.md — 12 rules · C 3 · D 9 · 14 caveats

## How agents should use this index

- To answer a question or synthesize a test, grep here for candidate rules,
  then read the owning chapter section. Weigh a rule by its ROLE plus
  its caveats and derived evidence; `implementation` rules are
  current-behaviour documentation and may drift with the code.
- When code under `middle_end/flambda2/` changes, grep the **Code anchors**
  column for the changed file, then follow the sync protocol in `README.md`.

## Regenerating this index

This file is generated. Do not edit it by hand — from the repository root,
run:

```
python3 middle_end/flambda2/docs/formalism/tools/regen_rule_index.py
```

(The `make regen-flambda2-formalism-rule-index` target and the Dune alias it
wraps are broken while the dune sandbox excludes `rocq/`, which the generator
must read; use the direct invocation.)

The underlying script scans each `NN-*.md` for fenced
`rule` blocks, reads the `RULE` / `ROLE` / `CODE` / `VERIFIED` / `CAVEAT` header
lines, buckets by namespace prefix (the text before the first `.` in the
ID), runs the consistency checks below, rewrites this file, and refreshes
the rule/case-study counts in [`README.md`](README.md). It exits non-zero
if any check fails (duplicate IDs, rules with no anchor, or unresolvable
anchors), recording the failures in the section below either way.

## Columns

- **Rule ID** — the rule's stable ID (`RULE` header). Globally unique; never
  renumbered or reused.
- **Role** — the rule's direction of authority (record 77; shown
  abbreviated): `spec` = specification (the code answers to the rule;
  divergence is a bug in code or spec), `impl` = implementation (the
  rule answers to the code; documents the spec-free surface and may
  drift), `def` = definition (formalism-side apparatus the other roles
  are judged against). Fences carry the full words. Evidence and
  grades are DERIVED by the tooling, never declared (record 76).
- **Grade** — the derived solidity grade (record 76), monotone on the two
  evidence axes: A = validated × mechanized (no demoting flag), B =
  validated alone or code-read × mechanized, C = one axis, D = neither;
  DISPUTED (with the open FIDELITY.md finding ids) suspends the letter.
  Flags in [brackets]: false-as-stated and pending-upstream demote A;
  compiler-bug, hybrid, and stale display without demoting. A
  (proved-in-model) badge marks a Qed — never a grade input. Derived by
  `tools/solidity_join.py` from the fence events, the .v artifact kind,
  and FIDELITY.md's stamps table and `RULES:` headers.
- **Artifact** — the rule's Rocq artifact kind in rocq/theories/ (defining
  clause, theorem-admitted/qed, documented-anchor, demoted-claim), with
  (hybrid) and [stale] markers where applicable.
- **Chapter** — the chapter file containing the rule block.
- **Code anchors** — every `CODE` anchor (`path#name`), as written in the
  block. Paths are relative to `middle_end/flambda2/` unless they already begin
  with `middle_end/`. A leading component may need `middle_end/flambda2/`
  prepended to resolve from the repository root.
- **Verified** — validation case study/studies (`VERIFIED` header), or `—` if
  none yet.

## WF — Kinding and well-formedness (ch. 03)

40 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `WF.Syntax.Anf` | spec | B (proved-in-model) | 02-syntax.md | theorem-qed | `middle_end/flambda2/terms/flambda.mli#expr_descr` | — |
| `WF.Syntax.LetKindUniform` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Syntax.SingletonNotSetOfClosures` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Syntax.SwitchScrutinee` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/terms/switch_expr.mli#t`<br>`middle_end/flambda2/terms/switch_expr.mli#create` | — |
| `WF.Syntax.SwitchMinArms` | spec | A | 02-syntax.md | defining | `middle_end/flambda2/terms/switch_expr.mli#t`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#close_switch`<br>`middle_end/flambda2/simplify/expr_builder.ml#create_switch`<br>`middle_end/flambda2/terms/flambda.mli#Invalid.t` | 14-validation/gadt_simplified_switch.md |
| `WF.Syntax.ExnHandlerNonRecursive` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/terms/flambda.mli#Continuation_handler`<br>`middle_end/flambda2/terms/flambda.mli#Let_cont_expr` | — |
| `WF.Syntax.ExnHandlerFirstParamBucket` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/terms/exn_continuation.mli#arity`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `WF.Syntax.EffectCalleeNone` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/terms/call_kind.mli#Effect`<br>`middle_end/flambda2/terms/apply_expr.mli#create` | — |
| `WF.Syntax.ContSecondClass` | spec | B (proved-in-model) | 02-syntax.md | theorem-qed | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr`<br>`middle_end/flambda2/terms/trap_action.mli#t`<br>`middle_end/flambda2/terms/flambda.mli#Function_params_and_body` | — |
| `WF.Syntax.NonRecOccursPositive` | spec | D | 02-syntax.md | documented-anchor | `middle_end/flambda2/terms/flambda.mli#let_cont_expr`<br>`middle_end/flambda2/terms/flambda.ml#Let_cont_expr.create_non_recursive0` | — |
| `WF.Syntax.StaticRecThroughCode` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/bound_identifiers/bound_static.mli#create` | — |
| `WF.Syntax.ImmutableArrayNonEmpty` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/terms/static_const.mli#t` | — |
| `WF.Syntax.NameModeInTerms` | spec | C | 02-syntax.md | defining | `middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms` | — |
| `WF.Subkind.Erasable` | spec | C | 03-kinds.md | defining | `kinds/flambda_kind.ml#With_subkind.erase_subkind`<br>`kinds/flambda_kind.ml#With_subkind.equal_ignoring_subkind`<br>`kinds/flambda_kind.ml#With_subkind.has_useful_subkind_info` | — |
| `WF.Subkind.Scannable` | impl | D | 03-kinds.md | documented-anchor | `kinds/flambda_kind.ml#With_subkind.must_be_gc_scannable` | — |
| `WF.Arity.Unarize` | spec | C | 03-kinds.md | defining | `kinds/flambda_arity.ml#unarize`<br>`kinds/flambda_arity.ml#Component.unarize` | — |
| `WF.Arity.ApplyFlavours` | spec | C | 03-kinds.md | defining | `terms/apply_expr.mli#create`<br>`terms/apply_expr.mli#args_arity`<br>`terms/apply_expr.mli#return_arity` | — |
| `WF.Kind.Var` | spec | C | 03-kinds.md | defining | `term_basics/simple.ml#kind`<br>`identifiers/int_ids.ml#Variable.kind` | — |
| `WF.Kind.Symbol` | spec | C | 03-kinds.md | defining | `term_basics/simple.ml#kind` | — |
| `WF.Kind.Const` | spec | C | 03-kinds.md | defining | `identifiers/reg_width_const.ml#kind`<br>`identifiers/int_ids.mli#Const.Descr` | — |
| `WF.Kind.Coerce` | spec | C | 03-kinds.md | defining | `identifiers/coercion0.mli#S`<br>`term_basics/simple.ml#kind` | — |
| `WF.Let.Singleton` | spec | C | 03-kinds.md | defining | `terms/flambda.ml#Named.kind`<br>`terms/flambda.mli#Named.kind`<br>`bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Let.SetOfClosures` | spec | C | 03-kinds.md | defining | `terms/flambda.mli#Named`<br>`bound_identifiers/bound_pattern.mli#t`<br>`simplify/simplify_set_of_closures.ml` | — |
| `WF.Let.Static` | spec | C | 03-kinds.md | defining | `terms/flambda.mli#Named`<br>`bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Named.Prim` | spec | C | 03-kinds.md | defining | `terms/flambda.ml#Named.kind`<br>`terms/flambda_primitive.mli#result_kind`<br>`terms/flambda_primitive.ml#result_kind'` | — |
| `WF.Named.Simple` | spec | C | 03-kinds.md | defining | `terms/flambda.ml#Named.kind` | — |
| `WF.Named.RecInfo` | spec | C | 03-kinds.md | defining | `terms/flambda.ml#Named.kind` | — |
| `WF.Prim.ArgKinds` | spec | C | 03-kinds.md | defining | `terms/flambda_primitive.mli#arg_kind_of_unary_primitive`<br>`terms/flambda_primitive.mli#args_kind_of_binary_primitive`<br>`terms/flambda_primitive.mli#args_kind_of_variadic_primitive` | — |
| `WF.Prim.MakeBlockMixed` | spec | A | 03-kinds.md | defining | `terms/flambda_primitive.ml#args_kind_of_variadic_primitive`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`kinds/flambda_kind.mli#Mixed_block_shape.field_kinds` | 14-validation/mixed-01-record.md |
| `WF.Switch.Scrutinee` | spec | C | 03-kinds.md | defining | `terms/switch_expr.ml#t`<br>`simplify/simplify_switch_expr.ml#simplify_arm` | — |
| `WF.Switch.NonEmpty` | spec | C | 03-kinds.md | defining | `simplify/simplify_switch_expr.ml`<br>`simplify/expr_builder.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.ApplyCont.Arity` | spec | C | 03-kinds.md | defining | `terms/apply_cont_expr.mli#create`<br>`bound_identifiers/bound_parameters.mli#arity`<br>`bound_identifiers/bound_parameter.mli#kind`<br>`simplify/env/continuation_uses.ml#add_use`<br>`simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation` | — |
| `WF.Apply.ArgKinds` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml#simplify_apply_shared`<br>`terms/apply_expr.mli#args_arity` | — |
| `WF.Apply.DirectArity` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml#arity_mismatch`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.DirectResultArity` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.Over` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml`<br>`simplify/simplify_common.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.Partial` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Region.Var` | spec | C | 03-kinds.md | defining | `from_lambda/closure_conversion.ml`<br>`terms/flambda_primitive.ml#result_kind_of_variadic_primitive` | — |
| `WF.RecInfo.MyDepth` | spec | C | 03-kinds.md | defining | `simplify/simplify_apply_expr.ml`<br>`simplify/simplify_set_of_closures.ml`<br>`terms/flambda.mli#Function_params_and_body.create` | — |
| `WF.Check.Gated` | impl | D | 03-kinds.md | documented-anchor | `ui/flambda_features.ml#kind_checks`<br>`driver/oxcaml_flags.ml`<br>`driver/oxcaml_args.ml`<br>`simplify/simplify_apply_expr.ml#simplify_apply_shared` | — |

## OS — Operational semantics (ch. 04)

29 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `OS.Simple.Eval` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/term_basics/simple.mli#t`<br>`middle_end/flambda2/term_basics/coercion.mli#t` | — |
| `OS.Let.Simple` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Let_expr`<br>`middle_end/flambda2/terms/flambda.mli#Named` | — |
| `OS.Let.Prim.Pure` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects`<br>`middle_end/flambda2/terms/effects.mli#t` | — |
| `OS.Let.Prim.Effect` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `OS.Let.SetOfClosures` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/set_of_closures.mli#create`<br>`middle_end/flambda2/bound_identifiers/alloc_mode.mli#For_allocations` | — |
| `OS.Let.Static` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/static_const.mli#t`<br>`middle_end/flambda2/terms/flambda.mli#Static_const_or_code`<br>`middle_end/flambda2/bound_identifiers/bound_static.mli#Pattern` | — |
| `OS.Let.RecInfo` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/term_basics/rec_info_expr.mli#t` | — |
| `OS.LetCont.NonRec` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr`<br>`middle_end/flambda2/terms/flambda.mli#Non_recursive_let_cont_handler`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `OS.LetCont.Rec` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr`<br>`middle_end/flambda2/terms/flambda.mli#Recursive_let_cont_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation`<br>`middle_end/flambda2/simplify/apply_cont_rewrite.ml#get_used_params` | — |
| `OS.ApplyCont` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/apply_cont_expr.mli#create`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `OS.ApplyCont.Return` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/apply_expr.mli#Result_continuation`<br>`middle_end/flambda2/terms/flambda.mli#Function_params_and_body` | — |
| `OS.ApplyCont.ExnBoundary` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/exn_continuation.mli#t`<br>`middle_end/flambda2/terms/exn_continuation.mli#extra_args` | — |
| `OS.ApplyCont.TrapPush` | spec | A | 04-opsem.md | defining | `middle_end/flambda2/terms/trap_action.mli#t`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps` | 14-validation/new-06-trap.md |
| `OS.ApplyCont.TrapPop` | spec | A | 04-opsem.md | defining | `middle_end/flambda2/terms/trap_action.mli#t`<br>`middle_end/flambda2/terms/apply_cont_expr.ml#is_raise` | 14-validation/new-06-trap.md |
| `OS.ApplyCont.Raise` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/apply_cont_expr.ml#is_raise`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#close_raise0`<br>`middle_end/flambda2/terms/trap_action.mli#Raise_kind` | — |
| `OS.Switch` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/switch_expr.mli#create`<br>`middle_end/flambda2/terms/switch_expr.mli#arms` | — |
| `OS.Switch.Undef` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/switch_expr.mli#create`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch` | — |
| `OS.Apply.Direct` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/apply_expr.mli#create`<br>`middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/flambda.mli#Function_params_and_body`<br>`middle_end/flambda2/terms/code_metadata.mli#params_arity` | — |
| `OS.Apply.IndirectUnknownArity.Full` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/code_metadata.mli#params_arity` | — |
| `OS.Apply.IndirectUnknownArity.Partial` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/code_metadata.mli#first_complex_local_param`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application` | — |
| `OS.Apply.IndirectUnknownArity.Over` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/apply_expr.mli#return_arity`<br>`middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application` | — |
| `OS.Apply.IndirectKnownArity` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#Function_call` | — |
| `OS.Apply.CCall` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#t`<br>`middle_end/flambda2/terms/apply_expr.mli#create` | — |
| `OS.Apply.Method` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/call_kind.mli#Method_kind` | — |
| `OS.Apply.Effect` | impl | D | 04-opsem.md | documented-anchor | `middle_end/flambda2/terms/call_kind.mli#Effect` | — |
| `OS.Apply.NeverReturns` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/apply_expr.mli#Result_continuation`<br>`middle_end/flambda2/terms/apply_expr.mli#returns` | — |
| `OS.Invalid` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda.mli#expr_descr`<br>`middle_end/flambda2/terms/flambda.mli#Invalid` | — |
| `OS.Unit.Init` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda_unit.mli#create` | — |
| `OS.Unit.Final` | spec | C | 04-opsem.md | defining | `middle_end/flambda2/terms/flambda_unit.mli#return_continuation`<br>`middle_end/flambda2/terms/flambda_unit.mli#module_symbol` | — |

## P — Primitive denotations (ch. 05-06)

100 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `P.Contract.NoRaiseNoControl` | spec | B (proved-in-model) | 05-primitives-scalar.md | theorem-qed | `middle_end/flambda2/terms/flambda_primitive.mli#t` | — |
| `P.Unary.IntArith.SwapByteEndianness` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Unary_int_arith`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#For_int16s`<br>`middle_end/flambda2/numbers/target_ocaml_int.ml#get_least_significant_16_bits_then_byte_swap` | — |
| `P.Unary.FloatArith` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op`<br>`middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen` | — |
| `P.Unary.NumConv` | spec | C [compiler-bug] | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_int_conv`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Unary.NumConv.FloatToInt.OutOfRange` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/numbers/target_ocaml_int.ml#of_float`<br>`middle_end/flambda2/numbers/numeric_types.ml#Short_int`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Unary.NumConv.Int32ToInt64.SignExtend` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/number_adjuncts.ml#For_int32s`<br>`middle_end/flambda2/z3/sign_extension.py` | — |
| `P.Unary.BooleanNot` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_boolean_not` | — |
| `P.Unary.TagImmediate` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_tag_immediate` | — |
| `P.Unary.UntagImmediate` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate` | — |
| `P.Unary.Reinterpret64.Int64AsFloat64` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_unboxed_float64`<br>`middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen` | — |
| `P.Unary.Reinterpret64.Float64AsInt64` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_float64_as_unboxed_int64` | — |
| `P.Unary.Reinterpret64.Int64AsTaggedInt63` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_tagged_int63` | — |
| `P.Unary.Reinterpret64.TaggedInt63AsInt64` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_tagged_int63_as_unboxed_int64` | — |
| `P.Unary.ReinterpretBoxedVector` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#convert_lprim`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_reinterpret_boxed_vector`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive` | — |
| `P.Unary.BoxNumber` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_box_number`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.UnboxNumber` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Binary.IntArith.Total` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Binary.IntArith.DivMod` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/numbers/target_ocaml_int.ml#div` | — |
| `P.Binary.IntArith.DivModByZero` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_zero_division` | — |
| `P.Binary.IntArith.DivMinIntByMinusOne` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/numbers/target_ocaml_int.ml#div`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#For_int64s`<br>`backend/cmm_helpers.ml#div_int`<br>`backend/cmm_helpers.ml#make_safe_divmod` | — |
| `P.Binary.IntShift` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#with_shift` | — |
| `P.Binary.IntShift.OutOfRange.FolderPicksZero` | impl | D | 05-primitives-scalar.md | documented-anchor | `middle_end/flambda2/simplify/number_adjuncts.ml#with_shift` | — |
| `P.Binary.IntShift.ByZero` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift` | — |
| `P.Binary.IntComp.Bool` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#compare_unsigned_generic` | — |
| `P.Binary.IntComp.CompareFunction` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp` | — |
| `P.Binary.FloatArith` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Binary.FloatComp.Bool` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Binary.FloatComp.CompareFunction` | spec | C | 05-primitives-scalar.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Effects.PureScalars` | spec | C | 05-primitives-scalar.md | theorem-admitted | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive` | — |
| `P.Effects.FloatRoundingMode` | spec | C | 05-primitives-scalar.md | theorem-admitted | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive` | — |
| `P.Effects.BoxNumber` | spec | C | 05-primitives-scalar.md | theorem-admitted | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.MixedShape.FieldKinds` | spec | A (proved-in-model) | 06-primitives-memory.md | theorem-qed | `middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.from_prefix_size_and_suffix_elements`<br>`middle_end/flambda2/kinds/flambda_kind.ml#Scannable_block_shape.element_kind`<br>`middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape.field_kinds` | 14-validation/mixed-01-record.md |
| `P.MixedShape.Offset` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words`<br>`middle_end/flambda2/kinds/flambda_kind.ml#Flat_suffix_element0.size_in_words`<br>`middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.size_in_words` | — |
| `P.Effects.Classification` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/effects_and_coeffects.ml`<br>`middle_end/flambda2/terms/effects.ml`<br>`middle_end/flambda2/terms/coeffects.ml` | — |
| `P.Effects.NoEffects` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.OnlyGenerative` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.Arbitrary` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.Coeffects` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/coeffects.ml#t` | — |
| `P.Effects.Placement` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/placement.ml#t` | — |
| `P.Effects.DelayDuplicable` | spec | C | 06-primitives-memory.md | theorem-admitted | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects`<br>`middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding` | — |
| `P.Effects.Validity` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/validity.ml#t` | — |
| `P.Effects.Pure` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/effects_and_coeffects.ml#pure`<br>`middle_end/flambda2/terms/effects_and_coeffects.ml#pure_can_be_duplicated` | — |
| `P.Effects.ReadingFromBlock` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_block` | — |
| `P.Effects.Writing` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_block` | — |
| `P.Effects.Allocation` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_variadic_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#coeffects_of_mode` | — |
| `P.Variadic.MakeBlock.Values` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind`<br>`middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_block` | — |
| `P.Variadic.MakeBlock.NakedFloats` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind` | — |
| `P.Variadic.MakeBlock.Mixed` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind`<br>`middle_end/flambda2/terms/flambda_primitive.ml#args_kind_of_variadic_primitive`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape` | 14-validation/mixed-01-record.md |
| `P.Variadic.MakeArray` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive`<br>`middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_array` | — |
| `P.Static.MixedBlock` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/static_const.ml#t`<br>`middle_end/flambda2/terms/static_const.ml#block_field_kind`<br>`middle_end/flambda2/to_cmm/to_cmm_static.ml#static_const0` | 14-validation/mixed-02-static.md |
| `P.Variadic.BeginRegion` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region` | — |
| `P.Variadic.BeginTryRegion` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive` | — |
| `P.Unary.BlockLoad` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_mutable_block_load` | — |
| `P.Unary.BlockLoad.NakedFloats` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind` | — |
| `P.Unary.BlockLoad.Mixed` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind`<br>`middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.element_kind_for_load`<br>`middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape` | 14-validation/mixed-01-record.md |
| `P.Binary.BlockSet` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set` | — |
| `P.Binary.BlockSet.NakedFloats` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set` | — |
| `P.Binary.BlockSet.Mixed` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind`<br>`middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set` | 14-validation/mixed-03-mutable-set.md |
| `P.Unary.DuplicateBlock` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.DuplicateBlock.Mixed` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#Duplicate_block_kind`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.DuplicateArray` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.IsInt.Immediate` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int` | — |
| `P.Unary.IsInt.Pointer` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int` | — |
| `P.Unary.IsNull` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.GetTag` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag` | — |
| `P.Unary.GetHeader` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.ArrayLength` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length` | — |
| `P.Unary.StringLength` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length` | — |
| `P.Unary.BigarrayLength` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | 14-validation/bigarray_access.md |
| `P.Unary.ProjectFunctionSlot` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot` | — |
| `P.Unary.ProjectValueSlot` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot` | — |
| `P.Unary.IsBoxedFloat` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.EndRegion` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.EndTryRegion` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.ObjDup` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.OpaqueIdentity` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.MakeLazy` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.IntAsPointer` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Binary.ArrayLoad` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load` | — |
| `P.Binary.ArrayLoad.Vector` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load` | — |
| `P.Binary.StringOrBigstringLoad` | spec | C [false-as-stated, pending-upstream] | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load` | — |
| `P.Binary.PhysEqual` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal` | — |
| `P.Bigarray.Indexing` | impl | B | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#bigarray_indexing`<br>`middle_end/flambda2/from_lambda/lambda_to_lambda_transforms.ml#transform_primitive` | 14-validation/bigarray_access.md |
| `P.Binary.BigarrayLoad` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_bigarray`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_bigarray_load` | 14-validation/bigarray_access.md |
| `P.Binary.BigarrayGetAlignment` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`backend/cmm_helpers.ml#bigstring_get_alignment` | — |
| `P.Binary.AtomicLoadField` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Binary.Poke` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Binary.ReadOffset` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Unary.Peek` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Ternary.ArraySet` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_array_set` | — |
| `P.Ternary.ArraySet.Vector` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set0`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition` | — |
| `P.Ternary.BytesOrBigstringSet` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bytes_or_bigstring_set` | — |
| `P.Ternary.BigarraySet` | spec | A | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_bigarray`<br>`middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bigarray_set` | 14-validation/bigarray_access.md |
| `P.Ternary.AtomicSetField` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive` | — |
| `P.Ternary.WriteOffset` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive` | — |
| `P.Quaternary.AtomicCompareAndSetField` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#quaternary_primitive` | — |
| `P.Nullary.StateAccessors` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive` | — |
| `P.Nullary.ControlBarriers` | spec | C | 06-primitives-memory.md | defining | `middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive` | — |
| `P.Unchecked.FrontendInsertsChecks` | spec | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_array_access`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_bound`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#checked_access`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives_helpers.ml#bind_recs` | — |
| `P.Unchecked.WideAccess` | impl | D | 06-primitives-memory.md | documented-anchor | `middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#actual_max_length_for_string_like_access_as_nativeint`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#bigstring_alignment_validity_condition` | — |

## T — Abstract domain, meet/join, provers, reification (ch. 07-08)

60 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `T.Role.SinglePass` | spec | D | 07-types-domain.md | documented-anchor | `middle_end/flambda2/types/grammar/type_grammar.mli#t` | — |
| `T.Grammar.TypeDescr` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_descr.mli#Descr.t`<br>`middle_end/flambda2/types/grammar/type_descr.mli#descr` | — |
| `T.Grammar.Variant` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#create_variant` | — |
| `T.Grammar.RowLike.Index` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_index_domain`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#row_like_index_domain`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#check_field_tys`<br>`middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape` | — |
| `T.Grammar.NakedImmediate.Relational` | spec | A | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#is_int_for_scrutinee`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#get_tag_for_block` | 14-validation/issue5721.md<br>14-validation/naked_immediates_many_relations.md |
| `T.Grammar.NakedNumber.NonEmptySet` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_float` | — |
| `T.Grammar.RecInfoRegion.Trivial` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_region`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_rec_info` | — |
| `T.Env.Canonical.Least` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/aliases.mli#get_canonical_element_exn`<br>`middle_end/flambda2/types/env/binding_time.ml#consts`<br>`middle_end/flambda2/types/env/aliases.mli#Alias_set.find_best` | — |
| `T.Env.Canonical.NoEqualsOnCanonical` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias` | — |
| `T.Env.Canonical.ConcreteOnCanonical` | impl | D | 07-types-domain.md | documented-anchor | `middle_end/flambda2/types/env/typing_env.ml#replace_equation` | — |
| `T.Env.ConstCanonicalPersists` | spec | B (proved-in-model) | 07-types-domain.md | theorem-qed | `middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals`<br>`middle_end/flambda2/types/env/aliases.mli#find_best`<br>`middle_end/flambda2/types/env/binding_time.ml#consts`<br>`middle_end/flambda2/types/env/meet_env.ml#record_demotion` | — |
| `T.Env.AliasesAuthoritative` | spec | B (proved-in-model) | 07-types-domain.md | theorem-qed | `middle_end/flambda2/types/env/aliases.ml#add`<br>`middle_end/flambda2/types/env/meet_env.ml#record_demotion`<br>`middle_end/flambda2/types/expand_head.ml#expand_head0`<br>`middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias` | — |
| `T.Env.Find.Canonical` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn`<br>`middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn` | — |
| `T.Env.Find.Bottom` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'`<br>`middle_end/flambda2/types/env/typing_env.ml#make_bottom` | — |
| `T.Env.Find.SymbolDefault` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'`<br>`middle_end/flambda2/types/env/typing_env.ml#initial_symbol_type` | — |
| `T.Env.Equation.Closed` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/typing_env.ml#invariant_for_new_equation` | — |
| `T.Env.Scope.Existential` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/env/binding_time.ml#With_name_mode.scoped_name_mode`<br>`middle_end/flambda2/types/env/typing_env.mli#cut` | — |
| `T.Gamma.Kind` | def | B (proved-in-model) | 07-types-domain.md | theorem-qed | `middle_end/flambda2/types/grammar/type_grammar.mli#kind` | — |
| `T.Gamma.TopBottom` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_descr.mli#unknown`<br>`middle_end/flambda2/types/grammar/type_descr.mli#bottom` | — |
| `T.Gamma.Alias` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#alias_type_of`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#get_alias_exn` | — |
| `T.Gamma.Value.Nullability` | def | A | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_value` | 14-validation/n_way_join_preserves_null.md |
| `T.Gamma.Value.Variant` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null` | — |
| `T.Gamma.Value.RowLikeBlocks` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_blocks` | — |
| `T.Gamma.Value.Boxed` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null` | — |
| `T.Gamma.Value.Closures` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_closures`<br>`middle_end/flambda2/types/grammar/type_grammar.mli#closures_entry` | — |
| `T.Gamma.Closures.CodeAgeLoose` | def | C (proved-in-model) | 07-types-domain.md | theorem-qed | `middle_end/flambda2/types/meet_and_join.ml#meet_code_id`<br>`middle_end/flambda2/types/env/code_age_relation.ml#meet`<br>`middle_end/flambda2/types/env/typing_env.ml#add_to_code_age_relation` | — |
| `T.Gamma.Naked.Set` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float` | — |
| `T.Gamma.Naked.Relational` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate` | — |
| `T.Gamma.EnvExtension` | def | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#env_extension`<br>`middle_end/flambda2/types/env/typing_env_extension.mli#t` | — |
| `T.Grammar.Disjunction.Extensions` | spec | C | 07-types-domain.md | defining | `middle_end/flambda2/types/grammar/type_grammar.ml#variant_extensions`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#row_like_case` | — |
| `T.Meet.Dispatch` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/ui/flambda_features.ml#use_n_way_join`<br>`middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join` | — |
| `T.Meet.Sound` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#meet` | — |
| `T.Meet.Bottom` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null` | — |
| `T.Meet.GreatestLowerBound` | spec | D [false-as-stated] | 08-meet-join.md | demoted-claim | `middle_end/flambda2/types/meet_and_join.mli#meet` | — |
| `T.Meet.AliasAlias` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/env/meet_env.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals` | — |
| `T.Meet.AliasConcrete` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/env/meet_env.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical` | — |
| `T.Meet.Store.CoercionErasure` | impl | B (proved-in-model) | 08-meet-join.md | theorem-qed | `middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical`<br>`middle_end/flambda2/types/env/meet_env.ml#record_demotion`<br>`middle_end/flambda2/identifiers/coercion0.mli#change_depth`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#apply_coercion` | — |
| `T.Meet.NakedNumber` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/meet_and_join.ml#meet_expanded_head0`<br>`middle_end/flambda2/types/meet_and_join.ml#set_meet` | — |
| `T.Meet.ValueHeadIncompatible` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null` | — |
| `T.Meet.MutableBlockMissedBottom` | impl | B (proved-in-model) | 08-meet-join.md | theorem-qed | `middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null` | — |
| `T.Meet.Variant` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/meet_and_join.ml#meet_variant`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_relation` | — |
| `T.Meet.BlockShape` | spec | A | 08-meet-join.md | defining | `middle_end/flambda2/types/meet_and_join.ml#meet_row_like_for_blocks`<br>`middle_end/flambda2/types/meet_and_join.ml#join_row_like_for_blocks`<br>`middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape`<br>`middle_end/flambda2/kinds/flambda_kind.ml#Block_shape.equal` | 14-validation/mixed-04-join.md |
| `T.Meet.Relational` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/meet_and_join.ml#reduce_inverse_relations`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_naked_immediate` | — |
| `T.Meet.Terminates` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/env/meet_env.ml#adding_equation_for_name`<br>`middle_end/flambda2/types/meet_and_join.ml#meet` | — |
| `T.Join.Sound` | spec | A | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/meet_and_join.ml#join`<br>`middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join` | 14-validation/n_way_join_null.md<br>14-validation/n_way_join_preserves_null.md |
| `T.Join.SharedAlias` | spec | C | 08-meet-join.md | defining | `middle_end/flambda2/types/meet_and_join.ml#join` | — |
| `T.Join.ConstAgreement` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/meet_and_join.ml#join`<br>`middle_end/flambda2/types/env/aliases.mli#find_best`<br>`middle_end/flambda2/types/env/typing_env.ml#alias_is_bound_strictly_earlier`<br>`middle_end/flambda2/types/env/binding_time.ml#consts` | — |
| `T.Join.Head` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/meet_and_join.ml#join_expanded_head`<br>`middle_end/flambda2/types/meet_and_join.ml#join_head_of_kind_value_non_null` | — |
| `T.Join.Cutoff` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/meet_and_join.ml#join`<br>`middle_end/flambda2/ui/flambda_features.ml#join_depth` | — |
| `T.Join.Levels` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join`<br>`middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0` | — |
| `T.Join.Existentials` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0`<br>`middle_end/flambda2/types/join_levels_old.ml#cut_and_n_way_join` | — |
| `T.Join.RecursiveParamsUnknown` | impl | D | 08-meet-join.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types` | — |
| `T.Prove.Sound` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/provers.mli#proof_of_property`<br>`middle_end/flambda2/types/provers.ml#prove_is_int` | — |
| `T.Prove.MeetShortcut` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/provers.mli#meet_shortcut`<br>`middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates` | — |
| `T.Prove.MeetShortcut.NullPremise` | impl | B (proved-in-model) | 08-meet-join.md | theorem-qed | `middle_end/flambda2/types/provers.ml#gen_value_to_meet`<br>`middle_end/flambda2/types/provers.ml#gen_value_to_proof`<br>`middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates` | — |
| `T.Prove.SimpleModeBoundary` | spec | B (proved-in-model) | 08-meet-join.md | theorem-qed | `middle_end/flambda2/types/provers.ml#prove_equals_to_simple_of_kind`<br>`middle_end/flambda2/types/provers.ml#meet_block_field_simple`<br>`middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot` | — |
| `T.Prove.GetTag` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/provers.ml#prove_get_tag` | — |
| `T.Expand.Head` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/expand_head.ml#expand_head`<br>`middle_end/flambda2/types/expand_head.ml#expand_head0` | — |
| `T.Reify.Sound` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/reify.ml#reify` | — |
| `T.Reify.LiftLocalGuard` | spec | C | 08-meet-join.md | theorem-admitted | `middle_end/flambda2/types/reify.ml#reify`<br>`middle_end/flambda2/types/provers.ml#never_holds_locally_allocated_values` | — |

## S — Simplify structure, rewrites, inlining, unboxing (ch. 09-12)

119 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `S.Struct.Run` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/flambda2.ml#flambda_to_flambda0` | — |
| `S.Struct.Run.ClosedResult` | spec | C | 09-simplify-structure.md | defining | `middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.Run.NoPendingConstants` | spec | C | 09-simplify-structure.md | defining | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/simplify/lifting/lifted_constant_state.mli` | — |
| `S.Struct.SingleRound` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/flambda2.ml#flambda_to_flambda0`<br>`middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.TwoPhase` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr`<br>`middle_end/flambda2/simplify/simplify_common.mli` | — |
| `S.Struct.Dacc` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/env/downwards_acc.ml#t`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#t` | — |
| `S.Struct.Uacc` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/env/upwards_acc.ml#t`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#create` | — |
| `S.Struct.TypesMonotoneDown` | spec | D [false-as-stated] | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/env/downwards_env.ml#add_variable`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_equation_on_name` | — |
| `S.Struct.EnvRefineOnly` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env`<br>`middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null`<br>`middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.SetOfClosuresEager` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_expr.mli`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_non_lifted_set_of_closures`<br>`middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common` | — |
| `S.Struct.LetCont.BodyFirst` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_let_cont0`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body` | — |
| `S.Struct.ContUse` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use`<br>`middle_end/flambda2/simplify/env/continuation_uses_env.mli`<br>`middle_end/flambda2/simplify/env/one_continuation_use.ml` | — |
| `S.Struct.Switch.ArmIsolation` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use`<br>`middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.JoinParams` | spec | C | 09-simplify-structure.md | defining | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/join_points.ml#join`<br>`middle_end/flambda2/types/flambda2_types.mli#cut_and_n_way_join` | — |
| `S.Struct.JoinParams.AnalysisExtraParams` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/join_points.ml#add_extra_params_from_join_analysis`<br>`middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join`<br>`driver/oxcaml_flags.ml#o2` | — |
| `S.Struct.NoJoinUnknown` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types` | — |
| `S.Struct.SingleInlinableUse` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.CSE.JoinPhi` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/common_subexpression_elimination.ml#join_one_cse_equation`<br>`middle_end/flambda2/simplify/common_subexpression_elimination.ml#cse_with_eligible_lhs`<br>`middle_end/flambda2/simplify/common_subexpression_elimination.ml#cut_cse_environment`<br>`middle_end/flambda2/simplify/join_points.ml#join` | — |
| `S.Struct.Rec.NoFixpoint` | spec | C | 09-simplify-structure.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers` | — |
| `S.Struct.Rec.InvariantVsVariant` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.ApplyContRewrite` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/apply_cont_rewrite.mli`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation`<br>`middle_end/flambda2/simplify/expr_builder.mli#rewrite_apply_cont` | — |
| `S.Struct.LetCont.UnreachableClosure` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_recursive_handlers`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont` | — |
| `S.Struct.LiftCont.Gate` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/are_lifting_conts.mli#t`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#get_continuation_lifting_budget`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body_and_handlers`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_switch`<br>`driver/oxcaml_flags.ml#o2` | — |
| `S.Struct.Turn` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common`<br>`middle_end/flambda2/simplify/flow/flow_analysis.mli#analyze`<br>`middle_end/flambda2/simplify/flow/flow_types.ml` | — |
| `S.Struct.Flow.RequiredNames` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_types.ml#Data_flow_result`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#required_names`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#reachable_code_ids` | — |
| `S.Struct.Flow.UnusedParams` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_analysis.mli`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive` | — |
| `S.Struct.Flow.DeadLoopParam` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/data_flow_graph.ml#required_names`<br>`middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Struct.Flow.Aliases` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_types.ml#Alias_result`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#extra_params_for_continuation_param_aliases`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler` | — |
| `S.Struct.Flow.ExnFirstParam` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/control_flow_graph.ml#minimize_extra_args_for_one_continuation`<br>`middle_end/flambda2/terms/exn_continuation.mli#t` | — |
| `S.Struct.Flow.MutableUnboxing` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/flow/mutable_unboxing.mli`<br>`middle_end/flambda2/simplify/flow/flow_types.ml#Mutable_unboxing_result`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Struct.Lift.Accumulate` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify`<br>`middle_end/flambda2/simplify/lifting/lifted_constant_state.mli`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#add_to_lifted_constant_accumulator` | — |
| `S.Struct.Lift.PlaceAtToplevel` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/simplify/expr_builder.mli#place_lifted_constants`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#at_unit_toplevel` | — |
| `S.Struct.Lift.EmptyAtEnd` | spec | C | 09-simplify-structure.md | defining | `middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.InlineResimplify` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml` | — |
| `S.Struct.SpeculativeSandbox` | spec | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#prepare_for_speculative_inlining`<br>`middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application` | — |
| `S.Struct.Resimplify` | impl | D | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#set_resimplify`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml` | — |
| `S.Struct.Loopify` | spec | B | 09-simplify-structure.md | documented-anchor | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body`<br>`middle_end/flambda2/simplify/loopify_state.mli` | 14-validation/loopify-01-escaping-tailrec.md |
| `S.Rewrite.Prim.Transfer` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0` | — |
| `S.Rewrite.Prim.ArgKindMismatch` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_primitive.ml#arg_kind_mismatch`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive` | — |
| `S.Rewrite.Prim.Relational` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_relational_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag` | — |
| `S.Rewrite.Alias.Canonicalize` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_simple.ml#simplify_simple0`<br>`middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn` | — |
| `S.Rewrite.Prim.ConstFold` | spec | B [compiler-bug, pending-upstream] | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unary_primitive` | 14-validation/new-01-constfold.md<br>14-validation/new-05-inline-fold.md |
| `S.Rewrite.Prim.ConstFold.Float` | impl | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen` | — |
| `S.Rewrite.Prim.ConstFold.PartialUndef` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like` | — |
| `S.Rewrite.Prim.Reify` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_named.ml#simplify_named0`<br>`middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify` | — |
| `S.Rewrite.Prim.IntIdentity` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift` | — |
| `S.Rewrite.Prim.FloatIdentity` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen` | — |
| `S.Rewrite.Prim.UntagTag` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number` | 14-validation/code_size_of_boolean_not_switch.md |
| `S.Rewrite.Prim.Projection` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load` | — |
| `S.Rewrite.Prim.PhysEqual` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal`<br>`middle_end/flambda2/types/provers.ml#prove_physical_equality` | — |
| `S.Rewrite.Prim.CompareRecovery` | impl | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#recover_comparison_primitive`<br>`middle_end/flambda2/simplify/comparison_result.ml#convert_result_compared_to_tagged_zero` | — |
| `S.Rewrite.Prim.ObjDupElide` | impl | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_obj_dup` | — |
| `S.Rewrite.CSE.Eligible` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/terms/flambda_primitive.ml#Eligible_for_cse.create`<br>`middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse`<br>`middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse` | 14-validation/cse_immutable_array_load_var_index.md<br>14-validation/cse_immutable_array_load.md |
| `S.Rewrite.CSE.Replace` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_primitive.ml#try_cse`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#apply_cse` | 14-validation/cse_immutable_array_load.md<br>14-validation/cse_immutable_array_load_var_index.md<br>14-validation/issue5721.md<br>14-validation/new-04-cse.md |
| `S.Rewrite.CSE.Extend` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_primitive.ml#try_cse`<br>`middle_end/flambda2/simplify/common_subexpression_elimination.ml#T0.add` | 14-validation/new-04-cse.md |
| `S.Rewrite.Share.StaticDynamicSplit` | spec | C [hybrid, pending-upstream] (proved-in-model (envelope)) | 10-simplify-rewrites.md | theorem-qed (hybrid) | `middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse`<br>`middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load` | — |
| `S.Rewrite.Switch.ArmPrune` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm` | 14-validation/array_element_kind_meet.md<br>14-validation/new-02-known-switch.md<br>14-validation/new-08-nested-switch.md |
| `S.Rewrite.Switch.Merge` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_arm` | 14-validation/new-02-known-switch.md |
| `S.Rewrite.Switch.Identity` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | 14-validation/n_way_join_null.md |
| `S.Rewrite.Switch.BooleanNot` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | 14-validation/code_size_of_boolean_not_switch.md<br>14-validation/new-08-nested-switch.md |
| `S.Rewrite.Switch.Invalid` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | — |
| `S.Rewrite.Let.DeadBinding` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/terms/flambda_primitive.ml#at_most_generative_effects` | 14-validation/array_element_kind_meet.md |
| `S.Rewrite.Let.DeadRegion` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region` | — |
| `S.Rewrite.Let.Phantom` | impl | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Rewrite.Let.Invalid` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_expr.ml#simplify_let0`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0` | — |
| `S.Rewrite.LetCont.Inline` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation` | 14-validation/new-03-letcont-inline.md |
| `S.Rewrite.LetCont.InlineForcesElimination` | spec | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont` | — |
| `S.Rewrite.LetCont.DeadHandler` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont` | — |
| `S.Rewrite.LetCont.Shortcut` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/continuation_shortcut.ml#apply`<br>`middle_end/flambda2/simplify/continuation_shortcut.ml#to_alias`<br>`middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts` | — |
| `S.Rewrite.LetCont.ShortcutFlat` | spec | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/continuation_shortcut.ml#apply`<br>`middle_end/flambda2/simplify/env/upwards_env.ml#add_continuation_shortcut` | — |
| `S.Rewrite.LetCont.UnusedParam` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive`<br>`middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze` | — |
| `S.Rewrite.LetCont.AliasedParam` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler` | — |
| `S.Rewrite.LetCont.InvalidHandler` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont` | — |
| `S.Rewrite.LetCont.Specialize` | impl | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#specialize_continuation_if_needed`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont` | — |
| `S.Rewrite.LetCont.DemoteExn` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#demote_exn_handler`<br>`middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind`<br>`middle_end/flambda2/simplify/simplify_common.ml#clear_demoted_trap_action`<br>`middle_end/flambda2/simplify/simplify_common.ml#patch_unused_exn_bucket`<br>`middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Rewrite.Apply.IndirectToDirect` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_function_call`<br>`middle_end/flambda2/types/provers.ml#meet_single_closures_entry` | 14-validation/naked_immediates_many_relations.md |
| `S.Rewrite.Apply.OverApplication` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_over_application`<br>`middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application` | — |
| `S.Rewrite.Apply.PartialApplication` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application` | — |
| `S.Rewrite.Apply.Invalid` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_apply_expr.ml#replace_apply_by_invalid`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call` | — |
| `S.Rewrite.Loopify.Attribute` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function`<br>`middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Acc.add_name_to_free_names` | 14-validation/loopify-03-not-purely-tailrec.md |
| `S.Rewrite.Loopify.Attribute.ValueSlotExempt` | spec | C | 10-simplify-rewrites.md | theorem-admitted | `middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Let_with_acc.create`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function` | — |
| `S.Rewrite.Loopify.Body` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body`<br>`middle_end/flambda2/simplify/loopify_state.mli`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body` | 14-validation/loopify-01-escaping-tailrec.md |
| `S.Rewrite.Loopify.SelfTailCall` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call` | 14-validation/loopify-06-mutual-and-mixed.md |
| `S.Rewrite.Loopify.AttributeUpdate` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0` | 14-validation/loopify-01-escaping-tailrec.md |
| `S.Rewrite.Code.RecursiveRecompute` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body` | 14-validation/loopify-01-escaping-tailrec.md |
| `S.Rewrite.LetCont.Demote` | spec | A | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers` | 14-validation/loopify-04-loop-attr-no-tailcall.md |
| `S.Rewrite.Loopify.TailrecEmitsNonRecursive` | spec | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/from_lambda/closure_conversion_aux.ml#create_apply`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body` | — |
| `S.Rewrite.Loopify.InvariantArgElim` | spec | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/flow/dominator_graph.ml#dominator_analysis`<br>`middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call` | — |
| `S.Rewrite.Loopify.SimplifyExposed` | impl | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call`<br>`middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0`<br>`middle_end/flambda2/flambda2.ml#flambda_to_flambda0` | — |
| `S.Rewrite.Loopify.ResimplifyIdempotent` | spec | D | 10-simplify-rewrites.md | documented-anchor | `middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers` | — |
| `S.Rewrite.Invalid.Propagate` | spec | C | 10-simplify-rewrites.md | defining | `middle_end/flambda2/simplify/expr_builder.ml#rebuild_invalid`<br>`middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr` | — |
| `S.Inline.ModeMismatchInvalid` | spec | C | 11-inlining.md | defining | `middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline` | — |
| `S.Inline.Substitute` | spec | A | 11-inlining.md | defining | `middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body`<br>`middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application` | 14-validation/new-05-inline-fold.md |
| `S.Inline.Substitute.Region` | spec | C | 11-inlining.md | defining | `middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body` | — |
| `S.Inline.Substitute.ExnExtraArgs` | spec | C | 11-inlining.md | defining | `middle_end/flambda2/simplify_shared/inlining_helpers.ml#wrap_inlined_body_for_exn_extra_args` | — |
| `S.Inline.DeclDecision` | impl | B | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify_shared/function_decl_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/terms/function_decl_inlining_decision_type.ml#behaviour` | 14-validation/code_size_of_single_arg_switch.md |
| `S.Inline.Decision` | impl | B | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision`<br>`middle_end/flambda2/simplify_shared/call_site_inlining_decision_type.ml#can_inline` | 14-validation/missing_code.md |
| `S.Inline.Speculative` | impl | B | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#might_inline`<br>`middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining`<br>`middle_end/flambda2/terms/cost_metrics.ml#evaluate` | 14-validation/inlining_cost_of_primitive_on_parameters.md<br>14-validation/removed_operations_of_switch.md<br>14-validation/speculative_inlining_lifted_constants.md |
| `S.Inline.DepthLimit` | impl | D | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/terms/inlining_state.ml#is_depth_exceeded`<br>`middle_end/flambda2/simplify/simplify_rec_info_expr.ml#depth_may_exceed`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#enter_inlined_apply` | — |
| `S.Inline.Unroll.Begin` | impl | D | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body`<br>`middle_end/flambda2/term_basics/coercion.ml#change_depth` | — |
| `S.Inline.Unroll.Continue` | impl | D | 11-inlining.md | documented-anchor | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/simplify_rec_info_expr.ml#known_remaining_unrolling_depth` | — |
| `S.Unbox.ContParam.Hook` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions` | — |
| `S.Unbox.Optimistic.Number` | impl | B | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision`<br>`middle_end/flambda2/simplify/unboxing/unboxers.ml` | 14-validation/new-07-float-unbox.md |
| `S.Unbox.Optimistic.Block` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_fields` | — |
| `S.Unbox.Optimistic.Closure` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_vars_within_closure` | — |
| `S.Unbox.Depth` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`driver/oxcaml_flags.ml#Flambda2.Expert` | — |
| `S.Unbox.ExtraArg.Available` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg`<br>`middle_end/flambda2/simplify/unboxing/unboxers.ml` | — |
| `S.Unbox.ExtraArg.Project` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg` | — |
| `S.Unbox.ExtraArg.Invalid` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg` | — |
| `S.Unbox.Refine.Pass` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#refine_decision_based_on_arg_types_at_uses`<br>`middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_one_decision_and_use` | — |
| `S.Unbox.Beneficial` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#is_unboxing_beneficial_for_epa`<br>`middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#filter_non_beneficial_decisions` | — |
| `S.Unbox.Loopify.AccumBoxElim` | impl | C [hybrid] | 12-unboxing.md | theorem-admitted (hybrid) | `middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions`<br>`middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler` | — |
| `S.Unbox.Optimistic.Variant` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant` | — |
| `S.Unbox.Variant.Discriminator` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#extra_args_for_const_ctor_of_variant` | — |
| `S.Unbox.FunParam.Wrapper` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps_function`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#compute_body_of_unboxed_function`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#make_unboxed_function_wrapper` | — |
| `S.Unbox.Denv.Equation` | spec | C | 12-unboxing.md | defining | `middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#denv_of_decision`<br>`middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#add_equation_on_var` | — |
| `S.Unbox.ContParam.Rewrite` | impl | A | 12-unboxing.md | defining | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#add_extra_params_and_args`<br>`middle_end/flambda2/simplify/apply_cont_rewrite.ml#create`<br>`middle_end/flambda2/simplify/expr_builder.ml#rewrite_apply_cont`<br>`middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args` | 14-validation/new-07-float-unbox.md |
| `S.Unbox.Mutable.Candidate` | impl | D | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/flow/mutable_unboxing.ml#blocks_to_unbox`<br>`middle_end/flambda2/simplify/flow/mutable_unboxing.ml#escaping` | — |
| `S.Unbox.Mutable.Rewrite` | impl | B | 12-unboxing.md | documented-anchor | `middle_end/flambda2/simplify/flow/mutable_unboxing.ml#Fold_prims.apply_prim`<br>`middle_end/flambda2/simplify/flow/mutable_unboxing.ml#compute_rewrites` | 14-validation/new-07-float-unbox.md |

## CM — Core Cmm operational semantics (ch. 15, 19)

28 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `CM.Syntax.Fragment` | impl | C | 15-cmm.md | defining | `backend/cmm.mli#expression`<br>`backend/cmm.mli#operation`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr` | — |
| `CM.Mem.LoadStore` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#memory_chunk`<br>`backend/cmm.mli#size_of_memory_chunk`<br>`backend/cmm_helpers.ml#mk_load_immut` | — |
| `CM.Context` | impl | C | 15-cmm.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr`<br>`backend/cmm.mli#expression` | — |
| `CM.Op.Pure` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#operation`<br>`backend/cmm_helpers.ml#add_int`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple` | — |
| `CM.Op.TupleField` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Ctuple_field`<br>`backend/cmm_helpers.ml#tuple_field`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call` | — |
| `CM.Load` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cload`<br>`backend/cmm_helpers.ml#mk_load_immut` | — |
| `CM.Store` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cstore`<br>`backend/cmm_helpers.ml#setfield_computed` | — |
| `CM.If` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cifthenelse`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch` | — |
| `CM.Switch` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cswitch`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch`<br>`backend/cmm_helpers.ml#transl_switch_clambda` | — |
| `CM.Catch.NonRec` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Ccatch`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined`<br>`backend/cmm_helpers.ml#create_ccatch` | — |
| `CM.Catch.Rec` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Ccatch`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec` | — |
| `CM.Exit` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cexit`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation` | — |
| `CM.Exit.Trap` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#trap_action`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise` | — |
| `CM.Exit.Return` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#exit_label`<br>`backend/cmm_helpers.ml#trap_return` | — |
| `CM.Catch.Exn` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Exn_handler`<br>`backend/cmm_helpers.ml#trywith`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler` | — |
| `CM.Raise` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Craise`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise`<br>`backend/cmm_helpers.ml#raise_prim` | — |
| `CM.Apply` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Capply`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0` | — |
| `CM.Apply.Raise` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Capply`<br>`backend/cmm.mli#Craise` | — |
| `CM.Extcall` | spec | C | 15-cmm.md | defining | `backend/cmm.mli#Cextcall`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call` | — |
| `CM.Invalid` | spec | C | 15-cmm.md | theorem-admitted | `backend/cmm.mli#Cinvalid`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid`<br>`middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid` | — |
| `CM.Unit.Final` | spec | C | 15-cmm.md | defining | `middle_end/flambda2/to_cmm/to_cmm.ml#unit`<br>`backend/cmm.mli#Cdata` | — |
| `CM.Alloc.Heap` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm.mli#Calloc`<br>`backend/cmm_helpers.ml#make_alloc_generic` | — |
| `CM.Region.Begin` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm.mli#Cbeginregion`<br>`middle_end/flambda2/terms/flambda_primitive.mli#Begin_region`<br>`middle_end/flambda2/terms/flambda_primitive.mli#Begin_try_region`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#variadic_primitive` | — |
| `CM.Alloc.Local` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm.mli#Calloc`<br>`backend/cmm_helpers.ml#local_block_header`<br>`middle_end/flambda2/to_cmm/to_cmm_shared.ml#alloc_mode_for_allocations_to_cmm` | — |
| `CM.Region.End` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm.mli#Cendregion`<br>`middle_end/flambda2/terms/flambda_primitive.mli#End_region`<br>`middle_end/flambda2/terms/flambda_primitive.mli#End_try_region`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive` | — |
| `CM.Alloc.GC` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm_helpers.ml#make_alloc_generic`<br>`backend/cmm.mli#machtype_component` | — |
| `CM.Addr.NoSurvive` | spec | C | 19-cmm-memory-gc.md | theorem-admitted | `backend/cmm.mli#machtype_component`<br>`backend/cmm_helpers.ml#field_address`<br>`backend/cmm_helpers.ml#setfield_computed` | — |
| `CM.Alloc.Exhaustion` | spec | C | 19-cmm-memory-gc.md | defining | `backend/cmm_helpers.ml#make_alloc_generic` | — |

## TC — to_cmm translation (ch. 16, 18)

36 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `TC.Expr.Dispatch` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr` | — |
| `TC.LetCont.Classify` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont`<br>`middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler` | — |
| `TC.LetCont.Inline` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_inlined`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#add_inline_cont` | — |
| `TC.LetCont.Jump` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#add_jump_cont` | — |
| `TC.LetCont.Exn` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler`<br>`backend/cmm_helpers.ml#trywith` | — |
| `TC.LetCont.Rec` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec` | — |
| `TC.ApplyCont.Jump` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation` | — |
| `TC.ApplyCont.Return` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_return_continuation`<br>`backend/cmm_helpers.ml#trap_return` | — |
| `TC.ApplyCont.Inline` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_cont` | — |
| `TC.ApplyCont.Raise` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise`<br>`backend/cmm_helpers.ml#raise_prim` | — |
| `TC.Apply.Call` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0` | — |
| `TC.Apply.Return` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr` | — |
| `TC.Apply.ExnWrapper` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply`<br>`backend/cmm_helpers.ml#trywith`<br>`backend/cmm_helpers.ml#raise_prim` | — |
| `TC.Switch` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch`<br>`backend/cmm_helpers.ml#transl_switch_clambda`<br>`backend/cmm_helpers.ml#ite` | — |
| `TC.Invalid` | spec | C | 16-to-cmm-control.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid`<br>`middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid` | — |
| `TC.Simple` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_shared.ml#simple`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple` | — |
| `TC.Prim.Sound` | spec | C | 18-to-cmm-data.md | theorem-admitted | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_complex`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `TC.Let.Simple` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple` | — |
| `TC.Let.Prim` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_prim`<br>`middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding` | — |
| `TC.Let.Subst` | spec | C | 18-to-cmm-data.md | theorem-admitted | `middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#bind_variable`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#add_binding_to_env`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `TC.Let.SetOfClosures` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#let_dynamic_set_of_closures` | — |
| `TC.Let.Static` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_static.ml#static_consts`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0` | — |
| `TC.Prim.TagUntag` | spec | C | 18-to-cmm-data.md | defining | `backend/cmm_helpers.ml#tag_int`<br>`backend/cmm_helpers.ml#untag_int`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion` | — |
| `TC.Prim.BoxUnbox` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number`<br>`backend/cmm_helpers.ml#box_int_gen`<br>`backend/cmm_helpers.ml#unbox_int`<br>`backend/cmm_helpers.ml#box_vector`<br>`backend/cmm_helpers.ml#unbox_vector` | — |
| `TC.Prim.ReinterpretBoxedVector` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive` | — |
| `TC.Prim.MakeBlock` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block`<br>`backend/cmm_helpers.ml#make_alloc_generic` | — |
| `TC.Prim.BlockLoad` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load`<br>`backend/cmm_helpers.ml#get_field_computed`<br>`backend/cmm_helpers.ml#field_address` | — |
| `TC.Prim.BlockSet` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_set`<br>`backend/cmm_helpers.ml#setfield_computed` | — |
| `TC.Prim.ProjectFunctionSlot` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple`<br>`backend/cmm_helpers.ml#infix_field_address` | — |
| `TC.Prim.ProjectValueSlot` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple`<br>`backend/cmm_helpers.ml#get_field_computed` | — |
| `TC.Prim.NumConv` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion`<br>`backend/cmm_helpers.ml#float32_of_int`<br>`backend/cmm.mli#static_cast` | — |
| `TC.Prim.StringLoad` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#string_like_load`<br>`backend/cmm_helpers.ml#unaligned_load_16` | — |
| `TC.Prim.ArrayAccess` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set0`<br>`backend/cmm_helpers.ml#array_indexing` | — |
| `TC.Prim.ArrayAccess.Vector` | spec | C | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load_vector`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set_vector`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load` | — |
| `TC.Prim.BigarrayAccess` | spec | A | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#bigarray_load_or_store`<br>`backend/cmm_helpers.ml#bigarray_load`<br>`backend/cmm_helpers.ml#bigarray_store` | 14-validation/bigarray_access.md |
| `TC.Prim.BigarrayLength` | spec | A | 18-to-cmm-data.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive`<br>`backend/cmm_helpers.ml#field_address` | 14-validation/bigarray_access.md |

## R — Representation relation (ch. 17)

16 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `R.Val.Imm` | spec | C | 17-representation.md | defining | `backend/cmm_helpers.ml#tag_int`<br>`backend/cmm_helpers.ml#untag_int`<br>`middle_end/flambda2/kinds/flambda_kind.ml#t` | — |
| `R.Val.NakedNumber` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unbox_number`<br>`backend/cmm_helpers.ml#sign_extend`<br>`backend/cmm.mli#machtype_component` | — |
| `R.Val.Pointer` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load`<br>`backend/cmm_helpers.ml#field_address` | — |
| `R.Val.Clos` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout`<br>`backend/cmm_helpers.ml#infix_field_address` | — |
| `R.Header` | spec | C | 17-representation.md | defining | `backend/cmm_helpers.ml#block_header`<br>`backend/cmm_helpers.ml#Mixed_block_support`<br>`backend/cmm_helpers.ml#caml_black` | — |
| `R.Obj.Block` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block`<br>`backend/cmm_helpers.ml#make_alloc_generic` | — |
| `R.Obj.Lazy` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#Lazy_block_tag` | — |
| `R.Obj.FloatBlock` | spec | C | 17-representation.md | defining | `backend/cmm_helpers.ml#float_header`<br>`backend/cmm_helpers.ml#make_float_alloc`<br>`backend/cmm_helpers.ml#floatarray_header` | — |
| `R.Obj.MixedBlock` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block`<br>`backend/cmm_helpers.ml#make_mixed_alloc`<br>`middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words` | — |
| `R.Obj.Array` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block`<br>`backend/cmm_helpers.ml#array_indexing`<br>`backend/cmm_helpers.ml#Unboxed_or_untagged_array_tags`<br>`backend/cmm_helpers.ml#unboxed_or_untagged_packed_array_length` | — |
| `R.Obj.Bytes` | spec | C | 17-representation.md | defining | `backend/cmm_helpers.ml#string_header` | — |
| `R.Obj.Bigarray` | spec | A | 17-representation.md | defining | `backend/cmm_helpers.ml#bigarray_load`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive` | 14-validation/bigarray_access.md |
| `R.Obj.Boxed` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number`<br>`backend/cmm_helpers.ml#float_header`<br>`backend/cmm_helpers.ml#boxedint64_header`<br>`backend/cmm_helpers.ml#boxedvec128_header` | — |
| `R.Obj.Closures` | spec | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot`<br>`backend/cmm_helpers.ml#pack_closure_info`<br>`backend/cmm_helpers.ml#infix_header`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout` | — |
| `R.Heap` | def | C | 17-representation.md | defining | `middle_end/flambda2/to_cmm/to_cmm.ml#unit` | — |
| `R.Observe` | spec | C | 17-representation.md | defining | `middle_end/flambda2/terms/flambda_unit.mli#module_symbol`<br>`middle_end/flambda2/to_cmm/to_cmm.ml#unit` | — |

## INV — Global invariants and to_cmm soundness (ch. 13, 20)

25 rules.

| Rule ID | Role | Grade | Chapter | Artifact | Code anchors | Verified |
|---|---|---|---|---|---|---|
| `INV.Simplify.Preserves` | spec | C | 13-soundness.md | theorem-admitted | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/flambda2.ml#flambda_to_flambda0` | — |
| `INV.Rewrite.Local` | spec | C | 13-soundness.md | theorem-admitted | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `INV.NameMode.Coherent` | spec | C | 13-soundness.md | theorem-admitted | `middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `INV.KindChecks.Gated` | impl | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/ui/flambda_features.ml#kind_checks`<br>`driver/oxcaml_args.ml`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_apply_shared` | — |
| `INV.Simplify.EffectfulDeletionInventory` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/terms/flambda_primitive.ml#is_end_region`<br>`middle_end/flambda2/simplify/named_rewrite.mli#Prim_rewrite`<br>`middle_end/flambda2/simplify/flow/mutable_unboxing.ml#make_result` | — |
| `INV.Simplify.RegionPairAtomic` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_acc.ml#record_let_binding`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/terms/flambda_primitive.ml#is_end_region`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region` | — |
| `INV.Simplify.RequiredNamesSound` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze`<br>`middle_end/flambda2/simplify/flow/dominator_graph.ml#create`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#filter_and_choose_alias`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#find_cse_simple` | — |
| `INV.Simplify.DeadCodeBodyLocal` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#record_free_names_of_apply_as_used`<br>`middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info`<br>`middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze` | — |
| `INV.Simplify.LiftedConstGranularity` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/flow/flow_acc.ml#normalize_lifted_constant_aux`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#keep_lifted_constant_only_if_used`<br>`middle_end/flambda2/simplify/expr_builder.ml#create_let_symbol0`<br>`middle_end/flambda2/simplify/flow/data_flow_graph.ml#reachable_code_ids` | — |
| `INV.Simplify.DeadValueSlotCoherence` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot`<br>`middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info`<br>`middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#value_slot_is_used`<br>`middle_end/flambda2/cmx/exported_code.ml#prepare_for_export` | — |
| `INV.Simplify.AliasesMonotoneDown` | spec | C [hybrid] | 13-soundness.md | theorem-admitted (hybrid) | `middle_end/flambda2/types/env/aliases.mli#add`<br>`middle_end/flambda2/types/env/aliases.ml#add`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env`<br>`middle_end/flambda2/types/env/binding_time.ml#consts` | — |
| `INV.Loopify.TrapNeutral` | spec | D | 13-soundness.md | documented-anchor | `middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_env.ml#add_continuation`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda.ml#compile_staticfail`<br>`middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr` | — |
| `INV.ToCmm.Control` | spec | C | 16-to-cmm-control.md | theorem-admitted | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr`<br>`middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler` | — |
| `INV.ToCmm.Simulates` | spec | C | 20-to-cmm-soundness.md | theorem-admitted | `middle_end/flambda2/to_cmm/to_cmm.ml#unit`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr` | — |
| `INV.ToCmm.EndToEnd` | spec | C [false-as-stated] | 20-to-cmm-soundness.md | theorem-admitted | `middle_end/flambda2/flambda2.ml#flambda_to_flambda0`<br>`middle_end/flambda2/to_cmm/to_cmm.ml#unit` | — |
| `INV.ToCmm.InvalidUnreached` | spec | C | 20-to-cmm-soundness.md | theorem-admitted | `middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#nullary_primitive`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive`<br>`backend/cmm.mli#Cinvalid` | — |
| `INV.ToCmm.SlotLiveness` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/flambda2.ml#build_run_result`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#finalize`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#mark_slot_as_removed`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot`<br>`middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots`<br>`middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive`<br>`middle_end/flambda2/cmx/exported_code.ml#prepare_for_export`<br>`middle_end/flambda2/types/env/cached_level.ml#remove_unused_value_slots_and_shortcut_aliases` | — |
| `INV.ToCmm.ClosureScanBoundary` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/simplify_shared/slot_offsets.ml#update_set_for_slot`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#layout_aux`<br>`middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot`<br>`backend/cmm_helpers.ml#pack_closure_info` | — |
| `INV.ToCmm.AddrConfined` | spec | D [hybrid] | 20-to-cmm-soundness.md | documented-anchor (hybrid) | `middle_end/flambda2/to_cmm/to_cmm_shared.ml#machtype_of_kind`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets`<br>`middle_end/flambda2/kinds/flambda_kind.ml#t`<br>`backend/cmm.mli#machtype_component` | — |
| `INV.ToCmm.EffectLinear` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr` | — |
| `INV.ToCmm.CallConvCoherent` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/terms/code_metadata.ml#function_slot_size`<br>`middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot`<br>`backend/cmm_helpers.ml#curry_function_sym`<br>`middle_end/flambda2/simplify_shared/slot_offsets.ml#create_function_slot` | — |
| `INV.ToCmm.StaticUpdateBarrier` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update`<br>`middle_end/flambda2/to_cmm/to_cmm_static.ml#update_field`<br>`backend/cmm_helpers.ml#setfield` | — |
| `INV.ToCmm.LoweringTotal` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/to_cmm/to_cmm_env.ml#inline_variable`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#get_continuation`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation`<br>`middle_end/flambda2/to_cmm/to_cmm_shared.ml#param_machtype_of_kinded_parameter` | — |
| `INV.ToCmm.SymbolInitPlacement` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/to_cmm/to_cmm_env.ml#place_symbol_inits`<br>`middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_bindings`<br>`middle_end/flambda2/to_cmm/to_cmm_shared.ml#make_update` | — |
| `INV.ToCmm.SymbolLocality` | spec | D | 20-to-cmm-soundness.md | documented-anchor | `middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol`<br>`middle_end/flambda2/to_cmm/to_cmm_result.ml#symbol_of_code_id`<br>`middle_end/flambda2/to_cmm/to_cmm_result.ml#raw_symbol`<br>`middle_end/flambda2/flambda2.ml#build_run_result` | — |

## Consistency check results

_Generated by scanning chapters 02-20. Headline counts (rules, claims,
grades, chapters) live in the [Solidity
scoreboard](#solidity-scoreboard) above; this section holds the
mechanical checks._

- **Duplicate rule IDs:** none.
- **Rules with no code anchor:** none (all rules have ≥ 1 `CODE`).

### Anchor-resolution check

An anchor path is considered resolvable if it exists relative to the
repository root or relative to `middle_end/flambda2/`. Name resolution checks
that the last `.`-separated component of `#name` occurs as a whole-word token
(`grep -w`) in the file.

All anchors resolve: every `CODE` path exists (under one of those two
roots) and every anchored name occurs as a whole-word token in its file.

### Duplicate-ID check

No duplicate rule IDs across chapters 02-20. In particular the `P.Effects.*`
family is cleanly partitioned: chapter 05 defines `P.Effects.PureScalars`,
`P.Effects.FloatRoundingMode`, `P.Effects.BoxNumber`; chapter 06 defines the
remaining `P.Effects.*` rules. No collision between the two chapters.

### Fence-parse check

- clean.

### Artifact-classifier check

- clean.

### Doc/.v join check

- clean.

### Event and staleness check

- Event and staleness checks: all evidence events well-formed, commit keys resolve as ancestors of the base, no lift-ready provisos, no stale evidence.

### Solidity join and grade check

- WARNING[hybrid-no-clauses] INV.Simplify.AliasesMonotoneDown (13-soundness.md:499): HYBRID id INV.Simplify.AliasesMonotoneDown with no clause inventory and no clause-labeled evidence; grading at a single implicit clause with no evidence
- WARNING[hybrid-no-clauses] INV.ToCmm.AddrConfined (20-to-cmm-soundness.md:291): HYBRID id INV.ToCmm.AddrConfined with no clause inventory and no clause-labeled evidence; grading at a single implicit clause with no evidence
- WARNING[hybrid-no-clauses] S.Rewrite.Share.StaticDynamicSplit (10-simplify-rewrites.md:526): HYBRID id S.Rewrite.Share.StaticDynamicSplit with no clause inventory and no clause-labeled evidence; grading at a single implicit clause with no evidence
- WARNING[hybrid-no-clauses] S.Unbox.Loopify.AccumBoxElim (12-unboxing.md:367): HYBRID id S.Unbox.Loopify.AccumBoxElim with no clause inventory and no clause-labeled evidence; grading at a single implicit clause with no evidence
