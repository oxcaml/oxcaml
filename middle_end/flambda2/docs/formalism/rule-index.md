# Rule index

Machine-greppable index of every formal rule in the Flambda 2 formalism
(chapters 02-13; chapter 01 has none). Each
row corresponds to one ```` ```rule ```` block in a chapter. See
[`README.md`](README.md) ("Rule blocks" and "Rule ID namespaces") for the
block format and the meaning of each namespace prefix.

## Columns

- **Rule ID** — the rule's stable ID (`RULE` header). Globally unique; never
  renumbered or reused.
- **Status** — `normative` (code must satisfy it), `descriptive` (documents
  the current algorithm/heuristic; may change), or `conjectured` (believed but
  not yet verified against the code).
- **Chapter** — the chapter file containing the rule block.
- **Code anchors** — every `CODE` anchor (`path#name`), as written in the
  block. Paths are relative to `middle_end/flambda2/` unless they already begin
  with `middle_end/`. A leading component may need `middle_end/flambda2/`
  prepended to resolve from the repository root.
- **Verified** — validation case study/studies (`VERIFIED` header), or `—` if
  none yet.

## How agents should use this index

- To answer a question or synthesize a test, grep here for candidate rules,
  then read the owning chapter section. Trust `normative` rules; treat
  `descriptive` as current-behaviour documentation and `conjectured` as leads
  to verify against the code.
- When code under `middle_end/flambda2/` changes, grep the **Code anchors**
  column for the changed file, then follow the sync protocol in `README.md`.

## Regenerating this index

This file is mechanically derived from the ```` ```rule ```` blocks in
chapters `02`-`13`. To rebuild: scan each `NN-*.md` for fenced `rule` blocks,
read the `RULE` / `STATUS` / `CODE` / `VERIFIED` header lines of each, bucket
by namespace prefix (the text before the first `.` in the ID), and emit one
table per namespace plus the consistency-check section below. The extraction
and check scripts used for the current revision live only in the generating
agent's scratch space; the procedure above is the source of truth.

## WF — Kinding and well-formedness (ch. 02-03)

39 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `WF.Syntax.Anf` | normative | 02-syntax.md | `middle_end/flambda2/terms/flambda.mli#expr_descr` | — |
| `WF.Syntax.LetKindUniform` | normative | 02-syntax.md | `middle_end/flambda2/bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Syntax.SingletonNotSetOfClosures` | normative | 02-syntax.md | `middle_end/flambda2/bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Syntax.SwitchScrutinee` | normative | 02-syntax.md | `middle_end/flambda2/terms/switch_expr.mli#t`<br>`middle_end/flambda2/terms/switch_expr.mli#create` | — |
| `WF.Syntax.SwitchMinArms` | normative | 02-syntax.md | `middle_end/flambda2/terms/switch_expr.mli#t`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#close_switch`<br>`middle_end/flambda2/simplify/expr_builder.ml#create_switch`<br>`middle_end/flambda2/terms/flambda.mli#Invalid.t` | `14-validation/gadt_simplified_switch.md` |
| `WF.Syntax.ExnHandlerNonRecursive` | normative | 02-syntax.md | `middle_end/flambda2/terms/flambda.mli#Continuation_handler`<br>`middle_end/flambda2/terms/flambda.mli#Let_cont_expr` | — |
| `WF.Syntax.ExnHandlerFirstParamBucket` | conjectured | 02-syntax.md | `middle_end/flambda2/terms/exn_continuation.mli#arity`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `WF.Syntax.EffectCalleeNone` | normative | 02-syntax.md | `middle_end/flambda2/terms/call_kind.mli#Effect`<br>`middle_end/flambda2/terms/apply_expr.mli#create` | — |
| `WF.Syntax.ContSecondClass` | normative | 02-syntax.md | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr` | — |
| `WF.Syntax.NonRecOccursPositive` | conjectured | 02-syntax.md | `middle_end/flambda2/terms/flambda.mli#let_cont_expr`<br>`middle_end/flambda2/terms/flambda.ml#Let_cont_expr.create_non_recursive0` | — |
| `WF.Syntax.StaticRecThroughCode` | normative | 02-syntax.md | `middle_end/flambda2/bound_identifiers/bound_static.mli#create` | — |
| `WF.Syntax.ImmutableArrayNonEmpty` | normative | 02-syntax.md | `middle_end/flambda2/terms/static_const.mli#t` | — |
| `WF.Syntax.NameModeInTerms` | normative | 02-syntax.md | `middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms` | — |
| `WF.Subkind.Erasable` | normative | 03-kinds.md | `kinds/flambda_kind.ml#With_subkind.erase_subkind`<br>`kinds/flambda_kind.ml#With_subkind.equal_ignoring_subkind`<br>`kinds/flambda_kind.ml#With_subkind.has_useful_subkind_info` | — |
| `WF.Subkind.Scannable` | descriptive | 03-kinds.md | `kinds/flambda_kind.ml#With_subkind.must_be_gc_scannable` | — |
| `WF.Arity.Unarize` | normative | 03-kinds.md | `kinds/flambda_arity.ml#unarize`<br>`kinds/flambda_arity.ml#Component.unarize` | — |
| `WF.Arity.ApplyFlavours` | normative | 03-kinds.md | `terms/apply_expr.mli#create`<br>`terms/apply_expr.mli#args_arity`<br>`terms/apply_expr.mli#return_arity` | — |
| `WF.Kind.Var` | normative | 03-kinds.md | `term_basics/simple.ml#kind`<br>`identifiers/int_ids.ml#Variable.kind` | — |
| `WF.Kind.Symbol` | normative | 03-kinds.md | `term_basics/simple.ml#kind` | — |
| `WF.Kind.Const` | normative | 03-kinds.md | `identifiers/reg_width_const.ml#kind`<br>`identifiers/int_ids.mli#Const.Descr` | — |
| `WF.Kind.Coerce` | normative | 03-kinds.md | `identifiers/coercion0.mli#S`<br>`term_basics/simple.ml#kind` | — |
| `WF.Let.Singleton` | normative | 03-kinds.md | `terms/flambda.ml#Named.kind`<br>`terms/flambda.mli#Named.kind`<br>`bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Let.SetOfClosures` | normative | 03-kinds.md | `terms/flambda.mli#Named`<br>`bound_identifiers/bound_pattern.mli#t`<br>`simplify/simplify_set_of_closures.ml` | — |
| `WF.Let.Static` | normative | 03-kinds.md | `terms/flambda.mli#Named`<br>`bound_identifiers/bound_pattern.mli#t` | — |
| `WF.Named.Prim` | normative | 03-kinds.md | `terms/flambda.ml#Named.kind`<br>`terms/flambda_primitive.mli#result_kind`<br>`terms/flambda_primitive.ml#result_kind'` | — |
| `WF.Named.Simple` | normative | 03-kinds.md | `terms/flambda.ml#Named.kind` | — |
| `WF.Named.RecInfo` | normative | 03-kinds.md | `terms/flambda.ml#Named.kind` | — |
| `WF.Prim.ArgKinds` | normative | 03-kinds.md | `terms/flambda_primitive.mli#arg_kind_of_unary_primitive`<br>`terms/flambda_primitive.mli#args_kind_of_binary_primitive`<br>`terms/flambda_primitive.mli#args_kind_of_variadic_primitive` | — |
| `WF.Switch.Scrutinee` | normative | 03-kinds.md | `terms/switch_expr.ml#t`<br>`simplify/simplify_switch_expr.ml#simplify_arm` | — |
| `WF.Switch.NonEmpty` | normative | 03-kinds.md | `simplify/simplify_switch_expr.ml`<br>`simplify/expr_builder.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.ApplyCont.Arity` | normative | 03-kinds.md | `terms/apply_cont_expr.mli#create`<br>`bound_identifiers/bound_parameters.mli#arity`<br>`bound_identifiers/bound_parameter.mli#kind`<br>`simplify/env/continuation_uses.ml#add_use`<br>`simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation` | — |
| `WF.Apply.ArgKinds` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml#simplify_apply_shared`<br>`terms/apply_expr.mli#args_arity` | — |
| `WF.Apply.DirectArity` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml#arity_mismatch`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.DirectResultArity` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.Over` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml`<br>`simplify/simplify_common.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Apply.Partial` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml`<br>`terms/flambda.mli#Invalid` | — |
| `WF.Region.Var` | normative | 03-kinds.md | `from_lambda/closure_conversion.ml`<br>`terms/flambda_primitive.ml#result_kind_of_variadic_primitive` | — |
| `WF.RecInfo.MyDepth` | normative | 03-kinds.md | `simplify/simplify_apply_expr.ml`<br>`simplify/simplify_set_of_closures.ml`<br>`terms/flambda.mli#Function_params_and_body.create` | — |
| `WF.Check.Gated` | descriptive | 03-kinds.md | `ui/flambda_features.ml#kind_checks`<br>`driver/oxcaml_flags.ml`<br>`driver/oxcaml_args.ml`<br>`simplify/simplify_apply_expr.ml#simplify_apply_shared` | — |

## OS — Operational semantics (ch. 04)

29 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `OS.Simple.Eval` | normative | 04-opsem.md | `middle_end/flambda2/term_basics/simple.mli#t`<br>`middle_end/flambda2/term_basics/coercion.mli#t` | — |
| `OS.Let.Simple` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Let_expr`<br>`middle_end/flambda2/terms/flambda.mli#Named` | — |
| `OS.Let.Prim.Pure` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects`<br>`middle_end/flambda2/terms/effects.mli#t` | — |
| `OS.Let.Prim.Effect` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `OS.Let.SetOfClosures` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/set_of_closures.mli#create`<br>`middle_end/flambda2/bound_identifiers/alloc_mode.mli#For_allocations` | — |
| `OS.Let.Static` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/terms/static_const.mli#t`<br>`middle_end/flambda2/terms/flambda.mli#Static_const_or_code`<br>`middle_end/flambda2/bound_identifiers/bound_static.mli#Pattern` | — |
| `OS.Let.RecInfo` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Named`<br>`middle_end/flambda2/term_basics/rec_info_expr.mli#t` | — |
| `OS.LetCont.NonRec` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr`<br>`middle_end/flambda2/terms/flambda.mli#Non_recursive_let_cont_handler`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `OS.LetCont.Rec` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#Let_cont_expr`<br>`middle_end/flambda2/terms/flambda.mli#Recursive_let_cont_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation`<br>`middle_end/flambda2/simplify/apply_cont_rewrite.ml#get_used_params` | — |
| `OS.ApplyCont` | normative | 04-opsem.md | `middle_end/flambda2/terms/apply_cont_expr.mli#create`<br>`middle_end/flambda2/terms/flambda.mli#Continuation_handler` | — |
| `OS.ApplyCont.Return` | normative | 04-opsem.md | `middle_end/flambda2/terms/apply_expr.mli#Result_continuation`<br>`middle_end/flambda2/terms/flambda.mli#Function_params_and_body` | — |
| `OS.ApplyCont.ExnBoundary` | normative | 04-opsem.md | `middle_end/flambda2/terms/exn_continuation.mli#t`<br>`middle_end/flambda2/terms/exn_continuation.mli#extra_args` | — |
| `OS.ApplyCont.TrapPush` | normative | 04-opsem.md | `middle_end/flambda2/terms/trap_action.mli#t`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps` | `14-validation/new-06-trap.md` |
| `OS.ApplyCont.TrapPop` | normative | 04-opsem.md | `middle_end/flambda2/terms/trap_action.mli#t`<br>`middle_end/flambda2/terms/apply_cont_expr.ml#is_raise` | `14-validation/new-06-trap.md` |
| `OS.ApplyCont.Raise` | normative | 04-opsem.md | `middle_end/flambda2/terms/apply_cont_expr.ml#is_raise`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#close_raise0`<br>`middle_end/flambda2/terms/trap_action.mli#Raise_kind` | — |
| `OS.Switch` | normative | 04-opsem.md | `middle_end/flambda2/terms/switch_expr.mli#create`<br>`middle_end/flambda2/terms/switch_expr.mli#arms` | — |
| `OS.Switch.Undef` | normative | 04-opsem.md | `middle_end/flambda2/terms/switch_expr.mli#create`<br>`middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch` | — |
| `OS.Apply.Direct` | normative | 04-opsem.md | `middle_end/flambda2/terms/apply_expr.mli#create`<br>`middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/flambda.mli#Function_params_and_body`<br>`middle_end/flambda2/terms/code_metadata.mli#params_arity` | — |
| `OS.Apply.IndirectUnknownArity.Full` | normative | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/code_metadata.mli#params_arity` | — |
| `OS.Apply.IndirectUnknownArity.Partial` | conjectured | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/code_metadata.mli#first_complex_local_param`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application` | — |
| `OS.Apply.IndirectUnknownArity.Over` | conjectured | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Function_call`<br>`middle_end/flambda2/terms/apply_expr.mli#return_arity`<br>`middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application` | — |
| `OS.Apply.IndirectKnownArity` | normative | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Function_call` | — |
| `OS.Apply.CCall` | normative | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#t`<br>`middle_end/flambda2/terms/apply_expr.mli#create` | — |
| `OS.Apply.Method` | conjectured | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Method_kind` | — |
| `OS.Apply.Effect` | conjectured | 04-opsem.md | `middle_end/flambda2/terms/call_kind.mli#Effect` | — |
| `OS.Apply.NeverReturns` | normative | 04-opsem.md | `middle_end/flambda2/terms/apply_expr.mli#Result_continuation`<br>`middle_end/flambda2/terms/apply_expr.mli#returns` | — |
| `OS.Invalid` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda.mli#expr_descr`<br>`middle_end/flambda2/terms/flambda.mli#Invalid` | — |
| `OS.Unit.Init` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda_unit.mli#create` | — |
| `OS.Unit.Final` | normative | 04-opsem.md | `middle_end/flambda2/terms/flambda_unit.mli#return_continuation`<br>`middle_end/flambda2/terms/flambda_unit.mli#module_symbol` | — |

## P — Primitive denotations (ch. 05-06)

87 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `P.Contract.NoRaiseNoControl` | normative | 05-primitives-scalar.md | `middle_end/flambda2/terms/flambda_primitive.mli#t` | — |
| `P.Unary.IntArith.SwapByteEndianness` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Unary_int_arith`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#For_int16s`<br>`middle_end/flambda2/numbers/target_ocaml_int.ml#get_least_significant_16_bits_then_byte_swap` | — |
| `P.Unary.FloatArith` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op`<br>`middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen` | — |
| `P.Unary.NumConv` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_int_conv`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Unary.NumConv.FloatToInt.OutOfRange` | normative | 05-primitives-scalar.md | `middle_end/flambda2/numbers/target_ocaml_int.ml#of_float`<br>`middle_end/flambda2/numbers/numeric_types.ml#Short_int`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Unary.NumConv.Int32ToInt64.SignExtend` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/number_adjuncts.ml#For_int32s`<br>`middle_end/flambda2/z3/sign_extension.py` | — |
| `P.Unary.BooleanNot` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_boolean_not` | — |
| `P.Unary.TagImmediate` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_tag_immediate` | — |
| `P.Unary.UntagImmediate` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate` | — |
| `P.Unary.Reinterpret64.Int64AsFloat64` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_unboxed_float64`<br>`middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen` | — |
| `P.Unary.Reinterpret64.Float64AsInt64` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_float64_as_unboxed_int64` | — |
| `P.Unary.Reinterpret64.Int64AsTaggedInt63` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_tagged_int63` | — |
| `P.Unary.Reinterpret64.TaggedInt63AsInt64` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_tagged_int63_as_unboxed_int64` | — |
| `P.Unary.BoxNumber` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_box_number`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.UnboxNumber` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Binary.IntArith.Total` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#Num_common` | — |
| `P.Binary.IntArith.DivMod` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/numbers/target_ocaml_int.ml#div` | — |
| `P.Binary.IntArith.DivModByZero` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_zero_division` | — |
| `P.Binary.IntArith.DivMinIntByMinusOne` | normative | 05-primitives-scalar.md | `middle_end/flambda2/numbers/target_ocaml_int.ml#div`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#For_int64s`<br>`backend/cmm_helpers.ml#div_int`<br>`backend/cmm_helpers.ml#make_safe_divmod` | — |
| `P.Binary.IntShift` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#with_shift` | — |
| `P.Binary.IntShift.OutOfRange.FolderPicksZero` | descriptive | 05-primitives-scalar.md | `middle_end/flambda2/simplify/number_adjuncts.ml#with_shift` | — |
| `P.Binary.IntShift.ByZero` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift` | — |
| `P.Binary.IntComp.Bool` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp`<br>`middle_end/flambda2/simplify/number_adjuncts.ml#compare_unsigned_generic` | — |
| `P.Binary.IntComp.CompareFunction` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp` | — |
| `P.Binary.FloatArith` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Binary.FloatComp.Bool` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Binary.FloatComp.CompareFunction` | normative | 05-primitives-scalar.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen`<br>`middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics` | — |
| `P.Effects.PureScalars` | normative | 05-primitives-scalar.md | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive` | — |
| `P.Effects.FloatRoundingMode` | normative | 05-primitives-scalar.md | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive` | — |
| `P.Effects.BoxNumber` | normative | 05-primitives-scalar.md | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Effects.Classification` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/effects_and_coeffects.ml`<br>`middle_end/flambda2/terms/effects.ml`<br>`middle_end/flambda2/terms/coeffects.ml` | — |
| `P.Effects.NoEffects` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.OnlyGenerative` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.Arbitrary` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/effects.ml#t` | — |
| `P.Effects.Coeffects` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/coeffects.ml#t` | — |
| `P.Effects.Placement` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/placement.ml#t` | — |
| `P.Effects.Validity` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/validity.ml#t` | — |
| `P.Effects.Pure` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/effects_and_coeffects.ml#pure`<br>`middle_end/flambda2/terms/effects_and_coeffects.ml#pure_can_be_duplicated` | — |
| `P.Effects.ReadingFromBlock` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_block` | — |
| `P.Effects.Writing` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_block` | — |
| `P.Effects.Allocation` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_variadic_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#coeffects_of_mode` | — |
| `P.Variadic.MakeBlock.Values` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind`<br>`middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_block` | — |
| `P.Variadic.MakeBlock.NakedFloats` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind` | — |
| `P.Variadic.MakeBlock.Mixed` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#Block_kind`<br>`middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape` | — |
| `P.Variadic.MakeArray` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive`<br>`middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_array` | — |
| `P.Variadic.BeginRegion` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region` | — |
| `P.Variadic.BeginTryRegion` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive` | — |
| `P.Unary.BlockLoad` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_mutable_block_load` | — |
| `P.Unary.BlockLoad.NakedFloats` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind` | — |
| `P.Binary.BlockSet` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set` | — |
| `P.Unary.DuplicateBlock` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.DuplicateArray` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.IsInt.Immediate` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int` | — |
| `P.Unary.IsInt.Pointer` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int` | — |
| `P.Unary.IsNull` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.GetTag` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag` | — |
| `P.Unary.GetHeader` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.ArrayLength` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length` | — |
| `P.Unary.StringLength` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length` | — |
| `P.Unary.BigarrayLength` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.ProjectFunctionSlot` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot` | — |
| `P.Unary.ProjectValueSlot` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot` | — |
| `P.Unary.IsBoxedFloat` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.EndRegion` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.EndTryRegion` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.ObjDup` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.OpaqueIdentity` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive` | — |
| `P.Unary.MakeLazy` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Unary.IntAsPointer` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Binary.ArrayLoad` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load` | — |
| `P.Binary.StringOrBigstringLoad` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load` | — |
| `P.Binary.PhysEqual` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal` | — |
| `P.Binary.BigarrayLoad` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_bigarray` | — |
| `P.Binary.BigarrayGetAlignment` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Binary.AtomicLoadField` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Binary.Poke` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Binary.ReadOffset` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive` | — |
| `P.Unary.Peek` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive` | — |
| `P.Ternary.ArraySet` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_array_set` | — |
| `P.Ternary.BytesOrBigstringSet` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bytes_or_bigstring_set` | — |
| `P.Ternary.BigarraySet` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_bigarray` | — |
| `P.Ternary.AtomicSetField` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive` | — |
| `P.Ternary.WriteOffset` | conjectured | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive` | — |
| `P.Quaternary.AtomicCompareAndSetField` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#quaternary_primitive` | — |
| `P.Nullary.StateAccessors` | descriptive | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive` | — |
| `P.Nullary.ControlBarriers` | normative | 06-primitives-memory.md | `middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive` | — |
| `P.Unchecked.FrontendInsertsChecks` | normative | 06-primitives-memory.md | `middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_array_access`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_bound`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#checked_access`<br>`middle_end/flambda2/from_lambda/lambda_to_flambda_primitives_helpers.ml#bind_recs` | — |

## T — Abstract domain, meet/join, provers, reification (ch. 07-08)

51 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `T.Role.SinglePass` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#t` | — |
| `T.Grammar.TypeDescr` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_descr.mli#Descr.t`<br>`middle_end/flambda2/types/grammar/type_descr.mli#descr` | — |
| `T.Grammar.Variant` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#create_variant` | — |
| `T.Grammar.RowLike.Index` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_index_domain`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#row_like_index_domain` | — |
| `T.Grammar.NakedImmediate.Relational` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#is_int_for_scrutinee`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#get_tag_for_block` | `14-validation/issue5721.md`<br>`14-validation/naked_immediates_many_relations.md` |
| `T.Grammar.NakedNumber.NonEmptySet` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_float` | — |
| `T.Grammar.RecInfoRegion.Trivial` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_region`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_rec_info` | — |
| `T.Env.Canonical.Least` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/aliases.mli#get_canonical_element_exn`<br>`middle_end/flambda2/types/env/binding_time.ml#consts`<br>`middle_end/flambda2/types/env/aliases.mli#Alias_set.find_best` | — |
| `T.Env.Canonical.NoEqualsOnCanonical` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias` | — |
| `T.Env.Canonical.ConcreteOnCanonical` | descriptive | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#replace_equation` | — |
| `T.Env.Find.Canonical` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn`<br>`middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn` | — |
| `T.Env.Find.Bottom` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'`<br>`middle_end/flambda2/types/env/typing_env.ml#make_bottom` | — |
| `T.Env.Find.SymbolDefault` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'`<br>`middle_end/flambda2/types/env/typing_env.ml#initial_symbol_type` | — |
| `T.Env.Equation.Closed` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/typing_env.ml#invariant_for_new_equation` | — |
| `T.Env.Scope.Existential` | normative | 07-types-domain.md | `middle_end/flambda2/types/env/binding_time.ml#With_name_mode.scoped_name_mode`<br>`middle_end/flambda2/types/env/typing_env.mli#cut` | — |
| `T.Gamma.Kind` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#kind` | — |
| `T.Gamma.TopBottom` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_descr.mli#unknown`<br>`middle_end/flambda2/types/grammar/type_descr.mli#bottom` | — |
| `T.Gamma.Alias` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#alias_type_of`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#get_alias_exn` | — |
| `T.Gamma.Value.Nullability` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_value` | `14-validation/n_way_join_preserves_null.md` |
| `T.Gamma.Value.Variant` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null` | — |
| `T.Gamma.Value.RowLikeBlocks` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_blocks` | — |
| `T.Gamma.Value.Boxed` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null` | — |
| `T.Gamma.Value.Closures` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_closures`<br>`middle_end/flambda2/types/grammar/type_grammar.mli#closures_entry` | — |
| `T.Gamma.Naked.Set` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float` | — |
| `T.Gamma.Naked.Relational` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate` | — |
| `T.Gamma.EnvExtension` | conjectured | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#env_extension`<br>`middle_end/flambda2/types/env/typing_env_extension.mli#t` | — |
| `T.Grammar.Disjunction.Extensions` | normative | 07-types-domain.md | `middle_end/flambda2/types/grammar/type_grammar.ml#variant_extensions`<br>`middle_end/flambda2/types/grammar/type_grammar.ml#row_like_case` | — |
| `T.Meet.Dispatch` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/ui/flambda_features.ml#use_n_way_join`<br>`middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join` | — |
| `T.Meet.Sound` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#meet` | — |
| `T.Meet.Bottom` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet.ml#meet`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null` | — |
| `T.Meet.GreatestLowerBound` | conjectured | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.mli#meet` | — |
| `T.Meet.AliasAlias` | normative | 08-meet-join.md | `middle_end/flambda2/types/env/meet_env.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals` | — |
| `T.Meet.AliasConcrete` | normative | 08-meet-join.md | `middle_end/flambda2/types/env/meet_env.ml#meet`<br>`middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical` | — |
| `T.Meet.NakedNumber` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#meet_expanded_head0`<br>`middle_end/flambda2/types/meet_and_join.ml#set_meet` | — |
| `T.Meet.ValueHeadIncompatible` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null` | — |
| `T.Meet.Variant` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#meet_variant`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_relation` | — |
| `T.Meet.Relational` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#reduce_inverse_relations`<br>`middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_naked_immediate` | — |
| `T.Meet.Terminates` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/env/meet_env.ml#adding_equation_for_name`<br>`middle_end/flambda2/types/meet_and_join.ml#meet` | — |
| `T.Join.Sound` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#join`<br>`middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join` | `14-validation/n_way_join_null.md`<br>`14-validation/n_way_join_preserves_null.md` |
| `T.Join.SharedAlias` | normative | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#join` | — |
| `T.Join.Head` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#join_expanded_head`<br>`middle_end/flambda2/types/meet_and_join.ml#join_head_of_kind_value_non_null` | — |
| `T.Join.Cutoff` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/meet_and_join.ml#join`<br>`middle_end/flambda2/ui/flambda_features.ml#join_depth` | — |
| `T.Join.Levels` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join`<br>`middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0` | — |
| `T.Join.Existentials` | descriptive | 08-meet-join.md | `middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0`<br>`middle_end/flambda2/types/join_levels_old.ml#cut_and_n_way_join` | — |
| `T.Join.RecursiveParamsUnknown` | descriptive | 08-meet-join.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types` | — |
| `T.Prove.Sound` | normative | 08-meet-join.md | `middle_end/flambda2/types/provers.mli#proof_of_property`<br>`middle_end/flambda2/types/provers.ml#prove_is_int` | — |
| `T.Prove.MeetShortcut` | normative | 08-meet-join.md | `middle_end/flambda2/types/provers.mli#meet_shortcut`<br>`middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates` | — |
| `T.Prove.GetTag` | normative | 08-meet-join.md | `middle_end/flambda2/types/provers.ml#prove_get_tag` | — |
| `T.Expand.Head` | normative | 08-meet-join.md | `middle_end/flambda2/types/expand_head.ml#expand_head`<br>`middle_end/flambda2/types/expand_head.ml#expand_head0` | — |
| `T.Reify.Sound` | normative | 08-meet-join.md | `middle_end/flambda2/types/reify.ml#reify` | — |
| `T.Reify.LiftLocalGuard` | normative | 08-meet-join.md | `middle_end/flambda2/types/reify.ml#reify`<br>`middle_end/flambda2/types/provers.ml#never_holds_locally_allocated_values` | — |

## S — Simplify structure, rewrites, inlining, unboxing (ch. 09-12)

94 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `S.Struct.Run` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/flambda2.ml#flambda_to_flambda0` | — |
| `S.Struct.Run.ClosedResult` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.Run.NoPendingConstants` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/simplify/lifting/lifted_constant_state.mli` | — |
| `S.Struct.SingleRound` | normative | 09-simplify-structure.md | `middle_end/flambda2/flambda2.ml#flambda_to_flambda0`<br>`middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.TwoPhase` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr`<br>`middle_end/flambda2/simplify/simplify_common.mli` | — |
| `S.Struct.Dacc` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/env/downwards_acc.ml#t`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#t` | — |
| `S.Struct.Uacc` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/env/upwards_acc.ml#t`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#create` | — |
| `S.Struct.TypesMonotoneDown` | conjectured | 09-simplify-structure.md | `middle_end/flambda2/simplify/env/downwards_env.ml#add_variable`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_equation_on_name` | — |
| `S.Struct.SetOfClosuresEager` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_expr.mli`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_non_lifted_set_of_closures`<br>`middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common` | — |
| `S.Struct.LetCont.BodyFirst` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_let_cont0`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body` | — |
| `S.Struct.ContUse` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use`<br>`middle_end/flambda2/simplify/env/continuation_uses_env.mli`<br>`middle_end/flambda2/simplify/env/one_continuation_use.ml` | — |
| `S.Struct.JoinParams` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/join_points.ml#join`<br>`middle_end/flambda2/types/flambda2_types.mli#cut_and_n_way_join` | — |
| `S.Struct.NoJoinUnknown` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types` | — |
| `S.Struct.SingleInlinableUse` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.Rec.NoFixpoint` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers` | — |
| `S.Struct.Rec.InvariantVsVariant` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/join_points.ml#compute_handler_env` | — |
| `S.Struct.ApplyContRewrite` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/apply_cont_rewrite.mli`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation`<br>`middle_end/flambda2/simplify/expr_builder.mli#rewrite_apply_cont` | — |
| `S.Struct.Turn` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common`<br>`middle_end/flambda2/simplify/flow/flow_analysis.mli#analyze`<br>`middle_end/flambda2/simplify/flow/flow_types.ml` | — |
| `S.Struct.Flow.RequiredNames` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/flow/flow_types.ml#Data_flow_result`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#required_names`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#reachable_code_ids` | — |
| `S.Struct.Flow.UnusedParams` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/flow/flow_analysis.mli`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive` | — |
| `S.Struct.Flow.Aliases` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/flow/flow_types.ml#Alias_result`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#extra_params_for_continuation_param_aliases`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler` | — |
| `S.Struct.Flow.MutableUnboxing` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/flow/mutable_unboxing.mli`<br>`middle_end/flambda2/simplify/flow/flow_types.ml#Mutable_unboxing_result`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Struct.Lift.Accumulate` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify`<br>`middle_end/flambda2/simplify/lifting/lifted_constant_state.mli`<br>`middle_end/flambda2/simplify/env/downwards_acc.ml#add_to_lifted_constant_accumulator` | — |
| `S.Struct.Lift.PlaceAtToplevel` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/simplify/expr_builder.mli#place_lifted_constants`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#at_unit_toplevel` | — |
| `S.Struct.Lift.EmptyAtEnd` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify.ml#run` | — |
| `S.Struct.InlineResimplify` | normative | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml` | — |
| `S.Struct.Resimplify` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common`<br>`middle_end/flambda2/simplify/env/upwards_acc.ml#set_resimplify`<br>`middle_end/flambda2/simplify/simplify_set_of_closures.ml` | — |
| `S.Struct.Loopify` | descriptive | 09-simplify-structure.md | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body`<br>`middle_end/flambda2/simplify/loopify_state.mli` | — |
| `S.Rewrite.Prim.Transfer` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0` | — |
| `S.Rewrite.Prim.ArgKindMismatch` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_primitive.ml#arg_kind_mismatch`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive` | — |
| `S.Rewrite.Prim.Relational` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_relational_primitive`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag` | — |
| `S.Rewrite.Alias.Canonicalize` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_simple.ml#simplify_simple0`<br>`middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn` | — |
| `S.Rewrite.Prim.ConstFold` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unary_primitive` | `14-validation/new-01-constfold.md`<br>`14-validation/new-05-inline-fold.md` |
| `S.Rewrite.Prim.ConstFold.Float` | descriptive | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen` | — |
| `S.Rewrite.Prim.ConstFold.PartialUndef` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like` | — |
| `S.Rewrite.Prim.Reify` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_named.ml#simplify_named0`<br>`middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify` | — |
| `S.Rewrite.Prim.IntIdentity` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift` | — |
| `S.Rewrite.Prim.FloatIdentity` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen` | — |
| `S.Rewrite.Prim.UntagTag` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number` | `14-validation/code_size_of_boolean_not_switch.md` |
| `S.Rewrite.Prim.Projection` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0`<br>`middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot`<br>`middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load` | — |
| `S.Rewrite.Prim.PhysEqual` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal`<br>`middle_end/flambda2/types/provers.ml#prove_physical_equality` | — |
| `S.Rewrite.Prim.CompareRecovery` | descriptive | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_binary_primitive.ml#recover_comparison_primitive`<br>`middle_end/flambda2/simplify/comparison_result.ml#convert_result_compared_to_tagged_zero` | — |
| `S.Rewrite.Prim.ObjDupElide` | descriptive | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_obj_dup` | — |
| `S.Rewrite.CSE.Eligible` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/terms/flambda_primitive.ml#Eligible_for_cse.create`<br>`middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse`<br>`middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse` | `14-validation/cse_immutable_array_load_var_index.md`<br>`14-validation/cse_immutable_array_load.md` |
| `S.Rewrite.CSE.Replace` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_primitive.ml#try_cse`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#apply_cse` | `14-validation/cse_immutable_array_load.md`<br>`14-validation/cse_immutable_array_load_var_index.md`<br>`14-validation/issue5721.md`<br>`14-validation/new-04-cse.md` |
| `S.Rewrite.CSE.Extend` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_primitive.ml#try_cse`<br>`middle_end/flambda2/simplify/common_subexpression_elimination.ml#T0.add` | `14-validation/new-04-cse.md` |
| `S.Rewrite.Switch.ArmPrune` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm` | `14-validation/array_element_kind_meet.md`<br>`14-validation/new-02-known-switch.md`<br>`14-validation/new-08-nested-switch.md` |
| `S.Rewrite.Switch.Merge` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch`<br>`middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_arm` | `14-validation/new-02-known-switch.md` |
| `S.Rewrite.Switch.Identity` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | `14-validation/n_way_join_null.md` |
| `S.Rewrite.Switch.BooleanNot` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | `14-validation/code_size_of_boolean_not_switch.md`<br>`14-validation/new-08-nested-switch.md` |
| `S.Rewrite.Switch.Invalid` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch` | — |
| `S.Rewrite.Let.DeadBinding` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let`<br>`middle_end/flambda2/terms/flambda_primitive.ml#at_most_generative_effects` | `14-validation/array_element_kind_meet.md` |
| `S.Rewrite.Let.DeadRegion` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Rewrite.Let.Phantom` | descriptive | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `S.Rewrite.Let.Invalid` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_expr.ml#simplify_let0`<br>`middle_end/flambda2/simplify/simplify_named.ml#simplify_named0` | — |
| `S.Rewrite.LetCont.Inline` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/join_points.ml#compute_handler_env`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation` | `14-validation/new-03-letcont-inline.md` |
| `S.Rewrite.LetCont.DeadHandler` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont` | — |
| `S.Rewrite.LetCont.Shortcut` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/continuation_shortcut.ml#apply`<br>`middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts` | — |
| `S.Rewrite.LetCont.UnusedParam` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive`<br>`middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze` | — |
| `S.Rewrite.LetCont.AliasedParam` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler` | — |
| `S.Rewrite.LetCont.InvalidHandler` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont` | — |
| `S.Rewrite.LetCont.Specialize` | descriptive | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#specialize_continuation_if_needed`<br>`middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont` | — |
| `S.Rewrite.Apply.IndirectToDirect` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_function_call`<br>`middle_end/flambda2/types/provers.ml#meet_single_closures_entry` | `14-validation/naked_immediates_many_relations.md` |
| `S.Rewrite.Apply.OverApplication` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_over_application`<br>`middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application` | — |
| `S.Rewrite.Apply.PartialApplication` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application` | — |
| `S.Rewrite.Apply.Invalid` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/simplify_apply_expr.ml#replace_apply_by_invalid`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call` | — |
| `S.Rewrite.Invalid.Propagate` | normative | 10-simplify-rewrites.md | `middle_end/flambda2/simplify/expr_builder.ml#rebuild_invalid`<br>`middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr` | — |
| `S.Inline.ModeMismatchInvalid` | normative | 11-inlining.md | `middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline` | — |
| `S.Inline.Substitute` | normative | 11-inlining.md | `middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body`<br>`middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application` | `14-validation/new-05-inline-fold.md` |
| `S.Inline.Substitute.Region` | normative | 11-inlining.md | `middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body` | — |
| `S.Inline.Substitute.ExnExtraArgs` | normative | 11-inlining.md | `middle_end/flambda2/simplify_shared/inlining_helpers.ml#wrap_inlined_body_for_exn_extra_args` | — |
| `S.Inline.DeclDecision` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify_shared/function_decl_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/terms/function_decl_inlining_decision_type.ml#behaviour` | `14-validation/code_size_of_single_arg_switch.md` |
| `S.Inline.Decision` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision`<br>`middle_end/flambda2/simplify_shared/call_site_inlining_decision_type.ml#can_inline` | `14-validation/missing_code.md` |
| `S.Inline.Speculative` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#might_inline`<br>`middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining`<br>`middle_end/flambda2/terms/cost_metrics.ml#evaluate` | `14-validation/inlining_cost_of_primitive_on_parameters.md`<br>`14-validation/removed_operations_of_switch.md`<br>`14-validation/speculative_inlining_lifted_constants.md` |
| `S.Inline.DepthLimit` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/terms/inlining_state.ml#is_depth_exceeded`<br>`middle_end/flambda2/simplify/simplify_rec_info_expr.ml#depth_may_exceed`<br>`middle_end/flambda2/simplify/env/downwards_env.ml#enter_inlined_apply` | — |
| `S.Inline.Unroll.Begin` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body`<br>`middle_end/flambda2/term_basics/coercion.ml#change_depth` | — |
| `S.Inline.Unroll.Continue` | descriptive | 11-inlining.md | `middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0`<br>`middle_end/flambda2/simplify/simplify_rec_info_expr.ml#known_remaining_unrolling_depth` | — |
| `S.Unbox.ContParam.Hook` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler`<br>`middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions` | — |
| `S.Unbox.Optimistic.Number` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision`<br>`middle_end/flambda2/simplify/unboxing/unboxers.ml` | `14-validation/new-07-float-unbox.md` |
| `S.Unbox.Optimistic.Block` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_fields` | — |
| `S.Unbox.Optimistic.Closure` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_vars_within_closure` | — |
| `S.Unbox.Depth` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`driver/oxcaml_flags.ml#Flambda2.Expert` | — |
| `S.Unbox.ExtraArg.Available` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg`<br>`middle_end/flambda2/simplify/unboxing/unboxers.ml` | — |
| `S.Unbox.ExtraArg.Project` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg` | — |
| `S.Unbox.ExtraArg.Invalid` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg` | — |
| `S.Unbox.Refine.Pass` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#refine_decision_based_on_arg_types_at_uses`<br>`middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_one_decision_and_use` | — |
| `S.Unbox.Beneficial` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#is_unboxing_beneficial_for_epa`<br>`middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#filter_non_beneficial_decisions` | — |
| `S.Unbox.Optimistic.Variant` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant` | — |
| `S.Unbox.Variant.Discriminator` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant`<br>`middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#extra_args_for_const_ctor_of_variant` | — |
| `S.Unbox.FunParam.Wrapper` | descriptive | 12-unboxing.md | `middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps_function`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#compute_body_of_unboxed_function`<br>`middle_end/flambda2/from_lambda/closure_conversion.ml#make_unboxed_function_wrapper` | — |
| `S.Unbox.Denv.Equation` | normative | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#denv_of_decision`<br>`middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#add_equation_on_var` | — |
| `S.Unbox.ContParam.Rewrite` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#add_extra_params_and_args`<br>`middle_end/flambda2/simplify/apply_cont_rewrite.ml#create`<br>`middle_end/flambda2/simplify/expr_builder.ml#rewrite_apply_cont`<br>`middle_end/flambda2/simplify/simplify_let_cont_expr.ml#compute_extra_params_and_args` | `14-validation/new-07-float-unbox.md` |
| `S.Unbox.Mutable.Candidate` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/flow/mutable_unboxing.ml#blocks_to_unbox`<br>`middle_end/flambda2/simplify/flow/mutable_unboxing.ml#escaping` | — |
| `S.Unbox.Mutable.Rewrite` | descriptive | 12-unboxing.md | `middle_end/flambda2/simplify/flow/mutable_unboxing.ml#Fold_prims.apply_prim`<br>`middle_end/flambda2/simplify/flow/mutable_unboxing.ml#compute_rewrites` | `14-validation/new-07-float-unbox.md` |

## INV — Global invariants and soundness (ch. 13)

4 rules.

| Rule ID | Status | Chapter | Code anchors | Verified |
|---|---|---|---|---|
| `INV.Simplify.Preserves` | conjectured | 13-soundness.md | `middle_end/flambda2/simplify/simplify.ml#run`<br>`middle_end/flambda2/flambda2.ml#flambda_to_flambda0` | — |
| `INV.Rewrite.Local` | conjectured | 13-soundness.md | `middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr`<br>`middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive`<br>`middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects` | — |
| `INV.NameMode.Coherent` | conjectured | 13-soundness.md | `middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms`<br>`middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let` | — |
| `INV.KindChecks.Gated` | descriptive | 13-soundness.md | `middle_end/flambda2/ui/flambda_features.ml#kind_checks`<br>`driver/oxcaml_args.ml`<br>`middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_apply_shared` | — |

## Consistency check results

_Generated 2026-07-13 by scanning chapters 02-13._

- **Total rules:** 304
- **By status:** normative 205, descriptive 65, conjectured 34
- **By chapter:**
  - 02-syntax.md: 13
  - 03-kinds.md: 26
  - 04-opsem.md: 29
  - 05-primitives-scalar.md: 30
  - 06-primitives-memory.md: 57
  - 07-types-domain.md: 27
  - 08-meet-join.md: 24
  - 09-simplify-structure.md: 28
  - 10-simplify-rewrites.md: 39
  - 11-inlining.md: 10
  - 12-unboxing.md: 17
  - 13-soundness.md: 4
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

No duplicate rule IDs across chapters 02-13. In particular the `P.Effects.*`
family is cleanly partitioned: chapter 05 defines `P.Effects.PureScalars`,
`P.Effects.FloatRoundingMode`, `P.Effects.BoxNumber`; chapter 06 defines the
remaining `P.Effects.*` rules. No collision between the two chapters.
