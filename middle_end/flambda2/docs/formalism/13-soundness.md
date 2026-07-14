# Soundness: what Simplify preserves

*Part of the Flambda 2 formalism; see [README.md](README.md).*

The preceding chapters define a term language ([`02`](02-syntax.md),
[`03`](03-kinds.md)), an operational semantics ([`04`](04-opsem.md),
[`05`](05-primitives-scalar.md), [`06`](06-primitives-memory.md)), an abstract
domain ([`07`](07-types-domain.md), [`08`](08-meet-join.md)), and the rewrites
Simplify performs ([`09`](09-simplify-structure.md)–[`12`](12-unboxing.md)).
This chapter states the property that ties them together: **Simplify preserves
observable behaviour**. The claim is not proved — Flambda 2 has no mechanized
correctness proof — but it is the design intent of every rule in the earlier
chapters, and it has been empirically validated against the case studies in
[`14-validation/`](14-validation/) (§5). We state it precisely, decompose it
into the obligation each rewrite carries, list the global invariants Simplify's
output must satisfy, and — importantly for a descriptive formalism — record the
places where this document or its companions are known to be inaccurate (§4).

## 1. The soundness claim

Recall the observable behaviour of a run from [§04 §8.2](04-opsem.md)
(`OS.Unit.Final`). A run of a unit `U` from an initial configuration is one of:

- **normal termination**, observing the final module block value `H(sym_mod)`
  and the trace of external effects performed by C calls;
- **termination by uncaught exception**;
- **divergence** (an infinite `⟶` sequence);
- **undefined behaviour**, i.e. reaching `Invalid` (`OS.Invalid`), a missing
  switch arm (`OS.Switch.Undef`), or any primitive application whose denotation
  is `undef` (`⟦p⟧ = undef`; chapters [`05`](05-primitives-scalar.md),
  [`06`](06-primitives-memory.md)) — a stuck or wild state.

Two terms are **observationally equivalent** when, started from the same heap,
they induce the same observations: the same C-call effect trace and the same
termination outcome (including the same final module block value at `sym_mod`).
Alpha-renaming of bound variables/continuations and the insertion or removal of
coercions (`Coercion.t`) are *not* observable: coercions are erased before
`to_cmm` and stand for identity-at-runtime retagging of names ([§02](02-syntax.md)).

```rule
RULE INV.Simplify.Preserves
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify.ml#run
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
---
U : Flambda_unit.t,  Γ ⊢ U ok            -- U well-formed (chapters 02, 03)
Simplify(U) = U′                         -- run terminates with result U′
U does not exhibit undefined behaviour   -- no reachable ⟦p⟧ = undef, no missing
                                            switch arm, no reachable Invalid
--------------------------------------------------
U and U′ are observationally equivalent (§1): from every starting heap they
produce the same C-call effect trace and the same termination outcome, including
the same final module block value at sym_mod.
NOTES: Claimed and empirically validated (§5), not proved; hence STATUS
conjectured. This is the single property the whole formalism exists to make
precise. The "modulo undefined behaviour" hypothesis is essential and is
discussed below; it is not a weakness peculiar to Flambda 2 but the standard
shape of an optimizing-compiler correctness statement for a language with
undefined behaviour.
```

### The "modulo undefined behaviour" clause

Simplify is permitted to change the behaviour of a term that *already* had
undefined behaviour. The clearest instance is `Invalid`. When the abstract
domain proves a program point unreachable — a type-incorrect operation, a
`Switch` whose scrutinee misses every arm, an out-of-bounds read of a known
immutable array, a call whose arity cannot match — Simplify replaces the code at
that point with `Invalid` (`S.Rewrite.Invalid.Propagate`, and the more specific
`S.Rewrite.Switch.Invalid`, `S.Rewrite.Let.Invalid`, `S.Rewrite.Apply.Invalid`).
Operationally `Invalid` is a stuck state with no transition (`OS.Invalid`): the
semantics permits *any* behaviour if it is reached.

Worked example. Consider

```
match (x : bool) with true -> a | false -> b
```

compiled to a `Switch` on the untagged scrutinee with arms `{ 0 → …; 1 → … }`.
If the domain has proved `x : {1}` (say, from an enclosing test), the `0` arm is
unreachable: `{1} ⊓ {0} = ⊥`, so `S.Rewrite.Switch.ArmPrune` deletes it. If
*every* arm is pruned the whole `Switch` becomes `Invalid`
(`S.Rewrite.Switch.Invalid`). This is sound precisely *because* the type claim
`x : {1}` is correct: a well-typed run can never take the deleted arm, so
deleting it changes no reachable behaviour. If the type claim were *wrong* — if
some reachable run really had `x = 0` — then the original term had a defined
behaviour that `U′` no longer reproduces, and soundness would be violated. But a
wrong type claim is itself a bug in the domain (a violation of `T.Gamma.*`,
`T.Meet.Sound`, `T.Join.Sound`, or a prover), i.e. a compiler bug — not a
counterexample to the "modulo undef" statement. The validation campaign (§5),
including the GADT / refined-scrutinee cases
([`gadt_simplified_switch.md`](14-validation/gadt_simplified_switch.md),
[`new-08-nested-switch.md`](14-validation/new-08-nested-switch.md)), found no
such wrong type claim.

The same reasoning covers the other undefined-behaviour sources: constant
folding a partial operation (`x / 0`) is only performed when the domain does not
prove the operation is reached with the undefined argument
(`S.Rewrite.Prim.ConstFold.PartialUndef`), and a `Switch` with a scrutinee
outside its arms is already `undef` in the source semantics (`OS.Switch.Undef`).

## 2. Per-rewrite soundness obligations

The global claim of §1 factors through a *local* obligation on every rewrite.
Each rule of the `E ⊢ e ⇝ e′` judgment (all of `S.Rewrite.*`, `S.Inline.*`,
`S.Unbox.*`) must preserve the machine semantics *in any well-formed context*,
given its side conditions and the typing environment `E` it fires under. Because
Simplify's rewrites are applied compositionally as the traversal rebuilds the
term ([§09](09-simplify-structure.md)), local preservation under an arbitrary
context is exactly what composes up to the whole-unit statement `INV.Simplify.Preserves`.

```rule
RULE INV.Rewrite.Local
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr
CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
---
For every rewrite E ⊢ e ⇝ e′ (a rule in S.Rewrite.*, S.Inline.*, S.Unbox.*):
whenever E is a sound abstraction of the runtime state at e (every equation in E
holds of the actual values, per T.Gamma.*) and the rule's side conditions hold,
then for every context C[·] and heap such that C[e] is reachable and well-formed,
C[e] and C[e′] are observationally equivalent.
--------------------------------------------------
INV.Simplify.Preserves follows by composing the local obligations along the
traversal that rebuilds the term.
NOTES: Two coupling points carry the weight of this obligation and are called
out because they are where the term semantics and the abstract domain must
agree:
(i)  Constant folding (S.Rewrite.Prim.ConstFold, and the arithmetic identity
     rules S.Rewrite.Prim.IntIdentity / FloatIdentity / UntagTag) is sound only
     if the folded result equals the primitive's denotation ⟦p⟧ from chapters
     05/06. Every case study that folds (e.g. new-01-constfold) checks this
     equality against the real backend, not just against this document.
(ii) Type-based side conditions (arm pruning, indirect-to-direct, CSE via the
     alias/typing environment, unboxing) are sound only if the domain is a sound
     abstraction of the concrete semantics: T.Gamma.* (concretization),
     T.Meet.Sound, T.Join.Sound, and prover soundness T.Prove.Sound. A rewrite
     inherits the soundness of the prover query it fires on.
```

The two coupling points are worth stating plainly because they are the seams a
reviewer should distrust first:

- **Denotational agreement.** `S.Rewrite.Prim.ConstFold` asserts, for the cases
  it fires, that the folded constant equals `⟦p⟧` as defined in chapters
  [`05`](05-primitives-scalar.md)/[`06`](06-primitives-memory.md). If those
  chapters' denotations disagree with the backend, folding is unsound. §4 records
  one concern of this shape (`min_int / -1`) and its refutation.
- **Domain soundness.** Every type-driven rewrite is only as sound as the
  `γ`-soundness of the domain (`T.Gamma.*`) and the soundness of meet, join and
  the provers (`T.Meet.Sound`, `T.Join.Sound`, `T.Prove.Sound`). These are the
  normative rules the rewrite chapters lean on; a bug in any of them is a
  soundness bug even if every rewrite rule is "locally correct" relative to it.

## 3. Global invariants of the result

`Simplify(U) = U′` is not only observationally equivalent to `U`; the result
`U′` also satisfies structural invariants that the pipeline downstream (Reaper,
`to_cmm`) relies on. The genuinely global ones are stated as normative rules in
chapter [`09`](09-simplify-structure.md); we reference them here rather than
restate them:

- **Closedness.** `U′` has no free variables or continuations except the unit's
  parameters and imported symbols/code IDs — `S.Struct.Run.ClosedResult`.
- **No pending lifted constants.** Every constant lifted during traversal has
  been placed at the toplevel by the time `run` returns; the lifted-constant
  accumulator is empty — `S.Struct.Run.NoPendingConstants`,
  `S.Struct.Lift.EmptyAtEnd`, `S.Struct.Lift.PlaceAtToplevel`.
- **Data-flow / required-names consistency.** The set of required names and
  reachable code IDs computed by flow analysis is consistent with the rebuilt
  term: names deleted as unused really are unused, and every name kept is
  reachable — `S.Struct.Flow.RequiredNames`, `S.Struct.Flow.UnusedParams`.

Two invariants are stated here because they have no dedicated chapter-09 rule.

```rule
RULE INV.NameMode.Coherent
STATUS conjectured
CODE middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
---
In U′, every name occurring in a term position (a Simple in a Named, an
Apply/Apply_cont argument, a Switch scrutinee) is bound with a name mode for
which can_be_in_terms holds (Normal). Names demoted to Phantom during
simplification (S.Rewrite.Let.Phantom) occur only in debugging information, never
in term positions; In_types names never occur in terms.
--------------------------------------------------
A well-formed simplified unit never references a Phantom or In_types name from a
term, so lowering to Cmm (which materializes only term-position names) is total.
NOTES: This is the run-time counterpart of WF.Syntax.NameModeInTerms for the
*output* of Simplify; rebuild_let is where bindings are dropped or demoted to
Phantom based on whether the bound var is still required in terms.
```

```rule
RULE INV.KindChecks.Gated
STATUS descriptive
CODE middle_end/flambda2/ui/flambda_features.ml#kind_checks
CODE driver/oxcaml_args.ml
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_apply_shared
---
The kind/arity consistency checks of chapter 03 (WF.Check.Gated) are OFF by
default: flambda_features.kind_checks defaults to false.
--------------------------------------------------
When kind checks are disabled, a kind or arity mismatch that WF.* would reject is
not a fatal error at simplify time; instead the offending Apply/Switch/primitive
is replaced by Invalid (S.Rewrite.Apply.Invalid, WF.Apply.DirectArity, …) and the
"modulo undefined behaviour" clause of INV.Simplify.Preserves covers it.
NOTES: See WF.Check.Gated. Enabling -flambda2-kind-checks turns the same
mismatches into hard errors, which is a debugging aid, not a semantic change to
correct programs.
```

## 4. Known discrepancies and stale documentation

A descriptive formalism is only useful if it says where it — or the companion
prose — is known to be wrong. Each item below is recorded with an evidence
pointer so an agent can re-check it after code changes. Grep this section when a
claim elsewhere seems too clean.

1. **`../types.md` is stale on relational storage.** It states (docs/types.md,
   "Relational domains and reduction": "The equation is currently stored in one
   way only … but it would be possible to store the relation at both points")
   that the `Is_int`/`Get_tag` relation between a naked immediate and its
   block/variant is stored one-way. The current `type_grammar.ml` stores **both
   directions**: the `Variant` head carries forward `is_int` / `get_tag`
   `Variable.t option` fields
   (`middle_end/flambda2/types/grammar/type_grammar.ml`, `head_of_kind_value_non_null`),
   and the naked-immediate head carries `Inverse_relations` /
   `Naked_immediates_and_inverse_relations`
   (same file, `head_of_kind_naked_immediate`). See chapter [`07` §2.4](07-types-domain.md)
   / `T.Grammar.NakedImmediate.Relational`. The two-way storage is what makes
   [`naked_immediates_many_relations.md`](14-validation/naked_immediates_many_relations.md)
   and [`issue5721.md`](14-validation/issue5721.md) resolve both calls in a branch.

2. **`-flambda2-meet-algorithm` is a parsed no-op.** `driver/oxcaml_args.ml`
   accepts `-flambda2-meet-algorithm=basic|advanced` but its handler validates
   the string and then does nothing (the match arm returns `()`), whereas
   `-flambda2-join-algorithm=binary|n-way|checked` actually calls
   `Oxcaml_options_impl.flambda2_join_algorithm`. Meet-vs-join algorithm
   selection is entirely through the *join* flag. See chapter [`08`](08-meet-join.md)
   / `T.Meet.Dispatch`. (The separate `-flambda2-basic-meet` flag is documented
   in its own `mk_*` help text as "deprecated, does nothing".)

3. **Meet is not a greatest lower bound in general.** Meeting a `Variant` type
   against a `Mutable_block` type returns the `Mutable_block` input and **drops
   the `Row_like` equations on the known immutable fields**
   (`middle_end/flambda2/types/meet_and_join.ml`, the `Variant … , Mutable_block`
   case near line 1017, with an explanatory `CR` comment and a worked
   `type r = { a : int; mutable b : int }` example). This is a deliberate,
   documented precision loss: the result is a *sound* lower bound but not the
   *greatest* one, so two immutable loads that would be shared through a
   `Variant` type are not shared through the more-precise `Mutable_block` type.
   Soundness is unaffected — a smaller (less precise) type is still a valid
   over-approximation. See `T.Meet.GreatestLowerBound` (STATUS conjectured; its
   NOTES record this counterexample).

4. **CSE folklore "projections/loads are never CSE'd" is wrong for immutable
   array loads.** Earlier prose claimed the per-primitive CSE-eligibility
   predicate excludes all loads, with load information carried only through
   type-based projection propagation (`S.Rewrite.Prim.Projection`). In fact
   `binary_primitive_eligible_for_cse`
   (`middle_end/flambda2/terms/flambda_primitive.ml`) returns `true` for
   `Array_load (_, _, (Immutable | Immutable_unique))`, and immutable array reads
   are `(No_effects, No_coeffects)`, so they *are* CSE-eligible. This was caught
   by validation:
   [`cse_immutable_array_load_var_index.md`](14-validation/cse_immutable_array_load_var_index.md)
   is the sharp witness (a *variable* index that type-propagation cannot handle,
   yet the two loads are still deduplicated — proving the mechanism is genuine
   CSE), with [`cse_immutable_array_load.md`](14-validation/cse_immutable_array_load.md)
   the companion. The rule `S.Rewrite.CSE.Eligible` was corrected accordingly
   (block loads, closure/value-slot projections, and mutable/string/bigarray
   loads excluded; immutable array loads and immutable header reads
   length/tag/is_int eligible).

5. **REFUTED concern (`min_int / -1` and `min_int mod -1`), recorded as a
   positive finding.** A natural worry is that constant-folding `min_int / -1`
   (which overflows) or `min_int mod -1` in Simplify might disagree with the
   generated code. It does not: `backend/cmm_helpers.ml` (`make_safe_divmod` and
   the surrounding `Cdivi`/`Cmodi` lowering, PR#5513) special-cases a divisor of
   `-1`, forcing `x / -1 = -x` and `x mod -1 = 0`, which is exactly what the
   `Int_ops_for_binary_arith` folding in Simplify computes. So there is **no**
   soundness gap here; the coupling point (i) of `INV.Rewrite.Local` holds for
   these cases. See `S.Rewrite.Prim.ConstFold.PartialUndef`.

6. **Two chapter-01 claims are cited to prose docs.** Chapter [`01`](01-overview.md)
   describes the Reaper and `to_cmm` as *context only*, citing the companion
   prose:
   - The **Reaper** description is cited to `docs/reaper.md` and has **not** been
     re-verified against `Flambda2_reaper.Reaper.run` in this campaign; treat it
     as unverified context.
   - The **`to_cmm` "inlines non-recursive continuations used exactly once"**
     claim is now **VERIFIED against source**:
     `to_cmm/to_cmm_effects.ml#classify_continuation_handler` returns `May_inline`
     exactly when `cont_is_known_to_have_exactly_one_occurrence` holds and the
     handler is not an exn handler, not cold, and not applied with traps;
     `to_cmm/to_cmm_expr.ml#let_cont_inlined` (via `Env.add_inline_cont`) then
     inlines it at its unique use site. The chapter-01 wording is accurate.

7. **OPEN soundness bug: int→float32 constant folding double-rounds.** Folding a
   `Num_conv` into `Naked_float32`
   (`middle_end/flambda2/simplify/number_adjuncts.ml`, the four `to_naked_float32`
   functions: `For_int64s` / `For_nativeints` / `For_tagged_immediates` /
   `For_naked_immediates`) computes `Float32_by_bit_pattern.create (X.to_float t)`
   = `Int32.bits_of_float (Int64.to_float t)`, i.e. int → `float64` → `float32`, a
   **double rounding**. The generated code single-rounds via `cvtsi2ss`
   (`backend/cmm_helpers.ml#Scalar_type.static_cast`, `Integral→Float`;
   `backend/amd64/emit.ml`, `Float_of_int Float32`). The fold is not gated on
   `-flambda2-float-const-prop` (`simplify/simplify_unary_primitive.ml#Make_simplify_int_conv`),
   so it fires by default. Witness: `Float32.of_int 9007199791611905` folds to
   float32 bits `0x5a000000` but computes `0x5a000001` at run time — an observable
   miscompilation of a conversion that has no undefined behaviour, so it violates
   coupling point (i) of `INV.Rewrite.Local` (`P.Unary.NumConv` as implemented is
   unsound). Confined to the `Naked_float32` destination with sources wider than
   `float64`'s exact range; `Naked_float` and `int32`→`float32` are exact. This is
   the live form of Open Question #3 in
   [`05-primitives-scalar.md`](05-primitives-scalar.md); it is a **code** bug (the
   fix is to single-round in `to_naked_float32`), not a documentation error. See
   [`14-validation/float32_double_round.md`](14-validation/float32_double_round.md).
   The `to_cmm` Stage-2 model localizes this precisely: `to_cmm` emits a single
   `Cstatic_cast (Float_of_int Float32)` (one rounding;
   [`18-to-cmm-data.md`](18-to-cmm-data.md) `TC.Prim.NumConv`,
   [`20-to-cmm-soundness.md`](20-to-cmm-soundness.md) §5), so the double rounding is
   unambiguously in the Simplify constant fold, not the lowering.

## 5. Validation summary

The formalism is validated by *prediction*, not by post-hoc reading. The
protocol for each case study in [`14-validation/`](14-validation/) is:

1. Take a test input (an existing `testsuite/tests/flambda2` test, or a
   synthesized program).
2. **Before** looking at the actual Simplify output, predict it, citing the
   rule IDs that should fire and the reasoning.
3. Read the actual output (`.simplify.reference` / `-dfexpr` dump / expect
   block).
4. Record a verdict: MATCH, PARTIAL, or MISMATCH, plus a diagnosis.

The mismatch protocol is strict: **a MISMATCH or PARTIAL is resolved by fixing
the formalism, never by silently editing the prediction to match.** When a case
study contradicts a rule, the rule is corrected and the correction is noted in
the case study and here in §4.

There are **33 case studies**:

- **14 from existing testsuite tests** — verdicts: **12 MATCH**, **1 PARTIAL**
  ([`cse_immutable_array_load.md`](14-validation/cse_immutable_array_load.md):
  right outcome, wrong mechanism attribution), **1 MISMATCH now fixed**
  ([`cse_immutable_array_load_var_index.md`](14-validation/cse_immutable_array_load_var_index.md):
  drove the `S.Rewrite.CSE.Eligible` correction of §4 item 4).
- **8 synthesized** (`new-01`…`new-08`) — verdicts: **8/8 MATCH**, covering
  integer constant folding, known-scrutinee switch pruning, single-use
  continuation inlining, CSE of a pure primitive, inline-then-fold, trap-action
  preservation around an opaque call, float-accumulator unboxing across a loop,
  and refined-scrutinee arm pruning (which additionally exposed a chained
  `BooleanNot` rewrite).
- **4 mixed-block** (`mixed-01`…`mixed-04`) — verdicts: **4/4 MATCH**, covering
  construction, load, mutable set and join of mixed blocks.
- **1 soundness-bug witness** (`float32_double_round`) — verdict: **MISMATCH**,
  an open compiler soundness bug (int→float32 constant folding double-rounds; §4
  item 7), demonstrated by a runtime A/B where the folded and run-time results of
  the same conversion differ.
- **6 loopification** (`loopify-01`…`loopify-06`) — verdicts: **6/6 MATCH**,
  covering the `S.Rewrite.Loopify.*` chain end to end: an escaping
  purely-tail-recursive function surviving only as a continuation-loop wrapper,
  a local one being inlined away entirely, the negative cases (non-tail and
  mutual recursion untouched), and the two boundary cases (`[@loop]` with no
  self tail call collapsing back via `S.Rewrite.LetCont.Demote`, and a
  provably-dead loop leaving a non-recursive residue).

The CSE MISMATCH was not a soundness failure — the compiler was correct and the
*document* was wrong — but it is exactly the kind of finding the prediction-first
protocol is designed to surface: a rule (`S.Rewrite.CSE.Eligible`) that read
plausibly but did not match the code. The second MISMATCH
(`float32_double_round`) is the opposite: the *compiler* is wrong (int→float32
folding double-rounds; §4 item 7), an open soundness bug the same protocol
surfaced. The remaining case studies confirmed,
among other things, the cost-model constants of chapter [`11`](11-inlining.md)
(`inlining_cost_of_primitive_on_parameters`), the nullability handling of the
n-way join (`n_way_join_null`, `n_way_join_preserves_null`), and the
two-way relational storage of §4 item 1 (`naked_immediates_many_relations`,
`issue5721`).

See [`14-validation/README.md`](14-validation/README.md) for the full index of
case studies with their source tests, verdicts, and primary rules.
