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

### Dead-code, flow-closure, and region invariants

The following invariants are whole-body (or whole-unit) properties of Simplify's
output that no local rewrite establishes; each is the composition of a flow
analysis with the traversal, and several are non-local across subsystems. They
underwrite the dead-code discipline `to_cmm` and the Reaper rely on.

```rule
RULE INV.Simplify.EffectfulDeletionInventory
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
CODE middle_end/flambda2/terms/flambda_primitive.ml#is_end_region
CODE middle_end/flambda2/simplify/named_rewrite.mli#Prim_rewrite
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#make_result
---
e ⇝* e′ over a whole Simplify run; p a primitive application in e with
  effects = Arbitrary_effects, in reachable code; p absent from e′
--------------------------------------------------
p is one of exactly two shapes, each licensed by a whole-body flow analysis:
(a) End_region — and ONLY End_region, never End_try_region — on a region variable
    ∉ required_names (S.Rewrite.Let.DeadRegion; gate is_end_region_for_unused_region
    in rebuild_let), licensed by region liveness (uses of regions in End_region are
    not counted as uses: flow_acc.record_let_binding). is_end_region matches only
    Unary (End_region _, _); for End_try_region must_be_kept_for_its_effects is
    always true; or
(b) Block_set deleted (Remove_prim) or invalidated by mutable unboxing's
    let_rewrites, licensed by the escape analysis proving the block non-escaping
    (mutable_unboxing.ml; let_rewrites touch only Is_int/Get_tag/Make_block/
    Block_load/Block_set, of which only Block_set is Arbitrary_effects).
Every other deletion path requires Named.at_most_generative_effects.
NOTES: A global quantification: no purely local rule of ch10 deletes an effect; the
two effect-deleting rewrites are each justified by a non-local analysis (region
liveness; escape). Scope: Simplify only; covers PRIMITIVE applications — effectful
Apply exprs (extcalls) are simplify_extcall's type-licensed specialization,
outside it; trap push/pop removal via exn-handler demotion (S.Rewrite.LetCont.DemoteExn)
is control structure, not a primitive. Composes: S.Rewrite.Let.DeadRegion,
INV.Simplify.RegionPairAtomic, S.Unbox.Mutable.Rewrite.
```

```rule
RULE INV.Simplify.RegionPairAtomic
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/flow_acc.ml#record_let_binding
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
CODE middle_end/flambda2/terms/flambda_primitive.ml#is_end_region
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region
---
let ρ = Begin_region in a body B, with End_region ρ at each of its n ≥ 1 exits
  (ordinary regions; NOT Begin_try_region/End_try_region)
--------------------------------------------------
After one Simplify pass over B, exactly one of:
(i)  ρ ∈ required_names: the Begin_region binding and ALL n End_region bindings
     survive (is_end_region_for_used_region forces each End kept);
(ii) ρ ∉ required_names: ALL n End_region bindings are deleted
     (S.Rewrite.Let.DeadRegion), whereupon Begin reaches zero occurrences and is
     deleted too (eligible since Begin_region is Only_generative Mutable).
Never a mixed outcome. Moreover ρ ∈ required_names iff some (conditionally) live
non-End use of ρ exists, because flow_acc.record_let_binding explicitly does NOT
count End_region's use of ρ as a use.
NOTES: The seed pattern realized: the Begin's only surviving uses are the Ends, and
each End's deletion is licensed by the same global fact ρ ∉ required_names. The
non-counting of End uses is the deliberate coinductive cut breaking the circularity
by which the pair would keep itself alive (flow_acc.ml:360-362). For an unused
region will_delete_binding is TRUE unconditionally; sound because simplify_end_region
PINS the End's result to tagged 0 (S.Rewrite.Let.DeadRegion). End_try_region is
excluded twice over (is_end_region and the flow-acc use-skip both match only
End_region); try-region deletion belongs to exn-handler demotion. Region simples are
necessarily variables (no region constants), so is_end_region's non-Variable fatal
is unreachable. One-round lag if the last allocation into ρ dies only upwards.
Composes: S.Rewrite.Let.DeadRegion, INV.Simplify.RequiredNamesSound,
INV.Simplify.EffectfulDeletionInventory.
```

```rule
RULE INV.Simplify.RequiredNamesSound
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
CODE middle_end/flambda2/simplify/flow/dominator_graph.ml#create
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#filter_and_choose_alias
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#find_cse_simple
---
R = required_names as returned by Flow.Analysis.analyze (after the union of mutable
  unboxing's additional_epa params); e′ = the rebuilt term for the analyzed body
--------------------------------------------------
free_variables(e′) at normal name mode ⊆ R (plus the exn-bucket exception: an exn
handler's first param counts as used at zero occurrences). Splits in two:
(a) downwards completeness: every occurrence in the INPUT is recorded in flow_acc
    unconditionally or conditioned on a name that survives only if in R (EPAs are
    registered wholesale per handler before the turn; New_let_binding args carry
    their prim free names into conditional edges — so rewrite_apply_cont's
    materialization is downwards completeness, not an upwards introducer);
(b) upwards stability: every upwards occurrence-INTRODUCER draws only from names
    already forced into R. Inventory (FIVE): (i) aliased-param lets_to_introduce and
    extra args (dominator graph restricted to R by construction); (ii)
    mutable-unboxing additional_epa (unioned into R wholesale by analyze); (iii)
    mutable-unboxing Replace_by_binding (dominator representatives, R-restricted);
    (iv) Switch.Merge argument selection (filter_and_choose_alias from R-filtered
    alias sets); (v) the lookup-table/single-arg switch find_cse_simple (same R
    filter).
Hence R remains an over-approximation of actual free names at every point of the
upwards rebuild — the precondition making the two runtime checks unfailing (the
fatal in decide_param_usage_non_recursive; the fatal in rebuild_let).
NOTES: Quantifies over every upwards rewrite in chapters 09-12; its content is the
exhaustive enumeration of occurrence-introducers (the code comment at
decide_param_usage_non_recursive states this as an UNCHECKED precondition). Freshly
BOUND upwards variables (rebuild_switch's tagged_scrutinee) are exempt (not free in
e′). Bonus lemma: the R-filter in (iv) never REFUSES a surviving arg — a surviving
arg position belongs to a Used param, forcing the arg into R — so merge-refusal on
that ground is dead code; the filter only chooses among aliases. Composes:
S.Struct.Flow.RequiredNames, S.Struct.ApplyContRewrite, S.Struct.Flow.DeadLoopParam.
```

```rule
RULE INV.Simplify.DeadCodeBodyLocal
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#record_free_names_of_apply_as_used
CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
---
one Simplify pass; two distinct function bodies B_caller and B_callee related only
by a surviving (non-inlined) Apply
--------------------------------------------------
Liveness-based deletion never crosses the Apply, in either direction:
(1) caller→callee: Simplify never deletes/reorders a Code binding's parameters —
    simplify_function_body returns the very `params` it received. An argument
    computation is kept even if the callee ignores that parameter (the Apply's free
    names are unconditionally used: record_free_names_of_apply_as_used);
(2) callee→caller: a value feeding B_callee's return continuation is kept even if
    every caller discards the result (args to the body's own return/exn continuation
    are unconditionally used).
The ONLY channels by which cross-body facts influence liveness-based deletion are
exactly three: (a) inlining (merges the bodies into one flow domain); (b)
used_value_slots (whole-unit, INV.Simplify.DeadValueSlotCoherence); (c)
reachable_code_ids at unit toplevel (whole-Code deletion).
NOTES: Flambda 2's dead-code elimination is intraprocedural plus
closure-slot-global plus whole-code-granular, with inlining the sole fine-grained
interprocedural lever. Forced by S.Struct.SetOfClosuresEager and S.Struct.Turn
(analyze is per-function-body). Distinguish liveness from TYPES: cross-body type
facts (code metadata, return types) can trigger folding/Invalid in the other body
but never a liveness-based deletion. Corollary witnessed live: a dead accumulating
parameter of a non-inlined, non-loopified recursive FUNCTION provably survives
Simplify — contrast the loopified self-continuation case (S.Struct.Flow.DeadLoopParam).
Composes: INV.Simplify.DeadValueSlotCoherence, S.Struct.SetOfClosuresEager.
```

```rule
RULE INV.Simplify.LiftedConstGranularity
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/flow_acc.ml#normalize_lifted_constant_aux
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#keep_lifted_constant_only_if_used
CODE middle_end/flambda2/simplify/expr_builder.ml#create_let_symbol0
CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#reachable_code_ids
---
L = a lifted constant (one "let symbol" group, possibly mutually recursive) binding
  symbols s₁ … sₙ and code IDs c₁ … cₘ, floated to unit toplevel
--------------------------------------------------
Dead-code elimination on L operates at exactly TWO granularities:
(1) SYMBOLS: all-or-nothing per group. Flow side: every definition's deps include
    being_defined = {s₁ … sₙ} (normalize_lifted_constant_aux), so any reachable sᵢ
    makes every sibling required. Keep side: placed iff ANY bound symbol ∈
    required_names or any bound code is (ancestor-)reachable
    (keep_lifted_constant_only_if_used) — never split;
(2) CODE: per-definition. Code IDs are EXCLUDED from being_defined; their liveness
    is the separate reachable_code_ids closure. Within a KEPT group, each code ID ∉
    live_code_ids is emitted as Deleted_code (create_let_symbol0).
Consequence: referencing ONE symbol of a group retains every sibling symbol, their
closure blocks, AND the real code of every function slot in those blocks —
record_lifted_function_slot_aux unions the set's function_decls free names (all
slots' code IDs) into every closure symbol's deps (semantically forced: the closure
block embeds every code ptr). Deleted_code strikes exactly code ids unreachable
through the closure-block/function_decls edges — in practice newer_version_of
ancestors.
NOTES: Non-local twice: symbol liveness is a whole-unit reachability fact, and the
two granularities are implemented in three files whose agreement is the invariant.
The per-definition CODE granularity's bite is narrow: the closure-block edge (the
function_decls flow channel) keeps all slot code of a live group; only
version-chain-only code is deleted in practice. Caveat: with reachable_code_ids
Unknown (non-toplevel, or speculative), the code side degrades to binds_code (keep
everything). Composes: S.Struct.Lift.PlaceAtToplevel, S.Struct.Flow.RequiredNames.
```

```rule
RULE INV.Simplify.DeadValueSlotCoherence
STATUS conjectured
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_use_of_value_slot
CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
CODE middle_end/flambda2/simplify/expr_builder.ml#remove_unused_value_slots
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#value_slot_is_used
CODE middle_end/flambda2/cmx/exported_code.ml#prepare_for_export
---
U = dacc.used_value_slots: the value slots projected anywhere in the WHOLE unit
  (accumulated across every function body and the toplevel; never save/restored).
  Recorded by simplify_project_value_slot (DA.add_use_of_value_slot);
w = a value slot defined by this unit with w ∉ U and Value_slot.is_imported w = false
--------------------------------------------------
(1) Dead-capture elimination, whole-unit and non-local: at the toplevel flow
    analysis the capture edge closure_name → captured_contents(w) is gated on
    is_value_slot_used (add_continuation_info), so a variable captured ONLY in such
    slots leaves required_names and its binding is deleted — even when the closure
    itself is live, escaping, or exported. The fate of a binding at the top of the
    unit depends on the absence of projections at the bottom.
(2) FIVE consumers prune with the SAME set U and must agree: (i) flow-graph capture
    edges; (ii) term — STATIC sets of closures drop slot w
    (expr_builder.remove_unused_value_slots, only from create_let_symbols); DYNAMIC
    sets keep flambda-level slots, pruned only at (iii); (iii) layout — slot_offsets
    omits w (value_slot_is_used), covering dynamic sets at to_cmm; (iv) cmx —
    exported result types drop w (prepare_for_export → remove_unused_value_slots);
    (v) exported CODE bodies pruned by the same set (the to_cmm side,
    INV.ToCmm.SlotLiveness). Cross-module: another unit learns of w only through our
    exported types (iv)/code (v), pruned consistently with the layout (iii); slots
    of other units are protected everywhere by is_imported.
--------------------------------------------------
This is the Simplify SIDE of a MANDATORY merge with INV.ToCmm.SlotLiveness
([§20](20-to-cmm-soundness.md)), the to_cmm side; they are two faces of ONE pruning
event. There are THREE distinct value-slot accumulators: (1) build_run_result's
final-term free_names (→ offsets + cmx type/code pruning); (2) the DOWNWARDS
DA.add_use_of_value_slot → expr_builder.remove_unused_value_slots, pruning STATIC set
captures only (imported slots exempt, value_slot_is_used_or_imported); (3) the
UPWARDS snapshot barrier (cannot record).
NOTES: DA.add_use_of_value_slot has a SOLE caller — simplify_project_value_slot's
Need_meet branch. A projection resolved Known_result (the S.Rewrite.Prim.Projection
fast path) deliberately does NOT record: the folded projection leaves no runtime
load. This is the SURVIVAL ⇒ RECORDED lemma (premise P1 of INV.ToCmm.SlotLiveness),
and it is ACCIDENTAL, not structural: unboxing's unbox_arg materializes a
Project_value_slot extra-arg without recording into accumulator (1), and accumulator
(3) cannot record — coherence holds only by the alignment survival ⇒ extra param
Used ⇒ in-handler projection folded onto it ⇒ slot recorded via an in-unit Need_meet
projection (imported slots exempt). If that alignment breaks, a REACHABLE projection
gets a Dead offset entry ⇒ Cinvalid EXECUTED at runtime. A defensive
DA.add_use_of_value_slot in unbox_arg would make it structural (see
consolidation-code-hygiene.md). The second merge premise (P2, Exported_code inlinable
bodies ⊆ final-term code bindings) lives on the to_cmm side. Composes:
INV.ToCmm.SlotLiveness, S.Rewrite.Prim.Projection, S.Struct.Flow.DeadLoopParam
(blocker 4).
```

```rule
RULE INV.Simplify.AliasesMonotoneDown
STATUS conjectured
CODE middle_end/flambda2/types/env/aliases.mli#add
CODE middle_end/flambda2/types/env/aliases.ml#add
CODE middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env
CODE middle_end/flambda2/types/env/binding_time.ml#consts
---
E₀ → E₁ → ⋯ → Eₙ is a straight-line descent lineage: each step is one of the
S.Struct.EnvRefineOnly operations (definitions, add_equation / add_env_extension,
code-age extension), excluding program-point switches (join handler entry, recorded
use envs)
--------------------------------------------------
The alias equivalence relation only COARSENS along the lineage: classes merge and
never split, no member is ever removed (the Aliases API has no removal operation),
and each class's canonical only moves toward earlier binding times (constants, at
binding time 0, absorbing — T.Env.ConstCanonicalPersists). Consequently every
prover answer derived PURELY from aliasing has STABLE PROVED-NESS under further
descent, with the witness monotonically UPGRADING toward earlier binding times
(Proved(var) can become Proved(sym) or Proved(c) within the same class — not
answer-stable). This is the semantically-monotone CORE of the refuted
S.Struct.TypesMonotoneDown: the concrete-type half is only algorithmically monotone
(its γ can grow — S.Struct.EnvRefineOnly (b)), but the alias half is genuinely
γ-monotone (each merge only adds equality constraints).
NOTES: Scope refinement: stability holds for CONSTANT and SYMBOL witnesses along the
whole descent; VARIABLE witnesses additionally degrade Proved→Unknown at closure
entry, because DE.enter_set_of_closures bumps min_binding_time (typing_env.ml:1037-38),
forcing outer variables In_types where the T.Prove.SimpleModeBoundary mode floor
filters them — and S.Struct.SetOfClosuresEager places closure bodies inside the
descent. Suggested disposition of the old S.Struct.TypesMonotoneDown: (i)
S.Struct.EnvRefineOnly (algorithmic), (ii) this rule (semantic, alias half), (iii) a
note that semantic monotonicity of concrete types fails at the documented non-GLB
corners (T.Meet.MutableBlockMissedBottom). Composes: S.Struct.EnvRefineOnly,
T.Env.ConstCanonicalPersists, T.Prove.SimpleModeBoundary, T.Join.ConstAgreement.
```

```rule
RULE INV.Loopify.TrapNeutral
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_env.ml#add_continuation
CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#compile_staticfail
CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
---
Code c is loopified (S.Rewrite.Loopify.Body), self continuation k
--------------------------------------------------
(a) the entry jump created by S.Rewrite.Loopify.Body and every redirect created by
    S.Rewrite.Loopify.SelfTailCall (simplify_self_tail_call builds Apply_cont with
    no trap action) are trap-action-FREE and sit where the trap stack equals the
    function-entry stack;
(b) EVERY apply_cont targeting k — including jumps retargeted at k by continuation
    shortcuts (which PRESERVE the site's trap action) — is trap-DEPTH-neutral: the
    trap stack AFTER its trap action equals the function-entry stack, so the handler
    runs at entry depth;
(c) under TC.LetCont.Rec / TC.ApplyCont.Jump the loop lowers to a recursive Ccatch
    whose every Cexit is depth-matched; a Cexit to the loop label MAY carry a Pop (a
    shortcut-retargeted try-exit) but NEVER a Push (Shortcut_to rejects trap-carrying
    handlers, simplify_let_cont_expr.ml:741-45; the only Push creation site is the
    trywith translation in lambda_to_flambda.ml, targeting the fresh try-body
    continuation at CPS conversion — self continuations do not exist until Simplify,
    which never creates Push actions);
(d) self tail calls in a `with`-handler branch are loopifiable (the handler runs
    after the Pop, at entry depth); self calls inside a `try` body are rejected
    (they fail SelfTailCall's exn-continuation premise).
NOTES: Connects three chapters. SelfTailCall's side condition "apply's exn
continuation = the function's with no extra args" is, by the CPS discipline (an
Apply's exn_handler is the innermost enclosing handler — lambda_to_flambda_env.
add_continuation), exactly the statement that no trap handler pushed since function
entry is still live at the call. Refuted at the letter as "every apply_cont to k
carries no trap action": a try's normal-exit continuation can collapse onto the loop
by Shortcut, yielding a Pop-carrying Cexit; the restatement weakens to legs
(a)/(b)/(c). Falsification: a Cexit to a loopify loop label carrying a Push, or any
jump to k whose post-trap-action depth ≠ entry depth, or a SelfTailCall redirect
bearing a trap action. Composes: S.Rewrite.Loopify.Body, S.Rewrite.Loopify.SelfTailCall,
S.Rewrite.LetCont.Shortcut, S.Rewrite.LetCont.DemoteExn.
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

8. **DESIGN CONFLICT (recorded, not resolved): CSE of immutable block
   construction vs deterministic `Phys_equal`.** `S.Rewrite.CSE.Eligible`
   admits `Make_block (_, Immutable, Heap)` (the `(Only_generative_effects
   Immutable, No_coeffects)` clause; `variadic_primitive_eligible_for_cse` and
   `Eligible_for_cse.create`, middle_end/flambda2/terms/flambda_primitive.ml —
   the latter's comment reads "Allow constructions of immutable blocks to be
   shared"), so two identical immutable allocations may be merged by
   `S.Rewrite.CSE.Replace`, whose NOTES claim the identity change "is not
   observable". `P.Binary.PhysEqual` says otherwise: it specifies `phys_equal`
   as deterministic machine-word equality, and the pair is jointly refutable
   against `INV.Simplify.Preserves`: `let x = Make_block(0, Immutable)[a] in
   let y = Make_block(0, Immutable)[a] in phys_equal x y` evaluates to `false`
   in every run of the source term (two fresh allocations), exhibits no
   undefined behaviour, and after CSE (`y ↦ x`) evaluates — indeed
   constant-folds — to `true` (`T.prove_physical_equality`,
   middle_end/flambda2/types/provers.ml, proves equality of identical aliases;
   `simplify_phys_equal` folds on it). The compiler is not wrong: the OCaml
   manual specifies `(==)` on non-mutable values as implementation-dependent
   (guaranteeing only that `e1 == e2` implies structural equality), and the
   fold is disciplined in exactly the way that keeps sharing coherent —
   `prove_physical_equality` proves *dis*equality only from content or kind
   incompatibility (disjoint numeric sets, differing tags/shapes/sizes), never
   from "distinct allocations", so every physical-equality fact Simplify
   generates is stable under the sharing it introduces. The over-claim is in
   the formalism: `P.Binary.PhysEqual` pins an answer the language
   deliberately leaves loose, and `INV.Simplify.Preserves` then demands it be
   preserved. (The same license covers other identity-affecting mechanisms:
   lifting to statics, unboxing's re-boxing at use sites.) The `-Oclassic`
   sibling of this conflict is recorded at
   [`14-validation/classic_physequal_box.md`](14-validation/classic_physequal_box.md):
   the same deterministic `P.Binary.PhysEqual` is refuted in the opposite
   direction (`to_cmm` Delay-duplication splits identity) and its repair #1
   coincides with option (b) below, so the resolution should be chosen jointly
   (see also `INV.ToCmm.EffectLinear` NOTES in
   [`20-to-cmm-soundness.md`](20-to-cmm-soundness.md)). Resolution is a
   design decision, deferred: (a) qualify observational equivalence (§1) to
   "up to physical identity of immutable values" (equivalently, relate U′'s
   heap to U's by a folding of immutable blocks); (b) make ⟦Phys_equal⟧
   nondeterministic on operands not provably the same word; (c) leave both
   rules as faithful transcriptions of code truth with this recorded
   refutation. The `S.Rewrite.CSE.Replace` NOTES sentence should in any case
   be corrected to "identity may be observed through `phys_equal`; the change
   is licensed because `(==)` on non-mutable values is implementation-
   dependent". Mechanization pointer: FINDING #16 at
   rocq/theories/RewritesPrim.v (`S.Rewrite.CSE.Eligible` site); the
   mechanization derives the same fold via `S_Rewrite_Prim_PhysEqual_Equal`
   (identical canonicals after `S_Rewrite_CSE_Replace` installs the alias), so
   the refutation is reproducible in-model by composing the two rules. A
   14-validation witness (two immutable allocations, phys_equal folded to
   true) would be a worthwhile follow-up.

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
