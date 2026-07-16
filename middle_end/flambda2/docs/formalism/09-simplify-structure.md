# Simplify: the downwards/upwards architecture

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter formalizes the *structure* of the Simplify pass: how it traverses a
`flambda_unit`, what state it threads, and the global analyses (continuation-use
joins, dataflow, lifting) that hold the local rewrites together. The local
rewrites themselves are chapter [`10`](10-simplify-rewrites.md); function
inlining is chapter [`11`](11-inlining.md); unboxing is chapter
[`12`](12-unboxing.md). Here we describe the scaffold those chapters plug into.

Most of what follows is *descriptive* — it documents an algorithm and its data
structures, which the code may legitimately restructure. A handful of statements
are genuine invariants the rest of the formalism (and chapter
[`13`](13-soundness.md)) relies on; those are `normative`. Statements believed
true but not yet checked against every relevant path are `conjectured`.

Rule IDs in this chapter live in the `S.Struct.*` namespace.

## 1. Top-level contract

Simplify is invoked as `Simplify.run`, called once from
`flambda2.ml#flambda_to_flambda0` in `Normal` mode (in `Classic` mode Simplify
is skipped; see chapter [`01`](01-overview.md)). It maps a `flambda_unit` to an
optimized `flambda_unit` together with side results (the final typing
environment, all `Code` defined in the unit, slot offsets, and the unit's free
names).

```rule
RULE S.Struct.Run
STATUS normative
CODE middle_end/flambda2/simplify/simplify.ml#run
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
---
Simplify.run : cmx_loader:… -> machine_width:… -> round:int ->
               code_slot_offsets:… -> flambda_unit -> simplify_result
where simplify_result =
  { free_names; final_typing_env; all_code; slot_offsets; unit }
--------------------------------------------------
run builds an initial downwards environment E₀ (DE.create) and an initial
downwards accumulator dacc₀ = DA.create E₀ code_slot_offsets
Continuation_uses_env.empty, then simplifies the unit body with
Simplify_expr.simplify_toplevel against the unit's return and exception
continuations. The rebuilt body is converted to a Flambda.Expr.t and returned as
unit; final_typing_env is the typing env recorded at the (single) use of the
return continuation.
NOTES: Meaning-preservation of run — that unit and its output denote the same
function of the program's inputs — is the soundness claim, stated and discussed
(unproved) in [§13](13-soundness.md). This rule only fixes the interface and the
orchestration.
```

Two post-conditions of `run` are checked by the code itself and are worth
recording, because later chapters assume them.

```rule
RULE S.Struct.Run.ClosedResult
STATUS normative
CODE middle_end/flambda2/simplify/simplify.ml#run
---
after simplification, the only variable permitted to occur free in the whole
compilation unit's body is toplevel_my_region; every other free name must be a
symbol
--------------------------------------------------
run folds over UA.name_occurrences of the result and calls Misc.fatal_error if
any free variable other than toplevel_my_region is found. Free symbols are
allowed (they are the unit's imports and its own defined symbols).
```

```rule
RULE S.Struct.Run.NoPendingConstants
STATUS normative
CODE middle_end/flambda2/simplify/simplify.ml#run
CODE middle_end/flambda2/simplify/lifting/lifted_constant_state.mli
---
LCS.is_empty (UA.lifted_constants uacc) at the end of run
--------------------------------------------------
Every lifted constant discovered during simplification has been placed (as a
"let symbol" binding) by the time the upwards pass reaches the top of the unit;
run calls Misc.fatal_error otherwise. See S.Struct.Lift.EmptyAtEnd.
```

### One round, no whole-unit fixpoint

Flambda 2's Simplify is *not* run to a fixpoint over the whole unit. The driver
passes `round = 0` and never iterates.

```rule
RULE S.Struct.SingleRound
STATUS normative
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
CODE middle_end/flambda2/simplify/simplify.ml#run
---
flambda_to_flambda0 sets round = 0 and calls Simplify.run exactly once per unit
--------------------------------------------------
There is no outer loop re-running Simplify over the whole unit. The round field
is threaded into the environment (affecting inlining-depth defaults) but is
always 0 in this pipeline.
NOTES: Bounded, *local* re-simplification does happen — a single function body
may be re-simplified a bounded number of times (S.Struct.Resimplify), and
inlined bodies are traversed by the ongoing pass (S.Struct.InlineResimplify) —
but neither is a whole-unit fixpoint. Recursive continuations are handled in a
single traversal without iteration (S.Struct.Rec.NoFixpoint).
```

## 2. Two-phase structure

Simplification of every expression has a **downwards** phase and an **upwards**
phase (`simplify_expr.mli`, module header). Downwards, the traversal follows the
scope order of the term, accumulating what is known; upwards, it rebuilds the
term bottom-up. The two phases are woven together by two nested layers of
continuation-passing (`simplify_common.mli`):

- the **term-level continuation** `down_to_up`, which carries values forward
  until they are needed and marks the point at which the downwards traversal of
  a subexpression has finished; and
- the **compiler-level continuation** `rebuild` / `after_rebuild`, which keeps
  the simplifier tail-recursive while rebuilding.

```rule
RULE S.Struct.TwoPhase
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr
CODE middle_end/flambda2/simplify/simplify_common.mli
---
simplify_expr dacc e ~down_to_up dispatches on the head of e (Let, Let_cont,
Apply, Apply_cont, Switch, Invalid). Each case traverses downwards carrying a
downwards accumulator dacc, and at a terminator calls down_to_up dacc ~rebuild,
where rebuild : Upwards_acc.t -> after_rebuild:… -> … rebuilds the term against
an upwards accumulator uacc.
--------------------------------------------------
The traversal has the shape
  down :  dacc  →  (uacc → (e′, uacc′))         [the rebuild closure]
  up   :  uacc  →  (e′, uacc′)
The downwards environment (denv) is discarded when a terminator is reached; the
downwards accumulator (dacc) is threaded through the whole traversal.
```

In general the *entire* expression is traversed downwards before any of it is
rebuilt upwards. There is one important exception (below): the right-hand sides
of `Let`-bindings of `Set_of_closures` are simplified fully — down *and* up —
during the downwards pass of the enclosing expression.

### The downwards accumulator

The downwards accumulator `dacc` (`downwards_acc.ml`) carries both the
scope-following environment and the non-scoped information gathered across the
whole traversal.

```rule
RULE S.Struct.Dacc
STATUS descriptive
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#t
CODE middle_end/flambda2/simplify/env/downwards_env.ml#t
---
dacc = { denv;                       (* the downwards environment, below *)
         continuation_uses_env;      (* recorded uses of in-scope conts *)
         shareable_constants;        (* static consts eligible for sharing *)
         used_value_slots;           (* accumulated, never save/restored *)
         lifted_constants;           (* LCS awaiting placement *)
         flow_acc;                   (* dataflow accumulator, §4 *)
         demoted_exn_handlers;
         code_ids_to_remember / _to_never_delete / _never_simplified;
         slot_offsets;
         debuginfo_rewrites;
         are_lifting_conts;          (* continuation-lifting state *)
         lifted_continuations; continuation_lifting_budget;
         continuations_to_specialize; specialization_map }
--------------------------------------------------
The downwards environment denv (downwards_env.ml) contains, among other fields:
  typing_env : E              (* the abstract domain, chapters 07–08 *)
  round; machine_width;
  inlining_state              (* inlining depth + arguments *)
  disable_inlining;
  at_unit_toplevel : bool     (* governs lifting placement, §5 *)
  cse; comparison_results;    (* CSE table, [§10](10-simplify-rewrites.md) *)
  are_rebuilding_terms;       (* false during speculative inlining *)
  closure_info;               (* in a closure? which code id? *)
  all_code;                   (* Code defined so far in the unit *)
  loopify_state;              (* §6 *)
  replay_history;             (* for specialization replay *)
  specialization_cost; join_analysis;
  defined_variables_by_scope; lifted   (* continuation lifting *)
NOTES: denv is the piece that behaves like a conventional typing environment
and is discarded at a terminator; the surrounding dacc fields persist. Only the
fields load-bearing for this chapter are listed.
```

### The upwards accumulator

```rule
RULE S.Struct.Uacc
STATUS descriptive
CODE middle_end/flambda2/simplify/env/upwards_acc.ml#t
CODE middle_end/flambda2/simplify/env/upwards_acc.ml#create
---
uacc = { uenv;                (* upwards env: continuation handlers/rewrites *)
         creation_dacc;       (* the dacc from which this uacc was made *)
         lifted_constants;    (* LCS still to be placed on the way up *)
         all_code;            (* Exported_code accumulated for cmx *)
         name_occurrences;    (* free names of the piece being rebuilt *)
         cost_metrics;        (* size/benefit of rebuilt code *)
         slot_offsets;
         flow_result;         (* result of flow analysis, §4 *)
         resimplify : bool }  (* §6 *)
--------------------------------------------------
The uacc is created at "the turn" (§4) by UA.create ~flow_result uenv dacc; it
literally embeds the dacc it was created from (creation_dacc), so upwards code
can still read downwards information (typing env, used value slots, shareable
constants, specialization map).
NOTES: name_occurrences and cost_metrics are repeatedly cleared and restored as
the rebuild crosses binders (e.g. Let_cont handlers), so that each rebuilt
subterm's free names and cost can be measured in isolation; used_value_slots by
contrast is accumulated in the dacc and never save/restored.
```

### Invariants of the downwards phase

```rule
RULE S.Struct.TypesMonotoneDown
STATUS conjectured
CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_variable
CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_equation_on_name
---
As the downwards traversal descends into a subexpression, the typing env E only
gains equations; the type assigned to any given in-scope name only moves down
(more precise) in the lattice, never up.
--------------------------------------------------
denv exposes add_variable / add_equation_on_* which extend E; there is no
operation that weakens an existing equation during a straight-line descent.
(Meet at control-flow merges — join points — is a separate operation, §3, and
produces the *entry* type of a handler, not a weakening of an existing name.)
NOTES: Now SPLIT into two verified halves plus a caveat (this rule is retained as
the umbrella intuition, but its universal γ-reading is REFUTED). The audit shows:
(1) the ALGORITHMIC half — every downwards site refines by define/meet/extend —
holds and is stated as S.Struct.EnvRefineOnly below; (2) the SEMANTIC half, that
γ of a name only shrinks, holds for the ALIAS sub-domain
(INV.Simplify.AliasesMonotoneDown, [§13](13-soundness.md)) but FAILS for stored
concrete types at the non-GLB meet corner (T.Meet.MutableBlockMissedBottom:
adding a Mutable_block fact to a field-precise Variant strictly ENLARGES its γ).
Soundness is not threatened (types may soundly move up); the casualty is only the
"a prover query that succeeded earlier still succeeds later" argument, which holds
for constant/symbol (alias) witnesses but not for arbitrary concrete types.
```

```rule
RULE S.Struct.EnvRefineOnly
STATUS conjectured
CODE middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env
CODE middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical
CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
---
E is the typing env at some downwards point; E' is the env installed by ANY
DE.with_typing_env / DE.map_typing_env / DA.map_denv reachable in the downwards
pass
--------------------------------------------------
(a) [algorithmic monotonicity] E' is obtained from the env it replaces
    exclusively by: name/parameter definitions, add_equation /
    add_env_extension(_with_extra_variables) (meets onto canonicals), or
    code-age-relation extension — EXCEPT the two sites installing the env of a
    DIFFERENT program point: the handler-entry env from a join (compute_handler_env)
    and the recorded use-env lineage (incl. the replay path), both themselves
    derived monotonically from the fork/recorded env.
(b) [negative half] the SEMANTIC reading γ_{E'}(x) ⊆ γ_E(x) does NOT follow from
    (a): add_concrete_equation_on_canonical stores the raw meet result, and meet
    is not a lower bound in the Variant ⊓ Mutable_block corner
    (T.Meet.MutableBlockMissedBottom), so a stored type's γ can strictly enlarge.
NOTES: Audit of all with_typing_env / map_typing_env installation sites: the join
fork+extension sites, the use-env lineage (define_parameters + equations-on-params,
all meets), arg-kind meets, projection meets, per-arm switch meets (flow only into
the recorded use, denv kept — S.Struct.Switch.ArmIsolation), unboxing-denv meets,
lifted-constant add_env_extension, relational-primitive adds, and
with_code_age_relation (code-age component only). The replay path does not touch
the typing env. Part (b) is an operation-level non-guarantee: Nietzsche found no
reachable Simplify trigger (Mutable_block types attach only to fresh names; shapes
are never Mutable_block; the realizable Mutable_block ⊓ Variant-shape leaves the
stored type unchanged), so it survives as an empirical per-run invariant when the
GLB conjunct is made explicit. Composes: S.Struct.TypesMonotoneDown,
INV.Simplify.AliasesMonotoneDown, T.Meet.GreatestLowerBound (refuted corner),
T.Join.Levels.
```

### Set_of_closures is simplified eagerly

```rule
RULE S.Struct.SetOfClosuresEager
STATUS normative
CODE middle_end/flambda2/simplify/simplify_expr.mli
CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_non_lifted_set_of_closures
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common
---
When a Let binds a Set_of_closures, the function bodies inside it are simplified
fully — both downwards and upwards — during the downwards pass of the enclosing
expression, before the enclosing Let's body is traversed.
--------------------------------------------------
simplify_non_lifted_set_of_closures simplifies each function body via
simplify_function_body, which is simplify_toplevel_common: a self-contained
down+up traversal returning a Rebuilt_expr.t. The resulting simplified body is
what the Flambda type of the closure records.
NOTES: This is a deliberate design choice (documented in simplify_expr.mli): the
type system must have the *simplified* function body available so that later
inlining of that function inlines already-optimized code. The cost is that
function-body simplification cannot use whole-program usage information that
only becomes available later. Everything else obeys the "all downwards, then all
upwards" rule of S.Struct.TwoPhase.
```

## 3. Continuation handling

Continuations are second-class (chapter [`01`](01-overview.md)); Simplify
processes a `Let_cont` by first traversing the **body** (so that all uses of the
continuation are seen), recording each use with the types of its arguments, and
only then simplifying the **handler** in an environment whose parameter types
are computed from those uses. The full staging is drawn as a diagram at the top
of `simplify_let_cont_expr.ml`; the load-bearing steps are below.

```rule
RULE S.Struct.LetCont.BodyFirst
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_let_cont0
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body
---
To simplify (let_cont k params = handler in body):
  1. save-and-clear the lifted constants;
  2. traverse body downwards, during which every (apply_cont k args) records a
     use of k carrying (env_at_use, arg_types);
  3. form the handler's entry environment from the recorded uses (join, below);
  4. traverse handler downwards;
  5. on the way up, rebuild handler then body, deciding inlining / removal.
--------------------------------------------------
The uses of k are read from the continuation_uses_env after the body has been
traversed (Continuation_uses_env.get_continuation_uses). If k has no uses, its
handler is not traversed at all and the Let_cont is dropped.

The continuation_uses_env is *not* cleared before the body: the body is
traversed with the ambient CUE intact, so uses of enclosing in-scope
continuations recorded in the body propagate upward. The only clear-to-empty
happens per-handler at the start of step 4 (simplify_handler:
DA.with_continuation_uses_env ~cont_uses_env:CUE.empty), not before the body.
(A stale comment in simplify_let_cont0 claims the CUE is reset before the body;
that is a code-comment issue, not the actual behaviour.)
```

Continuation scopes are bumped so the join happens at a well-defined level: for a
`let_cont` entered at scope `n`, the body is inspected at scope `n+2`, the join
of uses is performed at scope `n+1`, and the joined environment is bumped back to
`n+2` for the handler (`simplify_let_cont_expr.ml#simplify_let_cont0`, comment).

### Recording a use

```rule
RULE S.Struct.ContUse
STATUS descriptive
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use
CODE middle_end/flambda2/simplify/env/continuation_uses_env.mli
CODE middle_end/flambda2/simplify/env/one_continuation_use.ml
---
Each apply_cont / switch arm targeting an in-scope continuation k records a
One_continuation_use.t = { env_at_use = denv; id : Apply_cont_rewrite_id.t;
                           use_kind; arg_types : T list }
into the continuation_uses_env, generating a fresh rewrite id.
--------------------------------------------------
use_kind is Inlinable or Non_inlinable { escaping } and is derived from the
syntactic context (Simplify_common.apply_cont_use_kind). The rewrite id names
the use site so that a later Apply_cont_rewrite can adjust its arguments
(S.Struct.ApplyContRewrite).
```

A `Switch` arm records a continuation use whose environment is refined by the
arm's discriminant, but that refinement is confined to the recorded use — it never
escapes to the threaded dacc.

```rule
RULE S.Struct.Switch.ArmIsolation
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
---
Switch on a scrutinee with arms {iᵢ ↦ actionᵢ}; simplify_arm folds over the arms
with accumulator (arms, dacc)
--------------------------------------------------
(i) the meet of the scrutinee type with arm i's discriminant
    (T.meet typing_env_at_use scrutinee_ty (this_naked_immediate i)) yields env_i,
    recorded as arm i's continuation-use env — but NEVER installed in the threaded
    dacc (record_continuation_use touches only continuation_uses_env; the arm's
    args are simplified in a locally built DA whose denv is dropped);
(ii) hence no fact conditional on arm i can influence arm j or any later traversal:
    per-arm narrowing reaches program facts ONLY through the join of the
    destination continuation's recorded uses (compute_handler_env);
(iii) an arm whose meet is ⊥ is dropped WITHOUT recording a continuation use, so
    statically-dead switch edges contribute nothing to any parameter join.
NOTES: Isolation rests on three facts in three files — record_continuation_use
leaves denv untouched, the fold keeps the outer dacc, and compute_handler_env is
the sole consumer of env_i. A leak in any one would let an arm-conditional fact
(env_i asserts scrutinee = i) escape its guard — a soundness bug. Corollary:
typing outcomes are invariant under arm processing order. Arm-conditional facts DO
reach the arm's own action (sound — the arm runs only under its condition).
Composes: T.Meet.Bottom, T.Join.Levels, S.Struct.EnvRefineOnly.
```

### Parameter types are the join of argument types

```rule
RULE S.Struct.JoinParams
STATUS normative
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
CODE middle_end/flambda2/simplify/join_points.ml#join
CODE middle_end/flambda2/types/flambda2_types.mli#cut_and_n_way_join
---
For a continuation k with recorded uses u₁ … uₘ (m ≥ 2, or m = 1 non-inlinable),
each supplying argument types T¹ … Tⁿ for k's n parameters, the entry type of
parameter j in the handler environment is
        ⊔_i Tⁱⱼ     (n-way join, computed by T.cut_and_n_way_join)
taken over the use environments, provided joining is enabled.
--------------------------------------------------
compute_handler_env calls join, which calls T.cut_and_n_way_join over the use
environments cut after the fork scope. Joining is enabled when
  Flambda_features.join_points () holds, OR there are at most one use.
NOTES: -flambda2-join-points (Oxcaml_flags.Flambda2.join_points) gates the n-way
join for the ≥2-use case. The join also runs the CSE join and, with the new
join, introduces extra parameters for projections/aliases known in all uses
(join_points.ml#add_extra_params_from_join_analysis).
```

```rule
RULE S.Struct.JoinParams.AnalysisExtraParams
STATUS conjectured
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
CODE middle_end/flambda2/simplify/join_points.ml#add_extra_params_from_join_analysis
CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
CODE driver/oxcaml_flags.ml#o2
---
continuation k reaches the general (not single-inlinable-use) branch of
compute_handler_env
--------------------------------------------------
The n-way join-analysis extra-param path (add_extra_params_from_join_analysis) is
EXERCISED for k IFF should_do_join (= Flambda_features.join_points() ∨ #uses ≤ 1)
∧ Flambda_features.join_algorithm() ∈ {N_way, Checked}, since join_analysis is
returned Some exactly by the N_way/Checked arms of cut_and_n_way_join (Binary ⇒
None) and the whole path sits inside join, gated by should_do_join. ("Exercised",
not "introduces": actual introduction additionally needs candidate variables with
definitions at all uses — has_extra_arg_in_all_uses.)
NOTES: Resolves the ch-09 open question affirmatively AND adds the join_algorithm
conjunct ch-09 did not mention. Consequences: (a) -flambda2-join-points DOES gate
extra-param introduction; (b) under every standard profile (default/-O2/-O3/-O4 all
keep join_algorithm = Binary; only join_points flips at O2+) the machinery is INERT
— it never fires without an explicit -flambda2-join-algorithm n-way; (c) with n-way
selected, a SINGLE non-inlinable use also runs the analysis (should_do_join true for
≤ 1 use even with join_points off), so extra params can appear on single-use conts.
Hidden second entrance: -flambda2-match-in-match FORCES join_algorithm() = N_way
(flambda_features.ml:66-74), but defaults false in every profile, so (b) stands.
Same flag gate as T.Join.ConstAgreement (types side). Non-local: gate split across
oxcaml_flags (profiles), join_levels (algorithm dispatch), join_points
(should_do_join).
```

```rule
RULE S.Struct.NoJoinUnknown
STATUS descriptive
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types
---
When joining is disabled (Flambda_features.join_points () is false and k has ≥ 2
uses), each parameter of k is given the type unknown_with_subkind of its declared
subkind, discarding argument-type information from the use sites.
--------------------------------------------------
compute_handler_env falls into the branch that calls
add_parameters_with_unknown_types for both the ordinary and the extra params.
```

```rule
RULE S.Struct.SingleInlinableUse
STATUS descriptive
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
---
If k is non-recursive with exactly one use and that use is Inlinable, no join is
performed: the handler is simplified directly in the use's own environment
(augmented with the parameter equations and any lifted constants from the body).
The result is marked is_single_inlinable_use = true.
--------------------------------------------------
Such a continuation is a candidate to be inlined into (i.e. its handler spliced
at) its single call site; the decision and mechanism are [§10](10-simplify-rewrites.md). Exception
handlers are never marked single-inlinable.
```

The join at a merge point also joins the CSE tables of the predecessors; when a
CSE-eligible primitive is available on every path but with a different value, the
join synthesizes a phi node as a fresh continuation parameter.

```rule
RULE S.Struct.CSE.JoinPhi
STATUS conjectured
CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#join_one_cse_equation
CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#cse_with_eligible_lhs
CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#cut_cse_environment
CODE middle_end/flambda2/simplify/join_points.ml#join
---
continuation k, joining enabled (S.Struct.JoinParams gate), uses u₁ … uₘ;
p a CSE-eligible primitive (S.Rewrite.CSE.Eligible) such that for EVERY use uᵢ the
  CSE equations between the fork and uᵢ (cut_cse_environment) map p — after
  canonicalizing p's arguments per-use and rewriting them into fork scope through
  k's params/extra-args (cse_with_eligible_lhs) — to a value bᵢ
  (has_value_on_all_paths in join_one_cse_equation)
--------------------------------------------------
The handler env's CSE table maps p to: b when all bᵢ = b (a fork-scope simple);
else a FRESH continuation parameter v ("cse_param") appended to k, with bᵢ passed
as an extra argument at each use — a compiler-synthesized phi node (Is_int/Get_tag
also re-establish their relational equation on v). An occurrence of p in k's
handler is then replaced by v (S.Rewrite.CSE.Replace) — recomputation eliminated
ACROSS the merge, no code motion. If the handler has no occurrence of p, v is
unused and S.Rewrite.LetCont.UnusedParam removes it: the phi introduction is
self-cleaning, its harmlessness UNDERWRITTEN by dead-param elimination.
NOTES: Non-local three ways: availability is ∀-over-predecessors (one path lacking
p kills it — has_value_on_all_paths); the phi rewires every call site via
Apply_cont_rewrite; the introduction happens BEFORE the handler is simplified, so
its safety is a theorem about a different subsystem. Precision boundaries: an
argument of p canonicalizing at some use to a simple neither mappable to a
param/extra-param nor in fork scope; a bᵢ canonicalizing to a fork param with no
unique non-param alias (Alias_set.get_singleton — a known precision loss,
CR-someday lmaurer); equations already at the fork (prev_cse) never re-join.
Gating: the phi fires only when join_points is ON — -O2/-O3 set it true, so say
"when join_points is off", not "by default". Composes: S.Rewrite.CSE.Eligible,
S.Rewrite.CSE.Replace, S.Rewrite.LetCont.UnusedParam, S.Struct.JoinParams.
```

### Recursive continuations: no fixpoint

Recursive continuations are handled *without* iterating to a fixpoint. The key
is the split between **invariant** parameters (passed unchanged around the loop,
shared by all handlers in the recursive group) and each handler's own
**variant** parameters.

```rule
RULE S.Struct.Rec.NoFixpoint
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers
---
A recursive Let_cont is simplified in a single downwards traversal of each
handler; there is no iteration to a typing fixpoint over the loop.
--------------------------------------------------
prepare_dacc_for_handlers computes the invariant-parameter entry types once,
from the uses seen while traversing the body, and each handler is then traversed
exactly once (simplify_single_recursive_handler); recursive calls discovered
inside a handler only extend the set of *reachable* handlers still to traverse,
not the parameter types already fixed.
```

```rule
RULE S.Struct.Rec.InvariantVsVariant
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
---
For a recursive group with invariant params p̄ and, per continuation kᵢ, variant
params q̄ᵢ:
  - invariant params p̄ get the n-way join of the invariant arguments over the
    uses recorded during the body traversal (compute_handler_env, is_recursive)
    — but ONLY when joining is enabled, i.e. Flambda_features.join_points ()
    holds OR there are ≤ 1 recorded body uses (the should_do_join gate). Under
    default flags (join_points = false) a recursive group entered from ≥ 2 body
    sites instead gives the invariant params unknown_with_subkind of their
    declared subkind (add_parameters_with_unknown_types), exactly as
    S.Struct.NoJoinUnknown;
  - each kᵢ's variant params q̄ᵢ get unknown_with_subkind types (their declared
    subkinds only).
--------------------------------------------------
simplify_single_recursive_handler builds the handler env with
DE.add_parameters_with_unknown_types for q̄ᵢ. Only invariant params carry types
across the recursive edge, and those come from a single pass over the body.
The invariant params share the identical compute_handler_env gate described in
S.Struct.JoinParams / S.Struct.NoJoinUnknown (they flow through
prepare_dacc_for_handlers → compute_handler_env with ~is_recursive:true), so
under default flags the join in the first bullet is not taken.
NOTES: This is what makes the single-pass treatment sound: an invariant
parameter is by construction passed the same value at every (including
recursive) call, so a type valid at the body's entry uses remains valid; a
variant parameter, whose value changes around the loop, is conservatively
Unknown.
```

### Rewriting use sites

Adding or removing continuation parameters (dead-param elimination, unboxing,
CSE-introduced params, alias params, mutable-unboxing params) changes a
continuation's arity, so every recorded use must be rewritten to match. This is
the job of `Apply_cont_rewrite`.

```rule
RULE S.Struct.ApplyContRewrite
STATUS descriptive
CODE middle_end/flambda2/simplify/apply_cont_rewrite.mli
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation
CODE middle_end/flambda2/simplify/expr_builder.mli#rewrite_apply_cont
---
When a handler is rebuilt, an Apply_cont_rewrite is created from
  (original_params, extra_params_and_args, decide_param_usage)
and registered in the upwards env (UE.add_apply_cont_rewrite) under k. On the way
up, each (apply_cont k args) at a recorded use id is rewritten via
EB.rewrite_apply_cont: unused parameters' arguments are dropped and extra
arguments (looked up by the use's rewrite id) are appended.
--------------------------------------------------
decide_param_usage classifies each param Used / Unused / Used_as_invariant, from
the free names of the rebuilt handler (non-recursive) or from the dataflow
required-names set (recursive, where the handler cannot be re-examined after the
fact). For non-recursive handlers the two must agree — the code asserts that any
param actually free in the rebuilt handler was also marked required by the flow
analysis.
```

### Unreachable continuations cascade in one pass

Because uses are recorded only from traversed code, and zero-use handlers are not
traversed, the reachability of the continuation graph is a lazy fixpoint computed
by the traversal itself: a single proven-constant scrutinee can delete a whole
subgraph of continuations and the bindings feeding it, transitively, in one pass.

```rule
RULE S.Struct.LetCont.UnreachableClosure
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_recursive_handlers
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont
---
Body B under downwards traversal. Define the residual jump graph J: nodes =
{entry} ∪ continuations of B; an edge (site owner) → k′ for every apply_cont /
switch-arm use of k′ actually RECORDED downwards — i.e. the site is in traversed
code, its arm is not pruned by S.Rewrite.Switch.ArmPrune (simplify_arm's Bottom
branch records neither a continuation use nor flow-acc args), and it is not cut off
by S.Rewrite.Let.Invalid truncating traversal of its context
--------------------------------------------------
(1) a handler is downwards-traversed iff its continuation is reachable from entry
    in J (non-recursive: the "Continuation unused, no need to traverse" branch of
    simplify_handlers; recursive groups: the reachable_handlers_to_simplify worklist
    in simplify_recursive_handlers, dropping whole unreachable sub-SCCs);
(2) every continuation unreachable in J is deleted from the output this same pass
    (S.Rewrite.LetCont.DeadHandler), its handler never simplified;
(3) deadness is transitively closed within the pass: an untraversed handler
    contributes nothing to the continuation-uses env NOR to flow_acc, so
    continuations whose only uses sit in unreachable handlers are themselves
    unreachable, and names whose only consumers sit there leave required_names and
    die by S.Rewrite.Let.DeadBinding / S.Rewrite.LetCont.UnusedParam — no rewrite
    iteration.
NOTES: The closure is computed by the traversal: "use recording only from traversed
code" + "zero-use handlers are skipped" is a lazy reachability fixpoint over J.
Over-approximation boundary: uses killed only on the UPWARDS pass were already
recorded downwards; occurrence-based decisions (rebuild_let_cont's zero-occurrence
drop, rebuild_let's has_uses) read the REBUILT term inner-first and still fire, but
required_names-based decisions (recursive params, lifted constants) can lag one
round. The realizable lag mechanism is S.Rewrite.Switch.Merge: simplify_switch
records the scrutinee into used_in_current_handler only when >1 arms survive
downwards; if the arms then merge UPWARDS, the scrutinee occurrence evaporates after
required_names was fixed — a recursive param feeding such a scrutinee survives one
extra round while its own binding dies same-round. Composes: S.Rewrite.Switch.ArmPrune,
S.Rewrite.LetCont.DeadHandler, S.Struct.Flow.RequiredNames.
```

### Continuation lifting is budgeted and leaf-decided

Distinct from lifting *constants* to symbols (§5), Simplify can lift a nested
continuation out of an enclosing handler (the "match-in-match" enabler). This is
gated by a per-unit budget and five context reasons, and decided only at a switch
leaf.

```rule
RULE S.Struct.LiftCont.Gate
STATUS conjectured
CODE middle_end/flambda2/simplify/are_lifting_conts.mli#t
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#get_continuation_lifting_budget
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body_and_handlers
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_switch
CODE driver/oxcaml_flags.ml#o2
---
let_cont k' is nested inside the handler of let_cont k
--------------------------------------------------
k' can be lifted out of k only if ALL of:
  (a) budget > 0 ∧ lifting_cost ≤ budget; the budget is initialized from
      Flambda_features.Expert.cont_lifting_budget(): 0 at default/-O1/-Oclassic
      (mechanism INERT), 100 at -O2, 1000 at -O3, 3000 at -O4, and decrements by
      lifting_cost per performed lift;
  (b) the enclosing context carries none of the five no_lifting reasons
      (are_lifting_conts.mli): At_toplevel, In_speculative_inlining,
      In_continuation_specialization, In_recursive_continuation,
      In_inlinable_continuation;
  (c) the status has moved Analyzing{k} → Lifting_out_of{k} at a LEAF of k's
      handler — EXCLUSIVELY in simplify_switch (lift_continuations_out_of has one
      caller in the tree), never eagerly at the let_cont itself.
NOTES: Fills a ch-09 gap (S.Struct.Dacc lists are_lifting_conts /
lifted_continuations / continuation_lifting_budget with no rule). Corollary
(loopify link): entering a recursive-continuation handler sets no_lifting
In_recursive_continuation, so a continuation nested in a loopified self-loop
handler is never lifted out of the loop (same for single-inlinable-use handlers and
specialization replays). Descriptive-leaning: thresholds are heuristic, but the
five-reason gate and leaf-decision architecture are structural. Composes:
S.Struct.Dacc, S.Struct.SpeculativeSandbox.
```

## 4. The turn: dataflow analysis

Between the end of the downwards traversal of a function body (or the whole unit)
and the start of the upwards rebuild — "the turn" — Simplify solves a set of
dataflow equations over the control-flow/dataflow graph accumulated in
`flow_acc`. The result is stored in the `uacc` and consulted throughout the
upwards pass.

```rule
RULE S.Struct.Turn
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common
CODE middle_end/flambda2/simplify/flow/flow_analysis.mli#analyze
CODE middle_end/flambda2/simplify/flow/flow_types.ml
---
At the transition from downwards to upwards for a toplevel/function body,
Flow.Analysis.analyze is run on the accumulated Flow.Acc.t, producing a
Flow_result = { data_flow_result; aliases_result; mutable_unboxing_result }. The
upwards accumulator is then created (UA.create ~flow_result) carrying this
result, and rebuilding begins.
--------------------------------------------------
The flow accumulator is built during the downwards pass (Flow.Acc.record_*,
enter_continuation / exit_continuation mirroring the traversal). analyze is
per-function-body: each Set_of_closures function body runs its own analysis (as
does the unit toplevel), because the graph is local to a body.
```

The four facts the analysis computes:

```rule
RULE S.Struct.Flow.RequiredNames
STATUS descriptive
CODE middle_end/flambda2/simplify/flow/flow_types.ml#Data_flow_result
CODE middle_end/flambda2/simplify/env/upwards_acc.ml#required_names
CODE middle_end/flambda2/simplify/env/upwards_acc.ml#reachable_code_ids
---
data_flow_result = { required_names : Name.Set.t;
                     reachable_code_ids : Reachable_code_ids.t Or_unknown.t }
required_names over-approximates the names whose bindings must be kept; a binding
whose bound name is not required is dead and may be removed on the way up.
reachable_code_ids identifies the live Code (and ancestors) for cmx export;
it is Unknown inside closures (only meaningful at toplevel).
--------------------------------------------------
required_names is the reference used to keep/drop continuation parameters of
recursive continuations and to keep/drop lifted constants (§5). It is an
over-approximation: the free-name count of the rebuilt term is the ground truth,
and the code checks required_names ⊇ actual uses (S.Struct.ApplyContRewrite).
```

```rule
RULE S.Struct.Flow.UnusedParams
STATUS descriptive
CODE middle_end/flambda2/simplify/flow/flow_analysis.mli
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
---
For recursive continuations, the parameters not in required_names (and not
aliased away) are removed; this is the primary purpose of the dataflow analysis,
since a recursive handler cannot be re-inspected after rebuilding to discover its
own dead parameters.
--------------------------------------------------
Non-recursive handlers instead use the exact free names of the rebuilt handler;
the dataflow result serves there only as a consistency check.
```

The dataflow closure removes loop-carried dead cycles *in toto* in the single
upwards pass — cycles a purely local use-count could never break, because each
member has syntactic occurrences (in the recursive apply_cont's args or a sibling
binding).

```rule
RULE S.Struct.Flow.DeadLoopParam
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#required_names
CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
---
a function body B analyzed at the turn; G = the flow dependency graph with edges
  x → FN(defn of x) for at-most-generative bindings and direct aliases, and
  pⱼ → FN(aⱼ) for the argument aⱼ supplied for parameter pⱼ at each apply_cont to a
  non-(return/exn) continuation;
U = unconditionally_used: free names of arbitrary-effects bindings, apply
  callees/args, switch scrutinees, args to B's return/exn continuation,
  exn-handler first params (+ value-slot deps, see blockers);
X = a set of names of B closed under: no element of X is reachable from U in G
  (X may contain cycles: params fed only by themselves/each other through pure
  bindings)
--------------------------------------------------
In the single upwards pass following the turn: every continuation parameter in X is
removed (recursive conts via decide_param_usage_recursive on required_names;
non-recursive conts agree via the rebuilt handler's free names); every argument
position for a removed parameter is dropped at every use site by the
Apply_cont_rewrite; and every at-most-generative binding in X becomes
occurrence-free and is deleted by S.Rewrite.Let.DeadBinding. required_names =
reachability from U on the downwards-pass graph; its complement is removed all at
once — cycles included — so no rewrite iteration or resimplification round is
needed.
NOTES: Non-local because every name in a dead cycle HAS syntactic occurrences;
local use-counting can never remove the cycle — each deletion is licensed only by
the others. The recursive-continuation rewrite is registered before handlers are
rebuilt (make_rewrite_for_recursive_continuation), so in-handler recursive
apply_conts drop arguments during the handler's own rebuild. The consistency check
required_names ⊇ actual-uses cannot trip: dropping arguments only shrinks free
names. Blocking conditions (when the over-approximation keeps X alive): (1) a
consumer of x ∈ X with arbitrary effects; (2) x passed to B's return/exn
continuation; (3) x the first param of an exn handler, or an exn-cont extra arg;
(4) x captured in a value slot when used_value_slots is Unknown (any non-toplevel
body); (5) x feeds a mutable-unboxing candidate prim, over-approximated
pre-unboxing (fixed only by the resimplify round); (6) generate_phantom_lets ∧
user_visible x (survives as phantom). The true multi-handler recursive-group case
follows from the shared-required_names mechanism (rewrites for the whole group
registered before any handler rebuilt) but is untestable via fexpr today
(fexpr_to_flambda's recursive multi-handler path is unimplemented). Composes:
S.Struct.Flow.RequiredNames, S.Struct.Flow.UnusedParams, S.Rewrite.Let.DeadBinding.
```

```rule
RULE S.Struct.Flow.Aliases
STATUS descriptive
CODE middle_end/flambda2/simplify/flow/flow_types.ml#Alias_result
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#extra_params_for_continuation_param_aliases
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
---
aliases_result identifies continuation parameters that are aliases of one
another or of an outer name. Aliased params are removed and replaced either by
extra args threaded from the call sites or by let-bindings introduced around the
handler (lets_to_introduce).
--------------------------------------------------
This recovers, across control flow, aliases that the straight-line downwards
alias tracking could not (e.g. loop-carried aliases); discovering a useful one in
a loop can trigger resimplification (§6).
```

The bucket (first) parameter of an exception handler is exempt from alias-based
removal, and alias equations are oriented around it rather than through it.

```rule
RULE S.Struct.Flow.ExnFirstParam
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/control_flow_graph.ml#minimize_extra_args_for_one_continuation
CODE middle_end/flambda2/terms/exn_continuation.mli#t
---
k is an exception handler with first (bucket) parameter x_b that REMAINS an
  exception handler through Simplify (not demoted per S.Rewrite.LetCont.DemoteExn —
  a demoted handler is ordinary and the ordinary AliasedParam rules apply);
the dominator/alias analysis proves x_b aliased to some simple s
--------------------------------------------------
x_b is NEVER placed in removed_aliased_params_and_extra_params — the bucket param
always survives alias-based removal — and the alias info is used only restrictedly:
  - s a symbol or poison: the fact is DISCARDED, and alias-based removal is disabled
    for EVERY parameter of that handler ("we cannot remove the exn param, so even if
    it is aliased to a symbol, we cannot make use of that information");
  - s a variable a: rematerialization is INVERTED relative to ordinary params —
    `let a = x_b` is introduced at the handler top (alias recovered FROM the bucket),
    and any OTHER parameter of k whose alias is a is rerouted to alias x_b instead.
NOTES: An undocumented WF constraint forced by a cross-chapter fact: the bucket
arrives via the raise machinery (OS.ApplyCont.ExnBoundary; in Cmm the trywith
handler binds it in a fixed location, TC.LetCont.Exn), not via a rewritable
apply_cont argument list, so no Apply_cont_rewrite can drop or re-source it. Local
reading of S.Rewrite.LetCont.AliasedParam ("a param pinned to one value is removed
and rematerialized") is exactly wrong for exn handlers; this rule records the
inversion. Caveat: no live trigger for the Aliased{var} branch was found —
handlers reachable only from explicit raises tend to be DEMOTED first
(S.Rewrite.LetCont.DemoteExn), and any Apply-exn edge makes the bucket its own
dominator; the branch is defensive. All four branches code-exact
(control_flow_graph.ml:244-337). Composes: S.Struct.Flow.Aliases,
S.Rewrite.LetCont.DemoteExn.
```

```rule
RULE S.Struct.Flow.MutableUnboxing
STATUS descriptive
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.mli
CODE middle_end/flambda2/simplify/flow/flow_types.ml#Mutable_unboxing_result
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
---
mutable_unboxing_result turns a mutable block (a ref-like allocation) that does
not escape into a set of mutable continuation parameters threaded through the
control-flow graph, eliminating the allocation. It produces
  { did_unbox_a_mutable_block; additional_epa; let_rewrites }
where additional_epa adds params/args to continuations and let_rewrites rewrites
or deletes the block's load/store/alloc primitives on the way up.
--------------------------------------------------
This is the second stated goal of the dataflow module (moving allocations off the
hot path of a loop). Doing it forces a resimplification pass (§6) so the new
parameters get proper types. The detailed transform is out of scope for this
chapter.
```

## 5. Lifting constants to symbols

During simplification, values that turn out to be statically allocatable are
*reified* to fresh symbols and their definitions accumulated as *lifted
constants*, to be emitted as "let symbol" bindings on the way up.

```rule
RULE S.Struct.Lift.Accumulate
STATUS descriptive
CODE middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify
CODE middle_end/flambda2/simplify/lifting/lifted_constant_state.mli
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_to_lifted_constant_accumulator
---
When a binding's type shows it denotes a constant (or a block/closure of
constants), Reification.try_to_reify produces a fresh symbol, records a
Lifted_constant describing its static definition into dacc.lifted_constants
(a Lifted_constant_state), and rewrites the binding to reference the symbol.
--------------------------------------------------
Lifted constants form a set that may be mutually recursive (Lifted_constant_state
sorts them innermost-first for placement). They flow up through uacc.lifted_
constants until placed. Sets of closures are lifted through a dedicated path
(Simplify_set_of_closures), not through generic reification.
```

The *placement* policy: a lifted constant is emitted at the innermost point of
the traversal that is still at the unit toplevel. Constants discovered under a
lambda (or otherwise not at toplevel) cannot be placed there — "let symbol" is
disallowed under a lambda — so they float up in the accumulator until a toplevel
binding is reached.

```rule
RULE S.Struct.Lift.PlaceAtToplevel
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
CODE middle_end/flambda2/simplify/expr_builder.mli#place_lifted_constants
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
CODE middle_end/flambda2/simplify/env/downwards_env.ml#at_unit_toplevel
---
Lifted constants are placed (as "let symbol" bindings) only where
DE.at_unit_toplevel holds. On the way up, at a toplevel Let (or around a toplevel
continuation handler), all pending constants whose symbols/code are live per the
flow analysis are placed; the rest continue to float up. No constant is ever
placed under a lambda.
--------------------------------------------------
rebuild_let calls EB.place_lifted_constants only when at_unit_toplevel;
otherwise it returns the constants in uacc for an outer binding to place. A
constant is kept only if its symbol is in required_names or its code is
reachable (keep_lifted_constant_only_if_used).
NOTES: at_unit_toplevel is maintained during the downwards pass: it stays true
through a non-recursive Let_cont whose handler postdominates its body and whose
body escapes only to toplevel (simplify_handlers), and is set false on entering a
closure or a recursive continuation.
```

```rule
RULE S.Struct.Lift.EmptyAtEnd
STATUS normative
CODE middle_end/flambda2/simplify/simplify.ml#run
---
When the upwards pass reaches the top of the unit, the lifted-constant
accumulator is empty: every discovered constant has been placed.
--------------------------------------------------
Guaranteed because the unit's top is at_unit_toplevel; run asserts it
(S.Struct.Run.NoPendingConstants).
```

## 6. Resimplification, inlining re-traversal, loopify

### Inlined bodies are re-traversed by the same pass

```rule
RULE S.Struct.InlineResimplify
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml
---
When an Apply is inlined, the inlined body (produced by Inlining_transforms.inline
with the call's arguments substituted) is handed straight back to simplify_expr
in the current downwards pass:  simplify_expr dacc inlined ~down_to_up.
--------------------------------------------------
The inlined code is therefore simplified in the calling context, with the caller's
types in scope. This is re-simplification within the single pass, not a new round
(cf. S.Struct.SingleRound). The inlining decision and cost model are [§11](11-inlining.md).
```

### The speculative-inlining sandbox is hermetic

The inlining oracle may run the full Simplify machinery on a candidate body to
measure its cost. That speculative traversal is hermetic: only cost metrics escape.

```rule
RULE S.Struct.SpeculativeSandbox
STATUS conjectured
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#prepare_for_speculative_inlining
CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
---
an Apply reaches the inlining oracle and speculative_inlining runs the full
Simplify machinery (down + up, including a private flow analysis) on the candidate
inlined body
--------------------------------------------------
(1) Hermeticity: speculation contributes nothing to the real pass. It runs on a
    COPY of dacc with a freshly-installed flow_acc (init_toplevel with a dummy
    toplevel continuation); its flow analysis runs ~speculative:true with empty
    code_age_relation, Unknown used_value_slots, empty code_ids_to_never_delete; the
    speculative dacc/uacc are discarded after cost-metrics extraction — no
    continuation use, flow edge, value-slot use, CSE equation or lifted constant
    reaches the real accumulators (only cost metrics escape).
(2) Flow-dependent rewrites are disabled inside the sandbox: loopification
    (do_not_rebuild_terms — "flow analysis would not see the self continuation"),
    continuation lifting (no_lifting In_speculative_inlining), nested INLINING
    (set_do_not_rebuild_terms_and_disable_inlining), and term rebuild itself
    (are_rebuilding_terms = false).
(3) No speculative artifact is emitted: if the decision is Inline, the body is
    re-simplified from scratch against the REAL dacc (S.Struct.InlineResimplify),
    re-recording all uses/flow/slots.
NOTES: The sole escape is Cost_metrics (plus lifted-const sizes); no other caller
of analyze ~speculative:true exists. Benign leaks (fresh-name stamp counters,
Profile counters, debug dumps) touch no compilation state. The loopify guard sits
at the Apply conversion, and the CR gbury there admits speculation UNDERCOUNTS
loopification benefit — a documented cost-model imprecision. Composes:
S.Struct.InlineResimplify, S.Struct.LiftCont.Gate, S.Struct.Turn.
```

### Bounded per-function resimplification

```rule
RULE S.Struct.Resimplify
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common
CODE middle_end/flambda2/simplify/env/upwards_acc.ml#set_resimplify
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml
---
At the turn, uacc is marked resimplify = true if the flow analysis performed
mutable unboxing (did_perform_mutable_unboxing) or added a useful alias in a loop
(added_useful_alias_in_loop). A function body so marked is simplified again, up
to a bounded number of times (max_function_simplify_run in
Simplify_set_of_closures), so the newly-introduced parameters/aliases get proper
types.
--------------------------------------------------
This loop is local to a single function body and bounded by a fixed count; it is
not a whole-unit fixpoint. The unit toplevel can likewise be flagged, but the
re-run is driven from the set-of-closures / function-body machinery.
```

### Loopify and unrolling

```rule
RULE S.Struct.Loopify
STATUS normative
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
CODE middle_end/flambda2/simplify/loopify_state.mli
VERIFIED 14-validation/loopify-01-escaping-tailrec.md
---
A function whose loopify attribute satisfies should_loopify — an attribute fixed
deterministically at closure conversion (S.Rewrite.Loopify.Attribute,
[§10](10-simplify-rewrites.md)), not chosen by a cost oracle — is
unconditionally "loopified": its body is wrapped in a recursive continuation and
the self-tail-calls become continuation calls, so the loop is optimized without
going through the function-call machinery. loopify_state (in denv) records
Do_not_loopify or Loopify cont; when Loopify cont, the body is simplified as a
recursive Let_cont (simplify_as_recursive_let_cont).
--------------------------------------------------
The rewrites themselves are S.Rewrite.Loopify.Body and
S.Rewrite.Loopify.SelfTailCall ([§10](10-simplify-rewrites.md)); together they
entail that a purely self-tail-recursive function never survives in its
original Apply-recursive form. Unrolling and other recursion controls are driven by
rec_info on the recursive call; details are [§11](11-inlining.md).
Loopification interacts with recursive-continuation handling (§3): the
loopified body is exactly the recursive-continuation case.
```

## 7. Diagram: dacc/uacc flow over a Let / Let_cont nest

Consider the expression

```
let x = prim in                         (* Let, binds x *)
let_cont k y = <handler> in             (* Let_cont, binds k *)
  <body mentioning x, calling k>        (* body of the Let_cont *)
```

The traversal threads `dacc` downwards (left column, top-to-bottom) and `uacc`
upwards (right column, bottom-to-top). Time flows down the left column, then
across the bottom, then up the right column.

```
  DOWNWARDS (carry dacc)                          UPWARDS (carry uacc)
  ─────────────────────────                       ─────────────────────────
  simplify_let (x = prim)
    │  denv ⊕ {x : T_prim}
    │  flow_acc ⊕ binding(x)
    │  (maybe reify x → symbol, into
    │   dacc.lifted_constants)
    ▼
  simplify_let_cont k
    │  save+clear cont_uses_env,
    │  lifted_constants
    │  scope n → body at n+2
    ▼
  traverse BODY downwards ───────────────┐
    │  each (apply_cont k args) records  │
    │  a use: (env_at_use, arg_types,    │
    │  fresh rewrite id) in cont_uses_env│
    ▼                                     │
  ══ THE TURN (per function body) ══      │  (bodies of any Set_of_closures
    │  Flow.Analysis.analyze flow_acc     │   met on the way down were already
    │  → flow_result (required_names,     │   simplified down+up: §2)
    │    aliases, mutable_unboxing)       │
    │  uacc = UA.create ~flow_result …    │
    │  (uacc embeds this dacc as          │
    │   creation_dacc)                    │
    ▼                                     │
  join uses of k  ◀──────────────────────┘
    │  param types of k := ⊔ arg_types
    │  (or Unknown if -no-join & ≥2 uses)
    ▼
  traverse HANDLER of k downwards
    │  denv ⊕ {y : joined type}
    ▼
  ── reached terminators; rebuild ──▶  rebuild HANDLER (uacc)
                                          │  free names, cost metrics of handler
                                          │  decide used/unused params
                                          │  build Apply_cont_rewrite for k,
                                          │    register in uenv
                                          │  (add lets/phantoms around handler;
                                          │   place lifted consts if toplevel)
                                          ▲
                                       rebuild BODY (uacc)
                                          │  rewrite each (apply_cont k args)
                                          │    via k's Apply_cont_rewrite:
                                          │    drop unused args, append extra
                                          │  drop dead Let (x) if x ∉ required
                                          ▲
                                       rebuild Let_cont
                                          │  drop k if 0 uses (leave body);
                                          │  inline k if single inlinable use;
                                          │  else emit Let_cont k = handler
                                          ▲
                                       rebuild Let (x = prim)
                                          │  place x's lifted constant if at
                                          │    toplevel; else float up in uacc
                                          ▲
                                       (e′, uacc′) returned to enclosing scope
```

The essential asymmetry: **downwards** decisions are made with the typing
environment (what is known about each name) and record obligations (continuation
uses, dataflow edges, lifted constants); **upwards** work consumes the
turn's flow result and the recorded uses to actually rebuild, drop, rewrite, and
place. The `dacc` at the turn is frozen into `uacc.creation_dacc`, so the upwards
pass can still read everything the downwards pass knew.

## Summary for the sync protocol

- **Rule IDs introduced** (`S.Struct.*`): `Run`, `Run.ClosedResult`,
  `Run.NoPendingConstants`, `SingleRound`, `TwoPhase`, `Dacc`, `Uacc`,
  `TypesMonotoneDown`, `EnvRefineOnly`, `SetOfClosuresEager`, `LetCont.BodyFirst`,
  `ContUse`, `Switch.ArmIsolation`, `JoinParams`, `JoinParams.AnalysisExtraParams`,
  `NoJoinUnknown`, `SingleInlinableUse`, `CSE.JoinPhi`, `Rec.NoFixpoint`,
  `Rec.InvariantVsVariant`, `ApplyContRewrite`, `LetCont.UnreachableClosure`,
  `LiftCont.Gate`, `Turn`, `Flow.RequiredNames`, `Flow.UnusedParams`,
  `Flow.DeadLoopParam`, `Flow.Aliases`, `Flow.ExnFirstParam`,
  `Flow.MutableUnboxing`, `Lift.Accumulate`, `Lift.PlaceAtToplevel`,
  `Lift.EmptyAtEnd`, `InlineResimplify`, `Resimplify`, `SpeculativeSandbox`,
  `Loopify`.
- **dacc / uacc composition**: recorded in `S.Struct.Dacc` and `S.Struct.Uacc`;
  crucially `uacc` embeds the creating `dacc` (`creation_dacc`), and the flow
  result is attached to `uacc` at the turn.
- **How recursive continuations avoid a fixpoint**
  (`S.Struct.Rec.NoFixpoint` / `Rec.InvariantVsVariant`): invariant params get
  the join of invariant arguments over uses seen in the body (single pass);
  variant params get `Unknown` (subkind only); each handler is traversed once.
- **Open questions / conjectured** (for chapter-6 verification):
  - `S.Struct.TypesMonotoneDown` is split: the algorithmic half is
    `S.Struct.EnvRefineOnly` (verified), the semantic half holds for aliases
    (`INV.Simplify.AliasesMonotoneDown`, [§13](13-soundness.md)) and FAILS for
    concrete types at the non-GLB corner (`T.Meet.MutableBlockMissedBottom`).
  - RESOLVED: `-flambda2-join-points` DOES gate
    `add_extra_params_from_join_analysis`, additionally conjoined with
    `join_algorithm ∈ {N_way, Checked}` — see
    `S.Struct.JoinParams.AnalysisExtraParams` (and `T.Join.ConstAgreement` from
    the types side).
  - The precise set of uses feeding the invariant-param join for recursive
    groups (body uses only vs. any recursive uses) is stated as body-only from
    `simplify_handlers`; confirm no later path re-joins with in-handler uses.
  - `max_function_simplify_run` bound and whether resimplification can be
    triggered by anything other than mutable unboxing / loop aliases (see
    `S.Rewrite.Loopify.ResimplifyIdempotent`, [§10](10-simplify-rewrites.md)).
```
