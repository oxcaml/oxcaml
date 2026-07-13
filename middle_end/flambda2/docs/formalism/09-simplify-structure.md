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
NOTES: Partial positive evidence: the equation-mutating entry points
(add_equation_on_variable / _symbol / _name and add_variable, all in
downwards_env.ml) route through Typing_env.add_equation, which *meets* the new
type onto the name's existing type — a refinement, never a weakening. What keeps
this conjectured rather than proved is DE.with_typing_env (and the specialization
/ replay paths), which can install a wholesale-replaced typing env for a name;
those are not audited to be monotone. Conjectured pending the chapter-13 audit;
it underpins the soundness argument that downwards facts remain valid deeper in
scope.
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
  1. save-and-clear the lifted constants and the continuation_uses_env;
  2. traverse body downwards, during which every (apply_cont k args) records a
     use of k carrying (env_at_use, arg_types);
  3. form the handler's entry environment from the recorded uses (join, below);
  4. traverse handler downwards;
  5. on the way up, rebuild handler then body, deciding inlining / removal.
--------------------------------------------------
The uses of k are read from the continuation_uses_env after the body has been
traversed (Continuation_uses_env.get_continuation_uses). If k has no uses, its
handler is not traversed at all and the Let_cont is dropped.
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
    uses recorded during the body traversal (compute_handler_env, is_recursive);
  - each kᵢ's variant params q̄ᵢ get unknown_with_subkind types (their declared
    subkinds only).
--------------------------------------------------
simplify_single_recursive_handler builds the handler env with
DE.add_parameters_with_unknown_types for q̄ᵢ. Only invariant params carry types
across the recursive edge, and those come from a single pass over the body.
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
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
CODE middle_end/flambda2/simplify/loopify_state.mli
---
A self-recursive function may be "loopified": its body is wrapped in a recursive
continuation and the self-tail-calls become continuation calls, so the loop is
optimized without going through the function-call machinery. loopify_state (in
denv) records Do_not_loopify or Loopify cont; when Loopify cont, the body is
simplified as a recursive Let_cont (simplify_as_recursive_let_cont).
--------------------------------------------------
Unrolling and other recursion controls are driven by rec_info on the recursive
call; details are [§11](11-inlining.md). Loopification interacts with recursive-continuation
handling (§3): the loopified body is exactly the recursive-continuation case.
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
  `TypesMonotoneDown`, `SetOfClosuresEager`, `LetCont.BodyFirst`, `ContUse`,
  `JoinParams`, `NoJoinUnknown`, `SingleInlinableUse`, `Rec.NoFixpoint`,
  `Rec.InvariantVsVariant`, `ApplyContRewrite`, `Turn`, `Flow.RequiredNames`,
  `Flow.UnusedParams`, `Flow.Aliases`, `Flow.MutableUnboxing`, `Lift.Accumulate`,
  `Lift.PlaceAtToplevel`, `Lift.EmptyAtEnd`, `InlineResimplify`, `Resimplify`,
  `Loopify`.
- **dacc / uacc composition**: recorded in `S.Struct.Dacc` and `S.Struct.Uacc`;
  crucially `uacc` embeds the creating `dacc` (`creation_dacc`), and the flow
  result is attached to `uacc` at the turn.
- **How recursive continuations avoid a fixpoint**
  (`S.Struct.Rec.NoFixpoint` / `Rec.InvariantVsVariant`): invariant params get
  the join of invariant arguments over uses seen in the body (single pass);
  variant params get `Unknown` (subkind only); each handler is traversed once.
- **Open questions / conjectured** (for chapter-6 verification):
  - `S.Struct.TypesMonotoneDown` is `conjectured`; needs the chapter-13 audit to
    confirm no downwards path weakens an existing equation.
  - Exact interaction of `-flambda2-join-points` with the *new* n-way join's
    extra-parameter introduction (`add_extra_params_from_join_analysis`) is only
    sketched here; whether the flag also gates that path is worth checking.
  - The precise set of uses feeding the invariant-param join for recursive
    groups (body uses only vs. any recursive uses) is stated as body-only from
    `simplify_handlers`; confirm no later path re-joins with in-handler uses.
  - `max_function_simplify_run` bound and whether resimplification can be
    triggered by anything other than mutable unboxing / loop aliases.
```
