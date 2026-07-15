# Inlining

*Part of the Flambda 2 formalism; see [README.md](README.md).*

Inlining replaces a direct call to a known function by a freshened copy of that
function's body, with the parameters bound to the arguments. It is the single
most consequential rewrite Simplify performs: it does not by itself remove much
code, but it exposes the callee's body to the caller's abstract environment, so
that the rewrites of chapter [`10-simplify-rewrites.md`](10-simplify-rewrites.md)
and the unboxing of [`12-unboxing.md`](12-unboxing.md) can then fire on a body
they could not previously see into. Because inlining a recursive function can
diverge, and because inlining an arbitrary function bloats code, the *whether*
of inlining is governed by a decision oracle with a cost model and several depth
limits; the *how* is a single, mechanical substitution.

This chapter separates those two concerns. Section 1 gives the transformation as
one normative rewrite rule, `S.Inline.Substitute`, whose side condition is the
oracle's verdict and whose soundness is beta-reduction with respect to the
operational semantics of chapter [`04-opsem.md`](04-opsem.md). Sections 2–3
describe the oracle and the recursion/unrolling machinery *descriptively*: they
document the current heuristic and its knobs, which the code may legitimately
change. Sections 4–5 cover what inlining unlocks and cross-module inlining.

The companion prose in [`../inlining.md`](../inlining.md) motivates the
heuristics (predictability, speed) and the cross-module inlining-arguments
story; this chapter records what the code does.

Namespace: `S.Inline.*`.

## 1. The transformation

Inlining is consulted only for a **direct, full application** of an OCaml
function whose code is available: the call kind is `Function { function_call }`,
the callee's closure type resolves to a single code ID `cid`, the arity matches
exactly (not partial, not over-application), and `cid`'s `params_and_body` is
present in the environment (not merely its metadata). Those preconditions are
established by `simplify_apply` → `simplify_function_call` →
`simplify_direct_function_call` → `simplify_direct_full_application`
(`middle_end/flambda2/simplify/simplify_apply_expr.ml`); partial applications,
over-applications, tupled-calling-convention adjustments, method calls, C calls,
and effect operations are handled by sibling functions and are never inlined by
this rule. (Partial applications instead build a wrapper closure, which is
itself a stub that the oracle then always inlines; see
`simplify_direct_partial_application`.)

The application carries an *alloc mode* `α` (`Alloc_mode.For_applications.t`,
either `Heap` or `Local { region; ghost_region }`) describing where the call's
result may be allocated. The code carries a *result mode* (`Alloc_heap` or
`Alloc_local`). One combination is statically impossible and is turned into
`Invalid` rather than inlined:

```rule
RULE S.Inline.ModeMismatchInvalid
STATUS normative
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline
---
call_kind(apply) = Function, direct to cid    code(cid) available
alloc_mode(apply) = Heap    result_mode(code(cid)) = Alloc_local
--------------------------------------------------
E ⊢ apply ⇝ Invalid (Calling_local_returning_closure_with_normal_apply apply)
NOTES: A heap-allocating call to a locally-returning closure should have been
ruled out by the type checker, so this is GADT-caused unreachable code. The
remaining three combinations of (α, result_mode) are all permitted (Local into
Alloc_heap is allowed by subtyping) and proceed to S.Inline.Substitute.
```

### The substitution

Let the callee code `cid` bind, via `Function_params_and_body`, a return
continuation `k_ret^c`, an exception continuation `k_exn^c`, parameters
`x̄ = x₁ … xₙ`, a self-closure variable `x_myclos`, a self-region alloc mode
`α^c`, a depth variable `x_mydepth`, and a body `e_body`. Let the application be
`Apply cid s_callee (s̄ = s₁ … sₙ)` with result continuation `κ_ret`
(either `Return k` or `Never_returns`), exception continuation `κ_exn`, and
alloc mode `α`. Let `d₀` be the callee's *rec-info* as known at the call site
(section 3), obtained by meeting the closure type's rec-info in the environment.

```rule
RULE S.Inline.Substitute
STATUS normative
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#inline
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
VERIFIED 14-validation/new-05-inline-fold.md
---
apply = Apply cid s_callee s̄ with return κ_ret, exn κ_exn, alloc mode α
code(cid) binds ⟨k_ret^c, k_exn^c, x̄, x_myclos, α^c, x_mydepth, e_body⟩
Inline?(apply, code_metadata(cid), E) = inline{unroll_to = ⊥, was_inline_always}
θ = [k_ret^c ↦ k]  (if κ_ret = Return k; empty if Never_returns)
  ∘ [k_exn^c ↦ exn_handler(κ_exn)]
  ∘ ρ_region        (region substitution, below)
--------------------------------------------------
E ⊢ apply ⇝
  let x_mydepth = d₀ in
  let x_myclos  = s_callee in
  let x₁ = s₁ in … let xₙ = sₙ in
  θ(e_body)
NOTES: The lets bind the depth variable outermost, then the self-closure, then
the parameters (innermost). θ renames only continuations and region variables;
the let-bound variables x_mydepth, x_myclos, x̄ keep their identity because they
are freshened by α-conversion when the code is instantiated. The unroll_to = ⊥
case is the ordinary one; unroll_to = Some depth is S.Inline.Unroll.Begin.
```

**Region wiring** (`ρ_region`). The callee's body allocates in its own region,
named by the variables inside `α^c`. Inlining must retarget those allocations to
the caller's region:

```rule
RULE S.Inline.Substitute.Region
STATUS normative
CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#make_inlined_body
---
--------------------------------------------------
ρ_region =
  ∅                                              if α = Heap
  [x_myregion ↦ region, x_myghost ↦ ghost_region] if α = Local { region; ghost_region }
                                                  and α^c = Local { x_myregion; x_myghost }
  ∅                                              if α = Local … and α^c = Heap
NOTES: When α = Heap the call returns a heap value and never allocates in a
caller region, so the callee's region variables are unused in e_body and need no
renaming. The region substitution is a genuine renaming (not a let-binding)
because the callee's region variables are known fresh for e_body, so a
permutation cannot capture an existing occurrence.
```

**Callee binding and depth.** In the ordinary (non-unrolling) case the
self-closure is bound directly to the simplified callee simple `s_callee`, and
the depth variable is bound to `d₀`. If the callee simple is absent — the
`Apply` has no callee, which happens for certain compiler-generated direct calls
— then no `x_myclos` let is emitted and the depth variable is instead bound to
`do_not_inline` (`Const{depth = ∞; unrolling = Do_not_unroll}`), preventing any
further inlining of self-calls in the copied body.

**Exception extra arguments.** If `κ_exn` carries *extra arguments* (an
exception continuation `k_exn` applied to trailing values, used to thread
values past a `try`), the body cannot simply rename `k_exn^c ↦ k_exn`: raises
from the inlined body must re-raise with those extra arguments appended. The
transform wraps the inlined body in a fresh trap frame that pops to a wrapper
which re-raises with the extra args:

```rule
RULE S.Inline.Substitute.ExnExtraArgs
STATUS normative
CODE middle_end/flambda2/simplify_shared/inlining_helpers.ml#wrap_inlined_body_for_exn_extra_args
---
Inline?(apply, …, E) = inline    extra_args(κ_exn) = ā (nonempty)
k1 fresh (exn handler)    k_pop fresh    k_push fresh
--------------------------------------------------
E ⊢ apply ⇝
  let_cont_exn k1 (exn) =
    Apply_cont ⟨pop exn_handler(κ_exn), Reraise⟩ exn_handler(κ_exn) (exn :: ā) in
  let_cont k_pop (r̄) = Apply_cont ⟨pop k1⟩ κ_ret r̄ in
  let_cont k_push () = ⟨inlined body of S.Inline.Substitute, with κ_exn ↦ k1
                       and κ_ret ↦ Return k_pop⟩ in
  Apply_cont ⟨push k1⟩ k_push ()
NOTES: When κ_ret = Never_returns the k_pop wrapper is omitted and the body
returns nowhere. See [§04](04-opsem.md) for trap push/pop semantics.
```

### Soundness

`S.Inline.Substitute` is beta-reduction for the function/continuation calculus
of [§04](04-opsem.md): binding `xᵢ = sᵢ` and jumping to `k` where the code returned to
`k_ret^c` is exactly the small-step behaviour of `OS.Apply` followed by the
`let` rules, up to the renaming θ (which is a bijective renaming of bound
continuations and region variables and so preserves meaning). The formal
soundness obligation — that the left- and right-hand configurations are
observationally equivalent under any `⟨ρ, K, H, T, R⟩` — is stated in chapter
[`13-soundness.md`](13-soundness.md); this chapter asserts it, and the
validation case studies in [`14-validation/`](14-validation/) check instances.
The region rule is the delicate part: it is sound precisely because `α`'s region
is live at the call site (the type checker guarantees the caller region outlives
the callee's allocations for the permitted `(α, result_mode)` combinations), so
retargeting the callee's `Endregion`/allocation to the caller's region does not
extend any lifetime observably.

## 2. The decision oracle (descriptive)

The oracle is the judgment `Inline?(apply, code_metadata, env)`. In the code its
result is richer than `{inline, keep}`: it is a
`Call_site_inlining_decision_type.t` recording *why*, which
`Call_site_inlining_decision_type.can_inline` collapses to either
`Do_not_inline { erase_attribute_if_ignored }` (= `keep`) or
`Inline { unroll_to; was_inline_always }` (= `inline`, feeding
`S.Inline.Substitute`). The oracle runs in two layers: a per-declaration
classification computed once when the code is defined (section 2.1), consulted
by a per-call-site procedure (section 2.2).

### 2.1 Per-declaration classification

```rule
RULE S.Inline.DeclDecision
STATUS descriptive
CODE middle_end/flambda2/simplify_shared/function_decl_inlining_decision.ml#make_decision0
CODE middle_end/flambda2/terms/function_decl_inlining_decision_type.ml#behaviour
VERIFIED 14-validation/code_size_of_single_arg_switch.md
---
Given the code's inline attribute, stub flag, cost metrics (size), functor flag,
recursive flag, and the inlining arguments in force at the definition:

  Never_inline attribute                        ⇒ Never_inline_attribute   (cannot)
  Always_inline attribute                       ⇒ Attribute_inline         (must)
  stub                                          ⇒ Stub                     (must)
  recursive ∧ ¬unrolls ∧ ¬can_inline_recursive  ⇒ Recursive               (cannot)
  size ≥ large_size ∧ attr ≠ Available_inline   ⇒ *_Body_too_large        (cannot)
  size ≤ small_size                             ⇒ Small_function/_functor (must)
  otherwise                                     ⇒ Speculatively_inlinable  (could)
NOTES: "large_size"/"small_size" are the functor thresholds when is_a_functor,
else the function thresholds. behaviour(·) maps the constructors to
{Must_be_inlined, Cannot_be_inlined, Could_possibly_be_inlined}. A declaration
that Cannot_be_inlined is not exported in the .cmx (section 5). The three-way
classification — must / cannot / could — is exactly what the call site consults.
```

The size compared is `Cost_metrics.size` of the code, the abstract code-size
metric of section 2.3. `can_inline_recursive_functions` is the
`-flambda2-expert-can-inline-recursive-functions` flag.

### 2.2 Per-call-site procedure

`make_decision0`
(`middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml`) runs
these steps in order, short-circuiting at the first that decides. `d` below is
the call-site rec-info (section 3); `attr` is the call's `Inlined_attribute.t`
(`Apply.inlined`, one of `Never_inlined`, `Default_inlined`, `Always_inlined`,
`Hint_inlined`, `Unroll n`).

```rule
RULE S.Inline.Decision
STATUS descriptive
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision
CODE middle_end/flambda2/simplify_shared/call_site_inlining_decision_type.ml#can_inline
VERIFIED 14-validation/missing_code.md
---
0. jsir mode                          ⇒ Jsir_inlining_disabled            (keep)
1. attr = Never_inlined               ⇒ Never_inlined_attribute           (keep)
2. code(cid) missing / metadata-only  ⇒ Missing_code                      (keep)
3. remaining unrolling depth of d = 0 ⇒ Unrolling_depth_exceeded          (keep)
4. remaining unrolling depth of d > 0 ⇒ Continue_unrolling                (inline) [step 2]
5. (unrolling state unknown, below:)
   inlining-state depth exceeded      ⇒ Max_inlining_depth_exceeded       (keep)
6. policy from attr and recursiveness:
     Default_inlined                          → `Heuristic
     Unroll n                                 → `Unroll n
     Always_inlined/Hint_inlined, recursive   → `Unroll 1
     Always_inlined/Hint_inlined, non-recursive → `Always
   `Always                            ⇒ Attribute_always                  (inline, always)
   `Unroll n, can_unroll(d)           ⇒ Begin_unrolling n                 (inline) [step 1]
   `Unroll n, ¬can_unroll(d)          ⇒ Unrolling_depth_exceeded          (keep)
   `Heuristic, depth_may_exceed(d, max_rec_depth) ⇒ Recursion_depth_exceeded (keep)
   `Heuristic, replay says must-inline ⇒ Replay_history_says_must_inline  (inline)
   `Heuristic, otherwise              ⇒ might_inline(…)                    (§2.3)
NOTES: Steps 3–4 implement recursive unrolling (§3); the "unrolling state
unknown" branch (5–6) is the common case for a free depth variable, i.e. an
internal recursive self-call or a first inlining. A None function_type (no
rec-info available at all) short-circuits to keep before this procedure runs.
```

Attribute semantics, then, are: `[@inlined never]` never inlines; `[@inlined
always]`/`[@inlined hint]` inline unconditionally for non-recursive functions
(and behave like `[@unrolled 1]` for recursive ones, so a normally-non-recursive
function that turns out recursive still terminates); `[@unrolled n]` triggers
bounded unrolling; and with no attribute (`Default_inlined`) the heuristic and
its speculative step decide. Requesting inlining of a function whose code is
absent (step 2) yields `Missing_code`, which is where an `Inlining_impossible`
warning is owed (the code has outstanding CRs about emitting it).

### 2.3 The heuristic step: speculative inlining

When step 6 reaches `might_inline`, the oracle first consults the
per-declaration classification of section 2.1 via the callee's
`Code_metadata`:

```rule
RULE S.Inline.Speculative
STATUS descriptive
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#might_inline
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining
CODE middle_end/flambda2/terms/cost_metrics.ml#evaluate
VERIFIED 14-validation/inlining_cost_of_primitive_on_parameters.md
VERIFIED 14-validation/removed_operations_of_switch.md
VERIFIED 14-validation/speculative_inlining_lifted_constants.md
---
in a stub                                    ⇒ In_a_stub                    (keep)
declaration says must_be_inlined             ⇒ Definition_says_inline       (inline)
declaration says cannot_be_inlined           ⇒ Definition_says_not_to_inline (keep)
already doing speculative inlining           ⇒ Doing_speculative_inlining   (keep)
¬argument_types_useful(apply)                ⇒ Argument_types_not_useful    (keep)
otherwise:
  cm = cost_metrics of (inline apply; simplify body with speculation disabled)
  evaluated_to = size(cm) − Σ removed_operations(cm)  [weighted by inlining args]
  if evaluated_to ≤ threshold                ⇒ Speculatively_inline         (inline)
  else                                       ⇒ Speculatively_not_inline     (keep)
NOTES: Speculative inlining actually performs S.Inline.Substitute and runs
Simplify on the result (with further speculative inlining prohibited and a
restricted flow analysis), purely to measure the resulting cost metrics; the
rewritten body is discarded and only cm is kept. The acceptance test is thus
"does the simplified inlined body come in under the size/benefit threshold",
not a static size test. argument_types_useful requires at least one argument to
have non-unknown type unless the flag disabling that requirement is set.
```

The `evaluate` formula weights the abstract code size upward and subtracts a
bonus for each removed operation (calls, allocations, primitives, branches,
indirect-to-direct conversions, polymorphic-comparison specialisations), each
scaled by a per-operation cost knob. So a call that, once inlined and
simplified, deletes many allocations or resolves an indirect call can come in
under threshold even if the raw body is not small.

### 2.4 The cost model (descriptive)

`Cost_metrics.t` pairs an abstract `Code_size.t` with a `Removed_operations.t`
counter set (`middle_end/flambda2/terms/cost_metrics.ml`). Code size is an
arbitrary, size-correlated unit accumulated structurally over an expression
(`middle_end/flambda2/terms/code_size.ml`): a `Switch` costs 5 per arm; an
`Apply_cont` costs 1, plus 4 for a `Push` trap or 2 for a `Pop` trap (i.e. 5 or 3
with a trap action, 1 without); an `Apply` costs by call kind
(`direct_call_size = 4`, `indirect_call_size = 6`, C-call sizes 4 or 10
depending on whether it needs `caml_c_call`); a `Let` costs body plus defining
expression; a primitive costs a per-primitive amount; and a set of closures
costs the sum of its closures' bodies plus `alloc_size = 5` plus the allocation's
word count (the number of value slots, plus a per-closure 2 for arity ≤ 1 or 3
for arity > 1) — the bodies are included because inlining a function that defines
closures copies those bodies too. This model is purely descriptive:
the constants and the structural rules are heuristics the code may retune.

### 2.5 Inlining arguments and predictability (descriptive)

Every declaration and every `Apply` carries an `Inlining_state.t`
(`middle_end/flambda2/terms/inlining_state.ml`): a set of inlining *arguments*
(the cost/size/threshold knobs) plus an inlining *depth*. Thresholds and sizes
come from the flags skimmed below, defaulted per optimization level
(`-O2`/`-O3`/`-O4`) in
`middle_end/flambda2/ui/flambda_features.ml#Inlining`:

- `-flambda2-inline-threshold` — the acceptance threshold in `S.Inline.Speculative`.
- `-flambda2-inline-small-function-size`, `-flambda2-inline-large-function-size`
  (and the functor variants) — the section-2.1 size bands.
- `-flambda2-inline-max-depth` — bound on `Inlining_state` depth (step 5).
- `-flambda2-inline-max-rec-depth` — bound on rec-info depth (§3).
- `-flambda2-inline-{call,alloc,prim,branch,poly-compare,indirect-call}-cost` —
  the per-operation bonuses in `Cost_metrics.evaluate`.
- `-flambda2-expert-can-inline-recursive-functions`,
  `-flambda2-expert-fallback-inlining-heuristic`,
  `-flambda2-speculative-inlining-only-if-arguments-useful` — binary switches
  referenced above.

Two inlining states can be *met* (`Inlining_state.meet`): the argument sets are
met (the result inlines *no more* than either), while the depths are *summed*.
Declaration and call-site decisions both use the meet of the environment's state
with the declaration's / apply's state; when a call is actually inlined,
`Downwards_env.enter_inlined_apply` installs the met arguments and increments the
depth (see §3). Summing depths on meet is what makes inlining predictable across
compilation units: code that arrives via a chain of inlinings carries the summed
depth of that chain, so a unit compiled at `-O1` cannot have its inlined-in code
re-inlined more aggressively at `-O3` than its original settings allowed. The
extended rationale is in [`../inlining.md`](../inlining.md).

## 3. Recursive functions and unrolling

Termination of the inliner rests on *rec-info* (`Rec_info_expr.t`,
`middle_end/flambda2/identifiers/rec_info_expr0.ml`), the depth expression
attached to every closure value and threaded through `my_depth`:

```
d ::= Const { depth : int | ∞ ; unrolling }      -- a known depth/unrolling
    | x                                            -- a depth variable (my_depth)
    | Succ d                                       -- one level deeper
    | Unroll_to (n, d)                             -- request unrolling to depth n
unrolling ::= Not_unrolling | Unrolling { remaining_depth : int } | Do_not_unroll
```

Distinguished constants: `initial = Const{0, Not_unrolling}`,
`unknown = Const{∞, Not_unrolling}`, `do_not_inline = Const{∞, Do_not_unroll}`.
When a function is inlined, `S.Inline.Substitute` binds `my_depth = d₀` (the
callee's rec-info at the call site). The function's own recursive self-calls are
expressed in its body as `Succ my_depth` (established at closure conversion), so
each inlined level increments the observed depth by one. `Simplify_rec_info_expr`
(`middle_end/flambda2/simplify/simplify_rec_info_expr.ml`) evaluates a `d` in
the environment: `Succ` increments a finite depth (and decrements a positive
`remaining_depth`), `Unroll_to (n, ·)` raises `remaining_depth` to at least `n`.

Two limits keep the inliner terminating; both are side conditions of
`S.Inline.Decision`:

```rule
RULE S.Inline.DepthLimit
STATUS descriptive
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
CODE middle_end/flambda2/terms/inlining_state.ml#is_depth_exceeded
CODE middle_end/flambda2/simplify/simplify_rec_info_expr.ml#depth_may_exceed
CODE middle_end/flambda2/simplify/env/downwards_env.ml#enter_inlined_apply
---
inlining-state depth limit: inline only if Inlining_state.depth(apply) <
  max_inlining_depth(arguments); enter_inlined_apply increments the depth by
  depth_scaling_factor (= 10), or by 1 when was_inline_always, or by 0 in a stub.
recursion-depth limit (heuristic policy only): inline only if the evaluated
  rec-info depth of d does not exceed max_rec_depth.
NOTES: The two are different counters. The inlining-state depth bounds total
nesting of inlinings (any function); the rec-info depth bounds how deep a single
recursive function is followed. Scaling the state depth by 10 lets always-inline
functions (which add only 1) nest much more than heuristic inlinings before the
limit bites.
```

Explicit unrolling is a two-step dance driven by the `unrolling` field, both
steps living in `S.Inline.Decision`:

```rule
RULE S.Inline.Unroll.Begin
STATUS descriptive
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
CODE middle_end/flambda2/term_basics/coercion.ml#change_depth
---
attr = Unroll n (n > 0), unrolling state of d is Not_unrolling, can_unroll(d)
--------------------------------------------------
decision = Begin_unrolling n = inline{unroll_to = Some n}
and S.Inline.Substitute then binds
  my_depth = Unroll_to(n, d₀)
  x_myclos = s_callee @ (Change_depth { from = d₀; to_ = my_depth })
NOTES: The Change_depth coercion rewrites the self-closure's depth from its
call-site value d₀ to the fresh my_depth so that the copied body's self-calls
see the Unrolling state. This is step 1 of unrolling.
```

```rule
RULE S.Inline.Unroll.Continue
STATUS descriptive
CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#make_decision0
CODE middle_end/flambda2/simplify/simplify_rec_info_expr.ml#known_remaining_unrolling_depth
---
remaining unrolling depth of d = m
  m > 0 ⇒ Continue_unrolling = inline{unroll_to = ⊥}
  m = 0 ⇒ Unrolling_depth_exceeded = keep
NOTES: On the self-call inside a body inlined by step 1, d has unrolling state
Unrolling{remaining_depth = m} (because its depth is Succ my_depth, decrementing
the count). Step 2 inlines while m > 0 and stops at 0, giving exactly n copies.
An unknown/free unrolling state falls through to the ordinary heuristic instead.
```

`Do_not_unroll` (produced e.g. when the callee simple is absent, §1) makes
`can_unroll` false and blocks all of this, so such copies never unroll.

## 4. What inlining enables

Inlining's payoff is not the substitution itself but the resimplification it
unlocks. After `S.Inline.Substitute` rewrites the `Apply`,
`simplify_direct_full_application` immediately calls `simplify_expr` on the
inlined body *in the same downwards pass*, with the caller's abstract
environment now covering the body: the arguments' Flambda types (chapter
[`07-types-domain.md`](07-types-domain.md)) flow into the parameters, so switch
scrutinees become known (`S.Rewrite.Switch.*`), projections of just-built blocks
fold away, indirect self-calls in the body become direct, and dead bindings are
reaped — all the rewrites of [§10](10-simplify-rewrites.md), plus parameter/result unboxing of
[§12](12-unboxing.md), applied to code that was opaque a moment earlier. The oracle's
`Removed_operations` accounting (§2.3) is precisely an estimate of this
downstream benefit, which is why speculative inlining runs a real simplification
pass to measure it.

## 5. Cross-module inlining (descriptive)

A callee defined in another compilation unit is inlinable only if its code — not
merely its `Code_metadata` — was exported in that unit's `.cmx` and imported
here. Per section 2.1, a declaration classified `Cannot_be_inlined` at its
definition site is *not* exported, so at a call site in another unit
`DE.find_code_exn` finds metadata but no code (or nothing at all), and
`S.Inline.Decision` step 2 returns `Missing_code` → `keep`. This is the reported
"the code could not be found (is a .cmx file missing?)" outcome and is the only
cross-module-specific behaviour in the inliner: everything else (the oracle, the
substitution, the depth accounting) is identical to the intra-unit case, with
the imported inlining arguments meet-combined exactly as in section 2.5 so that
the aggressiveness of the *defining* unit is respected. The cmx import/export
machinery itself (`Flambda_cmx`) is context only; see chapter
[`01-overview.md`](01-overview.md).
</content>
