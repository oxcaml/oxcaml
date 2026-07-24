# Unboxing

*Part of the Flambda 2 formalism; see [README.md](README.md).*

Unboxing replaces a value passed *boxed* (as a heap block, a boxed number, or a
single-entry closure) by its unboxed *components*, so that the allocation that
produced the box, and the loads that read it, can be simplified away. Flambda 2
performs unboxing in three distinct places, only the first of which is a genuine
Simplify rewrite:

1. **Continuation-parameter unboxing** (`simplify/unboxing/`, driven from
   `simplify/simplify_let_cont_expr.ml`). When a continuation's parameter is
   proven — from the join of its argument types over all use sites — to be a box
   of known shape, the parameter is replaced by fresh parameters for its
   components. Each use site passes the components (projecting them if
   necessary); the handler is simplified against a typing environment that
   equates the original parameter with the reconstructed box, so field reads
   collapse to the new parameters and the box becomes dead. This is the bulk of
   the chapter.

2. **Mutable unboxing** ("ref-to-var"), during the dataflow turn between the
   downwards and upwards passes (`simplify/flow/mutable_unboxing.ml`). A mutable
   block (typically an `int ref` / `float ref`) that is created locally and
   never escapes is dissolved into the SSA-style parameters of the continuations
   through which it is live; loads and stores become reads and rebindings of
   those parameters.

3. **Function parameter/result unboxing** (the `[@unboxable]` annotation). This
   is *not* a Simplify transformation: it is set up at CPS/closure-conversion
   time (`from_lambda/lambda_to_flambda.ml`,
   `from_lambda/closure_conversion.ml`) by generating a boxed *wrapper* around an
   unboxed *main* function. Simplify then removes the boxing using mechanisms 1
   and inlining. See [§5](#5-function-parameterresult-unboxing-wrappers).

The decision oracle for mechanism 1 is the judgment

```
Unbox?(param, uses) ∈ {unbox to shape U, keep}
```

declared in the [README](README.md); `U` ranges over the *decision* language of
[§2](#2-the-decision-language). Mechanism 2 has no per-parameter oracle: it
unboxes every non-escaping required mutable block it finds.

Throughout, `E` is a typing environment ([§07](07-types-domain.md)), `⊓` is meet ([§08](08-meet-join.md)), and the
provers `prove_X` are those of [§08](08-meet-join.md).

## 1. Where continuation-parameter unboxing hooks in

Unboxing is decided during the *downwards* traversal, when Simplify first enters
a continuation handler and has joined the argument types from the (known) use
sites into parameter types. The relevant call sites in
`simplify/simplify_let_cont_expr.ml`:

- Non-recursive handlers: `prepare_dacc_for_handlers` (with `~is_recursive:false`) calls
  `Unbox_continuation_params.make_decisions` with
  `Non_recursive arg_types_by_use_id` (all uses are known at this point), unless
  the handler is an inlinable single-use continuation, an exception handler, or a
  return/toplevel-return continuation — in those cases `make_do_not_unbox_decisions`
  is used and nothing is unboxed. Inlinable single-use continuations are left
  alone because the handler will be inlined at its one use and the typing
  environment there already avoids re-reading fields.
- Recursive handlers: `simplify_single_recursive_handler` calls
  `make_decisions` with `Recursive`. Not all use sites are known yet (the
  recursive back-edges are discovered while simplifying the handler), so the
  decision is made *optimistically* from the parameter type alone and is *not*
  refined against or filtered by use-site information (see the `Recursive`
  branch of `make_decisions`).

`make_decisions` returns an updated downwards environment (`DE.t`) — carrying the
equations that let the handler see the parameter as its components — together
with a `Decisions.t` recording, per parameter, the chosen decision. The
*extra parameters and arguments* (the new parameters and the projections at each
use) are computed later, on the upwards pass, by
`Unbox_continuation_params.compute_extra_params_and_args`, once every use site
(including recursive back-edges) is known.

```rule
RULE S.Unbox.ContParam.Hook
CLAIM descriptive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions
---
handler k(x̄) is Normal_or_exn or Define_root_symbol, not an exn handler,
    and (if non-recursive) not an inlinable single use
E ⊢ params x̄ have joined types T̄ from the use sites
--------------------------------------------------
Unbox decisions for x̄ are made downwards; extra params/args are emitted upwards
NOTES: Return, Toplevel_return, exn handlers, and inlinable single-use
  continuations are never unboxed. Recursive handlers decide optimistically
  without use-site refinement.
```

## 2. The decision language

A *decision* (`Unboxing_types.decision`) is either `Do_not_unbox reason` or
`Unbox U`, where `U` (`unboxing_decision`) is a tree describing how one box
decomposes into components. Each component is itself a `field_decision` carrying
its own nested `decision`, so unboxing recurses into fields.

```
decision      ::= Do_not_unbox reason
                | Unbox U

U             ::= Unique_tag_and_size { tag; shape; fields }      (block)
                | Variant { tag; const_ctors; fields_by_tag }     (variant)
                | Closure_single_entry { function_slot;           (closure)
                                         vars_within_closure }
                | Number (nnk, epa)                               (boxed number)

field_decision ::= { epa; decision; kind }
const_ctors   ::= Zero
                | At_least_one { is_int : epa; ctor : decision }
reason        ::= Not_beneficial | Max_depth_exceeded
                | Incomplete_parameter_type
                | Not_enough_information_at_use | Not_of_kind_value
                | Unboxing_not_requested | All_fields_invalid
```

An `epa` (`Extra_param_and_args.t`) is the unit of the transformation: a fresh
component parameter `param` (a `Variable.t` of the component's kind) together
with a map `args : Extra_arg.t` keyed by `Apply_cont_rewrite_id.t` — one entry
per use site, giving the argument to pass there. The shapes are:

- **`Number (nnk, epa)`** — the parameter is a boxed number of naked-number kind
  `nnk` (float, float32, int32/64, nativeint, immediate/tagged, vec128/256/512).
  `epa` is the single naked component. Tagged immediates are treated as a
  degenerate "boxed number" whose unboxing is untagging.
- **`Unique_tag_and_size { tag; shape; fields }`** — the parameter is a block of
  statically-known `tag`, `Block_shape.t` (value-only, mixed-record, or
  float-record), and size; `fields` is one `field_decision` per field.
- **`Variant { tag; const_ctors; fields_by_tag }`** — the parameter is a variant
  with possibly several non-constant constructors. `tag : epa` is a synthesized
  discriminator holding the block tag; `const_ctors` handles the constant
  constructors; `fields_by_tag` gives the field decisions for each possible
  scannable tag. See [§4](#4-variants).
- **`Closure_single_entry { function_slot; vars_within_closure }`** — the
  parameter is a set of closures containing exactly one closure (`function_slot`);
  each value slot becomes a `field_decision`.

The `Decisions.t` bundled per continuation additionally records
`rewrite_ids_seen` (use sites already accounted for) and
`rewrites_ids_known_as_invalid` (use sites at which passing the components is
impossible — the corresponding `Apply_cont` is unreachable / `Invalid`).

## 3. The oracle: optimistic decision, refinement, filtering

`Unbox?` is computed in three stages, per parameter, in `make_decisions`:

1. **Optimistic decision** from the parameter type alone
   (`Optimistic_unboxing_decision.make_optimistic_decision`).
2. **Refinement** against each use site's argument type, which both discards
   decisions that cannot be realized and accumulates the extra arguments
   (`refine_decision_based_on_arg_types_at_uses`, calling into
   `Unboxing_epa`). Skipped for recursive continuations.
3. **Benefit filtering** (`Is_unboxing_beneficial.filter_non_beneficial_decisions`),
   demoting to `Do_not_unbox Not_beneficial` any unboxing that would not remove
   real work. Skipped for recursive continuations.

### 3.1 Optimistic decision

The optimistic decision inspects the parameter type with the provers of [§08](08-meet-join.md),
trying number, then block, then variant, then closure, and recursing into
components. Two guards fire first: a parameter that is an alias to a symbol is
left alone (`Not_beneficial` — it is already statically allocated), and a
parameter whose kind is not `Value` is not of a shape we unbox
(`Not_of_kind_value`).

```rule
RULE S.Unbox.Optimistic.Number
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision
CODE middle_end/flambda2/simplify/unboxing/unboxers.ml
VERIFIED 14-validation/new-07-float-unbox.md @ c59c5780b0
---
E ⊢ prove_is_a_boxed_<nnk>(param_type) ⇒ Proved ()   for some naked-number kind nnk
--------------------------------------------------
Unbox?(param, uses) → Unbox (Number (nnk, fresh epa))
NOTES: The deciders are tried in order: immediate (untagging), float, float32,
  int32, int64, nativeint, vec128, vec256, vec512.
```

```rule
RULE S.Unbox.Optimistic.Block
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_fields
---
E ⊢ prove_unique_tag_and_size(param_type) ⇒ Proved (tag, shape, size)
depth < max_unboxing_depth
for each field i: fresh epaᵢ of kind element_kind(shape, i)
E' = E extended with definitions of the epaᵢ
E' ⊢ param_type ⊓ immutable_block tag shape (alias-types of epaᵢ) = T ▷ ε   (≠ ⊥)
--------------------------------------------------
Unbox?(param, uses) → Unbox (Unique_tag_and_size { tag; shape; fields })
  where fieldᵢ.decision = Unbox?(epaᵢ, uses) computed at depth+1 against field type
NOTES: The meet against the reconstructed block both checks feasibility and
  produces the field types that drive the recursive decisions. If the meet is
  ⊥ for all fields the decision becomes Do_not_unbox All_fields_invalid.
```

```rule
RULE S.Unbox.Optimistic.Closure
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_vars_within_closure
---
E ⊢ prove_single_closures_entry(param_type) ⇒ Proved (function_slot, _, entry, _)
not recursive
depth < max_unboxing_depth
--------------------------------------------------
Unbox?(param, uses) → Unbox (Closure_single_entry { function_slot;
  vars_within_closure = { w ↦ Unbox?(fresh epa_w, uses) | w ∈ value slots of entry } })
```

```rule
RULE S.Unbox.Depth
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
CODE driver/oxcaml_flags.ml#Flambda2.Expert
---
recursion depth ≥ max_unboxing_depth   (default 3, -flambda2-expert-max-unboxing-depth)
param_type is not a boxed number
--------------------------------------------------
Unbox?(param, uses) → Do_not_unbox Max_depth_exceeded
NOTES: Boxed numbers are decided before the depth check, so a number at any
  depth is still unboxed; the bound only limits nesting of blocks/variants/closures.
```

Numbers, blocks, variants, and closures are individually gated by the internal
booleans `unbox_numbers`, `unbox_blocks`, `unbox_variants`, `unbox_closures`
(all `true`). Variants and closures are additionally suppressed for recursive
continuations (`not recursive` above): a recursive continuation's parameter type
must be an invariant over the loop, and the code does not attempt variant/closure
unboxing there.

### 3.2 Refinement against uses and extra-argument computation

For a non-recursive continuation, each optimistic decision is walked against the
argument type at every use site (`Unboxing_epa`, entered through
`refine_decision_based_on_arg_types_at_uses`). This determines, for each
component `epa` and each use site (identified by its `Apply_cont_rewrite_id`),
*what argument to pass* there — recorded as an `Extra_arg.t`:

```
Extra_arg ::= Already_in_scope simple            (component already available)
            | New_let_binding (var, prim)         (compute it by a projection)
            | New_let_binding_with_named_args (var, args ↦ prim)   (wrapper case)
```

The core is `unbox_arg`, which given an *unboxer* (a projection primitive and a
prover for the component, `Unboxers.unboxer`) and the argument being unboxed at
this use, decides how the component is obtained:

```rule
RULE S.Unbox.ExtraArg.Available
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg
CODE middle_end/flambda2/simplify/unboxing/unboxers.ml
---
the argument at this use is a Simple s available in the use's typing env E_use
E_use ⊢ unboxer.prove_simple(alias_type_of s) ⇒ Known_result s'
--------------------------------------------------
extra arg at this use = Already_in_scope s'
NOTES: The component is already a Simple in scope (e.g. the box was itself built
  from a known field), so no projection is emitted — this is what makes the
  unboxing beneficial (see §3.3).
```

```rule
RULE S.Unbox.ExtraArg.Project
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg
---
the argument at this use is a Simple s available in E_use
E_use ⊢ unboxer.prove_simple(alias_type_of s) ⇒ Need_meet
--------------------------------------------------
extra arg = New_let_binding (fresh var, unboxer.unboxing_prim s)
NOTES: The component is not statically known; the use site gains a let-binding
  performing the projection (Untag_immediate / Unbox_number / Block_load /
  Project_value_slot) to compute it. This projection is a cost weighed in §3.3.
```

```rule
RULE S.Unbox.ExtraArg.Invalid
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg
---
E_use ⊢ unboxer.prove_simple(alias_type_of s) ⇒ Invalid
--------------------------------------------------
raise Invalid_apply_cont; this use site is recorded in rewrites_ids_known_as_invalid
NOTES: The argument's type proves the box cannot have this shape here, so control
  cannot actually reach this Apply_cont; it is treated as unreachable rather than
  blocking the unboxing.
```

When the use's argument is not available as a `Simple` (its type is not an alias
to a normal-mode name), refinement records
`Added_by_wrapper_at_rewrite_use { nth_arg }` and, on the `Filter` pass, may
`Prevent_current_unboxing` — turning the decision into
`Do_not_unbox Not_enough_information_at_use`. The `Poison` case supplies a poison
constant for components that are statically dead (e.g. the fields of a variant
tag that is not taken at a given use); poison arguments never count as
beneficial.

The two-pass structure (`pass = Filter | Compute_all_extra_args`) exists because
recursive back-edges are not known during the downwards `Filter` pass. `Filter`
decides feasibility from the known uses; `Compute_all_extra_args`, run upwards
once all uses are known, fills in the remaining extra args and must not
encounter a `Prevent_current_unboxing` (that would be a compiler bug).

```rule
RULE S.Unbox.Refine.Pass
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#refine_decision_based_on_arg_types_at_uses
CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_one_decision_and_use
---
pass = Filter: fold over known use sites; a Prevent_current_unboxing demotes the
    decision to Do_not_unbox Not_enough_information_at_use
pass = Compute_all_extra_args: fold over all use sites; fills remaining extra args
--------------------------------------------------
each component epa gains one Extra_arg per use site, or the use is marked invalid
```

### 3.3 Benefit filtering

An unboxing is *beneficial* at a component if, at some use site, its extra
argument is `Already_in_scope` with a non-poison value — i.e. the component was
already available and no projection had to be introduced. Equivalently: the box
was (at least sometimes) constructed near the call and the unboxing lets us skip
building/reading it. A block/variant/closure decision is beneficial iff any of
its (transitively reachable) components is; otherwise it is demoted to
`Do_not_unbox Not_beneficial`.

```rule
RULE S.Unbox.Beneficial
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#is_unboxing_beneficial_for_epa
CODE middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#filter_non_beneficial_decisions
---
Unbox U with no component epa having an Already_in_scope non-poison extra arg
    at any use site
--------------------------------------------------
demote to Do_not_unbox Not_beneficial
NOTES: Exception — Number Naked_immediate is always kept, since at worst it
  untags an integer (cheap). Boxed numbers other than immediates, and every
  block/variant/closure, must show a beneficial component.
```

### 3.4 Loopified boxed accumulators

Continuation-parameter unboxing composes with loopification ([§10](10-simplify-rewrites.md)
S.Rewrite.Loopify.Body) to determine exactly when a loop-carried boxed-number
accumulator loses its per-iteration allocation.

```rule
RULE S.Unbox.Loopify.AccumBoxElim
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
CAVEAT disclosure: both directions witnessed on float-accumulator variants (boxed exit keeps box, int_of_float exit removes it); no code-anchored proof.
---
Code c is loopified (S.Rewrite.Loopify.Body), self continuation k;
parameter pⱼ of k has a boxed-number declared subkind (boxed float / float32 /
  int32 / int64 / nativeint / vec*);
pⱼ is LOOP-VARYING: it is not eliminated by the alias/dominator analysis
  (S.Rewrite.Loopify.InvariantArgElim / BERK-2 takes precedence — an invariant
  boxed parameter is removed entirely, no thread of either kind, no allocation)
--------------------------------------------------
(1) the unboxing DECISION for pⱼ always fires, from the declared subkind alone: the
    recursive path gives the param unknown_with_subkind
    (S.Struct.Rec.InvariantVsVariant), on which prove_is_a_boxed_<nnk> is Proved
    (S.Unbox.Optimistic.Number); use-site refinement and the benefit filter are
    skipped for recursive continuations. For a loop-varying pⱼ the naked component
    parameter survives in the emitted loop, with the entry edge projecting it once
    from the boxed function parameter;
(2) the BOXED parameter pⱼ′ itself — and with it the box allocation on every
    back-edge — is eliminated IFF the handler, after rewriting under the
    param = box(component) equation (S.Unbox.Denv.Equation), does not consume the
    boxed value as a box on any path; in particular, returning a VARYING accumulator
    at a loop exit (apply_cont ret pⱼ′) keeps pⱼ′ ∈ required_names, so the residual
    loop threads BOTH the boxed and naked value and allocates a fresh box every
    iteration. No exit-edge re-boxing is ever introduced to break this.
NOTES: A performance-model theorem locating the exact loopification+unboxing payoff
boundary, invisible to local reasoning: the retention condition is a whole-body
required_names fact computed at the turn (S.Struct.Flow.RequiredNames →
S.Struct.Flow.UnusedParams for recursive conts). Number unboxing is not subject to
max_unboxing_depth (the depth check lives in the block/variant branch only) and
unbox_numbers is hardcoded true, so the decision is purely additive when
non-beneficial — a deliberate optimism worth recording. Witnessed both directions:
`let rec f (acc:float) x = if x=0 then acc else f (acc+.1.) (x-1)` keeps the
back-edge box (accumulator returned boxed at exit), while replacing the exit with
`int_of_float acc` removes it. The loop-varying premise is load-bearing: without
it, an invariant returned-boxed accumulator is eliminated WHOLESALE by
InvariantArgElim (no thread, no allocation). Composes: S.Rewrite.Loopify.Body,
S.Rewrite.Loopify.InvariantArgElim, S.Unbox.Optimistic.Number,
S.Unbox.Denv.Equation, S.Unbox.ContParam.Rewrite, S.Struct.Rec.InvariantVsVariant.
```

## 4. Variants

A variant parameter is unboxed by synthesizing three kinds of extra parameter:

- a **tag** discriminator `tag : epa` (naked immediate) holding the block tag,
  fixed at each use from the use's known constructor;
- for constant constructors, an **`is_int`** discriminator and optionally the
  **constant-constructor value** (both naked immediates), under
  `const_ctors = At_least_one { is_int; ctor }`; when there are no constant
  constructors, `const_ctors = Zero`;
- the **fields**, one `field_decision` list per possible scannable tag.

At each use, `compute_extra_args_for_variant` reads the use's `meet_variant_like`
to learn which constructors are live there. If only non-constant constructors are
possible it must be a single known tag; `is_int` is set false and that tag's
fields are projected, while the fields of the *other* tags are filled with poison
(they are dead on this branch). If only constant constructors are possible,
`is_int` is true, the constructor value is recovered by `meet_tagging_of_simple`,
and all block fields are poison. If both are possible at one use the unboxing is
prevented (`Prevent_current_unboxing`). The handler side installs the
discriminators via `Get_tag` and `Is_int` relations in the typing environment
(see [§6](#6-the-handler-side-typing-environment)).

The optimistic decision collapses a variant with no constant constructors and a
single non-constant tag into a plain `Unique_tag_and_size` (a block), rather than
carrying the redundant tag/`is_int` discriminators.

```rule
RULE S.Unbox.Optimistic.Variant
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant
---
E ⊢ prove_variant_like(param_type) ⇒ Proved { const_ctors; non_const_ctors_with_sizes }
not recursive
depth < max_unboxing_depth
at least one non-constant constructor's fields survive the meet
--------------------------------------------------
Unbox?(param, uses) → Unbox (Variant { tag; const_ctors; fields_by_tag })
  collapsing to Unique_tag_and_size when const_ctors = Zero and fields_by_tag is a singleton
```

```rule
RULE S.Unbox.Variant.Discriminator
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#extra_args_for_const_ctor_of_variant
---
at a use where only non-const ctors are possible: single tag t required;
    tag ← t, is_int ← false, tag t's fields projected, other tags' fields ← poison
at a use where only const ctors are possible:
    is_int ← true, ctor ← recovered immediate, all block fields ← poison
at a use where both are possible: Prevent_current_unboxing
--------------------------------------------------
extra args for tag, is_int, ctor, and each tag's fields at this use
```

## 5. Function parameter/result unboxing (wrappers)

The `[@unboxable]` annotation on a function parameter or on a function's return
does **not** change the calling convention (that is the role of layouts/jkinds).
Instead, at CPS/closure-conversion time the annotated function is compiled into
two code items: a *main* code item whose annotated parameters and/or return are
in unboxed form, and a small *boxed wrapper* (keeping the original function slot
and boxed calling convention) that unboxes the arguments, calls the main
function, and re-boxes the result. Because the wrapper is small and not marked
`[@inline never]` (that annotation applies only to the main function), Simplify
can inline the wrapper at a direct call site; the boxing/unboxing primitives then
cancel against the caller's own values and disappear. This is the strategy
documented in [`../unboxed_params.md`](../unboxed_params.md).

The set-up is entirely in `from_lambda`, out of Simplify's scope; this chapter
records the anchors for traceability but states no Simplify rule for it. The
criterion for a layout being unboxable is: boxed number, or a variant with no
constant constructors and exactly one block case with tag zero (records/pairs).

```rule
RULE S.Unbox.FunParam.Wrapper
CLAIM descriptive
CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps_function
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#compute_body_of_unboxed_function
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#make_unboxed_function_wrapper
---
a function has [@unbox_return] and/or a parameter with [@unbox_param], and is not a stub
--------------------------------------------------
CPS conversion emits an Unboxed_calling_convention: a boxed wrapper (original
  function slot) that unboxes params, calls the unboxed main function (fresh
  "_unboxed" function slot), and re-boxes the return; the actual removal of
  boxing is left to Simplify (wrapper inlining + §1 unboxing + constant folding)
NOTES: Cross-function unboxing performed later by the Reaper pass is out of scope
  for this chapter.
```

## 6. The handler-side typing environment

The rewrite has two halves that must stay in agreement: the *use side* passes
components (§3.2), and the *handler side* must be able to simplify the handler
body as if the box were still present. `Build_unboxing_denv.denv_of_decision`
extends the downwards environment so that, for each unboxing decision, the
original parameter is *equated* with the box rebuilt from its component
parameters.

```rule
RULE S.Unbox.Denv.Equation
CLAIM normative
CODE middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#denv_of_decision
CODE middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#add_equation_on_var
---
decision Unbox U for parameter param, with component parameters epa̅ (defined as
    extra variables of their kinds in the denv)
shape T built from U:  Number → boxed_<nnk>_alias_to epa;
    Unique_tag_and_size → immutable_block tag shape (alias-types of the field epas);
    Closure_single_entry → closure_with_at_least_these_value_slots;
    Variant → variant with const_ctors / non_const_ctors from the field epas
--------------------------------------------------
E_handler ⊢ param : T   (added by meeting param's alias type with T)
NOTES: This equation is what makes the handler's Block_load/Unbox_number/
  Project_value_slot on param reduce, via prove/meet, to the component parameters,
  so the box becomes dead. For variants the denv also records Get_tag and Is_int
  relations (add_get_tag_relation / add_is_int_relation) and the corresponding
  CSE bindings so the tag/is_int discriminators are recovered from the components.
```

Because the box is now recoverable from the components, the original parameter
is (usually) unused after simplification. On the upwards pass, `Apply_cont_rewrite`
records each parameter's usage via `decide_param_usage`; an unused original
parameter is dropped, and `make_rewrite` at each use site emits the
`New_let_binding` projections and the final argument list. If the handler *does*
still use the whole box, the original parameter is kept `Used` and each use site
continues to pass the original boxed argument alongside the components. So there
is no explicit "reconstruction" step: reconstruction is exactly the original
parameter being retained when needed.

```rule
RULE S.Unbox.ContParam.Rewrite
CLAIM descriptive
CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#add_extra_params_and_args
CODE middle_end/flambda2/simplify/apply_cont_rewrite.ml#create
CODE middle_end/flambda2/simplify/expr_builder.ml#rewrite_apply_cont
CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args
VERIFIED 14-validation/new-07-float-unbox.md @ c59c5780b0
CAVEAT disclosure: exact term shape is algorithm-dependent (a descriptive claim); trust rests on separately anchored ingredient rules plus the §8 example vs a real .simplify.reference.
---
Unbox?(x, uses) → Unbox U with component parameters x₁…xₙ (flattened from U)
E_handler ⊢ x : box(x₁…xₙ)     (S.Unbox.Denv.Equation)
for each use site r with Apply_cont k(…, s, …):  extra args ā_r for x₁…xₙ (§3.2)
--------------------------------------------------
E ⊢  Let_cont k(x̄) = e_handler in e_body
       ⇝  Let_cont k(x̄, x₁…xₙ) = e_handler′ in e_body′
where each Apply_cont k(…, s, …) at r becomes  L_r[ Apply_cont k(…, s, ā_r) ]
  (L_r the New_let_binding projections), e_handler′ is e_handler simplified under
  E_handler, and x (and any xᵢ) found unused is dropped by decide_param_usage
NOTES: The `Apply_cont_rewrite.t` carries the original parameters (each tagged
  `Used`/`Unused`/`Used_as_invariant` by `decide_param_usage`), the extra
  parameters x₁…xₙ, and, per use-site rewrite id, the `Extra_arg.t` list; the
  upwards `rewrite_apply_cont` appends those extra args (emitting the
  `New_let_binding` projections as the L_r lets) and drops the arguments for
  dropped parameters. Descriptive rather than normative because this exact term
  shape (extra-param threading, projection ordering) is algorithm-dependent. The
  individual ingredients (S.Unbox.Denv.Equation, S.Unbox.ExtraArg.*,
  S.Unbox.Beneficial) are separately anchored, and the §8 worked example
  exercises the whole rewrite against a real `.simplify.reference`.
```

## 7. Mutable unboxing (ref-to-var)

At the dataflow turn (between downwards and upwards traversal of a toplevel
expression), `simplify/flow/mutable_unboxing.ml` dissolves mutable blocks that
are created locally and never escape. A `Make_block` is a candidate iff it is
*required* (its result is live) and does *not* escape; escaping is computed by a
union of three analyses over the flow graph:

- **`escaping_by_alias`** — the block flows to something it is not equal to under
  the dominator alias map (it is aliased to a name that outlives it);
- **`escaping_by_use`** — the block (or an alias) appears somewhere other than as
  the block argument of one of the unboxable primitives (`Is_int`, `Get_tag`,
  `Block_load`, `Block_set`) — e.g. it is used whole in a handler, stored into
  another block, or captured in a closure environment;
- **`escaping_by_return`** — the block is among the arguments passed to the
  return or exception continuation.

Only non-escaping required makeblocks survive as `blocks_to_unbox` (a map from
the block `Simple` to its tag, mutability, and field kinds). Requiring
non-escaping *and* required is a correctness condition, not merely an
optimization: a non-required block does not mark its fields as escaping, so
unboxing it could drop the last reference to another unboxed block whose
constructing primitive has been removed.

```rule
RULE S.Unbox.Mutable.Candidate
CLAIM descriptive
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#blocks_to_unbox
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#escaping
CAVEAT disclosure: the claim's boundary is the completeness of the three escape analyses (especially escaping_by_use via New_let_binding dependencies and value-slot capture).
---
prim = Make_block { tag; mut; fields } binding var, reachable in the flow graph
Simple.var var is required (live)
Simple.var var ∉ escaping   (not escaping_by_alias ∪ escaping_by_use ∪ escaping_by_return)
--------------------------------------------------
var is unboxed: recorded in blocks_to_unbox with its tag and field kinds
```

Once the candidate blocks are known, a fixpoint over the control-flow graph
(`continuations_with_live_block`) computes, for each continuation, which unboxed
blocks are live *across* it (used below but not defined by it). Those blocks'
fields become extra parameters of the continuation, threaded like SSA values.
The toplevel continuation must have no such live block (it would mean a block
escaped to toplevel — a fatal error).

`Fold_prims.apply_prim` then turns each mutable primitive on an unboxed block
into a rewrite (`Named_rewrite`) over the local field bindings:

```rule
RULE S.Unbox.Mutable.Rewrite
CLAIM descriptive
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#Fold_prims.apply_prim
CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#compute_rewrites
VERIFIED 14-validation/new-07-float-unbox.md @ c59c5780b0
---
block b ∈ blocks_to_unbox with current field bindings fields
--------------------------------------------------
Make_block b            ⇝  remove_prim; fields ← the makeblock arguments
Block_load b field i    ⇝  replace_by_binding var ← fields[i]   (invalid if i absent)
Block_set b field i v   ⇝  remove_prim; fields[i] ← v           (invalid if i absent)
Is_int b                ⇝  replace_by_binding var ← false
Get_tag b               ⇝  replace_by_binding var ← b's static tag
NOTES: Because a Block_set updates the field binding threaded forward, a store
  followed by a load reads the stored value directly — this is exactly the
  ref-to-var effect. did_unbox_a_mutable_block distinguishes mutable blocks
  (which enable a further Simplify round) from immutable ones.
```

Aliasing is handled by canonicalizing every block reference through the
dominator alias map before lookup (`with_unboxed_block`, `with_unboxed_fields`),
so distinct names for the same block agree. Because escape analysis has already
established that the block is single-source and never aliased-out, threading its
fields as continuation parameters is sound. This is the mutable analogue of
continuation-parameter unboxing: the extra params/args produced here are merged
into the normal EPA machinery by `add_to_extra_params_and_args`.

## 8. Worked example: unboxing a float accumulator in a loop

Source (`testsuite/tests/flambda2/examples/float_unboxing.ml`):

```ocaml
let f () =
  let r = ref 0. in
  for i = 1 to 1000 do
    let x = float i in
    let y = if i mod 2 = 0 then x else x +. 1. in
    r := !r +. y
  done;
  !r
```

After CPS conversion the loop is a recursive continuation whose parameters are
the (boxed) loop counter and the (boxed) `float ref` accumulator, with a boxed
float threaded around the loop. Two unboxings apply. First, `r` is a mutable
`float ref` that never escapes (`!r` is read only through `Block_load`, `r := …`
only through `Block_set`, and the block is not returned as a block — only its
contents are), so mutable unboxing (§7) dissolves it into a threaded field.
Second, the recursive loop continuation's float accumulator parameter is a boxed
float of known shape, so continuation-parameter unboxing (§1–3) replaces it by a
naked-float parameter; the loop counter is similarly unboxed to a naked `int64`.

The simplified result (`float_unboxing.simplify.reference`), lightly abridged:

```
cont k3 (1L, 0x0p+0)                         (* enter loop: counter=1, acc=0.0 *)
  where rec k3 (for_counter_naked : int64, unboxed_float : float) =
    let prim   = %num_conv.[int64].[imm] (for_counter_naked) in
    let i      = %tag_imm (prim) in
    let prim_1 = %num_conv.[imm].[float] (prim) in           (* x = float i *)
    ((let int_mod = %int_barith.mod (i, 2) in
      let prim_2  = %phys_eq (int_mod, 0) in
      switch prim_2 | 0 -> k5 | 1 -> k4 (prim_1)
        where k5 = let prim_3 = %bfloat_arith.add (prim_1, 0x1p+0) in
                   cont k4 (prim_3))
       where k4 (unboxed_float_1 : float) =                  (* y, unboxed *)
         let prim_2 = %bfloat_arith.add (unboxed_float, unboxed_float_1) in
         let for_next_naked = %int_barith.int64.add (for_counter_naked, 1L) in
         let prim_3 = %int_comp.int64.le (for_next_naked, 1000L) in
         switch prim_3
           | 0 -> k2 (%box_num.float (prim_2))               (* exit: re-box once *)
           | 1 -> k3 (for_next_naked, prim_2))               (* back-edge: naked float *)
```

Everything inside the loop now operates on naked floats and a naked `int64`
counter: `%bfloat_arith.add` consumes and produces unboxed floats, the back-edge
`k3 (for_next_naked, prim_2)` passes the naked accumulator directly (an
`Already_in_scope` extra arg — no projection, hence beneficial per
S.Unbox.Beneficial), and the only box that survives is the single
`%box_num.float` on the loop-exit edge to `k2`, which produces the boxed result
the function must return. The `ref` cell and every `!r` load / `r := …` store
have vanished (S.Unbox.Mutable.Rewrite), and the boxed float that would have been
threaded around the loop is gone (S.Unbox.ContParam.Rewrite with the naked-float
component parameter `unboxed_float`).

## Summary and open questions

`S.Unbox.*` rules introduced in this chapter:

| Rule | What |
|---|---|
| `S.Unbox.ContParam.Hook` | where/when continuation-param unboxing runs |
| `S.Unbox.Optimistic.Number` | boxed-number optimistic decision |
| `S.Unbox.Optimistic.Block` | block optimistic decision (meet feasibility + field types) |
| `S.Unbox.Optimistic.Variant` | variant optimistic decision |
| `S.Unbox.Optimistic.Closure` | single-entry closure optimistic decision |
| `S.Unbox.Depth` | max-unboxing-depth cutoff |
| `S.Unbox.ExtraArg.Available` / `.Project` / `.Invalid` | how each component arg is obtained at a use |
| `S.Unbox.Refine.Pass` | Filter vs Compute_all_extra_args two-pass structure |
| `S.Unbox.Beneficial` | the benefit filter |
| `S.Unbox.Loopify.AccumBoxElim` | loop-carried boxed-accumulator box elimination boundary |
| `S.Unbox.Variant.Discriminator` | tag / is_int / ctor extra args at uses |
| `S.Unbox.FunParam.Wrapper` | `[@unboxable]` wrapper set-up (from_lambda) |
| `S.Unbox.Denv.Equation` | handler-side "param = box(components)" equation |
| `S.Unbox.ContParam.Rewrite` | the overall Let_cont rewrite |
| `S.Unbox.Mutable.Candidate` | non-escaping required makeblock selection |
| `S.Unbox.Mutable.Rewrite` | ref-to-var primitive rewrites |

The **decision shape language** ("decisions") is
`Unboxing_types.decision`/`unboxing_decision`, summarized in
[§2](#2-the-decision-language): `Number | Unique_tag_and_size | Variant |
Closure_single_entry`, each carrying `Extra_param_and_args` (`epa`) components,
recursively. `Extra_arg` (`Already_in_scope | New_let_binding |
New_let_binding_with_named_args`) is the per-use-site payload.

**Function parameter/result unboxing** lives in `from_lambda` (wrapper
generation, `S.Unbox.FunParam.Wrapper`), not in Simplify; Simplify only removes
the resulting boxing by inlining the wrapper and applying §1 unboxing. The
Reaper pass performs a further, cross-function unboxing that is out of scope here
(see [`../reaper.md`](../reaper.md)).

Open questions / conjectures to verify:

1. Whether the "beneficial" criterion (§3.3) can ever demote a decision whose
   handler-side equation was already installed in the denv, and if so whether the
   denv equation is harmlessly redundant or must be rolled back. The code
   installs the denv from the *filtered* decision (`denv_of_decision` is called
   after `filter_non_beneficial_decisions`), so this appears consistent, but it
   is worth confirming.
2. The precise interaction between continuation lifting (extra params added for
   lifted continuations) and unboxing decisions on recursive vs non-recursive
   continuations — the `Rec`/`Non_rec` split in
   `compute_extra_params_and_args` and the comments there suggest a subtlety
   about invariant params that a validation study should pin down.
4. Mutable unboxing's soundness condition is stated (§7) as "non-escaping and
   required"; the exact completeness of the three escape analyses (especially
   `escaping_by_use` via `New_let_binding` dependencies and value-slot capture)
   deserves an adversarial check against `tests/ref_to_var/`.
