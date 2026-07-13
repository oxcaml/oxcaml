# Simplify rewrites: the local transformations

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter states the individual rewrites Simplify performs as rules of the
judgment

```
E ⊢ e ⇝ e′
```

"in typing environment `E`, Simplify rewrites `e` to `e′`". Chapter
[`09-simplify-structure.md`](09-simplify-structure.md) owns the *traversal* that
decides when and where each rewrite fires (the downwards/upwards split, the
accumulators `dacc`/`uacc`, flow analysis, lifting, join points); this chapter
owns the *rewrites themselves* and their side conditions. Function inlining
(chapter [`11-inlining.md`](11-inlining.md)) and unboxing (chapter
[`12-unboxing.md`](12-unboxing.md)) are the two rewrite families large enough to
have their own chapters; everything else local lives here.

Every side condition is a query against the abstract domain — a prover
(`prove_*`) or a meet (`meet_*`) from chapter [`08-meet-join.md`](08-meet-join.md)
— or an effects/coeffects classification of a primitive. We cite provers by code
anchor (`provers.ml#...`), never by rule ID, because chapters 07 and 08 are
written concurrently.

## How to read a rewrite

Two structural facts about Simplify shape every rule below.

**Downwards records, upwards commits.** Almost every rewrite is *decided* on the
downwards pass (where types are known) but *committed* on the upwards pass (where
free-name and cost information is exact). For a switch, for example,
`simplify_switch_expr.ml#simplify_switch` prunes arms downwards and
`simplify_switch_expr.ml#rebuild_switch` emits the replacement expression
upwards. The rules state the net source-to-source effect `e ⇝ e′`; where the two
halves live in different functions we anchor both.

**Canonicalization is pervasive.** Before any other rewrite looks at a `Simple`,
that `Simple` has already been replaced by the canonical element of its alias
class (rule `S.Rewrite.Alias.Canonicalize`). So "the argument is the constant
`c`" in a folding rule always means "the argument's canonical form is `c`", and
"replace the primitive by an alias to `s`" means Simplify binds the result
variable to `s`'s canonical form.

### The result carrier for `named`

Rewrites of a `let`-bound `named` do not produce an expression directly. They
produce a `Simplify_primitive_result.t` (`simplify_primitive_result.mli`,
abbreviated `SPR`), whose payload is a `Simplified_named.t Or_invalid.t` plus a
`try_reify` flag and the updated `dacc`. The four shapes that matter here:

- `SPR.create named ~try_reify dacc` — keep the (possibly rewritten) defining
  expression `named`, with `result_var`'s type set in `dacc`;
- `SPR.create_simplified` — same, for an already-built `Simplified_named.t`
  (used by CSE);
- `SPR.create_invalid dacc` — the binding is unreachable;
- `SPR.create_rewritten` — emit a small expression in place of the binding.

`simplify_named.ml#simplify_named0` turns an `Invalid` payload into a `let` whose
body is discarded (rule `S.Rewrite.Let.Invalid`), and, when `try_reify` is set,
runs reification (rule `S.Rewrite.Prim.Reify`).

## The abstract transfer function

The judgment `⟦p⟧♯(T̄) = T ▷ ε` is the abstract counterpart of the primitive
denotation `⟦p⟧` (chapters 05–06): given the types `T̄` of `p`'s arguments it
returns a type `T` for the result together with an environment extension `ε`
recording any *relational* facts (see below). This chapter defines `⟦p⟧♯` only
through the rewrites that consult it; the per-primitive type computations are
implemented in the `simplify_*_primitive.ml` files and rest on the type
constructors (`T.this_*`, `T.these_*`, `T.box_*`, `T.tag_immediate`, …) owned by
[§07](07-types-domain.md).

```rule
RULE S.Rewrite.Prim.Transfer
STATUS normative
CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
---
n = Prim(p, dbg)   where p = p₀(s₁ … sₙ)
E ⊢ sᵢ ⇝ sᵢ′       (each argument canonicalized; Tᵢ = type of sᵢ′ in E)
kind(Tᵢ) = expected argument kind of p₀ for every i
⟦p₀(s₁′ … sₙ′)⟧♯(T₁ … Tₙ) = T ▷ ε   (no more specific rewrite below applies)
--------------------------------------------------
E ⊢ (let x = Prim(p, dbg) in e) ⇝ (let x = Prim(p₀(s₁′ … sₙ′), dbg) in e)
  with x : T and E extended by ε for the simplification of e
NOTES: The base case: arguments are canonicalized and the primitive is kept, but
  the result variable gains the type computed by the per-primitive analysis. All
  folding/strength-reduction rules below are refinements that additionally
  replace the primitive by a constant, an alias, or a cheaper primitive. The kind
  check is `simplify_primitive.ml#arg_kind_mismatch`.
```

```rule
RULE S.Rewrite.Prim.ArgKindMismatch
STATUS normative
CODE middle_end/flambda2/simplify/simplify_primitive.ml#arg_kind_mismatch
CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
---
n = Prim(p, dbg)   p₀ expects argument kinds κ̄
some argument's type has kind ≠ the corresponding κᵢ
Flambda_features.kind_checks() = false
--------------------------------------------------
E ⊢ (let x = Prim(p, dbg) in e) ⇝ Invalid
NOTES: A kind mismatch on a primitive's arguments can only arise in dead code
  (the argument's type would be Bottom for the expected kind). With kind checks
  on, this is instead a fatal compiler error. Sound because the binding is
  unreachable; see S.Rewrite.Invalid.Propagate.
```

Relational primitives — `Is_int`, `Get_tag`, `Is_null` — return an ordinary
immediate type for the result *and* record a relation between the result and the
scrutinee in the environment extension `ε`. The relation lets a later meet
recover the scrutinee's shape from a test on the result (e.g. learning inside a
`switch` arm that `Is_int x = 1` that `x` is an immediate).

```rule
RULE S.Rewrite.Prim.Relational
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_relational_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag
---
n = Prim(is_int(x))   [resp. get_tag(x), is_null(x)]
x is not resolved to a constant by the prover
--------------------------------------------------
E ⊢ (let r = Prim(is_int(x)) in e) ⇝ (let r = Prim(is_int(x)) in e)
  with ε = add_is_int_relation(r, x)   [resp. add_get_tag_relation, add_is_null_relation]
NOTES: The result variable is related to the scrutinee rather than folded. Only
  the general `is_int` (`variant_only = false`) consults provers.ml#prove_is_int
  and folds to a constant when the scrutinee's type already determines the answer
  (S.Rewrite.Prim.ConstFold); `is_int` with `variant_only` set (the variant
  discriminator case), and `get_tag` and `is_null`, always take the relational
  path via simplify_relational_primitive. `get_tag` never folds to a constant tag
  directly; the constant is recovered downstream by meet/reify through the
  relation.
```

## Aliases and canonicalization

The single most frequently applied rewrite: replace every `Simple` by the
canonical member of its alias equivalence class in the typing environment,
carrying coercions correctly.

```rule
RULE S.Rewrite.Alias.Canonicalize
STATUS normative
CODE middle_end/flambda2/simplify/simplify_simple.ml#simplify_simple0
CODE middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn
---
s occurs in a term position requiring name mode ≥ m
E gives s the type T and canonical simple s_can at mode m (via type_simple_in_term_exn)
--------------------------------------------------
E ⊢ s ⇝ s_can
NOTES: `s_can` is the representative of s's alias class that is available at the
  requested name mode. If s carried a coercion co, the result is
  s_can₀ @ (simplify co), i.e. the coercion is re-simplified and composed onto
  the underlying canonical simple (simplify_simple.ml#simplify_simple0,
  simplify_coercion.ml#simplify_coercion). Sound because the typing environment
  only makes s and s_can aliases when γ_E forces them equal at runtime; the
  min-name-mode requirement guarantees s_can is actually in scope at the use
  site. Applied to every Simple in every rewrite via S.simplify_simple(s).
```

## Constant folding

The general principle couples Simplify to the operational semantics: if the
arguments are proven to be specific constants and the primitive's denotation
`⟦p⟧` is defined on them, the primitive is replaced by the constant it computes,
and that constant equals the runtime result.

```rule
RULE S.Rewrite.Prim.ConstFold
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unary_primitive
VERIFIED 14-validation/new-01-constfold.md
VERIFIED 14-validation/new-05-inline-fold.md
---
p = p₀(s₁ … sₙ)
for each i the prover proves sᵢ equals a constant cᵢ (Known_result)
⟦p₀⟧(c₁ … cₙ; H) = (c, H)   (denotation defined, no heap effect)
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ (let x = c in e)   with x : {c}
NOTES: The folded constant is exactly ⟦p₀⟧ applied to the argument constants —
  this is the key soundness coupling to chapters 05–06 (P.*). Implemented per
  primitive family: integer/float arithmetic and comparison via the
  `Binary_arith_like` functor's "both sides Known_result" branch, which uses
  `Numeric_types`/`Target_ocaml_int` semantics modelling target-width wraparound
  and IEEE bit patterns exactly; unary arithmetic, conversions, byte-swap,
  bit-reinterpretation, and boolean-not in simplify_unary_primitive.ml. Result
  type is `T.these_*` of the (bounded, ≤10) set of possible constants; a
  singleton set collapses to the constant. Sound because γ_E(Tᵢ)={cᵢ} means each
  argument's only runtime value is cᵢ, so the machine computes ⟦p₀⟧(c̄).
```

```rule
RULE S.Rewrite.Prim.ConstFold.Float
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen
---
p is a float arithmetic primitive with all arguments proven constant
DE.propagating_float_consts(denv) = true
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ (let x = c in e)   c = ⟦p⟧ under IEEE_semantics
NOTES: Float folding is gated on the `propagating_float_consts` flag (the
  argument's constant must be preserved bit-for-bit, including NaN payloads and
  signed zeros). Descriptive because the flag can change; the computed value,
  when folding does occur, is normative (S.Rewrite.Prim.ConstFold).
```

```rule
RULE S.Rewrite.Prim.ConstFold.PartialUndef
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like
---
p = div(x, 0) or mod(x, 0)   (integer division/remainder by a proven-zero divisor)
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ Invalid
NOTES: Division or remainder by zero is undefined (`⟦div⟧(_, 0) = undef`,
  [§05](05-primitives-scalar.md)). Simplify only reaches a proven-zero divisor in unreachable code
  (a live division by zero would have trapped), so it emits Invalid rather than a
  value. Realized as the `Invalid` outcome of `op_lhs_unknown` (the divisor is
  the known right-hand side) and of `op` (both sides known: `I.Num.div`/`mod_`
  return `None`) in `Int_ops_for_binary_arith`, which `check_possible_results`
  turns into `SPR.create_invalid`.
```

After folding (or after any primitive whose result type is precise enough),
Simplify may replace the binding by a reference to a lifted constant.

```rule
RULE S.Rewrite.Prim.Reify
STATUS normative
CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
CODE middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify
---
let x = Prim(p) simplified with try_reify = true
x : T   and reification finds T concretizes to a single value representable as
  a constant c or a symbol sym (allow_lifting requires p only_generative_effects)
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ (let x = c in e)   [or a reference to sym]
NOTES: Reification reads the *type* rather than the primitive: if x's type pins
  it to one value it can be materialized directly. Lifting a fresh
  statically-allocated constant (`sym`) is only allowed when the primitive has at
  most generative effects and x is at normal mode, so a projection out of a
  larger structure is not mistaken for an allocation. Sound because γ_E(T)={c}.
```

## Strength reduction and algebraic identities

These rewrites replace a primitive by a cheaper primitive or by an alias, using
only *partial* knowledge of the arguments. Only the identities that exist in the
code are documented; notably there is **no** power-of-two multiply → shift
rewrite (integer multiply folds only for the constants 0, 1 and −1), and **no**
`x + 0.0` float rewrite (it is unsound for signed zeros).

```rule
RULE S.Rewrite.Prim.IntIdentity
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift
---
p is an integer binary arithmetic/logical/shift primitive
one operand is proven to be a neutral/absorbing constant for that operation
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ e′  where the primitive is replaced per the table
  (the named constant is the *known* operand; the other may be unknown, and for
  shifts the left operand is the value and the right operand the shift amount):
  x+0, x-0, x|0, x^0, x*1, x&(-1), x/1, x<<0, x>>>0, x>>0   →  the other operand
  x*0, x&0, x%1, x%(-1), 0<<n, 0>>>n, 0>>n                  →  the constant 0
  x|(-1), (-1)>>n                                           →  the constant -1
  x*(-1), 0-x, x/(-1)                                       →  Sub(0, other) (negation)
NOTES: Realized by the one-side-known outcomes `The_other_side`, `Exactly v`,
  `Negation_of_the_other_side` of `op_lhs_unknown`/`op_rhs_unknown`/
  `symmetric_op_one_side_unknown`. Sound by the ring/lattice identities of
  fixed-width two's-complement arithmetic, which hold bit-exactly under
  wraparound. `div`/`mod` by 0 is S.Rewrite.Prim.ConstFold.PartialUndef. Out-of-
  range shift amounts are *not* simplified (they cannot be made Invalid, as the
  IR is type-safe).
```

```rule
RULE S.Rewrite.Prim.FloatIdentity
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen
---
p is a float binary arithmetic primitive; DE.propagating_float_consts = true
one operand is a proven constant that is a bit-pattern-preserving neutral element
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ e′ where:
  mul by 1.0, div by 1.0     →  the other operand
  mul by -1.0, div by -1.0   →  Neg(other)   (float negation)
NOTES: Only multiply/divide by ±1.0 are simplified — these preserve every bit of
  the other operand (the code carries `[@z3 ...]` machine-checkable obligations
  to that effect). `x + 0.0` and `x - 0.0` are deliberately *not* simplified
  because +0.0 + (−0.0) = +0.0 ≠ −0.0. Sound only because the surviving identities
  are bitwise-exact for all IEEE inputs including NaN and signed zeros.
```

```rule
RULE S.Rewrite.Prim.UntagTag
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number
VERIFIED 14-validation/code_size_of_boolean_not_switch.md
---
p = untag_imm(x)   [resp. unbox_number(x)]
E proves x's type carries an alias to a naked immediate s [resp. naked number s]
  (via the projection shape `tagged_immediate_alias_to` [resp. `boxed_*_alias_to`])
--------------------------------------------------
E ⊢ (let r = Prim(untag_imm(x)) in e) ⇝ (let r = s in e)
NOTES: `Untag(Tag s) → s` and `Unbox(Box s) → s`, realized structurally: the
  forward `tag_imm`/`box_number` sets x's type to record the payload alias s, so
  the projection's meet binds r to s. The inverse direction (`Tag(Untag ...)`,
  `Box(Unbox ...)`) is recovered by CSE: unbox/untag add a CSE equation for the
  matching box/tag (unbox only when the alloc mode is provably Heap). Sound
  because the alias type faithfully records that the boxed/tagged value's payload
  equals s.
```

```rule
RULE S.Rewrite.Prim.Projection
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load
---
p reads a field of an immutable aggregate at a known position:
  Block_load(imm, x, i)  |  Project_value_slot(w, x)  |  Project_function_slot(f, x)
  |  Array_load(imm array, x, i)  |  Array_length(x)  |  String_length(x)
E proves x's type records the field/slot/length as a Simple s (or constant)
  (meet_block_field_simple / meet_project_value_slot_simple / prove_is_immutable_array …)
--------------------------------------------------
E ⊢ (let r = Prim(p) in e) ⇝ (let r = s in e)
NOTES: Reading a component of a *provably immutable* aggregate at compile time.
  Immutability is essential: mutable block/array loads (`simplify_mutable_block_
  load`) never fold. For `Array_load` the index must also be a proven constant
  and in bounds (out of bounds ⇒ Invalid). Closure value/function slots are always
  immutable. Sound because γ_E pins the aggregate's contents, and immutability
  guarantees the runtime component equals the recorded s.
```

```rule
RULE S.Rewrite.Prim.PhysEqual
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal
CODE middle_end/flambda2/types/provers.ml#prove_physical_equality
---
p = phys_equal(x, y)
prove_physical_equality proves the two values definitely equal or definitely distinct → b
--------------------------------------------------
E ⊢ (let r = Prim(phys_equal(x, y)) in e) ⇝ (let r = b in e)
NOTES: The prover only concludes when the addresses provably coincide (same
  canonical simple) or provably differ; otherwise the primitive is kept. Sound
  because physical equality is referential identity, which the prover respects.
```

```rule
RULE S.Rewrite.Prim.CompareRecovery
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#recover_comparison_primitive
CODE middle_end/flambda2/simplify/comparison_result.ml#convert_result_compared_to_tagged_zero
---
p = cmp((compare x y), 0)   where cmp is an integer relation against the constant 0
  and `compare x y` was recorded as a comparison result in denv
--------------------------------------------------
E ⊢ (let r = Prim(cmp((compare x y), 0)) in e) ⇝ (let r = Prim(x ⟨cmp⟩ y) in e)
NOTES: `(compare x y) <op> 0` collapses to the direct relation between x and y,
  trying both operand orders (flipping Lt↔Gt etc.). Descriptive: relies on denv
  having recorded the `compare` result. Sound because `compare x y <op> 0` is
  equivalent to the corresponding direct order relation.
```

```rule
RULE S.Rewrite.Prim.ObjDupElide
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_obj_dup
---
p = obj_dup(x)   and x's type proves it is an immutable boxed/tagged number or a string
--------------------------------------------------
E ⊢ (let r = Prim(obj_dup(x)) in e) ⇝ (let r = x in e)
NOTES: Duplicating an immutable heap value is observationally a no-op, so the copy
  is elided (for local-mode boxed numbers it becomes an unbox-then-rebox instead).
  Sound because physical identity of an immutable value is not observable in a way
  a duplicate could change.
```

## Common subexpression elimination

CSE remembers, per primitive application, the variable it was bound to; a later
syntactically-equal application in scope is replaced by an alias to that
variable. Eligibility is by effects/coeffects class plus a per-primitive
predicate: *block loads and closure projections are deliberately excluded* —
the information they carry is propagated through types (the
`S.Rewrite.Prim.Projection` family), not through CSE — but *immutable array
loads and header reads (`Is_int`, `Get_tag`, lengths) are eligible*, because
types cannot track, say, a load at a variable index, while CSE can.

```rule
RULE S.Rewrite.CSE.Eligible
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#Eligible_for_cse.create
CODE middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse
CODE middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse
VERIFIED 14-validation/cse_immutable_array_load_var_index.md
VERIFIED 14-validation/cse_immutable_array_load.md
---
p is eligible for CSE iff:
  its per-primitive eligibility predicate holds (see NOTES for the shape), and
  at least one argument is a variable, and
  effects_and_coeffects(p) ∈ { (No_effects, No_coeffects),
                                (Only_generative_effects Immutable, No_coeffects) }
--------------------------------------------------
(no rewrite by itself — this is the side condition for S.Rewrite.CSE.Replace/Extend)
NOTES: The per-primitive predicate excludes primitives whose results are already
  propagated through types: Block_load, Project_function_slot/Project_value_slot,
  Duplicate_block/array, and all mutable or raw loads (mutable Array_load,
  String_or_bigstring_load, Bigarray_load). It *includes* immutable and
  immutable_unique Array_load — a genuine load that CSE, not type-based
  projection, deduplicates (decisive when the index is a variable; see the
  VERIFIED case study) — as well as the header-read family Is_int, Is_null,
  Get_tag, Get_header, Array_length, String_length, Bigarray_get_alignment,
  arithmetic/comparison primitives, and construction of immutable blocks (so
  identical immutable allocations may be shared). Anything with coeffects (a
  read of mutable state) or non-generative/mutable effects is never CSE'd. The
  "≥1 variable argument" clause avoids CSEing all-constant primitives (those
  fold instead).
```

```rule
RULE S.Rewrite.CSE.Replace
STATUS normative
CODE middle_end/flambda2/simplify/simplify_primitive.ml#try_cse
CODE middle_end/flambda2/simplify/simplify_primitive.ml#apply_cse
VERIFIED 14-validation/cse_immutable_array_load.md
VERIFIED 14-validation/cse_immutable_array_load_var_index.md
VERIFIED 14-validation/issue5721.md
VERIFIED 14-validation/new-04-cse.md
---
p eligible (S.Rewrite.CSE.Eligible), min name mode = normal
denv's CSE table maps p (with canonicalized arguments) to a simple s in scope
--------------------------------------------------
E ⊢ (let x = Prim(p) in e) ⇝ (let x = s in e)   with x : alias-of s
NOTES: The prior equal application already computed the value bound to s; reusing
  it is sound because p is pure (or an immutable allocation, where sharing changes
  only identity of an immutable value, which is not observable). CSE runs *before*
  the per-primitive analysis in `simplify_primitive`. Only fires at normal name
  mode.
```

```rule
RULE S.Rewrite.CSE.Extend
STATUS normative
CODE middle_end/flambda2/simplify/simplify_primitive.ml#try_cse
CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#T0.add
VERIFIED 14-validation/new-04-cse.md
---
p eligible, no prior equal application in scope
--------------------------------------------------
E ⊢ (let x = Prim(p) in e): record p ↦ x in the CSE table, keep the binding
NOTES: The CSE table is scoped (`by_scope`/`combined` in
  common_subexpression_elimination.ml) so equations survive across join points
  only where valid in every predecessor. No source-to-source change here; this is
  the bookkeeping half of S.Rewrite.CSE.Replace.
```

## Switch simplification

A `switch` scrutinizes a naked immediate against integer discriminants, each arm
an `apply_cont`. Simplify prunes arms by type, merges arms that agree, recognizes
the identity and boolean-not patterns (replacing the switch by arithmetic), and
emits `Invalid` when nothing survives.

```rule
RULE S.Rewrite.Switch.ArmPrune
STATUS normative
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm
VERIFIED 14-validation/array_element_kind_meet.md
VERIFIED 14-validation/new-02-known-switch.md
VERIFIED 14-validation/new-08-nested-switch.md
---
switch on scrutinee x with arms { dᵢ → kᵢ āᵢ }
E ⊢ (type of x) ⊓ {dᵢ} = ⊥   for some arm dᵢ
--------------------------------------------------
E ⊢ switch x { …, dᵢ → kᵢ āᵢ, … } ⇝ switch x { … (arm dᵢ removed) … }
NOTES: An arm whose discriminant is incompatible with the scrutinee's type is
  unreachable and dropped (the meet against `this_naked_immediate dᵢ` is Bottom).
  Sound because γ_E excludes dᵢ from x's runtime values. "Known scrutinee" is the
  special case where x's type is a single immediate: every arm but one prunes, and
  the survivor is emitted directly (S.Rewrite.Switch.Merge).
```

```rule
RULE S.Rewrite.Switch.Merge
STATUS normative
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_arm
VERIFIED 14-validation/new-02-known-switch.md
---
after pruning, every surviving arm targets the same continuation k with args that
  intersect (over alias sets) to a common list ā, and no arm has a trap action
--------------------------------------------------
E ⊢ switch x { d₁ → k ā₁, …, dₘ → k āₘ } ⇝ apply_cont k ā
NOTES: Covers both the single-surviving-arm case and the all-arms-identical case
  (they share the `switch_merged` path). Args are merged by intersecting alias
  sets so a common argument survives even if written via different names. The
  scrutinee is no longer needed. Sound because whichever arm would be taken jumps
  to k ā regardless.
```

```rule
RULE S.Rewrite.Switch.Identity
STATUS normative
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch
VERIFIED 14-validation/n_way_join_null.md
---
switch on x, every arm d → k [d]  (arm passes its own discriminant, tagged, to one k)
--------------------------------------------------
E ⊢ switch x { d → k [d] | d ∈ D } ⇝ (let t = tag_imm(x) in apply_cont k [t])
NOTES: The switch is an identity map through tagging, replaced by a single
  `Tag_immediate`. Sound because for every reachable discriminant d, tagging x
  yields exactly the value the arm would have passed.
```

```rule
RULE S.Rewrite.Switch.BooleanNot
STATUS normative
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch
VERIFIED 14-validation/code_size_of_boolean_not_switch.md
VERIFIED 14-validation/new-08-nested-switch.md
---
switch on x with discriminants exactly {0, 1}, arm 0 → k [1] and arm 1 → k [0]
--------------------------------------------------
E ⊢ switch x { 0 → k [1]; 1 → k [0] } ⇝
      (let t = tag_imm(x) in let n = boolean_not(t) in apply_cont k [n])
NOTES: Recognizes a two-arm boolean switch that negates its scrutinee and
  replaces it by `Boolean_not` on the tagged scrutinee. This is how switches
  produced by `Is_int` or integer comparisons (whose results are the booleans
  0/1) become straight-line arithmetic. Sound because for x ∈ {0,1}, boolean_not
  computes the swapped value each arm supplies.
```

```rule
RULE S.Rewrite.Switch.Invalid
STATUS normative
CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#rebuild_switch
---
after pruning, zero arms survive
--------------------------------------------------
E ⊢ switch x { … } ⇝ Invalid   (reason Zero_switch_arms)
NOTES: If the scrutinee's type is Bottom, or every arm's discriminant is
  incompatible with it, no arm remains and the switch is unreachable. Sound
  because γ_E(type of x) = ∅ means control never reaches the switch. See
  S.Rewrite.Invalid.Propagate.
```

The large-switch lookup-table and affine rewrites
(`rebuild_switch_with_single_arg_to_same_destination`,
`rebuild_affine_switch_to_same_destination`, gated on ≥3 arms) are size
optimizations that replace a switch feeding one destination by `Int_arith`
multiply/add; they are **descriptive** and not given a rule here.

## Let bindings: dead code

A pure (or merely allocating) binding whose bound variable is unused is deleted.
The side condition is exactly the effects/coeffects classification: a defining
expression with *more than generative effects* must be kept for its effects even
if unused.

```rule
RULE S.Rewrite.Let.DeadBinding
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
CODE middle_end/flambda2/terms/flambda_primitive.ml#at_most_generative_effects
VERIFIED 14-validation/array_element_kind_meet.md
---
let x = n in e
n has at most generative effects (Named.at_most_generative_effects n)
x (and all names bound by the pattern) has no occurrences in the simplified e
  (greatest name mode absent), and no phantom binding is required
--------------------------------------------------
E ⊢ (let x = n in e) ⇝ e
NOTES: "At most generative effects" allows allocations (pure or immutable
  construction) to be dropped, but not writes, external calls, or other arbitrary
  effects. Sound because an unused pure/allocating binding cannot influence the
  result or the observable effect trace. The decision is committed in
  `put_bindings_around_body`, producing `Delete_binding`; usage is read from the
  upwards free-name set cross-checked against the flow analysis' `required_names`.
```

```rule
RULE S.Rewrite.Let.DeadRegion
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
---
let x = End_region { region = ρ } in e   where ρ is not in required_names
--------------------------------------------------
E ⊢ (let x = End_region ρ in e) ⇝ e
NOTES: An `End_region` for a region whose corresponding `Begin_region` was itself
  eliminated (region unused) is dropped even though it is effectful, because with
  no allocations in the region there is nothing to pop. This is the one case where
  an effectful binding is deleted; it is handled specially via
  `is_end_region_for_unused_region`.
```

```rule
RULE S.Rewrite.Let.Phantom
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
---
let x = n in e   n at most generative effects, x unused in terms
generate_phantom_lets is on and x is user-visible
--------------------------------------------------
E ⊢ (let x = n in e) ⇝ (let x = n in e)   with x demoted to phantom name mode
NOTES: Instead of deleting, the binding is kept at phantom mode so debuggers can
  still report x. Descriptive because it depends on the debug-info flag; phantom
  bindings have no runtime meaning ([§04](04-opsem.md)).
```

```rule
RULE S.Rewrite.Let.Invalid
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_expr.ml#simplify_let0
CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
---
let x = n in e   and simplification of n yields SPR.create_invalid (Invalid payload)
--------------------------------------------------
E ⊢ (let x = n in e) ⇝ Invalid   (reason Defining_expr_of_let)
NOTES: When the defining expression is unreachable (e.g. a primitive whose
  arguments' types meet to Bottom), the whole let is Invalid and the body is not
  even traversed. Sound because control cannot reach a binding that computes an
  impossible value. See S.Rewrite.Invalid.Propagate.
```

## Continuations

Continuation inlining is the counterpart of function inlining for the CPS-level
control flow. **Where it happens:** the eligibility ("used exactly once,
inlinably") is computed on the downwards pass in
`join_points.ml#compute_handler_env` and re-checked in
`simplify_let_cont_expr.ml#sort_handlers`; the continuation is registered as
linearly-used-and-inlinable in the upwards environment by
`simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler`; and the actual
substitution of the handler for the call happens at the call site in
`simplify_apply_cont_expr.ml#inline_linearly_used_continuation`.

```rule
RULE S.Rewrite.LetCont.Inline
STATUS normative
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation
VERIFIED 14-validation/new-03-letcont-inline.md
---
let_cont k p̄ = h in e   (k non-recursive)
k has exactly one use in e, and that use has use-kind Inlinable
  (an apply_cont in tail position, no trap action, k not a return/exn continuation)
k's handler is not cold
--------------------------------------------------
E ⊢ (let_cont k p̄ = h in e) ⇝ e[ apply_cont k ā ↦ let p̄ = ā in h ]
NOTES: The single call is replaced by the handler with parameters bound to the
  call's arguments (via `let p = a`), and the `let_cont` binder disappears (its
  only occurrence is gone; see S.Rewrite.LetCont.DeadHandler). The "exactly one
  use" test is the *length of the uses list*, not an occurrence count. Exception
  handlers are never Inlinable (their uses carry trap actions). Sound because a
  continuation used exactly once is semantically a let-binding of its body at the
  call site.
```

```rule
RULE S.Rewrite.LetCont.DeadHandler
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont
---
let_cont k p̄ = h in e
k has zero free occurrences in the simplified e (counting excludes trap actions)
--------------------------------------------------
E ⊢ (let_cont k p̄ = h in e) ⇝ e
NOTES: An unused continuation is dropped, handler and all. Downwards, an unused
  handler is not even simplified; upwards, `rebuild_let_cont` drops the binder
  when `num_free_occurrences_of_cont_in_body = Zero`. Sound because a handler that
  is never jumped to cannot run.
```

```rule
RULE S.Rewrite.LetCont.Shortcut
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/continuation_shortcut.ml#apply
CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
---
let_cont k p̄ = apply_cont k′ ā in e   (handler is a single apply_cont, no trap action)
--------------------------------------------------
E ⊢ (let_cont k p̄ = apply_cont k′ ā in e) ⇝ e[ apply_cont k b̄ ↦ apply_cont k′ ā[p̄ ↦ b̄] ]
NOTES: A continuation whose body is just a jump to another continuation becomes a
  shortcut: calls to k are rewritten to call k′ directly, substituting k's
  arguments. When ā is exactly p̄ this is a pure alias k ≡ k′ (`to_alias`).
  Shortcuts are never created to a linearly-used-inlinable continuation, so they
  compose safely with S.Rewrite.LetCont.Inline. Sound because calling k always
  immediately calls k′ with the substituted arguments.
```

```rule
RULE S.Rewrite.LetCont.UnusedParam
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_non_recursive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
---
let_cont k (p₁ … pₙ) = h in e
parameter pⱼ is not required (∉ flow analysis required_names; for non-recursive k,
  ⟺ pⱼ absent from the rebuilt handler's free names)
--------------------------------------------------
E ⊢ let_cont removes pⱼ from k's parameter list, and every apply_cont k drops the
  corresponding argument (via the apply_cont rewrite)
NOTES: Unused parameters are removed from the handler and from every call site.
  For non-recursive continuations the rebuilt handler's free names are
  authoritative; for recursive continuations (which cannot be rewritten after the
  fact) the whole-body flow analysis `required_names` is authoritative. Removed
  parameters that still appear in free names become phantom parameters (bound to
  `Optimised_out`). Sound because a parameter that never contributes to the result
  or effects can be dropped consistently at definition and all call sites.
```

```rule
RULE S.Rewrite.LetCont.AliasedParam
STATUS normative
CODE middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
---
let_cont k (…, pⱼ, …) = h in e
flow analysis proves pⱼ always receives the same in-scope value sⱼ (a constant,
  symbol, or another parameter) across all uses of k
--------------------------------------------------
E ⊢ pⱼ is removed from k's parameters; the handler gains `let pⱼ = sⱼ` at its top;
  each apply_cont k drops pⱼ's argument
NOTES: The invariant/constant-parameter optimization. A parameter pinned to one
  value is replaced by re-materializing that value inside the handler
  (`lets_to_introduce`), so the call sites no longer pass it. Sound because sⱼ is
  in scope at the handler and equals what every caller would have supplied.
```

```rule
RULE S.Rewrite.LetCont.InvalidHandler
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont
---
let_cont k p̄ = h in e   and the rebuilt handler h is (removable as) Invalid
--------------------------------------------------
E ⊢ k is registered as an invalid continuation; every apply_cont k in e ⇝ Invalid
  (reason Apply_cont_of_unreachable_continuation)
NOTES: If a handler is unreachable, every jump to it is unreachable too. Sound by
  S.Rewrite.Invalid.Propagate.
```

```rule
RULE S.Rewrite.LetCont.Specialize
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#specialize_continuation_if_needed
CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#rebuild_apply_cont
---
let_cont k p̄ = h in e   (k a single non-recursive continuation with >1 use)
k ∈ dacc's continuations_to_specialize
--------------------------------------------------
E ⊢ a fresh copy of k is produced per use, its handler re-simplified specialized to
  that use's types, and each call site redirected to its copy
NOTES: A "match-in-match" style specialization: distinct call sites get distinct
  copies of the handler, each simplified under that site's argument types.
  Descriptive because it is heuristic and budget-limited (`Specialization_cost`).
  Recursive continuations are never specialized. Sound because each copy is a
  faithful renaming of k simplified under a *weaker* (more specific) environment.
```

## Function application (non-inlining rewrites)

The inlining decision itself is [§11](11-inlining.md)
(`Call_site_inlining_decision.make_decision` / `Inlining_transforms.inline`,
invoked from `simplify_apply_expr.ml#simplify_direct_full_application`). The
remaining application rewrites are here.

```rule
RULE S.Rewrite.Apply.IndirectToDirect
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_function_call
CODE middle_end/flambda2/types/provers.ml#meet_single_closures_entry
VERIFIED 14-validation/naked_immediates_many_relations.md
---
apply f (indirect call) with args ā
meet_single_closures_entry proves f's type is a single closure with function slot,
  a Function_type whose code id cid is available (find_code_exn), consistent with
  the call kind's code ids under the code-age relation
--------------------------------------------------
E ⊢ apply_indirect f ā ⇝ apply_direct[cid] f ā
NOTES: An indirect call whose callee type pins down one code id becomes a direct
  call (`Call_kind.direct_function_call cid`), enabling inlining and exact-arity
  calling conventions. Tupled callees are adapted by projecting the tuple first
  (`simplify_direct_tuple_application`). Sound because γ_E forces f to be that one
  closure, so the direct call targets the same code the indirect call would have
  dispatched to.
```

```rule
RULE S.Rewrite.Apply.OverApplication
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_over_application
CODE middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application
---
apply_direct[cid] f (a₁ … aₘ) → k   where cid takes n params and m > n
--------------------------------------------------
E ⊢ apply f (a₁ … aₘ) → k ⇝
     let g = apply_direct[cid] f (a₁ … aₙ) in
     apply_indirect g (aₙ₊₁ … aₘ) → k
NOTES: An over-application is split into a full application producing an
  intermediate closure g, then an indirect application of g to the remaining
  arguments. `Begin_region`/`End_region` are inserted around the pair when the
  full application returns a local-allocated closure. Sound: this is exactly the
  meaning of applying an n-ary function to more than n arguments.
```

```rule
RULE S.Rewrite.Apply.PartialApplication
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application
---
apply_direct[cid] f (a₁ … aₘ) → k   where cid takes n params and 0 < m < n
--------------------------------------------------
E ⊢ apply f (a₁ … aₘ) → k ⇝
     let g = <closure of a fresh stub taking the remaining n−m params,
              capturing a₁ … aₘ, whose body is the full application of f> in
     apply_cont k [g]
NOTES: A partial application builds a wrapper closure that captures the supplied
  arguments and, when eventually applied to the rest, performs the full call. The
  wrapper is a `stub` set of closures; its alloc mode is forced to Heap when all
  supplied args precede the first complex local parameter
  (`num_non_unarized_args ≤ first_complex_local_param`), else follows the apply's
  mode (an apply mode of Heap there is a dead GADT case ⇒ Invalid, reason
  `Partial_application_mode_mismatch`). The wrapper's code is created with
  `~stub:true` / `~inlining_decision:Stub`, so the oracle always inlines it
  ([§11](11-inlining.md)). Sound: the wrapper denotes λ(remaining). f(a₁…aₘ, remaining).
```

```rule
RULE S.Rewrite.Apply.Invalid
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#replace_apply_by_invalid
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_function_call
---
apply f ā, and any of:
  meet_single_closures_entry proves f's type Bottom/Invalid (Closure_type_was_invalid), or
  the callee's code ids meet to empty under the code-age relation, or
  an argument's or result's kind/arity provably mismatches the callee (kind_checks off)
--------------------------------------------------
E ⊢ apply f ā ⇝ Invalid
NOTES: An application whose callee cannot be any actual closure, or whose
  argument/return kinds are impossible, is unreachable. Sound by
  S.Rewrite.Invalid.Propagate. (With kind checks on, kind mismatches are fatal
  compiler errors instead.)
```

The C call path (`simplify_c_call` → `Simplify_extcall.simplify_extcall`) can
*specialise* certain external calls to primitives (`Specialised`), leave them
unchanged, or mark them `Invalid`; it is documented here only as this dispatch
and is otherwise **conjectured**/out of detailed scope. Method and effect calls
(`Call_kind.Method`, `Call_kind.Effect`) are rebuilt without arity conversion and
are out of scope per the [§01](01-overview.md) scope ledger.

## Invalid propagation

`Invalid` is Simplify's representation of unreachable code ([§04](04-opsem.md)). Many
rewrites above produce it; the unifying principle is that whenever the abstract
domain proves control cannot reach an expression — most often because a `meet`
returned `⊥`, giving a name the empty concretization — Simplify may replace that
expression by `Invalid`.

```rule
RULE S.Rewrite.Invalid.Propagate
STATUS normative
CODE middle_end/flambda2/simplify/expr_builder.ml#rebuild_invalid
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr
---
e is an expression that Simplify proves unreachable, via one of:
  a let whose defining expression is Invalid           (S.Rewrite.Let.Invalid)
  a switch with no surviving arms                      (S.Rewrite.Switch.Invalid)
  an apply proven to target no closure / impossible kinds (S.Rewrite.Apply.Invalid)
  an apply_cont to an invalid continuation             (S.Rewrite.LetCont.InvalidHandler)
  a primitive whose argument types meet to ⊥           (SPR.create_invalid)
--------------------------------------------------
E ⊢ e ⇝ Invalid
NOTES: The umbrella rule. Soundness is vacuous: if γ_E of the relevant name is ∅
  then no runtime state satisfies the environment at e, so replacing e by any
  expression — in particular Invalid — preserves observable behaviour. A source
  `Invalid { message }` in the input is preserved as-is
  (simplify_expr.ml#simplify_expr).
```

## Summary of rule IDs

Aliases: `S.Rewrite.Alias.Canonicalize`.
Transfer / relational: `S.Rewrite.Prim.Transfer`, `S.Rewrite.Prim.Relational`,
`S.Rewrite.Prim.ArgKindMismatch`.
Constant folding: `S.Rewrite.Prim.ConstFold`, `.ConstFold.Float`,
`.ConstFold.PartialUndef`, `S.Rewrite.Prim.Reify`.
Strength reduction: `S.Rewrite.Prim.IntIdentity`, `.FloatIdentity`,
`S.Rewrite.Prim.UntagTag`, `.Projection`, `.PhysEqual`, `.CompareRecovery`,
`.ObjDupElide`.
CSE: `S.Rewrite.CSE.Eligible`, `.Replace`, `.Extend`.
Switch: `S.Rewrite.Switch.ArmPrune`, `.Merge`, `.Identity`, `.BooleanNot`,
`.Invalid`.
Let: `S.Rewrite.Let.DeadBinding`, `.DeadRegion`, `.Phantom`, `.Invalid`.
Continuations: `S.Rewrite.LetCont.Inline`, `.DeadHandler`, `.Shortcut`,
`.UnusedParam`, `.AliasedParam`, `.InvalidHandler`, `.Specialize`.
Apply: `S.Rewrite.Apply.IndirectToDirect`, `.OverApplication`,
`.PartialApplication`, `.Invalid`.
Invalid: `S.Rewrite.Invalid.Propagate`.
