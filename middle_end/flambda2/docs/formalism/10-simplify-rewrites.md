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
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load
---
p reads a field of an immutable aggregate at a known position:
  Block_load(imm, x, i)  |  Project_value_slot(w, x)  |  Project_function_slot(f, x)
  |  Array_load(imm array, x, i)  |  Array_length(x)  |  String_length(x)
E proves x's type records the field/slot/length as a Simple s (or constant)
  (meet_block_field_simple / meet_project_value_slot_simple /
   meet_project_function_slot_simple / prove_is_immutable_array /
   T.array_of_length shape-meet / meet_strings)
--------------------------------------------------
E ⊢ (let r = Prim(p) in e) ⇝ (let r = s in e)     [fast-path group only]
NOTES: Reading a component of a *provably immutable* aggregate at compile time.
  Immutability is essential: mutable block/array loads (`simplify_mutable_block_
  load`) never fold. For `Array_load` the index must also be a proven constant
  and in bounds (out of bounds ⇒ Invalid). Closure value/function slots are always
  immutable. Sound because γ_E pins the aggregate's contents, and immutability
  guarantees the runtime component equals the recorded s.

  Two distinct mechanisms hide under this rule; only the first literally binds
  `let r = s`:
  (a) Fast-path alias binding (Known_result → Named.create_simple s): block_load,
      project_value_slot, project_function_slot. These are the ops that match the
      conclusion above.
  (b) Refine-and-reify (KEEP the primitive, refine r's type, fold only later via
      S.Rewrite.Prim.Reify): array_load and array_length (both go through
      Simplify_common.simplify_projection's Ok branch, SPR.create original_term
      ~try_reify:true, setting r's type to the element type / a length alias) and
      string_length (SPR.create original_term with r's type set to a set of naked
      immediates, T.these_naked_immediates). For string_length the immediate set
      may have several elements and thus never fold to a constant. For these ops
      the blanket conclusion `let r = s` does not hold directly; the constant, if
      any, appears only after reify.
```

```rule
RULE S.Rewrite.Prim.PhysEqual
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal
CODE middle_end/flambda2/types/provers.ml#prove_physical_equality
---
p = phys_equal(op, x, y)   where op ∈ {Eq, Neq}
prove_physical_equality proves the two values definitely equal or definitely distinct → b
--------------------------------------------------
E ⊢ (let r = Prim(phys_equal(op, x, y)) in e) ⇝ (let r = (b if op = Eq else ¬b) in e)
NOTES: The prover only concludes when the addresses provably coincide (same
  canonical simple) or provably differ; otherwise the primitive is kept. Both
  Eq and Neq (Lambda `!=`) are dispatched into simplify_phys_equal, which folds
  `result = match op with Eq -> b | Neq -> not b`; Phys_equal Neq is the boolean
  negation, mirroring the ch06 P.Binary.PhysEqual NOTE. Sound because physical
  equality is referential identity, which the prover respects.
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
  String_or_bigstring_load — including the String case, whose storage may
  actually be mutable bytes via the deprecated %caml_string_get* primitives —
  and Bigarray_load). It *includes* immutable and
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
  only the physical identity of an immutable value — identity MAY be observed
  through `phys_equal`; the change is licensed because `(==)` on non-mutable
  values is implementation-dependent, and `P.Binary.PhysEqual` (06) together
  with `INV.Simplify.Preserves`' refinement reading (13 §1, §4 item 8) grants
  exactly this license). CSE runs *before*
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

Which sharing mechanism a pure projection uses — CSE, or the types domain — is
determined by whether its access path is static (in the primitive payload) or
dynamic (a Simple operand).

```rule
RULE S.Rewrite.Share.StaticDynamicSplit
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse
CODE middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
---
p is a pure ((No_effects, No_coeffects)) projection primitive
--------------------------------------------------
Simplify's sharing mechanism for p is assigned by whether p's access path is
STATIC (encoded in the primitive payload) or DYNAMIC (a Simple operand):
(1) static path — Block_load (field a constant payload), Project_function_slot,
    Project_value_slot, Unbox_number, Untag_immediate — NOT CSE-eligible; shared
    exclusively through the types domain (meet_block_field_simple,
    meet_project_*_simple, boxed/tagged-contents shape meets). Each additionally
    installs an INVERSE-CONSTRUCTION CSE equation (Make_block ↦ block on a
    fully-known block load; Box_number ↦ arg on unbox, GATED on alloc mode Proved
    Heap; Tag_immediate ↦ arg on untag), sharing the re-construction;
(2) dynamic index — Array_load (Immutable | Immutable_unique) — forward
    CSE-eligible; the types domain recovers only constant-index instances;
(3) discriminators — Is_int, Get_tag, Is_null — carried by BOTH forward CSE and
    the relational types domain (a deliberate redundancy the code plans to retire).
    Get_header rides ONLY CSE (simplify_get_header installs no relation);
(4) contents-known fold only — String_or_bigstring_load (String): NOT
    CSE-eligible (the deprecated %caml_string_get* primitives may apply the
    String load to mutable bytes, so the primitive alone does not guarantee
    immutability); constant-index loads from statically-known string contents
    fold directly in simplify_string_or_bigstring_load (contents ride
    String_info in the types domain, so no per-load projection is needed).
    Variable-index immutable string loads are never shared. Bytes/Bigstring
    loads: neither mechanism (mutable).
NOTES: Sharpens the corrected S.Rewrite.CSE.Eligible into a design invariant WITH
ITS REASON: types can carry only a projection whose path is a static index into a
Row_like / product; CSE keys on the whole primitive so it can carry a variable
operand. Observable: identical immutable BLOCK loads share via the TYPES domain
(the first load's shape-meet writes field₀ = (= x) into the previously-Unknown
block type — type-sharing needs no a-priori block type; no forward Block_load CSE
equation appears, only the inverse Make_block one). Identical immutable ARRAY loads
at a VARIABLE index still deduplicate (forward CSE; types cannot carry a variable
index — witnessed by 14-validation/cse_immutable_array_load_var_index.md). Two
construction-sharing channels: the inverse Make_block equation at the LOAD and the
forward Immutable Make_block CSE equation at the ALLOCATION. Composes:
S.Rewrite.CSE.Eligible, T.Prove.MeetShortcut, T.Grammar.RowLike.Index.
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
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_end_region
---
let x = End_region { region = ρ } in e   where ρ is not in required_names
--------------------------------------------------
E ⊢ (let x = End_region ρ in e) ⇝ e
NOTES: An `End_region` for a region whose corresponding `Begin_region` was itself
  eliminated (region unused) is dropped even though it is effectful, because with
  no allocations in the region there is nothing to pop. This is the one case where
  an effectful binding is deleted; it is handled specially via
  `is_end_region_for_unused_region`. For an unused region, `will_delete_binding` is
  TRUE unconditionally — each End_region is deleted even if its bound unit variable
  has surviving uses. This is sound only because `simplify_end_region` PINS the
  End's result type to exactly tagged 0 (simplify_unary_primitive.ml:689-694), so
  any surviving use of the bound variable canonicalizes to that constant before the
  rebuild. That result-type pinning is part of this rule's soundness and is why the
  atomic Begin/End deletion of INV.Simplify.RegionPairAtomic ([§13](13-soundness.md))
  never orphans a use. Only `End_region` (not `End_try_region`) takes this path;
  see INV.Simplify.EffectfulDeletionInventory ([§13](13-soundness.md)).
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
RULE S.Rewrite.LetCont.InlineForcesElimination
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind
CODE middle_end/flambda2/simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont
---
k non-recursive, not an exception handler, not cold;
k's recorded uses list has length exactly 1, of kind Inlinable
--------------------------------------------------
The output term contains NO Let_cont binding k — unconditionally. Two terminal
cases: (a) the use site survives the upwards rebuild: it is spliced with k's
handler (inline_linearly_used_continuation; registration at
rebuild_single_non_recursive_handler is unconditional given the classification),
so the rebuilt body has zero occurrences of k and rebuild_let_cont drops the binder;
(b) the use site dies during rebuild (rewritten to Invalid or erased): zero
occurrences again, binder dropped.
NOTES: The subtle content: no OTHER occurrence form of k can exist —
apply_cont_use_kind makes switch-branch, trap-action-bearing, and return/exn-sort
uses all Non_inlinable, so a single-Inlinable-use k occurs exactly once as a bare
tail apply_cont; the CUE use-list length (downwards, already excluding
unreachable/pruned code) and the upwards syntactic occurrence count provably agree.
The not-cold condition is folded into is_single_inlinable_use. Shortcuts are never
created for these continuations, so no rewrite re-introduces an occurrence. A
must-disappear theorem: the classification alone forces the binder's elimination.
Composes: S.Rewrite.LetCont.Inline, S.Rewrite.LetCont.DeadHandler,
S.Struct.SingleInlinableUse.
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
CODE middle_end/flambda2/simplify/continuation_shortcut.ml#to_alias
CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
---
let_cont k p̄ = apply_cont k′ ā in e   (handler is a single apply_cont, no trap action)
--------------------------------------------------
E ⊢ (let_cont k p̄ = apply_cont k′ ā in e) ⇝ let_cont k p̄ = apply_cont k′ ā in e[ apply_cont k b̄ ↦ apply_cont k′ ā[p̄ ↦ b̄] ]
NOTES: A continuation whose body is just a jump to another continuation becomes a
  shortcut: calls to k are rewritten to call k′ directly, substituting k's
  arguments. The binder is kept: the substitution reaches apply_cont uses of k
  only — a use of k in an Apply's return/exn position is not rewritten — so
  `rebuild_let_cont` keeps the handler until
  `num_free_occurrences_of_cont_in_body = Zero`; once no uses survive,
  S.Rewrite.LetCont.DeadHandler composes to drop the binder, recovering the
  binder-dropping form. When ā is exactly p̄ this is a pure alias k ≡ k′
  (`to_alias`); only then are Apply return/exn uses of k also retargeted to k′
  (`apply_continuation_aliases`, `apply_exn_continuation_aliases`). Shortcuts are
  never created to a linearly-used-inlinable continuation, so they compose safely
  with S.Rewrite.LetCont.Inline. Sound because calling k always immediately calls
  k′ with the substituted arguments.
```

```rule
RULE S.Rewrite.LetCont.ShortcutFlat
STATUS conjectured
CODE middle_end/flambda2/simplify/expr_builder.ml#apply_continuation_shortcuts
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
CODE middle_end/flambda2/simplify/continuation_shortcut.ml#apply
CODE middle_end/flambda2/simplify/env/upwards_env.ml#add_continuation_shortcut
---
the upwards environment's continuation-shortcut map, at any point of the upwards
rebuild
--------------------------------------------------
FLATNESS INVARIANT: a registered shortcut's target is never itself a shortcut.
Consequently the single-step (non-looping) application in
apply_continuation_shortcuts achieves FULL path compression: no emitted apply_cont
targets a shortcut continuation, and a chain of n trampoline continuations
(kₙ → kₙ₋₁ → … → k₀) collapses to direct jumps to k₀ in one pass, after which every
intermediate kᵢ has zero occurrences and dies by S.Rewrite.LetCont.DeadHandler.
Proof of flatness: (a) acyclicity by scoping — a shortcut handler is non-recursive,
so its target is declared strictly OUTSIDE it (shortcut edges point outward, no
cycles); (b) registration order — handlers are rebuilt BEFORE the bodies containing
their uses, so when kᵢ's handler (a bare apply_cont) is rebuilt, its target's
shortcut (registered strictly earlier) has already been applied to it; the
behaviour check in rebuild_single_non_recursive_handler reads the ALREADY-REBUILT
handler, so a shortcut is never registered for a would-be-inlinable target.
NOTES: Non-local: flatness of a global map maintained across the whole rebuild;
single-step application is correct only because of a registration ORDER spanning
arbitrarily distant let_conts. Composition guarantee S.Rewrite.LetCont.Shortcut
("shortcuts never created to a linearly-used-inlinable continuation") prevents the
one interaction that could re-materialize a use. Site trap actions survive
retargeting (the shortcut condition is on the HANDLER's apply_cont having no trap
action, not the site's). Known precision (not soundness) leak (CR gbury):
retargeting can lose kind/subkind info the intermediate continuation's params
carried. Composes: S.Rewrite.LetCont.Shortcut, S.Rewrite.LetCont.DeadHandler,
S.Rewrite.LetCont.InlineForcesElimination.
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

An exception handler with no *escaping* use is demoted to an ordinary
continuation, its trap frame dissolved. This merges two verified findings (Leibniz
T1 ExnDemotion and Berkeley BERK-10): the escaping gate and its `-g` sensitivity,
the all-or-nothing trap-action erasure, the cross-rule enabling edges, and the
bucket-pinning fact.

```rule
RULE S.Rewrite.LetCont.DemoteExn
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers
CODE middle_end/flambda2/simplify/env/downwards_acc.ml#demote_exn_handler
CODE middle_end/flambda2/simplify/simplify_common.ml#apply_cont_use_kind
CODE middle_end/flambda2/simplify/simplify_common.ml#clear_demoted_trap_action
CODE middle_end/flambda2/simplify/simplify_common.ml#patch_unused_exn_bucket
CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
---
let_cont k = h in e, k an exception handler (is_exn_handler);
no recorded use of k is escaping (Non_inlinable { escaping = true }), which unfolds
to: (a) no surviving Apply carries k as its exn continuation (every Apply-borne exn
continuation use is recorded escaping = true), and (b) every raise targeting k
(apply_cont with a Pop trap action) has raise_kind No_trace, OR
Flambda_features.debug () is false (apply_cont_use_kind: Regular/Reraise raises are
escaping = true exactly when debug info is on, mirroring Cmm_helpers.raise_prim);
Push-carrying uses are always non-escaping and never block demotion
--------------------------------------------------
(1) k is demoted (DA.demote_exn_handler in dacc.demoted_exn_handlers, decided in
    prepare_dacc_for_handlers after the body traversal has recorded all uses;
    is_exn_handler = false thereafter);
(2) all-or-nothing at a distance: on the way up, EVERY Push and Pop trap action
    naming k, at every site in e, is erased (clear_demoted_trap_action); raises to k
    become plain jumps. Either k stays an exn handler and every Push/Pop survives,
    or it is demoted and every one is cleared;
(3) demotion UNLOCKS the normal-continuation rule set previously forbidden on k:
    parameter unboxing (the next step of prepare_dacc_for_handlers; the demoted
    handler falls into the unboxing branch genuine exn handlers never reach),
    single-use inlining, shortcuts — and the exn bucket loses its
    unconditional-use marking, becoming ordinary dead-param-eligible.
NOTES: -g DEPENDENCE: a local try…with (handler reachable only from direct raises)
costs a runtime trap frame iff debug info is on — without -g it compiles to
straight-line control flow, with -g the Push/Pop survive for backtraces;
raise_notrace demotes regardless. Bucket-pinning (Leibniz, inverting the original
claim): for a KEPT handler, flow_acc.enter_continuation UNCONDITIONALLY marks the
exn bucket param, and the param→arg edge forces every raise's value into
required_names — so the allocation feeding a raise to a kept handler always
survives; dead raise-value elimination happens ONLY via the demotion route of (3).
Consequently patch_unused_exn_bucket is DEAD CODE (kept handlers make
exn_value_is_used always true; demoted handlers have their Pop stripped FIRST, and
AC.is_raise requires a Pop) — it should be marked vestigial (see the code-hygiene
note). Two further facts: the TRY-REGION pair SURVIVES demotion (all traps cleared
but begin/end_try_region remain — a small missed optimization, consistent with
End_try_region never being deleted); and from_lambda emits End_try_region only on
the HANDLER path, so try regions must not be modelled as having an End at each exit.
BERK-8 (S.Struct.Flow.ExnFirstParam) applies only to handlers that REMAIN exn
handlers, i.e. not demoted here. Composes: S.Struct.Flow.ExnFirstParam,
S.Rewrite.LetCont.Inline, S.Unbox.ContParam.Rewrite,
INV.Simplify.EffectfulDeletionInventory ([§13](13-soundness.md)).
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

## Loopification

A function whose only self-references are tail calls can have its recursion
turned into a *continuation* loop, so the loop no longer goes through the
function-call machinery at all. Unlike inlining, this is not an oracle-guided
decision: the choice is an attribute computed once at closure conversion, and
Simplify obeys it unconditionally. The architectural placement (the
`loopify_state` component of `denv`, the body being simplified as a recursive
`Let_cont`) is `S.Struct.Loopify` in [§09](09-simplify-structure.md); the rules
here are the rewrites themselves. Together they entail that a purely
self-tail-recursive function can never survive Simplify in its original,
`Apply`-recursive form: such a function always gets the attribute
(`Loopify.Attribute`), the wrap is unconditional (`Loopify.Body`), every one of
its self-references qualifies for redirection (`Loopify.SelfTailCall`), and the
emitted code — if any escapes inlining and dead-code elimination at all — is
`Non_recursive` (`Code.RecursiveRecompute`). The `loopify-*` case studies in
[`14-validation/`](14-validation/) verify this end to end, including its
boundaries.

```rule
RULE S.Rewrite.Loopify.Attribute
STATUS normative
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function
CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Acc.add_name_to_free_names
VERIFIED 14-validation/loopify-03-not-purely-tailrec.md
---
Closure conversion emits Code c for a source function f
--------------------------------------------------
metadata(c).loopify =
  Always_loopify                   if f's loop attribute is Always_loop ([@loop])
  Never_loopify                    if f's loop attribute is Never_loop
  Default_loopify_and_tailrec      if f is the sole function of its recursive
                                     group and every occurrence of my_closure in
                                     its body is as the callee of a tail call
  Default_loopify_and_not_tailrec  otherwise
NOTES: The "purely tail-recursive" test (is_purely_tailrec) starts true only for
  a single-function let rec and is falsified by any occurrence of my_closure
  that is not a tail-call callee (add_name_to_free_names /
  add_free_names_and_check_my_closure_use), EXCEPT that a `let v =
  Project_value_slot(f_slot, w) my_closure` binding does NOT falsify it — captured
  variables read from the closure are exempt (S.Rewrite.Loopify.Attribute.ValueSlotExempt
  below). Mutual recursion therefore never gets Default_loopify_and_tailrec, and
  neither does a function whose body uses my_closure other than as a tail-call
  callee or a value-slot projection (passing or returning itself) — though the
  closure escaping the *defining scope* (e.g. an exported toplevel function) is
  irrelevant. This is the only place the loopify decision is made; Simplify consumes
  it via should_loopify (Loopify_attribute.should_loopify: true exactly for
  Always_loopify and Default_loopify_and_tailrec).
```

```rule
RULE S.Rewrite.Loopify.Attribute.ValueSlotExempt
STATUS conjectured
CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#Let_with_acc.create
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_one_function
---
closure conversion of a single-function recursive group f that captures outer
  variables; the converted body binds each captured variable at the top via
  `let v = Project_value_slot(f_slot, w) my_closure`
--------------------------------------------------
These my_closure occurrences do NOT falsify is_purely_tailrec: Let_with_acc.create
exempts Lets whose defining expression is a Project_value_slot primitive from
add_free_names_and_check_my_closure_use (they go through plain add_free_names).
Hence a CAPTURING local function whose self-references are all direct full tail
calls still receives Default_loopify_and_tailrec, is loopified, and (per
S.Rewrite.Loopify.TailrecEmitsNonRecursive) is emitted Non_recursive — while its
body retains genuine non-callee my_closure uses (the projections), so
is_my_closure_used stays true.
NOTES: CORRECTS the S.Rewrite.Loopify.Attribute NOTES ("falsified by ANY occurrence
of my_closure that is not a tail-call callee"). The exemption matches the PRIM
SHAPE — any unary Project_value_slot, regardless of its argument — not specifically
"argument is my_closure"; harmless because a projection cannot manufacture a
self-application. Without it the common idiom `let g y = let rec loop x = … in
loop 5` (every capturing loop projects from my_closure) would never loopify.
Project_function_slot bindings are NOT exempt (moot: multi-function groups already
excluded by the single-function premise). Non-local: the fact lives in the
from_lambda accumulator plumbing, two files from the attribute decision.
Composes: S.Rewrite.Loopify.Attribute, S.Rewrite.Loopify.TailrecEmitsNonRecursive.
```

```rule
RULE S.Rewrite.Loopify.Body
STATUS normative
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
CODE middle_end/flambda2/simplify/loopify_state.mli
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body
VERIFIED 14-validation/loopify-01-escaping-tailrec.md
---
should_loopify(metadata(c).loopify)      (⇔ Always_loopify | Default_loopify_and_tailrec)
k fresh (named "self")
terms are being rebuilt (not speculative inlining)
--------------------------------------------------
E ⊢ Code c = λ⟨ret,exn⟩ p̄ ⟨my_closure,…⟩. e
  ⇝ let_cont rec k p̄ = e in apply_cont k p̄
NOTES: Unconditional given the attribute — there is no cost model or oracle
  here. The wrapped term becomes the function's body: the handler re-binds p̄
  (shadowing the function's parameters), while the entry apply_cont's arguments
  refer to the function's parameters. loopify_state(denv) is set to Loopify k
  for the traversal of e (Simplify_set_of_closures.simplify_function_body
  creates k and sets the state; Simplify_expr.simplify_function_body builds the
  wrapper and simplifies it via simplify_as_recursive_let_cont). The original,
  unwrapped body never reaches the rebuilt output: what is emitted for this
  function is the simplification of the wrapper, in which the self tail calls
  have been redirected (S.Rewrite.Loopify.SelfTailCall).
```

```rule
RULE S.Rewrite.Loopify.SelfTailCall
STATUS normative
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call
VERIFIED 14-validation/loopify-06-mutual-and-mixed.md
---
loopify_state(denv) = Loopify k      (we are inside a body wrapped by S.Rewrite.Loopify.Body)
canon(callee) = canon(my_closure)    (canonical simples, coercions stripped)
the apply's return continuation is the function's return continuation, or Never_returns
the apply's exn continuation is the function's exn continuation, with no extra args
terms are being rebuilt (loopification is disabled during speculative inlining)
--------------------------------------------------
E ⊢ apply callee ā ⟨ret,exn⟩ ⇝ apply_cont k ā
NOTES: The self-call test is semantic, not syntactic: the callee is compared to
  my_closure after canonicalization through the typing environment and dropping
  coercions, so aliases of the closure qualify, including calls that only become
  self tail calls during simplification. The rec_info coercion on the callee is
  discarded with the Apply, which is what removes my_depth from the body's free
  names (see S.Rewrite.Code.RecursiveRecompute). An exn continuation carrying
  extra args disqualifies the call. During speculative inlining the rewrite is
  disabled because flow analysis would not see the self continuation
  (loopify_decision_for_call's do_not_rebuild_terms case); nothing from such a
  traversal is kept, so no unloopified body escapes this way.
```

```rule
RULE S.Rewrite.Loopify.AttributeUpdate
STATUS normative
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0
VERIFIED 14-validation/loopify-01-escaping-tailrec.md
---
Simplify rebuilds Code c (input attribute a = metadata(c).loopify)
--------------------------------------------------
the emitted code's loopify attribute is:
  a                    if a ∈ {Always_loopify, Never_loopify, Already_loopified}
  Already_loopified    if a = Default_loopify_and_tailrec
  Never_loopify        if a = Default_loopify_and_not_tailrec
NOTES: The Default_* values exist only between closure conversion and the first
  simplification of the code. Since should_loopify is false for every output
  value except Always_loopify, loopification is idempotent: a later round (or
  cross-unit import) re-loopifies only [@loop]-annotated code.
```

```rule
RULE S.Rewrite.Code.RecursiveRecompute
STATUS normative
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body
VERIFIED 14-validation/loopify-01-escaping-tailrec.md
---
Simplify rebuilds Code c with simplified body e′
--------------------------------------------------
the emitted code's recursive flag is:
  Recursive       if my_depth ∈ free_names(e′)
  Non_recursive   otherwise
NOTES: Not loopify-specific — this recompute applies to every code binding — but
  it is what makes loopification pay off: self-Applys are the only mentions of
  my_depth (via their rec_info coercions), so a function whose self tail calls
  were all redirected by S.Rewrite.Loopify.SelfTailCall and which had no other
  self-calls comes out Non_recursive, making it eligible for Small_function
  inlining ([§11](11-inlining.md)) where the recursive original was not. A
  function with surviving non-tail self-calls (Always_loopify on a
  not-purely-tailrec function) stays Recursive.
```

```rule
RULE S.Rewrite.LetCont.Demote
STATUS normative
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers
VERIFIED 14-validation/loopify-04-loop-attr-no-tailcall.md
---
let_cont rec k̄ = h̄ in e   (a declared-recursive group)
after the downwards traversal, handler k's recorded uses place it in no SCC
  loop of the group's use graph (sort_handlers classifies it No_loop)
--------------------------------------------------
k is rebuilt as a non-recursive let_cont (in dependency order), and becomes
eligible for the non-recursive rules: S.Rewrite.LetCont.Inline (its single-use
detection runs on the demoted handler), .DeadHandler, .Shortcut
NOTES: A general continuation rule, stated here because loopification is its
  chief client: a loopified body whose self continuation ends up with no
  recursive uses (e.g. [@loop] on a function with no self tail calls, or a loop
  whose recursive branch is proven dead) is demoted and then collapses back via
  LetCont.Inline — this is the one way a should_loopify function's body can
  emerge without the wrapper, and it can only happen when the loop is gone.
```

```rule
RULE S.Rewrite.Loopify.TailrecEmitsNonRecursive
STATUS conjectured
CODE middle_end/flambda2/from_lambda/closure_conversion_aux.ml#create_apply
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml#make_inlined_body
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function_body
---
Code c enters Simplify with metadata(c).loopify = Default_loopify_and_tailrec;
Simplify (Normal mode) rebuilds Code c′ for it (terms are being rebuilt)
--------------------------------------------------
my_depth ∉ free_names(body′) — the PRIMARY conclusion — hence
(S.Rewrite.Code.RecursiveRecompute) metadata(c′).recursive = Non_recursive,
UNCONDITIONALLY, including through unrolling/inlining of the self tail calls. Under
this premise it follows additionally that no self-Apply (callee canonicalizing to
c's my_closure) survives in body′; that equivalence needs the premise (a
Non_recursive body can in principle contain a symbol-callee recursive Apply, just
not under Default_loopify_and_tailrec).
NOTES: Strengthens the S.Rewrite.Loopify.* prose from "the rules redirect self tail
calls" to a universal output guarantee discharging the antecedent of
S.Rewrite.Code.RecursiveRecompute's NOTES. Sketch: closure conversion guarantees
every my_closure occurrence is either a Direct exact-arity tail-call callee with
matching ret/exn conts (the SelfTailCall side conditions) or a Project_value_slot
argument (S.Rewrite.Loopify.Attribute.ValueSlotExempt) — the latter carry no
rec_info and never mention my_depth. Loopify.Body wraps the body; each surviving
self-Apply hits simplify_direct_full_application and either redirects
(SelfTailCall, dropping the coercion — the only my_depth mention) or is inlined
(make_inlined_body renames ret/exn conts to the enclosing function's, re-traversed
in-pass, induction on bounded unroll depth). Speculative traversals have
do_not_rebuild_terms but none of their output is kept. Composes:
S.Rewrite.Loopify.Attribute, S.Rewrite.Loopify.Body, S.Rewrite.Loopify.SelfTailCall,
S.Rewrite.Code.RecursiveRecompute.
```

```rule
RULE S.Rewrite.Loopify.InvariantArgElim
STATUS conjectured
CODE middle_end/flambda2/simplify/flow/dominator_graph.ml#dominator_analysis
CODE middle_end/flambda2/simplify/flow/flow_types.mli#Continuation_param_aliases
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_self_tail_call
---
Code c is loopified (S.Rewrite.Loopify.Body wraps its body in let_cont rec k p̄′,
  entry apply_cont k p̄ passing the function's parameters);
for some index j, every self tail call in c's body passes, in position j, a simple
  that canonicalizes to the function's j-th parameter pⱼ (loop-invariant)
--------------------------------------------------
In the emitted body, k's j-th parameter is eliminated from the loop: the dominator
analysis maps pⱼ′ to pⱼ (its unique non-self dominator), pⱼ′ ∈
removed_aliased_params_and_extra_params with `let pⱼ′ = pⱼ` in lets_to_introduce at
the handler top, and no argument is passed at position j by the recursive
apply_conts — the residual loop threads strictly fewer parameters than the source
function's arity.
NOTES: An optimization IMPOSSIBLE in Apply-recursive form (a call must pass all
arity-many arguments each iteration) and invisible to any local rewrite: the entry
use (arg = pⱼ) and recursive uses (arg = pⱼ′) are reconciled only by the whole-body
dominator fixpoint at the turn. pⱼ′'s in-edges are {pⱼ (entry), pⱼ′ (each recursive
site)}; self-edges do not defeat domination, and pⱼ — edgeless in the graph —
dominates (variables CAN be dominator roots; the operative fact is pⱼ's
edgelessness). Composes: S.Rewrite.Loopify.Body, S.Rewrite.Loopify.SelfTailCall,
S.Struct.Flow.Aliases, S.Rewrite.LetCont.AliasedParam.
```

```rule
RULE S.Rewrite.Loopify.SimplifyExposed
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#loopify_decision_for_call
CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0
CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
---
during simplification of Code c's body, an Apply becomes a self tail call
  (canon(callee) = canon(my_closure), ret/exn conts = the function's) that was NOT
  a self tail call in the source — e.g. exposed by inlining a mutual-recursion
  partner into c's body
--------------------------------------------------
The exposed call is redirected to the loop continuation IFF
should_loopify(metadata(c).loopify) — i.e. iff c carried [@loop] (Always_loopify)
or was already purely self-tail-recursive at closure conversion — modulo
SelfTailCall's side conditions. In particular:
(1) NEGATIVE: a function not marked at closure conversion NEVER acquires a
    continuation loop from Simplify. Mutual tail recursion collapsed to
    self-recursion by inlining stays Apply-recursive and the code is emitted
    Recursive (loopify_state is set only by Loopify.Body, which runs only under
    should_loopify; AttributeUpdate freezes Default_loopify_and_not_tailrec to
    Never_loopify; S.Struct.SingleRound means no later whole-unit chance).
(2) POSITIVE: with [@loop], SelfTailCall's semantic canon test redirects calls that
    only BECAME self tail calls mid-simplification, so [@loop] + partner inlining is
    the one mechanism WITHIN SIMPLIFY (for Simplify-surviving Code) by which
    multi-function recursion becomes a single continuation loop.
NOTES: A phase-ordering completeness theorem: the decision predates all
simplification and is never revisited, so loopification opportunities CREATED by
Simplify are missed by design — the precise boundary of the ch10 prose "a purely
self-tail-recursive function never survives in Apply-recursive form", which is
false for functions that become purely self-tail-recursive DURING Simplify. Scope
note: upstream of closure conversion, Lambda's simplify_local_functions contifies
local non-escaping mutual recursion into staticcatch — arriving as recursive
continuation groups with no loopify attribute; this rule is about recursion
reaching Simplify as Code. Composes: S.Rewrite.Loopify.Body,
S.Rewrite.Loopify.SelfTailCall, S.Rewrite.Loopify.AttributeUpdate.
```

```rule
RULE S.Rewrite.Loopify.ResimplifyIdempotent
STATUS conjectured
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function
CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_function0
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#sort_handlers
---
simplify_function's bounded resimplification loop re-runs simplify_function0
  (run n+1) on the code emitted by run n (should_resimplify, count <
  max_function_simplify_run)
--------------------------------------------------
(1) Run n+1's input attribute is run n's OUTPUT attribute, so re-wrapping
    (Loopify.Body) occurs in run n+1 iff the attribute is Always_loopify:
    Default_loopify_and_tailrec became Already_loopified after run 1 and is never
    re-wrapped;
(2) when Always_loopify code IS re-wrapped, the previously-created loop is just an
    ordinary recursive Let_cont in the body — its apply_conts are not Applys, so
    the fresh wrapper collects recursive uses only from self-Applys still present;
    if none, the fresh wrapper collapses exactly (S.Rewrite.LetCont.Demote →
    single-Inlinable entry → S.Rewrite.LetCont.Inline);
(3) nesting splits by attribute: for Default_loopify_and_tailrec, exactly one wrap
    ever happens (run 1), so wrappers never nest; for Always_loopify, each
    resimplify run that NEWLY exposes a redirectable self tail call (e.g. via
    mutable unboxing + alias rematerialization making a stored self-closure
    canon-visible) adds ONE live wrapper — bounded by max_function_simplify_run
    (default 2, so ≤ 3 runs / ≤ 3 live wrappers); a re-wrap exposing nothing
    collapses exactly, adding none.
NOTES: Non-local: needs the resimplify feed-forward (simplify_function's `run`
recursion passes new_code, not the original), AttributeUpdate's lattice, Demote's
SCC classification, and the Inline collapse. Rules out the wrapper accumulation a
purely local reading of Loopify.Body (unconditional wrap) would suggest. The
original "wrappers never nest or accumulate" was refuted by a two-field mutable
record storing the function itself; leg (3) incorporates that mechanism and its
bound. Composes: S.Rewrite.Loopify.Body, S.Rewrite.LetCont.Demote,
S.Rewrite.LetCont.Inline, S.Struct.Resimplify.
```

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
Sharing: `S.Rewrite.Share.StaticDynamicSplit`.
Switch: `S.Rewrite.Switch.ArmPrune`, `.Merge`, `.Identity`, `.BooleanNot`,
`.Invalid`.
Let: `S.Rewrite.Let.DeadBinding`, `.DeadRegion`, `.Phantom`, `.Invalid`.
Continuations: `S.Rewrite.LetCont.Inline`, `.InlineForcesElimination`,
`.DeadHandler`, `.Shortcut`, `.ShortcutFlat`, `.UnusedParam`, `.AliasedParam`,
`.InvalidHandler`, `.Specialize`, `.Demote`, `.DemoteExn`.
Apply: `S.Rewrite.Apply.IndirectToDirect`, `.OverApplication`,
`.PartialApplication`, `.Invalid`.
Loopification: `S.Rewrite.Loopify.Attribute`, `.Attribute.ValueSlotExempt`,
`.Body`, `.SelfTailCall`, `.AttributeUpdate`, `.TailrecEmitsNonRecursive`,
`.InvariantArgElim`, `.SimplifyExposed`, `.ResimplifyIdempotent`;
`S.Rewrite.Code.RecursiveRecompute`.
Invalid: `S.Rewrite.Invalid.Propagate`.
