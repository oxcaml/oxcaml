# Kinds, subkinds, arities and well-formedness

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter defines the *static* classification of Flambda 2 terms: the kind
system, the subkind refinements layered on top of it, the arity language that
describes parameter and result layouts, and the two judgments the rest of the
formalism relies on — kinding of simples (`Γ ⊢ s : κ`) and well-formedness of
expressions (`Γ ⊢ e ok`).

Kinds answer one question: *how is this value represented in a machine
register?* Every variable, symbol, constant and primitive result has a kind,
and the kind is fixed at the point the name is created — a `Variable.t` stores
its kind (`identifiers/int_ids.ml#Variable.create`), never to change. Kinds are
therefore not inferred; they are carried around and checked for agreement.

[§02](02-syntax.md) owns the abstract syntax and the `WF.Syntax.*` rules governing binder
scoping and name modes. This chapter owns the `WF.*` rules governing kind
agreement. The two do not overlap: a term can be syntactically well-formed
(`WF.Syntax.*`) yet kind-ill-formed (`WF.*`), or vice versa.

## 1. Kind grammar

```
κ ::= Value                        (* OCaml value: immediate or GC-scanned pointer *)
    | Naked_number nnk             (* unboxed/untagged number *)
    | Region                       (* compile-time region token *)
    | Rec_info                     (* compile-time recursion depth *)

nnk ::= Naked_immediate            (* untagged int, native-width minus one bit *)
      | Naked_float32 | Naked_float
      | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_vec128 | Naked_vec256 | Naked_vec512
```

CODE: `kinds/flambda_kind.mli#t`, `kinds/flambda_kind.mli#Naked_number_kind`.

Meaning of each kind:

- **`Value`** — a normal OCaml value living in one register. At runtime it is
  either a tagged immediate (odd word, not a pointer) or a pointer to a
  GC-scanned, boxed heap block. The garbage collector must be able to
  distinguish and follow these, so anything of kind `Value` is GC-visible.

- **`Naked_number nnk`** — an unboxed, untagged number occupying one register
  (or one SIMD register, for the `vec` cases). The GC never scans these. Note
  the asymmetry the code flags in a `CR`: `Naked_immediate` is an untagged
  integer of *native width minus one bit* (the range of a tagged OCaml `int`
  without its tag bit), whereas `Naked_nativeint` is a full native-width
  integer. See the constructor comment at
  `identifiers/int_ids.mli#Const.naked_immediate`.

- **`Region`** — a compile-time token standing for a local-allocation region.
  Introduced by Flambda, never visible at the OCaml source level, and *not*
  present at runtime as a scannable value. It threads region begin/end
  operations (see [§04](04-opsem.md)'s region rules and [§06](06-primitives-memory.md)'s `Begin_region` /
  `End_region`).

- **`Rec_info`** — a compile-time recursion depth, used by the inliner to track
  how deeply a recursive function has been unrolled. Like `Region`, invisible at
  the source level; unlike `Region`, it has no runtime representation at all.

`Region` and `Rec_info` are erased before code generation; they exist only to
carry information through Simplify.

The kind stored on a `Variable.t` and returned by the kinding judgment is a bare
`κ`. Subkind information (§2) is *not* part of a variable's kind; it rides on
arities, parameters and types instead. Kind agreement is checked on bare kinds;
subkinds, when compared at all, use separate weaker relations.

## 2. Subkinds

A *kind-with-subkind* `κ̂` (`Flambda_kind.With_subkind.t`) is a triple of a bare
kind, a nullability flag, and a non-null value subkind:

```
κ̂ ::= (κ, nullable, vsk)

nullable ::= Nullable | Non_nullable

vsk ::= Anything
      | Tagged_immediate
      | Boxed_float32 | Boxed_float | Boxed_int32 | Boxed_int64
      | Boxed_nativeint | Boxed_vec128 | Boxed_vec256 | Boxed_vec512
      | Variant of { consts : imm set; non_consts : (block_shape × κ̂ list) tag_map }
      | Float_block of { num_fields : int }
      | Float_array | Immediate_array | Value_array | Generic_array
      | Unboxed_float32_array | Untagged_int_array | Untagged_int8_array
      | Untagged_int16_array | Unboxed_int32_array | Unboxed_int64_array
      | Unboxed_nativeint_array | Unboxed_vec128_array | Unboxed_vec256_array
      | Unboxed_vec512_array | Unboxed_product_array
```

CODE: `kinds/flambda_kind.mli#With_subkind`,
`kinds/flambda_kind.mli#With_subkind.Non_null_value_subkind`.

The subkind refines a value: `Boxed_float` says "a `Value` that is a pointer to
a boxed float", `Variant` records the possible constant and non-constant
constructors together with their block shapes and field kinds, and so on. The
representation stores a subkind slot for *every* kind, but it is only meaningful
for `Value`; other kinds must use `Anything`.

### Subkinds are erasable hints, not soundness-critical

```rule
RULE WF.Subkind.Erasable
CLAIM normative
CODE kinds/flambda_kind.ml#With_subkind.erase_subkind
CODE kinds/flambda_kind.ml#With_subkind.equal_ignoring_subkind
CODE kinds/flambda_kind.ml#With_subkind.has_useful_subkind_info
CAVEAT disclosure: subkind irrelevance is asserted locally only; the whole-pipeline claim is §13's.
---
erase_subkind(κ̂) = (kind(κ̂), Nullable, Anything)   when kind(κ̂) = Value
erase_subkind(κ̂) = κ̂                                otherwise
--------------------------------------------------
Two kinds-with-subkind that agree after erase_subkind are interchangeable for
all kind-agreement purposes; only kind(κ̂) is load-bearing for representation.
NOTES: `erase_subkind` collapses a Value subkind to (Anything, Nullable),
keeping only the bare kind. `has_useful_subkind_info` returns false exactly when
the subkind is this erased form, and never returns true for non-Value kinds.
Subkinds are refinement/optimization hints: they let Simplify skip tag checks,
choose specialized array operations, etc., but dropping them never changes
which representation a register holds.
```

Two subkind relations are used:

- **`equal_ignoring_subkinds`** (`kinds/flambda_arity.ml`,
  `kinds/flambda_kind.ml#With_subkind.equal_ignoring_subkind`) — erases subkinds
  before comparing. This is the relation the well-formedness checks below use.
- **`compatible κ̂ ~when_used_at`** (`kinds/flambda_kind.ml#With_subkind.compatible`)
  — requires equal bare kinds *and* that the source subkind refines the target
  subkind (`Non_null_value_subkind.compatible`). Used where Simplify wants to
  know a value is safe to use at a more precise subkind.

Because the bare kind is the only representationally significant part, the
soundness story ([§13](13-soundness.md)) is stated in terms of kinds, and subkind mismatches
are at worst missed optimizations, not miscompilations. This is *conjectured* at
the whole-pipeline level and verified only locally here.

### GC-scannability

```rule
RULE WF.Subkind.Scannable
CLAIM descriptive
CODE kinds/flambda_kind.ml#With_subkind.must_be_gc_scannable
---
must_be_gc_scannable(κ̂) = true
  iff kind(κ̂) = Value and non_null_value_subkind(κ̂) ≠ Tagged_immediate
--------------------------------------------------
Only Value-kinded values that are not known to be tagged immediates must be
treated as GC roots.
NOTES: This is how block shapes decide which fields need scanning. It is
descriptive because it documents the current classification, not a semantic
invariant the code must preserve across changes.
```

### Block shapes

Both the `Variant` subkind above and the block primitives ([§06](06-primitives-memory.md))
describe a block's field layout with a **block shape**. A shape is either
value-only, a mixed record (a scanned value prefix followed by a flat suffix of
unboxed scalars), or an all-naked-float record:

```
block_shape ::= Scannable scannable_shape
              | Float_record
scannable_shape ::= Value_only
                  | Mixed_record σ
σ (mixed_block_shape) ::= ⟨ value_prefix_size = p,  flat_suffix = e₁ … e_m,
                            field_kinds = κ̄ ⟩
e (flat_suffix_element) ::= Naked_float | Naked_float32
                          | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
                          | Naked_nativeint | Naked_immediate
                          | Naked_vec128 | Naked_vec256 | Naked_vec512
```

CODE: `kinds/flambda_kind.mli#Block_shape`,
`kinds/flambda_kind.mli#Scannable_block_shape`,
`kinds/flambda_kind.mli#Mixed_block_shape`,
`kinds/flambda_kind.mli#flat_suffix_element`.

A mixed shape's `field_kinds` array is derived from `(p, ē)` and is the kind of
each of its `p + m` logical fields: `Value` for the first `p`, then the
naked-number kind of each `eⱼ`. [§06](06-primitives-memory.md) (rules
`P.MixedShape.FieldKinds`, `P.MixedShape.Offset`) owns the layout semantics —
`field_kinds`, `size_in_words` and `offset_in_words` — since they matter for the
memory primitives; here the grammar exists so the `Variant` subkind and the
`Make_block(Mixed …)` kinding rule can name it. Tagged immediates are never
flat_suffix_elements. `Block_shape.equal` treats distinct shapes as
incompatible (no subshaping), which [§08](08-meet-join.md) relies on.

## 3. Arities and unarization

An arity describes the layout of an ordered list of parameters (or results). It
is a list of *components*, one per parameter:

```
arity      ::= component list
component  ::= Singleton κ̂              (* one register-width parameter *)
             | Unboxed_product [ component, … ]   (* only in [`Complex] arities *)
```

CODE: `kinds/flambda_arity.mli#t`,
`kinds/flambda_arity.mli#Component_for_creation`, `kinds/flambda_arity.ml`.

The type is phantom-tagged:

- **`[`Complex]`** arities may contain `Unboxed_product` components. They
  preserve the full unboxed-product structure of a parameter list.
- **`[`Unarized]`** arities contain only `Singleton`s.

*Unarization* is the flattening that turns a `[`Complex]` arity into the flat
list of register-width kinds the backend actually passes:

```rule
RULE WF.Arity.Unarize
CLAIM normative
CODE kinds/flambda_arity.ml#unarize
CODE kinds/flambda_arity.ml#Component.unarize
---
unarize(Singleton κ̂)          = [κ̂]
unarize(Unboxed_product [])   = []            (* "void": no runtime registers *)
unarize(Unboxed_product cs)   = concat_map unarize cs
unarize(c₁ … cₙ)              = unarize(c₁) ++ … ++ unarize(cₙ)
--------------------------------------------------
Γ ⊢ unarize(a) : the flat list of register-width kinds for arity a
NOTES: `num_params` counts top-level components (source-level parameters);
`cardinal_unarized` counts the flattened registers. A nullary unboxed product
is "void" — it has zero registers but is not the same as "no parameter", and
the empty-product component must be propagated because it interacts with
currying (void parameters cannot be erased). See the note at
`kinds/flambda_arity.mli#Component_for_creation.Singleton`.
```

Key derived operations, all in `kinds/flambda_arity.ml`:

- `num_params a` — number of top-level components (≈ source parameters).
- `cardinal_unarized a` — `List.length (unarize a)`.
- `unarize_per_parameter a` — one flat kind list per component.
- `partially_apply a ~num_non_unarized_params_provided` — drops that many
  leading components, giving the arity of the remaining parameters.
- `concat` — appends two arities.

`Apply_expr` uses the two flavours asymmetrically:

```rule
RULE WF.Arity.ApplyFlavours
CLAIM normative
CODE terms/apply_expr.mli#create
CODE terms/apply_expr.mli#args_arity
CODE terms/apply_expr.mli#return_arity
---
args_arity(apply)   : [`Complex]     (* unboxed-product structure retained *)
return_arity(apply) : [`Unarized]    (* already flattened *)
--------------------------------------------------
An application records its argument arity in complex form (so Cmm translation
can optimize `caml_apply`) but its result arity already unarized.
```

## 4. Kinding judgment `Γ ⊢ s : κ`

The context `Γ` maps variables to kinds; in the implementation this is not a
separate structure but the kind stored on each `Variable.t`. The judgment is
total on well-scoped simples and is computed by `Simple.kind`.

```rule
RULE WF.Kind.Var
CLAIM normative
CODE term_basics/simple.ml#kind
CODE identifiers/int_ids.ml#Variable.kind
---
Γ(x) = κ
--------------------------------------------------
Γ ⊢ x : κ
NOTES: A variable's kind is fixed at creation (`Variable.create name κ`) and
read back by `Variable.kind`. "`Γ(x) = κ`" is exactly this stored kind.
```

```rule
RULE WF.Kind.Symbol
CLAIM normative
CODE term_basics/simple.ml#kind
---

--------------------------------------------------
Γ ⊢ sym : Value
NOTES: Symbols name statically-allocated data and are *always* of kind Value;
`Simple.kind` returns `Flambda_kind.value` for every symbol, ignoring any
coercion. (`Symbol.t` carries no kind field.)
```

```rule
RULE WF.Kind.Const
CLAIM normative
CODE identifiers/reg_width_const.ml#kind
CODE identifiers/int_ids.mli#Const.Descr
---
descr(c) = Tagged_immediate _  or  descr(c) = Null      ⟹  κ = Value
descr(c) = Naked_immediate _                            ⟹  κ = Naked_number Naked_immediate
descr(c) = Naked_float _                                ⟹  κ = Naked_number Naked_float
descr(c) = Naked_float32 _                              ⟹  κ = Naked_number Naked_float32
descr(c) = Naked_int8/16/32/64 _                        ⟹  κ = Naked_number Naked_int8/16/32/64
descr(c) = Naked_nativeint _                            ⟹  κ = Naked_number Naked_nativeint
descr(c) = Naked_vec128/256/512 _                       ⟹  κ = Naked_number Naked_vec128/256/512
descr(c) = Poison (κ', _)                               ⟹  κ = κ'
--------------------------------------------------
Γ ⊢ c : κ
NOTES: Each `Const.Descr` case has a fixed kind. `Tagged_immediate` and `Null`
are Value; naked numbers take their evident kind. `Poison` is a placeholder
constant that carries an explicit kind (any kind, including Region/Rec_info) and
stands for a value proved unreachable. There are no constants of kind Region or
Rec_info other than via `Poison`.
```

```rule
RULE WF.Kind.Coerce
CLAIM normative
CODE identifiers/coercion0.mli#S
CODE term_basics/simple.ml#kind
---
Γ ⊢ s : κ
--------------------------------------------------
Γ ⊢ (s @ co) : κ
NOTES: A coercion (currently only `Change_depth`, adjusting recursion depth) is
required by construction "not [to] alter the run-time value of its argument": a
coerced simple is still a register-sized value substitutable for the original.
`Simple.kind` discards the coercion, so kind is invariant under coercion.
```

## 5. Well-formedness judgment `Γ ⊢ e ok`

Well-formedness layers kind agreement on top of syntactic well-formedness
(`WF.Syntax.*`, [§02](02-syntax.md)). The judgment is *declarative*: it states the
agreements the IR is expected to satisfy. Section 6 describes when and how the
compiler actually enforces them.

### 5.1 Let bindings

A `Let` binds a `Bound_pattern`; the pattern's shape and the defining `named`
must agree in kind. Singleton patterns bind one variable whose kind must match
`Named.kind`.

```rule
RULE WF.Let.Singleton
CLAIM normative
CODE terms/flambda.ml#Named.kind
CODE terms/flambda.mli#Named.kind
CODE bound_identifiers/bound_pattern.mli#t
---
Γ ⊢ n : κ_n           where κ_n = Named.kind(n)
pattern = Singleton x            Γ(x) = κ_x
κ_x = κ_n
Γ, x:κ_n ⊢ body ok
--------------------------------------------------
Γ ⊢ Let (Singleton x = n) body ok
NOTES: `Named.kind` is defined only for singleton-bindable defining
expressions: `Simple s ↦ Simple.kind s`, `Prim (p,_) ↦ result_kind'(p)`,
`Rec_info _ ↦ Rec_info`. It is a fatal error to ask for the kind of a
`Set_of_closures` or `Static_consts` named (they are bound by the other
patterns). See WF.Named.* below.
```

```rule
RULE WF.Let.SetOfClosures
CLAIM normative
CODE terms/flambda.mli#Named
CODE bound_identifiers/bound_pattern.mli#t
CODE simplify/simplify_set_of_closures.ml
---
n = Set_of_closures _
pattern = Set_of_closures [x₁ … xₙ]         Γ(xᵢ) = Value for each i
Γ, x₁:Value … xₙ:Value ⊢ body ok
--------------------------------------------------
Γ ⊢ Let (Set_of_closures x̄ = n) body ok
NOTES: Each closure variable bound by a set-of-closures pattern is of kind Value
(a closure is an OCaml heap value). The `Bound_pattern` itself records no kinds,
so this is maintained by construction: closure conversion creates the closure
variables with `Variable.create … K.value`, and Simplify unconditionally defines
them at kind Value when opening the binding (`simplify_set_of_closures.ml`:
`DE.define_variable … K.value`, `T.alias_type_of K.value`).
```

```rule
RULE WF.Let.Static
CLAIM normative
CODE terms/flambda.mli#Named
CODE bound_identifiers/bound_pattern.mli#t
CAVEAT disclosure: the shape-matching contract against bound_static is asserted only coarsely here; pinning the precise per-piece contract is an open question deferred to 06.
---
n = Static_consts g
pattern = Static bound_static
bound_static and g have matching shapes (symbols for data, code ids for code)
Γ extended with the bound symbols (each : Value) and code ids ⊢ body ok
--------------------------------------------------
Γ ⊢ Let (Static bound_static = n) body ok
NOTES: Symbols and code ids bound by a static pattern are in scope over the
whole dominated region, not just syntactically below (see [§02](02-syntax.md)). Symbols
are of kind Value.
```

```rule
RULE WF.Named.Prim
CLAIM normative
CODE terms/flambda.ml#Named.kind
CODE terms/flambda_primitive.mli#result_kind
CODE terms/flambda_primitive.ml#result_kind'
---
Named.kind(Prim (p, dbg)) = result_kind'(p)
where result_kind'(p) = κ  if result_kind(p) = Singleton κ
      result_kind'(p) = Value  if result_kind(p) = Unit
--------------------------------------------------
Γ ⊢ Prim (p, dbg) : result_kind'(p)
NOTES: The per-primitive result kinds live in the `result_kind_of_*`
tables in `flambda_primitive.ml`; chapters 05 and 06 give the denotations. A
`Unit`-returning primitive has result kind Value (it produces the unit
immediate).
```

```rule
RULE WF.Named.Simple
CLAIM normative
CODE terms/flambda.ml#Named.kind
---
Γ ⊢ s : κ
--------------------------------------------------
Γ ⊢ Simple s : κ         (Named.kind(Simple s) = Simple.kind s)
```

```rule
RULE WF.Named.RecInfo
CLAIM normative
CODE terms/flambda.ml#Named.kind
---

--------------------------------------------------
Γ ⊢ Rec_info ri : Rec_info
```

### 5.2 Primitive argument kinds

```rule
RULE WF.Prim.ArgKinds
CLAIM normative
CODE terms/flambda_primitive.mli#arg_kind_of_unary_primitive
CODE terms/flambda_primitive.mli#args_kind_of_binary_primitive
CODE terms/flambda_primitive.mli#args_kind_of_variadic_primitive
---
p applied to arguments s₁ … sₙ
the expected argument kinds of p are κ₁ … κₙ  (from the arg-kind tables)
Γ ⊢ sᵢ : κᵢ  for each i
--------------------------------------------------
the primitive application p(s₁ … sₙ) is kind-correct
NOTES: The expected kinds come from `arg_kind_of_unary_primitive`,
`args_kind_of_binary_primitive`, `args_kind_of_ternary_primitive`,
`args_kind_of_quaternary_primitive`, and `args_kind_of_variadic_primitive`
(whose result is `Variadic_all_of_kind κ`, `Variadic_zero_or_one κ`,
`Variadic_mixed shape`, or `Variadic_unboxed_product κs`). This is the general
rule; the concrete tables per primitive are given in chapters 05 (scalar) and
06 (memory). This agreement is enforced dynamically during Simplify at
`simplify/simplify_primitive.ml#arg_kind_mismatch` (matching
`WF.Prim.MakeBlockMixed`'s anchor): gated by `Flambda_features.kind_checks ()`,
it is fatal when the flag is on and rewrites to `Invalid` when off — see §6.
```

```rule
RULE WF.Prim.MakeBlockMixed
CLAIM normative
CODE terms/flambda_primitive.ml#args_kind_of_variadic_primitive
CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
CODE kinds/flambda_kind.mli#Mixed_block_shape.field_kinds
VERIFIED 14-validation/mixed-01-record.md @ c59c5780b0
---
p = Make_block(Mixed(t, σ), μ, mode) applied to s₁ … sₙ       σ = ⟨p₀, ē⟩
args_kind_of_variadic_primitive(p) = Variadic_mixed σ
n = p₀ + |ē|          Γ ⊢ sᵢ : field_kinds(σ)(i−1)   for each i
--------------------------------------------------
the mixed Make_block application is kind-correct
NOTES: The specialisation of WF.Prim.ArgKinds for mixed blocks: the `Variadic_mixed σ`
result is expanded pointwise to the kind list `field_kinds(σ)` (the p₀ prefix
fields at kind Value, then one naked-number kind per flat_suffix_element), and
each argument is checked against its position's kind. `simplify_primitive.ml`
does the expansion with `List.combine arg_tys (Mixed_block_shape.field_kinds σ)`.
See [§06](06-primitives-memory.md) `P.Variadic.MakeBlock.Mixed` for the denotation.
```

### 5.3 Switch

```rule
RULE WF.Switch.Scrutinee
CLAIM normative
CODE terms/switch_expr.ml#t
CODE simplify/simplify_switch_expr.ml#simplify_arm
---
Γ ⊢ scrutinee : Naked_number Naked_immediate
arms : imm ⇀ Apply_cont_expr.t          (finite map keyed by target immediates)
Γ ⊢ action ok  for each arm action
--------------------------------------------------
Γ ⊢ Switch { scrutinee; arms } ok
NOTES: A `Switch` scrutinee is a *naked* immediate, not a tagged OCaml value:
CPS/closure conversion untags the value before the switch. Simplify confirms
this by meeting the scrutinee's type against `T.this_naked_immediate arm` for
each arm (`simplify_arm`), which is only meaningful at kind Naked_immediate. The
arm keys are `Target_ocaml_int.t`. This is the *kinding judgment*; the *structural*
form of the same scrutinee constraint is WF.Syntax.SwitchScrutinee in
[§02](02-syntax.md).
```

```rule
RULE WF.Switch.NonEmpty
CLAIM normative
CODE simplify/simplify_switch_expr.ml
CODE simplify/expr_builder.ml
CODE terms/flambda.mli#Invalid
---
a Switch with zero arms is not well-formed
--------------------------------------------------
Γ ⊬ Switch { arms = ∅ } ok
NOTES: Simplify turns a zero-arm switch into `Invalid Zero_switch_arms` (see
`expr_builder.ml` and `simplify_switch_expr.ml`); a well-formed reachable switch
has at least one arm. The stronger structural claim that a *surviving* switch has
at least two arms is WF.Syntax.SwitchMinArms in [§02](02-syntax.md).
```

### 5.4 Apply_cont

```rule
RULE WF.ApplyCont.Arity
CLAIM normative
CODE terms/apply_cont_expr.mli#create
CODE bound_identifiers/bound_parameters.mli#arity
CODE bound_identifiers/bound_parameter.mli#kind
CODE simplify/env/continuation_uses.ml#add_use
CODE simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation
CAVEAT disclosure: only the per-use add_use check is ungated; Continuation_uses.union's analogous arity check is gated by -flambda2-kind-checks — benign, as add_use already validated every use.
---
Apply_cont k s₁ … sₙ         k has parameters p₁ … pₙ with kinds κ̂₁ … κ̂ₙ
n = num params of k (after unarization)
Γ ⊢ sⱼ : kind(κ̂ⱼ)   (ignoring subkinds)  for each j
--------------------------------------------------
Γ ⊢ Apply_cont k s̄ [trap_action?] ok
NOTES: An `Apply_cont_expr` carries only an argument list, no explicit arity; the
match is positional against the target continuation's `Bound_parameters`, whose
kinds are given by `Bound_parameter.kind`. Unlike the Apply-expression checks in
§6, this agreement *is* enforced dynamically during Simplify and is **not** gated
by `-flambda2-kind-checks`: every recorded use goes through
`Continuation_uses.add_use`, which computes the use's arity from its argument
types (`T.arity_of_list`) and `Misc.fatal_error`s unless it is
`equal_ignoring_subkinds` to the continuation's recorded arity. Only this
per-use check in `Continuation_uses.add_use` is ungated (always fatal);
`Continuation_uses.union`, invoked when merging use sets, performs an analogous
arity check but that one *is* gated by `-flambda2-kind-checks`
(`Flambda_features.kind_checks ()`) — benign, since every use in either operand
was already validated ungated by `add_use` against the same arity. Separately, when a
continuation is inlined at an `Apply_cont`, the param/arg list *lengths* are
checked (`simplify_apply_cont_expr.ml`, fatal on mismatch). Before Simplify,
agreement is maintained by construction. Trap actions are covered by
[§04](04-opsem.md) / 02.
```

### 5.5 Apply (function / method / C-call / effect)

Applications carry an explicit `args_arity` (complex) and `return_arity`
(unarized). Well-formedness has three parts: the argument kinds must match the
argument types, and for direct OCaml calls the argument and result arities must
match the callee's code metadata. Over- and under-application are structurally
permitted and handled specially rather than rejected.

```rule
RULE WF.Apply.ArgKinds
CLAIM normative
CODE simplify/simplify_apply_expr.ml#simplify_apply_shared
CODE terms/apply_expr.mli#args_arity
---
apply with args s₁ … sₘ and args_arity a       unarize(a) = κ̂₁ … κ̂ₘ
Γ ⊢ sᵢ : κᵢ            kind(κ̂ᵢ) = κᵢ   for each i
--------------------------------------------------
the arguments of apply are kind-correct
NOTES: `simplify_apply_shared` walks `unarize(args_arity)` alongside the
argument types and requires `K.equal (kind κ̂ᵢ) (T.kind arg_typeᵢ)`. On mismatch:
fatal error if `-flambda2-kind-checks` is on, otherwise the whole application is
replaced by `Invalid (Application_argument_kind_mismatch …)`.
```

```rule
RULE WF.Apply.DirectArity
CLAIM normative
CODE simplify/simplify_apply_expr.ml#arity_mismatch
CODE terms/flambda.mli#Invalid
---
direct call of code cid       params_arity = Code_metadata.params_arity(cid)
args_arity of apply agrees with params_arity on their common prefix
--------------------------------------------------
the direct application is arity-correct
NOTES: `arity_mismatch` compares the shorter of the two arities against the
equal-length prefix of the other (so partial and over-application prefixes are
allowed). Mismatch ⟹ fatal error under `-flambda2-kind-checks`, else
`Invalid (Direct_application_parameter_kind_mismatch …)`. Tupled functions are
detupled first (`simplify_direct_tuple_application`).
```

```rule
RULE WF.Apply.DirectResultArity
CLAIM normative
CODE simplify/simplify_apply_expr.ml
CODE terms/flambda.mli#Invalid
---
exact direct call (provided_num_args = num_params)
result_arity (from code metadata) equal_ignoring_subkinds to
  return_arity of the apply
--------------------------------------------------
the exact direct application is result-arity-correct
NOTES: Checked only for *exact* applications. For partial applications the
front-end sets the apply's return kind to Value, and for over-applications only
the apply carries the true return arity, so in those cases the code merely
checks the returned kind is a single Value (see WF.Apply.Over /
WF.Apply.Partial). Mismatch ⟹ fatal under `-flambda2-kind-checks`, else
`Invalid (Application_result_kind_mismatch …)`.
```

```rule
RULE WF.Apply.Over
CLAIM normative
CODE simplify/simplify_apply_expr.ml
CODE simplify/simplify_common.ml
CODE terms/flambda.mli#Invalid
---
direct call with provided_num_args > num_params
--------------------------------------------------
over-application is well-formed; it is compiled as a full application whose
result is then applied to the remaining arguments
NOTES: Handled by `simplify_direct_over_application`. Under
`-flambda2-kind-checks`, the overapplied function's return arity must be a
single Value (`is_one_param_of_kind_value`), else fatal. An over-application
that cannot return is `Invalid (Over_application_never_returns …)`
(`simplify_common.ml`).
```

```rule
RULE WF.Apply.Partial
CLAIM normative
CODE simplify/simplify_apply_expr.ml
CODE terms/flambda.mli#Invalid
---
direct call with 0 < provided_num_args < num_params
--------------------------------------------------
partial application is well-formed; it is compiled to a closure capturing the
supplied arguments
NOTES: Handled by `simplify_direct_partial_application`. Under
`-flambda2-kind-checks`, the apply's return arity must be a single Value, else
fatal. Mode mismatches produce `Invalid (Partial_application_mode_mismatch …)`.
Calling a local-returning closure with a normal (non-local) apply is
`Invalid (Calling_local_returning_closure_with_normal_apply …)`
(`inlining/inlining_transforms.ml`).
```

### 5.6 Regions and recursion depths

```rule
RULE WF.Region.Var
CLAIM normative
CODE from_lambda/closure_conversion.ml
CODE terms/flambda_primitive.ml#result_kind_of_variadic_primitive
---
a region-introducing variable (my_region, over_app_region, …) has kind Region
Begin_region / Begin_try_region : Region        End_region / End_try_region take Region
--------------------------------------------------
region tokens are of kind Region
NOTES: Region variables are created with `Variable.create "…" K.region`.
`Begin_region`/`Begin_try_region` have result kind `Singleton Region`;
`End_region`/`End_try_region` take an argument of kind Region. See [§04](04-opsem.md)
for the region stack semantics.
```

```rule
RULE WF.RecInfo.MyDepth
CLAIM normative
CODE simplify/simplify_apply_expr.ml
CODE simplify/simplify_set_of_closures.ml
CODE terms/flambda.mli#Function_params_and_body.create
---
the `my_depth` variable bound by a function body has kind Rec_info
--------------------------------------------------
Γ(my_depth) = Rec_info
NOTES: `my_depth` is created with `Variable.create "my_depth" K.rec_info` and
added to the environment at kind Rec_info
(`simplify_set_of_closures.ml`: `T.unknown K.rec_info`). It is the depth against
which `Rec_info` defining expressions and coercions are interpreted.
```

## 6. Where kinds are checked in practice

Flambda 2 has **no standalone kind-checking / invariant pass** over whole terms.
The `invariant`-named code in `terms/flambda.ml` concerns `invariant_params` of
recursive continuations, not kinds. Kind agreement is instead checked
opportunistically inside Simplify, at application expressions and at primitive
applications.

```rule
RULE WF.Check.Gated
CLAIM descriptive
CODE ui/flambda_features.ml#kind_checks
CODE driver/oxcaml_flags.ml
CODE driver/oxcaml_args.ml
CODE simplify/simplify_apply_expr.ml#simplify_apply_shared
CAVEAT disclosure: the -flambda2-kind-checks checks are conservative and may fatally reject legitimate layout-poly GADT code, which is why the flag defaults to off.
---
`Flambda_features.kind_checks ()` defaults to false
if true : a kind/arity mismatch at an Apply is a `Misc.fatal_errorf`
if false: the mismatch instead rewrites the Apply to an `Invalid` node
--------------------------------------------------
kind checking is controlled by the `-flambda2-kind-checks` flag (default off)
NOTES: `Default.kind_checks = false` in `oxcaml_flags.ml`; the flag is
`-flambda2-kind-checks` / `-no-flambda2-kind-checks` (`oxcaml_args.ml`). The
help text warns it "may cause fatal errors with layout-poly GADT code", which is
why it is off by default: the checks are conservative and can reject legitimate
layout-polymorphic code. When off, the same mismatches are recorded as `Invalid`
(the checks are non-vestigial but non-fatal).
```

Consequences:

- The kind-mismatch `Invalid` constructors
  (`Application_argument_kind_mismatch`, `Direct_application_parameter_kind_mismatch`,
  `Application_result_kind_mismatch`, `Partial_application_mode_mismatch`,
  `Calling_local_returning_closure_with_normal_apply`, and the never-returns
  variants) in `terms/flambda.mli#Invalid` name the Apply-specific kind/arity/mode
  conditions Simplify actively checks. They do *not* enumerate every
  actively-checked kind condition: a primitive argument-kind mismatch is also
  actively checked (below) but surfaces as the generic `Flambda.Invalid` reason
  `Defining_expr_of_let` (via `Simplified_named.Invalid`), not a dedicated named
  constructor. See [§04](04-opsem.md) for the semantics of `Invalid`.
- The `Let`, `Apply_cont` and `Switch`-scrutinee agreements above (§5.1–5.3) are
  maintained *by construction* — CPS conversion, closure conversion and the
  type-directed rewrites produce kind-correct terms — rather than re-checked. The
  corresponding rules describe intended invariants; only those routed through the
  `kind_checks` machinery are dynamically enforced.
- Primitive-argument agreements (§5.2) are *not* merely by-construction: they are
  re-checked during Simplify via `Flambda_features.kind_checks ()` in
  `simplify_primitive.ml#arg_kind_mismatch` — fatal when the flag is on, and
  rewritten to an `Invalid` when off — exactly like the Apply checks
  (`WF.Check.Gated`).
- Dump-based validation (`-drawfexpr`, `-dfexpr`) shows kinds on binders and
  arities, so kind disagreements are usually caught by reading IR dumps in the
  test suite rather than by an in-compiler checker.

`Named.dummy_value ~machine_width κ` (`terms/flambda.ml#Named.dummy_value`)
produces a kind-correct (but not type-correct) defining expression for any
non-`Region`/`Rec_info` kind; Simplify uses it when it needs a placeholder of a
known kind, which is a small window into what "kind-correct" means operationally.

## Summary

**Subtleties worth flagging for later chapters:**

- *Subkinds are erasable* (`WF.Subkind.Erasable`): only the bare kind is
  representationally load-bearing. The kinding judgment yields a bare `κ`;
  subkinds live on arities/parameters/types and are compared by weaker relations
  (`equal_ignoring_subkinds`, `compatible`). [§13](13-soundness.md)'s soundness lean on this.
- *Unarization and void* (`WF.Arity.Unarize`): a nullary unboxed product has
  zero registers yet is not "no parameter" and must be propagated for currying.
  `num_params` counts source parameters, `cardinal_unarized` counts registers.
  Args arity is complex, return arity unarized (`WF.Arity.ApplyFlavours`).
- *Naked_immediate ≠ Naked_nativeint width* (§1); *Switch scrutinees are naked
  immediates*, not tagged (`WF.Switch.Scrutinee`); *symbols are always Value*
  and coercions preserve kind.
- *Kind checking is real but gated* (`WF.Check.Gated`): off by default, and even
  when off, Apply-level mismatches become `Invalid` rather than being ignored.

**Open questions / conjectures to verify:**

1. *(Resolved — `WF.ApplyCont.Arity` promoted to normative.)* `Apply_cont`
   argument agreement *is* enforced dynamically during Simplify, ungated by
   `-flambda2-kind-checks`: `Continuation_uses.add_use` `fatal_error`s when a
   recorded use's arity is not `equal_ignoring_subkinds` to the continuation's
   recorded arity. (`Continuation_uses.union` runs an analogous arity check on
   merge, but that one is gated by `Flambda_features.kind_checks ()`.)
2. *(Resolved — `WF.Let.SetOfClosures`.)* Set-of-closures-bound variables are
   created and defined at kind Value by closure conversion and Simplify
   (`simplify_set_of_closures.ml`); the pattern records no kinds, so this holds
   by construction.
3. The well-formedness contract for `Static_consts` (matching `bound_static`
   shape to the static-const group) is stated loosely here; the static-const
   code / [§06](06-primitives-memory.md) should pin it down.
4. Whole-pipeline subkind irrelevance to soundness is asserted locally
   (`WF.Subkind.Erasable`) but only conjectured globally — [§13](13-soundness.md).
