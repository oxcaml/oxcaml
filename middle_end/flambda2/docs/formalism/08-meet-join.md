# Meet, join, provers and reification

*Part of the Flambda 2 formalism; see [README.md](README.md).*

Chapter [`07-types-domain.md`](07-types-domain.md) defines the abstract domain:
the type grammar (metavariable `T`, code type `Type_grammar.t`), typing
environments (`E`, `Typing_env.t`), environment extensions (`ε`,
`Typing_env_extension.t`), and the concretization `γ_E(T) ⊆ Val` — the set of
runtime values a type denotes in an environment. This chapter covers the four
operations Simplify performs *on* that domain:

- **meet** (`⊓`) — the greatest lower bound, used by every transfer function
  that *adds* a constraint (branch conditions, subkind constraints from the
  OCaml type system, projections). Meet also drives reduction, so it returns not
  just a type but an environment extension `ε` recording newly-learned equations.
- **join** (`⊔`) — the least upper bound, used at control-flow merge points
  (continuation handlers with several predecessors).
- **provers** — read-only queries (`E ⊢ prove_X(s) ⇒ r`) that Simplify uses to
  decide whether a rewrite is applicable.
- **expand_head** and **reify** — expansion of an alias/type to its head, and
  the conversion of a type back to a term or static constant when it denotes a
  single known value (used for constant propagation and lifting).

This chapter references the domain and `γ_E` from [§07](07-types-domain.md); it does not
redefine them. It introduces two chapter-local judgment forms for expansion and
reification (declared in their sections), in addition to the meet/join/prove
forms declared in the README.

## Implementation variants and dispatch

There are two implementations of meet and join, selected together by the
`-flambda2-join-algorithm` flag (`Oxcaml_flags.join_algorithm`, values `Binary`
| `N_way` | `Checked`; **default `Binary`**).

- **`Binary`** (default): meet is `Meet_and_join.meet`; the merge at join points
  is the old level-based binary join in `Join_levels_old.cut_and_n_way_join`,
  which folds the branches pairwise through the binary `Meet_and_join.join`.
- **`N_way`**: meet is `Meet_and_n_way_join.meet`; the merge is the true n-way
  join `Join_env.cut_and_n_way_join_with_analysis`. Required by the
  match-in-match optimization (`Flambda_features.join_algorithm` forces `N_way`
  when `match_in_match` is on).
- **`Checked`**: runs both join implementations and reports names whose joined
  types differ (a debugging mode); the environment it returns is the new one.

`Flambda_features.use_n_way_join` returns `true` only for `N_way`, and
`Meet.meet` (`types/meet.ml`) dispatches the top-level meet on it. There is a
separate legacy flag `-flambda2-meet-algorithm=basic|advanced`; it is accepted
but is a **no-op** in the current code (it only validates its argument).

Except where noted, the rules below describe the **default (`Binary`)** path.
The soundness *specifications* (`normative` rules) are shared by both
implementations; the *algorithmic* descriptions (`descriptive`) follow
`Meet_and_join.ml` / `Join_levels_old`, with the n-way join described separately.

```rule
RULE T.Meet.Dispatch
STATUS descriptive
CODE middle_end/flambda2/types/meet.ml#meet
CODE middle_end/flambda2/ui/flambda_features.ml#use_n_way_join
CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
---
join_algorithm() = Binary  (default)
--------------------------------------------------
meet := Meet_and_join.meet    (binary)
join-at-merge-points := Join_levels_old.cut_and_n_way_join (pairwise binary join)
NOTES: N_way selects Meet_and_n_way_join.meet and Join_env.cut_and_n_way_join_with_analysis.
  Checked runs both merges and diffs them. -flambda2-meet-algorithm is a no-op.
```

## 1. Meet

### Specification

The public entry point is
`meet : Typing_env.t -> t -> t -> (t * Typing_env.t) Or_bottom.t`
(`flambda2_types.mli`). Given `E`, `T₁`, `T₂` (necessarily of the same kind), it
returns either `Bottom` or a pair `(T, E′)` where `E′` is `E` augmented with the
equations learned during the meet. In the judgment `E ⊢ T₁ ⊓ T₂ = T ▷ ε`, the
extension `ε` is exactly the delta `E′ ∖ E`; we write `E; ε` for `E′`.

Meet is the operation by which Simplify *refines* what it knows. The
safety-critical direction is therefore that meet must **not drop** any value
consistent with both inputs.

```rule
RULE T.Meet.Sound
STATUS normative
CODE middle_end/flambda2/types/meet.ml#meet
CODE middle_end/flambda2/types/env/meet_env.ml#meet
---
E ⊢ T₁ ⊓ T₂ = T ▷ ε
--------------------------------------------------
γ_{E;ε}(T) ⊇ γ_E(T₁) ∩ γ_E(T₂)
NOTES: Over-approximation of the intersection. The *ideal* meet is exact,
  γ_{E;ε}(T) = γ_E(T₁) ∩ γ_E(T₂); the domain returns a representable type that
  contains the intersection when it cannot represent it exactly. This is the
  direction that makes refinement sound: whatever value a name really has, if it
  was in both γ_E(T₁) and γ_E(T₂), it is still in the refined type. The
  extension ε is likewise sound: any concrete valuation of E in which the
  meet'd value lies in both γ_E(T₁) and γ_E(T₂) satisfies ε.
```

```rule
RULE T.Meet.Bottom
STATUS normative
CODE middle_end/flambda2/types/meet.ml#meet
CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
---
E ⊢ T₁ ⊓ T₂ = ⊥
--------------------------------------------------
γ_E(T₁) ∩ γ_E(T₂) = ∅
NOTES: A ⊥ result asserts the two facts are jointly unsatisfiable, i.e. the
  program point is unreachable. This must hold on the nose: Simplify turns a ⊥
  meet into dead code / Invalid, so an unsound ⊥ would delete live code.
```

Meet is also intended to be a genuine lower bound (`γ_{E;ε}(T) ⊆ γ_E(T₁)` and
`⊆ γ_E(T₂)`), i.e. the *greatest* lower bound in the lattice — the `.mli`
describes it as "greatest lower bound of two types". The implementation aims for
exactness; the lower-bound property can be lost only in the incomplete corners
of the `Row_like` domain (partially-known tag/size; see [§07](07-types-domain.md) and the
`join` incompleteness note below). We record this as the design intent rather
than a separately-verified guarantee.

```rule
RULE T.Meet.GreatestLowerBound
STATUS conjectured
CODE middle_end/flambda2/types/meet_and_join.mli#meet
---
E ⊢ T₁ ⊓ T₂ = T ▷ ε
--------------------------------------------------
γ_{E;ε}(T) ⊆ γ_E(T₁)   and   γ_{E;ε}(T) ⊆ γ_E(T₂)
NOTES: Design intent (GLB). Combined with T.Meet.Sound this would make meet
  exact. It is *refutable* as a universal claim: the Variant-vs-Mutable_block
  case in meet_head_of_kind_value_non_null returns a Mutable_block carrying only
  the met alloc modes, discarding the Row_like equations on the known immutable
  fields of the Variant input (the code comment there gives a concrete example
  where sharing is lost). The result's concretization is then *not* ⊆ the
  Variant input's. Exactness holds elsewhere; kept conjectured because the code
  itself documents this and the other incomplete Row_like corners as precision
  losses. Not separately verified.
```

### The three input shapes: aliases first

The top-level meet (`Meet_env.meet`, called via `Meet_and_join.meet`) classifies
each input by whether it is (or canonicalizes to) an *alias* type `= s`. Writing
`can_E(T)` for the canonical simple of `T` when `T` is an alias:

```rule
RULE T.Meet.AliasAlias
STATUS normative
CODE middle_end/flambda2/types/env/meet_env.ml#meet
CODE middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals
---
can_E(T₁) = s₁     can_E(T₂) = s₂
ε records the alias equation s₁ = s₂ (demoting the later-bound to the earlier)
the underlying types of s₁ and s₂ are met, contributing further equations to ε
--------------------------------------------------
E ⊢ T₁ ⊓ T₂ = (= s₁) ▷ ε        (equivalently (= s₂); both are correct)
NOTES: Meeting two aliases always records their equality, so returning either
  alias is sound (the code returns Both_inputs). Canonicalization uses the total
  order on names (binding times) from [§07](07-types-domain.md): the later name is demoted, its
  previous concrete type is met onto the canonical one so no information is lost.
```

```rule
RULE T.Meet.AliasConcrete
STATUS normative
CODE middle_end/flambda2/types/env/meet_env.ml#meet
CODE middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical
---
can_E(T₁) = s₁     T₂ is a non-alias (concrete) type
ε refines the type of s₁ by meeting its existing type with T₂ (and any
  reductions that triggers)
--------------------------------------------------
E ⊢ T₁ ⊓ T₂ = (= s₁) ▷ ε
NOTES: The result is the alias; the informative part is ε. Symmetric case
  (concrete on the left, alias on the right) returns (= s₂). When both inputs
  are concrete, the result is the meet of their expanded heads (next section).
```

`add_concrete_equation_on_canonical` is where reduction happens: adding a type
to a canonical name meets it with the name's *existing* type, and recursively
propagates demotions (`record_demotion`) and relational reductions. Recursive
equations on a name already being updated are dropped
(`adding_equation_for_name`), which is what bounds the reduction fixpoint.

### Head-vs-head meet

When both inputs are concrete, meet expands each to a head
(`Expand_head.expand_head0`) and dispatches on the kind
(`meet_expanded_head0`). Naked-number kinds meet by finite-set intersection;
`Value` heads meet structurally.

```rule
RULE T.Meet.NakedNumber
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#meet_expanded_head0
CODE middle_end/flambda2/types/meet_and_join.ml#set_meet
---
T₁, T₂ are heads of the same naked-number kind, denoting immediate sets S₁, S₂
--------------------------------------------------
E ⊢ T₁ ⊓ T₂ = (the head for S₁ ∩ S₂) ▷ ∅        (⊥ if S₁ ∩ S₂ = ∅)
NOTES: Naked floats/int8/16/32/64/nativeint/vec128/256/512 and naked immediates.
  No extension is produced (no relational content), except naked-immediate meet
  which additionally reduces is_int/get_tag/is_null relations (T.Meet.Relational).
```

```rule
RULE T.Meet.ValueHeadIncompatible
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
---
T₁, T₂ are non-null value heads with different top constructors
  (e.g. Variant vs Boxed_float, String vs Closures, ...)
(excluding the Variant/Mutable_block pair, handled specially)
--------------------------------------------------
E ⊢ T₁ ⊓ T₂ = ⊥
NOTES: "This assumes that all the different constructors are incompatible. This
  could break very hard for dubious uses of Obj." Boxed numbers of matching
  flavour, closures, strings and arrays meet componentwise; Variant meets
  Variant via T.Meet.Variant. Mutable_block vs Variant is treated as
  Mutable_block being the more precise (meeting only the alloc modes).
```

The `Value` kind additionally carries a nullability component (`is_null`):
`Not_null` meets `Maybe_null` to `Not_null`, and two `Maybe_null` components meet
their `is_null` relation variables. This is folded together with the non-null
head via `meet_disjunction`.

### Variants: relational information and per-case extensions

A `Variant` head bundles the immediates (tagged-integer constructors), the
`Row_like_for_blocks` (boxed constructors), and relational fields `is_int`,
`get_tag`, and per-disjunct `extensions` ([§07](07-types-domain.md)). Meeting variants must
preserve and propagate this relational content.

```rule
RULE T.Meet.Variant
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#meet_variant
CODE middle_end/flambda2/types/meet_and_join.ml#meet_relation
---
meeting Variant{is_int₁,get_tag₁,blocks₁,imms₁,ext₁} with Variant{is_int₂,...}
--------------------------------------------------
immediates := imms₁ ⊓ imms₂        blocks := blocks₁ ⊓ blocks₂ (Row_like meet)
is_int, get_tag: the relation variables are aliased (meet_relation)
extensions: joined across the surviving disjuncts (meet_disjunction)
result is ⊥ iff both the immediates and the blocks components are ⊥
NOTES: get_tag is met *inside* meet_disjunction because reductions it triggers
  (learning the tag) are only valid when the value is a block, so they must be
  captured in an extension rather than added to the ambient environment. is_int
  is met outside, in the ambient environment, because every variant has a
  well-defined is_int. is_unique is the OR of the inputs (propagated for both
  meet and join). This is the mechanism the types.md "extensions in the
  environment" section describes: when only one disjunct survives, its extension
  is added to the result.
```

```rule
RULE T.Meet.BlockShape
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#meet_row_like_for_blocks
CODE middle_end/flambda2/types/meet_and_join.ml#join_row_like_for_blocks
CODE middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape
CODE middle_end/flambda2/kinds/flambda_kind.ml#Block_shape.equal
VERIFIED 14-validation/mixed-04-join.md
---
meeting two block cases with shapes σ₁, σ₂ (as part of the Row_like block meet):
  Block_shape.equal σ₁ σ₂  ⟹  the met index keeps shape σ₁ (fields met pointwise)
  ¬ Block_shape.equal σ₁ σ₂ ⟹  the two cases do not meet (that index pair is ⊥)
joining two block cases:
  Block_shape.equal σ₁ σ₂  ⟹  the joined index keeps shape σ₁
  ¬ Block_shape.equal σ₁ σ₂ ⟹  the shape is Unknown for that index
--------------------------------------------------
Block-case meet/join require equal Block_shape (incl. equal Mixed_record σ);
otherwise meet yields ⊥ for that case and join drops to Unknown shape.
NOTES: meet_row_like_for_blocks passes meet_shape = (fun σ₁ σ₂ -> if
Block_shape.equal σ₁ σ₂ then Ok σ₁ else Bottom); join_row_like_for_blocks passes
join_shape = (fun σ₁ σ₂ -> if Block_shape.equal σ₁ σ₂ then Known σ₁ else Unknown).
When join keeps a shape but a field join is Unknown, join_int_indexed_product
fills that field with unknown_from_shape(σ, i) (the per-field top of the shape's
element kind, [§07](07-types-domain.md)) rather than a bare Unknown, so the field-kind discipline
of the shape is preserved. Block_shape.equal is exact (no subshaping): two mixed
shapes differing in prefix size or any flat_suffix_element are incompatible.
```

```rule
RULE T.Meet.Relational
STATUS descriptive
CODE middle_end/flambda2/types/meet_and_join.ml#reduce_inverse_relations
CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_naked_immediate
---
meeting a naked-immediate type carrying inverse relations Is_null / Is_int / Get_tag
against a narrowed immediate set S
--------------------------------------------------
if S determines the relation, add the implied equation to the related name:
  Is_int, S={1}  ⟹  related value : any_tagged_immediate
  Is_int, S={0}  ⟹  related value : any_block
  Get_tag, S determines tags  ⟹  related value : blocks_with_these_tags
  (Is_null analogous with null / any_non_null_value)
  S = ∅  ⟹  ⊥
NOTES: This is the reduction from the tag/is_int relational domain back onto the
  block/variant it constrains. Reverse reductions are only performed for
  relations not already known on that side (avoids redundant work / loops).
```

### Termination

Meet can recurse (head meet → field meet → ...) and the top-level extension
must itself be re-applied by a further meet
(`add_concrete_equation_on_canonical` meets against existing types).
Convergence is guaranteed *not* by widening but by two structural facts, per
types.md: the domains have no infinite descending chains, and meet never
generates an extension that fails to strictly constrain the environment.
`adding_equation_for_name` additionally drops equations on a name already being
updated, breaking cycles through aliases.

```rule
RULE T.Meet.Terminates
STATUS descriptive
CODE middle_end/flambda2/types/env/meet_env.ml#adding_equation_for_name
CODE middle_end/flambda2/types/meet_and_join.ml#meet
---
--------------------------------------------------
The meet/reduction fixpoint terminates: no infinite descending chains in the
domains, extensions strictly constrain, recursive equations on an in-progress
name are dropped.
```

## 2. Join

### Specification

Join is used to combine what is known on several incoming edges of a
control-flow merge (a continuation handler with multiple predecessors). It must
**over-approximate the union**: no value possible on any incoming edge may be
lost.

```rule
RULE T.Join.Sound
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#join
CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
VERIFIED 14-validation/n_way_join_null.md
VERIFIED 14-validation/n_way_join_preserves_null.md
---
E ⊢ T₁ ⊔ ⋯ ⊔ Tₙ = T
--------------------------------------------------
γ_E(T) ⊇ γ_E(T₁) ∪ ⋯ ∪ γ_E(Tₙ)
NOTES: Least upper bound (intent). Unlike meet, join produces no extension: its
  result type is added to the fork environment for the joined names. Join is
  allowed to be imprecise (return a larger type, up to Unknown); it must never
  be *smaller* than the union. There is no ⊥ result for a non-empty join (⊥
  inputs are absorbed: joining ⊥ with T gives T).
```

Join has no widening (types.md, Introduction): the analysis is single-pass, so
there is no fixpoint to widen. Imprecision is bounded instead by falling back to
`Unknown`.

### Binary join (default)

The binary join (`Meet_and_join.join`) operates in a `Join_env` carrying three
environments: the left branch env, the right branch env, and the *target*
(the common ancestor / fork env into which the result is written). It prefers to
return a **shared alias**: if `T₁` canonicalizes to `s₁` in the left env and `T₂`
to `s₂` in the right, and the two names have a common alias that is bound
strictly earlier than the name being defined, that alias is the join result.

```rule
RULE T.Join.SharedAlias
STATUS normative
CODE middle_end/flambda2/types/meet_and_join.ml#join
---
all_aliases_of(s₁) in target  ∩  all_aliases_of(s₂) in target  ∋  s
(and, if joining under a bound_name, s is bound strictly earlier than bound_name)
--------------------------------------------------
E ⊢ T₁ ⊔ T₂ = (= s)
NOTES: Aliases are preferred because the alias typically carries a concrete
  equation anyway. Self-aliases and mutually-redundant equations (x:(=y),
  y:(=x)) are filtered out via alias_is_bound_strictly_earlier.
```

```rule
RULE T.Join.Head
STATUS descriptive
CODE middle_end/flambda2/types/meet_and_join.ml#join_expanded_head
CODE middle_end/flambda2/types/meet_and_join.ml#join_head_of_kind_value_non_null
---
no shared alias
--------------------------------------------------
E ⊢ T₁ ⊔ T₂ = to_type(join of expanded heads)
  Bottom ⊔ H = H;  H ⊔ Bottom = H;  Unknown ⊔ _ = Unknown;  _ ⊔ Unknown = Unknown
  Value/naked-number heads joined structurally (set union for finite sets,
    componentwise for products, disjunct-wise for Row_like)
NOTES: When two alias types are joined and neither the shared-alias case nor a
  structural join applies, the relation variables (is_int/get_tag) are kept only
  if syntactically equal (join_relation); otherwise dropped.
```

```rule
RULE T.Join.Cutoff
STATUS descriptive
CODE middle_end/flambda2/types/meet_and_join.ml#join
CODE middle_end/flambda2/ui/flambda_features.ml#join_depth
---
join is already recursively joining the pair (s₁, s₂), or join depth exceeded
--------------------------------------------------
E ⊢ T₁ ⊔ T₂ = Unknown
NOTES: already_joining / now_joining track in-progress pairs; -flambda2-join-depth
  bounds recursion. This (not widening) is what tames recursive/cyclic types:
  the join returns Unknown rather than diverging.
```

### N-way join and levels

To avoid a full environment join at every merge point, environments are divided
into **levels** (types.md, "Typing_env_level"). Each branch shares a common
ancestor env (the fork). Rather than joining full environments, the join joins
the *level extensions* — the equations added since the fork — of all branches at
once, and adds their join to the ancestor (as if by a meet).

```rule
RULE T.Join.Levels
STATUS descriptive
CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
CODE middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0
---
fork env E₀; branches E₀;εᵢ (i = 1..n), each cut after scope [cut_after]
--------------------------------------------------
result = E₀ extended by (⊔ᵢ εᵢ), computed over the levels created since the fork.
NOTES: Binary mode (default) folds the branches pairwise (Join_levels_old);
  N_way mode joins all n at once in a fixpoint loop (cut_and_n_way_join0) that
  repeatedly joins canonicals and their types until no new equations appear,
  returning a Join_analysis describing per-use refinements. The n-way join is
  the only path that can produce the Join_analysis consumed by match-in-match.
```

### Existential variables

Names defined *after* the fork exist in a branch but not in the ancestor. On
join, each such name that survives is turned into an **existential variable**
([§07](07-types-domain.md)): its type is kept in the result but it is quantified existentially
in the concretization. Names present in only one branch are introduced in the
others with a special "poison"/bottom-like value that does not make the whole
environment bottom (types.md, "Join algorithm").

```rule
RULE T.Join.Existentials
STATUS descriptive
CODE middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0
CODE middle_end/flambda2/types/join_levels_old.ml#cut_and_n_way_join
---
name x defined after the fork, surviving the join
--------------------------------------------------
x is introduced into the result as an existential (In_types name mode)
NOTES: Continuation parameters are the exception: they are defined at the very
  end of each branch (as aliases to the continuation arguments) but correspond
  to real program variables, so they must NOT be existentially quantified
  (types.md, "Typing_env_level").
```

### Recursive continuation parameters

Recursive continuations create a genuine cycle: a handler's parameter types
would depend on uses that include the handler itself. Because there is no
fixpoint/widening, Simplify does **not** join the back-edge into the parameter
types. Instead the parameters of a recursive handler are given `Unknown` types
before the handler is simplified.

```rule
RULE T.Join.RecursiveParamsUnknown
STATUS descriptive
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types
---
handler is recursive
--------------------------------------------------
each handler's own (variant) parameters are added to its handler env with
  Unknown types (DE.add_parameters_with_unknown_types), not the join of the
  argument types
NOTES: This is why loops lose type information at variant recursive-continuation
  parameters: there is no join over the back edge. simplify_single_recursive_handler
  calls add_parameters_with_unknown_types on exactly the per-handler [params].
  The *invariant* parameters (passed unchanged around the loop) are the exception:
  they DO receive the n-way join of their arguments over the uses seen in the
  body (a single pass, via compute_handler_env with is_recursive), so they are
  not Unknown — but they are still not re-refined by any fixpoint over the back
  edge. Fuller treatment belongs to [§09](09-simplify-structure.md)
  (S.Struct.Rec.InvariantVsVariant).
```

## 3. Provers

Provers are read-only queries over a type in an environment. They live in
`Provers` (`types/provers.mli`) and are the interface Simplify's rewrites use to
test applicability. Two result flavours encode the two naming conventions:

- `prove_X : E -> T -> r proof_of_property`, where `proof_of_property` is
  `Proved of r | Unknown`. A pure read of the type; never reports a
  contradiction.
- `meet_X : E -> T -> r meet_shortcut`, where `meet_shortcut` is
  `Known_result of r | Need_meet | Invalid`. Computes the result *as if by
  meeting `T` with a shape*, so it can additionally detect `Invalid` (the meet
  would be `⊥` — a contradiction) and `Need_meet` (couldn't be answered cheaply;
  the caller should fall back to a full meet). `meet_X` returns a value, not a
  refined environment — the "meet" refers to the internal computation.

```rule
RULE T.Prove.Sound
STATUS normative
CODE middle_end/flambda2/types/provers.mli#proof_of_property
CODE middle_end/flambda2/types/provers.ml#prove_is_int
---
E ⊢ prove_X(T) ⇒ Proved(r)
--------------------------------------------------
every v ∈ γ_E(T) satisfies property X with witness r
NOTES: Proved is a universal claim over the concretization. Unknown is always a
  sound answer (claims nothing). A prover must never return Proved(r) unless the
  property holds for *all* concrete values of T.
```

```rule
RULE T.Prove.MeetShortcut
STATUS normative
CODE middle_end/flambda2/types/provers.mli#meet_shortcut
CODE middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates
---
E ⊢ meet_X(T) ⇒ r'
--------------------------------------------------
Known_result(r):  the meet of T with shape X yields witness r, sound like Proved
Invalid:           γ_E(T) ∩ γ_E(shape X) = ∅   (T is incompatible with X: ⊥)
Need_meet:         no cheap answer; equivalent to Unknown for soundness
NOTES: Invalid is the extra expressive power over prove_: it may only be returned
  when the intersection is genuinely empty (cf. T.Meet.Bottom). Because meet_X
  meets against a shape, it can be strictly more precise (find results / find
  Invalid) than the corresponding prove_X, which only reads the existing type.
```

Prover families (from `provers.mli`), grouped by what they establish:

- **Immediates / scalars.** `prove_equals_tagged_immediates`,
  `meet_equals_tagged_immediates`, `meet_equals_single_tagged_immediate`,
  `meet_naked_immediates`, and the per-kind `meet_naked_{float32,float,int8,
  int16,int32,int64,nativeints,vec128,vec256,vec512}` — the finite set of
  possible scalar values, used by constant folding.
- **Discrimination (is_int / get_tag / tag+size).** `prove_is_int`,
  `prove_is_not_a_pointer`, `meet_is_int_variant_only`, `prove_get_tag`,
  `prove_unique_tag_and_size`, `meet_is_null` — used to simplify `Switch` and
  `Is_int`/`Get_tag` primitives.
- **Boxing / tagging.** `prove_is_a_boxed_{float32,float,int32,int64,nativeint,
  vec128,vec256,vec512}`, `prove_is_a_tagged_immediate`,
  `prove_is_a_boxed_or_tagged_number`, `prove_is_or_is_not_a_boxed_float`,
  `prove_alloc_mode_of_boxed_number`, and the `meet_boxed_*_containing_simple` /
  `meet_tagging_of_simple` family that extract the boxed/tagged contents as a
  `Simple` (used to elide box/unbox pairs).
- **Variants.** `prove_variant_like`, `meet_variant_like` —
  the constant and non-constant constructors with their sizes.
- **Blocks / arrays / strings.**
  `prove_unique_fully_constructed_immutable_heap_block`,
  `meet_block_field_simple`, `meet_is_flat_float_array`,
  `meet_is_immutable_array`, `prove_is_immutable_array`,
  `prove_is_immediates_array`, `meet_is_non_empty_naked_number_array`,
  `prove_strings`, `meet_strings`.
- **Closures.** `prove_single_closures_entry`, `meet_single_closures_entry`
  (the sole function slot + closure contents of a known closure, used to turn an
  indirect call into a direct one), `meet_project_value_slot_simple`,
  `meet_project_function_slot_simple`.
- **Misc.** `prove_equals_to_simple_of_kind` (the canonical simple a type is
  equal to), `meet_rec_info`, `never_holds_locally_allocated_values`,
  `prove_physical_equality`, `prove_nothing` (always `Unknown`).

```rule
RULE T.Prove.GetTag
STATUS normative
CODE middle_end/flambda2/types/provers.ml#prove_get_tag
---
E ⊢ prove_get_tag(T) ⇒ Proved(tags)
--------------------------------------------------
every block v ∈ γ_E(T) has Tag(v) ∈ tags
NOTES: Representative discrimination prover; drives Switch simplification in
  [§10](10-simplify-rewrites.md) together with prove_is_int and prove_equals_tagged_immediates.
```

## 4. expand_head and reify

### expand_head

`expand_head E T` (`types/expand_head.ml`) resolves the outermost `Alias`: it
looks up the canonical simple of `T` and returns that name's *concrete*
(non-alias) head as an `Expanded_type.t`, or `Unknown`/`Bottom`. We write the
chapter-local judgment `E ⊢ T ⇓ H` ("`T` expands to head `H`").

```rule
RULE T.Expand.Head
STATUS normative
CODE middle_end/flambda2/types/expand_head.ml#expand_head
CODE middle_end/flambda2/types/expand_head.ml#expand_head0
---
E ⊢ T ⇓ H
--------------------------------------------------
γ_E(T) = γ_E(H)        (expansion preserves concretization)
NOTES: If T is not an alias, H is T's own head. If T = (= s), H is the head
  stored for the canonical of s; a canonical is never itself an Equals type
  ([§07](07-types-domain.md) invariant), so expansion terminates in one step. Phantom / absent
  canonicals expand to Unknown (sound: γ = all values). expand_head0 takes the
  precomputed canonical to avoid recomputation and is the form meet/join call.
```

### reify

`reify` (`types/reify.ml`) attempts to turn a type back into a term: a `Simple`
(a name or constant the value is known equal to) or an instruction to `Lift` a
static constant (immutable block / boxed number / immutable array). It returns
`reification_result = Lift of to_lift | Simple of Simple.t | Cannot_reify |
Invalid`. We write `E ⊢ reify(T) ⇒ r`.

```rule
RULE T.Reify.Sound
STATUS normative
CODE middle_end/flambda2/types/reify.ml#reify
---
E ⊢ reify(T) ⇒ r
--------------------------------------------------
r = Simple s   ⟹  every v ∈ γ_E(T) equals the value of s in E
r = Lift lift  ⟹  every v ∈ γ_E(T) equals the statically-allocated value [lift]
r = Invalid    ⟹  γ_E(T) = ∅
r = Cannot_reify ⟹  no claim
NOTES: Reify only commits to Simple / Lift when the type denotes a *single*
  known value (get_singleton on the immediates, a unique tag+size block with
  reifiable fields, etc.). Constant tagged immediates and fully-constructed
  immutable blocks/arrays are the main lifted shapes.
```

```rule
RULE T.Reify.LiftLocalGuard
STATUS normative
CODE middle_end/flambda2/types/reify.ml#reify
CODE middle_end/flambda2/types/provers.ml#never_holds_locally_allocated_values
---
reify considers lifting a block/array with fields that are variables
--------------------------------------------------
a field variable x is allowed only if x is defined at toplevel (or is a symbol
  projection) AND, for a Local/Heap_or_local alloc mode, x provably never holds
  locally-allocated values (never_holds_locally_allocated_values)
NOTES: Lifting a Local allocation to a static constant would extend the lifetime
  of any local value reachable from it past its region, so it is gated. Heap
  allocations are always liftable (the OCaml type system validated them). Fields
  under a non-identity coercion are not yet lifted.
```

Both `expand_head` and `reify` are read-only: neither adds equations to `E`.

## 5. Worked examples

These are adapted from `middle_end/flambda2/tests/meet_test.ml`, which drives the
real `T.meet` / `T.cut_and_n_way_join` and prints the environments.

### Example A — meet along an alias chain recovers a concrete type

From `test_meet_chains_two_vars`. Initial environment:

```
var1 : (Block (tag 0) (size 2) [ any_tagged_immediate ])
var2 : (= var1)
my_symbol : ⊤        (just defined, no equation)
```

We then learn `var2 : (= my_symbol)` and meet the two facts about `var2`:
`(= var1) ⊓ (= my_symbol)`. This is the alias-vs-alias case (`T.Meet.AliasAlias`):
both are aliases, so the extension records the equality. Ordering by binding time
(`var1` and `my_symbol` are both earlier than `var2`; the symbol is chosen as
canonical), the result environment is:

```
var1 : (= my_symbol)                    (var1 demoted to the symbol)
my_symbol : (Block (tag 0) (size 2) [ any_tagged_immediate ])   (block type moved here)
var2 : (= my_symbol)
```

The block type that was on `var1` is not lost: `record_demotion` meets it onto
the new canonical `my_symbol`. The three-variable variant
(`test_meet_chains_three_vars`) chains one step further and behaves the same way.

### Example B — meet with an incompatible constant yields Bottom

From `test_meet_bottom_after_alias`. We have `x : {-1, 0, 1}` (three tagged
immediates) and learn `x : (= 3)`. Meeting `{-1,0,1} ⊓ (= 3)` is the
alias-vs-concrete case; expanding `(= 3)` to the head `{3}` and intersecting the
immediate sets gives `{-1,0,1} ∩ {3} = ∅`. By `T.Meet.NakedNumber` /
`T.Meet.Bottom` the meet is `⊥`, and `x` becomes `Bottom` — the program point is
unreachable.

### Example C — join of array element kinds falls back correctly

From `test_meet_array_element_kinds`, which builds an "unknown array" by joining
a mutable and an immutable array (`T.cut_and_n_way_join` over two branch
environments), then meets it with a `Local` immutable float array. The join of a
mutable and an immutable array produces an array type whose *element kind* is
known but whose contents/mutability is `Unknown` (`T.Join.Head`). Meeting that
with an immutable `naked_float` array whose element kind is incompatible with the
joined `any_value` element kind yields `⊥` (`T.Meet.Bottom`) — the test exists
precisely to confirm this is bottom rather than a bogus empty-kind array.

## Summary: rule IDs in this chapter

Meet: `T.Meet.Dispatch`, `T.Meet.Sound`, `T.Meet.Bottom`,
`T.Meet.GreatestLowerBound`, `T.Meet.AliasAlias`, `T.Meet.AliasConcrete`,
`T.Meet.NakedNumber`, `T.Meet.ValueHeadIncompatible`, `T.Meet.Variant`,
`T.Meet.BlockShape`, `T.Meet.Relational`, `T.Meet.Terminates`.

Join: `T.Join.Sound`, `T.Join.SharedAlias`, `T.Join.Head`, `T.Join.Cutoff`,
`T.Join.Levels`, `T.Join.Existentials`, `T.Join.RecursiveParamsUnknown`.

Provers: `T.Prove.Sound`, `T.Prove.MeetShortcut`, `T.Prove.GetTag`.

Expand / reify: `T.Expand.Head`, `T.Reify.Sound`, `T.Reify.LiftLocalGuard`.
