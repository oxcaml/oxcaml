# Memory and structure primitives

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter gives the denotational semantics of the primitives that build,
inspect and mutate heap-allocated structure: blocks, arrays, strings, closures,
lazy values, atomics and raw memory. It also defines the **effects and
coeffects** classification that Simplify relies on to decide which primitive
applications may be reordered, deleted or duplicated.

Scalar arithmetic, comparison, conversion, boxing/unboxing and tagging
primitives are [§05](05-primitives-scalar.md)'s; this chapter references but does not redefine them.
The abstract machine, its configuration `⟨e, ρ, K, H, T, R⟩`, and the runtime
value grammar are [§04](04-opsem.md)'s. Where [§04](04-opsem.md) leaves the heap-object taxonomy
abstract, this chapter refines it (below); the refinement is consistent with the
README metavariables and must stay consistent with [§04](04-opsem.md).

The denotation judgment (declared in the README) is

```
⟦p⟧(v̄; H) = (v, H′)   |   undef
```

`p` is a primitive application whose arguments have already been evaluated to
runtime values `v̄`; `H` is the heap. The primitive may allocate or mutate,
producing a new heap `H′` and a result value `v`. `undef` marks undefined
behaviour: the primitive was applied outside its contract (wrong shape, index
out of range, …). Flambda 2 primitives are **unchecked** — safety comes from
checks the frontend inserts around them (see
[§ Unchecked primitives](#unchecked-primitives)); a primitive reached with an
out-of-contract argument has no defined meaning and the compiler is entitled to
assume it never happens.

Four families are *not* expressible by `⟦p⟧` as written, because their meaning
touches machine components other than `H`: the region delimiters
(`Begin_region`, `End_region`, `Begin_try_region`, `End_try_region`) act on the
region stack `R`, and the nullary control primitives (`Poll`, `Enter_inlined_apply`,
…) act on nothing observable in this model. For those we use an augmented
judgment threading `R`, cross-referenced to [§04](04-opsem.md), and flag it explicitly.

## Heap objects

[§04](04-opsem.md) owns `H` (a finite map from locations `ℓ` to heap objects) and the
runtime value grammar. This chapter refines the heap-object grammar `o`:

```
o ::= Block(t, μ, v̄)          scannable block: tag t : Tag.Scannable, mutability μ, fields v̄
    | FloatBlock(μ, f̄)         naked-float block (runtime Double_array_tag), floats f̄
    | MixedBlock(t, μ, σ, v̄)   mixed block: scannable tag t, shape σ, logical fields v̄
    | Array(ak, μ, v̄)          array of kind ak (unarized element sequence v̄)
    | Bytes(μ, b̄)              string/bytes: byte sequence b̄
    | Bigstring(b̄)             off-heap byte buffer (always mutable)
    | Bigarray(bk, layout, d̄, ē)  bigarray: element kind bk, layout, dims d̄ = [d₁ … dₙ], off-heap elements ē (always mutable)
    | Closures(…)              set of closures ([§04](04-opsem.md), built by OS.Let)
    | Boxed(κ, c)              boxed number of boxable kind κ holding constant c ([§05](05-primitives-scalar.md))
    | Lazy(t, v)               lazy/forward block, lazy_block_tag t ∈ {Lazy_tag, Forward_tag}
```

Runtime values `v` ([§04](04-opsem.md)) include at least tagged immediates `tagged_imm n`
and naked immediates `naked_imm n`, pointers `ptr ℓ` to heap objects, the null
pointer `null`, naked numbers, and region tokens `region ι`. `μ` ranges over `Mutability.t = {Immutable,
Immutable_unique, Mutable}`.

Allocation is written with the helper ([§04](04-opsem.md); restated for reference):

```
alloc(o, H) = (ℓ, H[ℓ ↦ o])     for some ℓ ∉ dom(H)
```

An allocation's `Alloc_mode.For_allocations.t` (`Heap` or `Local`) determines
whether `ℓ` lives on the GC heap or in the current region (top of `R`). Region
placement is tracked by [§04](04-opsem.md); the value produced and the heap update are
the same either way, so the `⟦p⟧` rules below elide the mode except where it
changes the coeffects (§ Effects).

### Access-kind taxonomy

The block, array and atomic primitives carry static descriptors fixing the
representation of the object and of the accessed field. The taxonomy this
chapter uses, read off `flambda_primitive.mli`:

- **`Block_access_kind`** (`Values {tag; size; field_kind}` | `Naked_floats
  {size}` | `Mixed {tag; size; field_kind; shape}`) tags the block's runtime
  representation for a `Block_load`/`Block_set`. In every case `tag :
  Tag.Scannable Or_unknown` and `size : Target_ocaml_int Or_unknown` are
  refinements used by the type system. For `Values`, `field_kind ∈ {Any_value,
  Immediate}` records whether the field is a scannable value or a tagged
  immediate (for `Block_load` it is always `Any_value`, noted in the code). For
  `Mixed`, `field_kind : Mixed_block_access_field_kind` is either `Value_prefix
  bfk` (with `bfk ∈ {Any_value, Immediate}`) for a prefix field or `Flat_suffix
  e` (with `e` a `flat_suffix_element`, [§03](03-kinds.md)) for a suffix field,
  and `shape : Mixed_block_shape` is the full block shape σ. The derived
  `element_kind_for_load`/`element_subkind_for_load` map the field kind to the
  loaded value's kind: `Value_prefix _ ↦ Value` (subkind `tagged_immediate` for
  `Immediate`, else `any_value`) and `Flat_suffix e ↦` the naked-number kind of
  `e` (`flambda_primitive.ml#Block_access_kind.element_kind_for_load`,
  `#element_subkind_for_load`).
- **`Block_kind`** (`Values (tag, κ̂ list)` | `Naked_floats` | `Mixed (tag,
  σ)`) is the corresponding descriptor for `Make_block`; it fixes the tag and
  the kinds of all fields. For `Mixed` the shape σ (`Mixed_block_shape.t`)
  determines the per-field kinds via `field_kinds(σ)` (see § Mixed blocks).
- **`Array_kind`** enumerates the element representation: `Immediates`,
  `Values`, `Gc_ignorable_values`, `Naked_floats`, `Naked_float32s`,
  `Naked_ints`, `Naked_int8s`/`16s`/`32s`/`64s`, `Naked_nativeints`,
  `Naked_vec128s`/`256s`/`512s`, and `Unboxed_product` of a list of kinds.
  Unboxed-product arrays are **unarized**: an `Unboxed_product [κ₁; …; κₘ]`
  array of `n` logical elements is stored as `n·m` scalars, and `Array_load`
  /`Array_set`/`Array_length` all speak in terms of the unarized (scalar)
  index and length.
- **`Array_load_kind`** / **`Array_set_kind`** are the load/store counterparts
  (no `Unboxed_product`: a single unarized access is at one scalar kind). The
  set kind's `Values` case additionally carries `Init_or_assign` (initializing
  write vs assignment, the latter recording the assignment's alloc mode for the
  write barrier).
- **`Array_kind_for_length`** (`Array_kind ak` | `Float_array_opt_dynamic`) is
  what `Array_length` carries; the dynamic case is for the float-array
  optimisation where the element kind is not known statically.
- **`Bigarray_kind`** (`Float16`, `Float32`, `Float32_t`, `Float64`, `Sint8`,
  `Uint8`, `Sint16`, `Uint16`, `Int32`, `Int64`, `Int_width_int`,
  `Targetint_width_int`, `Complex32`, `Complex64`) and **`Bigarray_layout`**
  (`C` | `Fortran`) describe bigarray element storage and index order. Each
  kind fixes both a storage width and the Flambda kind elements are
  loaded/stored at (`Bigarray_kind.element_kind`,
  `backend/cmm_helpers.ml#bigarray_elt_size_in_bytes`):

  | `Bigarray_kind` | storage (bytes) | access kind |
  |---|---|---|
  | `Float16` | 2 | `naked_float` (widen/narrow on access) |
  | `Float32` | 4 | `naked_float` (widen/narrow on access) |
  | `Float32_t` | 4 | `naked_float32` |
  | `Float64` | 8 | `naked_float` |
  | `Sint8`, `Uint8` | 1 | `naked_immediate` (sign-/zero-extend) |
  | `Sint16`, `Uint16` | 2 | `naked_immediate` (sign-/zero-extend) |
  | `Int32` | 4 | `naked_int32` |
  | `Int64` | 8 | `naked_int64` |
  | `Int_width_int` | word | `naked_immediate` |
  | `Targetint_width_int` | word | `naked_nativeint` |
  | `Complex32` | 8 (2×f32) | `value` (boxed complex; read allocates) |
  | `Complex64` | 16 (2×f64) | `value` (boxed complex; read allocates) |

  These are the only element kinds: `Lambda.bigarray_kind` additionally has
  `Pbigarray_unknown` (and layout `Pbigarray_unknown_layout`), but those never
  reach Flambda — see P.Bigarray.Indexing.
- **`string_accessor_width`** (`Eight`, `Eight_signed`, `Sixteen`,
  `Sixteen_signed`, `Thirty_two`, `Single`, `Sixty_four`, `One_twenty_eight`,
  `Two_fifty_six`, `Five_twelve`, the vector ones carrying `{aligned}`) gives
  the width of a string/bytes/bigstring element access.

### Mixed blocks

A **mixed block** is a scannable block whose fields split into a *value prefix*
(ordinary GC-scanned `Value` fields) followed by a *flat suffix* of unboxed
scalars the GC does not scan. Its layout is fixed by a
`Flambda_kind.Mixed_block_shape.t`

```
σ ::= ⟨ value_prefix_size = p,  flat_suffix = ē = e₁ … e_m ⟩
e ::= Naked_float | Naked_float32 | Naked_int8 | Naked_int16 | Naked_int32
    | Naked_int64 | Naked_nativeint | Naked_immediate
    | Naked_vec128 | Naked_vec256 | Naked_vec512      (flat_suffix_element, [§03](03-kinds.md))
```

where `p ≥ 0` is the number of value-prefix fields and `ē` is the array of
flat-suffix element descriptors. Tagged immediates never appear in `ē` (the
suffix holds only unboxed scalars; `flambda_kind.ml` comment on
`flat_suffix_element`). A mixed block has `p + m` **logical fields**, indexed
`0 … p+m−1`.

```rule
RULE P.MixedShape.FieldKinds
STATUS normative
CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.from_prefix_size_and_suffix_elements
CODE middle_end/flambda2/kinds/flambda_kind.ml#Scannable_block_shape.element_kind
CODE middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape.field_kinds
VERIFIED 14-validation/mixed-01-record.md
---
field_kinds(σ) = [ Value, … (p copies) …, kind(e₁), …, kind(e_m) ]
field_kinds(σ)(i) = Value                     if 0 ≤ i < p
field_kinds(σ)(i) = kind(e_{i−p})             if p ≤ i < p + m
--------------------------------------------------
The kind of logical field i of a block of shape σ. The prefix fields are of kind
Value; each suffix field has the naked-number kind of its flat_suffix_element
(Flat_suffix_element.kind: Naked_float ↦ Naked_number Naked_float, etc.).
NOTES: field_kinds(σ) is stored on the shape and is uniquely determined by
(p, ē). It is the description used throughout Flambda 2; the flat_suffix array
itself is only needed to compute physical offsets for to_cmm (see
P.MixedShape.Offset).
```

```rule
RULE P.MixedShape.Offset
STATUS normative
CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words
CODE middle_end/flambda2/kinds/flambda_kind.ml#Flat_suffix_element0.size_in_words
CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.size_in_words
---
size_in_words(e) = 1  for scalar e;  = 2 (vec128) | 4 (vec256) | 8 (vec512)
offset_in_words(σ, i) = i                              if 0 ≤ i ≤ p
offset_in_words(σ, i) = p + Σ_{0 ≤ j < i−p} size_in_words(e_j)   if i > p
size_in_words(σ) = p + Σ_{0 ≤ j < m} size_in_words(e_j)
--------------------------------------------------
The physical word offset of logical field i. Prefix fields occupy one word each,
so their logical index and word offset coincide; each suffix element occupies
size_in_words words, so once a wide vector appears in ē the logical field index
and the physical word offset diverge.
NOTES: This word-offset arithmetic is a to_cmm concern, not part of the abstract
machine: the operational semantics below indexes MixedBlock by *logical* field,
and offset_in_words / size_in_words are cited only to ground the machine model in
the real layout. to_cmm_primitive.ml uses get/set_field_unboxed at
offset_in_words for suffix accesses, and to_cmm_static.ml emits the block header
with size size_in_words(σ) and scannable_prefix_len p.
```

The heap object built from such a shape is `MixedBlock(t, μ, σ, v̄)` (§ Heap
objects) with `|v̄| = p + m` logical fields, field i of kind
`field_kinds(σ)(i)`. The machine stores one runtime value per logical field; the
value-prefix/flat-suffix *physical* split is a to_cmm concern grounded by
`P.MixedShape.Offset`, not represented in `H`.

---

## Effects and coeffects

Every primitive application is classified by
`Flambda_primitive.effects_and_coeffects` into a quadruple

```
(Effects, Coeffects, Placement, Validity)
```

This is the contract Simplify obeys when moving, deleting or duplicating a
primitive. The four components are independent axes.

```rule
RULE P.Effects.Classification
STATUS normative
CODE middle_end/flambda2/terms/effects_and_coeffects.ml
CODE middle_end/flambda2/terms/effects.ml
CODE middle_end/flambda2/terms/coeffects.ml
---
Effects   ∈ { No_effects, Only_generative_effects(μ), Arbitrary_effects }
Coeffects ∈ { No_coeffects, Has_coeffects }
Placement ∈ { Delay, Strict }
Validity  ∈ { Can_move_anywhere, Can't_move_before_any_branch, Control_flow_point }
--------------------------------------------------
effects_and_coeffects(p) = (Effects, Coeffects, Placement, Validity)
NOTES: An *effect* is something the primitive does to the world (allocation,
mutation, divergence, raising). A *coeffect* is a dependence on the world
(reading mutable state). The two axes are dual: an effectful primitive cannot
be freely deleted/reordered because doing so changes the world; a co-effectful
one cannot be reordered past an effectful one because the world it reads might
change. Placement and Validity further restrict motion (below).
```

### The Effects axis

```rule
RULE P.Effects.NoEffects
STATUS normative
CODE middle_end/flambda2/terms/effects.ml#t
---
--------------------------------------------------
No_effects: the primitive may be deleted if its result is unused, and duplicated,
without changing observable behaviour. (It may still have coeffects, which
constrain reordering.)
```

```rule
RULE P.Effects.OnlyGenerative
STATUS normative
CODE middle_end/flambda2/terms/effects.ml#t
---
--------------------------------------------------
Only_generative_effects(μ): the primitive's only effect is to allocate a fresh
object of mutability μ. It may be deleted if its result is unused. It may be
duplicated only if μ = Immutable (duplicating a Mutable allocation would create
two distinct cells where the source had one, changing identity/aliasing).
NOTES: Fresh allocation is "generative": it produces a value not equal (by
physical equality) to any pre-existing value, but reveals nothing about, and
changes nothing in, the pre-existing heap.
```

```rule
RULE P.Effects.Arbitrary
STATUS normative
CODE middle_end/flambda2/terms/effects.ml#t
---
--------------------------------------------------
Arbitrary_effects: the primitive may mutate the heap, raise, diverge, or perform
I/O. It may not be deleted, reordered relative to other effects/coeffects, or
duplicated.
```

### The Coeffects axis

```rule
RULE P.Effects.Coeffects
STATUS normative
CODE middle_end/flambda2/terms/coeffects.ml#t
---
--------------------------------------------------
No_coeffects: the result does not depend on mutable state; two applications with
equal arguments at different program points yield equal results (⇒ CSE-able).
Has_coeffects: the result may depend on mutable state; the application may not be
moved across an effectful operation that could change what it reads, and is not
CSE-able across such operations.
```

### The Placement and Validity axes

```rule
RULE P.Effects.Placement
STATUS normative
CODE middle_end/flambda2/terms/placement.ml#t
---
--------------------------------------------------
Delay: the binding may be sunk to (delayed until) its use site, and dropped if
unused. Used for pure projections and heap boxing so they land next to their
consumer.
Strict: the binding must be evaluated where it is written (subject to the other
axes); it is not a candidate for delaying.
NOTES: join(Delay, Strict) = Strict.
```

Delay placement is the enabling condition for `to_cmm`'s duplicate-at-each-use
lowering, so it carries a side condition on the other two axes that no assertion
enforces but every producer respects.

```rule
RULE P.Effects.DelayDuplicable
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects
CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding
---
for every primitive p: placement(p) = Delay
--------------------------------------------------
coeffects(p) = No_coeffects  and  effects(p) ∈ { No_effects,
  Only_generative_effects Immutable }. Delay-placed defining expressions are
therefore safe to move to their (possibly multiple) use sites: they read no
mutable state (no coeffect to invalidate) and their only effect is immutable
allocation, whose duplication/dropping the semantics licenses.
NOTES: A side condition TC.Let.Subst ([§18](18-to-cmm-data.md)) silently depends
on: classify_let_binding maps ANY Delay binding with >1 occurrences to
Must_inline_and_duplicate, re-evaluating the defining expression at each use with
no effect/coeffect staging. Witnessed exhaustively — THREE Delay producers: two
arms of flambda_primitive.ml#effects_and_coeffects (Project_function_slot /
Project_value_slot = No_effects; Box_number Heap in classic mode =
Only_generative_effects Immutable), plus Ece.pure_can_be_duplicated minted inside
to_cmm, pure by construction. Box_number Local is deliberately Strict (local
allocations have coeffects, must not move past begin/end region). Placement.join
only demotes Delay→Strict. Enforced nowhere (no assertion); the classic-mode
Box_number arm actively pushes the boundary — and is precisely what makes the
headline INV.ToCmm.Simulates PhysEqual discrepancy possible
([§20](20-to-cmm-soundness.md)). Composes: P.Effects.Placement,
INV.ToCmm.EffectLinear.
```

```rule
RULE P.Effects.Validity
STATUS normative
CODE middle_end/flambda2/terms/validity.ml#t
---
--------------------------------------------------
Can_move_anywhere: no constraint from control flow.
Can't_move_before_any_branch: must not be hoisted above a preceding branch
(e.g. a read whose safety a preceding test established).
Control_flow_point: a genuine branch point that can refine types/shapes (e.g. a
GADT match) or check preconditions; nothing may be moved across it. Produced only
by `effects_and_coeffects.all`, attached in to_cmm to function/external calls,
switches, and effect-handler ops (perform/reperform/resume) — any call may hide
such a point. (Poll, Probe_is_enabled and Invalid are NOT Control_flow_point;
they are Can't_move_before_any_branch — see P.Nullary.ControlBarriers.)
NOTES: join is the max in the order Can_move_anywhere < Can't_move_before_any_branch
< Control_flow_point. Almost every memory primitive uses
Can't_move_before_any_branch, reflecting that the frontend inserts a bounds
test *before* the access; the access must not float above that test.
```

### Named classifications

The code names four combinations and derives the block/array reads and writes
from `Reading`/`Writing` helpers.

```rule
RULE P.Effects.Pure
STATUS normative
CODE middle_end/flambda2/terms/effects_and_coeffects.ml#pure
CODE middle_end/flambda2/terms/effects_and_coeffects.ml#pure_can_be_duplicated
---
--------------------------------------------------
pure                 = (No_effects, No_coeffects, Strict, Can't_move_before_any_branch)
pure_can_be_duplicated = (No_effects, No_coeffects, Delay, Can't_move_before_any_branch)
all                  = (Arbitrary_effects, Has_coeffects, Strict, Control_flow_point)
read                 = (No_effects, Has_coeffects, Strict, Can't_move_before_any_branch)
```

```rule
RULE P.Effects.ReadingFromBlock
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_block
---
μ = Immutable ∨ μ = Immutable_unique  ⟹  Coeffects = No_coeffects
μ = Mutable                            ⟹  Coeffects = Has_coeffects
--------------------------------------------------
reading_from_a_block(μ) = (No_effects, Coeffects, Strict, Can't_move_before_any_branch)
NOTES: This is the crucial CSE distinction. A load from an *immutable* block is
coeffect-free — its result is fixed once the block exists — so repeated loads
can be shared and the load can be reordered freely (subject to Validity). A load
from a *mutable* block has coeffects and cannot be moved past a write.
reading_from_an_array and reading_from_a_string_or_bigstring are defined
identically; a String load is always Immutable, a Bytes/Bigstring load Mutable.
```

```rule
RULE P.Effects.Writing
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_block
---
--------------------------------------------------
writing_to_a_block = (Arbitrary_effects, No_coeffects, Strict, Can't_move_before_any_branch)
NOTES: A write has Arbitrary_effects (it mutates) but No_coeffects (its own
result — unit — does not depend on the heap). Shared by writing_to_an_array and
writing_to_bytes_or_bigstring.
```

```rule
RULE P.Effects.Allocation
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_variadic_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#coeffects_of_mode
---
Alloc mode = Heap   ⟹  Coeffects = No_coeffects
Alloc mode = Local  ⟹  Coeffects = Has_coeffects
--------------------------------------------------
Make_block/Make_array : (Only_generative_effects(μ), Coeffects, Strict, Can't_move_before_any_branch)
NOTES: A local (region) allocation carries coeffects so it cannot be moved
across a Begin_region/End_region boundary — its lifetime is tied to the current
region. A heap allocation is coeffect-free.
```

The float-arithmetic primitives are also classified in this file even though
their denotation is [§05](05-primitives-scalar.md)'s: they are `No_effects` but acquire
`Has_coeffects` when `Flambda_features.float_const_prop ()` is false, so that a
rounding-mode change in intervening C code cannot be reordered past them (see
`effects_and_coeffects_of_unary_primitive`, `Float_arith` case).

---

## Variadic primitives: allocation

### Make_block

```rule
RULE P.Variadic.MakeBlock.Values
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind
CODE middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_block
---
p = Make_block(Values(t, [κ̂₁…κ̂ₙ]), μ, mode)      v̄ = v₁ … vₙ
alloc(Block(t, μ, v̄), H) = (ℓ, H′)
--------------------------------------------------
⟦p⟧(v̄; H) = (ptr ℓ, H′)
NOTES: Allocates a scannable block with scannable tag t, mutability μ and the
given fields in order. n must match the arity of the field-kind list (a
frontend/kinding invariant, [§03](03-kinds.md)); a mismatch is ill-formed, not undef.
```

```rule
RULE P.Variadic.MakeBlock.NakedFloats
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind
---
p = Make_block(Naked_floats, μ, mode)      v̄ = f₁ … fₙ  (naked floats)
alloc(FloatBlock(μ, f̄), H) = (ℓ, H′)
--------------------------------------------------
⟦p⟧(v̄; H) = (ptr ℓ, H′)
NOTES: Runtime representation uses Double_array_tag.
```

```rule
RULE P.Variadic.MakeBlock.Mixed
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind
CODE middle_end/flambda2/terms/flambda_primitive.ml#args_kind_of_variadic_primitive
CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
CODE middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape
VERIFIED 14-validation/mixed-01-record.md
---
prim = Make_block(Mixed(t, σ), μ, mode)      σ = ⟨p, ē⟩,  p + |ē| = n
v̄ = v₁ … vₙ      kind(vᵢ) = field_kinds(σ)(i−1)  for each i  (P.MixedShape.FieldKinds)
alloc(MixedBlock(t, μ, σ, v̄), H) = (ℓ, H′)
--------------------------------------------------
⟦prim⟧(v̄; H) = (ptr ℓ, H′)
NOTES: Allocates a mixed block with scannable tag t, mutability μ, shape σ, and
the n logical fields in order. The argument kinds are given by
args_kind_of_variadic_primitive = Variadic_mixed σ; simplify_primitive.ml expands
this to the per-argument kind list field_kinds(σ) (via List.combine with
Mixed_block_shape.field_kinds) and checks each argument against it. Effects and
coeffects are those of allocation (P.Effects.Allocation), identical to the
Values case: (Only_generative_effects μ, Coeffects, Strict,
Can't_move_before_any_branch) with Coeffects = Has_coeffects iff mode = Local.
A field whose kind disagrees with field_kinds(σ) is ill-formed by kinding
([§03](03-kinds.md), WF.Prim.MakeBlockMixed), not undef.
```

### Make_array

```rule
RULE P.Variadic.MakeArray
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive
CODE middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_array
---
p = Make_array(ak, μ, mode)      v̄ = v₁ … vₙ  (unarized elements conforming to ak)
alloc(Array(ak, μ, v̄), H) = (ℓ, H′)
--------------------------------------------------
⟦p⟧(v̄; H) = (ptr ℓ, H′)
NOTES: n is the unarized element count. For ak = Unboxed_product [κ₁…κₘ] the
logical length is n/m. The empty array (n = 0) is allowed for every kind; there
is no separate empty-array primitive. With the float-array optimisation, a
Values array must never hold floats (that is a Naked_floats array instead) —
an invariant the frontend maintains, not checked here.
```

### Static mixed blocks

A mixed block can also be *statically allocated* as a `Static_const.t`, installed
once by `OS.Let.Static` ([§04](04-opsem.md)) rather than by a `Make_block`
primitive. Static blocks reuse the ordinary `Block` static const, whose shape
field is a `Scannable_block_shape` (`Value_only` or `Mixed_record σ`).

```rule
RULE P.Static.MixedBlock
STATUS normative
CODE middle_end/flambda2/terms/static_const.ml#t
CODE middle_end/flambda2/terms/static_const.ml#block_field_kind
CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#static_const0
VERIFIED 14-validation/mixed-02-static.md
---
sc = Block(t, μ, Mixed_record σ, [s₀ … sₙ₋₁])      n = p + |ē|
kind(sᵢ) = field_kinds(σ)(i)  for each i  (block_field_kind = Scannable_block_shape.element_kind)
μ = Immutable ∨ μ = Immutable_unique
--------------------------------------------------
OS.Let.Static installs MixedBlock(t, μ, σ, [v₀ … vₙ₋₁]) at the bound symbol,
where vᵢ is the value of the Simple.With_debuginfo sᵢ (constant or ρ(x))
NOTES: A mutable static mixed block is REJECTED at to_cmm (to_cmm_static.ml:
"the GC does not currently support mutable fields in statically-allocated
values"), so statically-allocated mixed blocks are effectively immutable. The
emitted Cmm header carries size size_in_words(σ) (physical words, not the logical
field count n) and scannable_prefix_len = p (P.MixedShape.Offset). This grounds
the static case; the machine object is the same MixedBlock as for Make_block.
```

### Region delimiters (augmented judgment)

`Begin_region`/`Begin_try_region` do not fit `⟦p⟧(v̄; H) = (v, H′)` because they
push onto the region stack `R`. We use the augmented form `⟦p⟧(v̄; H, R) = (v,
H′, R′)`; [§04](04-opsem.md)'s machine rules embed these. They are presented here for
completeness and marked `conjectured` pending [§04](04-opsem.md)'s exact `R` structure.

```rule
RULE P.Variadic.BeginRegion
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region
---
ι fresh      R′ = push(ι, R)
--------------------------------------------------
⟦Begin_region⟧(·; H, R) = (region ι, H, R′)
NOTES: Returns a fresh region token naming the newly opened local-allocation
region. Classified (Only_generative_effects Mutable, Has_coeffects, Strict,
Can't_move_before_any_branch): it must not move (Has_coeffects) but may be
deleted if the region is never used (special-cased in Simplify_let_expr). The
optional single argument is a parent/ghost region; ghost regions carry {ghost =
true} and exist only to structure nesting without a runtime effect.
```

```rule
RULE P.Variadic.BeginTryRegion
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive
---
ι fresh      R′ = push(ι, R)
--------------------------------------------------
⟦Begin_try_region⟧(·; H, R) = (region ι, H, R′)
NOTES: As Begin_region, but opens the region used for the body of a
try…with, so that exceptions crossing the handler correctly tear down local
allocations. Same classification as Begin_region.
```

---

## Unary primitives

### Block_load

```rule
RULE P.Unary.BlockLoad
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_mutable_block_load
---
p = Block_load { kind; mut; field = i }
H(ℓ) = Block(t, μ, [v₀ … vₙ₋₁])      0 ≤ i < n
kind and μ consistent with the block (see NOTES)
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (vᵢ, H)
NOTES: The field index i is an immediate carried on the primitive, not a runtime
argument (Block_load is Unary). Reads field i. undef if the argument is not a
pointer to a block, if i is out of range, or if the block's representation does
not match `kind` (e.g. loading a value field from a Naked_floats block). The
`mut` field records the mutability *assumed for this load* and drives the
coeffect (P.Effects.ReadingFromBlock): an Immutable load is CSE-able. For
Naked_floats blocks the loaded value is a naked float; for Mixed blocks see
P.Unary.BlockLoad.Mixed (result kind = field_kinds(σ) at the field index).
Block_load is a genuine projection
and is therefore NOT eligible for CSE (projections propagate through types, not
CSE) — see unary_primitive_eligible_for_cse.
```

```rule
RULE P.Unary.BlockLoad.NakedFloats
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind
---
p = Block_load { kind = Naked_floats _; mut; field = i }
H(ℓ) = FloatBlock(μ, [f₀ … fₙ₋₁])      0 ≤ i < n
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (fᵢ, H)
```

```rule
RULE P.Unary.BlockLoad.Mixed
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind
CODE middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.element_kind_for_load
CODE middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape
VERIFIED 14-validation/mixed-01-record.md
---
p = Block_load { kind = Mixed { shape = σ; field_kind = fk; … }; mut; field = i }
H(ℓ) = MixedBlock(t, μ, σ, [v₀ … vₙ₋₁])      0 ≤ i < n = p + |ē|
fk = from_block_shape(Scannable (Mixed_record σ), i)     (see NOTES)
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (vᵢ, H)
NOTES: Reads logical field i of a mixed block. The field kind fk must be the one
selected by from_block_shape for shape σ and index i: Value_prefix bfk when
i < p and Flat_suffix e_{i−p} when i ≥ p. The loaded value's kind is then
element_kind_for_load(kind) = field_kinds(σ)(i) (P.MixedShape.FieldKinds): a
Value for a prefix field (subkind tagged_immediate if bfk = Immediate, else
any_value) and the naked-number value of e_{i−p} for a suffix field. The mut
field drives the coeffect exactly as for Values blocks
(P.Effects.ReadingFromBlock); like every Block_load it is a projection and NOT
CSE-eligible. undef if the argument is not a pointer to a mixed block of shape σ,
if i is out of range, or if fk disagrees with from_block_shape(σ, i).
```

### Block_set

```rule
RULE P.Binary.BlockSet
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set
---
p = Block_set { kind; init; field = i }
H(ℓ) = Block(t, μ, [v₀ … vₙ₋₁])      0 ≤ i < n
H′ = H[ℓ ↦ Block(t, μ, [v₀ … vᵢ₋₁, v, vᵢ₊₁ … vₙ₋₁])]
--------------------------------------------------
⟦p⟧(ptr ℓ, v; H) = (tagged_imm 0, H′)
NOTES: Returns unit (tagged_imm 0). Field index i is carried on the primitive; the two
runtime arguments are the block and the new value. `init` is Init_or_assign:
Initialization is the initial store during construction (mutable arrays/blocks
built field-by-field, or the initializing writes into an immutable block that
Flambda still treats as a Block_set); Assignment(mode) is a true assignment and
mode drives the write barrier for the GC. undef if not a block pointer, i out of
range, or representation mismatch. There is no operational mutability check: the
store proceeds regardless of μ (`simplify_block_set` ignores both kind and init).
`μ ≠ Immutable` is a frontend-guaranteed invariant for `init = Assignment` stores,
not an operational precondition; Initialization writes into an immutable block are
expected. Storing into a genuinely immutable field via Assignment is a frontend
error, not modelled as undef here.
```

```rule
RULE P.Binary.BlockSet.Mixed
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind
CODE middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set
VERIFIED 14-validation/mixed-03-mutable-set.md
---
p = Block_set { kind = Mixed { shape = σ; field_kind = fk; … }; init; field = i }
H(ℓ) = MixedBlock(t, μ, σ, [v₀ … vₙ₋₁])      0 ≤ i < n = p + |ē|
kind(v) = field_kinds(σ)(i)      fk = from_block_shape(Scannable (Mixed_record σ), i)
H′ = H[ℓ ↦ MixedBlock(t, μ, σ, [v₀ … vᵢ₋₁, v, vᵢ₊₁ … vₙ₋₁])]
--------------------------------------------------
⟦p⟧(ptr ℓ, v; H) = (tagged_imm 0, H′)
NOTES: Writes logical field i of a mixed block, returning unit. The new value v
must have kind field_kinds(σ)(i); fk is selected by from_block_shape as in
P.Unary.BlockLoad.Mixed. init is Init_or_assign exactly as for Values (the
Assignment mode drives the write barrier for a prefix (value) field; suffix
fields hold unboxed scalars and need no barrier). writing_to_a_block classifies
it (Arbitrary_effects, No_coeffects, …). undef if not a mixed-block pointer of
shape σ, i out of range, or fk/kind disagreement. As in P.Binary.BlockSet,
`μ ≠ Immutable` is a frontend invariant for Assignment stores, not an operational
precondition; the store proceeds regardless of μ.
```

### Duplicate_block / Duplicate_array

```rule
RULE P.Unary.DuplicateBlock
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
p = Duplicate_block { kind }
H(ℓ) = o      o a block matching `kind`
alloc(o, H) = (ℓ′, H′)
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (ptr ℓ′, H′)
NOTES: Shallow copy: a fresh block with the same tag, length and field values.
May not change tag or mutability (stated in the mli). Classified
(Only_generative_effects Mutable, Has_coeffects, …): the copy reads the source's
fields (which are assumed possibly-mutable, hence Has_coeffects) so it cannot be
moved past a write to the source.
```

```rule
RULE P.Unary.DuplicateBlock.Mixed
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#Duplicate_block_kind
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
p = Duplicate_block { kind = Mixed }
H(ℓ) = MixedBlock(t, μ, σ, v̄)
alloc(MixedBlock(t, μ, σ, v̄), H) = (ℓ′, H′)
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (ptr ℓ′, H′)
NOTES: Special case of P.Unary.DuplicateBlock for a mixed block. The
Duplicate_block_kind.Mixed constructor carries *no* tag/length/shape payload (the
mli notes the other cases' fields are used only for printing), so the semantics
copies the underlying MixedBlock — tag, shape σ and all logical fields —
wholesale. Same classification as the general rule.
```

```rule
RULE P.Unary.DuplicateArray
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
p = Duplicate_array { kind; source_mutability = μₛ; destination_mutability = μ_d }
H(ℓ) = Array(ak, μ, v̄)      ak matching `kind`
alloc(Array(ak, μ_d, v̄), H) = (ℓ′, H′)
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (ptr ℓ′, H′)
NOTES: Shallow array copy. The destination gets mutability μ_d. Coeffects depend
on μₛ: an Immutable source copy is coeffect-free (its contents are fixed), a
Mutable source copy has coeffects. Effects are Only_generative_effects(μ_d).
```

### Is_int, Is_null, Get_tag, Get_header

```rule
RULE P.Unary.IsInt.Immediate
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int
---
v = tagged_imm n
--------------------------------------------------
⟦Is_int { variant_only }⟧(v; H) = (naked_imm 1, H)
NOTES: Is_int returns a naked immediate 1 (true) on a tagged immediate.
```

```rule
RULE P.Unary.IsInt.Pointer
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int
---
v = ptr ℓ
--------------------------------------------------
⟦Is_int { variant_only }⟧(v; H) = (naked_imm 0, H)
NOTES: Is_int on ANY pointer (block, array, closure, boxed number, …) yields 0.
This is the exact result the task calls out: "Is_int on pointers = 0". The
`variant_only` flag is a *promise from the frontend* that the argument is a
variant value (an immediate constructor or a block), which lets the simplifier
record an is-int *relation* between argument and result (add_is_int_relation)
rather than only constant-folding; it does not change the runtime result. null
is a pointer-shaped value; see Is_null for the null test.
```

```rule
RULE P.Unary.IsNull
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
--------------------------------------------------
⟦Is_null⟧(null; H)    = (naked_imm 1, H)
⟦Is_null⟧(v; H)       = (naked_imm 0, H)    for v ≠ null
NOTES: Tests for the null pointer of an `or_null`-typed value. Distinct from
Is_int: a non-null pointer and a non-null immediate both give 0.
```

```rule
RULE P.Unary.GetTag
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag
---
H(ℓ) = Block(t, μ, v̄)    (or FloatBlock/MixedBlock with runtime tag t)
--------------------------------------------------
⟦Get_tag⟧(ptr ℓ; H) = (naked_imm t, H)
NOTES: Returns the block's tag as a naked immediate. No_effects, No_coeffects
(tags are immutable). Simplify records a get-tag relation (add_get_tag_relation)
rather than constant-folding eagerly. undef on a value whose tag is not readable
this way — in particular Get_header, not Get_tag, must be used to read raw
headers, and the tag of a possibly-lazy value must go through caml_obj_tag
(opaque to the compiler), never Get_tag. The result on an immediate argument is
conjectured undef (Get_tag is a block operation).
```

```rule
RULE P.Unary.GetHeader
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
H(ℓ) = o      o a block with tag ≤ No_scan_tag
--------------------------------------------------
⟦Get_header⟧(ptr ℓ; H) = (naked_nativeint hdr(o), H)
NOTES: Reads the raw block header (encoding tag, size and GC colour bits). undef
on an immediate argument (stated in the mli) and must not be used to read tags
above No_scan_tag. Only the colour bits' "is this locally allocated" information
is reliable. No_effects, No_coeffects. Marked conjectured: the exact header
encoding is target-specific and not modelled precisely.
```

### Length primitives

```rule
RULE P.Unary.ArrayLength
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length
---
p = Array_length akl
H(ℓ) = Array(ak, μ, [v₀ … vₙ₋₁])      ak consistent with akl
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (tagged_imm n, H)
NOTES: Returns the *unarized* length n as a tagged immediate: for an
Unboxed_product array the result is a multiple of the product width. No_effects,
No_coeffects (array length is immutable). Simplify treats it as a projection
against the array's type.
```

```rule
RULE P.Unary.StringLength
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length
---
p = String_length sb
H(ℓ) = Bytes(μ, [b₀ … bₙ₋₁])
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (naked_imm n, H)
NOTES: n is the byte length. No_effects, No_coeffects — even for Bytes, the
*length* is immutable (only the contents mutate). CSE-eligible.
```

```rule
RULE P.Unary.BigarrayLength
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
VERIFIED 14-validation/bigarray_access.md
---
p = Bigarray_length { dimension = d }
H(ℓ) = Bigarray(bk, layout, [d₁ … dₙ], ē)      1 ≤ d ≤ n
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (naked_imm d_d, H)
NOTES: Returns the size of dimension d of the bigarray (1 ≤ d ≤ 3, restricted by
the frontend). Classified reading_from_a_block(Mutable) — Has_coeffects —
because a bigarray's dimensions are read from a mutable descriptor field, unlike
a plain array length. So bigarray length reads are NOT freely CSE-able across
effectful code.
```

### Closure projections

```rule
RULE P.Unary.ProjectFunctionSlot
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot
---
p = Project_function_slot { move_from; move_to }
H(ℓ) = Closures(…)      move_from and move_to are function slots of this set
ptr ℓ_move_to = the closure for move_to within the same set of closures at ℓ
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (ptr ℓ_move_to, H)
NOTES: Given a pointer to *one* closure (identified by move_from) in a set of
closures, returns the pointer to *another* closure (move_to) in the same set. At
runtime this is pointer arithmetic on the shared block; no allocation. No_effects,
No_coeffects, Delay (it is a pure projection, sunk to its use). undef if ℓ is not
a set of closures, or move_from/move_to are not slots of that set.
```

```rule
RULE P.Unary.ProjectValueSlot
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot
---
p = Project_value_slot { project_from; value_slot = w }
H(ℓ) = Closures(…)      project_from a function slot of this set      w a value slot
env(ℓ, project_from)(w) = v
--------------------------------------------------
⟦p⟧(ptr ℓ; H) = (v, H)
NOTES: Reads a captured variable (value slot w) from the closure environment of
the set of closures reached via the closure for project_from. No_effects,
No_coeffects, Delay — the environment of a set of closures is immutable once
built, so value-slot reads are pure and CSE-able and are sunk to their uses. The
result kind is Value_slot.kind w. undef if ℓ is not the right set of closures or
w is not one of its value slots.
```

### Float-array-optimisation predicates

```rule
RULE P.Unary.IsBoxedFloat
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
H(ℓ) = Boxed(Naked_float, _)   ⟹  b = 1
H(ℓ) = o, o not a boxed float  ⟹  b = 0
--------------------------------------------------
⟦Is_boxed_float⟧(ptr ℓ; H) = (naked_imm b, H)
NOTES: Only valid when the float-array optimisation is enabled. Tests the
runtime tag to distinguish a boxed float from other blocks. No_effects,
No_coeffects (tags are immutable). Is_flat_float_array is analogous, testing for
a Double_array_tag (FloatBlock) array. Marked conjectured pending validation.
```

### Regions (unary side)

```rule
RULE P.Unary.EndRegion
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
R = push(ι, R′)
--------------------------------------------------
⟦End_region { ghost }⟧(region ι; H, R) = (tagged_imm 0, H, R′)
NOTES: Closes the region named by its argument, popping it from R and reclaiming
(conceptually deallocating) everything locally allocated in it since the matching
Begin_region. Returns unit. Classified (Arbitrary_effects, Has_coeffects, Strict,
Can't_move_before_any_branch): deliberately NOT Only_generative_effects, so it is
never deleted without regard to prior uses of the region (special-cased in
Simplify_let_expr / Expr_builder). Locally-allocated values must not escape their
region — this is a mode-system invariant enforced by the frontend (see
../types.md); the primitive trusts it and this model does not re-check it. The
augmented judgment threads R; [§04](04-opsem.md) owns the precise region-stack discipline.
```

```rule
RULE P.Unary.EndTryRegion
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
R = push(ι, R′)
--------------------------------------------------
⟦End_try_region { ghost }⟧(region ι; H, R) = (tagged_imm 0, H, R′)
NOTES: Matching delimiter for Begin_try_region. Same classification and escape
invariant as End_region.
```

### Obj_dup, Opaque_identity, Make_lazy

```rule
RULE P.Unary.ObjDup
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
H(ℓ) = o
alloc(o, H) = (ℓ′, H′)
--------------------------------------------------
⟦Obj_dup⟧(ptr ℓ; H) = (ptr ℓ′, H′)
NOTES: Corresponds to Obj.dup: a shallow copy of an arbitrary block. Classified
(Only_generative_effects Mutable, Has_coeffects, …) — Mutable is conservative
since the source's fields may be mutable, and Has_coeffects because it reads
them.
```

```rule
RULE P.Unary.OpaqueIdentity
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
--------------------------------------------------
⟦Opaque_identity { middle_end_only; kind }⟧(v; H) = (v, H)
NOTES: The identity function on values, but an optimisation barrier: classified
(Arbitrary_effects, Has_coeffects, Strict, Can't_move_before_any_branch) so the
optimiser learns nothing about the result from the argument and cannot move or
delete it. `middle_end_only = true` means it is erased before code generation
(a pure middle-end barrier); otherwise it survives to to_cmm.
```

```rule
RULE P.Unary.MakeLazy
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
p = Make_lazy t      t ∈ {Lazy_tag, Forward_tag}
alloc(Lazy(t, v), H) = (ℓ, H′)
--------------------------------------------------
⟦p⟧(v; H) = (ptr ℓ, H′)
NOTES: Allocates a lazy (or forward) block wrapping v. Only_generative_effects
Mutable, No_coeffects (effects_and_coeffects_of_unary_primitive, Make_lazy case).
Marked conjectured pending validation of the lazy/forward tag choice.
```

### Int_as_pointer, Reinterpret_boxed_vector

```rule
RULE P.Unary.IntAsPointer
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
--------------------------------------------------
⟦Int_as_pointer mode⟧(v; H) = (as_pointer(v), H)
NOTES: Reinterprets an integer as a pointer value (unsafe, used for low-level
interop). Effects No_effects; coeffects depend on `mode` (Local ⟹ Has_coeffects,
Heap ⟹ No_coeffects) via coeffects_of_mode, and only the Heap case is CSE-eligible.
Denotation marked conjectured — this is a raw address computation not otherwise
modelled.
```

---

## Binary primitives: reads and comparisons

### Array_load

```rule
RULE P.Binary.ArrayLoad
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load
---
p = Array_load(ak, lk, μ)
H(ℓ) = Array(ak, μ, [v₀ … vₙ₋₁])      i = tagged_imm j      0 ≤ j < n
lk consistent with ak
--------------------------------------------------
⟦p⟧(ptr ℓ, i; H) = (vⱼ, H)
NOTES: Unarized load: the index j and load kind lk are in the *scalar*
(unarized) representation, so an Unboxed_product element occupies several
consecutive scalar indices. Bounds are NOT checked here (see § Unchecked
primitives); j out of range is undef. Coeffect from reading_from_an_array(μ): an
Immutable array load is CSE-able, a Mutable one is not. undef on representation
mismatch.
```

### String / bigstring load

```rule
RULE P.Binary.StringOrBigstringLoad
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
---
p = String_or_bigstring_load(slv, width)      byte width w = byte_width_of_string_accessor_width width
H(ℓ) = Bytes(μ, b̄)  (slv ∈ {String, Bytes})  or  Bigstring(b̄) (slv = Bigstring)
i = naked_nativeint j      0 ≤ j      j + w ≤ length(b̄)
--------------------------------------------------
⟦p⟧(ptr ℓ, i; H) = (decode_width(b̄[j … j+w−1]), H)
NOTES: Reads a w-byte little-endian scalar of the given width from byte offset j.
The index kind is naked_nativeint (string_or_bigstring_index_kind). Coeffects:
String load is Immutable (coeffect-free, CSE-able); Bytes and Bigstring loads are
Mutable (Has_coeffects). Bounds NOT checked here; out-of-range is undef. The
vector widths additionally require alignment for their `aligned` variants;
misalignment is undef here (the frontend inserts an alignment check).
```

### Phys_equal

```rule
RULE P.Binary.PhysEqual
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal
---
p = Phys_equal Eq
--------------------------------------------------
⟦p⟧(v₁, v₂; H) = (naked_imm 1, H)   if v₁ and v₂ are the same machine word (same immediate, or same pointer, or both null)
⟦p⟧(v₁, v₂; H) = (naked_imm 0, H)   otherwise
NOTES: Phys_equal Neq is the boolean negation. Only for values of kind Value
(stated in the mli). It compares the *representation word*, not contents:
- Two tagged immediates are phys-equal iff numerically equal.
- Two pointers are phys-equal iff they point to the same location; two distinct
  boxed floats (or blocks) with equal contents are NOT phys-equal.
- Because floats are boxed at kind Value, `Phys_equal` on boxed floats is
  pointer comparison and can differ from IEEE float equality (and mishandles
  NaN/±0.0) — this is the well-known reason not to use (==) on floats.
No_effects, No_coeffects (pointer identity of already-existing values is stable):
Phys_equal is CSE-eligible.
```

### Bigarray_load, Bigarray_get_alignment

Bigarray accesses reach Flambda with a single flat offset argument; the
multi-dimensional surface form is lowered during `from_lambda`:

```rule
RULE P.Bigarray.Indexing
STATUS descriptive
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#bigarray_indexing
CODE middle_end/flambda2/from_lambda/lambda_to_lambda_transforms.ml#transform_primitive
VERIFIED 14-validation/bigarray_access.md
---
Pbigarrayref/Pbigarrayset with known kind bk and layout, indices i₁ … iₙ
--------------------------------------------------
lowered to Bigarray_load/Bigarray_set(n, bk, layout) applied to the flat offset
  C:       offset = (…((i₁·d₂ + i₂)·d₃ + i₃)…)·dₙ + iₙ
  Fortran: offset = (…(((iₙ−1)·dₙ₋₁ + (iₙ₋₁−1))·dₙ₋₂ + …)…)·d₁ + (i₁−1)
with, unless the access is unsafe, one bounds check per dimension
(0 ≤ iₖ < dₖ for C; 1 ≤ iₖ ≤ dₖ for Fortran) guarding the access
NOTES: The offset arithmetic is ordinary tagged-immediate Mul/Add, and each
dimension size dₖ is a Bigarray_length {dimension = k} read — issued once in
the bounds check and again in the offset computation (see the CR in
bigarray_indexing about the duplicated length access). The loaded/stored value
is boxed/tagged (resp. unboxed/untagged) around the primitive per
element_kind(bk). Accesses whose Lambda kind or layout is *unknown*
(Pbigarray_unknown/_unknown_layout) never become Flambda primitives: an earlier
Lambda-to-Lambda pass rewrites them to caml_ba_get_N/caml_ba_set_N C calls
(caml_ba_float32_get_N/_set_N for float32_t), supported for 1 ≤ N ≤ 3 only.
Descriptive: this documents the from_lambda lowering (context, [§01](01-overview.md)),
not a Flambda term rewrite.
```

```rule
RULE P.Binary.BigarrayLoad
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_bigarray
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_bigarray_load
VERIFIED 14-validation/bigarray_access.md
---
p = Bigarray_load(dims, bk, layout)
H(ℓ) = Bigarray(bk, layout, [d₁ … dₙ], ē)      n = dims
i = tagged_imm j      0 ≤ j < d₁·…·dₙ
bk ∉ {Complex32, Complex64}
--------------------------------------------------
⟦p⟧(ptr ℓ, i; H) = (decode_bk(ē[j]), H)
NOTES: Unchecked single-element read at flat offset j (in element units, layout
order); the frontend has already flattened multi-dimensional indexing and
inserted per-dimension bounds checks (P.Bigarray.Indexing). The offset argument
is a *tagged* immediate (bigarray_index_kind = value), unlike
String_or_bigstring_load's naked_nativeint index. decode_bk yields a naked
number of Bigarray_kind.element_kind(bk) per the taxonomy table: Float16/Float32
widen the stored 16-/32-bit float to naked_float; Sint8/Sint16 sign-extend and
Uint8/Uint16 zero-extend to naked_immediate; the remaining kinds read at their
stored width. For bk ∈ {Complex32, Complex64} the result is instead (ptr ℓ′, H′)
where (ℓ′, H′) = alloc(boxed complex of ē[j], H) — hence
Only_generative_effects Immutable. Effects/coeffects
(reading_from_a_bigarray): (No_effects, Has_coeffects, …) for non-complex
kinds (bigarray storage is always mutable, so never CSE-able), plus the
generative effect for complex kinds. undef on out-of-range j or representation
mismatch. Simplify never const-folds these loads
(simplify_bigarray_load returns unknown at element_kind(bk)).
```

```rule
RULE P.Binary.BigarrayGetAlignment
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
CODE backend/cmm_helpers.ml#bigstring_get_alignment
---
p = Bigarray_get_alignment n      n a power of two
H(ℓ) = Bigstring(b̄)      i = naked_imm j
--------------------------------------------------
⟦p⟧(ptr ℓ, i; H) = (naked_imm ((data_ptr(ℓ) + j) land (n − 1)), H)
NOTES: Returns the byte address of the bigstring's data pointer plus offset,
modulo n (as a bit-mask; the lowering is (n−1) land (data + j)); zero means the
access is n-byte aligned. Used by the frontend to guard aligned vector
bigstring accesses (lambda_to_flambda_primitives.ml
#bigstring_alignment_validity_condition); likewise, safe bigstring loads/sets
bound-check against Bigarray_length {dimension = 1} — a bigstring is a 1-d
Char bigarray. No_effects, No_coeffects, and CSE-eligible (the data pointer of
a given bigstring never changes).
```

### Atomic_load_field, Poke, Read_offset

```rule
RULE P.Binary.AtomicLoadField
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
---
p = Atomic_load_field fk      H(ℓ) = Block(t, μ, v̄)      i = tagged_imm j
--------------------------------------------------
⟦p⟧(ptr ℓ, i; H) = (vⱼ, H)   (with acquire semantics)
NOTES: Atomically reads field j of a block used as an atomic cell. Classified
(Arbitrary_effects, Has_coeffects, …) — the strongest classification — so it is
never reordered or removed, capturing the memory-ordering guarantees the model
does not otherwise represent. fk ∈ {Any_value, Immediate}. Descriptive: this
model has no concurrency, so the "atomic"/ordering content is prose only.
```

```rule
RULE P.Binary.Poke
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
---
p = Poke sif      addr a naked pointer      v a scalar of kind sif
H′ = H with the sif-scalar at address a set to v
--------------------------------------------------
⟦p⟧(a, v; H) = (tagged_imm 0, H′)
NOTES: Unsafe raw store of a scalar at a machine address. Returns unit.
Classified (Arbitrary_effects, No_coeffects, Strict, Can't_move_before_any_branch);
"prevent Poke from being moved" — it is pinned. Peek (unary, below) is the read
counterpart. undef unless a is a valid writable address of the right size and
alignment; this model does not track raw addresses, so treated descriptively.
```

```rule
RULE P.Binary.ReadOffset
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
---
p = Read_offset(κ̂, mut)      base b      byte offset δ
--------------------------------------------------
⟦p⟧(b, δ; H) = (value of kind κ̂ at (b + δ), H)
NOTES: Raw read of a κ̂-value at (base + byte_offset). Coeffects from mut
(Immutable ⟹ No_coeffects, Mutable ⟹ Has_coeffects); No_effects. Counterpart of
the ternary Write_offset. Conjectured/unsafe: raw-memory addressing is not
modelled.
```

The unary `Peek` is the read dual of `Poke`:

```rule
RULE P.Unary.Peek
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
---
p = Peek sif      addr a naked pointer
--------------------------------------------------
⟦p⟧(a; H) = (sif-scalar read from address a, H)
NOTES: Unsafe raw load. Classified (Arbitrary_effects, Has_coeffects, …) — pinned
"for the moment" — so it never moves. undef unless a is a valid readable address;
treated descriptively as raw addressing is unmodelled.
```

---

## Ternary primitives: writes

```rule
RULE P.Ternary.ArraySet
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_array_set
---
p = Array_set(ak, sk)
H(ℓ) = Array(ak, Mutable, [v₀ … vₙ₋₁])      i = tagged_imm j      0 ≤ j < n
H′ = H[ℓ ↦ Array(ak, Mutable, [v₀ … vⱼ₋₁, v, vⱼ₊₁ … vₙ₋₁])]
--------------------------------------------------
⟦p⟧(ptr ℓ, i, v; H) = (tagged_imm 0, H′)
NOTES: Unarized store into a mutable array. Returns unit. The set kind sk fixes
the element representation; its Values case carries Init_or_assign whose
Assignment mode drives the GC write barrier. Bounds NOT checked here (§ Unchecked
primitives); j out of range is undef, as is storing into an immutable array or a
representation mismatch. writing_to_an_array: (Arbitrary_effects, No_coeffects, …).
```

```rule
RULE P.Ternary.BytesOrBigstringSet
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bytes_or_bigstring_set
---
p = Bytes_or_bigstring_set(blv, width)      w = byte_width_of_string_accessor_width width
H(ℓ) = Bytes(Mutable, b̄) (blv = Bytes)  or  Bigstring(b̄) (blv = Bigstring)
i = naked_nativeint j      0 ≤ j      j + w ≤ length(b̄)
H′ = H with bytes b̄[j … j+w−1] set to the w-byte little-endian encoding of v
--------------------------------------------------
⟦p⟧(ptr ℓ, i, v; H) = (tagged_imm 0, H′)
NOTES: Writes a w-byte scalar at byte offset j. Returns unit. Index kind
naked_nativeint. Bounds NOT checked here; out-of-range or (for aligned vector
widths) misaligned is undef. writing_to_bytes_or_bigstring: (Arbitrary_effects,
No_coeffects, …).
```

```rule
RULE P.Ternary.BigarraySet
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_bigarray
CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bigarray_set
VERIFIED 14-validation/bigarray_access.md
---
p = Bigarray_set(dims, bk, layout)
H(ℓ) = Bigarray(bk, layout, [d₁ … dₙ], ē)      n = dims
i = tagged_imm j      0 ≤ j < d₁·…·dₙ
v of kind element_kind(bk)
--------------------------------------------------
⟦p⟧(ptr ℓ, i, v; H) = (tagged_imm 0, H[ℓ ↦ Bigarray(bk, layout, d̄, ē[j ↦ encode_bk(v)])])
NOTES: Write dual of P.Binary.BigarrayLoad; the frontend has flattened the
index and inserted bounds checks (P.Bigarray.Indexing). encode_bk narrows to
the storage width of the taxonomy table: Float16/Float32 round the naked_float
to 16-/32-bit precision, Sint8/Uint8/Sint16/Uint16 truncate to the low bits;
for bk ∈ {Complex32, Complex64}, v is a pointer to a boxed complex and both
parts are stored (reading its fields is of immutable data, so it adds no
observable coeffect). (Arbitrary_effects, No_coeffects, …) for every kind.
undef on out-of-range j or representation mismatch. Simplify treats the result
as opaque unit (simplify_bigarray_set).
```

### Atomic read-modify-write

```rule
RULE P.Ternary.AtomicSetField
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
---
p = Atomic_set_field fk      H(ℓ) = Block(t, μ, v̄)      i = tagged_imm j
H′ = H[ℓ.j ↦ v]  (with release semantics)
--------------------------------------------------
⟦p⟧(ptr ℓ, i, v; H) = (tagged_imm 0, H′)
NOTES: Atomic store to field j. (Arbitrary_effects, Has_coeffects, …).
Atomic_exchange_field is analogous but returns the *previous* value:
⟦Atomic_exchange_field fk⟧(ptr ℓ, i, v; H) = (vⱼ, H[ℓ.j ↦ v]).
Atomic_field_int_arith op (Fetch_add/Add/Sub/And/Or/Xor) reads field j, writes
(vⱼ `op` v), and returns the old value for Fetch_add / unit for the others.
Descriptive: no concurrency in this model.
```

```rule
RULE P.Ternary.WriteOffset
STATUS conjectured
CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
---
p = Write_offset(wk, κ̂, mode)      base b      byte offset δ      payload v
H′ = H with the κ̂-value at (b + δ) set to v
--------------------------------------------------
⟦p⟧(b, δ, v; H) = (tagged_imm 0, H′)
NOTES: Raw store of a κ̂ payload at (base + byte_offset). wk = Into_block requires
b to be a block pointer; Into_block_or_off_heap additionally allows b = NULL, in
which case δ is itself a non-OCaml-heap pointer written via a raw store. mode is
the assignment alloc mode (write barrier). writing_to_a_block classification.
Conjectured/unsafe: raw addressing unmodelled.
```

---

## Quaternary primitives: atomic compare-and-set

```rule
RULE P.Quaternary.AtomicCompareAndSetField
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#quaternary_primitive
---
p = Atomic_compare_and_set_field fk
H(ℓ) = Block(t, μ, v̄)      i = tagged_imm j      vⱼ = v_old  ⟹  H′ = H[ℓ.j ↦ v_new], b = 1
                                            vⱼ ≠ v_old  ⟹  H′ = H,             b = 0
--------------------------------------------------
⟦p⟧(ptr ℓ, i, v_old, v_new; H) = (tagged_imm b, H′)
NOTES: Arguments are (block, field-index, expected, desired). Atomically sets
field j to v_new iff its current value is physically equal to v_old, returning a
boolean success flag. (Arbitrary_effects, Has_coeffects, …).
Atomic_compare_exchange_field is the variant returning the *previous* value
rather than a boolean; it carries both the atomic's field kind and the field kind
in use on this occasion (which may be Immediate even for an Any_value atomic).
Descriptive: no concurrency in this model.
```

---

## Nullary primitives

```rule
RULE P.Nullary.StateAccessors
STATUS descriptive
CODE middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive
---
--------------------------------------------------
⟦Dls_get⟧(·; H)      = (ptr (domain-local state block), H)
⟦Tls_get⟧(·; H)      = (ptr (thread-local state block), H)
⟦Domain_index⟧(·; H) = (naked_imm (current domain index), H)
NOTES: All three are (No_effects, Has_coeffects, Strict, Can't_move_before_any_branch):
they read domain/thread context, so they may not be moved across code that could
change that context, but they do not themselves change the world. Descriptive
(the runtime context is not part of this model's state).
```

```rule
RULE P.Nullary.ControlBarriers
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive
---
--------------------------------------------------
Poll, Cpu_relax                              : (Arbitrary_effects, Has_coeffects, Strict, Can't_move_before_any_branch)
Probe_is_enabled _, Enter_inlined_apply _    : (Arbitrary_effects, Has_coeffects, Strict, Can't_move_before_any_branch)
Invalid _                                    : (Arbitrary_effects, Has_coeffects, Strict, Can't_move_before_any_branch)
Optimised_out _                              : (No_effects, No_coeffects, Strict, Can't_move_before_any_branch)
NOTES: Poll may run pending runtime actions (signals, finalizers, GC) so it must
neither move nor vanish. Probe_is_enabled and Enter_inlined_apply have no real
runtime effect but are given Arbitrary_effects precisely so the optimiser leaves
them in place. Invalid marks an unreachable/ill-formed primitive; Optimised_out
is a phantom-only placeholder with no effect. These have no data denotation
(Poll returns unit; Invalid has none — reaching it is undef).
```

---

## Unchecked primitives

Every array/string/bytes/bigstring/bigarray access primitive above is
**unchecked**: it assumes its index is in range (and, for aligned vector
widths, that the address is aligned), and its behaviour outside that contract is
`undef`. Safety is provided by the *frontend*, which wraps each safe access in an
explicit test that raises before the primitive runs.

```rule
RULE P.Unchecked.FrontendInsertsChecks
STATUS normative
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_array_access
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_bound
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#checked_access
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives_helpers.ml#bind_recs
---
A safe access `a.(i)` lowers to:  if  0 ≤_u i <_u length(a)  then  ⟨unchecked access⟩  else  raise Invalid_argument
An unsafe access `a.!(i)` lowers to:  ⟨unchecked access⟩  directly
--------------------------------------------------
The Flambda 2 access primitives are the ⟨unchecked access⟩; the bounds test is a
separate Int_comp (Yielding_bool (Lt Unsigned)) against Array_length / String_length
/ a bigarray dimension, compiled to a Switch by bind_recs.
NOTES: Concretely, check_bound emits `Binary (Int_comp (_, Yielding_bool (Lt Unsigned)),
index, bound)` and checked_access builds `Checked { primitive; validity_conditions;
failure = Index_out_of_bounds; dbg }`, lowered to a Switch (true → primitive, false →
raise) with the failure branch marked cold. Safety is selected per Lambda primitive:
the `…s` variants (Parrayrefs, Parraysets, …) and non-`unsafe` string/bytes accesses
get the wrapper; the `…u` variants (Parrayrefu, …) and `unsafe`-flagged accesses do
not. Division/modulo get the analogous check_zero_division wrapper
(failure = Division_by_zero). Consequently the primitive semantics above are the
"already checked" behaviour, and the compiler may assume indices are in range.
```

---

## Rule index for this chapter

Effects/coeffects: `P.Effects.Classification`, `P.Effects.NoEffects`,
`P.Effects.OnlyGenerative`, `P.Effects.Arbitrary`, `P.Effects.Coeffects`,
`P.Effects.Placement`, `P.Effects.Validity`, `P.Effects.Pure`,
`P.Effects.ReadingFromBlock`, `P.Effects.Writing`, `P.Effects.Allocation`.

Mixed-block shape: `P.MixedShape.FieldKinds`, `P.MixedShape.Offset`.

Variadic: `P.Variadic.MakeBlock.Values`, `P.Variadic.MakeBlock.NakedFloats`,
`P.Variadic.MakeBlock.Mixed`, `P.Variadic.MakeArray`, `P.Static.MixedBlock`,
`P.Variadic.BeginRegion`, `P.Variadic.BeginTryRegion`.

Unary: `P.Unary.BlockLoad`, `P.Unary.BlockLoad.NakedFloats`,
`P.Unary.BlockLoad.Mixed`, `P.Unary.DuplicateBlock`,
`P.Unary.DuplicateBlock.Mixed`, `P.Unary.DuplicateArray`,
`P.Unary.IsInt.Immediate`,
`P.Unary.IsInt.Pointer`, `P.Unary.IsNull`, `P.Unary.GetTag`, `P.Unary.GetHeader`,
`P.Unary.ArrayLength`, `P.Unary.StringLength`, `P.Unary.BigarrayLength`,
`P.Unary.ProjectFunctionSlot`, `P.Unary.ProjectValueSlot`, `P.Unary.IsBoxedFloat`,
`P.Unary.EndRegion`, `P.Unary.EndTryRegion`, `P.Unary.ObjDup`,
`P.Unary.OpaqueIdentity`, `P.Unary.MakeLazy`, `P.Unary.IntAsPointer`,
`P.Unary.Peek`.

Binary: `P.Binary.BlockSet`, `P.Binary.BlockSet.Mixed`, `P.Binary.ArrayLoad`,
`P.Binary.StringOrBigstringLoad`, `P.Binary.PhysEqual`, `P.Binary.BigarrayLoad`,
`P.Binary.BigarrayGetAlignment`, `P.Binary.AtomicLoadField`, `P.Binary.Poke`,
`P.Binary.ReadOffset`.

Ternary: `P.Ternary.ArraySet`, `P.Ternary.BytesOrBigstringSet`,
`P.Ternary.BigarraySet`, `P.Ternary.AtomicSetField`, `P.Ternary.WriteOffset`.

Quaternary: `P.Quaternary.AtomicCompareAndSetField`.

Nullary: `P.Nullary.StateAccessors`, `P.Nullary.ControlBarriers`.

Unchecked: `P.Unchecked.FrontendInsertsChecks`.

Bigarray lowering: `P.Bigarray.Indexing`.

Note that `Block_set` is a Binary primitive (block + value; the field index is
an immediate on the primitive), so its rule lives under `P.Binary.*`.
