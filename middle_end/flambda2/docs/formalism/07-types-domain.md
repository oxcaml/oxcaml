# The abstract domain: types, environments, concretization

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter describes the **types** abstract domain: the static-semantics
side of Flambda 2. Where [§04](04-opsem.md) gives an operational semantics over runtime
values `v` (the set `Val`, relative to a heap `H`), this chapter gives the
lattice of *approximations* of those values that Simplify propagates. A Flambda
type `T` denotes a set of runtime values; a typing environment `E` denotes a set
of concrete environments; and *concretization* `γ_E(T) ⊆ Val` ties the two
together.

Meet, join, provers, and reification live in [§08](08-meet-join.md) and are only named here.
This chapter owns the grammar, the environment structure, and concretization.

## 1. Role

Flambda types are an abstract-interpretation domain, not a type system in the
usual sense (`types.md` opens by making exactly this disclaimer). Two
consequences shape everything below.

First, the analysis runs in a **single Simplify pass**. It never revisits a
program point, so there is no fixpoint iteration and hence no widening. Loops
are handled by joining the entry state with a conservative approximation once,
not by iterating to convergence. This is why the domain has no widening operator
and why infinite ascending/descending chains are avoided by construction rather
than cut off by widening (`types.md`, "Introduction").

Second, types are **value approximations relative to a heap**. A type is a
sound over-approximation: `γ_E(T)` contains *at least* every value that the
corresponding runtime name can hold at that program point. `Unknown` (top) means
"any value of this kind"; `Bottom` means "no value — this point is unreachable".
Analysis facts only ever shrink the set below top, and every fact is an
over-approximation, so it is always safe to discard a type and replace it with
`Unknown` of the same kind.

```rule
RULE T.Role.SinglePass
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.mli#t
CAVEAT disclosure: no single CODE anchor proves widening-absence; it is a structural property of the code; soundness precision owned by 13-soundness.md.
---
Simplify performs one pass over each function body. The types domain provides
no widening operator; meet and join ([§08](08-meet-join.md)) are the only combinators, and
join is applied once at each control-flow merge rather than iterated.
NOTES: Verified against types.md ("we forbid ourselves from going over the
same piece of code more than once ... we will not have to bother with widening
operations"). The absence of a widening operator is a structural property of
the code, not something a single anchor proves; [§13](13-soundness.md) owns the soundness
statement that makes "over-approximation" precise.
```

## 2. Type grammar

Every type has a **kind** ([§03](03-kinds.md)). At the top level the kind is manifest in
the constructor of `Type_grammar.t`: `Value`, `Naked_immediate`, the naked
number kinds (`Naked_float`, `Naked_int8/16/32/64`, `Naked_nativeint`, three
vector widths), plus `Rec_info` and `Region`
(`type_grammar.mli#t`).

Underneath each kind constructor sits a `Type_descr.t`, which is the single most
important structural idea in the domain: **a type of any kind is either a
top/bottom marker, a "head" describing structure, or an alias to a `Simple`.**

```
T           ::= Value      td_value
              | Naked_immediate  td_imm
              | Naked_float  td_float   | Naked_float32 td_f32
              | Naked_int8   td_i8      | Naked_int16   td_i16
              | Naked_int32  td_i32     | Naked_int64   td_i64
              | Naked_nativeint td_nat
              | Naked_vec128 td_v128    | Naked_vec256  td_v256
              | Naked_vec512 td_v512
              | Rec_info     td_recinfo
              | Region       td_region

td_κ (a Type_descr.t over head_κ)
            ::= Unknown                      -- top of kind κ
              | Bottom                       -- bottom of kind κ
              | Ok (No_alias head_κ)         -- structural "head"
              | Ok (Equals s)                -- alias / equation: "= s"
```

`Type_descr.descr` exposes exactly this three-way split as
`head Or_unknown_or_bottom.t` where the `Ok` payload is `No_alias head | Equals
Simple.t` (`type_descr.mli#Descr.t`, `type_descr.mli#descr`). An `Equals s`
type is the domain's *relational* element: it says "this value is equal (up to
coercion) to the value of `s`" rather than describing structure directly. This
is what `types.md` calls a "type-lifted abstract element or an equality".

```rule
RULE T.Grammar.TypeDescr
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_descr.mli#Descr.t
CODE middle_end/flambda2/types/grammar/type_descr.mli#descr
---
For each kind κ, a type is Unknown, Bottom, a head (No_alias), or an alias
(Equals s) to a Simple. Unknown is the top element ("any value can flow here"),
Bottom the least element ("no value can flow here").
```

### 2.1 Value heads: nullability × non-null part

The head for kind `Value` is *not* a bare structural description; it is a
product of a nullability flag and an `Or_unknown_or_bottom` non-null structure
(`type_grammar.mli#head_of_kind_value`):

```
head_value        ::= { non_null : head_value_non_null Or_unknown_or_bottom.t;
                        is_null   : is_null }
is_null           ::= Not_null
                    | Maybe_null { is_null : Variable.t option }
```

The two `Or_unknown_or_bottom` layers are deliberately redundant with the one in
`Type_descr`: they let the head express "unknown but definitely not null",
"bottom but maybe null" (which is precisely the type of the `Null` constructor),
and so on (comment at `type_grammar.ml#head_of_kind_value`). The optional
`Variable.t` in `Maybe_null` is the naked-immediate variable that holds the
`is_null` test of this value (see §2.4).

```
head_value_non_null ::=
    Variant { is_int      : Variable.t option;
              immediates  : T Or_unknown.t;
              get_tag     : Variable.t option;
              blocks      : row_like_for_blocks Or_unknown.t;
              extensions  : variant_extensions;
              is_unique   : bool }
  | Mutable_block { alloc_mode }
  | Boxed_float32 T alloc_mode  | Boxed_float T alloc_mode
  | Boxed_int32 T alloc_mode    | Boxed_int64 T alloc_mode
  | Boxed_nativeint T alloc_mode
  | Boxed_vec128/256/512 T alloc_mode
  | Closures { by_function_slot : row_like_for_closures; alloc_mode }
  | String of String_info.Set.t
  | Array { element_kind : κ̂ Or_unknown_or_bottom.t;
            length : T; contents : array_contents Or_unknown.t; alloc_mode }
```

(`type_grammar.mli#head_of_kind_value_non_null`.)

### 2.2 Variants

A `Variant` is a disjunction of an immediate part and a block part
(`types.md`, "Variants"): `immediates` is the type of the tagged-integer arm
(kind `Naked_immediate`, or `Unknown`), and `blocks` is a `row_like_for_blocks`
describing the pointer arm (or `Unknown`). Either arm may be absent by being
`Bottom`; the whole variant is bottom when both are.

The `is_int` and `get_tag` fields hold the *forward* relational links: the
variable (of kind `Naked_immediate`) that is known to equal `%is_int` and
`%get_tag` of this value respectively. The `is_unique` bit tracks whether the
value came from a `[@unique]`/unboxable allocation. `extensions` is discussed in
§5.

```rule
RULE T.Grammar.Variant
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
CODE middle_end/flambda2/types/grammar/type_grammar.ml#create_variant
---
A Variant head is the disjoint union of an immediates arm (a Naked_immediate
type or Unknown) and a blocks arm (a row_like_for_blocks or Unknown), each of
which may independently be Bottom. Its is_int/get_tag fields optionally name the
Naked_immediate variables equal to %is_int and %get_tag of the value.
```

### 2.3 Row_like: tag/slot-indexed disjunctions of products

Blocks and closures are both described by the **Row_like** structure: an indexed
disjunction of cases, where each case is a product ([§08](08-meet-join.md) owns the lattice
operations; here we give the shape). The key subtlety is that indices may be
*partially known*:

```
row_like_index_domain 'lat ::= Known 'lat        -- exactly this index
                             | At_least 'lat      -- this index or any extension
row_like_index  ::= { domain : 'lat row_like_index_domain; shape : 'shape }
row_like_case   ::= { maps_to : 'maps_to;
                      index   : row_like_index;
                      env_extension : env_extension }
```

For **blocks** (`type_grammar.mli#row_like_for_blocks`):

```
row_like_for_blocks ::=
    { known_tags : row_like_block_case Or_unknown.t Tag.Map.t;
      other_tags : row_like_block_case Or_bottom.t;
      alloc_mode }
row_like_block_case = row_like_case with
    'lat = Block_size.t, 'shape = Block_shape.t, 'maps_to = T array
```

So a block case is indexed by a tag (map key) together with a `Block_size`
lattice element and a `Block_shape`, and maps to an array of field types. The
`At_least` index domain is what represents "a block of this tag with *at least*
this many fields", used when a field read reveals a lower bound on the size but
not the exact size (`types.md`, "Row_like, disjunction over products"). The
`other_tags` case (`Or_bottom`) covers tags not present as map keys; in practice
it is used for open block types.

For **closures** (`type_grammar.mli#row_like_for_closures`):

```
row_like_for_closures ::=
    { known_closures : row_like_case Function_slot.Map.t;
      other_closures : row_like_case Or_bottom.t }
    with 'lat = Set_of_closures_contents.t, 'shape = unit,
         'maps_to = closures_entry
closures_entry ::=
    { function_types   : function_type Or_unknown.t Function_slot.Map.t;
      closure_types    : function_slot_indexed_product;
      value_slot_types : value_slot_indexed_product }
function_type ::= { code_id : Code_id.t; rec_info : T }
```

The index is a function slot plus the `Set_of_closures_contents` (the set of
function slots and value slots present in the closure). A `closures_entry` maps
each function slot to a `function_type` (a code id and a `rec_info` type — see
§2.6) and each value/function slot to a component type. `other_closures` is
documented in the code as always `Bottom` and slated for removal
(`type_grammar.ml#row_like_for_closures`).

```rule
RULE T.Grammar.RowLike.Index
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_index_domain
CODE middle_end/flambda2/types/grammar/type_grammar.ml#row_like_index_domain
CODE middle_end/flambda2/types/grammar/type_grammar.ml#check_field_tys
CODE middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape
---
A row-like index is Known x (the singleton {x}) or At_least x (every index y
with x ⊆ y). Block cases are indexed by (Tag, Block_size, Block_shape) and map
to a field-type array; closure cases are indexed by (Function_slot,
Set_of_closures_contents) and map to a closures_entry. Each case additionally
carries an env_extension (§5).
NOTES: The Block_shape ([§03](03-kinds.md)) on a block index is Scannable Value_only,
Scannable (Mixed_record σ), or Float_record. It fixes the kind of each field
type in the maps_to array: check_field_tys requires field i's type to have kind
Block_shape.element_kind(shape, i) — Value for a Value_only field or a mixed
value-prefix field, and the naked-number kind of the corresponding
flat_suffix_element for a mixed flat-suffix field (so a mixed block's suffix
fields carry naked-number types, not Value types), and Naked_float for a
Float_record field. unknown_from_shape(shape, i) builds the top type of that
per-field kind, used to fill fields whose type is otherwise Unknown.
```

Only the *contents* of a Row_like case (field types, slot component types) are
themselves `T`; the indices (tags, sizes, slots) are plain data, never types.
This is the "type-lifted abstract element" discipline of `types.md`
("Flambda types"): sub-elements are replaced by types exactly where a relation
is worth tracking.

### 2.4 Naked immediates: sets and relational forms

The head for kind `Naked_immediate` is where the interesting *relational*
structure of the numeric side lives
(`type_grammar.ml#head_of_kind_naked_immediate`):

```
head_naked_immediate ::=
    Naked_immediates of Target_ocaml_int.Set.t          -- a finite set of values
  | Inverse_relations of (Name.Set.t) Relation.Map.t    -- purely relational
  | Naked_immediates_and_inverse_relations
      { naked_immediates : Target_ocaml_int.Set.t;
        inverse_relations : (Name.Set.t) Relation.Map.t }
Relation ::= Is_null | Is_int | Get_tag
```

The unified view (`type_grammar.mli#Head_of_kind_naked_immediate.descr`) is a
pair `{ naked_immediates : … Or_unknown.t; inverse_relations : Name.Set.t
Relation.Map.t }`. The set component is an ordinary non-relational abstraction
(a finite set of concrete immediates, or `Unknown` for top). The
`inverse_relations` component is the relational part: an entry `Is_int ↦ {x}`
means "this immediate value equals `%is_int x`", i.e. it is `1` iff `x` is a
tagged immediate; `Get_tag ↦ {x}` means it equals the tag of block `x`;
`Is_null ↦ {x}` means it equals `%is_null x` (`type_grammar.ml#Relation.descr`,
`type_grammar.ml#is_int_for_scrutinee`, `#get_tag_for_block`, `#is_null`).

This is the *inverse* direction of the `is_int`/`get_tag` fields on the
`Variant` head: from the immediate we recover the block/scrutinee it constrains.
`types.md` ("Relational domains and reduction") describes storing the relation
"one way only"; the current grammar stores both — the forward link on the
variant head *and* the inverse map here — so a note is warranted.

```rule
RULE T.Grammar.NakedImmediate.Relational
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate
CODE middle_end/flambda2/types/grammar/type_grammar.ml#is_int_for_scrutinee
CODE middle_end/flambda2/types/grammar/type_grammar.ml#get_tag_for_block
VERIFIED 14-validation/issue5721.md @ 1c1940b7ea
VERIFIED 14-validation/naked_immediates_many_relations.md @ 1c1940b7ea
CAVEAT disclosure: is_int_for_scrutinee (construction, `= true`) disagrees with Relation.of_const (meet reduction, `= false`) for a null constant; only is_int is anomalous.
CAVEAT disclosure: code diverges from types.md's 'one way only' design — the is_int/get_tag relation is stored at both ends (Variant forward fields plus inverse_relations map).
---
A Naked_immediate type is a finite set of immediate values (or Unknown) together
with a map from relations (Is_null, Is_int, Get_tag) to sets of Names. An entry
R ↦ N asserts that this immediate value equals R applied to each name in N
(%is_int / %get_tag / %is_null). Constants short-circuit differently at
construction versus meet-time reduction. The construction helpers special-case a
constant scrutinee: is_null builds `= RWC.is_null const`, is_int_for_scrutinee
builds `= true` UNCONDITIONALLY (even for a null constant), and get_tag_for_block
builds Unknown (NOT Bottom — deliberately loose, since get_tag relations may be
created on things not known to be blocks). Separately, the meet-time reducer
(reduce_head_of_kind_naked_immediate, [§08](08-meet-join.md)) resolves a relation
whose name is a constant via Relation.of_const (Is_int ↦ not is_null, Is_null ↦
is_null, Get_tag ↦ Bottom); a Bottom result is DELIBERATELY DISCARDED (the
relation is dropped, the immediate set kept) rather than making the type Bottom,
because inverse relations such as get_tag may have been recorded for non-blocks.
NOTES: is_int_for_scrutinee (construction, `= true`) and Relation.of_const (meet
reduction, `= false` for null) disagree for a null constant; only is_int is thus
anomalous, is_null agreeing with of_const by construction. types.md ("Relational
domains and reduction") describes storing this relation in one direction only.
The current code additionally stores a forward link (the is_int/get_tag
Variable.t option fields on the Variant head), so the relation is now represented
at both ends. Conjectured that the two are kept consistent by the meet-time
reduction ([§08](08-meet-join.md)).
```

The other naked-number heads (`head_of_kind_naked_float`, `…_int8/16/32/64`,
`…_nativeint`, `…_vec128/256/512`) are simply non-empty finite sets of the
corresponding constant type (`type_grammar.mli`, lines around
`head_of_kind_naked_float`). Emptiness is not represented as an empty set but as
the overall `Bottom` type (invariant stated at `type_grammar.mli`, comment above
`head_of_kind_naked_float32`). These domains have no relational or interval
form; `types.md` ("Numerical domains") notes intervals/congruences would fit but
are not implemented.

```rule
RULE T.Grammar.NakedNumber.NonEmptySet
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_float
---
For each naked number kind other than Naked_immediate, a head is a finite set of
constants of that kind, maintained non-empty; the empty set is represented by
the overall Bottom type rather than an empty head. There is no interval or
relational form.
```

### 2.5 Boxed numbers, arrays, strings

`Boxed_*` heads wrap a single type of the corresponding naked kind plus an
allocation mode. `Array` heads carry an element kind
(`κ̂ Or_unknown_or_bottom.t`), a `length` type (kind `Naked_immediate`... in
practice a tagged-immediate length), optional `contents`
(`Immutable { fields } | Mutable`), and an alloc mode. `String` heads are a set
of `String_info.t` (length and, for known constants, contents).
`Mutable_block` carries only an alloc mode — its contents are not tracked
because they can change. These are the "small amount of support for arrays and
immutable strings" mentioned in `types.md` (combined domain paragraph); the
formalism does not model them deeply.

### 2.6 Rec_info and Region

`head_of_kind_rec_info = Rec_info_expr.t` carries inlining-depth information
(unrolling/rec depth) used by the inliner; [§11](11-inlining.md) owns its meaning here it is
an opaque payload. `head_of_kind_region = unit`: the `Region` kind has exactly
one non-top/bottom inhabitant, so its type lattice is trivial
(`type_grammar.ml#head_of_kind_region`).

```rule
RULE T.Grammar.RecInfoRegion.Trivial
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_region
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_rec_info
---
head_of_kind_region carries no information (unit): the Region lattice is
{Bottom, Unknown} plus aliases. head_of_kind_rec_info is a Rec_info_expr.t whose
interpretation belongs to inlining ([§11](11-inlining.md)); the types domain treats it opaquely.
```

## 3. Typing environment E

A typing environment (`typing_env.ml#t`) is conceptually a *reduced product* of
several domains (`types.md`, "The combined domain"): a non-relational map from
names to types, plus the relational Aliases, projection, `Get_tag` and `Is_int`
domains. In the implementation the relational parts are largely folded into the
name-to-type map (an `Equals` type *is* the alias equation; a relational
naked-immediate type *is* the `Is_int`/`Get_tag` equation), so the concrete
record is smaller than that product suggests:

```
E ≈ { machine_width;
      resolver;                 -- fetches imported (.cmx) environments
      binding_time_resolver;
      defined_symbols : Symbol.Set.t;
      code_age_relation;        -- code-id specialisation order ([§08](08-meet-join.md))
      prev_levels; current_level;  -- the level stack (§3.3)
      next_binding_time; min_binding_time;
      is_bottom : bool }
```

The name→type map with binding times lives in the per-level `Cached_level`
(`cached_level.mli#names_to_types` returns `(Type_grammar.t *
Binding_time.With_name_mode.t) Name.Map.t`), and the alias equivalence relation
lives alongside it (`cached_level.mli#aliases`).

### 3.1 Binding times, canonical elements, aliases

Every name has a **binding time** (`binding_time.ml`). The reserved times order
the name universe: `consts = 0 < symbols = 1 < imported_variables = 2 <
earliest_var = 3 < …`, with locally-defined variables assigned successive times
from `earliest_var` upward as they are introduced
(`typing_env.ml#add_variable_definition`, which allocates `next_binding_time`
then bumps it). So *constants precede symbols precede variables*, and earlier
variables precede later ones.

The Aliases domain (`aliases.mli`) tracks a **must-alias** equivalence relation
on `Simple`s, broadened to equality *up to coercion*: `x ~ y` iff there is a
coercion `c` with `x = coerce y c`. Coercions form a groupoid, so `~` is a
genuine equivalence relation (`aliases.mli`, opening comment). Each class has a
distinguished **canonical element**, chosen as the least element by binding time
(subject to a name-mode / scope floor); `Alias_set.find_best` states the
preference order directly: constants are better than symbols, which are better
than variables (`aliases.mli#Alias_set.find_best`).

```rule
RULE T.Env.Canonical.Least
CLAIM normative
CODE middle_end/flambda2/types/env/aliases.mli#get_canonical_element_exn
CODE middle_end/flambda2/types/env/binding_time.ml#consts
CODE middle_end/flambda2/types/env/aliases.mli#Alias_set.find_best
---
The canonical element of an alias class is the element of least binding time
that satisfies the requested min_name_mode and min_binding_time. Ties in the
categories go constants < symbols < variables (consts=0, symbols=1,
variables ≥ earliest_var=3); constants are preferred, then symbols, then the
earliest variable.
```

The environment maintains two coupled invariants linking aliases to stored
types (`types.md`, "Relational domains and reduction"): the canonical element of
each class is the one that carries a *concrete* (non-alias) type, and every
non-canonical name carries an `Equals` type pointing (eventually) at the
canonical one. The code actively checks the first half.

```rule
RULE T.Env.Canonical.NoEqualsOnCanonical
CLAIM normative
CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias
---
No canonical name may be given an Equals (alias) type: adding an `Equals s`
equation on a name whose canonical element is itself is a fatal error. Dually,
non-canonical names carry Equals equations resolving toward the canonical
element.
```

```rule
RULE T.Env.Canonical.ConcreteOnCanonical
CLAIM descriptive
CODE middle_end/flambda2/types/env/typing_env.ml#replace_equation
CAVEAT disclosure: invariant checked only under debug flag concrete_types_only_on_canonicals; states intended representation, not an always-enforced property (hence its descriptive claim).
---
Concrete (non-alias) types are stored only on canonical names. When the debug
flag concrete_types_only_on_canonicals is set, adding a concrete type to a
non-canonical name is a fatal error.
NOTES: Guarded by a debug flag rather than always enforced, hence descriptive;
it documents the intended representation invariant.
```

Two consequences of this canonical/binding-time discipline are worth stating as
rules, because neither is guaranteed by a single function: constant knowledge is
indelible, and the aliases domain — not the stored `Equals` equations — is what
expansion relies on.

```rule
RULE T.Env.ConstCanonicalPersists
CLAIM normative
CODE middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals
CODE middle_end/flambda2/types/env/aliases.mli#find_best
CODE middle_end/flambda2/types/env/binding_time.ml#consts
CODE middle_end/flambda2/types/env/meet_env.ml#record_demotion
CAVEAT disclosure: read alias-class-only — non-constant Proved answers may degrade Proved→Unknown via non-GLB meet corner (T.Meet.GreatestLowerBound); raise_on_bottom:false silently drops conflicting const equations.
---
canonical_E(x) = c for a constant c (x's alias class contains c);
E' is obtained from E by any finite sequence of downwards-descent operations
  (add_equation / add_env_extension / add_alias, definitions of new names,
  T.meet calls) with no cut/join boundary crossed
--------------------------------------------------
Either E' is bottom (or x : Bottom in E'), or canonical_{E'}(x) = c. Moreover no
alias class in any reachable E' contains two distinct constants: TE.add_alias on
two distinct constant canonicals yields Bottom, mapped to a bottom env by
add_alias_between_canonicals.
NOTES: Composing T.Env.Canonical.Least + T.Meet.AliasAlias: (i) constants have
binding time consts = 0, strictly least, and find_best prefers constants, so a
constant in a class is always canonical; (ii) alias classes only merge; (iii) a
merge either brings a non-constant canonical (demoted to c — record_demotion /
the name-vs-const path fatals on "Unexpected demotion of constant") or a distinct
constant c', whence add_alias_between_canonicals bottoms the env. Boundary:
indelibility is for ALIAS-CLASS answers only — a non-constant Proved answer can
still degrade Proved→Unknown through the non-GLB meet corner
(T.Meet.GreatestLowerBound); and on a raise_on_bottom:false path a conflicting
const equation is silently dropped rather than bottoming, keeping canonical = c.
replace_concrete_equation normalizes a singleton concrete `x = c` into the
indelible alias domain. Composes: T.Env.Canonical.Least, [§08](08-meet-join.md)
meet. Witnessed in `middle_end/flambda2/tests/meet_test.ml`: `x = 3` then `x = 4`
⟹ bottom; `y = 5` then `y = sym` ⟹ both typed `(= 5)`.
```

```rule
RULE T.Env.AliasesAuthoritative
CLAIM normative
CODE middle_end/flambda2/types/env/aliases.ml#add
CODE middle_end/flambda2/types/env/meet_env.ml#record_demotion
CODE middle_end/flambda2/types/expand_head.ml#expand_head0
CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_alias
---
after any sequence of equation adds and alias demotions in a typing env
--------------------------------------------------
(a) the aliases domain is PATH-COMPRESSED: canonical_elements maps every
    non-canonical member DIRECTLY to its class canonical (Aliases.add re-points
    the whole demoted class on a merge);
(b) the stored name→type Equals equations are NOT path-compressed: record_demotion
    rewrites only the demoted name's own equation, so a stored Equals may target a
    non-canonical name whose own stored type is itself Equals (a stored chain);
(c) hence every read (expand_head0, type_simple_in_term_exn, all provers) resolves
    through the aliases domain, and the one-step property of T.Expand.Head is a
    consequence of (a) — NOT of the shape of the stored equations.
NOTES: Ch-07 §3.1 hedges correctly ("pointing (eventually) at the canonical one")
but T.Expand.Head's one-step termination rests on the compressed canonical_elements
lookup plus T.Env.Canonical.NoEqualsOnCanonical, not on the stored graph (which
does contain Equals-to-Equals edges). Consequence for tools/agents: never chase
stored Equals targets to find a canonical (they can be stale); use
get_canonical_simple. Witnessed by test_meet_chains_three_vars (checked-in
meet_test.expected): stored var3:(= var1), var2:(= var1), var1:(= my_symbol) while
canonical_elements maps var1/var2/var3 all directly to my_symbol. Composes:
T.Env.Canonical.NoEqualsOnCanonical, T.Expand.Head, T.Meet.AliasAlias.
```

Resolving a `Simple` to its type therefore means: find the canonical element,
look up its concrete type, and return an alias to the canonical. This is what
`type_simple_in_term_exn` does — it returns `alias_type_of kind canonical`
together with the canonical simple, so callers see the canonical name rather
than the queried one (`typing_env.ml#type_simple_in_term_exn`). Constants resolve
to their exact type (`type_for_const`).

```rule
RULE T.Env.Find.Canonical
CLAIM normative
CODE middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn
CODE middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn
---
type_simple_in_term_exn(E, s) returns (alias_type_of κ canonical, canonical),
where canonical is the canonical element of s's alias class (respecting name-mode
constraints) and κ is the kind of s. A constant resolves to its own exact type.
```

### 3.2 find, symbols, and missing code

`find E x κ` (`typing_env.ml#find`) returns the type stored for a name, with
three edge cases worth recording:

- If `E` is bottom, every lookup returns `bottom_like` of the appropriate kind
  (`typing_env.ml#find_with_binding_time_and_mode'`, final branch): once a point
  is proven unreachable, all names have type `Bottom`.
- A symbol that is defined but carries no equation defaults to the *initial
  symbol type*, `any_value` (`typing_env.ml#initial_symbol_type`, which is
  `MTC.unknown K.value`, used in the symbol branches of
  `find_with_binding_time_and_mode'`). Note `any_value = Value TD.unknown` is the
  *unrestricted* top of kind `Value` and admits `Null`; the non-null top is the
  distinct type `any_non_null_value` (`type_grammar.ml#any_non_null_value`),
  which is not what symbols default to.
- A variable coming from a **missing `.cmx`** resolves to `unknown` of its kind
  with binding time `imported_variables` (`find_with_binding_time_and_mode'`,
  the `None` resolver branch). We do not model cross-unit import precision (see
  the scope ledger, §6).

```rule
RULE T.Env.Find.Bottom
CLAIM normative
CODE middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'
CODE middle_end/flambda2/types/env/typing_env.ml#make_bottom
---
If is_bottom E, then find E x κ = Bottom (bottom_like of κ) for every name x.
```

```rule
RULE T.Env.Find.SymbolDefault
CLAIM normative
CODE middle_end/flambda2/types/env/typing_env.ml#find_with_binding_time_and_mode'
CODE middle_end/flambda2/types/env/typing_env.ml#initial_symbol_type
---
A defined symbol with no equation has type any_value = MTC.unknown K.value =
Value TD.unknown, the unrestricted top of kind Value. This admits Null; the
non-null top any_non_null_value is a distinct, more precise type not used here.
A symbol referenced from a missing .cmx also defaults to any_value; a variable
from a missing .cmx defaults to unknown of its kind at binding time
imported_variables.
```

New equations must be **closed** with respect to the environment: their free
names must already be bound, else the analysis has produced a dangling reference.

```rule
RULE T.Env.Equation.Closed
CLAIM normative
CODE middle_end/flambda2/types/env/typing_env.ml#invariant_for_new_equation
---
A new equation `name = T` may only mention (in T's free names) names already
bound in E; a locally-unbound free name is a fatal error.
```

### 3.3 Levels, scopes, and existentials

To make join cheap, `E` is stratified into **levels**
(`typing_env_level.mli`). Each `One_level` records a `Scope.t`, the level's
`Typing_env_level` (the definitions and equations *added at that level*), and a
`Cached_level` giving the full name→type map as of the end of that level
(`typing_env.ml#One_level`). The environment holds a `current_level` and a
stack of `prev_levels` ordered greatest-scope-first. `increment_scope` starts a
fresh level; branching points bump the scope so each branch accumulates its own
level(s) (`types.md`, "Typing_env_level: improved join algorithm").

A `Typing_env_level` is thus the difference between two environment states: a set
of newly-defined variables with kinds and binding times, plus the equations and
symbol projections added (`typing_env_level.mli#create`). `cut E ~cut_after`
extracts the levels strictly after a scope as one `Typing_env_level`, and
`cut_as_extension` returns them as a `Typing_env_extension`
(`typing_env.mli#cut`, `#cut_as_extension`). This is exactly the material that
join combines across branches ([§08](08-meet-join.md)).

**Existential variables** arise at scope exit. The field `min_binding_time`
marks the boundary: a variable whose binding time is *strictly earlier* than
`min_binding_time` is out of the current scope and its name mode is forced to
`In_types` (`binding_time.ml#With_name_mode.scoped_name_mode`). `In_types`
variables may appear inside types but not in terms — they are the existentially
quantified variables of `types.md` ("Existential variables"). Only ordinary
variables become existential (when they leave scope), and unrelated ones are
pruned, keeping their number in check.

```rule
RULE T.Env.Scope.Existential
CLAIM normative
CODE middle_end/flambda2/types/env/binding_time.ml#With_name_mode.scoped_name_mode
CODE middle_end/flambda2/types/env/typing_env.mli#cut
---
A variable with binding time strictly earlier than E's min_binding_time is out
of scope; its name mode is reported as In_types, making it an existential
variable that may occur inside types but not in terms. Existentials are
introduced when cutting a level at scope exit (cut / cut_as_extension).
```

The three name modes are `Normal` (usable in terms), `In_types` (usable only in
types — existentials and other type-only names), and `Phantom` (debugging)
(`binding_time.ml#With_name_mode.name_mode`).

## 4. Concretization γ_E

Concretization maps a type to the set of runtime values it approximates,
*relative to* an environment `E` and (implicitly) a heap `H` from [§04](04-opsem.md).
Because types are relational — an `Equals s` type refers to the value of `s`,
and a relational immediate refers to other names — `γ` must be indexed by a
*concrete environment* `ρ` (a map from names to values, in `E`'s domain) rather
than defined pointwise. Write `γ_E(T) ⊆ Val` for the set of values `v` such that
some `ρ` consistent with `E` maps the value in question to `v`; the per-former
clauses below define the constraint that `v` (and `ρ`) must satisfy. These
definitions are the intended denotation; [§13](13-soundness.md) owns the soundness theorem
that ties them to Simplify. Their claim is interpretive: the code does not
compute `γ`, so the definitions' correctness is testable only through
consequences ([§13](13-soundness.md)).

A note on symbols, inherited by every obligation in this section: symbols need
no entry in `ρ`. A symbol is always evaluable — through `ρ` when `ρ` binds it
(as `OS.Let.Static` arranges for static sets of closures), and to the static
pointer `ptr sym` otherwise ([§04](04-opsem.md), `OS.Simple.Eval`) — in either
case to a `Value`-kinded value, per symbols-denote-statics. So no clause built
on this section (consistency of `ρ` with `E`, satisfaction of an extension, or
a simulation premise over them) may demand that `ρ` literally bind a symbol;
obligations read names the way evaluation does, through `⟦·⟧ρ`.

```rule
RULE T.Gamma.Kind
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#kind
---
γ_E(T) ⊆ { v ∈ Val | v has kind κ(T) }. Concretization never crosses kinds; the
kinds partition Val ([§03](03-kinds.md)).
```

```rule
RULE T.Gamma.TopBottom
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_descr.mli#unknown
CODE middle_end/flambda2/types/grammar/type_descr.mli#bottom
---
γ_E(Unknown_κ) = { v ∈ Val | v has kind κ } (all values of the kind).
γ_E(Bottom_κ) = ∅ (the point is unreachable).
```

```rule
RULE T.Gamma.Alias
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#alias_type_of
CODE middle_end/flambda2/types/grammar/type_grammar.ml#get_alias_exn
---
γ_E(Equals s) under concrete environment ρ = { v | v = ρ(s) }, where ρ(s)
applies s's coercion to the value of its name/constant. An alias type denotes the
singleton "same value as s"; combined over all ρ consistent with E it is the set
of values s can take.
```

### 4.1 Value concretization

For kind `Value`, `γ` splits along nullability and then along the non-null head.

```rule
RULE T.Gamma.Value.Nullability
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_value
VERIFIED 14-validation/n_way_join_preserves_null.md @ 1c1940b7ea
---
γ_E({ non_null; is_null }) = γ_E(non_null) ∪ N, where N = { Null } if is_null =
Maybe_null and N = ∅ if is_null = Not_null; γ_E(non_null) is ∅ when non_null =
Bottom and all non-null values of kind Value when non_null = Unknown. When
is_null = Maybe_null { is_null = Some y }, ρ additionally satisfies ρ(y) =
(1 if ρ(this) = Null else 0).
```

```rule
RULE T.Gamma.Value.Variant
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
---
γ_E(Variant { immediates; blocks; is_int; get_tag; extensions }) =
γ_E(immediates) ∪ γ_E(blocks), where γ_E(immediates) are tagged immediates whose
untagged value lies in the immediates arm and γ_E(blocks) are pointers to blocks
described by the row_like_for_blocks. When is_int = Some b, ρ(b) = 1 on the
immediates arm and 0 on the blocks arm; when get_tag = Some g and the value is a
block, ρ(g) = that block's tag. Extensions constrain ρ per arm (§5).
```

```rule
RULE T.Gamma.Value.RowLikeBlocks
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_blocks
---
γ_E(row_like_for_blocks) = the set of block pointers p such that, for some case
(tag t, index d, fields F̄) in known_tags (or other_tags), p points to a block of
tag t whose size is admitted by d — exactly |F̄| when d = Known, at least |F̄|
when d = At_least — and whose field i holds a value in γ_E(F̄ᵢ) for each tracked
field i, and ρ satisfies that case's env_extension.
```

```rule
RULE T.Gamma.Value.Boxed
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
---
γ_E(Boxed_κ T alloc_mode) = the set of pointers to a boxed number of naked kind κ
whose contents value lies in γ_E(T). Similarly Array { element_kind; length;
contents } concretizes to array pointers whose length lies in γ_E(length), whose
elements have kind element_kind, and (when contents = Immutable F̄) whose element
i lies in γ_E(F̄ᵢ). String concretizes to string pointers whose (length,
contents) match the String_info set. Mutable_block concretizes to all block
pointers of the given alloc mode (contents untracked).
```

```rule
RULE T.Gamma.Value.Closures
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_closures
CODE middle_end/flambda2/types/grammar/type_grammar.mli#closures_entry
CAVEAT disclosure: code-pointer clause reads code_id up to the newer_version_of version class per T.Gamma.Closures.CodeAgeLoose; exact-match γ would be unsound before any meet.
---
γ_E(Closures { by_function_slot }) = the set of closure pointers p such that p
selects some function slot f present in known_closures, the closure block
contains at least the function and value slots named by the case's
Set_of_closures_contents, the code pointer for f implements SOME code-age-related
version of the function_type's code_id (the class of code_id under the
newer_version_of preorder), NOT necessarily code_id exactly, and each
value/function slot component holds a value in γ_E of the corresponding component
type.
NOTES: The code-pointer clause is loosened from exact-match to the version class
per T.Gamma.Closures.CodeAgeLoose: Simplify legitimately assigns an old-cid
closure type to a value that will carry the newer specialized code, so exact-match
γ would make ordinary closure typing unsound before any meet.
```

```rule
RULE T.Gamma.Closures.CodeAgeLoose
CLAIM interpretive
CODE middle_end/flambda2/types/meet_and_join.ml#meet_code_id
CODE middle_end/flambda2/types/env/code_age_relation.ml#meet
CODE middle_end/flambda2/types/env/typing_env.ml#add_to_code_age_relation
CAVEAT disclosure: meet_code_id abuses Both_inputs on unequal cids ('We are kind of lying here' code comment); nondeterministically imprecise, sound only under version-class γ.
CAVEAT disclosure: rule is a cross-chapter consistency constraint between ch-07 γ and ch-08 meet_code_id; no single file or CODE anchor exhibits it.
---
the γ clause for Closures code pointers (T.Gamma.Value.Closures) must read the
function_type's code_id up to the newer_version_of preorder, not exactly
--------------------------------------------------
The version-class reading is forced by two facts, not by T.Meet.Sound (which is
⊇-only and so vacuously satisfied by a non-⊥ result on disjoint inputs):
(a) meet_code_id distinguishes Bottom (provably unrelated cids) from Ok-newer
    (version-related cids, resolved to the newer — code_age_relation.ml#meet:
    "whichever is newer ... otherwise bottom"); under exact-match γ both
    intersections are empty, so the distinction the code carefully draws would be
    meaningless. Only the version-class γ makes it contentful.
(b) Type-assignment soundness: Simplify assigns old-cid closure types to values
    that carry the NEWER specialized code, unsound under exact γ before any meet.
    add_to_code_age_relation records newer↦older exactly when Simplify
    re-specializes the same function's code.
NOTES: Descriptive finding: in the Unknown-relatedness case (e.g. missing .cmx)
meet_code_id returns Both_inputs for UNEQUAL code ids, with the code comment "We
are kind of lying here — using either input is correct, but might lose different
information" — the Both_inputs protocol (inputs equally good) is deliberately
abused, so which cid survives depends on the caller's combine plumbing. Sound
under the version-class γ (either cid's version class over-approximates the value)
but nondeterministically imprecise. A consistency constraint BETWEEN the ch-07
concretization and the ch-08 algorithm; neither file alone shows it. Composes:
T.Gamma.Value.Closures (corrected), [§08](08-meet-join.md) meet_code_id.
```

### 4.2 Naked-number concretization

```rule
RULE T.Gamma.Naked.Set
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float
---
For a naked number kind, γ_E(head) = the finite set of constants named by the
head (a subset of the kind's values). γ_E(Unknown) is all values of the kind.
The empty set never occurs as a head (it is Bottom).
```

```rule
RULE T.Gamma.Naked.Relational
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate
---
γ_E of a Naked_immediate head with set S and inverse_relations M, under ρ, is
{ v | (S = Unknown ∨ v ∈ S) ∧ for every (R ↦ N) ∈ M and x ∈ N, v = R(ρ(x)) },
where Is_int(w) = 1 iff w is a tagged immediate, Get_tag(w) = tag of block w, and
Is_null(w) = 1 iff w = Null. The relations constrain the concrete environment,
not just the immediate's own value.
```

## 5. Environment extensions ε

An environment extension is a set of equations to be layered onto `E`:
`env_extension = { equations : T Name.Map.t }`
(`type_grammar.ml#env_extension`; the public `Typing_env_extension.t` is a
synonym, `typing_env_extension.mli#t`). Extensions are the currency of the meet
algorithm ([§08](08-meet-join.md)): meet returns a direct result *plus* an extension of
reductions still to apply, and the top-level meet re-applies the extension until
nothing changes. `With_extra_variables` additionally carries existential
variable definitions (`typing_env_extension.mli#With_extra_variables`), used when
an extension must introduce fresh existentials.

Concretely, an extension denotes the *intersection* of the constraints of its
equations: applying `ε` to `E` shrinks `γ` to the concrete environments
satisfying every equation in `ε` as well.

```rule
RULE T.Gamma.EnvExtension
CLAIM interpretive
CODE middle_end/flambda2/types/grammar/type_grammar.ml#env_extension
CODE middle_end/flambda2/types/env/typing_env_extension.mli#t
---
An extension ε = { name₁ = T₁, …, nameₙ = Tₙ } denotes the constraint that ρ(nameᵢ)
∈ γ_E∪ε(Tᵢ) for all i. Concretizing E extended by ε keeps exactly the concrete
environments consistent with E and with every equation of ε; γ_(E∪ε) ⊆ γ_E.
```

### 5.1 Extensions inside disjunctions

Row_like cases and variants carry their own extensions, giving the domain a form
of case-dependent reasoning (`types.md`, "Meet and disjunctions"). A
`variant_extensions` is either `No_extensions` or `Ext { when_immediate;
when_block }`, attaching one extension to the immediate arm and one to the block
arm of a variant (`type_grammar.ml#variant_extensions`). Likewise every
`row_like_case` has an `env_extension`. These encode constraints *implied by
selecting that disjunct*: when meet later proves all other disjuncts bottom, the
surviving case's extension is added to the result. The worked example in
`types.md` (the `Left`/`Right` type) shows why: selecting `Left` can be made to
imply `x = 0` via the case extension, which a plain product could not record.

```rule
RULE T.Grammar.Disjunction.Extensions
CLAIM normative
CODE middle_end/flambda2/types/grammar/type_grammar.ml#variant_extensions
CODE middle_end/flambda2/types/grammar/type_grammar.ml#row_like_case
---
Each row_like_case carries an env_extension, and a Variant carries
variant_extensions (No_extensions, or Ext with a when_immediate and a when_block
extension). These extensions hold constraints implied by selecting that disjunct;
γ of the disjunct is intersected with γ of its extension (§5). They are only
guaranteed to be exploited by meet on Row_like structures, not by join
(types.md, limitations).
```

## 6. Scope ledger: precision not modeled

The following are represented in the code but treated shallowly here, and should
be assumed opaque unless a later chapter says otherwise:

- **Symbol projections** (`typing_env.mli#add_symbol_projection`,
  `#find_symbol_projection`): the fact that a variable equals a projection out of
  a statically-allocated symbol. Tracked for lifting/rebuilding; not given a `γ`
  clause here.
- **`.cmx` import and missing code** (`typing_env.ml#resolver`, the
  missing-`.cmx` branches of `find`, `variable_is_from_missing_cmx_file`):
  cross-compilation-unit type import. We model the *default* results (`any_value`
  / `unknown`) but not the import machinery or `Missing_code`.
- **Depth / rec_info in function types** (`function_type.rec_info`,
  `head_of_kind_rec_info`): inlining-depth variables. Opaque here; [§11](11-inlining.md)
  owns them.
- **Alloc modes** on heads: carried through but not concretized (they affect
  where a value lives, [§04](04-opsem.md)'s heap discipline, not which values a type admits).
- **Coercions** on aliases: modeled only as "equality up to coercion"; the
  groupoid laws are stated (§3.1) but individual coercions are not enumerated.
