Below is a concrete v1 spec for the DSL and the OCaml it generates.

It covers:

* finite distributive **base lattices**
* **products**
* automatic **opposites**
* named **embeddings** between base lattices
* generated operations `join`, `meet`, `leq`, `sub`, `imply`
* product-field projections and their adjoints
* a public convenience layer with nested `Const` modules on lattices and
  nested `Axis` / `Const` modules on products
* optional top-level re-exports of embedding aliases

It does **not** cover the function-space lattices such as `Modality.*` / `Crossing.*`. Those should be a later extension.

---

# 1. Surface form

The DSL lives in a separate source file, for example:

```text
foo.lattice
```

The whole file is parsed as the lattice DSL:

```text
... declarations ...
```

A standalone generator reads the `.lattice` file and emits ordinary OCaml
structure items, for example into a generated `.ml` file.

It may also emit a companion generated OCaml test file.

---

# 2. Concrete syntax

## 2.1 Declarations

```text
decl ::=
    LatticeName "=" base_body
  | LatticeName "=" product_body
  | SmallName "<=" BigName "via" embedding_body [ "aliases" alias_body ]
```

There is no separate `op` declaration. Every declared lattice `L` automatically gets an opposite module `L_op`.

---

## 2.2 Base lattices

```text
base_body ::= "[" clause_list "]"

clause_list ::= clause (";" clause)* [";"]

clause ::=
    ElemName
  | ElemName ("<" ElemName)+
  | ElemName (">" ElemName)+
```

Examples:

```text
Bool = [
  False < True
]

Severity = [
  Low < Medium < High < Critical
]

Diamond = [
  Bot < Left < Top;
  Bot < Right < Top
]

Regionality = [
  Global < Regional < Local
]
```

Singleton clauses are allowed:

```text
L = [
  A;
  B < C
]
```

Meaning:

* every name mentioned in a clause declares an element of the lattice
* a chain clause contributes adjacent strict edges only
* then the global transitive closure is taken

So:

```text
A < B < C
```

means edges `A < B` and `B < C`.

---

## 2.3 Direction and native storage polarity

All non-singleton clauses in one base declaration must use the **same** comparison direction.

Allowed:

```text
L = [ A < B; C < D ]
```

Allowed:

```text
L = [ D > C > B > A ]
```

Rejected:

```text
L = [ A < B; C > D ]
```

The direction determines both the logical order and the native storage side:

* `<` means the declaration is written in logical order, and the native side is `L`
* `>` means the declaration is written in reverse logical order, and the native side is `L_op`

Formally:

* for `a < b`, the logical order edge is `a < b`
* for `a > b`, the logical order edge is `b < a`

So

```text
L = [ Low < Medium < High ]
```

and

```text
L = [ High > Medium > Low ]
```

define the same logical lattice, but different native storage polarity.

---

## 2.4 Products

```text
product_body ::= "{" field_list "}"

field_list ::= field (";" field)* [";"]

field ::= field_name ":" ty

ty ::= LatticeName | LatticeName "^op"
```

Examples:

```text
State = {
  sev   : Severity;
  flag  : Bool ^op;
  shape : Diamond;
}

Alloc = {
  area       : Locality;
  linearity  : Linearity;
  portability: Portability;
}
```

A field type `T ^op` means:

* same carrier as `T`
* opposite lattice structure

Internally, every declared product also induces a derived object/axis model:

* one product object for the product as a whole
* one axis object per field
* canonical generated names for projection and adjoint operations on that axis

This derived model is generator-internal; it does not require extra surface DSL
syntax.

---

## 2.5 Explicit embeddings

```text
embedding_body ::= "{" mapping_list "}"

mapping_list ::= mapping (";" mapping)* [";"]

mapping ::= SmallElem "->" BigElem

alias_body ::= "{" alias_list "}"

alias_list ::= alias (";" alias)* [";"]

alias ::= alias_slot "=" AliasName

alias_slot ::= "embed" | "left" Nat | "right" Nat
```

Example:

```text
Locality <= Regionality via {
  Global -> Global;
  Local  -> Local;
}
```

Optional aliases:

```text
Locality <= Regionality via {
  Global -> Global;
  Local  -> Local;
} aliases {
  embed     = Locality_as_regionality;
  left1     = Regional_to_local;
  right1    = Regional_to_global;
  left2     = Local_to_regional;
  right2    = Global_to_regional;
}
```

In v1, explicit embeddings are only allowed between **base lattices**.

---

# 3. Static semantics

## 3.1 Namespaces

Lattice names are global.

Element names are local to one base lattice declaration.

Field names are local to one product declaration.

---

## 3.2 Base-lattice validity

For each base lattice:

1. collect all declared element names
2. expand chain clauses into adjacent strict edges
3. apply transitive closure
4. reject cycles
5. compute `<=`
6. check that every pair has a unique meet and join
7. check existence of unique bottom and top
8. check distributivity

If any check fails, the declaration is rejected.

---

## 3.3 Product validity

For each product:

* every field type must refer to a previously declared lattice
* field names must be distinct

Products are finite distributive lattices by componentwise construction.

---

## 3.4 Embedding validity

For

```text
Small <= Big via { ... }
```

the generator checks that the given map:

* is total on `Small`
* is injective
* is monotone
* is order-reflecting

So it must be an order embedding.

If not, reject.

---

# 4. Generated OCaml items

For each declared lattice `L`, the generator emits:

* `module L`
* `module L_op`

For each explicit embedding `S <= B`, it generates:

* `module S_in_B`

The generated code is ordinary OCaml modules.

For products, the generator also emits explicit concrete projection/adjoint
names:

* `proj_<field>`
* `min_with_<field>`
* `max_with_<field>`

as synonyms for the immediate field projection and its two canonical adjoints.

If an embedding declares aliases, those aliases are also re-exported at the file
top level as value bindings of the same names, provided the generated top-level
value namespace stays collision-free.

If requested, the generator also emits one ordinary OCaml test module that
checks the generated modules.

---

# 4.1 Generated test file

If the caller requests test generation, the generator emits a companion `.ml`
test file.

The generated test file:

* depends only on the OCaml standard library
* targets the compilation unit implied by the generated `.ml` basename
* checks the generated base, product, and embedding modules

The generated test code uses:

* exhaustive checking when the relevant finite carrier is small enough
* deterministic seeded sampling otherwise

---

# 5. Name mangling

The DSL can use names like `Global`, `Read_write`, `External64`.

Generated OCaml value names are the lowercase snake_case versions.

Examples:

* `Global` -> `global`
* `Read_write` -> `read_write`
* `External64` -> `external64`
* `Locality_as_regionality` -> `locality_as_regionality`

If two DSL names collide after mangling in the same generated OCaml namespace,
reject.

Concretely:

* lattice module names must be globally distinct
* element value names must be distinct within one generated lattice module
* field names must be distinct within one generated product module
* generated `proj_<field>`, `min_with_<field>`, and `max_with_<field>` names
  must also be distinct within one generated product module
* embedding-function names and aliases must be distinct within one generated
  embedding module
* top-level re-exported embedding aliases must be globally distinct

Lattice names are preserved as module names and must therefore be valid OCaml module identifiers.

Field names are preserved and must therefore be valid OCaml identifiers.

---

# 6. Generated API for a base lattice

For

```text
Locality = [
  Global < Local
]
```

generate:

```ocaml
module Locality : sig
  type t = private int

  val global : t
  val local  : t

  val bottom : t
  val top    : t

  val leq    : t -> t -> bool
  val equal  : t -> t -> bool
  val join   : t -> t -> t
  val meet   : t -> t -> t
  val sub    : t -> t -> t
  val imply  : t -> t -> t

  val name    : t -> string
  val of_name : string -> t option
  val pp      : Format.formatter -> t -> unit
  val show    : t -> string

  module Repr : sig
    val bits      : int
    val mask      : int
    val to_int    : t -> int
    val of_int_exn: int -> t
  end

  module Const : sig
    type t = Locality.t

    val min    : t
    val max    : t
    val le     : t -> t -> bool
    val equal  : t -> t -> bool
    val join   : t -> t -> t
    val meet   : t -> t -> t
    val print  : Format.formatter -> t -> unit
    val legacy : t
  end
end

module Locality_op : sig
  type t = Locality.t

  val global : t
  val local  : t

  val bottom : t
  val top    : t

  val leq    : t -> t -> bool
  val equal  : t -> t -> bool
  val join   : t -> t -> t
  val meet   : t -> t -> t
  val sub    : t -> t -> t
  val imply  : t -> t -> t

  val name    : t -> string
  val of_name : string -> t option
  val pp      : Format.formatter -> t -> unit
  val show    : t -> string

  module Repr : sig
    val bits      : int
    val mask      : int
    val to_int    : t -> int
    val of_int_exn: int -> t
  end
end
```

Important:

```ocaml
type Locality_op.t = Locality.t
```

They are the same carrier. Only the operations differ.

The named element constants are shared values:

```ocaml
Locality_op.global = Locality.global
Locality_op.local  = Locality.local
```

---

# 7. Generated API for a product

For

```text
State = {
  sev   : Severity;
  flag  : Bool ^op;
  shape : Diamond;
}
```

generate:

```ocaml
module State : sig
  type t = private int

  type view = {
    sev   : Severity.t;
    flag  : Bool_op.t;
    shape : Diamond.t;
  }

  val make    : sev:Severity.t -> flag:Bool_op.t -> shape:Diamond.t -> t
  val view    : t -> view
  val of_view : view -> t

  val sev   : t -> Severity.t
  val flag  : t -> Bool_op.t
  val shape : t -> Diamond.t

  val proj_sev   : t -> Severity.t
  val proj_flag  : t -> Bool_op.t
  val proj_shape : t -> Diamond.t

  val with_sev   : Severity.t -> t -> t
  val with_flag  : Bool_op.t -> t -> t
  val with_shape : Diamond.t -> t -> t

  (* projection adjoints *)
  val sev_bot   : Severity.t -> t
  val sev_top   : Severity.t -> t
  val min_with_sev : Severity.t -> t
  val max_with_sev : Severity.t -> t
  val flag_bot  : Bool_op.t -> t
  val flag_top  : Bool_op.t -> t
  val min_with_flag : Bool_op.t -> t
  val max_with_flag : Bool_op.t -> t
  val shape_bot : Diamond.t -> t
  val shape_top : Diamond.t -> t
  val min_with_shape : Diamond.t -> t
  val max_with_shape : Diamond.t -> t

  val bottom : t
  val top    : t

  val leq    : t -> t -> bool
  val equal  : t -> t -> bool
  val join   : t -> t -> t
  val meet   : t -> t -> t
  val sub    : t -> t -> t
  val imply  : t -> t -> t

  val pp   : Format.formatter -> t -> unit
  val show : t -> string

  module Layout : sig
    val sev_shift   : int
    val sev_mask    : int
    val flag_shift  : int
    val flag_mask   : int
    val shape_shift : int
    val shape_mask  : int
  end

  module Repr : sig
    val bits      : int
    val mask      : int
    val to_int    : t -> int
    val of_int_exn: int -> t
  end

  module Axis : sig
    type _ t =
      | Sev : Severity.t t
      | Flag : Bool_op.t t
      | Shape : Diamond.t t

    type packed = P : 'a t -> packed

    val all : packed list
    val print : Format.formatter -> 'a t -> unit
  end

  module Const : sig
    type t = State.t
    type 'a axis = 'a Axis.t

    val min    : t
    val max    : t
    val le     : t -> t -> bool
    val equal  : t -> t -> bool
    val join   : t -> t -> t
    val meet   : t -> t -> t
    val print  : Format.formatter -> t -> unit
    val legacy : t

    val split : t -> view
    val merge : view -> t

    val proj     : 'a axis -> t -> 'a
    val min_with : 'a axis -> 'a -> t
    val max_with : 'a axis -> 'a -> t
  end
end

module State_op : sig
  type t = State.t
  ...
end
```

`State_op` mirrors the `State` API, but each field type is polarity-flipped once
more:

* a field declared as `K` has type `K_op.t` in `State_op`
* a field declared as `K ^op` has type `K.t` in `State_op`

The field projection adjoints satisfy:

* `sev_bot ⊣ sev ⊣ sev_top`
* `flag_bot ⊣ flag ⊣ flag_top`
* `shape_bot ⊣ shape ⊣ shape_top`

where `_bot` fills all other fields with their bottoms and `_top` fills them with their tops.

---

# 8. Generated API for an embedding

For

```text
Locality <= Regionality via {
  Global -> Global;
  Local  -> Local;
}
```

generate:

```ocaml
module Locality_in_Regionality : sig
  val embed  : Locality.t -> Regionality.t

  (* immediate adjoints, if they exist *)
  val left1  : Regionality.t -> Locality.t
  val right1 : Regionality.t -> Locality.t

  (* next adjoints outward, if they exist *)
  val left2  : Locality.t -> Regionality.t
  val right2 : Locality.t -> Regionality.t

  (* and so on, if more exist *)
end
```

If a listed adjoint does not exist, it is omitted from the generated module
signature and structure.

The canonical meaning is:

* `left1 ⊣ embed ⊣ right1`
* if it exists, `left2 ⊣ left1`
* if it exists, `right1 ⊣ right2`
* and so on outward

If aliases were provided, generate those as synonyms:

```ocaml
val locality_as_regionality : Locality.t -> Regionality.t
val regional_to_local       : Regionality.t -> Locality.t
val regional_to_global      : Regionality.t -> Locality.t
val local_to_regional       : Locality.t -> Regionality.t
val global_to_regional      : Locality.t -> Regionality.t
```

The same aliases are also re-exported at the generated file top level:

```ocaml
val locality_as_regionality : Locality.t -> Regionality.t
val regional_to_local       : Regionality.t -> Locality.t
...
```

In finite proper embeddings, the maximal adjoint chain is finite.

---

# 9. Hidden representation model

This is not part of the surface syntax, but it is part of the spec of the generated code.

## 9.1 Native side

Each base lattice chooses one native side from its declaration direction:

* `<` declaration: native side is `L`
* `>` declaration: native side is `L_op`

Let `N` be that native side.

The carrier is encoded as downsets of the join-irreducibles of `N`.

So the underlying bits encode elements of `N` canonically.

This is why `L.t = L_op.t` can hold: `L` and `L_op` are the same underlying set with different operations.

---

## 9.2 Hidden descriptor

Every generated module `L` has a hidden descriptor with this information:

```ocaml
type round = { shift : int; mask : int }

type desc = {
  bits      : int;
  mask      : int;

  (* bits that are native wrt this logical module *)
  nat_mask  : int;

  (* closure schedules on native bits *)
  down_nat  : round list;
  up_nat    : round list;

  (* closure schedules on dual bits *)
  down_dual : round list;
  up_dual   : round list;
}
```

Intuition:

* `nat_mask` marks the bits that behave natively for this module
* `dual_mask = mask land lnot nat_mask` marks the bits that behave dually

For a base lattice:

* if declared with `<`, `desc(L)` is native and `desc(L_op) = opp(desc(L))`
* if declared with `>`, `desc(L_op)` is native and `desc(L) = opp(desc(L_op))`

For a product:

* a field of type `K` contributes `desc(K)`
* a field of type `K ^op` contributes `opp(desc(K))`
* field descriptors are shifted by their bit offsets and combined

The operator `opp` on descriptors swaps native and dual roles.

---

## 9.3 Closure schedules

A downward implicant round is:

```ocaml
x <- x lor ((x land mask) lsr shift)
```

An upward implicant round is:

```ocaml
x <- x lor ((x land mask) lsl shift)
```

Applying a whole schedule gives:

* `run_down rounds x` = least down-closed superset of `x`
* `run_up   rounds x` = least up-closed superset of `x`

These schedules are precomputed per native base lattice and lifted into products.

---

# 10. Exact operation formulas

Let `m` be a descriptor and let:

```ocaml
let dual_mask = m.mask land lnot m.nat_mask

let run_down rounds x =
  List.fold_left
    (fun x {shift; mask} -> x lor ((x land mask) lsr shift))
    x rounds

let run_up rounds x =
  List.fold_left
    (fun x {shift; mask} -> x lor ((x land mask) lsl shift))
    x rounds
```

Then the generated operations are exactly:

## bottom / top

```ocaml
bottom = dual_mask
top    = m.nat_mask
```

This is the right general formula because dual bits reverse bottom and top.

---

## order

```ocaml
leq x y =
  (((x land lnot y) land m.nat_mask) = 0) &&
  (((y land lnot x) land dual_mask) = 0)
```

So:

* native bits use subset order
* dual bits use superset order

---

## join / meet

```ocaml
let o = x lor y
let a = x land y

join x y =
  (o land m.nat_mask) lor
  (a land dual_mask)

meet x y =
  (a land m.nat_mask) lor
  (o land dual_mask)
```

So:

* native bits use `join = lor`, `meet = land`
* dual bits use `join = land`, `meet = lor`

---

## sub / imply

This is the subtle part.

Let

```ocaml
let zxy = x land lnot y
let zyx = y land lnot x
```

Then:

```ocaml
sub x y =
  ((run_down m.down_nat zxy) land m.nat_mask) lor
  ((lnot (run_up m.up_dual zyx)) land dual_mask)
```

```ocaml
imply x y =
  ((lnot (run_up m.up_nat zxy)) land m.nat_mask) lor
  ((run_down m.down_dual zyx) land dual_mask)
```

This is the required formula.

The subtle point is that on the dual part, both `sub` and `imply` use the **swapped difference** `y & ~x`, not `x & ~y`.

Equivalently:

* native bits:

  * `sub(x,y)   = down(x & ~y)`
  * `imply(x,y) = ~up(x & ~y)`
* dual bits:

  * `sub(x,y)   = ~up(y & ~x)`
  * `imply(x,y) = down(y & ~x)`

These identities must hold:

```text
sub_L(x,y)   = imply_L_op(y,x)
imply_L(x,y) = sub_L_op(y,x)
```

If the implementation does not satisfy those, it is wrong.

---

# 11. Homogeneous fast paths

If a descriptor has only native bits:

```ocaml
dual_mask = 0
```

then:

* `join = lor`
* `meet = land`
* `sub x y   = run_down down_nat (x & ~y)`
* `imply x y = ~ run_up up_nat (x & ~y)` within the slice mask

If a descriptor has only dual bits:

```ocaml
nat_mask = 0
```

then:

* `join = land`
* `meet = lor`
* `sub x y   = ~ run_up up_dual (y & ~x)` within the slice mask
* `imply x y = run_down down_dual (y & ~x)`

This is why source-level `<` vs `>` matters: it lets the author choose which logical side gets the cheap native implementation.

---

# 12. Projection adjoints for products

For every product field projection:

```ocaml
val field : Product.t -> FieldType.t
```

generate the two canonical adjoints:

```ocaml
val field_bot : FieldType.t -> Product.t
val field_top : FieldType.t -> Product.t
```

with:

* `field_bot` filling all other fields with bottoms
* `field_top` filling all other fields with tops

and the adjunction laws:

```text
field_bot ⊣ field ⊣ field_top
```

For a field declared as `K ^op`, the projection type is `K_op.t`.

---

# 13. Example

Input file `example.lattice`:

```text
Locality = [
  Global < Local
]

Regionality = [
  Global < Regional < Local
]

Locality <= Regionality via {
  Global -> Global;
  Local  -> Local;
} aliases {
  embed  = Locality_as_regionality;
  left1  = Regional_to_local;
  right1 = Regional_to_global;
  left2  = Local_to_regional;
  right2 = Global_to_regional;
}

Uniqueness = [
  Unique < Aliased
]

State = {
  loc  : Regionality;
  uniq : Uniqueness ^op;
}
```

Generated items:

* `module Locality`
* `module Locality_op`
* `module Regionality`
* `module Regionality_op`
* `module Uniqueness`
* `module Uniqueness_op`
* `module State`
* `module State_op`
* `module Locality_in_Regionality`

with the signatures described above.

---

# 14. v1 scope limits

This spec intentionally excludes:

* lattices of monotone functions
* `join_const` / `meet_const` function lattices
* automatic synthesis of embeddings between products
* explicit embeddings between products
* user-defined custom names for product elements
* automatic enumeration of large product carriers

Those can be added later without changing the core base/product/embedding design.

---

# 15. Recommended implementation order

1. parse the `.lattice` file
2. validate finite distributive base lattices
3. compute native descriptor + closure schedules
4. generate `L` and `L_op`
5. parse products and generate shifted combined descriptors
6. generate field projections and `_bot` / `_top`
7. parse explicit embeddings and compute maximal adjoint chains
8. add aliases

If useful, I can turn this into an EBNF grammar and a precise internal AST next.
