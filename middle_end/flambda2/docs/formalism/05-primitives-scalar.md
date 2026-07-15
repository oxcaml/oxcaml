# Scalar primitives

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter gives the denotational semantics of the *scalar* primitives of
Flambda 2: integer and floating-point arithmetic, comparison, conversion,
tagging and boxing. Memory primitives (blocks, closures, arrays, strings, atomic
operations, regions) live in [§06](06-primitives-memory.md), which also
owns `Is_int`, `Is_null`, `Phys_equal`, `Get_tag`, `Get_header` and the general
heap-object model. This chapter uses the heap only for boxed numbers, and
cross-references [§06](06-primitives-memory.md) for the conventions it shares.

The judgment defined here is the primitive denotation from the README:

```
⟦p⟧(v̄; H) = (v, H′)      the application of p to argument values v̄ in heap H
⟦p⟧(v̄; H) = undef        p exhibits undefined behaviour on v̄
```

`v̄` is the sequence of (already-evaluated) argument values, in the order the
primitive's `Simple.t` arguments appear. `H` is the heap of [§04](04-opsem.md). For
every scalar primitive except `Box_number` the heap is threaded unchanged
(`H′ = H`); `Box_number` is the sole allocator here and returns a `H′` extending
`H`. The judgment is used by the operational semantics of [§04](04-opsem.md) (the
`Let (x = Prim p) e` step) and its abstract counterpart `⟦p⟧♯` by [§10](10-simplify-rewrites.md)
(constant folding). These denotations are meant to be validated against the
constant-folding branches of `simplify_unary_primitive.ml` and
`simplify_binary_primitive.ml`: for every rule below, whenever the code folds a
primitive to a concrete result, that result must equal `v`.

## Contract

`flambda_primitive.mli` opens with two guarantees that hold for *every*
primitive, scalar or not:

> No primitive raises an exception. (Bounds checking is handled separately.)

> "Primitive" operations: those that perform computation but never affect
> control flow.

```rule
RULE P.Contract.NoRaiseNoControl
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.mli#t
---
p is a primitive application, v̄ its argument values, H a heap
--------------------------------------------------
⟦p⟧(v̄; H) is either (v, H′) or undef; it never denotes an exception,
a trap, or any other transfer of control
NOTES: Where the naive OCaml source could raise (e.g. `1/0` raising
`Division_by_zero`), the frontend inserts an explicit check *around* the
primitive; the primitive proper is undefined on the failing input. See
P.Binary.IntArith.DivModByZero and the note on `check_zero_division`.
```

`undef` denotes genuine undefined behaviour: the operational semantics of
[§04](04-opsem.md) may take any transition, and the abstract transfer function of
[§10](10-simplify-rewrites.md) may return any type (in particular the constant folder is free to
pick a convenient value, and often does — see the shift rules). `undef` is *not*
`Invalid`: `Invalid` (the nullary primitive, [§04](04-opsem.md)) marks statically-known
dead code, whereas `undef` marks a run-time input the program promised never to
supply.

## Preliminaries: number domains

### Machine width

Let `M` be the target machine width (`Target_system.Machine_width.t`), either
32-bit or 64-bit. Several domains and several operations are parameterized by
`M`; where it is fixed by context we leave it implicit. Two widths derive from
`M`:

- `W` — the width of an OCaml `int` (`Sys.int_size`): **63** on a 64-bit target,
  **31** on a 32-bit target. This is the width of both *tagged* and *naked*
  immediates.
- `Wₙ` — the width of a native integer: **64** on a 64-bit target, **32** on a
  32-bit target.

### Integer domains

For a width `w`, write `ℤ_w` for the set of two's-complement integers of that
width, i.e. the integers in `[−2^{w−1}, 2^{w−1}−1]`. Write

```
wrap_w : ℤ → ℤ_w
```

for reduction modulo `2^w` into that range (two's-complement truncation). Signed
integer arithmetic on width `w` is `wrap_w` of the exact result:

```
a ⊕_w b = wrap_w(a + b)      a ⊖_w b = wrap_w(a − b)      a ⊗_w b = wrap_w(a · b)
```

Bitwise operations (`and`, `or`, `xor`, and byte-swaps) act on the `w`-bit
two's-complement representation. For unsigned comparison we write `[a]_w ∈
[0, 2^w)` for the *unsigned* value of the `w`-bit representation of `a` (so
`[a]_w = a` if `a ≥ 0`, else `a + 2^w`).

The width `w(κ)` of each `Standard_int` kind `κ`:

| kind `κ` | `w(κ)` | code (`number_adjuncts.ml`) |
|---|---|---|
| `Tagged_immediate` | `W` | `For_tagged_immediates` |
| `Naked_immediate` | `W` | `For_naked_immediates` |
| `Naked_int8` | 8 | `For_int8s` |
| `Naked_int16` | 16 | `For_int16s` |
| `Naked_int32` | 32 | `For_int32s` |
| `Naked_int64` | 64 | `For_int64s` |
| `Naked_nativeint` | `Wₙ` | `For_nativeints` |

Tagged and naked immediates carry the *same* integer domain `ℤ_W` — the
`Target_ocaml_int` of `numbers/target_ocaml_int.mli`, described as "63-bit
arithmetic on 64-bit targets; 31-bit arithmetic on 32-bit targets". They differ
only in representation (a tagged immediate is a `value`-kind object; a naked
immediate is `naked_immediate`-kind) and in the *shift cut-off* used by the
constant folder (see P.Binary.IntShift).

### Floating-point domains

`𝔽₆₄` and `𝔽₃₂` are the IEEE-754 double- and single-precision values,
represented *by bit pattern* (`Numeric_types.Float_by_bit_pattern`,
`Float32_by_bit_pattern`). Representing floats by bit pattern rather than by
mathematical value has two consequences that matter for the rules below:

- the many NaN bit patterns are distinct values, and `+0.` and `−0.` are
  distinct values;
- there are therefore *two* equivalences on floats, and the code is careful to
  use the right one in each place:
  - **bit-pattern** equality/order (`compare`, `equal` on `Float_by_bit_pattern`
    directly), used to decide whether an algebraic rewrite preserves the exact
    result;
  - **IEEE** equality/order (`Float_by_bit_pattern.IEEE_semantics.{equal,
    compare}`), used to denote the comparison *primitives*. Under IEEE equality
    `NaN ≠ NaN` and `+0. = −0.`.

Arithmetic on `𝔽₆₄`/`𝔽₃₂` is IEEE-754 (`IEEE_semantics.{add,sub,mul,div}` in
`numbers/numeric_types.ml`), computed at the operation's own precision. Note in
particular (`simplify_binary_primitive.ml`, comment above
`Binary_float32_arith`):

> float32 arithmetic operations need to be performed in 32-bit precision to
> preserve rounding behavior.

so `𝔽₃₂` arithmetic is genuine single precision (via `Flambda2_floats.Float32`),
not doubles rounded afterwards.

### Runtime scalar values

[§04](04-opsem.md) owns the runtime value grammar `v` (summarized in the README
[Notation](README.md#notation) section); this chapter uses the following scalar
fragment of it, spelled with [§04](04-opsem.md)'s canonical constructor names. Each
constructor records both the kind and the underlying number:

```
v ::= tagged_imm i            tagged immediate,   i ∈ ℤ_W        (kind value)
    | naked_imm i             naked immediate,    i ∈ ℤ_W        (kind naked_immediate)
    | naked_int8 i | naked_int16 i      naked small ints,  i ∈ ℤ_8 / ℤ_16
    | naked_int32 i | naked_int64 i     naked 32/64 ints,  i ∈ ℤ_32 / ℤ_64
    | naked_nativeint i       naked nativeint,    i ∈ ℤ_Wₙ
    | naked_float b | naked_float32 b   naked floats,  b ∈ 𝔽₆₄ / 𝔽₃₂  (bit patterns)
    | ptr ℓ                   heap pointer (used here only for boxed numbers)
    | ...                     (blocks, closures, immediates-as-pointers: [§06](06-primitives-memory.md))
```

Per the `flambda_primitive.mli` header, "primitives that accept float, int32,
int64 or nativeint values always take (or return) the unboxed versions": the
arithmetic and comparison primitives below operate directly on the naked
constructors (`naked_int32 ·`, `naked_float ·`, …) and on `tagged_imm ·` for the
`Tagged_immediate` kind. A *boxed* number appears only as an argument to
`Unbox_number` / result of `Box_number` and lives in the heap as a `Boxed` heap
object

```
H(ℓ) = Boxed(κ, nv)     κ a Boxable_number kind, nv the naked contents
```

whose general layout, tag and (im)mutability are [§06](06-primitives-memory.md)'s; here we
only read and allocate it.

To write the arithmetic rules once per family we use a kind-indexed pair of
injection/projection between the value constructors and the number domains: for a
`Standard_int` kind `κ`, `val_κ(i)` is the value constructor of kind `κ` holding
`i ∈ ℤ_{w(κ)}` (e.g. `val_{Naked_int32}(i) = naked_int32 i`,
`val_{Tagged_immediate}(i) = tagged_imm i`), and likewise
`val_{Naked_float}(b) = naked_float b`, `val_{Naked_float32}(b) = naked_float32 b`.

---

## Unary scalar primitives

Namespace `P.Unary.*`. All are pure (`H′ = H`) except `Box_number`.

### Integer unary arithmetic — byte swap

`unary_int_arith_op` has a **single** constructor, `Swap_byte_endianness`; there
is no unary integer negation (source-level `-x` on integers becomes
`Binary (Int_arith (κ, Sub))` with a zero left operand — see the note under
P.Binary.IntArith). `Swap_byte_endianness` is kind-dependent, and for the
immediate kinds it operates on a **16-bit** quantity, per the `mli`:

> `Swap_byte_endianness` on a `Tagged_immediate` treats the immediate as
> encoding a 16-bit quantity (in the least significant 16 bits after untagging)
> and exchanges the two halves of the 16-bit quantity. The higher-order bits are
> zeroed.

```rule
RULE P.Unary.IntArith.SwapByteEndianness
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Unary_int_arith
CODE middle_end/flambda2/simplify/number_adjuncts.ml#For_int16s
CODE middle_end/flambda2/numbers/target_ocaml_int.ml#get_least_significant_16_bits_then_byte_swap
---
κ is a Standard_int kind;  swap_κ : ℤ_{w(κ)} → ℤ_{w(κ)} defined below
--------------------------------------------------
⟦Int_arith(κ, Swap_byte_endianness)⟧(val_κ(i); H) = (val_κ(swap_κ(i)), H)
NOTES: swap_κ per kind:
  • Tagged_immediate, Naked_immediate: take the low 16 bits, swap the two
    bytes, zero-extend to W bits (get_least_significant_16_bits_then_byte_swap).
    Higher bits are zeroed, NOT preserved.
  • Naked_int8: the identity (For_int8s.swap_byte_endianness t = t).
  • Naked_int16: full 2-byte swap: (i & 0xff) << 8 | (i >> 8) & 0xff.
  • Naked_int32 / Naked_int64: full 4-/8-byte reversal
    (Numeric_types.Int32/Int64.swap_byte_endianness).
  • Naked_nativeint: full Wₙ/8-byte reversal (Targetint_32_64.swap_byte_endianness).
```

### Float unary arithmetic — abs, neg

```rule
RULE P.Unary.FloatArith
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op
CODE middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen
---
bw ∈ {Float64, Float32};  b ∈ 𝔽 (𝔽₆₄ if Float64, else 𝔽₃₂)
op ∈ {Abs, Neg};  fop = IEEE_semantics.abs if op = Abs, IEEE_semantics.neg if op = Neg
--------------------------------------------------
⟦Float_arith(bw, op)⟧(val(b); H) = (val(fop(b)), H)
NOTES: These are the IEEE sign operations, defined on the bit pattern:
`neg` flips the sign bit (so neg(+0.) = −0., neg(NaN) is a NaN with flipped
sign bit); `abs` clears the sign bit. They do not canonicalize NaNs.
Constant folding additionally requires `Flambda_features.float_const_prop ()`
(the `propagating_float_consts` guard); when disabled the folder leaves the
primitive in place, but the denotation is unchanged.
```

### Numeric conversion — `Num_conv`

`Num_conv { src; dst }` converts between any two `Standard_int_or_float` kinds.
When `src = dst` it is the identity (the folder passes the argument through
untouched, `simplify_unary_primitive.ml#Make_simplify_int_conv`). Otherwise it
applies the single conversion `src → dst`; the code deliberately never fuses a
chain of conversions, because intermediate truncations/sign-extensions are
observable (`flambda_primitive.mli`, CR gbury: a fused `int64 → int32 → float`
would wrongly drop the `int32` sign-extension).

The conversion `conv_{src→dst}` is defined componentwise below. Let
`⌊·⌋ : 𝔽 → ℤ` be truncation toward zero (drop the fractional part).

```rule
RULE P.Unary.NumConv
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_int_conv
CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common
---
src, dst ∈ Standard_int_or_float;  v = val_src(a) the argument value
conv_{src→dst}(a) defined below (undef where noted)
--------------------------------------------------
⟦Num_conv{src; dst}⟧(val_src(a); H) = (val_dst(conv_{src→dst}(a)), H)   [if defined]
⟦Num_conv{src; dst}⟧(val_src(a); H) = undef                            [if conv undefined]
```

Definition of `conv_{src→dst}(a)`, by the *destination* kind (source kind `src`
determines the domain of `a`):

- **to `Tagged_immediate` / `Naked_immediate`** (`to_immediate`): result in
  `ℤ_W`.
  - from `Tagged_immediate`/`Naked_immediate`: identity.
  - from `Naked_int8`/`Naked_int16`: sign-extend the small int to its signed
    value, then `wrap_W` (`of_int (to_int t)`; `to_int` is signed).
  - from `Naked_int32`/`Naked_int64`/`Naked_nativeint`: `wrap_W` of the signed
    value (`of_int32`/`of_int64`/`of_targetint`, "modulo the target word size").
  - from `Naked_float`/`Naked_float32`: `wrap_W(⌊a⌋)` (= `sign_extend_W` of
    `Int64.of_float f` / `Int32.of_float f`) **if** `⌊a⌋ ∈ ℤ_{Wₙ}`, else `undef`.
    The folder wraps through the `Wₙ`-wide host `Int64`/`Int32` conversion (`of_float`
    = `sign_extend (I.of_float …)`, `One_bit_fewer.Make.of_float`), so it is
    defined (and backend-matching) on the whole native range `ℤ_{Wₙ}`, not just
    `ℤ_W`. (The `mli` doc under-states this as `ℤ_W`.)
- **to `Naked_int8` / `Naked_int16`** (`to_naked_int8/16`): `wrap_8`/`wrap_16`
  of the signed value; from floats, `wrap_·(⌊a⌋)`, `undef` only for `±∞`/NaN or
  `⌊a⌋` outside the host `int` range (`of_int (Float.to_int f)`,
  `numeric_types.ml#Short_int`; the boundary is the host `int` width `63`, not
  `W` — see NOTES).
- **to `Naked_int32`** (`to_naked_int32`): the low 32 bits, i.e. `wrap_32` of the
  signed value; from `Naked_int64`/`Naked_nativeint`, `Int64.to_int32` /
  `Targetint.to_int32` (top bits dropped); from floats, `wrap_32(⌊a⌋)`
  (out-of-range `undef`).
- **to `Naked_int64`** (`to_naked_int64`): sign-extend to 64 bits from smaller
  signed sources (`Int64.of_int32` etc.); from floats, `⌊a⌋` clamped/`undef`
  out of `ℤ_64`.
- **to `Naked_nativeint`** (`to_naked_nativeint`): analogous with width `Wₙ`.
- **to `Naked_float`** (`to_naked_float`): from any integer, the IEEE
  round-to-nearest double of its signed value; from `Naked_float32`, the exact
  widening to double; from `Naked_float`, identity.
- **to `Naked_float32`** (`to_naked_float32`): from `Naked_float`, round double
  → single; from `Naked_float32`, identity; from any integer, `Float32.create
  (X.to_float a)` — the integer is first taken to a *double* and then rounded to
  single, so a large `int64` may be **double-rounded**.

```rule
RULE P.Unary.NumConv.FloatToInt.OutOfRange
STATUS normative
CODE middle_end/flambda2/numbers/target_ocaml_int.ml#of_float
CODE middle_end/flambda2/numbers/numeric_types.ml#Short_int
CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common
---
src ∈ {Naked_float, Naked_float32};  a ∈ 𝔽
u(dst) = Wₙ for dst ∈ {Tagged_immediate, Naked_immediate};
         u(dst) = w(dst) for dst ∈ {Naked_int32, Naked_int64, Naked_nativeint};
         u(dst) = 63 (the host `int` width) for dst ∈ {Naked_int8, Naked_int16}
⌊a⌋ ∉ ℤ_{u(dst)}  (in particular a = ±∞ or a is NaN)
--------------------------------------------------
⟦Num_conv{src; dst}⟧(val_src(a); H) = undef
NOTES: The undef boundary is the range of the conversion the folder actually
calls, NOT always the destination's own width — and, for the immediates, NOT `W`.
To the immediates it is `Target_ocaml_int.of_float` = `sign_extend_W(Int64.of_float f)`
(via `One_bit_fewer.Make.of_float`), which wraps through the `Wₙ`-wide host
`Int64`/`Int32` conversion: it is DEFINED (deterministic, backend-matching) for
every `⌊a⌋ ∈ ℤ_{Wₙ}` and undefined only outside `ℤ_{Wₙ}`, so `u = Wₙ`, not `W`.
E.g. `Num_conv{Naked_float; Tagged_immediate}(4.6e18)` (with `⌊a⌋ ∈ ℤ_{Wₙ}\ℤ_W`)
folds to the specific negative tagged immediate the backend's `cvttsd2si` also
produces, not `undef`. To `Naked_int32`/`_int64`/`_nativeint` it is the stdlib
`Int32.of_float`/`Int64.of_float`/`Targetint.of_float`, undefined outside that
type's range (= `w(dst)`). BUT to `Naked_int8`/`Naked_int16` the folder computes
`Int8/Int16.of_float f = of_int (Float.to_int f)` (`numeric_types.ml#Short_int`):
the host `Float.to_int` is undefined only for NaN/±∞ or `⌊a⌋` outside the host
`int` range (width `Sys.int_size = 63`), after which `of_int` *wraps* mod
`2^8`/`2^16`. So e.g. `Num_conv{Naked_float; Naked_int8}(200.0)` is the defined
wrapping result `−56`, not `undef` — only `⌊a⌋ ∉ ℤ_63` (hence `u = 63`, the host
`int` width, which equals `W` only on 64-bit targets) is undef for these two
kinds. number_adjuncts.ml top comment records that xclerc/mshinwell
checked the OCaml backend emits the same instructions as the C runtime
float-conversion functions, so where the result *is* undef it is
target-processor-defined (not portable) rather than a compile/run divergence.
```

```rule
RULE P.Unary.NumConv.Int32ToInt64.SignExtend
STATUS normative
CODE middle_end/flambda2/simplify/number_adjuncts.ml#For_int32s
CODE middle_end/flambda2/z3/sign_extension.py
---
a ∈ ℤ_32
--------------------------------------------------
⟦Num_conv{src = Naked_int32; dst = Naked_int64}⟧(naked_int32 a; H) = (naked_int64 a, H)
NOTES: `Int64.of_int32` sign-extends: the 32-bit value is preserved exactly as a
signed 64-bit value. `z3/sign_extension.py` SMT-verifies that the shift-based
sign-extension implementation `(x << unused) >> unused` used elsewhere in the
backend equals the reference sign-extension, machine-checking the identity this
conversion relies on.
```

### Boolean not

`Boolean_not` is defined only on the two tagged immediates `0` and `1` (kind
`value`); on any other input it is `undef`. The folder maps `0 ↦ 1`, `1 ↦ 0`,
and *drops* every other immediate (yielding `Invalid` when nothing survives —
that is dead code, [§10](10-simplify-rewrites.md), not `undef`).

```rule
RULE P.Unary.BooleanNot
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_boolean_not
---
i ∈ {0, 1}
--------------------------------------------------
⟦Boolean_not⟧(tagged_imm i; H) = (tagged_imm (1 − i), H)

i ∉ {0, 1}
--------------------------------------------------
⟦Boolean_not⟧(tagged_imm i; H) = undef
```

### Tagging and untagging immediates

At the value level these are the identity on the underlying integer `i ∈ ℤ_W`;
they change only the *kind* (`value` ↔ `naked_immediate`). (At the machine level
`Tag_immediate` computes `2i+1` and `Untag_immediate` computes `(i−1)/2`, but the
value model tracks the integer `i`, not its tagged encoding.)

```rule
RULE P.Unary.TagImmediate
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_tag_immediate
---
i ∈ ℤ_W
--------------------------------------------------
⟦Tag_immediate⟧(naked_imm i; H) = (tagged_imm i, H)
```

```rule
RULE P.Unary.UntagImmediate
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate
---
i ∈ ℤ_W
--------------------------------------------------
⟦Untag_immediate⟧(tagged_imm i; H) = (naked_imm i, H)
NOTES: The two are mutually inverse (P.Unary.TagImmediate). The simplifier
records this by adding the inverse CSE equation `Tag_immediate` when it untags,
and vice versa (simplify_unbox_number/simplify_untag_immediate `add_cse`).
```

### Reinterpretation of 64-bit words

`Reinterpret_64_bit_word` reinterprets a 64-bit quantity between four
representations. Two are pure bit reinterpretations between `int64` and
`float64`; the other two convert between an `int64` holding a *machine word* and
a *tagged* immediate, so they must (un)tag rather than copy bits.

```rule
RULE P.Unary.Reinterpret64.Int64AsFloat64
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_unboxed_float64
CODE middle_end/flambda2/numbers/numeric_types.ml#Float_by_bit_pattern_gen
---
b64 ∈ ℤ_64  (a 64-bit pattern)
--------------------------------------------------
⟦Reinterpret_64_bit_word Unboxed_int64_as_unboxed_float64⟧(naked_int64 b64; H)
    = (naked_float b64, H)
NOTES: `Float.of_bits` — the 64 bits are reinterpreted as an IEEE double,
including any NaN pattern. Total.
```

```rule
RULE P.Unary.Reinterpret64.Float64AsInt64
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_float64_as_unboxed_int64
---
b ∈ 𝔽₆₄
--------------------------------------------------
⟦Reinterpret_64_bit_word Unboxed_float64_as_unboxed_int64⟧(naked_float b; H)
    = (naked_int64 bits(b), H)
NOTES: `Float.to_bits` — the inverse of P.Unary.Reinterpret64.Int64AsFloat64.
Total.
```

```rule
RULE P.Unary.Reinterpret64.Int64AsTaggedInt63
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_tagged_int63
---
w ∈ ℤ_64  (an int64 holding a tagged machine word)
--------------------------------------------------
⟦Reinterpret_64_bit_word Unboxed_int64_as_tagged_int63⟧(naked_int64 w; H)
    = (tagged_imm wrap_W([w]_64 >>ᵤ 1), H)
NOTES: The folder computes `of_int64 (Int64.shift_right_logical w 1)`, i.e. a
logical right shift by one (untagging), reduced into ℤ_W. Its comment: "This
primitive is logical OR with 1 on machine words, but here we are working in the
tagged world, so a different computation is required." `>>ᵤ` is logical shift.
```

```rule
RULE P.Unary.Reinterpret64.TaggedInt63AsInt64
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_tagged_int63_as_unboxed_int64
---
i ∈ ℤ_W  (a tagged immediate)
--------------------------------------------------
⟦Reinterpret_64_bit_word Tagged_int63_as_unboxed_int64⟧(tagged_imm i; H)
    = (naked_int64 (2·i + 1), H)
NOTES: The folder computes `Int64.add (Int64.mul (to_int64 i) 2) 1`, i.e. the
tagged machine-word encoding `2i+1`. Inverse of the previous rule on tagged
inputs.
```

### Boxing and unboxing numbers

`Box_number(κ, a)` is the only scalar primitive with an effect: it **allocates**
a fresh boxed-number object holding its naked argument and returns a pointer to
it. `Unbox_number(κ)` projects the naked contents back out; boxed-number
contents are immutable, so unboxing is pure (no coeffect) even though it reads
the heap.

```rule
RULE P.Unary.BoxNumber
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_box_number
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
κ a Boxable_number kind;  nv a naked value of kind κ  (naked_float ·, naked_float32 ·,
  naked_int32 ·, naked_int64 ·, naked_nativeint ·, or a naked vector — [§06](06-primitives-memory.md))
a an Alloc_mode.For_allocations.t;  ℓ ∉ dom(H) fresh
--------------------------------------------------
⟦Box_number(κ, a)⟧(nv; H) = (ptr ℓ, H[ℓ ↦ Boxed(κ, nv)])
NOTES: Generative effect (`Only_generative_effects Immutable`). The alloc_mode
`a` selects heap vs. local-region allocation; the region discipline for local
allocation, and the `Boxed(κ, ·)` object layout/tag, are [§06](06-primitives-memory.md)'s. For `a = Heap`
the simplifier records the inverse `Unbox_number` CSE equation
(simplify_unbox_number `add_cse`); local allocations are not CSE'd.
```

```rule
RULE P.Unary.UnboxNumber
STATUS normative
CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
κ a Boxable_number kind;  H(ℓ) = Boxed(κ, nv)
--------------------------------------------------
⟦Unbox_number(κ)⟧(ptr ℓ; H) = (nv, H)
NOTES: Pure: `No_effects, No_coeffects`. Undefined if `ℓ` is not a boxed number
of kind κ (the frontend/type system guarantees the kind); this is a projection
whose well-typedness is [§03](03-kinds.md)'s responsibility.
```

---

## Binary scalar primitives

Namespace `P.Binary.*`. All are pure (`H′ = H`, `No_effects`).

### Integer arithmetic — `Int_arith`

`binary_int_arith_op` is `Add | Sub | Mul | Div | Mod | And | Or | Xor`, all
parameterized by a `Standard_int` kind `κ`. The six total operators fold with no
side condition; `Div` and `Mod` are partial (undefined when the divisor is 0).
There is no integer overflow trap: every result is `wrap_{w(κ)}`.

```rule
RULE P.Binary.IntArith.Total
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common
---
κ a Standard_int kind, w = w(κ);  a, b ∈ ℤ_w
op ∈ {Add, Sub, Mul, And, Or, Xor};  ⊙ the corresponding operation:
  Add ↦ ⊕_w   Sub ↦ ⊖_w   Mul ↦ ⊗_w
  And ↦ bitwise-and_w   Or ↦ bitwise-or_w   Xor ↦ bitwise-xor_w
--------------------------------------------------
⟦Int_arith(κ, op)⟧(val_κ(a), val_κ(b); H) = (val_κ(a ⊙ b), H)
NOTES: Add/Sub/Mul are two's-complement wrapping (`always_some` in
`Int_ops_for_binary_arith.op`). Source-level unary integer negation is
`Int_arith(κ, Sub)` applied to `(0, x)`; the simplifier even rewrites `x * (−1)`
and `x / (−1)` to this `Sub` form (`Negation_of_the_other_side`).
```

```rule
RULE P.Binary.IntArith.DivMod
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
CODE middle_end/flambda2/numbers/target_ocaml_int.ml#div
---
κ a Standard_int kind, w = w(κ);  a, b ∈ ℤ_w;  b ≠ 0
--------------------------------------------------
⟦Int_arith(κ, Div)⟧(val_κ(a), val_κ(b); H) = (val_κ(wrap_w(a ÷ b)), H)
⟦Int_arith(κ, Mod)⟧(val_κ(a), val_κ(b); H) = (val_κ(a − (a ÷ b)·b), H)
NOTES: `÷` rounds toward zero (`target_ocaml_int.mli#div`: "rounds the real
quotient towards zero, as specified for Stdlib.(/)"). `Mod` takes the sign of
the dividend `a`. `wrap_w` matters only for the overflow case a = min_int,
b = −1 (see P.Binary.IntArith.DivMinIntByMinusOne).
```

```rule
RULE P.Binary.IntArith.DivModByZero
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_zero_division
---
κ a Standard_int kind;  a ∈ ℤ_{w(κ)}
--------------------------------------------------
⟦Int_arith(κ, Div)⟧(val_κ(a), val_κ(0); H) = undef
⟦Int_arith(κ, Mod)⟧(val_κ(a), val_κ(0); H) = undef
NOTES: `Num.div`/`Num.mod_` return `None` on a zero divisor, so the folder emits
no result for that pair. The primitive itself never raises (P.Contract): the
frontend wraps *safe* division `Pdivint Safe`/`Pmodint Safe` in a `Checked`
primitive whose validity condition is `Int_comp(κ, Yielding_bool Neq)(b, 0)` and
whose failure raises `Division_by_zero` (`check_zero_division`). Reaching the
bare `Div`/`Mod` with a zero divisor is therefore unreachable in well-formed
programs, and the simplifier treats a statically-zero divisor as `Invalid`
(dead code), not as this `undef` (`op_lhs_unknown`, `Div | Mod → Invalid`).
```

```rule
RULE P.Binary.IntArith.DivMinIntByMinusOne
STATUS normative
CODE middle_end/flambda2/numbers/target_ocaml_int.ml#div
CODE middle_end/flambda2/simplify/number_adjuncts.ml#For_int64s
CODE backend/cmm_helpers.ml#div_int
CODE backend/cmm_helpers.ml#make_safe_divmod
---
κ a Standard_int kind, w = w(κ)
--------------------------------------------------
⟦Int_arith(κ, Div)⟧(val_κ(−2^{w−1}), val_κ(−1); H) = (val_κ(−2^{w−1}), H)
⟦Int_arith(κ, Mod)⟧(val_κ(−2^{w−1}), val_κ(−1); H) = (val_κ(0), H)
NOTES: The overflow case `min_int / −1` mathematically yields `+2^{w−1}`, which
wraps to `min_int`; `min_int mod −1` is `0`. The earlier worry — that a hardware
`idiv` traps (SIGFPE) on x86 while the folder does not — was investigated and
**refuted**: the backend never emits a bare trapping division for this case.
`cmm_helpers.div_int`/`mod_int` special-case a `−1` divisor to `neg dividend` /
`0`; when the divisor is not a known constant, `make_safe_divmod` inserts a
runtime `divisor = −1` test (guarded by `Arch.division_crashes_on_overflow`) and
falls back to `−dividend` / `0` (comment: PR#5513, "we force x / −1 = −x and
x mod −1 = 0"). The tagged-immediate path (`div_int_caml`) instead proves the
untagged dividend can never be machine `min_int`, so the raw division cannot
trap, and tagging wraps the out-of-range intermediate back to `min_int`. In
every case the runtime value equals the folded value (host `Stdlib`/`Int32`/
`Int64` division also returns `−dividend` for a `−1` divisor). Caveat: the
`Naked_immediate` kind shares the unresolved width question of open question 4;
the folder sign-extends to `ℤ_W` via `One_bit_fewer`, and whether a backend that
computed on full machine words would agree is moot only if such arithmetic is
never emitted.
```

### Integer shifts — `Int_shift`

`int_shift_op` is `Lsl | Lsr | Asr` (logical left, logical right, arithmetic
right), parameterized by `κ`. The *shift count* is a naked immediate (an OCaml
`int`), regardless of `κ`. A shift is defined only for a count in
`[0, w(κ))`; outside that range it is `undef`.

```rule
RULE P.Binary.IntShift
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift
CODE middle_end/flambda2/simplify/number_adjuncts.ml#with_shift
---
κ a Standard_int kind, w = w(κ);  a ∈ ℤ_w;  s ∈ ℤ_W with 0 ≤ s < w
op ∈ {Lsl, Lsr, Asr};  shift the corresponding operation on the w-bit value:
  Lsl ↦ logical left shift by s   Lsr ↦ logical (zero-filling) right shift by s
  Asr ↦ arithmetic (sign-filling) right shift by s
--------------------------------------------------
⟦Int_shift(κ, op)⟧(val_κ(a), naked_imm s; H) = (val_κ(shift(a, s)), H)

s < 0  or  s ≥ w  (or s does not fit a host int)
--------------------------------------------------
⟦Int_shift(κ, op)⟧(val_κ(a), naked_imm s; H) = undef
NOTES: `Lsl ↦ shift_left`, `Lsr ↦ shift_right_logical`, `Asr ↦ shift_right`
(`Int_ops_for_binary_shift.op`). The out-of-range case is genuine undefined
behaviour (`with_shift` comment: "assigning a semantics to an operation which
has undefined semantics"; `op_lhs_unknown` comment: "Shifting either way by
[size] or above, or by a negative amount, is undefined"). See the two
refinement notes below for what the *folder* does with it.
```

```rule
RULE P.Binary.IntShift.OutOfRange.FolderPicksZero
STATUS descriptive
CODE middle_end/flambda2/simplify/number_adjuncts.ml#with_shift
---
op ∈ {Lsl, Lsr, Asr};  s < 0 or s ≥ w(κ)  [in the folder's own cut-off]
--------------------------------------------------
⟦Int_shift(κ, op)⟧♯ picks val_κ(0)   (a legal refinement of undef)
NOTES: Descriptive of the constant folder, which is free to pick any value on
`undef` inputs and picks 0 (`with_shift ... if_undefined = zero`) — for Asr too,
not −1. The folder's cut-off is `integer_bit_width`, which is w(κ) for every
kind EXCEPT the immediates: it is **64** for `Tagged_immediate` (32 on 32-bit)
and 63/31 for `Naked_immediate`, even though both have value width W = 63/31
(`For_tagged_immediates` comment: "Note this doesn't say 31 and 63!"). So for a
tagged immediate and s ∈ {63} (64-bit) the folder calls the underlying
`Target_ocaml_int.shift_left`, itself unspecified there — see the open question.
```

```rule
RULE P.Binary.IntShift.ByZero
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift
---
op ∈ {Lsl, Lsr, Asr};  a ∈ ℤ_{w(κ)}
--------------------------------------------------
⟦Int_shift(κ, op)⟧(val_κ(a), naked_imm 0; H) = (val_κ(a), H)
NOTES: The one shift the simplifier simplifies with an unknown left operand
(`op_lhs_unknown`, `rhs = 0 → The_other_side`); also derivable from
P.Binary.IntShift since 0 ∈ [0, w).
```

### Integer comparison — `Int_comp`

`Int_comp(κ, cb)` where `cb : signed_or_unsigned comparison_behaviour` is either
`Yielding_bool c` (`c` a `comparison` with a signedness on the inequalities) or
`Yielding_int_like_compare_functions su`. The result is always a
**naked immediate** (`result_kind = K.naked_immediate`): `0`/`1` for the boolean
form, `−1`/`0`/`1` for the compare-function form. Comparison is total.

Let `cmp_s(a, b) ∈ {−1, 0, 1}` be signed comparison (compare `a`, `b` as
integers) and `cmp_u(a, b)` be unsigned comparison (compare `[a]_w`, `[b]_w`).

```rule
RULE P.Binary.IntComp.Bool
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp
CODE middle_end/flambda2/simplify/number_adjuncts.ml#compare_unsigned_generic
---
κ a Standard_int kind, w = w(κ);  a, b ∈ ℤ_w
c a comparison; ⟦c⟧(a,b) ∈ {true, false} defined by:
  Eq ↦ a = b            Neq ↦ a ≠ b
  Lt Signed   ↦ cmp_s(a,b) < 0     Lt Unsigned   ↦ cmp_u(a,b) < 0
  Gt Signed   ↦ cmp_s(a,b) > 0     Gt Unsigned   ↦ cmp_u(a,b) > 0
  Le Signed   ↦ cmp_s(a,b) ≤ 0     Le Unsigned   ↦ cmp_u(a,b) ≤ 0
  Ge Signed   ↦ cmp_s(a,b) ≥ 0     Ge Unsigned   ↦ cmp_u(a,b) ≥ 0
--------------------------------------------------
⟦Int_comp(κ, Yielding_bool c)⟧(val_κ(a), val_κ(b); H)
    = (naked_imm 1, H)   if ⟦c⟧(a,b)
    = (naked_imm 0, H)   otherwise
NOTES: Unsigned comparison compares the w-bit two's-complement patterns as
unsigned (`compare_unsigned_generic`: relies on the encoding; a negative operand
counts as larger than a non-negative one). Eq/Neq carry no signedness (bit
equality = value equality). `z3/comparisons.smt2` machine-checks that comparing
OCaml immediates and comparing their tagged/shifted representations agree.
```

```rule
RULE P.Binary.IntComp.CompareFunction
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp
---
κ a Standard_int kind, w = w(κ);  a, b ∈ ℤ_w;  su ∈ {Signed, Unsigned}
cmp = cmp_s if su = Signed, cmp_u if su = Unsigned
--------------------------------------------------
⟦Int_comp(κ, Yielding_int_like_compare_functions su)⟧(val_κ(a), val_κ(b); H)
    = (naked_imm cmp(a, b), H)
NOTES: The result is −1, 0 or 1, matching `compare`-style functions. This is the
`Int_comp` counterpart the simplifier can recover from a boolean comparison
against zero (`recover_comparison_primitive`).
```

### Float arithmetic — `Float_arith`

`binary_float_arith_op` is `Add | Sub | Mul | Div`, at `Float64` or `Float32`.
IEEE-754 arithmetic at the operation's own precision; total (no `undef` — `0./0.`
is a NaN, `x/0.` is `±∞`).

```rule
RULE P.Binary.FloatArith
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen
CODE middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics
---
bw ∈ {Float64, Float32};  𝔽 = 𝔽₆₄ if Float64 else 𝔽₃₂;  b₁, b₂ ∈ 𝔽
op ∈ {Add, Sub, Mul, Div};  fop = the IEEE_semantics operation
  Add ↦ add   Sub ↦ sub   Mul ↦ mul   Div ↦ div   (all at 𝔽's precision)
--------------------------------------------------
⟦Float_arith(bw, op)⟧(val(b₁), val(b₂); H) = (val(fop(b₁, b₂)), H)
NOTES: Total. `Float32` arithmetic is genuine single precision, not
double-then-round. Constant folding is gated on `float_const_prop ()`
(`ok_to_evaluate = propagating_float_consts`); when off, the primitive is left
in place but the denotation is unchanged. The folder's algebraic simplifications
are restricted to identities that preserve the *bit pattern* (`x*1.`, `x*(−1.)`,
`x/1.`, `x/(−1.)` — z3-checked; note `x+0.` is NOT simplified because `(−0.)+0.
= +0. ≠ −0.`).
```

### Float comparison — `Float_comp`

`Float_comp(bw, cb)` with `cb : unit comparison_behaviour` (no signedness).
Result is a **naked immediate**. The two behaviours differ sharply on NaN, so
they are separate rules.

For the *boolean* form the code uses IEEE equality and, for the inequalities,
returns `false` whenever either operand is NaN (the "unordered ⇒ false"
convention).

```rule
RULE P.Binary.FloatComp.Bool
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen
CODE middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics
---
bw ∈ {Float64, Float32};  𝔽 = 𝔽₆₄/𝔽₃₂;  b₁, b₂ ∈ 𝔽;  c a comparison (unit-signed)
--------------------------------------------------
⟦Float_comp(bw, Yielding_bool c)⟧(val(b₁), val(b₂); H) = (naked_imm r, H)
  where r = 1 if the predicate holds, else 0, and the predicate is:
    Eq  ↦ IEEE_equal(b₁, b₂)                 (NaN ≠ NaN;  +0. = −0.)
    Neq ↦ ¬ IEEE_equal(b₁, b₂)               (true if either is NaN)
    Lt/Gt/Le/Ge ↦ false                       if b₁ or b₂ is NaN
                ↦ IEEE_compare(b₁,b₂) {<,>,≤,≥} 0   otherwise
NOTES: NaN handling is explicit (`has_nan` branch): every inequality is `false`
on NaN, `Neq` is `true` on NaN, `Eq` is `false` on NaN. `IEEE_equal` is
`Stdlib.( = )` on the doubles, so it is IEEE equality, distinct from bit-pattern
equality (it equates `+0.`/`−0.` and distinguishes NaNs from everything
including themselves). When only one operand is a statically-known NaN the folder
still resolves the result (`result_of_comparison_with_nan`).
```

```rule
RULE P.Binary.FloatComp.CompareFunction
STATUS normative
CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen
CODE middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics
---
bw ∈ {Float64, Float32};  𝔽 = 𝔽₆₄/𝔽₃₂;  b₁, b₂ ∈ 𝔽
--------------------------------------------------
⟦Float_comp(bw, Yielding_int_like_compare_functions ())⟧(val(b₁), val(b₂); H)
    = (naked_imm c, H)   where c = IEEE_compare(b₁, b₂) ∈ {−1, 0, 1}
NOTES: `IEEE_compare` is `Stdlib.compare` on the doubles, a TOTAL order that
differs from the boolean comparison on NaN: it puts NaN *below* all other
values and treats NaN as equal to NaN (so `compare nan nan = 0`, `compare nan x
= −1` for non-NaN x). This is the `compare`-function behaviour (e.g. `Float.compare`),
deliberately unlike the `Yielding_bool` unordered-⇒-false convention. This is
the float counterpart recovered by `recover_comparison_primitive`.
```

---

## Effects and coeffects

Each primitive is classified by `Effects_and_coeffects.t` in
`flambda_primitive.ml`. For the scalar primitives:

| primitive | effects | coeffects | note |
|---|---|---|---|
| `Int_arith` (unary swap / binary all) | `No_effects` | `No_coeffects` | pure |
| `Int_shift`, `Int_comp` | `No_effects` | `No_coeffects` | pure |
| `Num_conv`, `Boolean_not` | `No_effects` | `No_coeffects` | pure |
| `Reinterpret_64_bit_word` | `No_effects` | `No_coeffects` | pure |
| `Tag_immediate`, `Untag_immediate` | `No_effects` | `No_coeffects` | pure |
| `Unbox_number` | `No_effects` | `No_coeffects` | pure read (contents immutable) |
| `Float_arith`, `Float_comp` | `No_effects` | `No_coeffects` or `Has_coeffects` | see below |
| `Box_number` | `Only_generative_effects Immutable` | `coeffects_of_mode` | allocates |

```rule
RULE P.Effects.PureScalars
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive
---
p ∈ { Int_arith(_, Swap_byte_endianness), Num_conv, Boolean_not,
      Reinterpret_64_bit_word, Reinterpret_boxed_vector,
      Tag_immediate, Untag_immediate, Unbox_number,
      Int_arith(_, {Add,Sub,Mul,Div,Mod,And,Or,Xor}), Int_shift, Int_comp,
      Phys_equal }
--------------------------------------------------
effects_and_coeffects(p) = (No_effects, No_coeffects, Strict,
                            Can't_move_before_any_branch)
NOTES: `no_effects_or_coeffects p` holds; these may be freely reordered,
duplicated, deleted if unused, and CSE'd. `Unbox_number` reads the heap but the
boxed contents are immutable, hence No_coeffects. (`Phys_equal` listed for
completeness; its denotation is [§06](06-primitives-memory.md).)
```

```rule
RULE P.Effects.FloatRoundingMode
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive
---
p ∈ { Float_arith(_, {Abs,Neg}) (unary),
      Float_arith(_, {Add,Sub,Mul,Div}) (binary), Float_comp }
--------------------------------------------------
effects_and_coeffects(p) =
  (No_effects, No_coeffects, Strict, Can't_move_before_any_branch)
      if Flambda_features.float_const_prop ()
  (No_effects, Has_coeffects, Strict, Can't_move_before_any_branch)
      otherwise
NOTES: Float operations read the globally-mutable FP rounding mode (changeable
only from C). When `float_const_prop ()` is false, the compiler must assume the
rounding mode may change, so float ops get `Has_coeffects` to prevent them being
moved across an effectful operation (e.g. a C stub that changes rounding). This
is also why constant folding of float primitives is gated on the same flag.
```

```rule
RULE P.Effects.BoxNumber
STATUS normative
CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
---
κ a Boxable_number kind;  a an Alloc_mode.For_allocations.t
--------------------------------------------------
effects_and_coeffects(Box_number(κ, a)) =
  (Only_generative_effects Immutable, coeffects_of_mode(a), placement, …)
NOTES: `only_generative_effects` holds: a `Box_number` whose result is unused may
be deleted, but it may not be duplicated freely across a state change the way a
pure primitive may. `coeffects_of_mode a` is `Has_coeffects` for a local
allocation (so it cannot be hoisted out of / past its region) and `No_coeffects`
for heap. `placement` is `Delay` for heap allocations in classic mode (to inline
the box into to_cmm) and `Strict` for local allocations. This is the `H′ ≠ H`
component of P.Unary.BoxNumber.
```

---

## Rule index for this chapter

Unary: `P.Unary.IntArith.SwapByteEndianness`, `P.Unary.FloatArith`,
`P.Unary.NumConv`, `P.Unary.NumConv.FloatToInt.OutOfRange`,
`P.Unary.NumConv.Int32ToInt64.SignExtend`, `P.Unary.BooleanNot`,
`P.Unary.TagImmediate`, `P.Unary.UntagImmediate`,
`P.Unary.Reinterpret64.Int64AsFloat64`, `P.Unary.Reinterpret64.Float64AsInt64`,
`P.Unary.Reinterpret64.Int64AsTaggedInt63`,
`P.Unary.Reinterpret64.TaggedInt63AsInt64`, `P.Unary.BoxNumber`,
`P.Unary.UnboxNumber`.

Binary: `P.Binary.IntArith.Total`, `P.Binary.IntArith.DivMod`,
`P.Binary.IntArith.DivModByZero`, `P.Binary.IntArith.DivMinIntByMinusOne`,
`P.Binary.IntShift`, `P.Binary.IntShift.OutOfRange.FolderPicksZero`,
`P.Binary.IntShift.ByZero`, `P.Binary.IntComp.Bool`,
`P.Binary.IntComp.CompareFunction`, `P.Binary.FloatArith`,
`P.Binary.FloatComp.Bool`, `P.Binary.FloatComp.CompareFunction`.

Contract & effects: `P.Contract.NoRaiseNoControl`, `P.Effects.PureScalars`,
`P.Effects.FloatRoundingMode`, `P.Effects.BoxNumber`.

## Open questions

1. **`min_int / −1` (P.Binary.IntArith.DivMinIntByMinusOne).** *Resolved.*
   Compile-time folding via the host `Stdlib`/`Int32`/`Int64` division returns
   `min_int` without trapping. A hardware `idiv` would trap on x86, but the
   backend never emits a bare `idiv` for this case: `cmm_helpers.div_int`/
   `mod_int` special-case a `−1` divisor, and `make_safe_divmod` inserts a
   runtime `−1` test guarded by `Arch.division_crashes_on_overflow`, forcing
   `x / −1 = −x` and `x mod −1 = 0` (PR#5513). Generated code therefore agrees
   with the folded value; the rule is now `normative`, not a soundness gap.

2. **Tagged-immediate shift cut-off of 64
   (P.Binary.IntShift.OutOfRange.FolderPicksZero).** `For_tagged_immediates`
   uses `integer_bit_width = 64` (not 63) as the folder's undefined-shift
   threshold, while the value width is `W = 63`. For a shift count of exactly 63
   on a 64-bit target the folder therefore calls `Target_ocaml_int.shift_left`,
   which the `mli` declares "unspecified" for `y ≥ 63`. The denotation treats
   `s ≥ 63` as `undef`, so this is a legal (if surprising) refinement, but the
   intent behind the `64` is unclear and worth confirming against the backend.

3. **`int64`/`nativeint` → `float32` double-rounding (P.Unary.NumConv).** The
   folder routes integer → float32 through a double
   (`Float32.create (Int64.to_float a)`), which can double-round for values that
   are not exactly representable. Whether the backend does the same single- vs.
   double-rounding is unverified.

4. **Naked-immediate arithmetic width.** `number_adjuncts.ml` carries a CR
   (vlaviron) questioning what bit width naked-immediate arithmetic should use,
   suggesting arbitrary arithmetic on naked immediates should perhaps not happen
   at all. The rules above assume naked immediates behave as `ℤ_W` like tagged
   immediates; if the frontend never emits such arithmetic this is moot, but it
   is not enforced.
