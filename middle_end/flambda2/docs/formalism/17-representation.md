# to_cmm, Stage 2: the representation relation `≈`

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter defines the **representation relation** `≈` linking the *abstract*
runtime values and heap objects of [`04`](04-opsem.md)/[`06`](06-primitives-memory.md)
— where `tagged_imm n`, `ptr ℓ`, and `Block(t, μ, v̄)` are opaque and word size is
symbolic — to the *concrete* Cmm machine words and byte memory of
[`15`](15-cmm.md). `≈` is the heart of the whole `to_cmm` formalization: it is
where the representation Flambda keeps abstract is committed, and where soundness
bugs of the `float32_double_round` family ([`13`](13-soundness.md) §4.7) live.

`≈` is *stated* here; the translation rules of [`18`](18-to-cmm-data.md) are each
obligated to *preserve* it, and the whole-program simulation
([`20`](20-to-cmm-soundness.md)) is stated in terms of it. Concrete allocation,
regions, and the moving GC — which extend and maintain `≈` at run time — are
[`19`](19-cmm-memory-gc.md).

Everything here is **64-bit little-endian** ([`15`](15-cmm.md) §0). The constants
(word width 8, header shifts, `pos_arity_in_closinfo = 56`, sign-extension of
`int32`) are those the compiler computes from `Arch`/`Config`; the prose marks
where a target-parameterized model would abstract them.

## 1. Shape of the relation

The abstract heap `H` uses opaque locations `ℓ` and symbols `sym`
([`04`](04-opsem.md) §1.3). To relate it to byte memory we introduce a
**location map**

```
L : (Loc ⊎ Symbol) ⇀ Addr        -- base address of each abstract object; Addr = word
```

`L(ℓ)` is the address of *field 0* of the object at `ℓ`; its header sits at
`L(ℓ) − 8`. `L` is an existential witness: `H ≈_L M` asserts there *exists* such
an assignment laying every abstract object out in `M`. The GC may change `L` (by
relocation) while preserving `H ≈_L M` ([`19`](19-cmm-memory-gc.md)); this is why
`L` is not fixed and why observations are compared *up to* `≈` rather than by
byte equality.

Three judgments:

```
v ≈ᵥ w             -- abstract value v is represented by Cmm value w (under an implicit L)
H ⊢ o @ a ≈ₒ M     -- object o, based at address a, is laid out in memory M
H ≈_L M            -- whole-heap relation (every object of H laid out per L in M)
```

## 2. Values

```rule
RULE R.Val.Imm
STATUS normative
CODE backend/cmm_helpers.ml#tag_int
CODE backend/cmm_helpers.ml#untag_int
CODE middle_end/flambda2/kinds/flambda_kind.ml#t
---
tagged_imm n  ≈ᵥ  word (2·n + 1)         -- kind Value: OCaml int, tag bit set
naked_imm  n  ≈ᵥ  word n                 -- kind Naked_immediate: untagged (switch scrutinee, tag)
--------------------------------------------------
The tagging convention is 2n+1 (tag_int: (n≪1)+1; untag_int: asr 1). n ranges over
the target int range (63-bit for tagged_imm on a 64-bit target).
NOTES: The single tag bit is word-size-independent; only the value range (63 bits
here) depends on word width. `naked_imm` is the *same* integer untagged; it
appears as a Switch scrutinee (OS.Switch) and as the result of Is_int/Get_tag.
The 63-vs-31-bit range is the seam a target-parameterized model would open.
```

```rule
RULE R.Val.NakedNumber
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unbox_number
CODE backend/cmm_helpers.ml#sign_extend
CODE backend/cmm.mli#machtype_component
---
naked_int64 n     ≈ᵥ  word n
naked_nativeint n ≈ᵥ  word n
naked_int32 n     ≈ᵥ  word (sign_extend₃₂ n)      -- kept sign-extended in the register
naked_int8 n      ≈ᵥ  word (sign_extend₈ n)
naked_int16 n     ≈ᵥ  word (sign_extend₁₆ n)
naked_float f     ≈ᵥ  flt f
naked_float32 f   ≈ᵥ  flt32 f                     -- in a Float32 register
--------------------------------------------------
Naked numbers are unboxed machine values (kind Naked_number nnk).
NOTES: TARGET-DEPENDENT. On a 64-bit target int32 lives in a 64-bit register held
*sign-extended* into the low 32 bits' range (to_cmm inserts sign_extend after each
int32 op; ch. 18). int8/int16 likewise. nativeint and int64 are full 64-bit words
(they coincide on this target; they would differ at 32-bit, where int64 is split —
deferred). `flt32` in a register is the Float32 machtype; when stored it becomes a
`Single` chunk (CM.Mem.LoadStore).
```

```rule
RULE R.Val.Pointer
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load
CODE backend/cmm_helpers.ml#field_address
---
ptr a      ≈ᵥ  word (L(a))                    -- a Val-typed pointer to field 0 of the object at a
null       ≈ᵥ  word 0                          -- the null pointer (for `_ or_null`)
--------------------------------------------------
A pointer value is the base address of its object (header at L(a)−8). Field i of a
uniform block is at L(a) + 8·i (field_address, stride = size_addr = 8).
NOTES: `ptr sym` uses L(sym) = the symbol's link-time address (Cconst_symbol). The
machtype is `Val` (a GC root). Derived pointers into the middle of a block (from
`Cadda`) are `Addr`-typed and are NOT related by ≈ᵥ to any abstract value — they
are transient and must not survive allocation (ch. 19).
```

```rule
RULE R.Val.Clos
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout
CODE backend/cmm_helpers.ml#infix_field_address
---
clos ℓ f  ≈ᵥ  word (L(ℓ) + 8·off(f))
where off(f) is the function-slot offset of f in the closure block's layout
(Exported_offsets / Slot_offsets.Layout).  off(f₀) = 0 for the first slot;
off(f) > 0 for a sibling slot, whose word at L(ℓ)+8·off(f)−8 is an infix header.
--------------------------------------------------
A closure value points at its function slot within the (shared) set-of-closures
block. For the first slot this is the block base (a Val pointer); for a sibling it
is an interior pointer legitimized by the infix header just before it (R.Obj.Closures).
NOTES: Move_within_set_of_closures produces clos ℓ f′ = L(ℓ)+8·off(f′), a
`Caddv`/`infix_field_address` adjustment by (off(f′)−off(f)) words from clos ℓ f
(ch. 18). The offsets are assigned by Slot_offsets and treated as given here
(as opaquely as 07 §6 treats them).
```

## 3. Header encoding

Every heap block is preceded by a one-word header. `to_cmm` builds it with
`cmm_helpers.ml#block_header` and friends.

```rule
RULE R.Header
STATUS normative
CODE backend/cmm_helpers.ml#block_header
CODE backend/cmm_helpers.ml#Mixed_block_support
CODE backend/cmm_helpers.ml#caml_black
---
The header word for a block of tag t, size sz (in words, not counting the header),
GC color col, and mixed scannable-prefix p is:
  hdr(t, sz, col, p) = (prefix_field ≪ 56) │ (sz ≪ 10) │ (col ≪ 8) │ t
where t occupies bits 0–7, col bits 8–9 (caml_black = caml_local = 3≪8), sz bits
10–55, and prefix_field = 0 for a non-mixed block, = p+1 for a mixed block with
scannable prefix p (bits 56–63; 0 means "not mixed").
--------------------------------------------------
This is the concrete meaning of the header word at L(ℓ)−8.
NOTES: 64-BIT-SPECIFIC. block_header = (sz≪10)+t; color OR'd in
(black_block_header / local_block_header); mixed via Mixed_block_support.make_header
= ((p+1) ≪ (64−8)) + hdr (reserved_header_bits = 8, addr_size_bits = 64). Static
"value" data is marked black (caml_black); local blocks carry caml_local (same bit
pattern, different meaning by context). `Get_header`/`Get_tag`/(array) length read
this word (06: P.Unary.GetHeader/GetTag were "target-specific, not modelled
precisely" — R.Header is that model). Tag names (closure_tag, infix_tag,
double_tag, double_array_tag, custom_tag, string_tag) are Obj.* constants.
```

## 4. Heap objects

Each rule says how object `o` based at address `a` occupies memory: the header at
`a−8` and the fields at their offsets. Write `M[a] = w` for "the word at byte
address `a` is `w`".

```rule
RULE R.Obj.Block
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
CODE backend/cmm_helpers.ml#make_alloc_generic
---
H ⊢ Block(t, μ, [v₀ … v_{n−1}]) @ a ≈ₒ M    iff
  M[a−8] = hdr(t, n, col(μ), 0)               -- col black if static, else white; caml_local if local
  and for each i: M[a + 8·i] = wᵢ with vᵢ ≈ᵥ wᵢ   (each field one Word_val/Word_int)
--------------------------------------------------
A scannable block: header then n one-word fields.
NOTES: make_block → make_alloc (all fields Word_val) → make_alloc_generic
(Cconst_natint header :: field words, or caml_alloc_shr_check_gc + fill_fields for
large blocks). Field i at offset 8·i (field_address, stride 8). Value fields are
Word_val (GC-scanned); immediate fields Word_int. Cmm image of P.Variadic.MakeBlock.Values
/ P.Unary.BlockLoad(06).
```

```rule
RULE R.Obj.FloatBlock
STATUS normative
CODE backend/cmm_helpers.ml#float_header
CODE backend/cmm_helpers.ml#make_float_alloc
CODE backend/cmm_helpers.ml#floatarray_header
---
H ⊢ FloatBlock(μ, [f₀ … f_{n−1}]) @ a ≈ₒ M    iff
  M[a−8] = hdr(double_array_tag, n, col(μ), 0)         -- n = n·(size_float/size_addr) = n words
  and for each i: the 8 bytes at a + 8·i decode (Double) to fᵢ
--------------------------------------------------
A naked-float block / float array: Double_array_tag, one 8-byte double per element.
NOTES: make_float_alloc; floatarray_header uses len·size_float/size_addr = len words
(size_float = size_addr = 8). Zero-length float arrays get tag 0, size 0
(floatarray_header). Cmm image of P.Variadic.MakeBlock.NakedFloats.
```

```rule
RULE R.Obj.MixedBlock
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
CODE backend/cmm_helpers.ml#make_mixed_alloc
CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words
---
H ⊢ MixedBlock(t, μ, σ, [v₀ … v_{k−1}]) @ a ≈ₒ M    iff
  M[a−8] = hdr(t, size_in_words(σ), col(μ), value_prefix_size(σ))
  and for each logical field j: the bytes at a + 8·offset_in_words(σ, j), of width
  chunk(σ, j), represent vⱼ (Word_val/Word_int for the value prefix; Double / Single /
  Byte_signed / Sixteen_signed / Thirtytwo_signed / Word_int for flat-suffix elements)
--------------------------------------------------
A mixed block: value prefix (scanned) then a flat suffix of unboxed elements, packed
by the shape.
NOTES: make_mixed_alloc; the physical word offset of a logical field is
Flambda_kind.Mixed_block_shape.offset_in_words (the seam P.MixedShape.Offset of 06/03,
now given concrete meaning). Flat-suffix chunk widths from memory_chunk_of_flat_suffix_element
(ch. 18). LITTLE-ENDIAN-ONLY for sub-word flat fields (get_field_unboxed fatal-errors
on big-endian; ch. 18). Cmm image of P.Variadic.MakeBlock.Mixed / P.Unary.BlockLoad.Mixed.
```

```rule
RULE R.Obj.Array
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
CODE backend/cmm_helpers.ml#array_indexing
CODE backend/cmm_helpers.ml#Unboxed_or_untagged_array_tags
CODE backend/cmm_helpers.ml#unboxed_or_untagged_packed_array_length
---
H ⊢ Array(ak, μ, [v₀ … v_{n−1}]) @ a ≈ₒ M    iff
  M[a−8] = hdr(tag(ak, n), size_words(ak, n), col(μ), prefix(ak))
  and element i occupies width w(ak) bytes at a + w(ak)·i, representing vᵢ
where, per array kind:
  Values / Gc_ignorable immediates → w = 8, tag = 0, prefix = 0 (Word_val / Word_int);
  Naked_floats → w = 8, tag = double_array_tag, prefix = 0 (Double), size = n words;
  Naked_int64 / nativeint → w = 8, prefix = 1;
  Naked_int32 / float32 → w = 4, prefix = 1 (packed 2 per word);
  Naked_int8 / int16 → w = 1 / 2, prefix = 1 (packed);
  vec128/256/512 → w = 16/32/64, prefix = 1;
  unboxed products → the n·m component scalars packed.
For the PACKED/unboxed kinds the header tag is LENGTH-DEPENDENT — a base tag plus
the last-word padding (k − n mod k) mod k (the number of unused element slots in the
final word; 0 when k divides n) (int32#/float32#: k = 2; int16#: k = 4; int8#: k = 8)
— and prefix = 1
(a MIXED header with scannable_prefix 0, so the header's top byte reads p+1 = 1).
The GC-scanned value/float arrays use a plain (non-mixed) header with prefix = 0.
--------------------------------------------------
An array; element addressing is a + (element_size · index) (array_indexing scales by
the element log2 size; it also folds the tagged-index untagging).
NOTES: array_indexing expects a *tagged* index and folds the ≫1 into the scale
(ch. 18). Tags and headers for packed / unboxed / untagged arrays come from
`Cmm_helpers.Unboxed_or_untagged_array_tags` (the shared array-tag source of truth,
also used by to_jsir): the element-count PARITY lives in the TAG, not a separate
count field, and the length is recovered as (size ≪ mod_log2) − (tag & (2^mod_log2 −
1)) by `unboxed_or_untagged_packed_array_length`. These arrays carry a mixed-block
header (scannable_prefix 0), witnessed e.g. as a 3-element int32# array with header
top byte 1, tag 3 = base 2 + (3 mod 2). Cmm image of P.Variadic.MakeArray /
P.Binary.ArrayLoad / P.Ternary.ArraySet.
```

```rule
RULE R.Obj.Bytes
STATUS normative
CODE backend/cmm_helpers.ml#string_header
---
H ⊢ Bytes(μ, b̄) @ a ≈ₒ M    iff  |b̄| = len,
  M[a−8] = hdr(string_tag, (len + 8)/8, col(μ), 0)
  and the bytes b̄ occupy a … a+len−1, followed by padding; the final byte of the
  last word holds (8·⌈(len+1)/8⌉ − len − 1), the padding count (OCaml string layout)
--------------------------------------------------
A string/bytes value: string_tag, (len+size_addr)/size_addr words, trailing padding
byte encodes the length modulo the word.
NOTES: string_header len = (len + size_addr)/size_addr words. Bigstrings
(Bigstring(b̄)) are off-heap: the block holds a data pointer read before indexing
(ch. 18, add_int_ptr with ptr_out_of_heap); the bytes are in malloc'd memory not
under L. String/bytes loads decode w-byte little-endian scalars (06,
P.Binary.StringOrBigstringLoad). LITTLE-ENDIAN baked in.
```

```rule
RULE R.Obj.Bigarray
STATUS normative
CODE backend/cmm_helpers.ml#bigarray_load
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
VERIFIED 14-validation/bigarray_access.md
---
H ⊢ Bigarray(bk, layout, [d₁ … dₙ], ē) @ a ≈ₒ M    iff
  M[a−8] = hdr(custom_tag, size, col, 0)
  M[a]      = the bigarray custom-operations pointer
  M[a+8]    = data, a pointer to off-heap element storage (not under L)
  M[a+16] = n (num_dims)      M[a+24] = flags (encoding bk and layout)
  M[a+32] = proxy
  M[a + 8·(4+d)] = d_d    for 1 ≤ d ≤ n
  and element ē[j] occupies the elt_size(bk) bytes at data + j·elt_size(bk)
  (little-endian; complex elements store re then im, each elt_size/2 bytes)
--------------------------------------------------
A bigarray: a custom block whose field 1 is the off-heap data pointer, with
dimension d at field 4+d; elements are addressed off the data pointer, scaled
by elt_size(bk).
NOTES: The block mirrors struct caml_ba_array (runtime/caml/bigarray.h) after
the ops word: data, num_dims, flags, proxy, dim[]. Bigarray_length {dimension=d}
lowers to a Word_int Mutable load of field 4+d (ch. 18 TC.Prim.BigarrayLength);
element access loads the data pointer from field 1 then indexes off-heap
(TC.Prim.BigarrayAccess). A Bigstring(b̄) is exactly a 1-d Uint8 bigarray under
this layout, which is why bigstring bounds checks read Bigarray_length
{dimension = 1} (06, P.Binary.BigarrayGetAlignment NOTES). LITTLE-ENDIAN baked
in.
```

```rule
RULE R.Obj.Boxed
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number
CODE backend/cmm_helpers.ml#float_header
CODE backend/cmm_helpers.ml#boxedint64_header
---
H ⊢ Boxed(κ, c) @ a ≈ₒ M    iff, by κ:
  Naked_float:      M[a−8] = hdr(double_tag, 1, col, 0);  the 8 bytes at a decode to c
  Naked_float32:    M[a−8] = hdr(custom_tag, 2, col, 0);  M[a] = &caml_float32_ops;
                      the 4 bytes at a+8 decode (Single) to c
  Naked_int32:      M[a−8] = hdr(custom_tag, 2, col, 0);  M[a] = &caml_int32_ops;
                      the value at a+8 is c (big-endian box stores c≪32; little-endian
                      stores sign_extend₃₂ c — target endianness, here LE)
  Naked_int64:      M[a−8] = hdr(custom_tag, 2, col, 0);  M[a] = &caml_int64_ops;  M[a+8] = c
  Naked_nativeint:  M[a−8] = hdr(custom_tag, 2, col, 0);  M[a] = &caml_nativeint_ops; M[a+8] = c
--------------------------------------------------
A boxed number: a double is a bare double_tag block; the others are custom blocks
whose first word is the operations pointer and whose payload follows.
NOTES: box_number → box_float / box_int_gen / box_float32. float_header =
double_tag size size_float/size_addr = 1. The custom-block headers are custom_tag
size 2 (boxedint64_header = 1 + 8/size_addr = 2 on 64-bit). unbox loads at offset
size_addr (a+8), skipping the ops word (ch. 18). ENDIANNESS-DEPENDENT for int32
(box_int_gen: big-endian stores in the high half). Cmm image of P.Unary.BoxNumber /
UnboxNumber(06).
```

```rule
RULE R.Obj.Closures
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
CODE backend/cmm_helpers.ml#pack_closure_info
CODE backend/cmm_helpers.ml#infix_header
CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout
---
H ⊢ Closures(funs, env) @ a ≈ₒ M    iff
  M[a−8] = hdr(closure_tag, total_size, col, 0)
  and for each function slot f at offset off(f):
    if off(f) > 0:  M[a + 8·off(f) − 8] = infix_header(off(f))       -- infix tag + slot offset
    for a 2-word slot (Full_application_only; Curried arity ≤ 1):
      M[a + 8·off(f)]     = the code pointer for funs(f)
      M[a + 8·off(f) + 8] = pack_closure_info(arity, startenv − off(f), is_last)
    for a 3-word slot (Full_and_partial_application; Curried arity ≥ 2 or Tupled):
      M[a + 8·off(f)]      = the generic partial-application entry (caml_curryN via
                             C.curry_function_sym, or C.fail_if_called_indirectly_sym
                             when only_full_applications)
      M[a + 8·off(f) + 8]  = pack_closure_info(arity, startenv − off(f), is_last)
      M[a + 8·off(f) + 16] = the code pointer for funs(f)
  and for each value slot w (Slot_offsets.Layout carries an is_scanned flag):
    if w is scanned:    off(w) ≥ startenv (in the GC-scanned environment),
                        M[a + 8·off(w)] = a Word_val representing env(w)
    if w is unscanned:  off(w) < startenv (below the scanned env, in the region the
                        GC skips), M[a + 8·off(w)] = a word representing env(w)
                        (a known immediate or naked number, per the slot's kind)
where pack_closure_info(ar, se, last) = (ar ≪ 56) │ (last ≪ 55) │ (se ≪ 1) │ 1.
--------------------------------------------------
A set-of-closures block: closure_tag; function slots (code pointer + packed closinfo
word, sibling slots preceded by an infix header); then, from startenv on, the
GC-scanned shared environment. Unscanned captures (immediates / naked numbers) sit
BELOW startenv, interleaved with / after the function slots.
NOTES: pos_arity_in_closinfo = 8·size_addr − 8 = 56 (arity in the top 8 bits, signed;
tupled functions negative). startenv is where the SCANNED environment begins: the GC
scans only value slots at offsets ≥ startenv. Everything below startenv — the
function-slot region AND any UNSCANNED value slots (Slot_offsets `Value_slot
{is_scanned = false}`: known-immediate or naked-number captures) — is skipped by the
GC, which is why closures need no mixed-block header (cmm_helpers `t = Regular_block`
comment). This scanned/unscanned split is essential: a captured `int` sits below
startenv, so the naive "all value slots ≥ startenv" is FALSE (witnessed:
tocmm-06-closure-unscanned). startenv = last_offset+size when there are no scanned
slots. Slot offsets from Slot_offsets / Exported_offsets, assigned once across the
unit (to_cmm.md, closure offsets; 07 §6). Cmm image of OS.Let.SetOfClosures /
P.Unary.ProjectFunctionSlot / ProjectValueSlot(06). Exact per-arity slot width
(2- vs 3-word function slots) is as assigned by Slot_offsets and not re-derived here.
The 2-word layout has the funs(f) code pointer at word 0; the 3-word layout puts the
generic curry/apply entry at word 0 and the funs(f) code pointer at word 2, with the
closinfo word at +8 in both (to_cmm_set_of_closures `fill_slot`, `List.rev acc`). All
existing closure case studies use 2-word slots; a 3-word closure case study (Curried
arity ≥ 2 or Tupled) is still needed to witness the word-2 code pointer.
```

`Code(cid ↦ code)` objects are not heap-represented as data: code becomes a
`Cfunction` phrase (`cmm.mli#fundecl`), reached by its code symbol; a `clos ℓ f`'s
code pointer (R.Obj.Closures) is the address of that symbol.

## 5. Whole heap and observation

```rule
RULE R.Heap
STATUS conjectured
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
---
H ≈_L M    iff there is a location map L : dom(H) ⇀ Addr, injective on distinct
live objects, such that for every ℓ ∈ dom(H) with a heap object o = H(ℓ):
  H ⊢ o @ L(ℓ) ≈ₒ M     (§4),
every symbol sym ∈ dom(H) has L(sym) = its link-time address, and pointer fields
resolve consistently (if a field of one object is ptr ℓ′, its stored word is L(ℓ′)).
--------------------------------------------------
The whole-heap relation: every abstract object is laid out somewhere in M per its
object rule, with pointers threaded through L.
NOTES: STATUS conjectured — this is the invariant the translation and the GC must
maintain; validated by the tocmm-* case studies (14-validation) checking the -dcmm
byte layout against §4. L is existential and mutable: the GC may relocate objects,
changing L but preserving H ≈_L M (ch. 19, CM.Alloc.GC).
```

```rule
RULE R.Observe
STATUS normative
CODE middle_end/flambda2/terms/flambda_unit.mli#module_symbol
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
---
The Flambda observation of a normal run — the module block value H(sym_mod) plus the
C-call effect trace (OS.Unit.Final) — is related to the Cmm observation — the byte
image reachable from the module symbol in M plus the Cextern trace (CM.Unit.Final) —
iff  H(sym_mod) @ L(sym_mod) ≈ₒ M  (transitively through pointer fields) and the two
effect traces are equal.
--------------------------------------------------
Observations are compared THROUGH ≈, not by byte equality.
NOTES: This is what lets the simulation theorem (ch. 20) equate a
representation-abstract Flambda observation with a concrete Cmm byte image. The
C-call trace is literally equal (the same axiomatized Cextern; OS.Apply.CCall ≙
CM.Extcall).
```

## 6. The target seam

Every `8`, the `≪10`/`≪56` header shifts, `pos_arity_in_closinfo = 56`, and the
`sign_extend₃₂` of `int32` are 64-bit-little-endian facts. A target-parameterized
model would replace `8` by `size_addr`, the header shifts by
`Config.reserved_header_bits`-derived quantities, and would make the int32 box
endianness-conditional (`R.Obj.Boxed` already notes the big-endian variant, which
the code implements but no shipped target exercises). Word width is the only axis
that a real 32-bit target (aspirational, PR #685) would change materially, and it
is *pervasive* here — which is precisely why fixing 64-bit keeps the chapter
faithful to the code that exists ([`01`](01-overview.md); [`15`](15-cmm.md) §0).

## 7. Summary of rules

Values: `R.Val.Imm`, `R.Val.NakedNumber`, `R.Val.Pointer`, `R.Val.Clos`.

Headers/objects: `R.Header`, `R.Obj.Block`, `R.Obj.FloatBlock`, `R.Obj.MixedBlock`,
`R.Obj.Array`, `R.Obj.Bytes`, `R.Obj.Bigarray`, `R.Obj.Boxed`, `R.Obj.Closures`.

Whole-heap/observation: `R.Heap`, `R.Observe`.
