# to_cmm, Stage 2: data and primitive lowering

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter formalizes the **data** half of the `to_cmm` translation: how
`Let`-bindings, `Simple`s, and every in-scope primitive
([`05`](05-primitives-scalar.md), [`06`](06-primitives-memory.md)) become Cmm
that computes on concrete words and memory. Each rule carries a **correctness
obligation**: the emitted Cmm *commutes with the representation relation `≈`*
([`17`](17-representation.md)). This is where the representation is committed and
where soundness bugs live; the int→float32 double-rounding of
[`13`](13-soundness.md) §4.7 is diagnosed precisely here.

It owns the pieces [`16`](16-to-cmm-control.md) deferred: the simple/variable
judgment `⤳ᵥ`, the `Let`-form translation, and the delayed-binding
("let-substitution") machinery `D` with its correctness. Concrete allocation and
the GC (`Calloc`, regions) are [`19`](19-cmm-memory-gc.md); layouts are
[`17`](17-representation.md).

## 1. Simples and the correctness schema

```rule
RULE TC.Simple
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#simple
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple
---
Θ ⊢ x ⤳ᵥ V(x)          -- a variable resolves to its (possibly delayed) Cmm expr
Θ ⊢ sym ⤳ᵥ Cconst_symbol sym
Θ ⊢ c ⤳ᵥ (the Cmm constant for c)   -- tagged_imm n ↦ Cconst_int (2n+1); naked ↦ Cconst_*
Θ ⊢ (s @ co) ⤳ᵥ (Θ ⊢ s ⤳ᵥ ·)        -- coercions erased
--------------------------------------------------
For every simple s and value v = ⟦s⟧ρ (OS.Simple.Eval), if ρ ≈ ce (each ρ(x) ≈ᵥ
ce-value of V(x)) then Θ ⊢ s ⤳ᵥ ce implies v ≈ᵥ (value of ce in ce, M).
NOTES: A variable maps to a Cmm expression that is either a `Cvar` or, if the
binding was delayed for substitution (§2), the defining expression itself
(bind_var_to_simple / add_alias). Constants use the R.Val.* encodings (17):
tagged immediates get `Cconst_int (2n+1)`, naked numbers the corresponding
`Cconst_*`. Coercions are identity at runtime (OS.Simple.Eval) and erased.
```

```rule
RULE TC.Prim.Sound
STATUS conjectured
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_complex
CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
---
For every primitive p, let ⟦p⟧ be its denotation (05/06) and let `TC.Prim.<p>`
give its Cmm emission e_p (below). The obligation: whenever the argument simples
translate to Cmm values that ≈ᵥ the abstract argument values, and H ≈_L M, then
evaluating e_p (CM.* rules, 15) yields a Cmm value w and memory M′ with
  ⟦p⟧(v̄; H) = (v, H′)  ⟹  v ≈ᵥ w  and  H′ ≈_{L′} M′  (for some L′ extending L)
and  ⟦p⟧(v̄; H) = undef  ⟹  e_p is undef or unconstrained (the "modulo UB" clause, 13).
--------------------------------------------------
Every per-primitive rule below is an instance of this schema; soundness of to_cmm
(20) composes them along the translation.
NOTES: This is the data-side analogue of INV.Rewrite.Local (13 §2): the seam where
the term semantics (05/06) and the concrete representation (17) must agree. The
two coupling points are (i) denotational agreement — the Cmm arithmetic equals
⟦p⟧ (the float32 concern, TC.Prim.NumConv); (ii) layout agreement — loads/stores
read/write the bytes ≈ says are there (TC.Prim.BlockLoad/Set etc.).
```

## 2. Let-bindings and let-substitution

`to_cmm` does not emit a Cmm `Clet` for every Flambda `Let`. Pure and
used-once bindings are *delayed* in `D` and substituted at the use site (or
dropped if unused); this is the most important `to_cmm` optimization and the
subtlest correctness argument (to_cmm.md, "Let-binding substitution").

```rule
RULE TC.Let.Simple
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#bind_var_to_simple
---
e = Let (x = Simple s) e_body
Θ′ = Θ with V[x ↦ (Θ ⊢ s ⤳ᵥ ·)]   (an alias; add_alias if s is a var)
Θ′ ⊢ e_body ⤳ e_c
--------------------------------------------------
Θ ⊢ e ⤳ e_c
NOTES: A let-of-simple binds x to the simple's translation in V; no Cmm is
emitted (the value is substituted at uses). Matches OS.Let.Simple.
```

```rule
RULE TC.Let.Prim
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_prim
CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding
---
e = Let (x = Prim (p, dbg)) e_body
classify_let_binding x ~num_occurrences ~effects_and_coeffects(p) selects:
  Drop_defining_expr / Regular       ⟹  bind x's translation in D, Do_not_inline
  May_inline_once                    ⟹  delay x's defining expr, substitutable once
  Must_inline_once / Must_inline_and_duplicate ⟹  delay (complex) defining expr
Θ′ = Θ extended accordingly (bind_variable_to_primitive);   Θ′ ⊢ e_body ⤳ e_c
--------------------------------------------------
Θ ⊢ e ⤳ e_c
NOTES: The emission of the primitive itself is `TC.Prim.<p>` (§3–§7), producing
the defining Cmm expression; whether it is inlined at the use site or bound by a
Cmm `Clet` at flush time is decided by classify_let_binding (Drop/Regular/
May_inline_once/Must_inline_once/Must_inline_and_duplicate). A dropped pure
binding is still translated so its own arguments can be inlined
(let_prim comment). Matches OS.Let.Prim.Pure / OS.Let.Prim.Effect.
```

```rule
RULE TC.Let.Subst
STATUS conjectured
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#flush_delayed_lets
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#bind_variable
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_binding_to_env
CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
---
Delayed bindings in D are held in TWO parallel "stage" stacks (add_binding_to_env
threads a binding onto both): an effect_stages stack — a stage is a set of
co-effect-only bindings, or one effectful binding — and a validity_stages stack,
whose stages are `Depend_on_control_flow` (a set of bindings that must not be
hoisted above a preceding branch) or `Control_flow_point` (a barrier). Substituting
a delayed binding at its use site, dropping an unused pure binding, and flushing D
to Cmm `Clet`s at branch/loop/apply points all preserve observable behaviour,
PROVIDED the reordering respects the effects/coeffects/placement/VALIDITY quadruple
(06, P.Effects.*, in particular the normative P.Effects.Validity): a coeffect-only
binding may not cross a write; an effectful binding is a barrier and is placed
exactly once; a Can't_move_before_any_branch binding (which includes essentially
all pure memory reads and tagged-int ops — a pure binding is off effect_stages but
ON validity_stages) must not be hoisted above a preceding branch; a
Control_flow_point is a hard barrier that nothing crosses; nothing substitutable
crosses into a recursive continuation.
--------------------------------------------------
The delayed-binding discipline is a behaviour-preserving reordering of the
evaluation of Let-bound primitives.
NOTES: A No_effects/No_coeffects binding is NOT free to move arbitrarily — it is
still validity-constrained (validity.mli: moving a tagged-int op before a GADT
match can produce garbage we mistake for a legal OCaml value, causing a segfault);
this is why the code keeps a separate validity_stages stack (add_to_validity_stages)
keyed on Ece.validity alongside effect_stages (add_to_effect_stages), and
can_substitute requires respecting BOTH. The substitution-time validity check is
gated by the `cmm_safe_subst` flag (off by default), but validity_stages
maintenance and flush-time validity branching are unconditional.
STATUS conjectured — this is the one genuinely subtle Stage-2 argument, and
the part of to_cmm "most sensitive to changes" (to_cmm.md). It reuses the
effects/coeffects theory the abstract semantics already has (06); the flush modes
(Flush_everything / Branching_point / Entering_loop) are where the environment is
made linear so no binding is duplicated or dropped incorrectly (the flush side
conditions cited throughout ch. 16). The End_region hack (let_expr0) forcibly
flushes to stop immutable loads of locally-allocated blocks from moving past an
End_region — a known imprecision of the effects model (19).
```

```rule
RULE TC.Let.SetOfClosures
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#let_dynamic_set_of_closures
---
e = Let (Set_of_closures bound_vars = Set_of_closures (soc, am)) e_body
--------------------------------------------------
Θ ⊢ e ⤳ (allocate the closure block per R.Obj.Closures, binding each bound var to
its clos-value; then Θ′ ⊢ e_body ⤳ ·)
NOTES: A dynamic set of closures becomes a `Calloc` (or the large-block path) of a
closure_tag block laid out as R.Obj.Closures — function slots (code ptr + closinfo,
infix headers) then the captured environment. `am` (Heap/Local) selects heap vs
region allocation (19). Cmm image of OS.Let.SetOfClosures. Offsets from
Slot_offsets (17, R.Val.Clos / R.Obj.Closures).
```

```rule
RULE TC.Let.Static
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_static.ml#static_consts
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_expr0
---
e = Let (Static bound_static = Static_consts consts) e_body
--------------------------------------------------
Θ ⊢ e ⤳ (emit Cdata phrases for the static constants at their symbols per R.Obj.*,
plus any deferred symbol-field updates; then Θ ⊢ e_body ⤳ ·, sequencing updates)
NOTES: Static constants become `Cdata` (cmm.mli#data_item): blocks, boxed numbers,
immutable/mutable arrays and strings, and sets of closures are laid out at their
symbols exactly as R.Obj.* prescribes, but as link-time data (black-marked
headers, R.Header). Or_variable "holes" filled from ρ become deferred stores
(update_opt, sequenced before the body). Cmm image of OS.Let.Static. RecInfo
bindings emit nothing (Singleton _, Rec_info; matches OS.Let.RecInfo).
```

## 3. Tagging and boxing

```rule
RULE TC.Prim.TagUntag
STATUS normative
CODE backend/cmm_helpers.ml#tag_int
CODE backend/cmm_helpers.ml#untag_int
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion
---
Tag_immediate:   ⟦naked_imm n⟧ ⤳ tag_int e = Cop(Caddi, [Cop(Clsl,[e;1]); 1])   -- 2n+1
Untag_immediate: ⟦tagged_imm n⟧ ⤳ untag_int e = Cop(Casr, [e; 1])               -- ⌊·/2⌋
--------------------------------------------------
Commutes with ≈: if n ≈ᵥ (untagged) e then (tagged_imm n) ≈ᵥ tag_int e, and
conversely (R.Val.Imm). undef-free (total on the value range).
NOTES: Untag also records an `Env.Untag` marker so a later Switch can consume the
tagged form directly (16, TC.Switch). tag_int has peephole cases; untag_int folds
`(x asr k) | 1` patterns. Cmm image of P.Unary.TagImmediate / UntagImmediate (06).
```

```rule
RULE TC.Prim.BoxUnbox
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number
CODE backend/cmm_helpers.ml#box_int_gen
CODE backend/cmm_helpers.ml#unbox_int
---
Box_number κ:   ⟦naked κ c⟧ ⤳ a Calloc of the boxed layout for κ (R.Obj.Boxed):
  float  → box_float  (double_tag block, value at offset 0)
  float32/int32/int64/nativeint → box_int_gen / box_float32 (custom block: ops word
    then payload; int32 stored sign-extended (LE) or ≪32 (BE))
Unbox_number κ: ⟦ptr a⟧ ⤳ a load of the payload:
  float → Cop(Cload Double,[a]);  others → Cop(Cload chunk,[Cadda(a, 8)])  (skip ops word)
--------------------------------------------------
Commutes with ≈: Box_number allocates an object o = Boxed(κ, c) with o @ a ≈ₒ M′
(R.Obj.Boxed) and returns ptr a; Unbox_number reads back c. undef-free.
NOTES: unbox loads at offset size_addr = 8 for custom blocks (skipping the ops
pointer), at 0 for a bare float. ENDIANNESS-DEPENDENT for int32 (box_int_gen). Cmm
image of P.Unary.BoxNumber / UnboxNumber (06). Allocation detail (heap vs local,
GC) in 19.
```

## 4. Blocks

```rule
RULE TC.Prim.MakeBlock
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
CODE backend/cmm_helpers.ml#make_alloc_generic
---
Make_block (kind, mut, am):  ⟦v̄⟧ ⤳ make_block, i.e. a Calloc whose first argument is
the header Cconst_natint (R.Header) and whose remaining arguments are the field
words (Values: Word_val; Naked_floats: Double via make_float_alloc; Mixed:
make_mixed_alloc with per-field chunks memory_chunk_of_flat_suffix_element).
--------------------------------------------------
Commutes with ≈: allocates o = Block/FloatBlock/MixedBlock(...) with o @ a ≈ₒ M′
(R.Obj.Block / FloatBlock / MixedBlock) and returns ptr a.
NOTES: Large heap blocks take the caml_alloc_shr_check_gc + fill_fields path
(make_alloc_generic) instead of a single Calloc; same resulting layout. Cmm image
of P.Variadic.MakeBlock.*. Mixed-block flat-suffix packing is little-endian-only
(get_field_unboxed fatal on BE). Allocation/region detail in 19.
```

```rule
RULE TC.Prim.BlockLoad
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load
CODE backend/cmm_helpers.ml#get_field_computed
CODE backend/cmm_helpers.ml#field_address
---
Block_load (kind, mut, field i):  ⟦ptr a⟧ ⤳ Cop(Cload{κ; mutability}, [field_address a i])
where field_address a i = Cadda(a, 8·off) (just `a` when off = 0, since field_address
folds a zero offset to the bare pointer), off = i for uniform blocks or
Mixed_block_shape.offset_in_words for mixed flat fields, and κ is the field's chunk
(Word_val / Word_int / Double / Single / Byte_signed / …).
--------------------------------------------------
Commutes with ≈: reads the word ≈ says holds field i of the object (R.Obj.Block /
MixedBlock / FloatBlock); result vᵢ ≈ᵥ the loaded value.
NOTES: `mutability` is the load's immutability hint (immutable loads are CSE-/
reorder-eligible, 06 P.Effects.ReadingFromBlock; 13 §4.4). Mixed flat sub-word
loads are little-endian-only (get_field_unboxed). Cmm image of P.Unary.BlockLoad.*.
```

```rule
RULE TC.Prim.BlockSet
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_set
CODE backend/cmm_helpers.ml#setfield_computed
---
Block_set (kind, init_or_assign, field i):  ⟦ptr a; v⟧ ⤳
  value/immediate field → setfield_computed: caml_modify(field_address a i, v) for a
    heap value assignment (GC write barrier), caml_modify_local / caml_initialize /
    plain Cstore(Word_int) per assignment_kind;
  naked-float field → Cop(Cstore(Double, Assignment), [field_address a i; v]);
  mixed flat field → Cop(Cstore(chunk, Assignment), [field_address a i; low_bits v])
--------------------------------------------------
Commutes with ≈: writes M so the object's field i now represents v (R.Obj.*),
returning Cmm unit.
NOTES: A value store to the heap MUST use caml_modify (GC write barrier), which is
why to_cmm emits it rather than a bare Cstore(Word_val) (setfield_computed;
CM.Store note). Sub-word flat stores truncate the value (low_bits) and are
little-endian-only. Cmm image of P.Binary.BlockSet.*.
```

## 5. Closures

```rule
RULE TC.Prim.ProjectFunctionSlot
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
CODE backend/cmm_helpers.ml#infix_field_address
---
Project_function_slot {move_from = f₁; move_to = f₂}:  ⟦clos ℓ f₁⟧ ⤳
  infix_field_address arg (off(f₂) − off(f₁)) = Cop(Caddv, [arg; 8·(off(f₂)−off(f₁))])
  (the byte offset 8·(off(f₂)−off(f₁)) may be NEGATIVE when f₂ precedes f₁; and
  infix_field_address returns arg UNCHANGED when the offset is 0, i.e. f₂ = f₁)
--------------------------------------------------
Commutes with ≈: moves within the same closure block from slot f₁ to sibling f₂,
i.e. clos ℓ f₁ ≈ᵥ arg ⟹ clos ℓ f₂ ≈ᵥ result (R.Val.Clos). No load, no allocation.
NOTES: A `Caddv` (Val-producing pointer adjust) by the signed slot-offset difference
in bytes (witnessed as `(+v soc 24)` / `(+v my_closure -24)` for a forward/backward
move); the result still points into the same block, legitimized by f₂'s infix header
(R.Obj.Closures). `infix_field_address` (cmm_helpers.ml) folds a zero offset to the
bare pointer. Cmm image of P.Unary.ProjectFunctionSlot / Move_within_set_of_closures (06).
```

```rule
RULE TC.Prim.ProjectValueSlot
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
CODE backend/cmm_helpers.ml#get_field_computed
---
Project_value_slot {project_from = f; value_slot = w}:  ⟦clos ℓ f⟧ ⤳
  Cop(Cload{chunk(w); Immutable}, [field_address arg (off(w) − off(f))])
--------------------------------------------------
Commutes with ≈: loads the captured value env(w) from the shared environment of the
closure block (R.Obj.Closures); result ≈ᵥ env(w).
NOTES: Offset is (value-slot offset − function-slot offset); chunk from the value
slot's kind. The environment is immutable once built (06,
P.Unary.ProjectValueSlot). Cmm image of P.Unary.ProjectValueSlot.
```

## 6. Scalar conversions — and the int→float32 double-rounding

```rule
RULE TC.Prim.NumConv
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#arithmetic_conversion
CODE backend/cmm_helpers.ml#float32_of_int
CODE backend/cmm.mli#static_cast
---
Num_conv {src; dst}:  ⟦e⟧ ⤳ Scalar_type.static_cast, i.e.
  Int→Int   → width-adjust (sign/zero extend + tagging), then sign_extend for int32 dst
  Float↔Float → Cstatic_cast (Float32_of_float | Float_of_float32)
  Int→Float → widen source to nativeint, then unary (Cstatic_cast (Float_of_int dst))
              — for dst = Float32 this is a SINGLE Cstatic_cast (Float_of_int Float32)
  Float→Int → Cstatic_cast (Int_of_float src), then narrow
--------------------------------------------------
Commutes with ≈ ⟺ the Cmm cast computes ⟦Num_conv⟧ (05). For int→float32 the Cmm is
ONE cast (single rounding). Therefore any int→float32 double-rounding is NOT
introduced here.
NOTES: This is the sharp end of the float32 soundness bug (13 §4.7,
float32_double_round). to_cmm emits `float32_of_int` = one
`Cstatic_cast (Float_of_int Float32)` on a nativeint-widened source, which lowers
to a single `cvtsi2ss` on amd64 (single rounding). The double rounding is committed
*earlier*, by Simplify's constant fold (number_adjuncts to_naked_float32 computing
int→float64→float32); so `TC.Prim.NumConv` is SOUND and the bug is in
`S.Rewrite.Prim.ConstFold` (P.Unary.NumConv), exactly as the case study concludes.
The nativeint widening is a 64-bit-target detail.
```

## 7. Strings and arrays

```rule
RULE TC.Prim.StringLoad
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#string_like_load
CODE backend/cmm_helpers.ml#unaligned_load_16
---
String/Bytes/Bigstring load (width w, index j untagged):  ⟦str; j⟧ ⤳
  Cop(Cload{chunk(w)}, [add_int_ptr base j])   where base = str (heap) or the data
  pointer loaded from field 1 (bigstring, ptr_out_of_heap); chunk per width
  (Byte_unsigned/…/unaligned_load_16/32/64/f32)
--------------------------------------------------
Commutes with ≈: reads the w-byte little-endian scalar at byte offset j of the
byte sequence (R.Obj.Bytes; 06 P.Binary.StringOrBigstringLoad).
NOTES: Unaligned multi-byte loads split into byte loads when
¬Arch.allow_unaligned_access, byte order per Arch.big_endian (unaligned_load_16);
here LE with unaligned access allowed, so a single Cload. Bigstrings index off-heap
memory (not under L). Cmm image of P.Binary.StringOrBigstringLoad.
```

```rule
RULE TC.Prim.ArrayAccess
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set0
CODE backend/cmm_helpers.ml#array_indexing
---
Array_load (ak, index i tagged):  ⟦arr; i⟧ ⤳ Cop(Cload{chunk(ak); mut}, [array_indexing log2(ak) arr i])
Array_set (ak, …):                ⟦arr; i; v⟧ ⤳ int_array_set / addr_array_store (caml_modify
  for value assignment) / float_array_set, at array_indexing log2(ak) arr i
--------------------------------------------------
Commutes with ≈: element i of the array at scaled byte offset (R.Obj.Array); loads
yield vᵢ ≈ᵥ ·, sets update M consistently.
NOTES: array_indexing takes the TAGGED index and folds the untag into the scale
(constant n ↦ (n≫1)≪log2; the (c≪1)|1 pattern ↦ c≪log2). Immutable array loads are
CSE-eligible (13 §4.4). Value stores use caml_modify (heap) / caml_modify_local
(local) / caml_initialize (init). Cmm image of P.Binary.ArrayLoad / P.Ternary.ArraySet.
Out-of-bounds is undef (06, checks inserted by the frontend, P.Unchecked.*).
```

## 8. Summary of rules

Simples / schema: `TC.Simple`, `TC.Prim.Sound`.

Let-forms: `TC.Let.Simple`, `TC.Let.Prim`, `TC.Let.Subst`, `TC.Let.SetOfClosures`,
`TC.Let.Static`.

Tagging/boxing: `TC.Prim.TagUntag`, `TC.Prim.BoxUnbox`.

Blocks: `TC.Prim.MakeBlock`, `TC.Prim.BlockLoad`, `TC.Prim.BlockSet`.

Closures: `TC.Prim.ProjectFunctionSlot`, `TC.Prim.ProjectValueSlot`.

Conversions / strings / arrays: `TC.Prim.NumConv`, `TC.Prim.StringLoad`,
`TC.Prim.ArrayAccess`.
