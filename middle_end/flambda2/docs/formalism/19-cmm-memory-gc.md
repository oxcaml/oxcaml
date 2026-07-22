# Cmm allocation, regions, and the garbage collector

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter completes the Cmm machine of [`15`](15-cmm.md) with the rules it
deferred: concrete **allocation** (`Calloc`, heap and local), the **region
stack** (`Cbeginregion`/`Cendregion`), and the **moving garbage collector** and
its interaction with the machtype `Addr` contract. These are the run-time
mechanics that *maintain* the representation relation `≈`
([`17`](17-representation.md)) as memory grows and is reclaimed. Its rules are in
the `CM.*` namespace (they are Cmm operational semantics), continuing
[`15`](15-cmm.md).

Two things here have no Flambda counterpart and are the reason `to_cmm` is a
*refinement* rather than an equivalence ([`20`](20-to-cmm-soundness.md)): the GC
(invisible to the abstract machine, which has a logical heap of opaque
locations, [`04`](04-opsem.md) §1.3) and **resource exhaustion** (a Cmm run can
fail to allocate; a Flambda run cannot). The local-allocation region machinery,
by contrast, *does* correspond to the Flambda region stack `R`
([`04`](04-opsem.md) §1.8).

## 1. Heap allocation

```rule
RULE CM.Alloc.Heap
STATUS normative
CODE backend/cmm.mli#Calloc
CODE backend/cmm_helpers.ml#make_alloc_generic
---
e_c = Cop(Calloc(Heap, kind), [word hdr; v₁ … vₙ], dbg)
a fresh, 8-byte-aligned, with a … a+8·(n−1) unmapped in M   (after a GC if needed, §3)
M′ = M with M′[a−8] = word hdr and M′[a + 8·(i−1)] = vᵢ
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨word a, ce, χ, M′, TT, RR⟩
NOTES: Allocates a heap block: the header word (built per R.Header) is written at
a−8 and the n field values at a…; the result is a Val pointer to a (R.Val.Pointer).
`kind` (alloc_block_kind) is GC bookkeeping (which allocator statistics). A minor
allocation that overflows the young region triggers a collection first (§3); a
large block takes the caml_alloc_shr_check_gc path (make_alloc_generic) but
produces the same layout. Cmm image of the allocation implicit in
P.Variadic.MakeBlock / OS.Let.SetOfClosures / P.Unary.BoxNumber (06). May fail:
resource exhaustion (§5).
```

## 2. Local allocation and regions

Local allocation places a block in the innermost open region rather than on the
GC heap; the whole region is freed at once when it ends. The Cmm region stack
`RR` mirrors the Flambda region stack `R` ([`04`](04-opsem.md) §1.8); region
handles correspond one-to-one.

```rule
RULE CM.Region.Begin
STATUS normative
CODE backend/cmm.mli#Cbeginregion
CODE middle_end/flambda2/terms/flambda_primitive.mli#Begin_region
CODE middle_end/flambda2/terms/flambda_primitive.mli#Begin_try_region
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#variadic_primitive
---
e_c = Cop(Cbeginregion, [], dbg);   ι fresh
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨word ι, ce, χ, M, TT, ι :: RR⟩
NOTES: Pushes a fresh local-allocation region and returns its handle. Cmm image
of a non-ghost Flambda Begin_region / Begin_try_region primitive (06; region stack
R, 04 §1.8), which lower identically to `Cbeginregion`. `to_cmm` emits
`Cbeginregion` only when stack allocation is enabled AND the primitive is not ghost.
There are two distinct erasure paths: when stack allocation is disabled the binding
is dropped in to_cmm_expr (let_expr0, is_begin_or_end_region); and a ghost region
(`Begin_region { ghost = true }` / `Begin_try_region { ghost = true }`, pervasive —
one per local function, bound_for_function.ml `my_ghost_region`) lowers in
variadic_primitive to `Cconst_int 0`, leaving RR unchanged. That ghost `Cconst_int
0` is already a Cmm value (word 0) and is not stepped by this region rule at all.
```

```rule
RULE CM.Alloc.Local
STATUS normative
CODE backend/cmm.mli#Calloc
CODE backend/cmm_helpers.ml#local_block_header
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#alloc_mode_for_allocations_to_cmm
---
e_c = Cop(Calloc(Local, kind), [word hdr; v₁ … vₙ], dbg);   RR = ι :: _
a fresh in region ι's arena;  hdr carries caml_local (R.Header)
M′ = M with the header and fields written at a (as CM.Alloc.Heap), tagged as belonging to ι
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨word a, ce, χ, M′, TT, RR⟩
NOTES: Allocates in the innermost open region (top of RR); the block lives until ι
is ended (CM.Region.End). The header is OR'd with caml_local (local_block_header);
otherwise the layout is identical to a heap block. Not moved by the GC (§3). Cmm
image of a local allocation (Alloc_mode.Local; OS.Let.SetOfClosures local case,
P.* allocation with Local mode). alloc_mode_for_allocations_to_cmm asserts stack
allocation is enabled.
```

```rule
RULE CM.Region.End
STATUS normative
CODE backend/cmm.mli#Cendregion
CODE middle_end/flambda2/terms/flambda_primitive.mli#End_region
CODE middle_end/flambda2/terms/flambda_primitive.mli#End_try_region
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
---
e_c = Cop(Cendregion, [word ι], dbg);   RR = ι₀ :: … :: ι :: RR₀
M′ = M with every block allocated in ι (or in a region above it in RR) reclaimed
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨word 1 (unit), ce, χ, M′, TT, RR₀⟩
NOTES: Pops down to and discards region ι, reclaiming everything allocated in it.
Cmm image of a non-ghost Flambda End_region / End_try_region primitive (06; the
R-update of 04 §1.8), which lower identically to `Cendregion`. As with
CM.Region.Begin, `Cendregion` is emitted only when stack allocation is enabled AND
the primitive is not ghost: stack-alloc-disabled drops the binding in to_cmm_expr
(is_begin_or_end_region), while a ghost `End_region { ghost = true }` /
`End_try_region { ghost = true }` lowers in unary_primitive to unit (word 1) with
RR unchanged, not stepped by this rule. A
value that escaped a region (returned past its End_region) would be a dangling
pointer — the type system / Simplify's alloc-mode discipline prevents this, and
to_cmm's End_region flush (18, TC.Let.Subst NOTES) stops immutable loads of
local blocks from being reordered past the End_region. Correspondence: RR ≈ R,
ι-in-RR ≙ ι-in-R.
```

## 3. The garbage collector

The GC is modelled *axiomatically*: it is any transition, triggered at an
allocation point, that reclaims unreachable heap blocks and may relocate live
ones, while **preserving `≈`**. It is invisible to the Flambda machine (whose
heap `H` is logical), so a GC step corresponds to no Flambda step — it is a
stutter in the simulation ([`20`](20-to-cmm-soundness.md)).

```rule
RULE CM.Alloc.GC
STATUS conjectured
CODE backend/cmm_helpers.ml#make_alloc_generic
CODE backend/cmm.mli#machtype_component
---
An allocation (CM.Alloc.Heap/Local) may be preceded by a collection, atomically:
at an allocation point, instead of allocating in M directly, the machine may take
the single fused step
  ⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_c″, ce′, χ, M″, TT, RR⟩
in which there is a relocation ϕ : Addr ⇀ Addr on live Val-reachable blocks such that:
  (i)   if H ≈_L M then H ≈_{ϕ∘L} M′   (the abstract heap is unchanged; only L moves);
  (ii)  every Val root in ce, every Val word pending in e_c (already-evaluated
        operands of the plugged context — including the allocation's own field
        operands), and every Val field in M′ is updated by ϕ (ce′ = ϕ∘ce on
        Val; e_c′ = e_c with its pending Val words rewritten by ϕ);
  (iii) Int/Float/naked values and their words are unchanged;
  (iv)  blocks unreachable from the Val roots may be dropped from M′ (reclaimed);
  (v)   local blocks (in regions on RR) are not moved.
and the collected configuration ⟨e_c′, ce′, χ, M′, TT, RR⟩ performs the
allocation by CM.Alloc.Heap/Local (with no further collection), reaching
⟨e_c″, ce′, χ, M″, TT, RR⟩. There is no standalone collection transition.
--------------------------------------------------
The GC preserves the representation relation: H ≈_{ϕ∘L} M′ still holds, so no
Flambda-observable changes. Modelled, not implemented, here.
NOTES: STATUS conjectured — this axiomatizes the OCaml moving minor/major collector
as ≈-preserving, rather than modelling collection. This is the standard "GC is
observational identity" assumption; its soundness rests on the machtype discipline
(GC roots are exactly the Val-typed live variables; cmm.mli header comment) and on
CM.Addr.NoSurvive. In the plugging presentation the expression under reduction
plays the register file's role: already-evaluated Val operands pending in e_c
are GC roots exactly like Val-typed variables in ce, which is why (ii) rewrites
them — most importantly the allocation's own field operands, which would
otherwise be stored as stale addresses immediately after the collection. It is why `L` is existential and mutable (R.Heap): observations
are compared up to ≈ (R.Observe), never by absolute address. Fusing collection
into the allocating transition removes the zero-progress reading (an identity ϕ
— nothing moves, nothing dropped — satisfies (i)–(v) vacuously, and as a
standalone step would let CM.Unit.Final classify every allocating program as
divergent via GC self-loops): every collection makes allocation progress, so an
infinite run necessarily contains infinitely many non-GC steps. Real collectors
run exactly at allocation points (make_alloc_generic), which the trigger
already required.
```

## 4. The `Addr` discipline

`cmm.mli` states that an `Addr`-typed value (a derived pointer into the middle of
a block, e.g. from `Cadda` during field addressing) "must never be live across an
allocation point or function call": the GC cannot treat it as a root (it does not
point just past a header) yet may move the block it points into. `to_cmm`
guarantees this by construction, and it is what makes `CM.Alloc.GC` sound.

```rule
RULE CM.Addr.NoSurvive
STATUS normative
CODE backend/cmm.mli#machtype_component
CODE backend/cmm_helpers.ml#field_address
CODE backend/cmm_helpers.ml#setfield_computed
---
No Addr-typed value (a Cadda/field_address result) is let-bound or held live across
any GC-permitting point: an allocating primitive (a CM.Alloc.Heap/Local, hence a
possible CM.Alloc.GC) or any Capply / alloc=true Cextcall. Each field_address (Cadda)
that to_cmm emits is recomputed inline from a Val base (a GC root) and consumed at
once — as the address operand of the enclosing Cload/Cstore (TC.Prim.BlockLoad,
immediate BlockSet, ArrayAccess), OR as the address argument of a caml_modify /
caml_initialize Cextcall (the GC write barriers for Word_val stores; TC.Prim.BlockSet
value case). Both barriers are alloc=false, so no GC occurs while the Addr is live.
--------------------------------------------------
Every reachable Cmm state's live Addr values point into blocks that the immediately
consuming step reads/writes with no intervening allocation, so CM.Alloc.GC's
relocation ϕ never invalidates a value that is subsequently used.
NOTES: This is the runtime counterpart, for to_cmm output, of the cmm.mli Addr
contract ("never live across an allocation point or function call", cmm.mli
machtype_component). The precise invariant to_cmm maintains is "no Addr is let-bound
or held across an alloc=true point" — NOT the stronger "Addr only feeds Cload/Cstore":
a Word_val store passes its `field_address` into a `caml_modify`/`caml_initialize`
Cextcall (setfield_computed; addr_array_initialize on the large-block fill path,
make_alloc_generic), consuming the Addr at call entry, and those barriers do not
allocate. `Caddv` results (ProjectFunctionSlot, TC.Prim.ProjectFunctionSlot) are Val
pointers (GC roots), not Addr, so they may survive. Violating the invariant would be
a to_cmm codegen bug that CM.Alloc.GC would expose as a dangling pointer.
```

## 5. Resource exhaustion

```rule
RULE CM.Alloc.Exhaustion
STATUS normative
CODE backend/cmm_helpers.ml#make_alloc_generic
---
An allocation (CM.Alloc.Heap/Local, after a possible CM.Alloc.GC) that cannot be
satisfied — no free memory / stack overflow — halts the run with an out-of-memory /
stack-overflow outcome.
--------------------------------------------------
This is an observable termination outcome of a Cmm run that the Flambda machine
does not have (04 §8.2 has no resource limit).
NOTES: The `to_cmm` simulation (20, INV.ToCmm.Simulates) is stated modulo this
outcome: a Cmm run may additionally exhaust resources where the corresponding
Flambda run diverges or terminates normally. This is the standard finiteness gap
between an idealized semantics and a real machine, not a `to_cmm` bug.
```

## 6. Summary of rules

Allocation: `CM.Alloc.Heap`, `CM.Alloc.Local`, `CM.Alloc.Exhaustion`.

Regions: `CM.Region.Begin`, `CM.Region.End`.

Garbage collector: `CM.Alloc.GC`, `CM.Addr.NoSurvive`.
