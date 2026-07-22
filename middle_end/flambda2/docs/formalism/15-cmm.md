# Core Cmm: syntax and operational semantics

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter defines the *target* language of the `to_cmm` translation
([`16`](16-to-cmm-control.md), [`18`](18-to-cmm-data.md)): a fragment of Cmm
(`backend/cmm.mli`, "Second intermediate language (machine independent)") and a
small-step abstract machine for it. It plays for the backend boundary the role
that [`04`](04-opsem.md) plays for Flambda: it owns the Cmm machine
configuration and transition judgment, and later chapters only invoke them.

Cmm is where the representation the abstract machine of [`04`](04-opsem.md) kept
opaque becomes concrete: values are machine words, memory is byte-addressed,
tagging/boxing/field-access are arithmetic and loads/stores. This chapter fixes
the machine and the *control* constructs; how abstract Flambda values and heap
objects correspond to concrete Cmm words and memory is the **representation
relation `≈`** of [`17`](17-representation.md), and the concrete allocation /
region / GC rules are [`19`](19-cmm-memory-gc.md). The `to_cmm` correctness
statement tying the two machines together is [`20`](20-to-cmm-soundness.md).

## 0. Target assumptions (64-bit little-endian)

This formalism fixes the target at **64-bit, little-endian**, `size_addr =
size_int = size_float = 8` bytes. This matches the code as it exists:
`to_cmm.ml#unit0` raises `Misc.fatal_error` on a 32-bit target, and the
mixed-block lowering asserts `size_float = size_addr` and a 64-bit address
(`cmm_helpers.ml#Mixed_block_support`). The two supported native targets, amd64
and arm64, are bit-for-bit identical in everything `to_cmm`/`cmm_helpers` read
except `division_crashes_on_overflow` (a trapping property of the hardware
divide instruction, not a representation property; see
[`05`](05-primitives-scalar.md) on checked division).

The seams where a genuinely target-parameterized model would branch — word
width, endianness, `size_float`-vs-`size_addr`, division trapping — are called
out in prose where they occur. Extending to 32-bit (aspirational; oxcaml
PR #685) or big-endian is deferred; see the scope ledger
([`01`](01-overview.md)). Word size `W = 64` and byte order LE are used
throughout without further comment.

## 1. Syntax

We model the fragment of `backend/cmm.mli` (`type expression`) that `to_cmm`
emits for the in-scope Flambda fragment. Omitted: SIMD *casts*
(`Cstatic_cast`/`Creinterpret_cast` vector variants — emitted only by the
arch-specific intrinsic lowering, not by the generic `to_cmm` paths), probes
(`Cprobe`/`Cprobe_is_enabled`), phantom lets (`Cphantom_let` — debugging only),
and the algebraic-effect operations — all out of scope per
[`01`](01-overview.md). SIMD vector *constants, chunks and machtypes* are in
scope.

```
value_expr ce ::=
    Cconst_int n | Cconst_natint n            -- machine-word constants
  | Cconst_float f | Cconst_float32 f          -- FP constants
  | Cconst_vec128 b | Cconst_vec256 b | Cconst_vec512 b   -- SIMD bit patterns
  | Cconst_symbol sym                          -- address of a symbol
  | Cvar x                                     -- x : Backend_var.t
  | Clet (x = ce₁) ce₂                          -- bind x to the value of ce₁ in ce₂
  | Csequence ce₁ ce₂                           -- evaluate ce₁ (for effect), then ce₂
  | Ctuple [ce₁ … ceₙ]                          -- a tuple of values (unboxed results)
  | Cop (op, [ce₁ … ceₙ], dbg)                  -- primitive operation (§5–6)
  | Cifthenelse (ce, ce_t, ce_f)                -- conditional
  | Cswitch (ce, index, [ce₀ … ce_{m}])          -- indexed multi-way branch
  | Ccatch (flag, [handlerᵢ], ce_body)          -- static catch (§7)
  | Cexit (lbl, [ce₁ … ceₙ], [ta₁ … ta_k])       -- static jump, with trap actions
  | Cinvalid { message }                       -- unreachable

op   ::= Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor
       | Clsl | Clsr | Casr | Ccmpi cmp | Caddv | Cadda
       | Cnegf w | Caddf w | Csubf w | Cmulf w | Cdivf w | Ccmpf (w, fcmp)
       | Cstatic_cast sc | Creinterpret_cast rc | Ctuple_field (n, tys)
       | Cload { memory_chunk; mutability; is_atomic }
       | Cstore (memory_chunk, init_or_assign)
       | Calloc (alloc_mode, alloc_block_kind)       -- see [19]
       | Capply { result_type; region; callees }     -- OCaml call
       | Cextcall { func; ty; ty_args; alloc; … }     -- C call
       | Craise raise_kind
       | Cbeginregion | Cendregion                    -- see [19]
       | Copaque | …

handler ::= ⟨lbl, [x₁ … xₙ], ce_h, is_cold⟩     -- Cmm.static_handler (params, body)
flag    ::= Normal | Recursive | Exn_handler     -- Cmm.ccatch_flag
ta      ::= Push lbl | Pop lbl                    -- Cmm.trap_action
```

`lbl` ranges over `Lambda.static_label` (the catch/exit labels; `Return_lbl`
denotes the enclosing function's return). Every non-`Return_lbl` `Cexit` targets
a lexically-enclosing `Ccatch` handler of the same label, exactly as Flambda
continuations are dominated by their binder ([`04`](04-opsem.md) §1.6); this is a
well-formedness precondition inherited from `to_cmm`, not re-checked here.

```rule
RULE CM.Syntax.Fragment
STATUS descriptive
CODE backend/cmm.mli#expression
CODE backend/cmm.mli#operation
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
---
The grammar above is the fragment of Cmm.expression / Cmm.operation that
to_cmm_expr.ml#expr and to_cmm_primitive.ml emit for the in-scope Flambda
fragment. SIMD casts (the vector variants of Cstatic_cast/Creinterpret_cast),
Cprobe(_is_enabled), Cphantom_let, and effect operations are omitted (out of
scope, ch. 01).
--------------------------------------------------
This chapter's rules range over the above grammar.
NOTES: `to_cmm` never emits `Ctuple` other than for unboxed multi-value
results, or `Cop(Cprobe …)`, in the in-scope fragment; Ctuple arises only from
unarized multi-value returns (translate_apply0, wrap). `Cconst_vecN` arises
from unboxing a statically-boxed vector symbol (unbox_vector's
structured_constant_of_sym shortcut, ch. 18). The SIMD casts belong to the
arch-specific vector-intrinsic lowering, which stays out of scope; the vectors
this fragment moves are untyped bit patterns ("SIMD vectors are untyped in the
backend", cmm.mli).
```

## 2. Machine values and memory

A Cmm value is a machine word, a floating-point number, or a SIMD vector; the
`machtype`
(`Val`/`Int`/`Addr`/`Float`/`Float32`/`Vec128`/`Vec256`/`Vec512`) is a *static*
classification for register
allocation and GC (`cmm.mli` header comment) and is not part of the runtime
value, with one exception the GC cares about (`Addr`; [`19`](19-cmm-memory-gc.md)).

```
cmm value  w ::= word n        -- a 64-bit machine word (0 ≤ n < 2⁶⁴, or its signed reading)
               | flt f          -- a 64-bit IEEE double
               | flt32 f        -- a single, when held in a register (Float32)
               | vec128 b | vec256 b | vec512 b   -- untyped SIMD bit patterns, in vector registers

memory     M : Addr ⇀ Byte      -- byte-addressed; Addr = machine words used as addresses
```

Vector machtypes are GC-ignored (same class as `Float`) and never LUB-combine
with any other component — `Cmm.lub_component` is a fatal error on any
cross-width or vector/scalar mix, so a join point cannot mix vector widths.

Memory is **byte-addressed** and little-endian. Multi-byte access is defined by

```rule
RULE CM.Mem.LoadStore
STATUS normative
CODE backend/cmm.mli#memory_chunk
CODE backend/cmm.mli#size_of_memory_chunk
CODE backend/cmm_helpers.ml#mk_load_immut
---
For a chunk κ of width b = size_of_memory_chunk κ bytes (Byte=1, Sixteen=2,
Thirtytwo/Single=4, Word_int/Word_val/Double=8,
Onetwentyeight_*=16, Twofiftysix_*=32, Fivetwelve_*=64):
  read(M, a, κ)  = the little-endian b-byte integer/float at M[a … a+b−1],
                   sign- or zero-extended to a word per κ's signedness
                   (Byte_signed/Sixteen_signed/Thirtytwo_signed sign-extend;
                    the _unsigned chunks zero-extend; Double/Single decode IEEE;
                    the vector chunks read/write the raw b-byte bit pattern)
  write(M, a, κ, w) = M with the low b bytes of w stored little-endian at a
--------------------------------------------------
read/write are the meaning of Cload/Cstore (§6); undefined (undef) if a is
unmapped or misaligned beyond what Arch.allow_unaligned_access permits. The
vector chunks come in `_aligned`/`_unaligned` pairs: the `_aligned` chunks are
undef unless a is 16/32/64-byte aligned; the `_unaligned` chunks require only
what the target's unaligned access allows (nothing, on amd64).
NOTES: 64-bit little-endian is baked in here (byte order, b = 8 for word chunks).
Sign/zero extension follows the chunk, matching the int32 sign-extension and
sub-word truncation discipline of to_cmm (ch. 18). `Single` decodes/【encodes a
32-bit float; when loaded to a Float register it becomes `flt` per the target's
Float_of_float32 convention (ch. 18, box/unbox of float32).
```

## 3. Machine state

```
cmm config  ⟨ce, χ, M, TT, RR⟩ over a current expression e_c

ce : Backend_var ⇀ cmm value            -- Cmm variable environment
χ  : Lambda.static_label ⇀ cmm_handler  -- installed static-catch handlers
TT ::= [] | lbl :: TT                    -- Cmm trap stack (exception handler labels)
RR ::= [] | ι :: RR                      -- Cmm region stack (see [19])

cmm_handler ::= CHandler ⟨x̄, e_h, ce_def, χ_def, d, kind⟩
```

The components mirror [`04`](04-opsem.md): `ce` is the concrete analogue of `ρ`,
`χ` of `K`, `M` of `H` (but byte-addressed and concrete), `TT` of the trap stack
`T`, `RR` of the region stack `R`. A `CHandler` records its params, body,
definition-time environments, the trap depth `d = |TT|` at definition, and
whether it is an ordinary catch (`Normal`/`Recursive`) or an exception handler
(`Exn_handler`). We write the transition

```
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_c′, ce′, χ′, M′, TT′, RR′⟩
```

subscripting `⟶c` to distinguish it from Flambda's `⟶`.

## 4. Evaluation contexts

Unlike Flambda (which is in ANF, so [`04`](04-opsem.md) needs no evaluation
contexts), Cmm has nested subexpressions. We evaluate them with left-to-right
evaluation contexts, reducing an innermost redex.

```
E ::= [·]
    | Clet (x = E) e₂
    | Csequence E e₂
    | Ctuple [w̄, E, ē]
    | Cop (op, [w̄, E, ē], dbg)
    | Cifthenelse (E, e_t, e_f)
    | Cswitch (E, index, cases)
    | Cexit (lbl, [w̄, E, ē], tā)
```

`Ccatch` bodies and handler bodies are **not** evaluation-context positions: they
step by the control rules of §7, which install/consult `χ`. `Craise`/`Capply`
argument positions are ordinary `Cop` positions.

```rule
RULE CM.Context
STATUS descriptive
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
CODE backend/cmm.mli#expression
---
If ⟨r, ce, χ, M, TT, RR⟩ ⟶c ⟨r′, ce′, χ′, M′, TT′, RR′⟩ for a redex r, then
⟨E[r], ce, χ, M, TT, RR⟩ ⟶c ⟨E[r′], ce′, χ′, M′, TT′, RR′⟩,
provided r′ does not itself transfer control (Cexit/Craise/Capply-tail); a
control transfer discards the surrounding E per §7.
--------------------------------------------------
Reduction is congruent under evaluation contexts for value-producing redexes.
NOTES: EVALUATION-ORDER MODELING CHOICE. Cmm does not fix the evaluation order of
`Cop` arguments (Selectgen chooses; historically right-to-left). We fix
left-to-right for determinism of the semantics. This is observable only for
subexpressions with interacting effects/coeffects; to_cmm's let-substitution
(ch. 18) only reorders across this boundary when the effects/coeffects quadruple
(ch. 06, P.Effects.*) proves it sound, so the correctness statement (ch. 20) is
independent of the chosen order. A control transfer (Cexit/Craise) out of E
abandons the pending context E, matching static jump. Adversarially confirmed: to_cmm
pre-sequences any interacting effects into explicit `Clet` bindings (the emitted flush
order is right-to-left), so two arguments of one `Cop` are never simultaneously
non-value with interacting effects — the residual `Cop`-argument order is therefore
unobservable and the left-to-right modelling choice is sound.
```

## 5. Pure and arithmetic operations

The scalar operations compute on machine words and floats. Rather than restate
[`05`](05-primitives-scalar.md), we define `⟦op⟧c` by reference: each Cmm integer
op is the corresponding function on 64-bit two's-complement words, each float op
the IEEE double/single operation, exactly as the Flambda naked-number
denotations of [`05`](05-primitives-scalar.md), but on *untagged* words (Cmm has
no tagging; tagging is arithmetic introduced by `to_cmm`, ch. 18).

```rule
RULE CM.Op.Pure
STATUS normative
CODE backend/cmm.mli#operation
CODE backend/cmm_helpers.ml#add_int
CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#prim_simple
---
op ∈ {Caddi,Csubi,Cmuli,Cdivi,Cmodi,Cand,Cor,Cxor,Clsl,Clsr,Casr,Ccmpi _,
      Cnegf _,Caddf _,Csubf _,Cmulf _,Cdivf _,Ccmpf _,Cstatic_cast _,
      Creinterpret_cast _,Caddv,Cadda,Copaque}
⟦op⟧c(w̄) = w        -- the word/float function on 64-bit values; = undef on the
                       partial cases (Cdivi/Cmodi by 0; Cstatic_cast out of range)
--------------------------------------------------
⟨Cop(op, [v̄], dbg), ce, χ, M, TT, RR⟩ ⟶c ⟨v, ce, χ, M, TT, RR⟩   where v = ⟦op⟧c(w̄)
NOTES: `Ccmpi`/`Ccmpf` return word 1 / word 0. `Caddv` produces a Val-typed
pointer, `Cadda` an Addr-typed derived pointer, `Caddi` an Int — same integer
addition, differing only in the machtype the GC sees (relevant only at
allocation points, ch. 19). `Cstatic_cast (Float_of_int Float32)` is a SINGLE
rounding (int→float32); this is the exact site the int→float32 double-rounding
concern (ch. 13 §4.7, ch. 18) is measured against — `to_cmm` emits one cast, so
the double rounding, when it occurs, is in Simplify's constant fold, not here.
Cdivi by −1 special-cases min_int (ch. 05, S.Rewrite.Prim.ConstFold.PartialUndef;
Arch.division_crashes_on_overflow governs the guard, the one amd64/arm64
divergence).
```

Multi-value results (from an unboxed-product `Cextcall`, and from unarized
multi-value returns) are carried by `Ctuple [v̄]`, a transient bundle projected by
`Ctuple_field`.

```rule
RULE CM.Op.TupleField
STATUS normative
CODE backend/cmm.mli#Ctuple_field
CODE backend/cmm_helpers.ml#tuple_field
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call
---
⟨Ctuple [v₀ … v_{k−1}], …⟩ is a k-value bundle value ⟨v₀ … v_{k−1}⟩ (Ctuple [] = void)
⟦Ctuple_field (n, tys)⟧c(⟨v₀ … v_{k−1}⟩) = vₙ
--------------------------------------------------
⟨Cop(Ctuple_field (n, tys), [⟨v̄⟩], dbg), ce, χ, M, TT, RR⟩ ⟶c ⟨vₙ, ce, χ, M, TT, RR⟩
NOTES: `to_cmm` emits `Ctuple` / `Ctuple_field` only to bundle and then destructure
an unboxed-product result — chiefly an unboxed-product return from a `Cextcall`
(translate_external_call, `C.tuple_field`), and the unarized multi-value returns of
CM.Syntax.Fragment. `tys` is the whole tuple's machtype (register-allocation info),
not part of the value semantics. Cmm image of the multi-value C_call return
(OS.Apply.CCall with a multi-component result arity).
```

## 6. Memory operations

`Cload`/`Cstore` read and write memory via `read`/`write` (§2); `Calloc` and the
region operations are the subject of [`19`](19-cmm-memory-gc.md) and only their
shape is given here. What the resulting bytes *mean* as an OCaml value is `≈`
([`17`](17-representation.md)).

```rule
RULE CM.Load
STATUS normative
CODE backend/cmm.mli#Cload
CODE backend/cmm_helpers.ml#mk_load_immut
---
⟨Cop(Cload{memory_chunk = κ; mutability = μ; is_atomic = false}, [word a], dbg),
 ce, χ, M, TT, RR⟩
  ⟶c ⟨read(M, a, κ), ce, χ, M, TT, RR⟩
--------------------------------------------------
undef if read(M,a,κ) is undef (§2). Mutability μ is a hint (immutable loads are
CSE-/reorder-eligible, ch. 06/18); it does not change the value read.
```

```rule
RULE CM.Store
STATUS normative
CODE backend/cmm.mli#Cstore
CODE backend/cmm_helpers.ml#setfield_computed
---
⟨Cop(Cstore(κ, ι), [word a; v], dbg), ce, χ, M, TT, RR⟩
  ⟶c ⟨word 1 (unit), ce, χ, write(M, a, κ, v), TT, RR⟩
--------------------------------------------------
NOTES: A store writes memory. In the backend a bare `Cstore` produces NO value
register (void); `to_cmm` always emits it as the first arm of a
`Csequence(Cstore …, Cconst_int 1)` (return_unit / setfield_computed), so the Cmm
unit result (word 1, the tagged 0) actually comes from the trailing `Cconst_int 1`,
not from the store node itself. The model totalizes the bare store to `word 1` for
convenience; a store's value is never consumed on its own, so this is
observationally irrelevant. ι ∈ {Initialization, Assignment} selects whether a GC
write barrier (caml_modify) is needed; the barrier does not change M's value
semantics at this level (it maintains GC invariants, ch. 19). The Word_val/
pointer-store barrier discipline is why to_cmm emits caml_modify for value stores
(ch. 18, Block_set).
```

`Calloc` (heap or local), `Cbeginregion`, `Cendregion`: see
[`19`](19-cmm-memory-gc.md) (`CM.Alloc.*`, `CM.Region.*`).

## 7. Control: conditionals, switch, static catch/exit, traps

```rule
RULE CM.If
STATUS normative
CODE backend/cmm.mli#Cifthenelse
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
---
⟨Cifthenelse(word n, e_t, e_f), ce, χ, M, TT, RR⟩
  ⟶c ⟨e_t, …⟩  if n ≠ 0
  ⟶c ⟨e_f, …⟩  if n = 0
--------------------------------------------------
NOTES: `to_cmm` emits `Cifthenelse` (via C.ite) for two-armed Flambda switches
(to_cmm_expr.ml#switch), testing the untagged (or, when smaller, tagged)
scrutinee; arm 0 is the else branch.
```

```rule
RULE CM.Switch
STATUS normative
CODE backend/cmm.mli#Cswitch
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
CODE backend/cmm_helpers.ml#transl_switch_clambda
---
e_c = Cswitch(word n, index, [ce₀ … ce_m])     -- index : int array of length ≥ n+1
0 ≤ n < |index|
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨ce_{index[n]}, ce, χ, M, TT, RR⟩
NOTES: `to_cmm` builds `index` over [0, max_discriminant]; a discriminant present
in the source Flambda `Switch` indexes a real arm; a gap in [0,max] indexes a
synthesized `Cinvalid "unreachable switch case"` case (to_cmm_expr.ml#switch,
`needs_unreachable`); n outside [0,max] is out of the table entirely. This is the
Cmm image of OS.Switch / OS.Switch.Undef (04-opsem.md §5).
```

```rule
RULE CM.Catch.NonRec
STATUS normative
CODE backend/cmm.mli#Ccatch
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined
CODE backend/cmm_helpers.ml#create_ccatch
---
e_c = Ccatch(Normal, [⟨lbl, x̄, e_h, _⟩], e_body)
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩
  ⟶c ⟨e_body, ce, χ[lbl ↦ CHandler ⟨x̄, e_h, ce, χ, |TT|, Normal⟩], M, TT, RR⟩
NOTES: Install the handler and run the body. `χ_def = χ` (before adding lbl): a
Normal catch is non-recursive, matching OS.LetCont.NonRec. Cmm image of a
non-inlined, non-exn Flambda continuation.
```

```rule
RULE CM.Catch.Rec
STATUS normative
CODE backend/cmm.mli#Ccatch
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec
---
e_c = Ccatch(Recursive, [⟨lblᵢ, x̄ᵢ, e_iⁱ, _⟩]ᵢ, e_body)
χ′ = χ[ lblᵢ ↦ CHandler ⟨x̄ᵢ, e_iⁱ, ce, χ′, |TT|, Normal⟩ ]ᵢ
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_body, ce, χ′, M, TT, RR⟩
NOTES: Recursive catch: every handler's χ_def is the extended χ′ (a fixed point,
well-defined because entries only record χ′; cf. OS.LetCont.Rec). Cmm image of a
recursive Flambda continuation group (loop). to_cmm flushes before entering
(let_cont_rec) so no binding is duplicated into the loop.
```

```rule
RULE CM.Exit
STATUS normative
CODE backend/cmm.mli#Cexit
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
---
e_c = Cexit(lbl, [v̄], [])         -- no trap actions
χ(lbl) = CHandler ⟨x̄, e_h, ce_def, χ_def, d, kind⟩,  |x̄| = |v̄|
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_h, ce_def[x̄ ↦ v̄], χ_def, M, TT, RR⟩
NOTES: Static jump to an enclosing catch, binding its params to the argument
values, running in the handler's definition environments (analogue of
OS.ApplyCont). The pending evaluation context is discarded (§4). |TT| = d holds
by the same trap-balance invariant as Flambda (04 §1.7), maintained by the trap
actions below.
```

```rule
RULE CM.Exit.Trap
STATUS normative
CODE backend/cmm.mli#trap_action
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise
---
e_c = Cexit(lbl, [v̄], tā),   tā ≠ []
Executing tā left-to-right updates TT:  Push lbl_h  pushes lbl_h;  Pop lbl_h
  requires TT = lbl_h :: TT₀ and pops it, giving TT′.
χ(lbl) = CHandler ⟨x̄, e_h, ce_def, χ_def, d, _⟩,  |x̄| = |v̄|
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_h, ce_def[x̄ ↦ v̄], χ_def, M, TT′, RR⟩
NOTES: The Cmm image of a Flambda Apply_cont carrying trap actions
(OS.ApplyCont.TrapPush / TrapPop). to_cmm emits `Push`/`Pop` trap actions on the
`Cexit` exactly where the Flambda Apply_cont carried them
(translate_jump_to_continuation), preserving the trap stack in lockstep (ch. 16,
INV.ToCmm.Control).
```

```rule
RULE CM.Exit.Return
STATUS normative
CODE backend/cmm.mli#exit_label
CODE backend/cmm_helpers.ml#trap_return
---
e_c = Cexit(Return_lbl, [v̄], tā)      -- Return_lbl is the enclosing function's return
Executing tā left-to-right updates TT as in CM.Exit.Trap (a `Pop lbl_h` requires
TT = lbl_h :: TT₀ and pops it; to_cmm does not emit a `Push` on a return exit).
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ returns v̄ from the current function body, with TT updated by
tā (reaching the function boundary; CM.Unit.Final at the unit toplevel).
NOTES: `Return_lbl` (Cmm.exit_label) is the function's return, not a χ handler, so
CM.Exit does not apply. A return exit yields the function's result; `to_cmm` attaches
a `Pop` trap action via `trap_return` when the Flambda return continuation carried a
Pop (TC.ApplyCont.Return), discarding the popped handler before returning.
CM.Unit.Final is the special case tā = [] at the toplevel initialiser.
```

```rule
RULE CM.Catch.Exn
STATUS normative
CODE backend/cmm.mli#Exn_handler
CODE backend/cmm_helpers.ml#trywith
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler
---
e_c = Ccatch(Exn_handler, [⟨lbl_h, x_exn :: x̄_extra, e_h, _⟩], e_body)
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩
  ⟶c ⟨e_body, ce, χ[lbl_h ↦ CHandler ⟨x_exn :: x̄_extra, e_h, ce, χ, |TT|, Exn_handler⟩],
       M, TT, RR⟩
NOTES: An exception handler is a `Ccatch` with flag `Exn_handler`; installing it
does NOT touch TT — this rule is CM.Catch.NonRec with the kind tag `Exn_handler`.
There is no `Ctrywith` machine node: `Cmm.ctrywith` / `cmm_helpers.trywith` are smart
constructors that BUILD this `Ccatch(Exn_handler, …)` (its params are the exception
value `x_exn` followed by any extra args `x̄_extra`). All trap-stack motion for
exceptions is separate and explicit: a `Push lbl_h` trap action on the `Cexit`
entering the try body pushes lbl_h (CM.Exit.Trap), a matching `Pop lbl_h` pops it on
the normal exit, and a raise pops it (CM.Raise). Modelling the *entry* as pushing
lbl_h would double-push (the body already carries an explicit `Push`) and unbalance
TT. Cmm image of a Flambda is_exn_handler continuation (let_cont_exn_handler;
TC.LetCont.Exn).
```

```rule
RULE CM.Raise
STATUS normative
CODE backend/cmm.mli#Craise
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise
CODE backend/cmm_helpers.ml#raise_prim
---
e_c = Cop(Craise raise_kind, v_exn :: v̄_extra, dbg)
TT = lbl_h :: TT′
χ(lbl_h) = CHandler ⟨x_exn :: x̄_extra, e_h, ce_def, χ_def, d, Exn_handler⟩
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨e_h, ce_def[x_exn ↦ v_exn, x̄_extra ↦ v̄_extra], χ_def, M, TT′, RR⟩
NOTES: Raise pops the top trap frame and enters its handler with the exception
value, mirroring OS.ApplyCont.Raise. `to_cmm` compiles a Flambda raise (an
Apply_cont to an exn handler with a Pop trap action) to `raise_prim`
(translate_raise). The exception value is the first `Cop` operand and the extra
handler args are the remaining operands (`arg :: extra_args`); cfg_selectgen
(emit_expr_raise) later places them into distinguished per-handler registers —
`Proc.loc_exn_bucket` for the exception value plus the extra-arg registers from
`env_find_regs_for_exception_extra_args`. raise_kind affects only the backtrace.
```

## 8. Application

A call runs the callee as a *nested run*: the callee's body starts with fresh
variable and catch environments and a trap stack at its own base, against the
caller's memory and the caller's region stack RR (a callee may allocate into
the caller's open regions — local-returning functions). A nested run has
exactly three outcomes, each visible at the
call site: it returns (CM.Apply); it raises an exception that exhausts its own
trap stack (CM.Apply.Raise — the exception resumes unwinding in the caller,
matching the runtime's single global exception stack); or it diverges, in which
case the caller's run diverges with it (CM.Unit.Final). The callee's Cextern
events are part of the caller's trace.

```rule
RULE CM.Apply
STATUS normative
CODE backend/cmm.mli#Capply
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0
---
e_c = Cop(Capply{result_type; region; callees}, [w_code; v̄_args], dbg)
w_code addresses a function whose code, applied to v̄_args, RETURNS: the
nested run of that function's body (fresh variable and catch environments,
trap stack at its own base, region stack starting at the caller's RR) reaches
a Cexit(Return_lbl, [r̄], []) with memory M′, its trap stack balanced back to
its base and its region stack back at RR
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨r̄, ce, χ, M′, TT, RR⟩
NOTES: A Cmm call to the code pointed to by w_code (a code symbol for a direct
call, C.direct_call; or the code pointer read from a closure for an indirect
call, C.indirect_call / indirect_full_call). A nested run has exactly three
outcomes, each visible at the call site: it returns (this rule), it raises an
exception that exhausts its own trap stack (CM.Apply.Raise), or it diverges —
in which case the caller's run diverges with it (CM.Unit.Final). The callee's
Cextern events are part of the caller's trace. The nested run inherits the
caller's region stack and must return with it restored: to_cmm closes every
non-ghost region it opens on every normal exit, and trap frames are matched
likewise (the two balance clauses). The detailed calling convention
(caml_apply/caml_curry arity dispatch) is the Cmm realization of
OS.Apply.Indirect* and is not expanded here (its arity behaviour is described in
04-opsem.md §6.2). `region`/`result_type` are calling-convention detail; `callees`
records possible direct targets (Indirect_known_arity).
```

```rule
RULE CM.Apply.Raise
STATUS normative
CODE backend/cmm.mli#Capply
CODE backend/cmm.mli#Craise
---
e_c = Cop(Capply{result_type; region; callees}, [w_code; v̄_args], dbg)
w_code addresses a function whose code, applied to v̄_args, RAISES: the nested
run of that function's body (fresh variable and catch environments, trap stack
at its own base, region stack starting at the caller's RR) reaches a Craise of
v_exn at its base trap frame, with memory M′ and region stack RR′
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨Cop(Craise Raise_reraise, [v_exn], dbg), ce, χ, M′, TT, RR′⟩
NOTES: An exception escaping the callee resumes unwinding in the caller: the
call site steps to a re-raise of the escaped value, which the caller's TT then
handles via CM.Raise/CM.Catch.Exn (or which reaches the base frame as an
uncaught exception, CM.Unit.Final). This transcribes the runtime's single
global exception stack — there is no per-activation handler barrier — into the
nested-run presentation. Raise_reraise: only backtrace recording is affected
by the raise kind (16-to-cmm-control.md, TC.Raise NOTES). RR′, not RR: an
escaping raise does not implicitly close regions the callee opened — the
runtime's unwind restores only the exception-handler frame (caml_raise_exn /
RESTORE_EXN_HANDLER_OCAML, runtime/amd64.S: %rsp and exn_handler only;
caml_local_sp is untouched) — so they remain open in the caller and are
reclaimed by the explicit End_region / End_try_region in the handler that
catches the exception (CM.Region.End's pop-down-to-ι shape, [19](19-cmm-memory-gc.md)).
```

```rule
RULE CM.Extcall
STATUS normative
CODE backend/cmm.mli#Cextcall
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_external_call
---
e_c = Cop(Cextcall{func; ty; ty_args; alloc; effects; coeffects; …}, [v̄], dbg)
(r̄, M′) ∈ Cextern(func, v̄, M)         -- the axiomatized external, as OS.Apply.CCall
--------------------------------------------------
⟨e_c, ce, χ, M, TT, RR⟩ ⟶c ⟨r̄, ce, χ, M′, TT, RR⟩
NOTES: The Cmm image of a Flambda C_call (OS.Apply.CCall). `Cextern` is the same
axiomatized external relation as in 04-opsem.md §6.3; it is the only source of
observable I/O and external mutation. The observable event of an external call
is the full application `Cextern(func, v̄, M) ∋ (r̄, M′)` — the call-time memory
M is part of the trace observable named by CM.Unit.Final, not just the argument
words. Small integer results are sign-extended by
to_cmm (translate_external_call, maybe_sign_extend) because the C ABI does not
promise it. A raising external transfers to the current trap handler.
```

## 9. Invalid

```rule
RULE CM.Invalid
STATUS normative
CODE backend/cmm.mli#Cinvalid
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid
---
e_c = Cinvalid { message }
--------------------------------------------------
⟨Cinvalid{message}, ce, χ, M, TT, RR⟩ has no transition (undefined behaviour)
NOTES: Cmm image of Flambda `Invalid` (OS.Invalid) and of the unreachable switch
case (CM.Switch). `to_cmm` emits it via `C.invalid`, which in the compiler
lowers to a call to `caml_flambda2_invalid` (aborting); operationally we treat it
as stuck, as [04](04-opsem.md) does. A correct pipeline never reaches it
(ch. 20).
```

## 10. Observation and final states

The Cmm run of a translated unit begins at the module-initialiser function's
body (a `Cfunction` whose `fun_body` is the translation of the unit's toplevel
expression; [`16`](16-to-cmm-control.md), [`20`](20-to-cmm-soundness.md)) and
ends by reaching `Return_lbl` (normal) or exhausting the trap stack with a raise
(uncaught exception).

```rule
RULE CM.Unit.Final
STATUS normative
CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit
CODE backend/cmm.mli#Cdata
---
Normal termination:  a Cexit(Return_lbl, [v̄], []) with TT balanced to the base
    ⟶c halt; the observable module value is the bytes of the module block reachable
    from the module symbol in M.
Uncaught exception:  a Craise reaching the base trap frame ⟶c halt with v_exn.
--------------------------------------------------
A Cmm run is one of: normal termination (observing the module block image in M
and the trace of Cextern effects), termination by uncaught exception, divergence
(an infinite ⟶c sequence, or a run reaching a call whose nested run diverges,
hereditarily), undefined behaviour (reaching Cinvalid or a stuck read/store), or
RESOURCE EXHAUSTION (allocation failure / stack overflow — an outcome absent
from the Flambda machine; see ch. 20).
NOTES: The module block value at the module symbol is a CONCRETE byte image; it is
compared to the Flambda observation H(sym_mod) through the representation relation
`≈` (ch. 17), not directly. This is the target-side of OS.Unit.Final
(04-opsem.md §8.2). Resource exhaustion is the one observable outcome Cmm can
produce that Flambda cannot; ch. 20 records it in the simulation statement.
Divergence inside a nested run surfaces at the call site via CM.Apply's outcome
trichotomy; escaped exceptions surface as re-raises (CM.Apply.Raise), so the
uncaught-exception outcome needs no special nesting clause.
```

## 11. Summary of rules

Structural: `CM.Syntax.Fragment`, `CM.Context`, `CM.Op.Pure`, `CM.Op.TupleField`,
`CM.Load`, `CM.Store`, `CM.Mem.LoadStore`.

Control: `CM.If`, `CM.Switch`, `CM.Catch.NonRec`, `CM.Catch.Rec`, `CM.Exit`,
`CM.Exit.Trap`, `CM.Exit.Return`, `CM.Catch.Exn`, `CM.Raise`.

Application: `CM.Apply`, `CM.Extcall`.

Invalid / whole-unit: `CM.Invalid`, `CM.Unit.Final`.

Allocation, regions, and the GC (`CM.Alloc.*`, `CM.Region.*`) are in
[`19`](19-cmm-memory-gc.md).
