# Operational semantics

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter defines the dynamic semantics of Flambda 2 terms as a small-step
abstract machine. It owns the precise definitions of the machine configuration
`⟨e, ρ, K, H, T, R⟩` and the transition judgment

```
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e′, ρ′, K′, H′, T′, R′⟩
```

declared in [`README.md`](README.md). Primitive denotations `⟦p⟧(v̄; H)` are
owned by chapters [05](05-primitives-scalar.md) and
[06](06-primitives-memory.md); the `Let`-of-`Prim` rule here only invokes them.

The term grammar and binders are owned by [02](02-syntax.md); kinds and
well-formedness by [03](03-kinds.md). This chapter assumes terms are
well-formed (`Γ ⊢ e ok`) and works up to alpha-equivalence of bound variables
and continuations, exactly as the code does (`flambda.mli` says terms are
"represented up to alpha-conversion of bound variables and continuations").

Everything about algebraic effects (the `Effect` call kind and the fibre/stack
machinery), probes, and SIMD vector values is **out of scope**; see the scope
ledger in [01](01-overview.md). Those forms are enumerated below but not given
transitions.

## 1. Runtime values and machine state

### 1.1 Values

The machine manipulates *runtime values*. A value has a kind ([§03](03-kinds.md)); the
grammar below groups the value forms the semantics needs. Naked vectors and the
exotic array element kinds are elided (marked `…`); they behave like the other
naked numbers for the purposes of this chapter.

```
address   a  ::= ℓ            -- a dynamically-allocated heap location
               | sym          -- a symbol (statically-allocated address)

value     v  ::= tagged_imm n           -- kind Value: an OCaml "int"
               | naked_imm n            -- kind Naked_immediate (tags, switch scrutinees)
               | naked_int32 n | naked_int64 n | naked_nativeint n
               | naked_float f | naked_float32 f
               | naked_vec128 … | …     -- naked vectors (elided)
               | ptr a                  -- kind Value: pointer to the heap object at a
               | clos ℓ f               -- kind Value: pointer to function slot f of the
                                        --   set-of-closures block at ℓ
               | null                   -- kind Value: the null pointer (for `_ or_null`)
               | region ι               -- kind Region: a region handle
               | rec_info               -- an inert value standing for a Rec_info_expr.t
```

`clos ℓ f` is a distinguished pointer form because an OCaml closure pointer
targets a specific function slot within a (possibly multi-function) closure
block; `Move_within_set_of_closures` produces `clos ℓ f′` for a sibling slot `f′`
and `Project_value_slot` reads the shared environment of `ℓ` (denotations in
[§06](06-primitives-memory.md)).

### 1.2 Heap objects

```
heap object o ::=
    Block(t, μ, v̄)                 -- scannable block: tag t, mutability μ, fields v̄
  | Closures(funs, env)            -- a set of closures (see below)
  | Boxed(κ, c)                    -- a boxed naked number of kind κ
  | Array(ak, μ, v̄)                -- an array of element-kind ak
  | Bytes(μ, b̄)                    -- a string/bytes value (b̄ a byte sequence)
  | Code(cid ↦ code)               -- a piece of code (see §1.5)
  | …                              -- FloatBlock, MixedBlock, Bigstring, Lazy: see [§06](06-primitives-memory.md)

funs : Function_slot ⇀ (Code_id × arity-info)
env  : Value_slot ⇀ value
```

`μ` ranges over mutability (`Mutability.t`); `ak` over array element kinds. This
is the coarse taxonomy the operational rules below need; [§06](06-primitives-memory.md)
refines it (the value-prefix/flat-suffix split of mixed blocks, the float-block
representation, string vs. bigstring, lazy/forward blocks) and owns the primitive
denotations that read and write these objects.

A `Closures(funs, env)` object is the runtime image of a
`Set_of_closures.t`: each function slot maps to the code id it will call and the
arity information needed for indirect dispatch (§4.2), and each value slot holds
one captured value (the closure environment). The individual variables bound by
a set-of-closures `Let` name the function slots, i.e. values `clos ℓ f`, not the
block `ℓ` itself.

### 1.3 Heap H

```
H : (address ⊎ Code_id) ⇀ heap object
```

`H` maps heap locations and symbols to heap objects, and code ids to `Code`
objects. Symbols and code ids denote statically-allocated entities: they are
addresses that are *preallocated* once (by `OS.Let.Static`, §3.4) and thereafter
immutable except through the object's own mutability (mutable blocks, arrays,
strings). We write `H[a ↦ o]` for update and `H, a ↦ o` (with `a ∉ dom H`) for
extension at a fresh address.

### 1.4 Environment ρ

```
ρ : (Variable ⊎ Symbol) ⇀ value
```

`ρ` resolves variables and symbols to values. A symbol `sym` resolves to
`ptr sym` (a symbol is its own address); the object it denotes is `H(sym)`.
Code ids are not `Simple`s and so are not in `ρ`; they are resolved against `H`.

We write `⟦s⟧ρ` for the value of a `Simple` `s = x | sym | c` under `ρ`:

```rule
RULE OS.Simple.Eval
STATUS normative
CODE middle_end/flambda2/term_basics/simple.mli#t
CODE middle_end/flambda2/term_basics/coercion.mli#t
---
⟦x⟧ρ = ρ(x)          ⟦sym⟧ρ = ptr sym          ⟦c⟧ρ = the value denoted by c
⟦s @ co⟧ρ = ⟦s⟧ρ
--------------------------------------------------
⟦·⟧ρ is defined as above
NOTES: A `Simple` may carry a coercion `co`. Coercions record inlining depth and
closure-specialization information and are the identity on runtime values; they
are erased here. Constants `c` (`Reg_width_const.t`) denote the obvious value:
tagged immediates, naked immediates, and naked numbers.
```

### 1.5 The code table

Code is bound by `Let (Static)` under a `Code` pattern (`bound_static.mli`,
`Pattern.Code`) and lives in `H` keyed by code id. A `code` value is the
runtime-relevant projection of a `Function_params_and_body.t` together with its
`Code_metadata.t`:

```
code ::= { return_continuation : k_ret;
           exn_continuation    : k_exn;
           params              : x̄;
           body                : e;
           my_closure          : x_clos;
           my_depth            : x_depth;
           my_alloc_mode       : (Heap | Local{region = x_reg; ghost_region = x_greg});
           params_arity        : ᾱ;          -- Code_metadata.params_arity
           is_tupled           : bool;        -- Code_metadata.is_tupled
           result_arity        : β̄;           -- Code_metadata.result_arity
           result_mode         : (Heap | Local) }
```

`params`, `my_closure`, `my_depth`, and the region variables of
`my_alloc_mode` are bound in `body`, as are the two continuations `k_ret`
(return) and `k_exn` (exception); see `Function_params_and_body`. A function
body's only free continuations are `k_ret` and `k_exn` (functions do not capture
continuations); its free variables are among `params`, the `my_*` variables, and
whatever symbols/code ids are in scope.

### 1.6 Continuation environment K

```
K : Continuation ⇀ entry

entry ::= Handler ⟨x̄, e, ρ_def, K_def, d⟩          -- an ordinary continuation
        | Return  ⟨x̄, dst, ρ_c, K_c, T_c, R_c⟩     -- a function's return boundary
        | Exn     ⟨x_b, k_x, v̄_extra, K_c, T_c, R_c⟩ -- a function's exn boundary
```

A `Handler` entry records its parameters `x̄`, handler body `e`, the value
environment `ρ_def` and continuation environment `K_def` in scope at the point
of definition, and `d`, the depth of the trap stack at the definition
(§1.7). Because continuations are **second-class** and are only ever entered
from within the region of the term they dominate, `ρ_def` and `K_def` are
*sub-environments* of the environments in force at every jump to the
continuation. Consequently no runtime closure is allocated for a continuation:
the backend simply reuses the variables already live at the jump. Recording
`ρ_def`/`K_def` is the denotational way to express that reuse (informative).

`Return` and `Exn` entries are *boundary* continuations installed by `Apply`
(§4). They carry the caller's context (`K_c`, `T_c`, `R_c`, and for `Return` the
caller's destination `dst : Result_continuation`; for `Exn` the caller's handler
`k_x` and the pre-evaluated extra-argument values `v̄_extra`). Entering a
boundary continuation *restores* the caller's trap stack and region stack; this
is what makes a function return or an escaping exception unwind correctly (§4.1).

### 1.7 Trap stack T

```
T ::= [] | k :: T          -- a stack of exception-handler continuations
```

`T` is the runtime trap stack described in
[`backend_exceptions.md`](../backend_exceptions.md). Each frame is the name of
an exception-handler continuation (a `Non_recursive` continuation whose
`is_exn_handler` flag is set). It is pushed by a `Push` trap action and popped by
a `Pop` trap action or by raising (§3.6). The stored depth `d` in a `Handler`
entry is `|T|` at the continuation's definition; the invariant that a
continuation is always re-entered with a trap stack of that same depth is what
lets the backend assign every program point a single, statically known trap
context. Trap actions on an `Apply_cont` are exactly the adjustments that make
the current trap stack match the target continuation's expected context.

### 1.8 Region stack R

```
R ::= [] | ι :: R          -- a stack of live local-allocation regions
```

`R` is the stack of currently-open local-allocation regions. `Begin_region`
pushes a fresh region handle `ι`; `End_region` pops down to (and discards) a
named region, reclaiming everything allocated in it (denotations in [§06](06-primitives-memory.md)).
"Allocating in a region `ι`" means the fresh heap object is tied to `ι` and is
reclaimed when `ι` is ended; allocating "on the heap" ties the object to no
region and leaves it for the GC. Function calls do not by themselves change `R`;
a local-returning callee allocates its result into a region supplied by the
caller through the `Apply`'s `alloc_mode` (§4.1).

## 2. Shape of the rules

Every configuration whose expression is a `Let`, `Let_cont`, `Apply_cont`,
`Switch`, `Apply`, or `Invalid` is handled below. `Let` and `Apply_cont` are the
only forms that "compute": `Let` evaluates a `named` and rebinds; `Apply_cont`
and `Apply` transfer control. There are no evaluation contexts and no nested
subexpressions to descend into — the language is in ANF, so every operand is a
`Simple` evaluated by `⟦·⟧ρ`.

## 3. Structural transitions

### 3.1 Let of a Simple

```rule
RULE OS.Let.Simple
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Let_expr
CODE middle_end/flambda2/terms/flambda.mli#Named
---
Let_expr.pattern_match : pattern = Singleton x, defining_expr = Simple s, body = e
v = ⟦s⟧ρ
--------------------------------------------------
⟨Let (x = Simple s) e, ρ, K, H, T, R⟩ ⟶ ⟨e, ρ[x ↦ v], K, H, T, R⟩
```

### 3.2 Let of a pure primitive

```rule
RULE OS.Let.Prim.Pure
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Named
CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
CODE middle_end/flambda2/terms/effects.mli#t
---
defining_expr = Prim (p, dbg),  pattern = Singleton x
p has No_effects or Only_generative_effects
⟦p⟧(⟦s̄⟧ρ; H) = (v, H′)          -- s̄ the argument Simples of p
--------------------------------------------------
⟨Let (x = Prim (p, dbg)) e, ρ, K, H, T, R⟩ ⟶ ⟨e, ρ[x ↦ v], K, H′, T, R⟩
NOTES: A `No_effects` primitive leaves the observable world unchanged (H′ = H
except possibly for fresh allocation of an immutable result), may be duplicated
or dropped; an `Only_generative_effects` primitive may allocate (so H′ extends H)
but is otherwise pure. The two are handled by the same transition; the
distinction matters only to Simplify (chapters 09–10). Region-affecting
primitives `Begin_region`/`End_region` also match here but additionally update
`R`; see [§06](06-primitives-memory.md) for their `⟦·⟧` and the corresponding `R` update.
```

### 3.3 Let of an effectful primitive

```rule
RULE OS.Let.Prim.Effect
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Named
CODE middle_end/flambda2/terms/flambda_primitive.mli#effects_and_coeffects
---
defining_expr = Prim (p, dbg),  pattern = Singleton x
p has Arbitrary_effects
⟦p⟧(⟦s̄⟧ρ; H) = (v, H′)
--------------------------------------------------
⟨Let (x = Prim (p, dbg)) e, ρ, K, H, T, R⟩ ⟶ ⟨e, ρ[x ↦ v], K, H′, T, R⟩
NOTES: Same transition as OS.Let.Prim.Pure; separated because Simplify may
neither drop nor duplicate an `Arbitrary_effects` application. If `⟦p⟧(v̄;H) =
undef` the machine is stuck (undefined behaviour), matching an out-of-bounds
access or similar; see [§06](06-primitives-memory.md). Primitives do not change control flow: a
primitive never itself jumps to a continuation. (A primitive that "can raise",
e.g. a checked division, is modelled in [§06](06-primitives-memory.md) as `undef` on the bad input,
not as a control transfer; genuine OCaml-level raising goes through
`Apply`/`Apply_cont`, §3.6 and §4.)
```

### 3.4 Let binding a set of closures (dynamic allocation)

```rule
RULE OS.Let.SetOfClosures
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Named
CODE middle_end/flambda2/terms/set_of_closures.mli#create
CODE middle_end/flambda2/bound_identifiers/alloc_mode.mli#For_allocations
---
defining_expr = Set_of_closures (soc, am),  pattern = Set_of_closures [x₁ … xₙ]
function_decls soc = (f₁ ↦ cid₁, …, fₘ ↦ cidₘ)   -- in slot order; Deleted slots omitted
value_slots soc = (w ↦ sᵥ)ᵥ
env = (w ↦ ⟦sᵥ⟧ρ)ᵥ
funs = (fⱼ ↦ (cidⱼ, arity-info from H(cidⱼ)))ⱼ
ℓ fresh          o = Closures(funs, env)
am = Heap  ⟹  H′ = (H, ℓ ↦ o), R unchanged
am = Local{region = x_r}  ⟹  H′ = (H, ℓ ↦ o) with ℓ allocated in region ρ(x_r)
[x₁ … xₙ] name the bound function slots in order: xᵢ ↦ clos ℓ fᵢ
--------------------------------------------------
⟨Let (x̄ = Set_of_closures (soc, am)) e, ρ, K, H, T, R⟩
  ⟶ ⟨e, ρ[xᵢ ↦ clos ℓ fᵢ], K, H′, T, R⟩
NOTES: One block is allocated for the whole set; the `n` bound variables all
point into it at their respective function slots. `alloc_mode` (an
`Alloc_mode.For_allocations.t`) selects heap vs. local allocation; a local set
of closures is allocated in the innermost caller-named region. The captured
environment `env` is computed eagerly from the value-slot `Simple`s at
allocation time.
```

### 3.5 Let binding static constants (symbol definition)

```rule
RULE OS.Let.Static
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Named
CODE middle_end/flambda2/terms/static_const.mli#t
CODE middle_end/flambda2/terms/flambda.mli#Static_const_or_code
CODE middle_end/flambda2/bound_identifiers/bound_static.mli#Pattern
---
defining_expr = Static_consts g,  pattern = Static bs
match_against_bound_static g bs pairs each piece with its bound symbol / code id:
  • Code cid ↦ code            ⟹  H₁ = H[cid ↦ Code(cid ↦ code)]
  • Deleted_code cid           ⟹  no object installed (code was removed)
  • Set_of_closures at symbols (fⱼ ↦ symⱼ)  ⟹  allocate one Closures block; each symⱼ ↦ clos ℓ fⱼ
  • Block_like sym ↦ Static_const sc  ⟹  allocate the object described by sc at sym
each Or_variable field `Var x` is read from ρ(x); each `Const c` is the constant c
H′ = H extended at all the above symbols and code ids (all fresh, allocated once)
--------------------------------------------------
⟨Let (bs = Static_consts g) e, ρ, K, H, T, R⟩ ⟶ ⟨e, ρ[symⱼ ↦ ptr symⱼ …], K, H′, T, R⟩
NOTES: Symbols and code ids are dominator-scoped, not syntactically scoped
(`bound_pattern.mli`, `Static`): they are considered in scope throughout the
term dominated by this binding. The objects are statically allocated — installed
into H once, never re-run — and are always on the heap (never in a region).
`Static_const.t` covers blocks (with tag/shape/mutability), boxed numbers,
immutable and mutable arrays and strings, and sets of closures; each `Or_variable`
"hole" is filled from `ρ` at install time. `Bound_static` requires every
recursive cycle among the defined names to pass through a code id, so the
one-shot installation is well-defined despite mutual reference.
```

### 3.6 Let binding recursion state (Rec_info)

```rule
RULE OS.Let.RecInfo
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Named
CODE middle_end/flambda2/term_basics/rec_info_expr.mli#t
---
defining_expr = Rec_info ri,  pattern = Singleton x
--------------------------------------------------
⟨Let (x = Rec_info ri) e, ρ, K, H, T, R⟩ ⟶ ⟨e, ρ[x ↦ rec_info], K, H, T, R⟩
NOTES: `Rec_info` records a state of recursive inlining and has no runtime
content. The machine binds `x` to the inert value `rec_info` so that later
`Simple` references remain well-defined; the value is never inspected. Equivalent
to erasing the binding entirely (the backend does not emit anything for it).
```

## 4. Continuations

### 4.1 Defining continuations

```rule
RULE OS.LetCont.NonRec
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
CODE middle_end/flambda2/terms/flambda.mli#Non_recursive_let_cont_handler
CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
---
e = Let_cont (Non_recursive { handler = h over k, body = e_body, … })
Continuation_handler.pattern_match h = (params x̄, handler body e_h)
entry = Handler ⟨x̄, e_h, ρ, K, |T|⟩          -- K_def = K: the handler cannot see k
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_body, ρ, K[k ↦ entry], H, T, R⟩
NOTES: The handler is scoped over `body`, so we install `k` and continue with the
body. `K_def = K` (the environment *before* adding `k`) because a non-recursive
handler cannot refer to itself. If `k` does not occur in `body` the frontend
elides the `Let_cont` entirely (`create_non_recursive`), so this rule only fires
when `k` is used.
```

```rule
RULE OS.LetCont.Rec
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
CODE middle_end/flambda2/terms/flambda.mli#Recursive_let_cont_handlers
CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation
CODE middle_end/flambda2/simplify/apply_cont_rewrite.ml#get_used_params
---
e = Let_cont (Recursive handlers)
Recursive_let_cont_handlers.pattern_match handlers = (invariant_params z̄, body e_body, hs)
Continuation_handlers.to_map hs = (kᵢ ↦ hᵢ)ᵢ,   Continuation_handler.pattern_match hᵢ = (x̄ᵢ, e_i)
K′ = K[ kᵢ ↦ Handler ⟨z̄ ++ x̄ᵢ, e_i, ρ, K′, |T|⟩ ]ᵢ      -- K_def = K′: handlers see each other and themselves
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_body, ρ, K′, H, T, R⟩
NOTES: The definition is recursive: every handler's `K_def` is the extended `K′`,
so the group may call itself and its siblings. `K′` is well-defined as a
fixed point because entries only *record* `K′`; they do not force it. The
`invariant_params z̄` prefix every handler's parameter list and are passed at each
call (§4.2). "Invariant" is a Simplify-level assumption — that the same values
are passed for `z̄` on every recursive call — not enforced by this rule.
The calling convention (invariant params bound as a leading prefix of each
handler's parameters, supplied at every `Apply_cont`) is confirmed:
`make_rewrite_for_recursive_continuation` builds the apply-cont rewrite with
`original_params = invariant_params ++ variant_params`, and
`Apply_cont_rewrite.get_used_params` partitions accordingly, so every
`Apply_cont` to the group supplies the invariant args as a prefix followed by
that handler's own args.
```

### 4.2 Applying a continuation

`Apply_cont k (s̄)` binds the argument values to `k`'s parameters and enters its
handler in the *definition* environment recorded for `k`.

```rule
RULE OS.ApplyCont
STATUS normative
CODE middle_end/flambda2/terms/apply_cont_expr.mli#create
CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
---
e = Apply_cont k (s̄),  trap_action(e) = None
K(k) = Handler ⟨x̄, e_h, ρ_def, K_def, d⟩
v̄ = ⟦s̄⟧ρ,   |x̄| = |v̄|
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_h, ρ_def[x̄ ↦ v̄], K_def, H, T, R⟩
NOTES: The handler runs in `ρ_def`/`K_def` (its definition-time environments)
extended by the arguments, not in the caller's `ρ`/`K`. The trap stack `T` and
region stack `R` are unchanged (no trap action). The invariant of §1.7 says
`|T| = d`; the frontend inserts trap actions precisely to maintain this. A zero-
arity `Apply_cont` is a plain "goto".
```

Boundary continuations (installed by `Apply`, §4.3) are entered by the same
`Apply_cont` syntax but take these two rules instead:

```rule
RULE OS.ApplyCont.Return
STATUS normative
CODE middle_end/flambda2/terms/apply_expr.mli#Result_continuation
CODE middle_end/flambda2/terms/flambda.mli#Function_params_and_body
---
e = Apply_cont k_ret (s̄)
K(k_ret) = Return ⟨x̄, dst, ρ_c, K_c, T_c, R_c⟩
v̄ = ⟦s̄⟧ρ
dst = Return k_c
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨Apply_cont k_c (values v̄), ρ_c, K_c, H, T_c, R_c⟩
NOTES: Returning from a function: the callee reaches its return continuation
`k_ret` with results `v̄`; control transfers to the caller's destination `k_c`
with the caller's trap and region stacks restored. (Formally we re-enter with a
synthetic `Apply_cont k_c` carrying the already-computed values; equivalently,
directly bind `k_c`'s parameters to `v̄` via OS.ApplyCont in `K_c`.) If `dst =
Never_returns` the callee promised not to return, so reaching `k_ret` is
undefined behaviour (OS.Invalid); see OS.Apply.NeverReturns.
```

```rule
RULE OS.ApplyCont.ExnBoundary
STATUS normative
CODE middle_end/flambda2/terms/exn_continuation.mli#t
CODE middle_end/flambda2/terms/exn_continuation.mli#extra_args
---
e = Apply_cont k_exn (s_b)          -- raised bucket, plus possibly extra args already in s̄
K(k_exn) = Exn ⟨x_b, k_x, v̄_extra, K_c, T_c, R_c⟩
v_b = ⟦s_b⟧ρ
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨Apply_cont k_x (values (v_b :: v̄_extra)), _, K_c, H, T_c, R_c⟩
NOTES: An exception escaping the callee reaches the callee's exn continuation
`k_exn`; control transfers to the caller's handler `k_x` with the exception
bucket followed by the caller's extra arguments `v̄_extra` (evaluated in the
caller's `ρ` at the call site — see §4.3), and with the caller's trap/region
stacks restored. Ordering `(bucket :: extra_args)` matches `close_raise0` and the
try-with handler parameters in `lambda_to_flambda.ml`.
```

### 4.3 Trap actions: try/with and raise

Trap actions are always attached to an `Apply_cont` and executed *before* the
jump (`trap_action.mli`). Entering a `try … with` pushes the handler; leaving the
non-exceptional path pops it; raising pops it and jumps to it.

```rule
RULE OS.ApplyCont.TrapPush
STATUS normative
CODE middle_end/flambda2/terms/trap_action.mli#t
CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps
VERIFIED 14-validation/new-06-trap.md
---
e = Apply_cont k (s̄),  trap_action(e) = Some (Push { exn_handler = k_h })
K(k) = Handler ⟨x̄, e_h, ρ_def, K_def, d⟩
v̄ = ⟦s̄⟧ρ,   T′ = k_h :: T
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_h, ρ_def[x̄ ↦ v̄], K_def, H, T′, R⟩
NOTES: `k_h` names a `Non_recursive`, `is_exn_handler` continuation already in
`K`. The push installs it as the innermost handler, then jumps to `k` (the body
of the try). Generated as the entry edge of a `Ltrywith` body.
```

```rule
RULE OS.ApplyCont.TrapPop
STATUS normative
CODE middle_end/flambda2/terms/trap_action.mli#t
CODE middle_end/flambda2/terms/apply_cont_expr.ml#is_raise
VERIFIED 14-validation/new-06-trap.md
---
e = Apply_cont k (s̄),  trap_action(e) = Some (Pop { exn_handler = k_h; raise_kind })
k ≠ k_h                                   -- not a raise (cf. is_raise)
K(k) = Handler ⟨x̄, e_h, ρ_def, K_def, d⟩
v̄ = ⟦s̄⟧ρ,   T = k_h :: T′
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_h, ρ_def[x̄ ↦ v̄], K_def, H, T′, R⟩
NOTES: The non-exceptional exit of a `try … with`: pop the try's handler `k_h`
off the trap stack and continue to the join continuation `k` (≠ `k_h`) with the
body's result. The `Pop`'s `exn_handler` need not equal the target `k` (the note
on `trap_action.mli` `Pop`); it names *which* frame is popped.
```

```rule
RULE OS.ApplyCont.Raise
STATUS normative
CODE middle_end/flambda2/terms/apply_cont_expr.ml#is_raise
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_raise0
CODE middle_end/flambda2/terms/trap_action.mli#Raise_kind
---
e = Apply_cont k_h (s̄),  trap_action(e) = Some (Pop { exn_handler = k_h; raise_kind })
                                          -- target equals the popped handler: is_raise
v̄ = ⟦s̄⟧ρ                                  -- v̄ = exception bucket :: extra args
T = k_h :: T′
K(k_h) = Handler ⟨x̄, e_h, ρ_def, K_def, d⟩
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_h, ρ_def[x̄ ↦ v̄], K_def, H, T′, R⟩
NOTES: Raising an exception. `close_raise0` compiles `raise e` to `Apply_cont
k_h (bucket :: extra_args)` with `Pop { exn_handler = k_h; … }`, where `k_h` is
the current exception handler — i.e. the top of `T`. So a raise pops the top
frame and jumps to it, carrying the bucket and the handler's extra arguments.
`raise_kind ∈ {Regular, Reraise, No_trace}` affects only the backtrace recorded
by the runtime, not the value or control flow, and is otherwise ignored here.
When `k_h` is a callee's exn continuation, it is an `Exn` boundary entry and
OS.ApplyCont.ExnBoundary fires instead, forwarding to the caller's handler.
```

## 5. Switch

```rule
RULE OS.Switch
STATUS normative
CODE middle_end/flambda2/terms/switch_expr.mli#create
CODE middle_end/flambda2/terms/switch_expr.mli#arms
---
e = Switch { scrutinee = s; arms }
⟦s⟧ρ = naked_imm n
arms(n) = ac        -- an Apply_cont_expr.t
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨Apply_cont ac, ρ, K, H, T, R⟩
NOTES: The scrutinee is of kind `Naked_immediate` (`switch_expr.mli`). The arm
selected by the discriminant `n` is itself an `Apply_cont` (possibly with a trap
action), which then steps by the §4 rules. A `Switch` has at least two arms and
no default case.
```

```rule
RULE OS.Switch.Undef
STATUS normative
CODE middle_end/flambda2/terms/switch_expr.mli#create
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
---
e = Switch { scrutinee = s; arms }
⟦s⟧ρ = naked_imm n
n ∉ dom(arms)
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ is stuck (undefined behaviour)
NOTES: There is no default case (`switch_expr.mli`). A well-typed, well-formed
program guarantees the discriminant is always one of the arms; if not, behaviour
is undefined, exactly as for OS.Invalid. This is confirmed by the to_cmm
lowering (`to_cmm_expr.ml#switch`): it builds an index table over
`[0, max_discriminant]`, and any gap (a value below the maximum with no arm)
is directed to an explicit `Cmm` "unreachable switch case" invalid; a value
*above* the maximum discriminant is out of the jump table entirely (genuine
UB). Neither is a control transfer to a real handler, matching "stuck". Simplify
may replace a `Switch` whose scrutinee is known to miss all arms with `Invalid`.
```

## 6. Application

An `Apply` (`apply_expr.mli`) has a callee `Simple option`, argument `Simple`s,
a `Result_continuation` (`Return k` or `Never_returns`), an `Exn_continuation`
(handler `k_x` plus extra args), a `Call_kind`, and an `alloc_mode`
(`Alloc_mode.For_applications.t`, giving the region for a local-returning call).

### 6.1 Direct function calls

```rule
RULE OS.Apply.Direct
STATUS normative
CODE middle_end/flambda2/terms/apply_expr.mli#create
CODE middle_end/flambda2/terms/call_kind.mli#Function_call
CODE middle_end/flambda2/terms/flambda.mli#Function_params_and_body
CODE middle_end/flambda2/terms/code_metadata.mli#params_arity
---
e = Apply { callee = Some s_f; args = s̄; call_kind = Function (Direct cid);
            continuation = dst; exn_continuation = ⟨k_x, ē_extra⟩;
            alloc_mode = am }
⟦s_f⟧ρ = clos ℓ f                     -- the closure value
H(cid) = Code(cid ↦ code)
code = { return_continuation = k_ret; exn_continuation = k_exn; params = x̄;
         body = e_body; my_closure = x_clos; my_depth = x_depth;
         my_alloc_mode = mam; … }
v̄ = ⟦s̄⟧ρ,  |x̄| = |v̄|                  -- args_arity matches params_arity (else OS.Invalid)
v̄_extra = ⟦ē_extra⟧ρ                   -- extra args evaluated now, in the caller
ρ_body = ρ_code[ x̄ ↦ v̄, x_clos ↦ clos ℓ f, x_depth ↦ rec_info,
                 region vars of mam ↦ regions from am ]
K_body = { k_ret ↦ Return ⟨x̄_r, dst, ρ, K, T, R⟩,
           k_exn ↦ Exn ⟨x_b, k_x, v̄_extra, K, T, R⟩ }
T_body = [k_exn]                        -- the activation's base trap frame
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e_body, ρ_body, K_body, H, T_body, R⟩
NOTES: `ρ_code` supplies the symbols/code ids visible to the body. `my_closure`
is bound to the callee closure so the body can project its own value slots and
sibling function slots; `my_depth` to the inert `rec_info`. If the call is local
(`am = Local{region; ghost_region}`), the body's `my_alloc_mode` region variables
are bound to those caller regions and the result is allocated there; if `am =
Heap`, `mam` is `Heap` and there is nothing to bind. The callee runs with a fresh
trap stack whose base `k_exn` forwards escaping exceptions to the caller
(OS.ApplyCont.ExnBoundary); it returns through `k_ret` (OS.ApplyCont.Return).
The body's free continuations are exactly `k_ret` and `k_exn`, so `K_body` need
carry nothing else.
```

### 6.2 Indirect calls: runtime arity dispatch

For indirect calls the code id is not known statically; it is read from the
closure's function slot, and the number of supplied arguments is compared to the
callee's arity. The three cases — full, partial, and over-application — are what
`caml_applyN` (see [`../to_cmm.md`](../to_cmm.md)) implements at runtime.

```rule
RULE OS.Apply.IndirectUnknownArity.Full
STATUS normative
CODE middle_end/flambda2/terms/call_kind.mli#Function_call
CODE middle_end/flambda2/terms/code_metadata.mli#params_arity
---
e = Apply { callee = Some s_f; args = s̄; call_kind = Function Indirect_unknown_arity;
            continuation = dst; exn_continuation = ⟨k_x, ē_extra⟩; alloc_mode = am }
⟦s_f⟧ρ = clos ℓ f
H(ℓ) = Closures(funs, env),  funs(f) = (cid, arity-info),  H(cid) = Code(cid ↦ code)
|s̄| = arity of code (fully applied)
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ (as OS.Apply.Direct with this cid, s̄, s_f, dst, k_x, ē_extra, am)
NOTES: When the argument count matches the callee's arity, an indirect call
behaves exactly like a direct call to the code id found in the closure. Tupled
functions (`code_metadata.is_tupled`) expect their arguments as a single tuple
block that the generic-apply path unpacks; the "arity" compared here is the one
after that adjustment.
```

```rule
RULE OS.Apply.IndirectUnknownArity.Partial
STATUS conjectured
CODE middle_end/flambda2/terms/call_kind.mli#Function_call
CODE middle_end/flambda2/terms/code_metadata.mli#first_complex_local_param
CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_partial_application
---
e = Apply { callee = Some s_f; args = s̄; call_kind = Function Indirect_unknown_arity;
            continuation = Return k_c; exn_continuation = _; alloc_mode = am }
⟦s_f⟧ρ = clos ℓ f,  funs(f) = (cid, arity-info),  arity = m,  |s̄| = j,  0 < j < m
ℓ_pa fresh    o = a "partial application" closure recording (clos ℓ f, ⟦s̄⟧ρ) and
                  expecting the remaining m − j arguments
H′ = (H, ℓ_pa ↦ o)         -- allocated on the heap, or in am's region if local
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨Apply_cont k_c (clos ℓ_pa f_pa), ρ, K, H′, T, R⟩
NOTES: Too few arguments: allocate a closure that, when later applied to the
remaining arguments, performs the original call. The wrapper shape is grounded
for the *direct* case: `simplify_direct_partial_application` converts `f a b`
into `let f_partial x y z = f a b x y z in f_partial`, a fresh closure whose
value slots capture the already-applied args (constants/uncoerced symbols are
left out) and whose remaining params are `param_arity` minus the supplied
prefix. Its allocation mode is the application's `alloc_mode`, checked against
the callee's parameter modes (`first_complex_local_param`, `param_modes`): if
the supplied args stop at or before the first complex local param the closure is
forced onto the heap, and a genuine mode mismatch yields
`Expr.create_invalid (Partial_application_mode_mismatch (apply, …))`. Still
`conjectured` because the rule is stated for the *runtime* generic
(`Indirect_unknown_arity`) path via `caml_curry`, whose precise wrapper shape
and allocation region are not verified line-by-line against the direct-case
model above.
```

```rule
RULE OS.Apply.IndirectUnknownArity.Over
STATUS conjectured
CODE middle_end/flambda2/terms/call_kind.mli#Function_call
CODE middle_end/flambda2/terms/apply_expr.mli#return_arity
CODE middle_end/flambda2/simplify/simplify_common.ml#split_direct_over_application
---
e = Apply { callee = Some s_f; args = s̄; call_kind = Function Indirect_unknown_arity;
            continuation = Return k_c; exn_continuation = ⟨k_x, ē_extra⟩; alloc_mode = am }
⟦s_f⟧ρ = clos ℓ f,  funs(f) = (cid, arity-info),  arity = m,  |s̄| = j,  j > m
s̄ = s̄₁ ++ s̄₂,  |s̄₁| = m,  |s̄₂| = j − m
k_mid fresh
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶
  ⟨Apply { callee = s_f; args = s̄₁; continuation = Return k_mid;
           exn_continuation = ⟨k_x, ē_extra⟩; call_kind = Function Indirect_unknown_arity;
           alloc_mode = am },
   ρ,
   K[ k_mid ↦ Handler ⟨[g], (Apply { callee = g; args = s̄₂; continuation = Return k_c;
                                     exn_continuation = ⟨k_x, ē_extra⟩;
                                     call_kind = Function Indirect_unknown_arity;
                                     alloc_mode = am }), ρ, K, |T| ⟩ ],
   H, T, R⟩
NOTES: Too many arguments: call the function with its first `m` arguments,
binding the (function) result to a fresh continuation `k_mid` that then applies
the remaining `s̄₂`. Both sub-applications share the caller's exn continuation.
The split is grounded for the *direct* case by
`Simplify_common.split_direct_over_application`: it splits the args at
`cardinal_unarized callee's_params_arity`, issues the first (full) call to a
fresh continuation `after_full_application` binding one result `func_var`, then
performs the second call `func_var remaining_args` as an
`Indirect_unknown_arity` application to the original return continuation, all
sharing `Apply.exn_continuation apply`. `Over_application_never_returns` is the
`Invalid` used both when the first call's result arity is not a single value and
when the whole over-application never returns. (A local-returning callee also
gets a fresh `Begin_region`/`End_region` wrapping both calls — a detail the rule
elides.) Still `conjectured`: the rule is stated for the runtime generic
(`caml_apply`) path, not verified line-by-line, and the region wrapping is not
modelled.
```

```rule
RULE OS.Apply.IndirectKnownArity
STATUS normative
CODE middle_end/flambda2/terms/call_kind.mli#Function_call
---
e = Apply { callee = Some s_f; call_kind = Function (Indirect_known_arity code_ids); … }
⟦s_f⟧ρ = clos ℓ f,  funs(f) = (cid, arity-info)
(code_ids = Known S  ⟹  cid ∈ S)
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ (as the Indirect_unknown_arity rules, using cid and the callee arity)
NOTES: The argument arity is statically known to match the callee's arity, so
only the full-application case applies; the optional `Code_id.Set.t` records the
set of possible callees (all with the same arity) without fixing which. Semantics
are otherwise identical to a full indirect call.
```

### 6.3 External (C) calls

```rule
RULE OS.Apply.CCall
STATUS normative
CODE middle_end/flambda2/terms/call_kind.mli#t
CODE middle_end/flambda2/terms/apply_expr.mli#create
---
e = Apply { callee = Some s_f; args = s̄; call_kind = C_call { effects; coeffects; … };
            continuation = Return k_c; exn_continuation = ⟨k_x, ē_extra⟩ }
v̄ = ⟦s̄⟧ρ
the external returns normally with results r̄ and heap H′:  (r̄, H′) ∈ Cextern(s_f, v̄, H)
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ ⟨Apply_cont k_c (r̄), ρ, K, H′, T, R⟩
NOTES: A C call is an opaque external. It is axiomatized as an arbitrary relation
`Cextern` that, given the callee, argument values, and heap, may read and mutate
any part of `H` (subject to its declared `effects`/`coeffects`) and yields result
value(s) and a new heap. This is the only place ordinary I/O and externally-
visible mutation enter the semantics (§7). A C call may also raise: in that case
it instead steps to `⟨Apply_cont k_x (v_exn :: v̄_extra), ρ, K, H′, T, R⟩` for
some exception value `v_exn` — a second axiomatized outcome. `needs_caml_c_call`
and `is_c_builtin` are backend calling-convention details with no effect on this
semantics.
```

### 6.4 Method calls and effect operations

```rule
RULE OS.Apply.Method
STATUS conjectured
CODE middle_end/flambda2/terms/call_kind.mli#Method_kind
---
e = Apply { call_kind = Method { kind; obj = s_obj }; args = s̄; continuation = Return k_c; … }
⟦s_obj⟧ρ = the object value;  method dispatch on `kind ∈ {Self, Public, Cached}`
resolves to a closure, which is then applied to s̄ (as an indirect call)
--------------------------------------------------
⟨e, ρ, K, H, T, R⟩ ⟶ (indirect application of the resolved method closure to s̄)
NOTES: Method calls arise from OCaml's object system; the middle end treats them
descriptively — the method `kind` selects a lookup strategy (self, public
dispatch, or a cached slot) that produces a closure, applied like any indirect
call. Marked `conjectured`: object-system lowering is not modelled in detail
here and the resolution is not grounded against the code.
```

```rule
RULE OS.Apply.Effect
STATUS conjectured
CODE middle_end/flambda2/terms/call_kind.mli#Effect
---
call_kind = Effect (Perform | Reperform | Resume | With_stack | With_stack_preemptible)
--------------------------------------------------
out of scope: no transition is given
NOTES: Algebraic-effect operations manipulate delimited fibres/stacks. Their
callee is `None` with an empty argument list (`call_kind.mli`, `Effect`), the
operands living in the constructor. Modelling them requires a fibre component
absent from `⟨e, ρ, K, H, T, R⟩`; deferred to the scope ledger ([§01](01-overview.md)).
Listed for completeness only.
```

### 6.5 Non-returning applications

```rule
RULE OS.Apply.NeverReturns
STATUS normative
CODE middle_end/flambda2/terms/apply_expr.mli#Result_continuation
CODE middle_end/flambda2/terms/apply_expr.mli#returns
---
e = Apply { continuation = Never_returns; … }
--------------------------------------------------
the callee is applied as in §6.1–6.3 but no Return boundary is installed:
K_body carries only k_exn ↦ Exn ⟨…⟩;  reaching the callee's k_ret is OS.Invalid
NOTES: `Result_continuation.Never_returns` marks an application known not to
return normally (`Apply_expr.returns = false`) — e.g. a tail call to a function
that always raises or loops, or the continuation of an over-application whose
first call never returns. The exn continuation is still installed (it may still
raise). If control nonetheless reaches the return continuation the program is
ill-formed (undefined behaviour).
```

## 7. Invalid

```rule
RULE OS.Invalid
STATUS normative
CODE middle_end/flambda2/terms/flambda.mli#expr_descr
CODE middle_end/flambda2/terms/flambda.mli#Invalid
---
e = Invalid { message }
--------------------------------------------------
⟨Invalid { message }, ρ, K, H, T, R⟩ has no transition (undefined behaviour)
NOTES: `Invalid` marks a program point that Simplify has proved unreachable
(type-incorrect code, an unreachable continuation body, an out-of-bounds access
to a known immutable array, a call whose arity or modes cannot match, a `Switch`
with no arms, …; see the `Invalid.t` reason enumeration in `flambda.mli`).
Operationally it is a stuck state: the semantics permits *any* behaviour if it is
ever reached, because a correct Simplify guarantees it never is. Soundness
([§13](13-soundness.md)) must show reachable configurations never step to `Invalid`.
```

## 8. Termination and observation

### 8.1 Initial configuration

```rule
RULE OS.Unit.Init
STATUS normative
CODE middle_end/flambda2/terms/flambda_unit.mli#create
---
u : Flambda_unit.t
body u = e₀,  return_continuation u = k_ret⁰,  exn_continuation u = k_exn⁰
toplevel_my_region u = x_reg⁰,  toplevel_my_ghost_region u = x_greg⁰,  module_symbol u = sym_mod
ι₀ fresh (the toplevel region)
ρ₀ = { x_reg⁰ ↦ region ι₀, x_greg⁰ ↦ region ι₀, predefined symbols ↦ their pointers }
H₀ = predefined symbols and external code ids ↦ their objects
K₀ = { k_ret⁰ ↦ Halt_return, k_exn⁰ ↦ Halt_exn }
--------------------------------------------------
initial(u) = ⟨e₀, ρ₀, K₀, H₀, [k_exn⁰], [ι₀]⟩
NOTES: The unit is entered with the toplevel region open, the toplevel exception
handler as the base trap frame, and the return/exn continuations bound to the two
halting entries below. Compilation of the module body eventually defines
`sym_mod` (via OS.Let.Static) to the module block and returns through `k_ret⁰`.
```

### 8.2 Final states and observations

```rule
RULE OS.Unit.Final
STATUS normative
CODE middle_end/flambda2/terms/flambda_unit.mli#return_continuation
CODE middle_end/flambda2/terms/flambda_unit.mli#module_symbol
---
Normal termination:  ⟨Apply_cont k_ret⁰ (v̄), ρ, K, H, T, R⟩ with K(k_ret⁰) = Halt_return
    ⟶ halt; the observable module value is H(sym_mod).
Uncaught exception:  ⟨Apply_cont k_exn⁰ (v_exn :: …), ρ, K, H, T, R⟩ with K(k_exn⁰) = Halt_exn
    ⟶ halt with the uncaught exception v_exn.
--------------------------------------------------
A run of u is one of: normal termination (observing H(sym_mod) and the trace of
C-call effects), termination by uncaught exception, divergence (an infinite ⟶
sequence), or undefined behaviour (reaching OS.Invalid or a stuck state).
NOTES: The *observable* behaviour of a program is (a) the sequence of external
effects performed by C calls (I/O and externally-visible mutation, §6.3), and
(b) whether and how the program terminates, including the final module block
value at `sym_mod`. Two terms are semantically equivalent when they induce the
same observations from every starting heap — this is the relation Simplify must
preserve ([§13](13-soundness.md)).
```

## 9. Worked example

A tiny `try … with` around a division, in lightly-sugared fexpr style. Source:

```ocaml
let r = try 10 / x with Division_by_zero -> -1 in r
```

After CPS conversion (return continuation `k`, exn continuation `k_exn⁰`,
schematically):

```
let_cont k_body () =
  let q = 10 / x in           (* checked division: raises Division_by_zero if x = 0 *)
  apply_cont k_join (q)  [Pop {exn_handler = h}]
and_cont h (exn) =            (* is_exn_handler *)
  apply_cont k_join (-1)
and_cont k_join (r) =
  apply_cont k (r)
in
apply_cont k_body ()  [Push {exn_handler = h}]
```

Take `x = 0`, initial `T = [k_exn⁰]`, so `K` binds `k_body`, `h`, `k_join`.

1. `apply_cont k_body ()  [Push {exn_handler = h}]`
   — OS.ApplyCont.TrapPush: push `h`, so `T = [h, k_exn⁰]`; enter `k_body`.
2. `let q = 10 / x in …` with `x = 0`
   — the checked-division primitive detects the zero divisor. In OCaml this
   raises; the frontend has lowered the raise to an `apply_cont` to the current
   handler, so the body continues as `apply_cont h (Division_by_zero) [Pop
   {exn_handler = h}]`.
3. `apply_cont h (Division_by_zero)  [Pop {exn_handler = h}]`
   — target equals the popped handler ⟹ OS.ApplyCont.Raise: pop `h` (now `T =
   [k_exn⁰]`) and enter `h`'s body with `exn = Division_by_zero`.
4. `apply_cont k_join (-1)`
   — OS.ApplyCont: enter `k_join` with `r = -1` (trap stack unchanged).
5. `apply_cont k (-1)`
   — control returns to the caller's continuation `k` with the value `-1`.

Had `x` been non-zero, step 2's division would have produced `q` normally and
the body would have reached `apply_cont k_join (q) [Pop {exn_handler = h}]`,
which is OS.ApplyCont.TrapPop (target `k_join ≠ h`): pop `h` and continue to
`k_join` with the quotient, never entering the handler. In both runs the trap
stack is balanced back to `[k_exn⁰]` before reaching `k_join`, illustrating the
invariant of §1.7.

## 10. Summary of rules

Structural: `OS.Simple.Eval`, `OS.Let.Simple`, `OS.Let.Prim.Pure`,
`OS.Let.Prim.Effect`, `OS.Let.SetOfClosures`, `OS.Let.Static`,
`OS.Let.RecInfo`.

Continuations: `OS.LetCont.NonRec`, `OS.LetCont.Rec`, `OS.ApplyCont`,
`OS.ApplyCont.Return`, `OS.ApplyCont.ExnBoundary`, `OS.ApplyCont.TrapPush`,
`OS.ApplyCont.TrapPop`, `OS.ApplyCont.Raise`.

Control: `OS.Switch`, `OS.Switch.Undef`, `OS.Apply.Direct`,
`OS.Apply.IndirectUnknownArity.Full`, `OS.Apply.IndirectUnknownArity.Partial`,
`OS.Apply.IndirectUnknownArity.Over`, `OS.Apply.IndirectKnownArity`,
`OS.Apply.CCall`, `OS.Apply.Method`, `OS.Apply.Effect`,
`OS.Apply.NeverReturns`, `OS.Invalid`.

Whole-unit: `OS.Unit.Init`, `OS.Unit.Final`.
