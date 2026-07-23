# Syntax: terms, binders, and Simples

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter gives the complete abstract syntax of the Flambda 2 term language —
the "raw Flambda" produced by CPS/closure conversion (see
[`01-overview.md`](01-overview.md)) and consumed and produced by Simplify. It
covers every expression form, the defining expressions (`named`), the binders,
`Simple`s and constants (including coercions), continuation handlers, functions,
sets of closures, code, trap actions, switches, and applications. It then
describes the binding structure and scoping (alpha-equivalence, second-class
continuations, name modes), states the *structural* well-formedness constraints
as `WF.Syntax.*` rule blocks, and maps the abstract grammar to the `fexpr`
concrete syntax with a worked example.

Kinding — which `Simple`s and expressions are kind-correct — is chapter
[`03-kinds.md`](03-kinds.md); this chapter states only constraints that are
structural (about the shape of terms), and marks the boundary where a constraint
is really a kinding fact owned by [§03](03-kinds.md).

The authoritative grammar and binding structure live in
`middle_end/flambda2/terms/flambda.mli`, whose module header records the design:
the language is in *double-barrelled* CPS (two continuations per function, one
for normal return and one for exceptions), continuations are *second class*, and,
despite being CPS, the language keeps a conventional `let` and is in A-normal
form (ANF).

## Metavariables

This chapter uses the metavariables fixed in the README
([Notation](README.md#metavariables)): `x` for variables, `sym` for symbols, `c`
for constants, `s` for `Simple`s, `k` for continuations, `f` for function slots,
`w` for value slots, `cid` for code IDs, `e` for expressions, `n` for `named`,
`p` for primitives, `κ` for kinds, `κ̂` for kinds-with-subkind, `co` for
coercions. In addition, this chapter locally uses: `P` for bound patterns, `H`
for continuation handlers, `bst` for `Bound_static.t`, `sc` for static consts,
`ri` for `rec_info` expressions, `am` for allocation modes, `i` for
target-machine integers (switch discriminants), `dbg` for `Debuginfo.t`.

Debug info (`dbg`), inlining/history metadata, `num_free_occurrences` hints and
similar auxiliary fields are carried throughout but are not semantically load
bearing; they are shown only where they matter.

## Expressions

```
e ::= Let (P = n) e                        -- Let: bind, then continue          [flambda.mli#expr_descr]
    | Let_cont lc                          -- Let_cont: define continuation(s)
    | Apply ap                             -- call a function / method / extern
    | Apply_cont ac                        -- call a continuation ("goto")
    | Switch sw                            -- conditional control flow
    | Invalid { message }                  -- unreachable / proved type-incorrect
```

Anchor: `middle_end/flambda2/terms/flambda.mli#expr_descr`. `expr` itself is an
abstract type (`flambda.mli#expr`) whose `descr` projects the `expr_descr` above.

The five non-`Invalid` forms partition into two roles. `Let` is the only *value*
binder in a term; its defining expression `n` is evaluated for its result (and
possible allocation/effect) but — crucially — **never affects control flow**
(the ANF invariant, WF.Syntax.Anf below). Control flow is expressed entirely by
`Apply` (calls, which pass control to a continuation on return), `Apply_cont`
(direct jumps to continuations, optionally manipulating the exception trap
stack), `Switch` (multi-way branch on a scrutinee), and `Invalid` (a dead end).

`Invalid` carries only a human-readable `message : string` in the term. It is
*created* from a richer reason type, `Flambda.Invalid.t`
(`flambda.mli#Invalid.t`), enumerating why the code is unreachable or otherwise
must be trapped: e.g. `Body_of_unreachable_continuation`,
`Zero_switch_arms`, `Application_never_returns`,
`Direct_application_parameter_kind_mismatch`, `Code_not_rebuilt`. Note (per the
code comment) that not all of these mean the code was genuinely unreachable —
`Defining_expr_of_let` can come from an out-of-bounds unsafe read of a known
immutable array.

### `Let`

A `Let` (`flambda.mli#let_expr`, module `Let_expr`) pairs a bound pattern `P`
(what is bound), a `named` `n` (the defining expression), and a body `e`. Its
constructor also records `free_names_of_body`, used to freshen bound variables.

### `Let_cont`

```
lc ::= Non_recursive { handler = H over k; num_free_occurrences; is_applied_with_traps; can_be_lifted }
     | Recursive rlch                                                            [flambda.mli#let_cont_expr]
```

Anchor: `middle_end/flambda2/terms/flambda.mli#let_cont_expr`.

- **Non-recursive** (`flambda.mli#non_recursive_let_cont_handler`): binds a
  single continuation `k` over the body, with handler `H`. The auxiliary fields
  are optimizer hints: `num_free_occurrences` (a `Num_occurrences.t
  Or_unknown.t`, always strictly greater than zero — WF.Syntax.NonRecOccursPositive),
  `is_applied_with_traps` (prevents inlining continuations applied with a trap
  action), and `can_be_lifted` (prevents some continuations being lifted during
  specialization).

- **Recursive** (`flambda.mli#recursive_let_cont_handlers`): binds a group of
  possibly mutually-recursive continuations, together with a shared list of
  *invariant parameters* (`Bound_parameters.t`), over both the body and all the
  handlers. Invariant parameters are parameters common to all continuations in
  the group whose values do not change around the recursion.

A continuation handler `H` (`flambda.mli#Continuation_handler`) is the
alpha-equivalence class of a parameter binding over a handler expression:

```
H ::= λ(params). handler          with flags is_exn_handler, is_cold           [flambda.mli#Continuation_handler]
```

`params : Bound_parameters.t`. The flags: `is_exn_handler` marks the
continuation as an exception handler (see WF.Syntax.ExnHandler* below), and
`is_cold` marks a handler expected to run rarely (affects layout, not
semantics).

### `Apply`

```
ap ::= { callee = s?;                     -- None for Effect, or Direct calls with erased callee  [apply_expr.mli#create]
         args = s̄;
         result_continuation = rc;
         exn_continuation = ec;
         args_arity : [`Complex];
         return_arity : [`Unarized];
         call_kind = ck;
         alloc_mode = am_app;
         inlined; inlining_state; probe; position; dbg; relative_history }

rc ::= Return k | Never_returns                                                 [apply_expr.mli#Result_continuation]
```

Anchor: `middle_end/flambda2/terms/apply_expr.mli#create`,
`apply_expr.mli#Result_continuation`. `Apply` calls a function, method, or
external, sending its result to the *result continuation* `rc` and any raised
exception to the *exception continuation* `ec`. `rc` is `Never_returns` for
calls that provably do not return (`apply_expr.mli#returns` is then `false`);
Simplify may turn such an `Apply` into `Invalid`
(`Application_never_returns`). The `callee` is optional: `callee = None` is
permitted iff the call kind is `Effect` (WF.Syntax.EffectCalleeNone) or
`Function { function_call = Direct _ }` (a direct call whose callee has been
erased — see `closure_conversion.ml`, classic mode with an unused
`my_closure`). `args_arity` is a
`` [`Complex] `` arity (before unarization); `return_arity` is `` [`Unarized] ``.
`alloc_mode` is an `Alloc_mode.For_applications.t` — `Heap`, or `Local {region;
ghost_region}` naming the region in which the result must be allocated.
`position` is `Normal` or `Nontail` (from `[@nontail]`).

The **call kind** (`call_kind.mli#t`) classifies the application:

```
ck ::= Function { function_call = fc }                                          [call_kind.mli#t]
     | Method { kind = mk; obj = s }
     | C_call { needs_caml_c_call; is_c_builtin; effects; coeffects }
     | Effect eff

fc ::= Direct cid                          -- code_id determines the callee     [call_kind.mli#Function_call]
     | Indirect_unknown_arity
     | Indirect_known_arity (Code_id.Set.t Or_unknown.t)

mk ::= Self | Public | Cached                                                   [call_kind.mli#Method_kind]

eff ::= Perform { eff }                                                         [call_kind.mli#Effect]
      | Reperform { eff; cont; last_fiber }
      | With_stack { valuec; exnc; effc; f; arg }
      | With_stack_preemptible { valuec; exnc; effc; handle_tick; f; arg }
      | Resume { cont; f; arg }
```

Anchors: `call_kind.mli#t`, `call_kind.mli#Function_call`,
`call_kind.mli#Method_kind`, `call_kind.mli#Effect`. For `Direct cid`, `cid`
uniquely determines the function symbol to call. For `Indirect_known_arity`, an
optional `Code_id.Set.t` narrows the possible callees. `C_call` records whether
the call needs `caml_c_call`, whether it is a C builtin, and its effects and
coeffects. The `Effect` operations carry their operands as `Simple`s *inside*
the call kind and correspondingly set `callee = None` with an empty argument
list, to avoid confusing these `Simple`s with ordinary call arguments
(WF.Syntax.EffectCalleeNone). (Per the scope ledger in 01, `Effect`
applications are documented here but given no operational rules; `Method`
applications get only a coarse dispatch rule, OS.Apply.Method in
[§04](04-opsem.md).)

### `Apply_cont`

```
ac ::= { continuation = k; args = s̄; trap_action = ta?; dbg }                   [apply_cont_expr.mli#create]

ta ::= Push { exn_handler = k }                                                 [trap_action.mli#t]
     | Pop  { exn_handler = k; raise_kind = rk? }

rk ::= Regular | Reraise | No_trace                                             [trap_action.mli#Raise_kind]
```

Anchors: `middle_end/flambda2/terms/apply_cont_expr.mli#create`,
`trap_action.mli#t`, `trap_action.mli#Raise_kind`. `Apply_cont` jumps to
continuation `k` with arguments `s̄`; in the zero-arity case it is just a "goto"
(`apply_cont_expr.mli#goto`). An optional **trap action** manipulates the
exception trap stack *before* the jump: `Push` installs `exn_handler` on the
trap stack; `Pop` removes it (with an optional raise kind). Raising an exception
is a `Pop` with a raise kind targeting the exception handler; hence
`apply_cont_expr.mli#is_raise`. Note (per `trap_action.mli#t`) that for `Pop`,
`exn_handler` need not equal the `Apply_cont`'s target continuation — e.g. when
returning a value out of the non-exceptional block of a `try...with`.

An **exception continuation** (`exn_continuation.mli#t`), used as the `ec` field
of an `Apply`, pairs an exception handler with extra arguments:

```
ec ::= { exn_handler = k; extra_args = (s, κ̂)̄ }                                 [exn_continuation.mli#create]
```

Anchor: `middle_end/flambda2/terms/exn_continuation.mli#create`. The handler `k`
receives the exception bucket plus the `extra_args` (each paired with its
kind-with-subkind `κ̂`); the continuation's arity
(`exn_continuation.mli#arity`) counts both the bucket and the extra args.

### `Switch`

```
sw ::= { scrutinee = s; arms = { i₁ ↦ ac₁, …, iₙ ↦ acₙ }; condition_dbg }        [switch_expr.mli#create]
```

Anchor: `middle_end/flambda2/terms/switch_expr.mli#create`. A `Switch` branches
on `scrutinee` (of kind `Naked_immediate` — WF.Syntax.SwitchScrutinee) by
matching it against the discriminants `iₖ` (target-machine integers,
`Target_ocaml_int.t`). Each arm's action is an `Apply_cont` (`acₖ`). There is
**no default case**: the arms map is understood to be total over the reachable
discriminants. `num_arms` counts arms, not distinct destinations
(`switch_expr.mli#num_arms`). A well-formed `Switch` node has at least two arms
(WF.Syntax.SwitchMinArms).

## `named` — defining expressions of `Let`

```
n ::= Simple s                             -- register-width value              [flambda.mli#named]
    | Prim (p, dbg)                         -- primitive operation
    | Set_of_closures (soc, am_alloc)       -- dynamically-allocated closures
    | Static_consts scg                     -- statically-allocated constants / code
    | Rec_info ri                           -- recursive-inlining state
```

Anchor: `middle_end/flambda2/terms/flambda.mli#named`. `Simple` lets a
`Simple.t` appear as a defining expression for convenience (it need not be
`Let`-bound). `Prim` is a primitive with debug info (primitives are chapters
05–06). `Set_of_closures` builds a *dynamically* allocated set of closures at
`am_alloc : Alloc_mode.For_allocations.t` (`Heap` or `Local {region}`).
`Static_consts` defines statically-allocated constants and/or code (below).
`Rec_info` defines a recursion-depth state, the RHS of a depth-variable binding.

### `rec_info`

```
ri ::= Const { depth = d; unrolling = u }                                       [rec_info_expr0.mli]
     | Var x                                -- variable of kind rec_info
     | Succ ri                              -- next depth
     | Unroll_to (int, ri)                  -- request unrolling to a depth

d ::= (int) | ∞                             -- int Or_infinity.t
u ::= Not_unrolling | Unrolling { remaining_depth } | Do_not_unroll
```

Anchor: `middle_end/flambda2/identifiers/rec_info_expr0.mli` (module type `S`),
re-exported at `middle_end/flambda2/term_basics/rec_info_expr.mli`. A `rec_info`
expression describes the state of recursive inlining at an occurrence:
`Const {depth; unrolling}` a literal state, `Var x` a depth variable (kind
`Flambda_kind.rec_info`), `Succ` the successor depth (inlining an occurrence at
depth `d` makes recursive references inside the body have depth `succ d`), and
`Unroll_to` a request to unroll to a given depth.

## Simples and constants

```
s ::= x | sym | c                          conceptually; concretely a Simple.t  [int_ids.mli#Simple]
```

A `Simple.t` (`int_ids.mli#Simple`, extended by
`term_basics/simple.mli`) is a register-width value: a name (variable `x` or
symbol `sym`) *optionally carrying a coercion*, or a constant `c`. Its
`pattern_match` exposes exactly two cases — `name` (with a `coercion:Coercion.t`)
and `const` — so a *variable or symbol* always has an attached coercion (`Id`
when absent) while a *constant* never does. We write the coercion explicitly,
following the README, as `s @ co` for a coerced name; `x` abbreviates `x @ Id`.

```
c ::= Naked_immediate i | Tagged_immediate i                                    [int_ids.mli#Const.Descr]
    | Naked_float f | Naked_float32 f
    | Naked_int8 n | Naked_int16 n | Naked_int32 n | Naked_int64 n
    | Naked_nativeint n
    | Naked_vec128 v | Naked_vec256 v | Naked_vec512 v
    | Null
    | Poison (κ, msg)
```

Anchor: `middle_end/flambda2/identifiers/int_ids.mli#Const.Descr` (the
metavariable `c` is `Reg_width_const.t`, which is `Int_ids.Const`, per
`identifiers/reg_width_const.mli`). A `Naked_immediate` is an untagged integer of
`n − 1` bits (machine width `n`); a `Tagged_immediate` is the tagged form.
`Null` is the null pointer; `Poison (κ, msg)` a deliberately-invalid value of
kind `κ`. A naked vector constant is a 128/256/512-bit pattern
(`Vector_types.VecN.Bit_pattern.t`, 2/4/8 int64 words, `word0` least
significant). Note an fexpr asymmetry: the printer emits naked vector constants
(as `vec128[w0:w1]` etc.) and `Fexpr.const` can represent them, but the parser
has no production for them in `simple`/term position — the `vecN[…]` literals
are only parseable inside `Immutable_vecN_array` static data
(`parser/flambda_parser.mly`).

### Coercions

```
co ::= Id                                                                       [coercion0.mli#S]
     | Change_depth { from = ri; to_ = ri }
```

Anchor: `middle_end/flambda2/identifiers/coercion0.mli#S` (re-exported at
`term_basics/coercion.mli`). A coercion is a compile-time function on `Simple`s
that adds type-level information without changing the run-time value, and must
have an inverse. Currently the only non-identity coercion is `Change_depth`,
which adjusts the recursion depth attached to a closure. Because coercions have
inverses and do not change run-time values, a coerced name is treated as an
*alias* of the original name.

## Sets of closures, functions, code

### Sets of closures

```
soc ::= { function_decls = fdecls; value_slots = { w₁ ↦ s₁, … } }               [set_of_closures.mli#create]
```

Anchor: `middle_end/flambda2/terms/set_of_closures.mli#create`. A set of
closures pairs *function declarations* (the code of the functions, indexed by
function slot) with *value slots* (the captured environment: a map from value
slot `w` to the `Simple` `s` stored there). A set with no value slots is *closed*
(`set_of_closures.mli#is_closed`).

```
fdecls ::= { f₁ ↦ cidfd₁, … }              -- Function_slot.Lmap (ordered)       [function_declarations.mli#create]

cidfd ::= Deleted { function_slot_size; dbg }                                    [function_declarations.mli#code_id_in_function_declaration]
        | Code_id { code_id = cid; only_full_applications }
```

Anchors: `middle_end/flambda2/terms/function_declarations.mli#create`,
`function_declarations.mli#code_id_in_function_declaration`. Each function slot
`f` maps either to a `Code_id` (naming the code, with a flag for
full-applications-only) or to `Deleted` (the code has been removed but the slot's
size is retained). The map is an `Lmap`, so declaration order is preserved.

### Code

A `Code` static const wraps a `Code0.t`
(`middle_end/flambda2/terms/code0.mli`), which bundles two things: the
*code metadata* (`code0.mli#code_metadata`) and the *params-and-body*
abstraction (`code0.mli#params_and_body`).

```
fpb ::= λ(return_continuation = k_ret,     -- normal-return continuation        [flambda.mli#Function_params_and_body.create]
          exn_continuation = k_exn,        -- exception continuation
          params,                          -- Bound_parameters.t
          my_closure = x_clo,
          my_alloc_mode = am_app,          -- binds my_region/my_ghost_region when Local
          my_depth = x_depth).
        body
```

Anchor:
`middle_end/flambda2/terms/flambda.mli#Function_params_and_body.create`. The
function-params-and-body abstraction binds, over the `body`: the two
continuations `k_ret` (where to jump with the result — if it takes several
arguments the backend returns multiple values) and `k_exn` (where to jump on a
raised exception); the ordinary `params`; `my_closure` (the variable through
which the body reaches its own captured value slots and sibling function slots,
via `Project_value_slot` / `Project_function_slot`); `my_depth` (the recursion
depth variable); and `my_alloc_mode : Alloc_mode.For_applications.t`. When the
alloc mode is `Local {region; ghost_region}`, those two variables — the
function's region and *ghost* region, corresponding to the concrete syntax's
`my_region` / `my_ghost_region` — are the ones bound here; when it is `Heap`, no
region variable is bound (`bound_identifiers/alloc_mode.mli#For_applications`).

The **code metadata** (`code_metadata.mli`) records, per code ID, the
information Simplify and the backend need about a function without looking at its
body. The semantically relevant fields (skimming
`code_metadata.mli#Code_metadata_accessors_result_type`):

- `params_arity : [`Complex]` and `result_arity : [`Unarized]` — the parameter
  and result arities;
- `param_modes : Alloc_mode.For_types.t list` and
  `first_complex_local_param : int` — locality of parameters (the latter locates
  the first local parameter, for partial-application alloc modes);
- `result_mode : Lambda.locality_mode` and `result_types` — locality and known
  types of results;
- `recursive : Recursive.t`, `stub : bool`, `is_tupled : bool`,
  `is_my_closure_used : bool`, `newer_version_of : Code_id.t option`;
- inlining-relevant metadata: `inline`, `inlining_decision`, `cost_metrics`,
  `inlining_arguments`, `is_a_functor`, `is_opaque`, `cold`, `loopify`.

The remaining fields (`zero_alloc_attribute`, `poll_attribute`,
`regalloc_attribute`, histories, `dbg`, `function_slot_size`) are backend/attribute
metadata not load-bearing for the semantics.

### Static constants

```
scg ::= [scoc₁, …, scocₘ]                                                       [flambda.mli#Static_const_group]

scoc ::= Code (Code0.t)                                                         [flambda.mli#static_const_or_code]
       | Deleted_code
       | Static_const sc
```

Anchors: `middle_end/flambda2/terms/flambda.mli#Static_const_group`,
`flambda.mli#static_const_or_code`. A static-const group is an ordered list of
entries, each either a piece of `Code`, `Deleted_code`, or a `Static_const`.

```
sc ::= Set_of_closures soc                                                      [static_const.mli#t]
     | Block (tag, mut, shape, [s.dbg, …])
     | Boxed_float32 ov | Boxed_float ov | Boxed_int32 ov | Boxed_int64 ov
     | Boxed_nativeint ov | Boxed_vec128 ov | Boxed_vec256 ov | Boxed_vec512 ov
     | Immutable_float_block [ov, …] | Immutable_float_array [ov, …]
     | Immutable_float32_array [ov, …]
     | Immutable_int_array [ov, …] | Immutable_int8_array [ov, …]
     | Immutable_int16_array [ov, …] | Immutable_int32_array [ov, …]
     | Immutable_int64_array [ov, …] | Immutable_nativeint_array [ov, …]
     | Immutable_vec128_array [ov, …] | Immutable_vec256_array [ov, …]
     | Immutable_vec512_array [ov, …]
     | Immutable_value_array [s.dbg, …]
     | Empty_array eak
     | Mutable_string { initial_value } | Immutable_string str

ov ::= (literal) | Var x                    -- Or_variable.t
```

Anchor: `middle_end/flambda2/terms/static_const.mli#t`. A `Static_const` is the
static structure of a symbol, possibly with holes (`Or_variable.Var x`) to be
filled at run time. `Block` carries a scannable tag, mutability, a scannable
block shape, and a list of fields (each a `Simple` with debug info). The boxed
scalars and immutable arrays hold either literals or variables. The
`Immutable_*_array` constructors always have at least one field
(WF.Syntax.ImmutableArrayNonEmpty); the empty case is `Empty_array eak`, which
records the array's element kind (empty arrays of unboxed numbers have a
different representation, affecting length computation).

## Binders

The three binder positions are the `Let` pattern, continuation-handler
parameters, and the function-params-and-body abstraction. This section covers
the first two; the third is `fpb` above.

### Bound patterns

```
P ::= Singleton bv                          -- one variable (not sets of closures) [bound_pattern.mli#t]
    | Set_of_closures [bv₁, …]              -- variables for closures in a set
    | Static bst                            -- symbols and code IDs

bv ::= { var = x; name_mode = nm; … }                                           [bound_var.ml]
```

Anchors: `middle_end/flambda2/bound_identifiers/bound_pattern.mli#t`,
`bound_identifiers/bound_var.ml`. A `Singleton` binds one variable and is *not*
used for sets of closures (WF.Syntax.SingletonNotSetOfClosures); `Set_of_closures`
binds one variable per closure in a dynamically-allocated set; `Static` binds
symbols and code IDs. A `Bound_var.t` is a variable together with its name mode
(and a debug UID). If a `Let` binds more than one name (the `Set_of_closures`
case), all bound names have the same kind (WF.Syntax.LetKindUniform).

```
bst ::= [bsp₁, …]                                                               [bound_static.mli]

bsp ::= Code cid                                                                [bound_static.mli#Pattern]
      | Set_of_closures { f₁ ↦ sym₁, … }    -- Function_slot.Lmap
      | Block_like sym
```

Anchor: `middle_end/flambda2/bound_identifiers/bound_static.mli#Pattern`. A
`Bound_static.t` is a list of patterns each binding a code ID (`Code`), a set of
closure symbols indexed by function slot (`Set_of_closures`), or a single
block-like symbol (`Block_like`). Any recursive cycle among the names bound by a
`Bound_static.t` must pass through at least one code ID
(WF.Syntax.StaticRecThroughCode).

### Bound parameters

A `Bound_parameters.t` (`bound_parameters.mli`) is a list of `Bound_parameter.t`,
each a variable paired with its kind-with-subkind `κ̂`
(`bound_parameter.mli#create`). Parameters within one binding must have distinct
variables (`bound_parameters.mli#check_no_duplicates`).

## The compilation unit

```
unit ::= { return_continuation = k;                                             [flambda_unit.mli#create]
           exn_continuation = k;
           toplevel_my_region = x;
           toplevel_my_ghost_region = x;
           module_symbol = sym;
           body = e;
           used_value_slots }
```

Anchor: `middle_end/flambda2/terms/flambda_unit.mli#create`. A `Flambda_unit.t`
is one compilation unit: a body `e`, the toplevel return and exception
continuations, the toplevel region and ghost region variables (in scope over the
whole body), the module symbol defined by the unit, and the set of value slots
used across the unit.

## Binding structure and scoping

Flambda 2 terms are represented up to **alpha-equivalence** of bound *variables*
and *continuations* (`flambda.mli` header; `Expr.t` is "the type of equivalence
classes of expressions up to alpha-renaming of bound `Variable`s and
`Continuation`s"). The machinery is nominal name-abstraction: each binder above
is an abstraction that can be `pattern_match`-ed to expose fresh bound names, and
`pattern_match_pair` opens two abstractions with a *common* set of bound names —
this is how the optimizer compares or combines two terms.

**Variables vs symbols and code IDs.** These scope differently
(`flambda.mli#Let_expr`): variables have ordinary *syntactic* scope (a
`Let`-bound or parameter variable is in scope exactly in the body/handler it is
bound over), whereas symbols and code IDs are treated as in scope in *all* parts
of the term dominated by their binding — dominator scope, not syntactic scope.
Correspondingly, `Static` bindings are **not** treated up to alpha-equivalence
(only variables and continuations are): symbols and code IDs are global-ish
names, not freshenable bound names.

**Continuations are second class** (`flambda.mli#Let_cont_expr`): they may be
*defined* (by `Let_cont`) and *called* (by `Apply_cont`, or as the
result/exception continuation of `Apply`), but they cannot be captured in
variables, stored, or returned — they do not escape, and they do not capture
variables (a continuation closes over nothing). This is what makes the
double-barrelled CPS tractable: control flow is a static graph of named
continuations.

**Name modes** (`nominal/name_mode.ml`) classify occurrences into a
three-element semilattice:

```
nm ::= Normal | Phantom | In_types                                              [name_mode.ml#t]
```

with `Normal` above `Phantom` above `In_types`. `Normal` is an ordinary runtime
occurrence; `Phantom` is present only for debugging (no runtime effect);
`In_types` occurs only inside Flambda types. Only `Normal` and `Phantom` may
occur in terms (`name_mode.ml#can_be_in_terms`), so a `Bound_var.t` in a term has
mode `Normal` or `Phantom`, never `In_types` (WF.Syntax.NameModeInTerms) — though
`In_types` does appear on bound vars used in typing contexts (e.g. function
return types), which are not terms.

## Structural well-formedness

The rules below are the *structural* invariants of well-formed terms — shape
constraints, not kinding ([§03](03-kinds.md)). They use the `WF.Syntax.*` namespace. Most
are enforced by construction (the smart constructors, or the shape of the
types); a `normative` status means the code must maintain the invariant, a
`conjectured` status means it is believed but not verified against every
construction site.

```rule
RULE WF.Syntax.Anf
CLAIM normative
CODE middle_end/flambda2/terms/flambda.mli#expr_descr
---
n is the defining expression of a Let binding
--------------------------------------------------
evaluating n has no effect on control flow (control flow is expressed only by
Apply, Apply_cont, Switch and Invalid)
NOTES: The ANF invariant, stated in the [Let] documentation of [expr_descr]:
"The defining expression ... never has any effect on control flow." A [named]
may allocate or have side effects, but does not branch, jump, or return.
```

```rule
RULE WF.Syntax.LetKindUniform
CLAIM normative
CODE middle_end/flambda2/bound_identifiers/bound_pattern.mli#t
---
Let (P = n) e   binds names x₁ … xₙ   (n > 1 only when P = Set_of_closures …)
--------------------------------------------------
x₁ … xₙ all have the same kind
NOTES: Stated in [Bound_pattern] header: "If a [Let]-expression binds more than
one name, all of those names have the same kind." The multi-name case is exactly
the [Set_of_closures] pattern.
```

```rule
RULE WF.Syntax.SingletonNotSetOfClosures
CLAIM normative
CODE middle_end/flambda2/bound_identifiers/bound_pattern.mli#t
---
P = Singleton bv
--------------------------------------------------
n (the defining expression) is not a Set_of_closures
NOTES: Per the [Singleton] doc: "This case is not used for sets of closures."
Dynamically-allocated sets of closures use the [Set_of_closures] pattern.
```

```rule
RULE WF.Syntax.SwitchScrutinee
CLAIM normative
CODE middle_end/flambda2/terms/switch_expr.mli#t
CODE middle_end/flambda2/terms/switch_expr.mli#create
---
Switch { scrutinee = s; arms; … }
--------------------------------------------------
s has kind Naked_immediate ; each discriminant iₖ is a Target_ocaml_int ;
there is no default arm (arms is total over reachable discriminants)
NOTES: The scrutinee-kind part is really a kinding fact; [§03](03-kinds.md) (WF.*) owns
kinding. Stated structurally here because it is intrinsic to the [Switch] form
(module header: "Scrutinees of [Switch]es are of kind [Naked_immediate]. There
are no default cases."). This rule is the *structural* form; the *kinding
judgment* counterpart is WF.Switch.Scrutinee in [§03](03-kinds.md).
```

```rule
RULE WF.Syntax.SwitchMinArms
CLAIM normative
CODE middle_end/flambda2/terms/switch_expr.mli#t
CODE middle_end/flambda2/from_lambda/closure_conversion.ml#close_switch
CODE middle_end/flambda2/simplify/expr_builder.ml#create_switch
CODE middle_end/flambda2/terms/flambda.mli#Invalid.t
VERIFIED 14-validation/gadt_simplified_switch.md @ 1c1940b7ea
CAVEAT disclosure: not enforced by Switch_expr.create; the fexpr parser can build a <2-arm Switch from hand-written input — invariant maintained by construction on producing paths, not by the constructor.
---
Switch sw appears in a well-formed term
--------------------------------------------------
sw has at least two arms
NOTES: The module header of switch_expr.mli states the invariant: "Switches
always have at least two cases." Verified on both term-producing paths: [close_switch]
routes a zero-arm switch to Invalid (Zero_switch_arms) and a single-arm switch to a
direct Apply_cont, only calling [Switch.create] for ≥ 2 arms; [Expr_builder.create_switch]
likewise emits Invalid for < 1 arm and Apply_cont for a singleton (or all-arms-equal)
switch. [Switch_expr.create] does not itself enforce this, and the fexpr parser can
build a violating node from hand-written test input, so the invariant is maintained by
construction rather than by the constructor. See also the weaker kinding-side fact
WF.Switch.NonEmpty in [§03](03-kinds.md), which records only that a *reachable* switch
has at least one arm.
```

```rule
RULE WF.Syntax.ExnHandlerNonRecursive
CLAIM normative
CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
CAVEAT disclosure: holds only by Flambda_to_cmm — transient mutual recursion through stubs is permitted earlier, relying on Simplify to inline it out before to_cmm.
---
continuation k is an exception handler (is_exn_handler = true)
--------------------------------------------------
by the time the term reaches Flambda_to_cmm, k is bound by a Non_recursive
Let_cont
NOTES: Per [Continuation_handler]: "Continuations used as exception handlers are
always [Non_recursive]." Mutual recursion through stubs is permissible
transiently, so long as Simplify inlines it out before to_cmm ([Let_cont_expr]
doc).
```

```rule
RULE WF.Syntax.ExnHandlerFirstParamBucket
CLAIM normative
CODE middle_end/flambda2/terms/exn_continuation.mli#arity
CODE middle_end/flambda2/terms/flambda.mli#Continuation_handler
CAVEAT disclosure: no compiler check enforces the parameter/extra_args alignment against Exn_continuation.t — the invariant is maintained by construction at producing sites only.
---
k is an exception handler with parameters p₁ … pₘ
--------------------------------------------------
p₁ is the exception bucket ; p₂ … pₘ correspond to the extra_args of the
Exn_continuation.t targeting k
NOTES: [Continuation_handler] notes exception handlers "may have more than one
parameter (see [Exn_continuation])"; the [Exn_continuation.arity] counts the
bucket plus [extra_args]. Conjectured: the exact parameter/extra-arg alignment is
not checked here.
```

```rule
RULE WF.Syntax.EffectCalleeNone
CLAIM normative
CODE middle_end/flambda2/terms/call_kind.mli#Effect
CODE middle_end/flambda2/terms/apply_expr.mli#create
---
Apply ap   with   call_kind ap = Effect eff
--------------------------------------------------
callee ap = None   and   args ap = []
NOTES: Per [Call_kind.Effect]: "The corresponding [Apply_expr] will have the
callee set to [None] and an empty argument list ... to ensure there is no
confusion between the different [Simple]s" (which are carried inside [eff]).
```

```rule
RULE WF.Syntax.ContSecondClass
CLAIM normative
CODE middle_end/flambda2/terms/flambda.mli#Let_cont_expr
---
k is a continuation
--------------------------------------------------
k occurs only as the target of an Apply_cont, or as the result/exception
continuation of an Apply, or as a bound continuation of a Let_cont; k is never
stored in a variable, symbol, or block, and captures no variables
NOTES: "Continuations are second-class. Continuations do not capture variables."
([Let_cont_expr] header.)
```

```rule
RULE WF.Syntax.NonRecOccursPositive
CLAIM normative
CODE middle_end/flambda2/terms/flambda.mli#let_cont_expr
CODE middle_end/flambda2/terms/flambda.ml#Let_cont_expr.create_non_recursive0
CAVEAT disclosure: not enforced by construction — create_non_recursive0 passes a Known 0 count straight to create0, which builds the Let_cont unconditionally; invariant maintained by callers and Simplify dead-continuation removal.
---
Let_cont (Non_recursive { num_free_occurrences = Known m; … })
--------------------------------------------------
m > 0
NOTES: Per [let_cont_expr]: [num_free_occurrences] "will always be strictly
greater than zero." [Or_unknown] also permits [Unknown]. Conjectured because the
low-level constructor does not enforce it: [create_non_recursive0] computes the
count via [Name_occurrences.count_continuation] and passes it straight to [create0],
which builds the [Let_cont] unconditionally — it neither drops the body-only case nor
rejects a [Known 0] count. (The [create_non_recursive] doc comment promises the
body-only case returns just the body, but that dropping is not done here; it relies
on callers, and on Simplify removing dead continuations.) So the invariant is
documented but maintained by callers, not by construction.
```

```rule
RULE WF.Syntax.StaticRecThroughCode
CLAIM normative
CODE middle_end/flambda2/bound_identifiers/bound_static.mli#create
---
bst binds names N ; there is a recursive cycle among N
--------------------------------------------------
the cycle passes through at least one Code code ID
NOTES: Per [Bound_static.create]: "All recursive cycles between the names bound
by the provided pattern(s) must go through at least one code ID." (E.g. a block
that points to itself is forbidden.)
```

```rule
RULE WF.Syntax.ImmutableArrayNonEmpty
CLAIM normative
CODE middle_end/flambda2/terms/static_const.mli#t
---
sc = Immutable_*_array [fields]
--------------------------------------------------
fields is non-empty
NOTES: Per [static_const.mli]: "[Immutable_*_array] constructors always have at
least one field. For empty arrays, [Empty_array] must be used." The smart
constructors ([immutable_*_array]) route empty lists to [Empty_array].
```

```rule
RULE WF.Syntax.NameModeInTerms
CLAIM normative
CODE middle_end/flambda2/nominal/name_mode.ml#can_be_in_terms
---
bv is a Bound_var.t occurring in a term
--------------------------------------------------
name_mode bv ∈ { Normal, Phantom }
NOTES: [name_mode.ml#can_be_in_terms] is true only for Normal and Phantom.
[In_types] bound vars appear in typing contexts (e.g. function return types),
which are not terms.
```

## Concrete syntax (`fexpr`)

The parser/printer AST `Fexpr.t`
(`middle_end/flambda2/parser/fexpr.ml`) is a *concrete*, human-writable surface
syntax for Flambda 2 terms, used by the `.fl` test programs and the `fexprc`
tool. It is deliberately **lossy**: it omits most metadata (debug info, inlining
history, cost metrics, `num_free_occurrences` and similar), it lets kinds be
omitted (`kinded_parameter.kind : … option`) and defaults them, and it uses
string identifiers rather than the nominal `Variable`/`Continuation` machinery,
so round-tripping through it is not identity.

Correspondence (abstract → concrete), with the concrete constructor from
`fexpr.ml`:

| Abstract | Concrete (`fexpr.ml`) | Notes |
|---|---|---|
| `Let (P = n) e` | `Let { bindings; value_slots; body }` (`let … in …`) | one `let` can carry several `bindings` and shared `value_slots` |
| `Let_cont` (non-rec) | `Let_cont { recursive = Nonrecursive; bindings; body }` (`… where k (…) = …`) | |
| `Let_cont` (rec) | `Let_cont { recursive = Recursive params; … }` | invariant params on the group |
| `Static_consts` / `Bound_static` | `Let_symbol { bindings; body }` (`let … in`) | `Data`, `Code`, `Deleted_code`, `Closure`, `Set_of_closures` |
| `Apply ap` | `Apply { func; continuation; exn_continuation; args; call_kind; … }` (`apply f (…) -> k * e`) | `func = None` printed without a callee |
| `Apply_cont ac` | `Apply_cont { cont; trap_action; args }` (`cont k (…)`) | |
| `Switch sw` | `Switch { scrutinee; cases }` (`switch s | i -> k`) | arms are `apply_or_inlined_cont` |
| `Invalid { message }` | `Invalid { message }` | |
| `named` `Simple`/`Prim`/`Set_of_closures`/`Rec_info` | `Simple`/`Prim`/`Closure`/`Rec_info` | `Prim`s are written `%name` |
| `Function_params_and_body` | `params_and_body { params; closure_var; region_var; ghost_region_var; depth_var; ret_cont; exn_cont; body }` | region vars always named in concrete syntax |
| coercion `s @ co` | `Coerce (simple, coercion)` | |

Two mismatches worth flagging. First, `params_and_body` in `fexpr` always names a
`region_var` and `ghost_region_var`, whereas in the abstract syntax those
variables are bound only when `my_alloc_mode` is `Local` (see `fpb` above).
Second, the special toplevel continuations are written `done` (normal return)
and `error` (exceptions) (`fexpr.ml#special_continuation`), and toplevel symbols
are written with a leading `$`.

### Worked example

From `middle_end/flambda2/tests/new_syntax.fl`:

```
let code f (x) myclosure -> k1 * e : val = cont k1 (x)
and $F = closure f
in
let $camlNew_syntax = Block 0 ($F) in
cont done ($camlNew_syntax)
```

Reading it against the abstract syntax:

- The outer `let … and … in` is a `Let_symbol` binding a `Static_consts` group
  (a `Bound_static` binding). Its first entry, `code f …`, is a `Code`
  static-const-or-code: a `Function_params_and_body` for `f` whose single
  parameter is `x`, whose closure variable is `myclosure`, whose return
  continuation is `k1` and exception continuation is `e`, and whose body is the
  `Apply_cont` `cont k1 (x)` — i.e. `f` immediately returns its argument. The
  `: val` annotates the result arity.
- `$F = closure f` is a `static_closure_binding`: a `Block_like`/closure symbol
  `$F` bound to a closure over code `f`.
- The inner `let $camlNew_syntax = Block 0 ($F) in` is a second `Let_symbol`
  binding a `Block`-shaped `Static_const` with tag `0` and one field, the symbol
  `$F`.
- The final `cont done ($camlNew_syntax)` is an `Apply_cont` to the toplevel
  return continuation `done`, returning the module block — the whole thing is the
  unit's `body`, terminating by returning `$camlNew_syntax` as the module value.

A slightly richer control-flow example, from
`middle_end/flambda2/tests/new_syntax_3.fl` (abbreviated), shows a `switch` with
two arms feeding continuations defined by `where`, `apply` with an explicit
result and exception continuation (`-> k3 * e`), and `%project_value_slot` /
`%project_function_slot` primitives reaching through `clo` (the `my_closure`
variable) into a set of closures:

```
let code rec even (i) clo -> k * e =
  let i_u = %untag_imm i in
  switch i_u
    | 0 -> k1
    | 1 -> k2
  where k1 = cont k (0)
  where k2 = (
    let f = %project_value_slot even.f clo in
    apply f (i) -> k3 * e
    where k3 (x) = ( … ))
```

Here the `Switch` scrutinee `i_u` is the untagged (`Naked_immediate`) form of the
tagged parameter `i`, satisfying WF.Syntax.SwitchScrutinee; the two arms
`0 -> k1` and `1 -> k2` satisfy WF.Syntax.SwitchMinArms; and each arm's action is
an `Apply_cont` (`cont k …`) per the `Switch` grammar.
