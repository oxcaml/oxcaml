# Vox erasure

We add an erasure mode axis and a construct that produces values on it.

    erased_ e            -- e is deleted from compilation
    @ erased             -- the value has no runtime representation
    { x : t @@ erased }  -- an erased record field

An erased value does not exist at run time. An erased function parameter is not
passed. An erased record field occupies no slot.

## Naming

`erased`, not `ghost`. The compiler already uses `ghost` for a different kind of
absence: `loc_ghost` (75 uses), `ghost_loc` (12) and `Location.ghost` (10)
across 32 files in `typing`, `parsing` and `lambda`, and `loc_ghost` is part of
the public parsetree surface. `erased` has no incumbent, only comments and one
local `is_erased` in `typeclass.ml`.

The stronger reason is that the mode guarantees a mechanism, not an intent.
What it enforces is the absence of a runtime representation. "Ghost" names a use
(specification-only), which is what logicality is for, and conflating the two
words invites conflating the two axes. Say in user-facing documentation that
this is what Why3 and Dafny call ghost code, so the term stays findable.

## Lattice

| axis | min | max | legacy | fragment |
|---|---|---|---|---|
| Erasure | `Retained` | `Erased` | `Retained` | comonadic |

`Retained <= Erased`. A real value may be used where an erased one is expected,
since the context is going to discard it. The reverse cannot hold.

Comonadic on the substantive test: a value's mode is bounded above by the meet
of what its uses demand, so using a value anywhere retained forces it retained.
That is what gives the discipline below. Capture behaves unusually, which is
covered under Closures.

## The rule

### Ambient erasure

Two things have to be kept apart, and conflating them is how the design goes
wrong.

*What may be used* at a position is governed by the expected mode, through
ordinary submoding. A value may be used only where the expected erasure is at
least its own.

*What gets deleted* is governed by the expression's own erasure. `erased_ e`
sets the expression to Erased, so it is deleted. An ordinary expression is
Retained and is evaluated, whatever position it sits in.

Deletion therefore never happens implicitly. It follows the source, not the
context.

An expression is checked at ambient Erased in two places: the body of
`erased_ e`, and the body of a closure that is itself erased. Everything else is
retained.

Because `Retained <= Erased`, real values are usable inside erased contexts,
which is what specifications need. Erased values fail everywhere else. Most of
the behaviour we want follows without further rules:

- `x + y` in retained code requires both retained
- `if x then _ else _` requires a retained condition, since branching reads a
  runtime value
- matching on an erased scrutinee fails in retained code and is allowed inside
  `erased_`
- `erased_ e` checks `e` at ambient Erased, so both retained and erased values
  are usable there

### Application

An erased function cannot be called, because there is no closure to jump to.
`f x` with `f @ erased` must fail in retained code, and `erased_ (f x)` must
succeed.

That is the ambient rule again, with the function position counted as a use of
`f`. It does not fall out on its own. OxCaml gives the function position of an
application its own `funct_mode` rather than the application's expected mode,
which is right for locality, since calling a local closure is fine, and wrong
for erasure. So one explicit constraint has to be added: the erasure component
of `funct_mode` is the ambient erasure.

Keep this separate from an erased *parameter*. `f : int @ erased -> int` and
`f @ erased : int -> int` are independent and both have to work.

### Arguments are not erased silently

An `@ erased` parameter accepts both kinds of argument, and they behave
differently on purpose:

    f (expensive ())            -- evaluated for effect, then dropped at the
                                -- boundary. Retained <= Erased, so this is
                                -- ordinary submoding.
    f (erased_ (expensive ()))  -- never evaluated. The user asked for it.
    let x = erased_ e in f x    -- x is already erased, nothing to write

The argument position does not create an erased context. If it did, an ordinary
call would silently drop its argument's effects with nothing in the source to
show for it. Deleting an evaluation is something the programmer writes, not
something a callee's signature does to them.

The cost is one evaluation the callee cannot observe. Once `e @ total` is
required the two forms coincide, since a total expression's evaluation is
unobservable, and at that point the non-evaluating form is simply faster.

### Erasure propagates outward, modalities insulate

Erasure flows through structural types the way other modes do. A tuple with an
erased component is an erased tuple, and a list with an erased element is an
erased list. There is no ambiguity about which slot is missing, because the
whole value is gone.

To hold an erased value inside a retained structure, use a declaration site that
carries a modality. Record fields have `ld_modalities` and constructor
arguments have `ca_modalities`; tuples have none, since `Ttuple` is
`(string option * type_expr) list`. So the idiom for a partially erased
structure is a record, and a single-field `@@ erased` record is the wrapper for
smuggling one erased value through a retained one. This is the same job `@@
global` does for a global field in a local record.

### Closures: capture

Capture propagates nothing. A retained closure may capture erased values.

    let x = erased_ e in
    fun y -> f x y

`f` takes its first argument `@ erased`, so nothing is passed, so the closure
needs no slot for `x`. The closure is an ordinary runnable closure. Confirmed
against the existing void machinery, which is the same situation:

    let mk (x : t) (n : int) = fun y -> f x (y + n)
    -> camlC__mk_1_4_code (n/311: int) : val (alloc 3319 L:"camlC__fn..._code" ...)

So the usual comonadic join is wrong here. Under it a closure's mode is the join
of its captures, and capturing an `Erased` value would force the closure to
`Erased`.

Two framings give the behaviour we want, and they need deciding between:

- A modality. The closure environment is a record and an erased capture is a
  field carrying `@@ erased`. With a constant modality,
  `capture_mode <= modality(closure_mode)` holds unconditionally, so the capture
  constrains nothing. This is how `@@ global` puts a global field in a local
  record.
- A carve-out. Erasure is excluded from the closure-lock join.

Prefer the modality if the machinery supports it, since it reuses a mechanism
rather than making an exception to one. Check first whether the closure lock can
carry a per-capture modality or whether it applies uniformly across captures. If
it is uniform, the carve-out is forced.

### Closures: the body

A closure's body is checked at the closure's own erasure.

    erased_ (fun y -> g y)      -- g @ erased is fine here

The lambda is erased, so it is deleted whole, so its body is an erased context.
Without this rule the body would be checked at Retained and that example would
fail, which would make `erased_` useless over anything containing a lambda. A
retained closure's body is a retained context, so it can touch erased values
only in erased sub-positions.

This is a different rule from capture. Capture is about the environment, the
body rule is about context, and the body rule is closer to a region than a lock
because it changes an ambient rather than adjusting individual lookups.

One consequence falls out rather than needing its own rule: an erased closure's
body is an erased context, so an erased closure can capture anything.

### Erasure is fixed top-down

The body rule means a closure's erasure has to be known before its body is
checked. It cannot be an inference variable that zaps later.

That is satisfiable, because the expected mode supplies it: `erased_ (fun ...)`
gives Erased, ordinary code gives Retained. It has to be taken from the expected
mode rather than inferred, and the implementation should enforce that.

The same constraint arrives independently from the ABI, since a function's
calling convention depends on which parameters are erased. Both say the same
thing. The erasure of a function is determined by its type and its position, and
never inferred from its body.

### Erasure is invariant in argument position

Modes on arrows are treated three different ways, and only one of them is a
problem. Measured on the baseline compiler with locality:

| path | argument | return |
|---|---|---|
| `unify`, ordinary passing | equated | equated |
| subsumption, module sealing | contravariant | covariant |
| `(e :> t)` coercion | contravariant | covariant |

`unify` calls `unify_alloc_mode_for Unify` on both the argument and the return
mode (`ctype.ml:5136`), which submodes in both directions and so equates them.
Sealing and coercion genuinely relate them: a local-returning implementation is
rejected against a global-returning signature and accepted the other way, and an
implementation needing a global argument is rejected against a signature
promising only a local one.

For erasure, the return direction is already safe, because the lattice matches
the ABI:

- implementation `Retained`, signature `Erased`: allowed. The callee writes a
  result the caller ignores. Harmless.
- implementation `Erased`, signature `Retained`: rejected by the order. The
  caller would read a result nobody produced.

Argument position has exactly one unsafe direction, and contravariance permits
it:

- signature `Erased`, implementation `Retained`: rejected by the order. Correct,
  since the callee wants a value nobody passes.
- signature `Retained`, implementation `Erased`: allowed, and it is an ABI
  mismatch. The caller passes an argument the callee has no parameter for.

So the rule is narrow. Erasure is invariant in argument position under
subsumption and coercion, and keeps the ordinary covariant rule in return
position. Nothing else needs changing, because unification already equates.

The reason locality is safe in the same direction is that both modes have
identical representation, so the coercion is a no-op. Erasure changes the
parameter list.

## How it compiles

### The ABI already exists

`Base Void` lowers to `layout_unboxed_product []` (`lambda.ml:3185`), and the
backend drops zero-width things end to end. Measured against the baseline
compiler with `type t : void`:

    let f (x : t) (y : int) = y + 1
    -> camlW__f_0_2_code (y/301: int) : int (+ y/301 2)

The void parameter is absent, and the function has the same ABI as one written
without it. Records behave the same way:

    type r = { a : int; ghost : t; b : int }
    let mk a b (g : t) = { a; ghost = g; b }
    -> camlR__mk_0_3_code (a: int b: int) : val (alloc 2048 a b)

Header 2048 is a two-word block, so the void field takes no slot.
`mixed_product_bytes.ml:81` gives void fields zero bytes.

Currying uses ABI arity rather than type arity, so partial application already
works and the arities may diverge:

    let f (x : t) (n : int) (m : int) = n + m
    -> camlP__f_0_4_code (n: int m: int),  caml_curry_V_V
    let use (x : t) = f x 1
    -> camlP__use_1_5_code () : val ...

Note that `use` compiles to a genuinely nullary function, since its only
parameter was erased. A normal `f ()` passes unit as a real immediate, so this
is a new shape and wants a test even though it works.

The strategy is therefore to give an erased occurrence a zero-width
representation and let the existing void path do the work. No new calling
convention.

### Getting the mode to codegen

Locality is the precedent. The typechecker resolves the mode, stores it on the
Typedtree node (`alloc_mode : Mode.Alloc.r`, `typedtree.mli:116`, carried by
`Texp_tuple`, `Texp_construct`, `Texp_function` and others), and `translcore`
reads it through `transl_alloc_mode` to emit `Alloc_heap` or `Alloc_local`.
Erasure follows the same route.

The mechanism differs even though the plumbing matches. Locality selects an
allocator and the value is still one word in one register, so the ABI shape does
not move. Erasure changes the shape.

### The seam

Representation is computed from the type alone. Every entry point in
`typeopt.mli` takes a type and an environment and no mode: `layout`,
`layout_of_sort`, `function_return_layout`, `value_kind`. Threading an
occurrence's mode into representation selection is the main cost of this piece
and the place where the mode and kind separation gets breached. Scope it before
starting, since it decides whether the piece is a week or a month.

Void gives the representation and nothing else. Void is a property of types and
erasure is a property of occurrences, so there is no existing analogue of "may
only be used in erased contexts". That part is new, and it is where review
effort should go.

## Constraints

Erasure cannot be mode-polymorphic. OCaml does not monomorphize, so a parameter
whose erasure varies per instantiation would need two ABIs.

The `.cmi` has to record erasure, because callers generate the calling sequence
from it.

A signature must not be able to change a field's erasure, or the two sides of a
module boundary disagree about layout. For parameters the rule is the argument
invariance above; for fields, fail closed.

Arrays reject void today. `Unsupported_void_in_array` fires at four sites in
`typeopt.ml`, so erased array elements are out unless that is extended.

## Scope of this piece

`erased_ e` type-checks `e` and then deletes it, including any effects it would
have had. `erased_ (print_string "hi")` prints nothing. This is deliberate for
now and it is unsound as a specification mechanism, which is why Why3 requires
ghost code to have no observable effect on non-ghost state.

The fix is to require `e @ total`, tying this piece to the totality piece. That
is deferred so the two can be built independently. Record the hazard in the
documentation meanwhile, so nobody builds on the current behaviour.

In scope: the axis and its lattice, `erased_`, the `@ erased` mode annotation,
the `@@ erased` field modality, the ambient rule with its application and
closure cases, zero-width representation for erased parameters, fields and
returns, `.cmi` round-tripping, and printing.

## Cases to pin before writing code

- An erased value returned from a retained function. `let f x = erased_ (g x)`
  gives `f : _ -> t @ erased`, where the call is retained and the result erased.
- A record all of whose fields are erased, which is legitimately zero-width.
- A constructor with erased arguments, the variant analogue of the field case.
- An erased mutable field, where writing is a no-op. Probably reject.
- Inference direction. Legacy is Retained and modes zap to legacy, so an
  unannotated binding cannot drift to Erased and vanish. The failure mode is
  invisible, so test it rather than reasoning about it.

## Tests

`testsuite/tests/vox/erasure.ml` for typing, plus codegen checks.

Typing, the ambient rule:

- a retained value accepted where erased is expected, an erased value rejected
  where retained is expected
- an erased value rejected as an `if` condition and as a match scrutinee in
  retained code, accepted for both inside `erased_`
- defaults: an unannotated value is retained and prints as it does today

Typing, application:

- `f x` rejected when `f @ erased`
- `erased_ (f x)` accepted for the same `f`
- an erased *parameter* and an erased *function* handled independently

Typing, closures:

- a closure capturing an erased value is retained, and calling it works
- the same closure rejected when it uses the erased value at a retained
  position, `fun y -> x + y`
- `erased_ (fun y -> g y)` accepted with `g @ erased`, which is the body rule
- an erased closure capturing a retained value, accepted

Boundaries:

- `.cmi` round-trip for an erased parameter and an erased field
- sealing, argument position, both directions: a signature with a retained
  parameter against an implementation with an erased one is rejected, which is
  the ABI-unsafe direction contravariance would otherwise permit, and the
  reverse is rejected too, since the rule is invariance
- sealing, return position, both directions: a retained-returning implementation
  against an erased-returning signature is accepted, the reverse rejected
- the same four cases through an explicit `(e :> t)` coercion
- a signature that changes a field's erasure is rejected

A trap worth knowing before writing any of these. Mode subsumption tests are
easy to write non-discriminatingly, because an unused parameter's mode is left
unconstrained by inference and will unify with whatever the signature asks for.
Testing locality this way accepted in *both* directions, which looks like "modes
do not submode on arguments" and is really "the test proved nothing". Force the
mode with a real use, such as letting the argument escape into a global
reference, and check that the fixture fails when the rule is removed.

Codegen, which is what this piece is really about, so check emitted code rather
than only that it compiles:

- an erased parameter is absent from the native function, and the ABI matches
  the same function written without it
- an erased record field takes no slot, checked against the allocation header
- a function whose parameters are all erased compiles to a nullary function
- partial application across an erased parameter uses the ABI arity
- a closure capturing an erased value has no slot for it, and the enclosing
  function's ABI is unchanged
- an erased return produces a function returning nothing
- the erased expression's effects are gone, pinning the deliberate unsoundness
  so the later totality requirement shows up as a diff

## Deferred

Requiring `e @ total` in `erased_ e`. Erased array elements. Interaction with
refinement predicates, which is what erasure exists for.

## Decisions taken during implementation

Recorded per AGENTS.md: choices at points the doc left open or where reality
disagreed with the doc, with alternatives considered.

### The ambient rule is an environment flag checked at the submode funnel

"Checked at ambient Erased" is implemented as a flag on the typing
environment (`Env.enter_erased_context`), consulted at the single point
where every expression's mode meets its expectation (`Typecore.submode`):
inside an erased context, the erasure axis simply is not checked, because
the context is deleted from compilation. This is compositional — it covers
variables, results of applications, nested `erased_`, everything — and it
gives the closure body rule for `erased_ (fun ...)` for free (the flag is
in the environment when the body is typed).

History: the first implementation was an environment *lock* that lowered
the erasure of looked-up identifiers. Review found it non-compositional
(an erased application result in a read position inside `erased_` was still
rejected); the funnel formulation subsumes it and is smaller.

### The default expectation requires retained; erased-tolerant positions are
the exception

`Erased` is the top of the axis, so an *unconstrained* expected mode would
accept erased values — and "read position" is a semantic notion nobody can
grep for. Review found exactly this hole (`while` and `for` used a raw
`Value.max`). So the polarity is flipped: the permissive expected mode
(`mode_max`) requires Retained on the erasure axis, and the erased-tolerant
positions are the closed, spelled-out set:

- type-driven positions (an `@ erased` arrow argument, an erased return);
- erased contexts (the ambient rule above);
- statement position (`e; ...`), which discards the value.

Positions built from fresh mode variables still need an explicit constraint
where they read: destructuring patterns (constants, tuples, constructors,
records, arrays, lazy, unpack, intervals — variables, wildcards and aliases
bind without reading, so `let x = erased_ e in ...` works), record field
access and mutation (`type_label_access`), and the function position of an
application (there is no closure to jump to).

### Closures: carve-outs in three places

- `Env.closure_mode` / `const_closure_mode` (captures): the capture check
  meets the captured value's erasure down to Retained; body uses still see
  the true erasure.
- `close_over` (partial application, both the mode and const versions): an
  erased argument is not stored in the wrapper closure, so its erasure does
  not join into the closure's mode. Without this, `use x` with an erased `x`
  incorrectly made the result erased.
- moregen (`moregen_alloc_mode`): erasure is equated in argument position in
  *both* directions there, not one direction plus the ambient variance —
  the ambient direction flips at each arrow nesting, and review produced a
  sealed module whose callback ABI mismatched at run time through a doubly
  nested arrow. The coercion path (`subtype_rec`) recurses with swapped
  sides, so its single check per level already yields invariance.

The doc offered a modality-based framing for captures; the machinery applies
locks uniformly across captures, so the carve-out was the available route
(as the doc anticipated).

### Erasure is fixed top-down: current approximation

A lambda's body is an erased context exactly when the lambda is syntactically
under `erased_`. A lambda written directly at an `@ erased` arg position
without `erased_` is checked with a retained body — stricter than the doc's
rule, sound (retained-checked bodies are usable as erased), and expressible
by wrapping in `erased_`. Reading "the expectation's erasure is constantly
Erased" off a right-mode without zapping it needs solver support that
doesn't exist; deferred.

### Codegen realized via the void layout

Erased occurrences translate at `Punboxed_product []`:

- function parameters whose arrow arg mode is erased (including
  function-cases), marked before body translation;
- variables let-bound to erased expressions (`erased_ e` or an alias of an
  erased variable), including through `Matching.for_let` via a Void sort;
- application arguments at erased arrow positions: erased arguments pass
  zero-width; retained arguments are evaluated for effects and dropped
  (`Lsequence`), pinning the doc's boundary semantics;
- the eta-wrapper parameter for out-of-order partial applications;
- over-applications of primitives thread the same per-position flags.

The per-unit table of erased identifiers is reset from `Translmod.reset`:
`Ident` stamps restart for every unit, so review showed a stale table giving
*another unit's* like-named parameter the void layout.

An erased expression at any other position translates to a placeholder of
whatever layout the context requests (`dummy_constant` for values, zeros
for unboxed numbers, recursively for unboxed products). This is what makes
structural erasure total: a tuple with an erased component builds with a
placeholder word and is then dropped at its erased boundary, instead of
putting a void operand inside a value-layout block (review found a dozen
compiler aborts of that shape). Vector layouts have no placeholder and
remain a compiler error.

Measured results match the doc's void experiments: erased params are absent
from native functions, a function whose only param is erased compiles to a
nullary symbol, and a closure whose only capture is erased becomes a static
closure (no allocation at all).

`erased_ e` at a retained-layout position (an erased value smuggled through
a value binding whose erasure the analysis doesn't see) emits
`Lambda.dummy_constant` — a recognizable placeholder that only other erased
positions consume.

### Deferred beyond the doc's deferred list

- **Erased record fields**: the doc's `@@ erased` mechanism does not exist.
  Comonadic modalities are `Meet_const` only — they can strengthen a field
  relative to the record (`@@ global` in a local record) but not weaken one,
  and an erased field in a retained record is a weakening. `@@ erased`
  today parses and is correctly reported as redundant (warning 220). Options
  are a comonadic `Join_const` modality (touches modality composition,
  zapping, inclusion, cmi) or a special-cased representation-bearing marker;
  both are big enough to be their own piece. **The doc's analogy to
  `@@ global` is wrong in direction; this needs a design decision.**
- **Erased returns**: typed correctly (`int -> int @ erased` infers and
  round-trips) but represented as a value-layout placeholder, not
  zero-width. Making them void requires translation-time knowledge of every
  application's return erasure; the placeholder is correct, just one word.
- **Structure-level erased bindings** (`let x = erased_ 5` at toplevel) are
  rejected: the structure's mode is the join of its items' modes, and
  compilation units are legacy. Insulating them needs the same missing
  modality direction as record fields.
- **Erased optional parameters** are not given the void representation, on
  both sides of a call: `function_arg_erasures` reads optional labels as
  retained, so caller and callee agree.
- **Externals cannot take erased parameters** (rejected at declaration):
  there is no erased calling convention across the FFI.
- **`erased_` in quotations** is rejected.
- **Erased args to external primitives** keep the retained calling
  convention (no `%`-primitive has erased params today).

### Known gap: instantiation treats arrow erasure as implicitly polymorphic

`instance` copies a generic arrow's modes into fresh, *independent*
variables. For locality this loses only precision; for erasure it changes
the ABI: `let id2 : ('a -> 'b) -> ('a -> 'b) = fun g -> g` applied to
`f : int @ erased -> int` gives an application whose visible arrow reads
retained while the underlying closure has the erased ABI, and the program
aborts at run time (review finding, repro in the report). Relatedly, an
unannotated function's parameter erasure can be inferred from a call site
(`let f x = 42` + `f (erased_ 5)` gives `f : 'a @ erased -> int`), against
this doc's "never inferred" rule — self-consistent per unit and across
`.cmi`s, but a silent ABI change.

Both have one root: arrow-mode instantiation must either preserve the
erasure component's identity (no fresh copy) or fix it to Retained unless
annotated. Each option needs mode-solver work that this piece does not
attempt; flagged for a decision.

### Erasure and mode crossing

No type crosses erasure, ever: an erased value does not exist, so treating
it as retained is unsound regardless of the type. Rather than pinning every
construction site (the first attempt; review found it scattered across five
sites and still missed one), the axis is pinned at the two points where
stored bounds are *read* as crossings: `Btype.Jkind0.Mod_bounds.crossing`
and `Axis_lattice.to_mode_crossing` (the ikind path). Construction sites may
pass through `min` freely; `mod erased` / `mod retained` are rejected as
kind modifiers, `mod everything` excludes erasure (precedent: staticity),
and the typecore/ctype crossings that do not pass through a reader take
`~erasure:false`. Zero-width types could in principle cross erasure soundly;
not exploited.
