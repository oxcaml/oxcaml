# Vox totality and logicality axes

Two new mode axes: totality and logicality. Linearity (`many`/`once`) already
exists in OxCaml and this piece leaves it alone.

    Totality    Total | Partial        comonadic
    Logicality  Physical | Logical     monadic

`total` says a function terminates and has no effects, so a specification may
call it. `logical` restricts a value to the parts the logic can talk about:
reading mutable state through it is rejected, much as it is through a
`contended` value. A logical value still exists at run time. Erasure, which is
what removes a value from compilation, is a separate axis in its own piece.

## Lattices and defaults

| axis | min | max | legacy | fragment |
|---|---|---|---|---|
| Totality | `Total` | `Partial` | `Partial` | comonadic, `Common_axis_pos` |
| Logicality | `Physical` | `Logical` | `Physical` | monadic, `Common_axis_neg` |

Both are two-point total orders. Legacy sits at the permissive end of each, so
every unannotated value is `partial` and `physical` and no existing type changes
meaning.

`Total <= Partial`, so a total function is accepted where a partial one is
expected. `Physical <= Logical`, so unrestricted access can be weakened to
logic-only access and never the reverse. Contention has the same shape, with
`Uncontended` as the accessible end.

## The axis pair

Totality and logicality are a comonadic/monadic pair, in the same sense as
portability and contention. The mode system already couples each comonadic axis
to a monadic partner, in `comonadic_to_monadic_op_max` (`mode.ml:2517`):

    linearity     ->  uniqueness
    portability   ->  contention
    totality      ->  logicality
    statefulness  ->  visibility

`Totality_to_logicality_op` and `Logicality_op_to_totality` sit beside
`Portability_to_contention_op` and its inverse. This is why the two axes belong
in one piece: neither does anything alone, and the coupling is a registration in
an existing table rather than a new rule.

Two consequences, both inherited from the portability and contention twin:

- A `total` closure can only capture `total` values, exactly as a `portable`
  closure can only capture `portable` ones. This is the ordinary comonadic
  closure lock.
- A `total` closure bumps its captures to `logical`, as a `portable` closure
  bumps its captures to `contended`. Inside a total closure you cannot read
  mutable state through anything you captured, which is what makes the closure
  callable from a specification.

The second is what gives logicality its purpose, and it needs no new rule. It
falls out of `comonadic_to_monadic_op_max` once the pair is registered.

## Surface syntax

Mode position:

    let f (g @ total) = ...
    let x @ logical = 42
    val f : 'a @ total -> 'a

Modality position:

    val empty : t @@ total
    val logical_int : int @@ logical

The names are `total`, `partial`, `physical`, `logical`. In modality position
the Physical end is spelled `nonlogical`, and `@@ physical` does not exist,
because the modality parser reads the monadic axis as a join.

Kind modifiers (`mod total`, `mod logical`) are supported.

## Mode crossing

Both axes participate in mode crossing and appear in `Jkind_axis.Axis`.

A type crosses logicality when it has no mutable parts, because the restriction
is then vacuous. A `logical` `int` may be used where a `physical` `int` is
expected, and the same holds for arrows. An `int ref` or an `Atomic.t` does not
cross, so it stays logical and reads through it stay rejected. The negative
cases carry the meaning of the axis, so cover them first.

A type crosses totality when it has no functions, just like portability.

## Scope of this piece

The axes, their lattices, surface syntax in mode and modality position, mode
crossing, submoding, printing, and the rules that make `total` mean something.

### What makes a closure partial

Three sources, and only the first is free.

Captured partial values. `ref`, `!` and `:=` are ordinary stdlib values sitting
at legacy `partial`, so a closure that uses one captures it and becomes partial.
The comonadic closure lock gives this with no extra rule.

Syntactic effect forms, which have no value to capture: `while`, `for`, mutable
record field assignment, array element assignment, instance variable assignment.
Each needs an arm that walks the enclosing closure locks and constrains them to
`partial`. Upstream already does this for effect handlers, which force enclosing
functions nonportable and stateful, via `walk_locks_for_legacy_construct`
(`env.ml:4002`); vox2 adds `constrain_enclosing_totality_partial` beside it
(`env.ml:4028`). Copy that shape rather than inventing one.

Recursion. A `let rec` self-reference makes the binding partial. With no
structural recursion check and no decreases clauses, no recursive function is
total in this piece.

### The hereditary rule

Capture-based propagation misses a local `let rec` inside a total closure,
because the self-reference never crosses a capture boundary. vox2 closes this
with an append-only closure-lock stack in `Env`: a function literal nested
inside a closure demanded total must itself be total, whatever the source of its
partiality, and walking the stack reaches let-bound literals that the
expected-mode edge (return, if and argument position) misses
(`typecore.ml:6790`). Take that approach. It is what makes the discipline hold
wherever the literal appears rather than only in the positions the expected mode
reaches.

### The primitive allowlist

For anything interesting to be total, total primitives have to be declared total.
Otherwise only closures that call nothing qualify.

Three further sources of partiality have no value to capture and no syntactic
form dedicated to them, so decide whether this piece covers them: a
non-exhaustive match, which can raise `Match_failure`; division and modulo,
which raise on a zero divisor; and `assert`. `raise` itself needs no rule, being
a value left at `partial`.

## Inter-axis implications

Total implies stateless.

## Tests

`testsuite/tests/vox/totality.ml`, expect tests.

- submoding both ways on each axis: `total` accepted where `partial` is
  expected, `partial` rejected where `total` is expected, and the same pair for
  physical and logical
- defaults: an unannotated value is `partial` and `physical`, and prints as it
  does today
- crossing positives: a `logical` `int` and a `logical` arrow used at `physical`
- crossing negatives: a `logical` `int ref` and a `logical` `Atomic.t` stay
  logical
- modality position: `val empty : t @@ total`, `val x : int @@ logical`
- kind modifiers: `mod total` and `mod logical` accepted
- printing: annotated values print their axis, unannotated values print exactly
  as before. This is the baseline-churn guard.
- with the allowlist: `let increment @ total = fun x -> x + 1` accepted, and a
  closure calling a partial function rejected at `@ total`

The capture bump, which is the pair working:

- a `total` closure capturing a `partial` value, rejected
- a `total` closure capturing an `int ref` and then reading it, rejected because
  the capture arrives `logical` and the read needs `physical`
- the same closure capturing an `int`, accepted, because `int` crosses
  logicality and the bump is vacuous
- the portability and contention twin behaving identically on the same
  fixtures, as a check that the pair was registered rather than special-cased

Effects, each rejected at `@ total`, and each also accepted without the
annotation so the test discriminates:

- `ref`, `!` and `:=`
- `while` and `for`
- mutable record field assignment, array element assignment
- a closure that captures a partial value from an outer scope

Recursion, which is where the interesting failures are:

- `let rec f x = f x` rejected at `@ total`
- the boundary case: a top-level recursive function captured into a total
  closure, rejected because the capture is partial
- the hereditary case: the same `let rec` written *locally* inside a total
  closure, where the self-reference never crosses a capture boundary. This is
  the one capture-based propagation misses, so it is the test that says whether
  the closure-lock stack works.
- a let-bound function literal inside a total closure, which the expected-mode
  edge does not reach
- mutual recursion, `let rec even x = ... odd x and odd x = ... even x`
- a recursive function that is only returned, never applied

## Decisions taken during implementation

Points the doc left open or where the implementation departs from vox2, with
the route taken and why.

- **Hereditary rule: closure-lock walk only.** vox2 carries both an
  `enclosing_totality` field threaded through `expected_mode` (the
  expected-mode edge) and the closure-lock walk. The body of a function literal
  is always typed under its enclosing closure locks, so submoding the literal's
  totality into every enclosing lock (`type_function`, first value parameter)
  subsumes the edge; the extra field was dropped.

- **(Rec) as a floor on the group's mode variable.** The recursive group's
  mode variable has its totality floored at `partial`, so the bound variables
  are partial both inside the right-hand sides and after the binding. This is
  deliberately group-wide: the base typechecker gives the whole `let rec`
  group a single mode variable, so a member that never refers to the group
  (`let rec a x = a x and b x = x`) also comes out partial (pinned by a test).
  Per-member precision needs per-binding mode variables for recursive groups,
  a base-machinery change left as a follow-up; moving such a member out of
  the group is the workaround. An arrow-free recursive value (`let rec x =
  1`) still crosses totality at its use sites.

- **The allowlist** covers `%identity`, integer and float arithmetic, boolean
  connectives, `%field0_immut`/`%field1_immut` (fst/snd), and
  `%apply`/`%revapply`. `%divint`/`%modint` are excluded (raise on a zero
  divisor). Comparisons are excluded (raise on functions, diverge on cyclic
  values); vox2's machinery admitting comparisons at immediate operand types
  was not ported — it can be a follow-up if total code needs `=` on ints.

- **`assert` and non-exhaustive matches are partial.** Both can raise
  (`Assert_failure`, `Match_failure`), both are syntactic with no partial
  value to capture, so both walk the closure locks like `while` and `for`.
  Non-exhaustive `let` patterns too. Exception handlers were already forced
  legacy (hence partial) by `walk_locks_for_legacy_construct`.

- **Reading or writing a mutable field through a `logical` value is rejected
  in the projection/mutation rule** (`mode_project_mutable`,
  `mode_mutate_mutable` require `physical`), completing "reading mutable state
  through it is rejected, much as it is through a contended value". vox2 only
  rejects reads that pass the ref to a function like `!` (argument submoding);
  a direct `r.contents` through a logical record slips through there.

- **Mutable-state *creation* is not constrained.** Allocating a mutable record
  or array literal inside a total closure is allowed; only assignment, loops,
  raising forms and captures constrain totality. The doc lists assignment
  forms only, and creation alone neither diverges nor observably affects
  state. (`ref` itself is partial only because it is an unlisted primitive.)

- **"Total implies stateless" is NOT implemented.** Implementing it as
  annotation defaulting (the mechanism behind `stateless` implying `portable`)
  makes every totality rejection surface as a statefulness error — legacy
  values are both stateful and partial, and the statefulness violation is
  reported first — and forces the primitive allowlist to also claim
  statelessness. The observable content of the implication is already
  delivered: state *writes* force partial syntactically, and state *reads*
  through captures are blocked by the logicality bump. Flagged for review
  rather than silently included.

- **`@ partial` on a binding inside a total closure does not force the
  enclosing closure partial.** The annotation is a bound on the binding, not
  evidence of partiality; if the body has a real partiality source, that
  source constrains the enclosing locks itself. vox2 has explicit arms for
  this; they reject only functions whose bodies are actually total, so they
  were not ported.

- **Toplevel bindings** stay pinned to legacy except that a `total` or
  `logical` annotation raises the floor on its axis, and an unannotated
  binding's logicality is left free upward (so rebinding a logical value
  works). This mirrors how toplevel bindings interact with `portable` today:
  the annotation is enforced on the right-hand side, and the printed
  signature does not show the axis (`val f : int -> int`), but the binding is
  usable at `total` later in the same unit.

- **Two latent vox2 defects fixed rather than ported**: `Mod_bounds.
  less_or_equal` and `get_max_axes` in `jkind.ml` enumerate axes by hand and
  in vox2 miss the two new axes, which made `mod total` invisible to kind
  printing/equality whenever it was the only modifier. vox2's
  `filter_axes_without_kind_modalities` in `ikind.ml` (which silently discards
  totality/logicality disagreements from subkind checks, and would let
  `type t : value mod total = int -> int` typecheck) was not ported; the new
  axes participate in subkind checks like every other axis.

- **Baseline shifts accepted during promotion.** (a) Inferred module signatures
  now include `total` beside `stateless` (same zap-to-strongest rule the
  existing comonadic axes use). (b) Arrow kinds print `mod ... logical`, since
  arrows cross logicality. (c) A `mod` list that crosses every old axis no
  longer prints as `everything`, because `everything` now includes the new
  axes. (d) `immutable_data with (t @@ immutable)` where `t` has mutable parts
  is now rejected: the `immutable` visibility modality does not lift
  logicality, so such a type no longer crosses it. Conservative and sound; a
  visibility-to-logicality modality implication could relax it later.
  (e) A degenerate recursive variant whose parameter never occurs in any
  payload (`type 'a many = Foo of ('a * 'a) many | Leaf`) now crosses
  contention, which the fixture itself marks as the desired outcome;
  non-degenerate variants still do not cross (checked with discriminating
  probes).

- **Totality is capture-based, exactly like portability.** Parameters are not
  captures: a total closure may call a `partial` parameter, read mutable
  state through a parameter, send a message to a parameter object, and the
  `%apply`/`%revapply` operators are total even though they call their
  function argument. The guarantee `total` provides is compositional:
  a total context can only supply total (and logical) arguments — submoding
  stops partial values at every entry point into the total fragment — so a
  call graph that lives entirely in the fragment terminates and performs no
  effects. The alternative reading, where `f @ total` promises termination
  for arbitrary well-moded arguments, would require constraining the callee's
  totality into the enclosing locks at every application; that would also
  diverge from the portability/contention twin, which accepts all three
  shapes above (pinned by tests). Flagged for explicit confirmation because a
  reviewer read the axis the other way.

- **KNOWN GAP: recursive modules can claim totality circularly.** In
  `module rec M : sig val loop : int -> int @@ total end = struct let loop x
  = M.loop x end`, the body is checked against the declared signature, so the
  `total` claim justifies its own recursive call and `M.loop` diverges at
  mode total. The (Rec) floor only covers `let rec`. Portability tolerates
  this self-assumption (it is a coinductive property); termination does not.
  The fix is to weaken the totality component of every value modality in the
  recursive-approximation environment (the analogue of (Rec) for
  `module rec`), which needs a signature-modality rewriting pass including
  named module-type expansion — deferred to a follow-up and pinned by a
  fixture in the test file. Without an explicit `@@ total` claim, recursive
  module values correctly stay partial.
