# Vox type formers

We add one type former and one binding rule:

    t{ predicate }       -- refinement of t; the value is _
    x:t -> b             -- x names the value and scopes over b

Examples:

    int{ _ >= 0 }                                  -- the hole _ is the int itself
    x:int{ x > 0 } -> int{ _ >= x }                -- x scopes over the codomain
    unit{ invariant tree = true }                  -- a proposition; no name needed
    val sub : s:string -> int{ _ < length s } -> char

Refinements are `Trefine` nodes in the type graph, not side tables. The refined
type carries the value's name, its payload type t, and the predicate expression
p. The predicate has the same shape as an ordinary OxCaml expression
(Typedtree), except it is a second version of the type of expressions for inside
the type language, plus it only allows the subset of expressions that are total
(e.g. tuples yes, ref no, let yes, while no).

The dependent function type is not a new type former. Instead we add an optional
binder to the existing function type. We use the same ident-based binding
mechanism as `Tpoly`, NOT De Bruijn or locally nameless.

## Labels and binders

OCaml has exactly one name-before-type slot in the type grammar (`arg_label ::=
optlabel | LIDENT COLON | empty`) and labels own it. There is no lighter free
spelling for a binder — the alternatives are heavier (parens) or misleading — so
we share the slot, and decide by use:

For `x:T -> b`:

- `x` is a **positional binder** iff `x` occurs free in a refinement in `T` or
  in `b`. The argument is then not labelled and the call site is unchanged:
  `f v`. The binder scopes over `T`'s own refinement and over `b`.
- Otherwise `x:T` is an ordinary labelled argument, exactly as today.
- `~x:T` is always a label, whatever occurs anywhere. This spelling is new
  (TILDE does not appear in the type grammar today, only in patterns and
  expressions) and matches how labels are written in terms and at call sites.
- `_` denotes the value of the refinement it appears in, and is always
  available.

All four combinations are spellable, and the choice of spelling is the signal:

    int{ _ > 0 }       -- positional, hole
    n:int{ n > 0 }     -- positional, named
    x:int{ _ > 0 }     -- labelled, hole
    ~x:int{ x > 0 }    -- labelled, named

Using the name is what makes it a binder. That is what keeps a name available
whenever one reads better than the hole — `m:matrix{ rows m = cols m }` rather
than `matrix{ rows _ = cols _ }` — without a carve-out for the argument's own
refinement.

Backward compatibility follows: existing code contains no refinements, so no
name can occur in one, so every existing `x:T` stays a label. `~x:T` did not
parse before, so no existing code uses it.

The intended end state is the uniform rule *bare `x:` binds, `~x:` labels*. The
third bullet is a compatibility shim for the existing tree; new code can write
`~x:` for labels from the start and converge on it.

Consequences to accept:

- A single arrow can mix conventions: in `x:int -> y:int -> int{ _ >= x }`, `x`
  is positional and `y` is labelled, and reading the convention requires
  scanning the whole type. Warn when one arrow ends up mixed.
- Retrofitting a spec onto an existing labelled function turns it positional as
  soon as the spec uses the name — including in the argument's own refinement —
  so this fires readily when specifying labelled APIs. The fix is one edit at
  the definition (`~x:`, or use `_`), and the breakage is a compile error at the
  call sites, not silent. Writing `~x:` on labelled APIs from the start avoids
  it entirely.
- Optional and position parameters never bind: the argument may be absent, so
  "the value of this parameter" is not defined. `?x:T` is always a label, and
  its refinement uses `_`.
- Binders under `Tpoly` are allowed. Open question to settle during
  implementation: whether the value binder scopes inside or outside the type
  quantifier, and whether `Tpoly`'s alpha-renaming freshens the value binder as
  well as the type variables. `Subst` will freshen two kinds of binder in one
  node and the order matters for capture.

The occurrence test decides a label, which is part of the type, so it runs
before name resolution: a syntactic free-occurrence check over the parsetree of
`T` and `b`, respecting binders introduced inside predicates (a predicate's own
`let`, `match` cases, nested arrows). This is the only genuinely novel logic in
the piece and the label/binder rule inherits any edge case it has, so implement
and test it first rather than last.

## Scope of this piece

No typing rules. Refinements are parsed, translated, printed and carried through
the type graph, and are otherwise inert: nothing is checked, nothing is stripped,
no verification condition is generated, no solver is involved.

A refined type is rigid and behaves like any other type. There is no
introduction rule and no elimination rule yet, so this piece can declare and
print refined types but cannot apply or consume a value of one:
`let f (x : int{ _ > 0 }) = x + 1` does not typecheck, and neither does `f 5`.
That is expected here; the rules land in a later piece. Fixtures must therefore
declare, annotate and print rather than use.

## Positions

A refinement is a type former: permitted in every `core_type` position, with no
exceptions. Restrictions that do not fall out of the semantics are not worth
their cost — they have to be specified, implemented, tested and explained, and
each one is a guess that gets baked in.

Type-declaration right-hand sides are included deliberately. `type wf = tree{ inv _ }`
is the main thing standing between the precondition/postcondition shape and
unreadable signatures, since most preconditions are named predicates.

## Equality and unification

- Refinements are rigid: never inferred, never solved for.
- `int{p}` and `int` do not unify. One-sided refinement is a clash.
- Two refined types are equal iff payloads are equal and predicates are
  syntactically alpha-equivalent — no normalization, no entailment, no solver.
- A predicate's interior types are updated by substitution like any other type.
  `Subst` freshens binder stamps on import; `Btype` does not.
- Type variables inside a predicate are live in the generic type-graph
  traversal: `Btype`'s fold and map descend into the predicate, so levels,
  generalization and the occur check reach them. They behave as they would
  anywhere else in the type.

## Erasure

The payload determines layout, jkind and runtime representation. Refinements
never reach lambda. Use "payload" consistently, in prose and in the field name.

## Syntax

- `t{p}` binds at atomic_type precedence, so `int{ _ > 0 } list` is
  `(int{ _ > 0 }) list`.
- `int {p}` and `int{p}` are the same; no whitespace sensitivity.
- The predicate sublanguage is a fixed closed subset enforced by construction in
  the Types mirror, not by a check on an unrestricted AST.
- Non-total forms (while, ref, assignment, sequencing) are rejected at
  translation with a form-specific message, not a generic parse error.
- Surface parsetree gains real `Ptyp_` constructors. No extension-node encoding.

## Printing

- The printed form is canonical source and round-trips.
- A binder is printed only when it is mentioned; otherwise `t{p}` with `_`.
- Printing must reproduce the label/binder distinction it read, including `~x:`.
- Error messages show the refinement, not the bare payload.

## How complete to be

Split by whether an arm encodes a decision.

Structural arms have one sensible implementation — descend into payload, view
type, and the predicate's interior types. There is no decision to defer and they
run on every path, including printing and error reporting. Write them:

- parser, parsetree, `ast_helper`/`ast_mapper`/`ast_iterator`
- `typetexp` translation, including the occurrence test
- `Btype` fold/map/iter
- `Subst` — runs as soon as a refinement appears in a signature, which the
  round-trip test requires
- `out_type`/`oprint`/`outcometree`, `printtyped`, `untypeast`, `pprintast`
- `Ctype`: structural equality, the rigid clash, `expand_head` → `Cannot_expand`
- `cmi_format`

Relational arms encode rules a later piece owns. Leaving them unimplemented is
the specification: subtyping, seal-context matching, expected-type weakening,
coercion, unification arms beyond the rigid clash.

Two kinds of unimplemented, and the difference matters:

- Unreachable from well-formed source in this piece → `Misc.fatal_error`.
- Reachable from valid source but unspecified → a real located error. A crash on
  valid input is a bug even in a work-in-progress piece.

Because refinements are allowed in every position, more arms are reachable than
you might expect. Classify each `Trefine` arm deliberately rather than reaching
for `assert false`.

## Tests

`vox/type-formers.ml` expect tests. Round-trip printing is necessary but weak —
it can pass while the type is semantically wrong — so also:

- the four spellings, each checked for both the refinement and the calling
  convention: `int{ _ > 0 }`, `n:int{ n > 0 }`, `x:int{ _ > 0 }`,
  `~x:int{ x > 0 }`
- a name occurring only in a *later* refinement (`x:int -> int{ _ >= x }`) binds
- `x:int{ x > 0 } -> int{ _ >= x }` binds once, not twice
- a mixed arrow, checking the warning fires
- occurrence-test edge cases: a name shadowed by a predicate's own `let` or
  `match` binder, and a name occurring only inside a nested arrow's refinement
- `?x:int{ _ > 0 }` stays a label and does not bind
- alpha-equivalence: differently-named binders compare and print as expected
- `.cmi` round-trip: write a refined signature, read it back, print it. The
  cheapest guard on the representation, and where a `Subst` or `Btype` gap shows
  up first.
- rejection cases as RED fixtures: unbound name in a predicate, `while` in a
  predicate, empty `{}`
- printing in error messages, not only at the toplevel
- refinements across positions — a record field, a constructor argument, a type
  declaration right-hand side — each with a `.cmi` round-trip
- a recursive type declaration whose predicate mentions the type being defined,
  to pin whatever the occur check does

## Deferred to later pieces

Introduction and elimination rules, including whether a refined type is usable
where its payload is expected; module sealing and signature matching;
verification condition generation; solver interfaces; naming the result of an
arrow (`_` suffices for now, and `-> z:t{p}` is free syntax if it is ever
wanted); rollout concerns (extension gating, `.cmi` magic bump, the vendored
merlin copy, keeping refinement-free printing byte-identical).

## Decisions taken

Recorded per AGENTS.md: where the design had a real fork, which route and why.

- **Sharing the label slot** rather than adding a parenthesised binder form.
  `(x : t)` was the alternative. It never changes a calling convention, but it
  puts parens on every argument a postcondition mentions, which in the
  precondition/postcondition shape is most of them. Sharing the slot keeps the
  common case light; the cost is mixed conventions within an arrow and the
  retrofit edit, both listed above.
- **`~x:` for explicit labels** rather than `(x:t)`. `~` is the label marker
  everywhere else in the language, whereas `(x : t)` in terms is a *positional*
  parameter, so parens-means-label would invert the existing intuition. `~x:`
  also gives a convergence path to a uniform rule.
- **Keeping `_`** rather than requiring a name everywhere. `unit{p}` is a
  proposition whose value is irrelevant, and naming it is pure noise.
- **The occurrence test covers the argument's own refinement**, not only the
  rest of the arrow. Excluding it would have made `n:int{ n > 0 }` a labelled
  argument, so a positional argument could only be refined through `_` — forcing
  the hole exactly where a name reads better. The cost is that the convention
  flips more readily on retrofit, mitigated by `~x:`.

## Decisions taken during implementation

Recorded per AGENTS.md: points where this document was ambiguous or silent,
which route was taken and why, and what was considered.

- **Where the value's name lives.** The document says the refined type
  carries the value's name. In the implementation `Trefine` carries only
  payload and predicate; the hole `_` is its own predicate form
  (`Rexp_hole`, meaning "the value of the innermost enclosing refinement"),
  and every *name* for the value is an arrow binder: `arrow_desc` carries
  an `Ident.t option` which for `x:T -> U` scopes over the predicates of
  both sides and for `~x:T -> U` scopes over the predicates of `T` only.
  This is what makes `x:int{ x > 0 } -> int{ _ >= x }` bind once — there is
  no second, per-refinement binder to alpha-rename or to capture. The
  alternative (a binder ident on every `Trefine`, holes as references to
  it) needs freshening and pairing machinery on every refinement for no
  observable difference. The binder is `Some` only when some predicate
  mentions it.
- **Scope of `~x:`.** Fixed to: all refinement predicates within the
  argument's type, not just its top-level refinement, and never the
  codomain. This is the domain-only restriction of the positional binder's
  scope, so `~x:(int{ x > 0 } * int)` works and there is no special case
  for "the argument's own refinement".
- **Scope of a positional binder over its own domain.** The document says
  the binder scopes over "T's own refinement and over b" while the
  occurrence test looks at refinements anywhere in `T`; the implementation
  scopes the binder over all of `T` and `b`, matching the test.
- **Predicates are resolved, not typed.** The mirror
  (`Types.refinement_expression`) resolves value names — bound names to
  idents, free names to paths via `Env.lookup_value` — but carries no
  types except those written in the source (`Rexp_constraint`).
  Constructor and record-label names are checked for existence at
  translation and stored as longidents; resolving them to descriptions
  would drag `constructor_description`s (and their types) into the type
  graph, and no rule consumes them yet. Consequence to revisit in a later
  piece: a module renaming via `Subst` rewrites value paths but not
  constructor names.
- **The predicate sublanguage, concretely.** Idents, constants,
  application, labeled tuples, constructors, record field access,
  `if`/`then`/`else`, single non-recursive `let x = e in e`, `fun x -> e`,
  `match` with variable/wildcard/constant/tuple/constructor/alias
  patterns, and type constraints. Non-total forms (`while`, `for`,
  sequencing, assignment, arrays, `try`, `assert`, `lazy`, `ref`/`!`/`:=`
  as bare identifiers) are rejected with the totality message; total but
  unsupported forms (records, or-patterns, recursive or multiple `let`,
  coercions, objects, …) get a located "not supported" error, so nothing
  reachable from source crashes.
- **Binders under `Tpoly`** (open question in the document): the value
  binder scopes inside the type quantifier — the binder is introduced at
  the arrow, which sits under the `Tpoly` node, and predicates are
  translated in the same pass, so `'a. x:'a t{ p x } -> …` just works.
  `Tpoly` alpha-renaming does not freshen value binders; `Subst` freshens
  all predicate binder stamps (and rewrites references) whenever a type is
  copied, which covers import. `Btype`'s generic copies keep stamps.
- **Printing pipeline.** `Otyp_refine of out_type * Parsetree.expression`:
  the predicate is untyped back to surface syntax (`Vox_rexp.untype`) at
  conversion time and printed by `Pprintast`. Interior types are rendered
  through the type printer and re-parsed with `Parse.core_type`, so paths
  and nested refinements print consistently with the enclosing output.
  The label slot of `Otyp_arrow` is the printed name slot verbatim
  (`Labelled "x"` for a binder, `Labelled "~x"` for an escaped label);
  outcometree consumers that pattern-match labels see strings, which is
  what they print. The alternative — new outcometree label constructors —
  touches every outcometree consumer for the same output.
- **When `~` is printed.** Only when needed: a label prints bare unless
  its name occurs in a refinement predicate in scope (its own binder, an
  outer binder with the same printed name, or a free identifier printed as
  that bare name), in which case re-parsing would turn it into a binder
  and the printer escapes it. This can over-escape in corner cases (an
  occurrence bound by a deeper arrow); the output still re-parses to the
  same type. Positional binders are renamed (`x` → `x1`) when they collide
  with an enclosing binder still in scope.
- **Unification and the binder.** The binder ident is not part of type
  identity. `unify`/`eqtype` pair the two binders on a stack (mirroring
  `univar_pairs`) and compare predicates alpha-equivalently
  (`Vox_rexp.equal`); a one-sided binder is ignored at the arrow and can
  only be followed by a predicate mismatch at the refinements themselves.
  `moregen`, `mcomp` beyond payload compatibility, and `subtype` have no
  refinement rules — signature matching and coercions are later pieces —
  so they fall into their generic mismatch cases.
- **Class types.** Refinements are permitted in every `core_type`,
  including class arrow domains, but class arrows keep today's label
  grammar and never bind: a name used in a refinement there is simply
  unbound (ordinary error). Class types are legacy; extending the binder
  rule to them buys nothing today.
- **Labeled tuples.** Tuple labels (`x:int * y:bool`) are not subject to
  the binder rule; a tuple component's label never binds in refinements.
  The document only gives the rule for arrows.
- **The mixed-arrow warning** is number 230, `vox-mixed-arrow-conventions`,
  and fires once per arrow chain (the maximal spine translated together),
  when the chain contains both a positional binder and a bare-spelled
  label. `~`-spelled and optional labels don't count against mixing: they
  are self-describing.
- **`varify_constructors`** (the `let f : 'a. …` path) does not descend
  into predicates; type variables in predicate interior types are not
  varified. Interior types are otherwise fully live in the graph (levels,
  generalization, occur check) through `Btype.fold_type_expr`.
- **The hole is not a name.** `int{ _ > 0 }` and `n:int{ n > 0 }` are
  different types: `Rexp_hole` and a binder reference are syntactically
  different predicates, and equality is syntactic alpha-equivalence with no
  normalization. The alternatives — normalizing a binder's occurrences in
  its own domain refinement to holes, or a context-dependent equality that
  identifies the hole with the enclosing arrow's binder — either erase the
  written name from the printed form or thread arrow context into
  predicate comparison; both cost more than the distinction is worth while
  refinements are inert. Revisit if it bites once refinements are used.
