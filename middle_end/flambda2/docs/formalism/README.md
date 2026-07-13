# The Flambda 2 formalism

This directory describes Flambda 2 — the term language and the Simplify pass —
as a formal system: a syntax, an operational semantics, an abstract domain, and
a set of transformation rules, each traceable to the code that implements it.

It has two audiences:

- **Humans**, reading front to back (or dipping into one chapter) to understand
  how Flambda 2 works, evaluate design decisions, or hunt for problems.
- **Agents/tools**, using the machine-readable rule blocks and
  [`rule-index.md`](rule-index.md) to answer questions about Flambda 2,
  synthesize tests that exercise particular rules, and keep this formalism in
  sync with the code.

The formalism is *descriptive of the code as it exists*, not aspirational.
Where the code's behaviour and this document disagree, one of them has a bug;
the sync protocol below says what to do.

## Chapters

| File | Contents |
|---|---|
| [`01-overview.md`](01-overview.md) | Pipeline context (CPS conversion, Simplify, Reaper, to_cmm) and the **scope ledger**: what is and is not formalized |
| [`02-syntax.md`](02-syntax.md) | Abstract syntax: expressions, `named`, binders, `Simple`, name modes, trap actions; correspondence with the fexpr concrete syntax |
| [`03-kinds.md`](03-kinds.md) | Kinds, subkinds, arities and unarization; kinding and well-formedness judgments |
| [`04-opsem.md`](04-opsem.md) | The abstract machine and small-step rules for every expression form, including traps, call kinds, regions, and `Invalid` |
| [`05-primitives-scalar.md`](05-primitives-scalar.md) | Denotations of arithmetic, comparison and conversion primitives, per numeric kind |
| [`06-primitives-memory.md`](06-primitives-memory.md) | Denotations of block/closure/array/string primitives; effects and coeffects; undefined-behaviour cases |
| [`07-types-domain.md`](07-types-domain.md) | The abstract domain: type grammar, typing environments (aliases, levels, existentials), concretization |
| [`08-meet-join.md`](08-meet-join.md) | Meet (with environment extensions), join and n-way join, provers, reification |
| [`09-simplify-structure.md`](09-simplify-structure.md) | Simplify's downwards/upwards architecture, accumulators, flow analysis, lifting, join points |
| [`10-simplify-rewrites.md`](10-simplify-rewrites.md) | The rewrite rules: constant folding, CSE, switch simplification, continuation inlining/specialization, dead code |
| [`11-inlining.md`](11-inlining.md) | Function inlining as a nondeterministic rule with a decision oracle; the actual cost model, descriptively |
| [`12-unboxing.md`](12-unboxing.md) | Unboxing of continuation parameters, function parameters and results |
| [`13-soundness.md`](13-soundness.md) | The (claimed, unproved) soundness statement tying Simplify to the operational semantics; global invariants; known discrepancies |
| [`14-validation/`](14-validation/) | Case studies: for each validated test, the predicted outcome (with rules cited), the actual outcome, and a verdict |
| [`rule-index.md`](rule-index.md) | Machine-greppable table of every rule: ID → chapter → code anchors → verification status |

## Status & validation

As of 2026-07-13 the formalism has **318 rules** across chapters 02-13:
**221 normative** (the code must satisfy them), **64 descriptive** (current
algorithms/heuristics that may change), and **33 conjectured** (believed but not
yet checked against the code). Counts and per-chapter breakdown are regenerated
in [`rule-index.md`](rule-index.md).

Chapters 01-12 were adversarially verified against the code, and the whole
system was validated against **33 case studies** in
[`14-validation/`](14-validation/) using a prediction-first protocol (predict the
Simplify output and cite rules *before* reading the actual output): **30 MATCH,
1 PARTIAL, 2 MISMATCH**. One mismatch (immutable array loads *are* CSE-eligible)
was resolved by fixing the formalism; the other
([`float32_double_round`](14-validation/float32_double_round.md)) witnesses an
open **compiler soundness bug** — int→float32 constant folding double-rounds
where the backend single-rounds. Chapter
[`13-soundness.md`](13-soundness.md) states the (claimed, empirically validated,
unproved) soundness property and records the known discrepancies between this
document, its companion prose, and the code.

Companion prose (not part of the formalism, but authoritative background) lives
in the parent directory: [`../types.md`](../types.md),
[`../encyclopaedia.md`](../encyclopaedia.md),
[`../backend_exceptions.md`](../backend_exceptions.md),
[`../inlining.md`](../inlining.md), [`../unboxed_params.md`](../unboxed_params.md).

## Notation

### Metavariables

| Metavariable | Ranges over | Code type |
|---|---|---|
| `x`, `y`, `z` | variables | `Variable.t` |
| `sym` | symbols (statically-allocated names) | `Symbol.t` |
| `c` | constants | `Reg_width_const.t` |
| `s` | simples: `x` \| `sym` \| `c`, optionally under a coercion `s @ co` | `Simple.t` |
| `k` | continuations | `Continuation.t` |
| `f` | function slots | `Function_slot.t` |
| `w` | value slots | `Value_slot.t` |
| `cid` | code IDs | `Code_id.t` |
| `e` | expressions | `Flambda.Expr.t` |
| `n` | `named` (defining expressions) | `Flambda.Named.t` |
| `p` | primitives | `Flambda_primitive.t` |
| `κ` | kinds | `Flambda_kind.t` |
| `κ̂` | kinds with subkind | `Flambda_kind.With_subkind.t` |
| `co` | coercions | `Coercion.t` |
| `v` | runtime values (defined in [§04](04-opsem.md)) | — |
| `T` | Flambda types (abstract-domain elements) | `Flambda2_types.t` |
| `E` | typing environments | `Flambda2_types.Typing_env.t` |
| `ε` | typing-environment extensions | `Typing_env_extension.t` |
| `Γ` | (syntactic) kinding contexts | — |

Sequences are written with an overbar (`x̄`, `s̄`) or subscripts (`s₁ … sₙ`).

### Judgment forms

All judgment forms are declared here; chapters define their rules.

| Judgment | Meaning | Chapter |
|---|---|---|
| `Γ ⊢ s : κ` | simple `s` has kind `κ` | 03 |
| `Γ ⊢ e ok` | expression `e` is well-formed | 03 |
| `⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e′, ρ′, K′, H′, T′, R′⟩` | small-step machine transition | 04 |
| `⟦p⟧(v̄; H) = (v, H′)` or `= undef` | primitive denotation (may allocate/mutate; `undef` = undefined behaviour) | 05, 06 |
| `⟦p⟧(v̄; H, R) = (v, H′, R′)` | region-augmented primitive denotation (region primitives `Begin_region`/`End_region`; also threads the region stack `R`) | 06 |
| `γ_E(T) ⊆ Val` | concretization of a Flambda type | 07 |
| `E ⊢ T₁ ⊓ T₂ = T ▷ ε` or `= ⊥` | meet, yielding an environment extension | 08 |
| `E ⊢ T₁ ⊔ ⋯ ⊔ Tₙ = T` | (n-way) join | 08 |
| `E ⊢ prove_X(s) ⇒ r` | prover query | 08 |
| `E ⊢ T ⇓ H` | expand_head: type `T` expands to head `H` (here `H` is a type *head*, not the heap) | 08 |
| `E ⊢ reify(T) ⇒ r` | reify a type back to a term / static constant (result `r`) | 08 |
| `⟦p⟧♯(T̄) = T ▷ ε` | abstract transfer function for a primitive | 10 |
| `E ⊢ e ⇝ e′` | Simplify rewrites `e` to `e′` (side conditions as prover queries / oracles) | 09–12 |
| `Inline?(apply, code_metadata, env) ∈ {inline, keep}` | inlining decision oracle | 11 |
| `Unbox?(param, uses) ∈ {unbox to shape U, keep}` | unboxing decision oracle | 12 |

The machine configuration `⟨e, ρ, K, H, T, R⟩` has components: `ρ` the value
environment (variables and symbols → values), `K` the continuation environment,
`H` the heap, `T` the trap stack, `R` the region stack. [§04](04-opsem.md) owns the
precise definitions; other chapters must not redefine them.

### Grammar

Grammars are given in BNF inside code blocks, using the constructor names from
the code wherever they exist, e.g.:

```
expr ::= Let (pat = n) e
       | Let_cont …
```

### Runtime values and heap objects

The operational semantics ([§04](04-opsem.md)) and the primitive denotations
([§05](05-primitives-scalar.md), [§06](06-primitives-memory.md)) share one
runtime value grammar, spelled with these canonical constructor names (owned by
[§04](04-opsem.md)):

```
v ::= tagged_imm n        -- kind Value: an OCaml "int"
    | naked_imm n         -- kind Naked_immediate (tags, switch scrutinees)
    | naked_int8 n | naked_int16 n | naked_int32 n | naked_int64 n | naked_nativeint n
    | naked_float f | naked_float32 f | naked_vec128 … | …
    | ptr a               -- kind Value: pointer to the heap object at address a
    | clos ℓ f            -- kind Value: pointer to function slot f of a closure block
    | null                -- kind Value: the null pointer
    | region ι            -- kind Region: a region handle
    | rec_info            -- inert value standing for a Rec_info_expr.t

a ::= ℓ | sym             -- address: heap location or symbol
```

Heap objects (`H : (address ⊎ Code_id) ⇀ o`; [§04](04-opsem.md) owns `H`,
[§06](06-primitives-memory.md) refines the object taxonomy):

```
o ::= Block(t, μ, v̄) | FloatBlock(μ, f̄) | MixedBlock(t, μ, σ, v̄)
    | Array(ak, μ, v̄) | Bytes(μ, b̄) | Bigstring(b̄)
    | Closures(funs, env) | Boxed(κ, c) | Lazy(t, v) | Code(cid ↦ code)
```

`μ` ranges over `Mutability.t`. See [§04](04-opsem.md) §1 for the machine state
and [§06](06-primitives-memory.md) for the object details and the primitive
denotations that build/read/mutate these objects.

Every primitive application is classified by a **quadruple** `(Effects,
Coeffects, Placement, Validity)` (`Flambda_primitive.effects_and_coeffects`); the
four axes and their motion constraints are defined in
[§06](06-primitives-memory.md). Chapter [§05](05-primitives-scalar.md) gives the
same quadruples for the scalar primitives (its "effects/coeffects" tables are the
first two axes of this quadruple).

## Rule blocks

Every formal rule appears in a fenced code block tagged `rule`, in exactly this
shape (order of header lines is fixed; `VERIFIED` and `NOTES` optional):

````
```rule
RULE OS.ApplyCont.TrapPush
STATUS normative
CODE middle_end/flambda2/terms/trap_action.mli#t
CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#compile_staticfail
VERIFIED 14-validation/traps.md
---
<premises and side conditions, one per line>
--------------------------------------------------
<conclusion: a transition, equation, or rewrite>
NOTES: <optional clarifying prose>
```
````

- **`RULE`** — the rule's stable ID (see namespaces below). IDs are never
  renumbered or reused; if a rule is deleted, its ID retires.
- **`STATUS`** — one of:
  - `normative` — a semantic fact the code must satisfy; a divergence is a bug
    in the code or in this document.
  - `descriptive` — documents the current algorithm/heuristic (e.g. a cost
    model); the code may legitimately change, invalidating the rule.
  - `conjectured` — believed but not yet verified against the code. Writers
    must use this rather than guess; verification promotes or deletes it.
- **`CODE`** — one or more anchors of the form `path#function` (or
  `path#Module.function`), relative to the repository root. Function names, not
  line numbers (line numbers rot). Every rule has at least one anchor.
- **`VERIFIED`** — path of the validation case study exercising this rule,
  added by the validation phase.

Prose *around* rule blocks explains intent and design rationale; prose never
contradicts a rule block. A claim important enough to be checked should be a
rule block, not prose.

### Rule ID namespaces

| Prefix | Meaning | Chapter |
|---|---|---|
| `WF.*` | kinding and well-formedness | 03 |
| `OS.*` | operational semantics (e.g. `OS.Let.Singleton`, `OS.ApplyCont.TrapPush`) | 04 |
| `P.<Arity>.*` | primitive denotations (e.g. `P.Binary.IntArith.Add`, `P.Variadic.MakeBlock`) | 05, 06 |
| `T.*` | abstract domain (`T.Gamma.*`, `T.Env.*`) | 07 |
| `T.Meet.*`, `T.Join.*`, `T.Prove.*`, `T.Reify.*` | meet/join/provers/reification | 08 |
| `S.Struct.*` | Simplify architecture invariants | 09 |
| `S.Rewrite.*` | local rewrites (e.g. `S.Rewrite.Switch.KnownScrutinee`) | 10 |
| `S.Inline.*` | inlining | 11 |
| `S.Unbox.*` | unboxing | 12 |
| `INV.*` | global invariants | 13 |

## Sync protocol (for agents)

When code under `middle_end/flambda2/` changes:

1. Grep `rule-index.md` (or all chapters) for `CODE` anchors mentioning the
   changed files.
2. For each affected rule, re-read the anchored code and check the rule still
   holds. Update the rule (and its `STATUS`) if not; a `normative` rule that no
   longer matches the code means either the change or the rule is wrong —
   surface it, don't silently rewrite.
3. Re-run any validation case studies listed under `VERIFIED` for affected
   rules; update `14-validation/` and the index.
4. Regenerate `rule-index.md` (and refresh the counts in this README's "Status
   & validation" section) by building the Dune alias, from the repository
   root:

   ```
   dune build --root=. --workspace=duneconf/main.ws \
     @middle_end/flambda2/docs/formalism/regen-rule-index --auto-promote
   ```

   The rule (see [`dune`](dune)) runs `tools/regen_rule_index.py`, which
   rescans the ```` ```rule ```` blocks and reruns the consistency checks; it
   fails (recording the failures in the index) on duplicate IDs, rules with
   no anchor, or unresolvable anchors. `--auto-promote` copies the
   regenerated files back into the source tree. The alias is not part of the
   default build. Never hand-edit `rule-index.md`.

When answering questions or synthesizing tests: find candidate rules in
`rule-index.md`, read the owning chapter section, and trust `normative` rules;
treat `descriptive` rules as current-behaviour documentation and `conjectured`
rules as leads to verify against the code.

## Validation

The formalism is validated by prediction: given a test input, predict the
Simplify output (citing rule IDs) *before* looking at the actual output, then
compare. Case studies live in [`14-validation/`](14-validation/). Useful
tooling:

- `testsuite/tests/flambda2/*.ml` with `.raw.reference` / `.simplify.reference`
  fexpr dumps; run one with `make -s test-one TEST=flambda2/<name>.ml`.
- `fexprc` (`middle_end/flambda2/tests/tools/`; build with `make -s
  test-tools`): parses a textual `.fl` program and runs the real Simplify
  pipeline on it.
- Compiler dump flags: `-drawfexpr[-to <file>]` (IR after CPS conversion),
  `-dfexpr[-to <file>]` (final IR), `-dfexpr-after <pass>`.
