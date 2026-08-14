# Vox solver interface

A standalone logic term language, an obligation type, a backend signature, and
two backends: printing and z3. Nothing in this piece knows about refinement
types, so it can be built and tested before the type formers exist.

## Why it stands alone

vox2's solver cannot be separated from the typechecker. `Vox_backend.obligation`
carries `env : Env.t`, and the encoder reaches into it during emission:
`Env.find_type`, `Env.find_value`, `Env.normalize_value_path`, `Env.fold_values`
at `vox_smt.ml:322, 552, 555, 1507, 1519`. `Vox_vc.t` names
`Types.refinement_expression` eight times.

Resolve every symbol during translation and hand the backend a closed
signature. That is the whole trick, and a later piece owns the translation from
refinements into these terms.

## The term language

    module Sort : sig
      type t =
        | Bool
        | Int                        (* mathematical, unbounded: Bigint *)
        | Bitvec of int              (* OCaml int is Bitvec 63 *)
        | Uninterpreted of string    (* abstract type behind a signature *)
        | Datatype of string
    end

    module Term : sig
      type t =
        | Var of string              (* sort comes from the signature *)
        | Const of literal
        | App of Op.t * t list       (* interpreted operators *)
        | Call of string * t list    (* uninterpreted function symbol *)
        | Ite of t * t * t
        | Construct of string * t list
        | Select of string * int * t
        | Test of string * t
    end

Quantifier-free, matching vox2's `reject_quantifiers`. Both `Int` and `Bitvec`
are present because the two integer notions are genuinely different:
`Bigint` is mathematical, OCaml `int` is `Bitvec 63` (`vox_smt.ml:167`).

State the `Bitvec 63` decision in this piece, prominently. It is the reason
`x >= 0 |- x + 1 >= 0` is false and `abs` in the naive form does not verify. A
reader who does not know it will conclude the solver is broken.

Variables carry no sort at the occurrence. The signature declares them, so two
occurrences of one variable cannot disagree, and ill-sortedness is
unrepresentable rather than checked.

## Obligations

    module Obligation : sig
      type hypothesis =
        { id     : int              (* stable; the unsat-core currency *)
        ; term   : Term.t
        ; origin : Origin.t }

      type t =
        { signature  : Signature.t  (* sorts, symbols, datatype declarations *)
        ; hypotheses : hypothesis list
        ; goal       : Term.t
        ; location   : Location.t }
    end

Hypothesis ids are how a backend names hypotheses in an unsat core. This is
cleaner than vox2, which parses selector names back out of the core.

## Outcome

    type verdict =
      | Proved  of { unused_hypotheses : int list option }
      | Refuted of model option
      | Unknown of reason

    type reason = Timeout | Incomplete of string

    type failure =
      | Unavailable of string
      | Error of { cause : string; raw : string }

    type outcome = (verdict, failure) result

Failure is separated from verdict. vox2 puts `Solver_error` and `Unavailable`
beside `Proved` in one type, and `protect_discharge` (`vox_backend.ml:1163`)
swallows every backend exception into `Solver_error` with the real text in a
tail the printer never reads. That is how a genuine oxsmt defect reached a user
as "the solver could not be run".

`unused_hypotheses = None` means the backend makes no claim. It never means
"all hypotheses were used". vox2's conservative reading is right and must
survive: an unsat core need not be minimal, so a fade may be missed but never
invented.

Models are term-valued, not strings. vox2 returns a raw `define-fun` sexp capped
at 4000 characters, and only for z3. Terms can carry constructor trees, which is
what a datatype counterexample needs.

### What Refuted means

This has to be pinned before any backend exists, because the obvious reading is
wrong.

vox2 runs two queries (`vox_smt.ml:3754`). The Prove query asks whether
`hypotheses AND NOT goal` is unsat; if so, `Proved`. If that does not settle it,
a second Disprove query runs, and `Disproved` is reported only when *that*
returns unsat. A bare `sat` on the prove query is `Not_proved`, and the model is
fetched afterwards as decoration.

So `Refuted` means the disprove query succeeded: the goal is false in every
state satisfying the hypotheses. It does not mean "the prove query found a
model". With uninterpreted functions and underspecified selectors, a prove-query
model can satisfy the encoding without corresponding to any reachable program
state, and reporting that as a refutation would be wrong.

A prove-query model is at most payload for `Unknown`.

Collapsing vox2's `Not_proved` and `Unknown` into one constructor is faithful,
because vox2's `Not_proved` detail is literally
`"prove query: <status>; disprove query: <status>"`, which is exactly what
`Unknown of reason` carries.

## Backends

    module type BACKEND = sig
      val name       : string
      val configured : config:Config.t -> (unit, string) result
      val discharge  : config:Config.t -> Obligation.t -> outcome
    end

    val backends : (module BACKEND) list
    val select   : string -> ((module BACKEND), string) result

A static list rather than a mutable registry. The defect in vox2 was leaking
`smt_solver` and `oxsmt_solver` into the generic `discharge`
(`vox_backend.mli:90`), and per-backend config fixes that without needing
dynamic registration. A static list also keeps the `-vox-backend` error message
derivable from the list.

`Config.t` carries a budget from the start. vox2 threads `timeout_seconds`
everywhere and enforces it two ways, an external `timeout` wrapper and SIGALRM
stubs. The enforcement belongs to backend pieces, but the field has to exist
now, and `Unknown Timeout` has to be distinguishable, because a timeout is the
most common non-answer in practice.

There is no `capabilities` record. With two backends the only question is
whether unused hypotheses are available, and `int list option` answers it at the
point of use.

## `none` is not a backend

`-vox-backend none` short-circuits before discharge. It is caller policy.

A backend returning `Unknown` would report every obligation as unproved; one
returning `Unavailable` would report every unit as failing. vox2 agrees:
`grep -c None_backend` in `vox_backend.ml` is zero, and typecheck-only is
`not_discharged_result` on the caller side (`vox_verify.ml:269`).

## The printing backend emits SMT-LIB

Not a bespoke pretty-print format. Printing and z3 share one renderer, so the
expect-test baselines are the bytes z3 receives.

A translation bug then shows up as a baseline diff rather than a mysterious
`unknown`, the renderer becomes the most-tested code in the piece, and there is
no way for what we print to drift from what we send. vox2 had that drift
structurally: `-vox-dump-vc` printed an internal sexp form while
`-vox-dump-vc-json-smt` carried the real queries separately.

Human-readable rendering for diagnostics and the IDE is a different job and
belongs elsewhere.

## Datatypes

Two mechanisms, and abstraction chooses between them. A type whose definition is
visible becomes `declare-datatypes` with constructors, selectors and testers. A
type behind a signature becomes `declare-sort` plus uninterpreted functions.

The same OCaml type is concrete inside its defining module and abstract outside
it. Getting this wrong would let client proofs depend on a representation the
interface does not expose. It is also what makes the `int_set` corpus work: to
the client, `t` is an uninterpreted sort and the laws are all that relate
`member` and `insert`.

Parametric types are monomorphised, one SMT sort per instantiation, matching
vox2 (`declare-sort <name> 0` plus an `instantiate` step). The limits vox2 found
are worth keeping as deliberate rejections rather than discoveries:

    "non-regular recursive datatype %s is not supported"
    "function-valued datatype fields are not supported"

Non-regular recursion would need infinitely many instantiations. Parametric
`declare-datatypes` would avoid the blow-up and z3 supports it, but it does not
rescue non-regular recursion either, so it is an optimisation.

Records and tuples are single-constructor datatypes. Mutually recursive types
must be grouped into one `declare-datatypes`, so the translation needs SCCs over
the type graph. Mutable fields need no rule here: logicality already stops a
logical value's mutable state being read, so the projection never reaches the
encoder.

One modelling wrinkle to document. An SMT selector applied to the wrong
constructor is underspecified, so `head Nil` is an arbitrary but consistent
value rather than an error.

## The z3 backend

Polarity: assert the hypotheses and the negated goal, ask for satisfiability.
`unsat` is `Proved`, and a refutation needs the second query described above.
Write this down; a sign error here silently inverts every result.

One process per obligation to start. A persistent `z3 -in` session with push and
pop is faster and is the obvious later optimisation, but it adds state and
failure modes this piece does not need.

Timeouts both ways: `(set-option :timeout ...)` in the script and a wall-clock
kill outside it, since a wedged process ignores the former.

Unused hypotheses come from `(get-unsat-core)` with `:named` assertions carrying
the hypothesis ids.

### Availability is checked once

Check the configured command at selection, not per obligation, and fail with one
message: no solver configured, pass `-vox-backend none` to typecheck only.

This is the lesson worth carrying over most. vox2 defaults to z3; z3 was not
installed on this machine; the result was 51 of 52 refinement tests failing with
`Refinement verification failed (unavailable)` on every obligation. That reads
as a broken feature rather than a missing binary. It is also why `Unavailable`
belongs in `failure` rather than beside `Proved`.

z3 4.8.5 is at `/j/office/app/z3/prod/4.8.5/install/bin/z3` and is the version
the vox2 corpus was tuned against.

## Tests

Obligations are built by hand; nothing produces them yet.

Renderer, which is the bulk:

- each sort and term form rendered to SMT-LIB, checked against a baseline
- a datatype group with mutual recursion in one `declare-datatypes`
- an abstract type as `declare-sort` plus uninterpreted functions
- rejections: non-regular recursive datatype, function-valued field
- a well-formedness failure for an undeclared variable or symbol

z3, gated on the binary being present:

- `Proved` for a valid goal, `Refuted` for one false under every model of the
  hypotheses, `Unknown Timeout` under a tiny budget
- a prove-query `sat` that is not a refutation reports `Unknown`, not `Refuted`.
  This is the discriminating test for the ruling above and should fail if
  someone rewires the protocol to one query.
- `unused_hypotheses` populated from an unsat core, with an unused hypothesis
  present
- a term-valued model for a datatype counterexample
- selection with no configured command fails once, at selection, and the message
  names `-vox-backend none`

Driver:

- `-vox-backend none` skips discharge and reports obligations as not discharged
- `-vox-backend printing` emits SMT-LIB and discharges nothing

## Deferred

Caching. vox2's `Cached` decorator is the right shape, but it needs
`cache_key : config:Config.t -> Obligation.t -> string option` on `BACKEND`,
with `None` bypassing the cache, and a key mixing compiler identity, solver
fingerprint and payload (`vox_backend.ml:890`). Add the field when the cache
lands rather than carrying an unused one now.

Cross-checking. It layers above `BACKEND` and needs a second real backend to
compare against. Recording it here because it was vox2's main validation tool
for oxsmt, and it is why vox2's `result` carries a list of `backend_result`. The
`Not_proved` to `Unknown` normalisation at `vox_backend.ml:1189` becomes a no-op
under these types.

Lean and in-process oxsmt backends. A persistent solver session. Translation
from refinement expressions into these terms. Recursive functions over
datatypes, which are uninterpreted symbols plus definitional-equation axioms.
Conversions between `Int` and `Bitvec` (vox2 needs `bv2int`/`int2bv` for
`Bigint.of_int`; the operators are added when the translation fixes their
semantics). A model payload on `Unknown` for near-counterexamples.

## Decisions taken

- `Refuted` requires the disprove query, not a prove-query model.
- `none` is driver policy, not a backend.
- Availability is checked once at selection.
- The printing backend emits SMT-LIB, shared with z3.
- Variables are declared in the signature, not sorted at each occurrence.
- Static backend list rather than a mutable registry, since a global mutable
  table is the pattern this rebuild is trying to leave behind.

## Decisions taken during implementation

Two protocol refinements from the review loop, stated here because they
sharpen rulings made above:

- **`Refuted` additionally requires the prove query to answer `sat`.** The
  protocol above runs the disprove query whenever the prove query "does not
  settle it"; but after a prove-query `unknown`, hypothesis satisfiability is
  unestablished, and `hyps AND goal` unsat can simply mean the hypotheses are
  contradictory — whose correct verdict is `Proved`, not `Refuted`. So an
  `unknown` prove query is reported as `Unknown` without running the disprove
  query. (vox2 ran it anyway; this is a deliberate hardening.)

- **"A prove-query model is at most payload for `Unknown`"** describes what a
  prove-query model may ever be used for; under the types above `Unknown`
  carries no model, so today it is simply dropped. Carrying it (the common
  unproved case has a useful near-counterexample sitting in the prove output)
  is deferred to the diagnostics work, noted below.

Recorded per AGENTS.md: points the spec above left open, with the route taken.

- **Module layout.** `typing/vox_logic` (sorts, ops, literals, terms, origins,
  datatypes, signatures, obligations), `typing/vox_smtlib` (the one renderer),
  `typing/vox_backend` (config, outcome, `BACKEND`, printing and z3, selection
  and driver policy). Following vox2's precedent that vox modules live in
  `typing/`, where later pieces will use them.

- **Monomorphisation lives here, as `Signature.instantiate`.** The signature a
  backend sees carries only ground datatypes (fields are `Sort.t`, which has no
  arrow and no type application, so neither rejection is even representable
  there). The parametric declaration language `Datatype.ty` exists as input to
  `instantiate`, which owns both deliberate rejections. The alternative — a
  parametric signature with instantiation in the renderer — would make every
  backend deal with instances. Non-regularity is detected exactly as in vox2:
  a recursive use at different arguments while the datatype's own definition
  is being expanded. The test is conservative — it also rejects some patterns
  whose reachable instance set is finite (a recursive use at constant
  arguments, say), not only the genuinely infinite ones; kept as-is for vox2
  parity, and the interface says so.

- **Instance naming.** `t` at `Int` is `t<Int>`; constructors and selectors get
  the same suffix (`Cons<Int>`, `head<Int>`); nullary instantiations keep their
  names. `<` and `>` are legal in SMT-LIB simple symbols (verified against
  z3 4.8.5); anything else is `|quoted|` by the renderer (multi-argument
  instances contain `,`, so they render quoted), and a symbol containing `|`
  or `\` is an error. `instantiate` rejects two instantiations that mangle to
  one name (`Sort.key` is not injective — an uninterpreted sort literally
  named `Int` would otherwise alias the builtin) and requires declaration,
  constructor and selector names to be globally unique before suffixing.

- **Builtin names are rejected, not quoted** (review-loop finding). Quoting is
  purely lexical — verified that in z3 4.8.5 a declared `|not|` shadows the
  boolean operator and `(|+| n n)` is `(+ n n)` — so a signature symbol
  spelling an interpreted operator, `true`/`false`/`ite`, a builtin sort
  name, or a hypothesis label `h<id>` is an ill-formed obligation. The
  translation piece owns name generation and can avoid these spellings.

- **One-shot scripts, directives included.** A `Prove` script always ends with
  `(get-unsat-core)`, `(get-model)`, `(get-info :reason-unknown)`; both query
  scripts ask for the reason. Verified against z3 4.8.5: an inapplicable
  directive prints an `(error ...)` line, execution continues, and the exit
  code becomes nonzero — so the backend ignores exit codes whenever a status
  line is present, and readers skip error entries. The alternative (re-running
  with extra directives once the status is known) costs a second process on
  every proof.

  Position matters, though (review-loop finding): an `(error ...)` printed
  *before* the status line means z3 rejected part of the script and answered
  a different question — verified that a dropped ill-sorted hypothesis turns
  into a spurious "refuted", and a dropped colliding `:named` assertion can
  invert a verdict. Pre-status errors are therefore a backend failure
  (`Error { cause; raw }`), never a verdict; only post-status errors are the
  ignorable inapplicable directives. This is what turns every future encoding
  defect into a loud failure rather than a silent wrong answer, which matters
  doubly because the renderer deliberately does not sort-check.

- **Timeout classification.** z3-side `(set-option :timeout)` is the primary
  budget; `(get-info :reason-unknown)` answering `timeout`/`canceled` maps to
  `Unknown Timeout` (both shapes observed from 4.8.5, engine-dependent). The
  budget is per query, so an obligation may take two budgets of wall clock. The
  wall-clock kill is a `timeout(1)` wrapper with one second of grace — the
  compiler cannot depend on the unix library, and vox2's alternative was C
  stubs, which this piece does not need. Exit 124 also maps to
  `Unknown Timeout`. If either query times out, the obligation is
  `Unknown Timeout`.

- **The printing backend emits the `Prove` query only.** The `Disprove` script
  differs in the polarity of one assert and the absence of core/model
  directives; printing both would double every baseline for one line of
  signal. The disprove polarity is pinned by its own renderer baseline and by
  the z3 protocol test.

- **`-vox-backend` policy ships as `Vox_backend.plan`; the flag itself is
  deferred.** Nothing produces obligations yet, so a command-line flag would
  be dead code with no observable behaviour; the piece that produces
  obligations wires `plan` to `-vox-backend`. `plan` implements exactly the
  spec's driver rules: `"none"` short-circuits before selection, anything else
  is selected and `configured` is checked once, with the failure message
  naming `-vox-backend none`.

- **Well-formedness is declaredness, arity and literal shape, not sort
  inference.** Undeclared variables/functions/constructors/sorts, arity and
  field-index mismatches, malformed literals, duplicate declarations and
  duplicate hypothesis ids are renderer errors. Re-implementing full sort
  checking would duplicate the solver's own checker at some length; the
  variables' sorts already cannot disagree by construction.

- **Models are read back best-effort.** Values of the obligation's variables
  parse into terms (constructor trees included); z3's opaque universe
  elements for uninterpreted sorts (`t!val!0`) read back as `Term.Var`; a
  value that does not parse drops that variable rather than failing the
  verdict.

- **`Origin.t`** is `{ label; location }` — enough for unused-hypothesis
  diagnostics; the translation piece will decide what labels mean.

- **`Config.t`** is `{ timeout_seconds : float option; z3_command : string
  option }` with default ten seconds and no solver. Temporary files for
  solver I/O go through `Filename.temp_file`, which honours `TMPDIR`.
