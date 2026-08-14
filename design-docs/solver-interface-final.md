# Vox solver interface — final report

Status of the piece after implementation and the review loop, on branch
`jujacobs/vox/solver-interface`:

    2cd747db89 GREEN: fix the defects the review loop found
    85f47e8512 RED: record review-loop failure cases in the vox-solver tests
    ccf4ed3ce7 Add the vox solver interface
    ef98a4a693 Add design doc for the solver-interface piece

## What was built

Per `design-docs/solver-interface.md`:

- `typing/vox_logic` — sorts (with `Bitvec 63` for OCaml int), interpreted
  operators, literals, terms, parametric datatypes plus the monomorphising
  `Signature.instantiate` with vox2's two deliberate rejections, closed
  signatures, obligations.
- `typing/vox_smtlib` — the single SMT-LIB renderer both backends share, so
  the expect-test baselines are the bytes z3 receives.
- `typing/vox_backend` — the verdict/failure split, `BACKEND`, printing and
  z3 backends, the static backend list, and `plan`, where `none` is caller
  policy rather than a backend.
- `testsuite/tests/vox-solver/` — `renderer.ml` and `driver.ml` expect tests
  and a z3-gated protocol test (`has_z3.sh` skips via ocamltest's exit-125
  convention when no solver is installed; `VOX_TEST_Z3` overrides).

Every z3 protocol fact was probed against the pinned 4.8.5 binary before
being relied on (one-shot directive behaviour, `(:reason-unknown ...)`
shapes, 2.6 datatype syntax, `<`/`>` in simple symbols, model shapes).

## Review loop

Three lanes off commit `ccf4ed3ce7`, each in its own
`solver-interface/review/<name>` worktree: **claude-design**,
**claude-correctness**, **codex** (`gpt-5.6-sol`). All three delivered
`report.md` with VERIFIED reproductions. Fixes landed as a RED→GREEN pair so
the baseline diff of the GREEN commit shows each behaviour flip.

### Verified defects, fixed

- **Verdict inversion** (all three lanes; worst form found by codex). A
  variable named `h0` collides with the renderer's `:named h0` label; z3
  drops the colliding assertion with a pre-status `(error ...)` line and a
  vacuously provable obligation read *"refuted"*. Fixed twice over:
  hypothesis labels join the checked symbol namespace, and any `(error ...)`
  before the status line is now `Error { cause; raw }`, never a verdict.
  The latter converts every future encoding defect into a loud failure —
  which matters doubly because the renderer deliberately does not
  sort-check. Pinned by the z3 test flipping
  `n = b (ill-sorted) |- ...: refuted, e.g. n = -1` to
  `error: the solver rejected the query`.
- **Builtin shadowing** (correctness lane). A symbol named `not` or `+`
  silently shadows the interpreted operator, giving spurious `Proved`. The
  reviewer proposed `|quoting|`; that claim was **falsified by a probe**
  (z3 4.8.5 treats `|not|` as `not` and `(|+| n n)` as `(+ n n)`), so the
  fix rejects renderer-emittable builtin names (operator spellings,
  `true`/`false`/`ite`, builtin sort names) as ill-formed instead.
- Nullary `Call` rendered invalid `(f)` (codex): `f |- f` flipped from
  `unknown` to `proved`.
- Negative `Select` index escaped `render` as `Invalid_argument`
  (codex, correctness): now the same ill-formed error as an out-of-range
  index.
- Instance-name aliasing (design, correctness): `Sort.key` is not
  injective, so `box` at builtin `Int` and at an abstract sort named `Int`
  silently merged into one instance — in principle unsound (two types
  identified). `instantiate` now rejects two distinct instantiations that
  mangle to one name.
- **`Refuted` now requires the prove query to answer `sat`** (correctness
  F6). After a prove-query `unknown`, disprove-unsat can simply mean the
  hypotheses are contradictory, whose correct verdict is `Proved`; the
  disprove query is no longer run in that case. Deliberate hardening over
  vox2's protocol, recorded in the design doc.
- Smaller: negative hypothesis ids rejected; leading-zero numerals rejected
  (z3 accepts them, SMT-LIB does not); empty model atoms no longer read
  back as empty integer literals.

### Coverage and test hygiene

Added: empty unsat core (all hypotheses unused), opaque-universe model
elements (`t!val!0` reads back as `Term.Var`), wall-clock kill of a wedged
solver (exit 124), an unrunnable configured command, width-1/63/64 bitvec
literals, duplicate declarations, field-index bounds. Removed ~120 lines of
no-signal expect echo (helper definitions now live in their use sites, so
baselines show only rendered SMT-LIB).

### Suggestions declined (spec-pinned shapes; decision points)

All coherent, all contradict shapes the ratified spec pins; recorded here
rather than adopted. Reports: `solver-interface/review/*/report.md`.

- Backends as record values with config bound at selection, replacing
  `BACKEND` / `backends` / `select` (design D5). Would simplify a future
  cache/session/cross-check; the spec pins the module shape.
- Printing as a dump-mode (`-vox-dump-vc` style) orthogonal to backend
  choice, not a backend (D6). The spec pins the printing backend.
- Generic well-formedness moving to `Vox_logic.Obligation.check`, leaving
  the renderer purely SMT-LIB spelling (D7). Revisit when a second real
  backend wants the same checks.
- Splitting the z3 backend into `typing/vox_z3.ml` (D8). The file is ~500
  lines; split when a second solver backend lands.
- `Unknown` carrying the prove-query model as a near-counterexample (D9).
  Deferred; noted in the design doc alongside `Int↔Bitvec` conversion
  operators (D10), which the translation piece will need for
  `Bigint.of_int`.
- `Select` by selector name; a `Ground of Sort.t` field-type language;
  dropping `Arrow` in favour of a translation-side rejection (D11);
  `instantiate` returning the uninterpreted sorts it grounded (D12).

The two spec deviations that *were* taken — `Refuted` requires `sat`, and
the non-regularity test documented as conservative (it rejects some finite
instantiation patterns, exactly as vox2 does) — are flagged in the design
doc's decisions section.

## Friction found on the way

- `dune rpc build` against an already-idle watcher wedges indefinitely;
  `make dev-stop` before scripted `dev-test`/`dev-promote` runs is a
  reliable workaround. Candidate for a `tools/dev-watcher.py` fix.
- New compilerlibs modules need `make install_for_test` (slow, one-time per
  library change) before `make dev-test DIR=...` can link them; the failure
  mode is `Unbound module Vox_logic`. Worth a hint in the dev loop.
- The expect harness captures formatters, not file descriptors — hence the
  printing backend emits via `Format.std_formatter`.
- ocamltest's `script` action defines `stdout`/`stderr` as a side effect,
  which silently disables output redirection for a later `run` action; the
  z3 test header re-points them (commented in the test).
- System autoconf is 2.69; `configure` must be copied from an existing
  worktree (it is generated by 2.71).
