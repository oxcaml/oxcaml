# Type formers: final report

Status: implemented, review-looped twice, green on the full test suite.
Branch `jujacobs/vox/type-formers`, three commits, 69 files, +2799/-168.

## What landed

- `24bc7ffacc` — the feature. `Ptyp_refine` and `arrow_arg_name`
  (`Pan_nolabel`/`Pan_name`/`Pan_tilde`/`Pan_optional`) in the parsetree;
  `Trefine` plus the resolved total-subset predicate mirror in `Types`;
  the binder ident in `arrow_desc`; the occurrence test in
  `parsing/vox_binding.ml`; predicate operations in `typing/vox_rexp.ml`;
  rigid `unify`/`eqtype`/`moregen` with alpha-equivalence via a
  `univar_pairs`-style binder stack; `Subst` freshening; printing via
  `Otyp_refine` + `Pprintast`, with `~`-escape decided on the converted
  output; warning 230 for mixed arrows; expect tests, a `.cmi` round-trip
  test, and an empty-`{}` RED fixture.
- `41d8b7a761` — first review round fixes. First-class value-path
  substitution in `Subst`/`Env` (same-signature references survive `.cmi`
  import); predicates print from substituted paths; partial application
  keeps the binder; string constants compare without their locations;
  predicate-local binders scope into interior types; quotations get a
  located error; `nondep_type` refuses to erase predicate-mentioned
  modules; typed outcometree constructors (`Binder`/`Tilde_labelled`);
  `Ttyp_refine` carries the source predicate; printer binder renaming
  deleted.
- `449fa54bf6` — verification round. Constructor paths are resolved,
  substituted and printed (a functor application prints `AC.C`, not a
  dangling `X.C`), and consuming a dependent parameter — applying it, or
  defining a `fun` against a dependent annotation — is rejected with a
  located error instead of letting the type escape its binder.

## Review loop

Two codex and two claude reviewers, each in its own worktree under
`type-formers/review/<name>/` (codex reports at
`type-formers/review/*/report.md`), followed by a codex verification pass
over the fix round, which reproduced and confirmed every fix and found no
regression in the new `Subst.values` map, the outcometree constructors,
the omitted-binder threading, or the renaming removal.
`make dev-test-all`: 2445 passed / 0 failed, after both rounds.

## Decisions to check (AGENTS.md: notify on ambiguous-spec choices)

All are recorded with rationale in `design-docs/type-formers.md`; these
are the ones worth a human look:

1. **The hole is not a name**: `int{ _ > 0 }` ≠ `n:int{ n > 0 }` —
   strict syntactic alpha-equivalence, no hole/name normalization.
2. **`Trefine` carries no name**; the value's name always lives on the
   arrow (`arrow_desc` binder; `~x:` is a domain-only binder). The
   spec's "the refined type carries the value's name" is realized this
   way to make "binds once" structural.
3. **Dependent-arrow consumption is rejected** until the elimination
   piece: the verification round argued the binder escape violated the
   printing contract, and the rejection replaced a pinned-escape
   behaviour.
4. Smaller: `~x:` scopes over the whole argument type; class arrows and
   tuple labels never bind; record labels in predicates stay longidents
   (the compiler has no label-path representation); `ref`/`!`/`:=` are
   rejected by bare name only.

## Caveats and follow-ups

- Dev-loop friction worth fixing separately: `dev-runtime` does not
  refresh `runtime_stdlib_install` when the compiler itself changes, so
  a `Types` layout change (no `.cmi` magic bump yet, per spec) silently
  corrupts imports — symptom is segfaults, cure is `make runtime-stdlib`.
- `ocamlc.byte`-style tests (including `vox/roundtrip` and
  `vox/empty-predicate` here) cannot run under the dev harness (runtime
  magic mismatch, preexisting); they were verified manually with the dev
  compiler and will run in full CI.
- During the first round the headless claude reviewers ran read-only
  (the session's permission mode refused `claude -p` with shell access),
  so their findings were file:line-grounded while codex did the executed
  repros; a scoped permission rule (`Bash(claude -p --allowedTools:*)`)
  has since been added for future loops.
- Deferred by the spec and untouched: introduction/elimination rules,
  signature matching beyond identity, subtyping/coercion, VC generation,
  solvers, `.cmi` magic bump, extension gating, vendored merlin.
