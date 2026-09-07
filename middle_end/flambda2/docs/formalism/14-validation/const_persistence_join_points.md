# const-persistence: constant knowledge survives merges only when join_points is on

MATCH (with a corrected precondition). Exercises `T.Join.ConstAgreement` and
`T.Env.ConstCanonicalPersists` ([`../08-meet-join.md`](../08-meet-join.md),
[`../07-types-domain.md`](../07-types-domain.md)) and the default-flags gate they
share with `S.Struct.JoinParams.AnalysisExtraParams`
([`../09-simplify-structure.md`](../09-simplify-structure.md)). Driven by the
prediction-first witnesses added to `middle_end/flambda2/tests/meet_test.ml`, not by
a `formalism/` fexpr example (the phenomenon is a typing-env fact, exercised through
the meet/join algebra rather than a `-dfexpr` dump).

## The claim under test

Once a name is provably equal to a constant `c`, that equality (i) survives every
straight-line refinement in the same descent except a move to `⊥`
(`T.Env.ConstCanonicalPersists`), and (ii) survives a control-flow merge whose live
edges all agree on `c` (`T.Join.ConstAgreement`) — but (ii) only when
`Flambda_features.join_points()` is true, because at default flags
`compute_handler_env` SKIPS the join for a continuation with ≥2 uses and gives its
params subkind-only types, LOSING the constant.

## Witnesses (verified, `meet_test.ml`, checked-in `meet_test.expected`)

- **Persistence (claim 2).** Add `x = 3` then `x = 4` (distinct constants of the
  same kind) ⟹ `x : (Val ⊥)`, `is_bottom: true` (the class cannot hold two
  constants; `add_alias` bottoms the env). Add `y = 5` then alias `y = sym` ⟹ both
  typed `(= 5)` — the symbol is demoted to the constant, never the reverse
  (`aliases.ml#add` fatals on "Cannot demote a constant"). Confirms indelibility.
- **Join agreement (claim 5).** Join of two branch envs each with `x = 42` ⟹ joined
  `x : (= 42)`, under BOTH the Binary and (with
  `Oxcaml_flags.Flambda2.join_algorithm := Set N_way`) the n-way join — the N_way
  caveat is closed.

## The gating refinement (before running the pipeline)

The `meet_test` join witness calls `cut_and_n_way_join` DIRECTLY, below the
`compute_handler_env` gate, so it validates the join *algebra* but not the pipeline
path. Prediction for the pipeline: two branches that each pass the constant `42` to a
join continuation `k` with ≥2 uses fold to `(= 42)` in `k`'s handler ONLY when
`join_points()` holds (set at `-O2`/`-O3`); at default flags the merge runs NO join
and the constant is LOST (`k`'s param gets `unknown_with_subkind`).

## Verdict

MATCH, with the join_points() precondition folded in. The join algebra delivers
constant agreement on both algorithms (witnessed), but the persistence-through-merge
theorem is conditioned on `join_points() = true` — the SAME flag gate as
`S.Struct.JoinParams.AnalysisExtraParams`, resolving the ch-09 open question from the
types side. This is why `T.Join.ConstAgreement` and `INV.Simplify.AliasesMonotoneDown`
both carry the explicit `join_points()` premise for their cross-merge clauses.

Repro: `middle_end/flambda2/tests/meet_test.ml` (the three prediction-first
witnesses at end-of-file), run via
`./_build/main/middle_end/flambda2/tests/meet_test.exe`.
