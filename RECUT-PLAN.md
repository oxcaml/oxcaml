# Re-cut plan: partition the legacy-deletion finale across two PRs

Campaign: ikind-unification. This document records the Phase-1 re-partition of the
existing `ik/legacy-deletion` finale (11 commits off `3f936718a`, previously all on
PR #6460) into two PRs, per the user's re-drawn boundary (2026-07-12):

- **PR #6451** (`ik/pr1-deletions`, off `3f936718a`): every deletion that is
  **provably not behavior-changing** — the compiler's observable output (all printed
  text incl. error messages, exit codes, generated artifacts) is byte-identical with
  and without it, under default flags, over the full testsuite.
- **PR #6460** (`ik/pr2-print-deletions`, rebuilt on the PR1 tip): the deletions that
  **change print behavior** (which history/reason/text is chosen), plus the test
  promotions that record that churn, plus the campaign docs.

## Partition criterion (applied strictly)

A piece goes to PR1 iff observable output is byte-identical with and without it under
default flags on the full testsuite. Flag/test deletions qualify if default behavior
is unchanged. Anything touching WHICH history/reason/text is chosen goes to PR2.

## Source commits (old `ik/legacy-deletion`, base `3f936718a`)

```
PLAN 6432474b8 (docs)          M4 3a673aa84 (ordering switch + 20 promotions)
M5 253546882 (seam error-text) S9 e2a74febf (flag surface + 40 twin deletes)
notes 52ac66c16 (docs)         PERF 47abb4097 (defer ordering to display)
FINAL 193b5136f (sub chain)    S5 67becebc0 (docs)  S7 86a50f9c4 (docs)
amendments 259503bc1 (docs)    5e-exit ba7da7299 (docs)
CI-fix 0c310a2cd (20 promotions)
```

## Caller census (at the PR1 base `3f936718a`) driving the FINAL split

`combine_histories` (jkind.ml) calls `Jkind_desc.sub` EAGERLY at the base. The upper
legacy sub chain is reached only through the two ikind seams and the `-no-ikinds`
fallbacks:

| function | callers at base | dead after |
|---|---|---|
| `Jkind.sub` (public) | none (verified whole-tree) | already dead → PR1 |
| `check_sub` | `sub_with_reason` only | PR1 |
| `sub_with_reason` | `sub`, `sub_or_intersect` fn | PR1 |
| `sub_or_intersect` fn | `sub_or_error` fn; ikind seam (M5); `-no-ikinds` (S9) | PR1 |
| `sub_or_error` fn | ikind seam ×2 (M5); `-no-ikinds` (S9) | PR1 |
| `sub_jkind_l` (Jkind) | `-no-ikinds` (S9) | PR1 |
| `get_mode_crossing` (Jkind) | `-no-ikinds` (S9); S1 validate tripwire | PR1 |
| `With_bounds.to_best_eff_map` | `sub_jkind_l` (Jkind) | PR1 |
| `Base.sub_expanded` | `Jkind_desc.sub_expanded`, `sub_jkind_l` | PR2 (kept alive by `Jkind_desc.sub_expanded`) |
| `Jkind_desc.sub_expanded` | `Jkind_desc.sub` | PR2 |
| `Jkind_desc.sub` | `combine_histories` (eager), `check_sub` | PR2 (kept alive by combine) |

Consequence: the ENTIRE upper chain (`Jkind.sub`, `check_sub`, `sub_with_reason`,
`sub_or_intersect` fn, `sub_or_error` fn, `sub_jkind_l`, `get_mode_crossing`,
`to_best_eff_map`) dies once M5+S9 remove the seams and fallbacks — output-neutral, so
PR1. Only `Jkind_desc.sub` + `Jkind_desc.sub_expanded` + `Base.sub_expanded` stay live
(via `combine_histories`' eager legacy orderer) until M4+PERF switch that selector to
the ikind verdict — so their deletion is coupled to the print-changing switch → PR2.

## Hunk → PR assignment

### PR1 `ik/pr1-deletions` (off 3f936718a) — output-preserving

1. **M5** (`typing/ikind.ml`): route the two ikind seams (`sub_or_error` reject,
   `sub_or_intersect` layout-fail) to build reason lists ikind-natively. Zero churn
   (sub_jkind_l was already the primary ikind-native error path). *Re-proven
   standalone against the PR1 base, without M4.*
2. **S9** (`driver/{compenv,main_args}.ml{,i}`, `utils/clflags.ml{,i}`,
   `typing/{ikind,jkind,ctype,typecore}.ml`, 40 test deletions): delete the
   `-no-ikinds` opt-in flag surface + collapse the `if not !Clflags.ikinds`
   fallbacks; delete the 40 legacy-twin tests (each has an `_ikinds` sibling).
   Default output unchanged (flag was opt-in).
3. **FINAL-subset** (`typing/jkind.ml{,i}`, `typing/ikind.ml`, `typing/ctype.ml`):
   the FINAL sub-chain deletion MINUS the two hunks that stay alive via
   `combine_histories` — i.e. delete the whole upper chain above, plus the S1
   crossing tripwire + `crossing_reroute` counters, minus `Jkind_desc.{sub,
   sub_expanded}` and `Base.sub_expanded`.
4. **Stale doc-comment fixes** (`typing/types.mli`, `typing/ikind.ml`): the two
   comments referencing the now-deleted `Jkind.sub`/`Jkind.sub_jkind_l` and the
   removed `-no-ikinds` mode (from the original amendments commit). Logically
   PR1's — they would dangle against symbols PR1 deletes if deferred to PR2.
   Comments only; output-neutral.
5. **RECUT-PLAN.md** (this file).

### PR2 `ik/pr2-print-deletions` (rebuilt on PR1 tip) — print-changing

5. **M4** (`typing/jkind.ml` + 20 `[%%expect]` promotions): `combine_histories`
   selects the displayed history from the ikind sub verdict instead of legacy
   `Jkind_desc.sub`; `score_reason` de-prioritizes notify-JS placeholder histories.
   Changes ~57% of displayed histories → the promoted churn.
6. **PERF** (`typing/jkind.ml{,i}`, `typing/ikind.ml`): defer the ordering to
   error-display time (removes the last per-combine `Jkind_desc.sub` caller). Ikind-
   ordering form → coupled to M4.
7. **FINAL-remainder** (`typing/jkind.ml`): delete `Jkind_desc.sub`,
   `Jkind_desc.sub_expanded`, `Base.sub_expanded` — now uncalled after M4+PERF.
8. **CI promotions** (20 `[%%expect]` promotions in 13 `typing-layouts-*` dirs): the
   same M4 history-selection churn, in the wider testsuite.
9. **Campaign docs** (`STAGE5M-*.md`, `STAGE5D-MIGRATION.md`, `STAGE5E-EXIT.md`, …):
   kept on PR2 to avoid polluting PR1. (The source-comment part of the original
   amendments commit moved to PR1 item 4.)

## Acceptance

- PR1: boot-green; affected suites (the dirs the finale swept + the 13
  `typing-layouts-*` CI dirs) byte-identical vs `3f936718a` — ZERO churn.
- PR2 (on PR1 tip): boot-green; full suites green; churn = exactly the 40 promoted
  tests, nothing else.
- Combined PR2 tip tree content-identical to `0c310a2cd` (modulo RECUT-PLAN.md
  docs-placement).
