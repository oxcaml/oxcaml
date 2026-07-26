# Stage 5d migration design: the legacy engine is not dead — measure, then migrate-then-delete

Campaign: ikind-unification. Branch `ik/stage5d-deletion` off `4156b7eef`
(integrated wave-2 head: 5a + 5b + 5c + format-lock-in). This document replaces
the "mechanical deletion" framing of `STAGE5-DESIGN.md` §5d with an
evidence-based migrate-then-delete plan, per team-lead adjudication
(2026-07-12).

## 0. TL;DR

`STAGE5-DESIGN.md` §5d says the legacy `Base_and_axes.normalize` fixpoint and
`Jkind_desc.sub` "never run" and can be deleted mechanically. **They run
heavily.** Probe + full-corpus measurement (below) shows:

- The legacy engine's **verdict role is already nil**: across 7 seam-heavy
  testsuite dirs, the reject-defer at `sub_or_error` overturned **0** verdicts
  (of 106 ikind-rejects) and produced **0** soundness-direction disagreements.
- The legacy engine's **bulk use is non-verdict**: history ordering
  (`combine_histories`, ~267k calls), error-text pretty-printing
  (`Violation.of_`, ~180k), crossing (1238, all the with-bounds-free fast path,
  5c-proven equal to the ikind floor), decl-jkind normalization (the H2 print
  sidecar), and `round_up`.

**Consequence:** deleting the legacy *sub* + subsumption ops is a days-scale
re-route (4 differential-provable slices). Deleting the legacy *normalize*
fixpoint is a real re-architecture whose long pole is the byte-identical
error-text path (`Violation.of_`), a standing user constraint exercised by
~180k calls / every distinct error shape in the corpus. The two are separable;
§6 gives the staged plan and §7 the deletability map.

## 1. Not a wave-2 regression (option (C) ruled out)

The live legacy callers predate stage 5, so the doc's "never runs" was stale
analysis, not a regression introduced by wave 2:

- `typing/typedecl.ml:3540` (`normalize_decl_jkind` → `Jkind.normalize`):
  `git blame` = 2025-03-05 (Ryan Tjoa, #3526).
- `typing/ctype.ml:3589/3590/8438` (`Jkind.round_up`) and
  `typing/jkind.ml:3270` (`Violation.of_` normalizes for printing):
  `git blame` = 2026-02-14 (Chris Casinghino, abstract-kinds #5071).

Stage 5 (2026-07) never touched them. No rebuild / bisect needed.

## 2. Static reachability (why the doc's premise is false)

The doc's §5d normalize caller census cites `ctype.ml:2195, 3477`; `2195` is a
CR *comment*, not a call. The actual live, un-`!Clflags.ikinds`-gated callers of
the legacy engine are:

| legacy entry | live caller(s) | role |
|---|---|---|
| `Jkind.normalize` (jkind.ml:2561) | typedecl.ml:3540 | per-decl normalization / print sidecar |
| `normalize` (via `Violation.of_`, jkind.ml:3276-79) | ikind.ml:1759 (`sub_jkind_l`) + all error paths | error-text pretty-printing |
| `Jkind.round_up` (jkind.ml:3782) → normalize | ctype.ml:3589/3590/8438 | intersect / nondep type ops |
| `Jkind.get_mode_crossing` (jkind.ml:2664) → normalize | ikind.ml:1826 (crossing fast path) | mode crossing (Layout+empty) |
| `Jkind_desc.sub` (jkind.ml:1494) → normalize | jkind.ml:3724 (`combine_histories`) + the ikind seams | history ordering + verdict classification |

Even after deleting the `-no-ikinds` flag surface (the `if not !Clflags.ikinds
then <fallback>` branches), the ikind **else**-branches themselves still call
legacy: `sub_or_error` reject-defer (ikind.ml:2049), `sub_or_intersect`
layout-fail classification (ikind.ml:1946), crossing fast path (ikind.ml:1826),
and every error's `Violation.of_`.

## 3. Measured numbers

**Method.** Instrumented `Base_and_axes.normalize`, `Jkind_desc.sub`,
`Violation.of_`, `round_up`, and the three ikind seams (`sub_or_error`,
`sub_or_intersect`, `crossing_of_jkind`) with per-process at-exit counters,
gated on `OXCAML_IK5D_MEASURE` (non-perturbing when off: the reject-defer and
layout-fail counters piggyback on legacy calls the seams already make; the
accept-side soundness counter forces the existing debug dual-run only under the
measure flag). Ran `make test-one DIR=<d>` over 7 seam-heavy dirs
(typing-jkind-bounds, typing-abstract-kinds{,-missing-cmi}, typing-layouts,
typing-layouts-or-null, typing-layouts-products, typing-modules), default
ikinds-on, ~1246 compiler processes, summed. Instrumentation reverted after
measuring (not committed).

**Totals:**

| quantity | count | note |
|---|---:|---|
| `sub_or_error` ikind-path calls | 124 | |
| — ikind-rejects | 106 | |
| — **OVERTURNS (ikind-reject → legacy-ACCEPT)** | **0** | the reject-defer never fired |
| — soundness dir (ikind-accept → legacy-REJECT) | 0 | ikind never more permissive |
| `sub_or_intersect` ikind-path calls | 5,827,059 | dominant subkind check |
| — legacy classification (layout-fail branch) | 88,941 | Disjoint=1,472, May_have_intersection=87,469 |
| `crossing_of_jkind` calls / legacy fast-path fires | 1,238 / 1,238 | 100% hit the legacy fast path |
| `Base_and_axes.normalize` invocations (incl. recursion) | 2,810,080 | |
| — via `Jkind_desc.sub` | 356,694 | |
| — of which `combine_histories` (residual) | ~267,629 | jkind.ml:3724, non-verdict history ordering |
| — of which seam classification | 88,941 | = `sub_or_intersect` layout-fail |
| `Violation.of_` (error-text) constructions | 180,492 | each normalizes ×2 |
| `round_up` | 274 | |

Per-dir overturn (all 0): jkind-bounds 8 calls/0 rej; layouts 46/42; or-null
18/12; modules 52/52; abstract-kinds 0; products 0.

**`combine_histories` provenance** (jkind.ml:3722-3732): the only `Jkind_desc.sub`
caller outside the seam→`check_sub` chain. It calls legacy `sub` to decide
which of two histories to *keep* (`Less`→a, `Not_le`→b, `Equal`→higher score) —
purely error-message quality, never a type-checking verdict. Residual
arithmetic: 356,694 − 88,941 (seam) − 124 (soe) = 267,629.

## 4. The reframe: verdict role vs derivation/print role

The ikind engine is already the sole *verdict* authority: `sub_or_error` never
defers (0/106), the soundness direction is clean (0), and `sub_or_intersect`'s
accept/reject on the layout-OK path is pure ikind. The only verdict-shaped
legacy residue is `sub_or_intersect`'s **Disjoint-vs-May_have_intersection
classification on the layout-FAIL path** (88,941), which is a `Base`
intersection question (`may_have_intersection`) plus a failure *reason* — not
the mod-bounds/with-bounds axis engine.

Everything else the legacy engine does is the H2-surviving "derivation input +
print sidecar" machinery:

- **history ordering** (`combine_histories`, ~267k) — chooses the displayed
  history; non-verdict.
- **error-text pretty-printing** (`Violation.of_`, ~180k) — normalizes for
  best-normalized display; byte-identical output is a **standing user
  constraint**.
- **crossing** (1,238, all fast path) — 5c's differential proved the ikind
  const base == `to_axis_lattice mod_bounds` for with-bounds-free jkinds.
- **decl normalization** (typedecl) — the stored/printed `type_jkind` sidecar.
- **round_up** (274) — type-op rounding.

## 5. Per-use migration design + churn risk

| # | use | migration | churn risk |
|---|---|---|---|
| M1 | crossing fast path (ikind.ml:1826) | replace `Jkind.get_mode_crossing` with `Axis_lattice.to_mode_crossing (Mod_bounds.to_axis_lattice jk.mod_bounds)` — reads the surviving lattice floor, no normalize; keeps the perf fast path | **LOW** (5c-proven equal; keep a validate cross-check) |
| M2 | `sub_or_error` reject-defer (ikind.ml:2049) | delete the defer; trust the ikind reject; keep a validate-gated dual-run tripwire | **LOW** (0 overturns / 0 soundness dir); error *text* still via M5 |
| M3 | `sub_or_intersect` classification (ikind.ml:1946) | on layout-fail, classify Disjoint/May via `may_have_intersection` (a `Base` op — keep) and build the reason ikind-natively (layout reason from `sub_layout_or_error`; axis reason as `sub_jkind_l` already does) | **MEDIUM** (reason payload in Disjoint/May error text) — needs a coexistence differential over 88,941 sites |
| M4 | `combine_histories` (jkind.ml:3724) | replace `Jkind_desc.sub` with the ikind sub (`Ldd.leq_with_reason` both directions → Less/Not_le/Equal) | **MEDIUM** (which history string is shown; non-verdict but high volume ⇒ tiny divergence = churn) — needs a Less/Not_le/Equal-agreement differential |
| M5 | `Violation.of_` error text (jkind.ml:3270) | derive the best-normalized display jkind from the ikind (extend 5c `-print-from-ikinds`); must be **byte-identical** to legacy normalize output for every error shape | **HIGH** (user constraint; ~180k calls; with-bounds display collapse is exactly what normalize does) — the long pole |
| M6 | decl normalization (typedecl.ml:3540) | store raw `with_bounds` + derive normalized display on demand from the ikind | **MED-HIGH** (`-i`/.mli/decl print byte-identical) |
| M7 | `round_up` (ctype.ml ×3) | route to `Ikind.round_up_type` / ikind round_up | **MEDIUM** (274 sites; semantic equivalence of the rounded jkind) |

All migrations follow the coexistence-window doctrine: land a validate-gated
differential that proves ikind-native == legacy over the corpus **before**
deleting the legacy path.

## 6. Staged, bisectable plan

- **S0 — dead toggle (BANKED).** `reset_constructor_ikind_on_substitution`
  deleted, commit `c27e8d888`, boot-green. The one genuinely-dead §5d item.
- **S1 — M1 crossing re-route.** Deletes the seam's `get_mode_crossing` use (ikind.ml:1826).
- **S2 — M2 reject-defer deletion** + validate tripwire. Deletes the seam's
  `sub_or_error` legacy use (ikind.ml:2049).
- **S3 — M3 classification migration** + differential. Deletes the seam's
  `sub_or_intersect` legacy use (ikind.ml:1946).
- **S4 — M4 combine_histories migration** + differential. Deletes the
  jkind.ml:3724 `Jkind_desc.sub` call.
  - → **PAYOFF 1: delete legacy `Jkind_desc.sub` (jkind.ml:1494) +
    `Mod_bounds.{meet,less_or_equal}` + `With_bounds.{meet,join}`** (now dead;
    `check_sub`/`sub`/`sub_with_reason`/`sub_or_*` lose all callers). KEEP
    `Mod_bounds.{to,of}_axis_lattice`, `With_bounds.{to_seq,map_type_expr,format}`.
- **S5 — M7 round_up migration.**
- **S6 — M5 error-text migration** + byte-identical differential (**highest
  risk**).
- **S7 — M6 decl-normalize migration.**
  - → **PAYOFF 2: delete `Base_and_axes.normalize` fixpoint (~500 lines) +
    `Jkind.normalize` + `get_mode_crossing`/`round_up`** (now dead).
- **S8 — mod_bounds field.** `mod_bounds : Mode.Crossing.t` →
  `ikind_floor : Axis_lattice.t` (69 literals; the floor value survives, only
  the `Mode.Crossing.t` typing goes). Mechanical once normalize is gone.
  - → **PAYOFF 3.**
- **S9 — `-no-ikinds` flag surface.** Delete `Clflags.ikinds`, `-no-ikinds`,
  the `if not !Clflags.ikinds` fallbacks, the "ikinds disabled" sentinel
  (ikind.ml:768), and the §D dual-engine differential. **Requires
  deleting/converting the 40 legacy-twin tests** that pass `-no-ikinds` (18
  typing-jkind-bounds, 8 subsumption, 4 typing-abstract-kinds, 3
  regression-tests, plus others — see §8). Test-heavy; its own slice.
  - → **PAYOFF 4.**

Independence: S1/S2/S5/S9 are independent; S3, S4 gate PAYOFF 1; S5+S6+S7 (+S1)
gate PAYOFF 2; S8 follows PAYOFF 2. The error-text differential (S6) is the
critical path for deleting normalize.

## 7. Deletability map (which §5d payoff after which step)

| §5d target | deletable after |
|---|---|
| `Jkind_desc.sub` + `Mod_bounds`/`With_bounds` subsumption ops | S3 + S4 (verdict + history off legacy) |
| `Base_and_axes.normalize` fixpoint + `Jkind.normalize` | S1 + S5 + S6 + S7 (crossing + round_up + error-text + decl-norm off legacy) |
| `mod_bounds : Mode.Crossing.t` field | S8 (after normalize/crossing stop reading it) |
| `-no-ikinds` surface + dual-engine differential | S9 (independent; 40-test change) |

## 8. Size estimate + the call

- **Days-scale (S1-S4):** deletes legacy *sub* + subsumption ops. All four are
  0-overturn or non-verdict, each differential-provable. Low-to-medium risk.
- **Real re-architecture (S5-S7):** deletes legacy *normalize*. Dominated by the
  byte-identical **error-text** path (M5): ~180k calls, must reproduce
  normalize's best-normalized display for every error shape from the ikind.
  This is the effort/risk sink and the reason the deletion is not "mechanical".
- **Mechanical tails (S8, S9):** 69 field literals; 40 twin tests.

**Recommendation:** execute S1-S4 (days-scale, high-confidence, delivers PAYOFF
1) as the next wave; scope S5-S7 (the normalize deletion) as a separate
error-text-migration project with the byte-identical differential as its
acceptance gate — that is the piece the team-lead flagged as the "upstairs"
call. S8/S9 land after their prerequisites.

## Appendix: flag-surface twin-test dependency (task detail)

40 testsuite files pass `-no-ikinds` (the legacy-engine coexistence twins):
typing-jkind-bounds 18, .../subsumption 8, typing-abstract-kinds 4,
.../regression-tests 3, typing-layouts-or-null 2, and 5 others. Each is a
near-duplicate of its `_ikinds` counterpart (same source; differs in the `flags`
line and the `[%%expect]` blocks that pin legacy-vs-ikind output). Deleting
`-no-ikinds` (S9) therefore deletes/converts all 40 and retires the §D
dual-engine differential (`OXCAML_IKINDS_VALIDATE`'s recompute-from-with_bounds
harness SURVIVES per §D — it does not depend on `-no-ikinds`).
