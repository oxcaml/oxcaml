# Stage 5m plan: physically delete the legacy jkind engine (adjusted design)

Campaign: ikind-unification. Branch `ik/legacy-deletion` off `3f936718a`
(wave-A head: STAGE5D S0-S4 landed, CI 36/36 green). This document adjusts the
`STAGE5D-MIGRATION.md` M-table + S-plan for the two constraint changes the user
handed down (2026-07-12) and records the seam-level findings that reshape the
work. It is the design of record for the finale; `STAGE5D-NOTES.md` remains the
wave-A as-built.

## 0. The two constraint changes (and what they unblock / re-constrain)

**(1) Error-text byte-identity is WAIVED on the ikind path.** "ikind errors can't
be byte identical." The S4 history divergence (~57%) and the M3/M5 reason-list
differences are **no longer blockers** — they are *expected churn to promote*.
Discipline (mandatory, per changed [%%expect]/reference):
- (a) each change must be *caused by an intentional switch* (M4/M5/etc.), not an
  accident;
- (b) each is *reviewed by me as correct-or-better before promotion* — the new
  text must still name the right axes/types (no garbage errors);
- (c) each is *tallied in STAGE5M-NOTES.md* (count + representative before/after).
- **Zero ACCIDENTAL churn: non-error tests must not change.**

**(2) NON-ERROR default printing stays BYTE-IDENTICAL.** The `with_bounds`
print-sidecar rule is unchanged. This *re-constrains* S5-S7: whatever feeds
successful-path printing (`normalize_decl_jkinds` → the stored/printed
`type_jkind`, `-i`/`.mli` output) must keep byte-identical output — migrate
*provably* or *keep* those parts. Deleting the ~500-line fixpoint is licensed
only where its products are **error-text (waived)** or **verdicts (ikind-owned
since wave A)**.

## 1. Seam census on the wave-A head (line numbers verified 2026-07-12)

The live legacy-engine entry points and their callers:

| legacy entry | live caller(s) | product class | slice |
|---|---|---|---|
| `Jkind_desc.sub` (jkind.ml:3882 `check_sub`) | `combine_histories` (jkind.ml:3764); `Jkind.sub*`/`sub_or_error`/`sub_or_intersect` via `check_sub`→`sub_with_reason` | verdict + error-text + history | M4, M5, S9 |
| `Jkind.sub_or_error` (jkind.ml) | ikind.ml:2142 (`-no-ikinds` fallback), :2176 (debug dual-run), :2192 (error-text on reject) | error-text + flag fallback | M5, S9, FINAL |
| `Jkind.sub_or_intersect` (jkind.ml:3898) | ikind.ml:2069 (layout-fail reason), :2119 (`-no-ikinds` fallback) | error-text + flag fallback | M5, S9 |
| `Jkind.get_mode_crossing` (jkind.ml:2646) | ikind.ml:1917 (`-no-ikinds` fallback), :1942 (S1 validate tripwire) | crossing (default path already ikind, S1) | S9, FINAL |
| `Jkind.round_up` (jkind.ml:3865→`normalize`) | ctype.ml:3589/3590/8438 | type-op (nondep/intersect) | S5 |
| `Jkind.normalize` (jkind.ml:2561→`Base_and_axes.normalize`) | typedecl.ml:3540 (`normalize_decl_jkinds`); `Violation.of_` (jkind.ml:3276-84); `round_up` | **decl-print (byte-identical) + error-text (waived) + type-op** | S5, S6, **S7 (crux)** |
| `Base_and_axes.normalize` fixpoint (jkind.ml:896-~1393, ~500 lines) | all of the above via `sub`/`normalize`/`round_up` | — | gated on ALL callers gone |

### 1a. Two findings that reshape the M-table (STOP-AND-SCOPE, not blocker)

**Finding A — `normalize_decl_jkinds` is load-bearing for byte-identical
non-error printing.** typedecl.ml:3539-3548 stores `type_jkind = Jkind.normalize
~mode:Require_best ...` as the *authoritative* decl jkind. That stored jkind is
what `-i`/`.mli`/decl printing renders. Under constraint (2) this output must
stay byte-identical. Its product is **neither error-text nor a verdict** — so per
the user's own licensing, deleting the fixpoint *for this use* is NOT licensed
unless an ikind-derived normalized decl jkind is proven byte-identical to legacy
`normalize` over the corpus. This is exactly the "long pole" `STAGE5D-MIGRATION.md`
§8 flagged (M6, MED-HIGH). **Consequence:** PAYOFF 2 (delete
`Base_and_axes.normalize`) + PAYOFF 3 (relabel the field, which `normalize`
reads) are *gated on the S7 decl-normalize differential succeeding*. If it
diverges (as S4 did for histories), `normalize` STAYS for the decl path, is
documented as the surviving print sidecar, and the residual is reported to the
user — a sanctioned "keep those parts" outcome, not a failure.

**Finding B — `Mod_bounds.{meet,less_or_equal}` have non-engine callers.**
`Mod_bounds.meet` is used in `subst.ml:588,610` (floor meet during
substitution — legitimate, not subsumption) and inside `Jkind_desc.intersection`
(jkind.ml:1533). `Mod_bounds.less_or_equal` is used at jkind.ml:1475/1735/4016
(inside the legacy engine). So the `STAGE5D-MIGRATION.md` "DELETE
`Mod_bounds.{meet,less_or_equal}`" is not a clean delete: the subst.ml floor-meet
survives and becomes `Axis_lattice.meet` only after S8 (field relabel). FINAL
deletes the ops only once the engine + subst callers are re-routed.

## 2. Achievable end-state (honest)

- **Confident (days-scale, this branch):** PAYOFF 1 — delete `Jkind_desc.sub` +
  `check_sub`/`sub`/`sub_with_reason`/`sub_or_*` + the engine-internal
  subsumption ops; PAYOFF 4 — delete `-no-ikinds` surface + 40 twin tests + the
  dual-engine differential; retire the wave-A hooks (S3/S4/overturn detector) and
  the S1 legacy-crossing tripwire. All churn is error-text (waived) or code
  deletion (no product change).
- **Gated on the S7 differential:** PAYOFF 2 (`Base_and_axes.normalize` fixpoint)
  + PAYOFF 3 (`mod_bounds : Mode.Crossing.t` → `ikind_floor : Axis_lattice.t`).
  Attempt-then-decide; measure before deleting.

## 3. Bisectable slice order (one commit each; per-commit boot-green)

Prefix every commit `ikind: legacy-deletion -- `.

1. **PLAN** (this file).
2. **M4** — `combine_histories` selects on the ikind verdict, not `Jkind_desc.sub`.
   Always-compute the verdict (drop the gate on the *selection* path; keep the
   differential counters as telemetry). Promote error-text churn (histories
   shown in errors change on ~57% of combines — the S4-measured divergence).
   Removes the `combine_histories` `Jkind_desc.sub` caller.
3. **M5** — error-text off legacy:
   - `sub_or_error` reject (ikind.ml:2192): drop the `Jkind.sub_or_error`
     error-text call; build `Not_a_subjkind` reasons from the ikind violating
     axes (the overturn branch at :2203-2214 already does this — extend to the
     normal reject).
   - `sub_or_intersect` layout-fail (ikind.ml:2069): classify Disjoint/May via
     `Jkind.may_have_intersection` (a `Base` op — KEEP) + build the reason list
     ikind-natively (layout reason from `sub_layout_or_error`, axis reasons as
     `sub_jkind_l` does). Drop the `Jkind.sub_or_intersect` call.
   - `Violation.of_` (jkind.ml:3270): the `normalize ~mode:Require_best` for
     display is error-only — migrate to ikind-derived rendering (extend 5c
     `-print-from-ikinds`) or drop the normalize where the raw jkind renders
     acceptably. Promote+review+tally.
   Removes the error-text callers of `Jkind_desc.sub` and the `Violation.of_`
   caller of `normalize`.
4. **S5** — `round_up` (ctype.ml ×3, type-ops): route to `Ikind.round_up_type` /
   the ikind round_up. Removes `round_up`'s `normalize` caller. (Verdict-adjacent;
   any error text is waived, but these feed type-ops — assert semantic
   equivalence of the rounded jkind via a validate differential first.)
5. **S9** — delete `-no-ikinds`: `Clflags.ikinds`, `-no-ikinds`
   (main_args.ml:827/2397, compenv.ml:581, clflags.ml:216), the `if not
   !Clflags.ikinds then <fallback>` branches (ikind.ml:1916/2118/2141 + the two
   `sub_jkind_l` sites), the debug dual-run (ikind.ml:2176), the `"ikinds
   disabled"` sentinel (ikind.ml:830). KEEP `-ikinds-debug`, `-print-from-ikinds`.
   40 twin tests: per file, delete (if the `_ikinds` twin covers it) or convert;
   tally. Retire the dual-engine differential; **KEEP the
   `OXCAML_IKINDS_VALIDATE` recompute-from-`with_bounds` harness + seeded-fault
   infra (§D permanent).**
6. **FINAL-a** — with M4+M5+S9 done, `Jkind_desc.sub` + `check_sub` + `sub` +
   `sub_with_reason` + `Jkind.sub_or_*` have no callers. Delete them + the
   engine-internal subsumption ops (`Mod_bounds.less_or_equal` engine sites,
   `With_bounds.{meet,join}`), the S1 `get_mode_crossing` tripwire, and the
   wave-A differential hooks (`set_sub_verdict_from_ikind`, `sub_or_error`
   overturn detector, `soi_class_*`). KEEP `Mod_bounds.{to,of}_axis_lattice`,
   `With_bounds.{to_seq,map_type_expr,format}`, `may_have_intersection`.
7. **S7** — decl-normalize differential (the crux). Derive the normalized decl
   jkind from the ikind; validate-gated differential vs legacy `normalize` over
   the corpus. GREEN → migrate `normalize_decl_jkinds` + delete
   `Base_and_axes.normalize` + `Jkind.normalize` (PAYOFF 2). DIVERGENT → KEEP
   `normalize` for the decl path, document as the print sidecar, report residual.
8. **S8** (iff S7 green) — `mod_bounds : Mode.Crossing.t` → `ikind_floor :
   Axis_lattice.t` (69 literals; warning 9 off so patterns unchanged); re-route
   the subst.ml floor-meet to `Axis_lattice.meet`; delete `Mod_bounds.meet`
   (PAYOFF 3).

Independence: M4, M5, S5, S9 are independent (do in order for clean bisect).
FINAL-a gates on M4+M5+S9. S7 gates PAYOFF 2; S8 gates on S7.

## 4. Acceptance (per the user's gate)

- Boot-green per commit; suites: **non-error tests ZERO churn**; error-text churn
  only where intentional, each promoted + justified + tallied in STAGE5M-NOTES.md.
- `OXCAML_IKINDS_VALIDATE` recompute corpus **0 HARD** end-state; cross-unit /
  residue counters unchanged (residue-neutralized=3, imported-residues=9).
- Net LOC strongly negative.
- ocamlformat on enforced files; ≤80-col added lines in non-enforced files (edit
  those via Bash/python — the Edit-tool formatter hook wholesale-reformats).
- FREEZE at each major landing (M4, M5, post-FINAL-a, S7-decision, S8) + at any
  blocker >45 min, for adversarial review. NEVER push.

## 5. Where I will STOP-AND-SCOPE

- If the S7 decl-normalize differential diverges: report; keep `normalize`; do
  NOT force a byte-identical-print change (constraint (2) is a hard user gate).
- If any non-error test churns: stop; that is accidental churn, a bug in the
  migration, not promotable.
- If a legacy op turns out to have a caller the census missed (Finding B class):
  re-route or keep, do not delete blind.
