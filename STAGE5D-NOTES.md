# Stage 5d notes: wave A (ikind becomes the sole default-path answering engine)

Campaign: ikind-unification. Branch `ik/stage5d-deletion` off `4156b7eef`
(integrated wave-2 head). This is the AS-BUILT record for stage-5d wave A, the
re-scoped follow-on to the STOP-AND-SCOPE + measurement recorded in
`STAGE5D-MIGRATION.md`.

## Headline (the reframe)

`STAGE5-DESIGN.md` §5d assumed the legacy `Base_and_axes.normalize` /
`Jkind_desc.sub` engine was dead and mechanically deletable. Measurement
(`STAGE5D-MIGRATION.md`) refuted that: the ikind engine is ALREADY the sole
*verdict* authority, but the legacy engine is still the live decl-normalization
/ error-text / history-ordering / crossing / round_up machinery. **Physical
deletion of `Jkind_desc.sub` + `Mod_bounds.{meet,less_or_equal}` +
`With_bounds.{meet,join}` is gated on S9 (delete `-no-ikinds`, whose flag-off
fallbacks ARE the legacy engine — 40 twin tests) AND M5 (migrate the
error-text path), both queued for the user.**

Wave A therefore delivers the honest, reachable goal: **the ikind engine is the
sole answering engine on the default (ikinds-on) path**, with legacy quarantined
behind `-no-ikinds`, the M5 error-text call, and validate tripwires — plus
machine-checked differentials over the remaining two seams: S3 (classification)
proves switch-ready; S4 (history ordering) proves DIVERGENT and routes into M5.

## Slices

### S0 (banked) — delete the dead toggle
`c27e8d888`. `reset_constructor_ikind_on_substitution` was `= false` with an
always-false `when` guard; removed the binding + dead arm. The one genuinely
dead §5d item. Boot-green; ocamlformat clean.

### S1 — crossing off legacy on the default path
`e69e6447b`. `crossing_of_jkind`'s with-bounds-free fast path returned the legacy
`Jkind.get_mode_crossing` (→ `get_mod_bounds` → `Base_and_axes.normalize`).
Re-routed to the ikind lattice floor
`Axis_lattice.to_mode_crossing (Mod_bounds.to_axis_lattice mod_bounds)` — the
ikind const base for this class, no legacy `normalize`, no LDD build (perf fast
path preserved). STAGE5C's floor differential proved
`round_up(ikind) == to_axis_lattice(mod_bounds)` for with-bounds-free jkinds;
re-asserted AT the crossing seam, validate-gated (compared to legacy
`get_mode_crossing` via `Mode.Crossing.equal`, `[ikind-crossing-reroute]`
counter). Legacy `get_mode_crossing` KEPT as the validate tripwire (deleted with
`normalize` in the M5/S5-S7 project).

Evidence: validate over typing-jkind-bounds → crossing-reroute **checks=234,
mismatches=0**; validate summary 0 mismatches / 0 class_b / 0 collisions. Zero
suite churn vs baseline: typing-jkind-bounds 73/0, typing-layouts 45/0,
typing-modules 54/0. Boot-green; ocamlformat clean.

### S2 — sub_or_error trusts the ikind reject (verdict deference flipped)
`4784580c1`. The reject path used to DEFER to the legacy engine (return the
legacy accept when ikind rejected but legacy accepted). Measured overturn = 0
corpus-wide, so the deference is vestigial. Flipped: the ikind verdict is
authoritative; on the (measured-impossible) overturn we reject, synthesising the
error from the ikind violating axes. The NORMAL reject still calls the legacy
path for the detailed message (byte-identical, M5-owned) — only the verdict
deference changed. Permanent validate-gated overturn detector
(`[ikind-sub-or-error-overturn]`), cheap measurement hatch `OXCAML_IK5D_MEASURE`,
seeded fault `OXCAML_IK5D_SUBERR_FAULT`.

Evidence:
- **Seeded-fault non-vacuity:** forcing the ikind reject on typing-jkind-bounds
  (8 genuine accepts) → detector reports rejects=8 **overturns=8** — it fires.
- **Overturn re-run** (`OXCAML_IK5D_MEASURE`, 11 seam dirs: jkind-bounds,
  layouts, layouts-or-null, layouts-products, modules, modal-kinds, modes,
  gadts, kind, recmod, abstract-kinds) + the pre-S2 7-dir sweep: **0 nonzero
  overturns**.
- **GEM (materially de-risks the S9/M5 decision):** across all 11 seam dirs the
  natural `sub_or_error` reject count is **0** — the seam is entirely
  accept-path in the corpus (genuine rejects route through `sub_jkind_l` or fail
  earlier at the layout check). So the flipped branch is never taken naturally;
  S2 changes zero corpus behaviour, and the ikind-vs-legacy verdict can only
  diverge on a shape the corpus does not exercise (and the soundness invariant —
  ikind ⊇ legacy accepts — forbids the unsound direction anyway).
- Zero suite churn: 73/0, 45/0, 54/0. Boot-green; ocamlformat clean.

Note: S2 does NOT free `Jkind_desc.sub` — `sub_or_error` still calls the legacy
path for error text (M5), and the `-no-ikinds` fallbacks remain (S9). S2 is a
pure default-path verdict migration.

### S3 — classification coexistence differential (switch deferred to M5)
DIFFERENTIAL ONLY, zero behaviour change. In `sub_or_intersect`'s layout-FAIL
branch the legacy `Jkind.sub_or_intersect` can only return Disjoint/May (never
Sub — layouts are already incompatible), and its class is *exactly*
`may_have_intersection`. So the class migration is equivalent BY CONSTRUCTION;
the only real content is the failure-reason LIST, which feeds the error printer
(M5-owned). We keep returning the legacy classification and add a validate-gated
differential (`[ikind-soi-class]`) proving the ikind-native class
(`Jkind.may_have_intersection`) agrees with the legacy Disjoint/May, breaking
the count down by outcome. The actual switch (reason-list reproduction) rides
with the M5 project.

Evidence (`OXCAML_IK5D_MEASURE`, 9 seam dirs): `[ikind-soi-class]`
**checks=88831 disjoint=1466 may=87365 mismatches=0** — the ikind-native class
agrees with the legacy Disjoint/May on every layout-fail classification (the
1466/87365 split matches the pre-S2 baseline 1472/87469). Boot-green; ocamlformat
clean; behaviour unchanged (legacy value still returned) so suites are
byte-identical by construction.

### S4 — combine_histories coexistence differential (switch deferred to M5)
DIFFERENTIAL ONLY, zero behaviour change. `combine_histories`
(`jkind.ml`) uses legacy `Jkind_desc.sub` to order two histories
(Less→a, Not_le→b, Equal→higher-score). A future switch to the ikind sub is
zero-churn iff it selects the identical history on every combine. We add a
validate-gated differential comparing the legacy Less/Not_le/Equal to the
ikind verdict (via the `Jkind`→`Ikind` hook), counting agreement. The selected
history feeds error text, so the switch itself rides with M5.

**STATUS: DONE — S4 is DIVERGENT** (implemented by the S4 finisher; the
handoff spec is retained below for provenance). The differential is boot-green
with ZERO behaviour change (byte-identical suites), but the ikind-vs-legacy
history selection **DIVERGES corpus-wide (~57%)**, so the M4 switch is NOT
zero-churn and is routed firmly into the M5 error-text project. A divergent
differential is a valid wave-A result: it converts "is M4 a mechanical switch?"
from an assumption into a measured NO.

#### S4 as-built (differential — DIVERGENT)

Hook (`jkind.ml`, mirroring `set_floor_from_ikind`): `set_sub_verdict_from_ikind`
installs an `Ikind` function that, for two jkinds `a`/`b`, derives both ikind
polynomials in ONE scratch ctx (Normal mode) under `Ldd.with_isolated_pending`
and returns `Less`/`Equal`/`Not_le` via raw `Ldd.leq_with_reason` both directions
(`Less` = a≤b ∧ ¬b≤a, `Equal` = both, `Not_le` = ¬a≤b). `combine_histories`'
`choose_subjkind_history` computes the legacy `Jkind_desc.sub` result as before —
**it still chooses the history, behaviour UNCHANGED** — and, only under the gate
(`ikinds_validate || OXCAML_IK5D_MEASURE`), compares the ikind verdict to it,
counting checks/agreements/mismatches (`[ikind-combine-history]` at_exit summary;
each mismatch logs its `legacy=_ ikind=_` transition). Hot-path discipline: when
the gate is off, `ikind_verdict_of` returns `None` BEFORE the hook is called, so
the ~267k-call combine path never touches the ikind engine — confirmed by the
byte-identical suites below.

**Result (OXCAML_IK5D_MEASURE, 7 seam dirs — jkind-bounds, abstract-kinds{,
-missing-cmi}, layouts, layouts-or-null, layouts-products, modules): DIVERGENT.**
checks are order-of-100k (~154k by raw mismatch-line scale; ≥22.4k as a
distinct-process lower bound) with a **~57% mismatch rate corpus-wide**. Per dir:
jkind-bounds 32%, abstract-kinds 53%, layouts 73%, layouts-or-null 53%,
layouts-products 65%, modules 52% (abstract-kinds-missing-cmi 0%, but only 5
checks). The mismatch transitions are near-uniformly the **ikind verdict
collapsing to `Equal`**: `legacy=Not_le ikind=Equal` (95.8%), `legacy=Less
ikind=Equal` (4.2%), `legacy=Not_le ikind=Less` (4 total). I.e. the two combine
operands have EQUAL ikind denotations (mutually `leq`) while legacy
`Jkind_desc.sub` reports them incomparable or strictly-ordered.

**Robustness / not a harness artifact.** An alternate implementation routing the
two directions through the full `sub_jkind_l`/`compute_subcheck_polys` verdict
machinery (Rhs_top fast path + round-up) produced **byte-identical numbers**
(3176/356/2 on jkind-bounds either way), so the divergence is a genuine property
of the ikind polynomials, not an artifact of the poly-derivation choice. The
ikind derivation runs BEFORE the legacy `Jkind_desc.sub` (evaluated as the call
argument), so it is not a post-mutation effect either.

**Mechanism (hypothesis).** `combine_histories` orders the two INTERSECTION
OPERANDS (jkind.ml passes `t1`/`t2`, not the intersection result), which are
frequently incomparable AS KINDS — that is why they are being intersected — yet
share an ikind floor. Legacy `Jkind_desc.sub` here is on acknowledged-wrong
footing (the in-code CR: "this will be wrong if we ever have a non-trivial meet
in the kind lattice -- which is now! So this is actually wrong."). So the ikind
`Equal` is arguably the more-correct answer, but it still SELECTS A DIFFERENT
HISTORY than legacy on ~57% of combines, and the chosen history is displayed
error text.

**Consequence.** The M4 switch (combine_histories off legacy `Jkind_desc.sub`)
is NOT zero-churn and cannot be mechanical. It is routed into the M5 error-text
project, which must decide the history-ordering semantics (keep the legacy
ordering for display, or accept the ikind ordering and re-bless the ~57% of
error-text histories that change). **PAYOFF 1 (deleting legacy `Jkind_desc.sub`)
is therefore gated on M4's reconciliation inside M5, not on a clean S4.**

Non-vacuity: the differential fires on ~88k natural mismatches — strictly
stronger evidence that it catches a wrong history choice than an artificial seed;
no seeded fault was added (commits are seed-marker-free). Boot-green; ZERO suite
churn (typing-jkind-bounds 73/0, typing-layouts 45/0, typing-modules 54/0, before
== after); ocamlformat clean.

### S4 handoff spec (differential ONLY, zero behaviour change)

Goal: prove that switching `combine_histories`' history ordering from the legacy
`Jkind_desc.sub` to the ikind sub would select the IDENTICAL history on every
combine (which is the zero-churn condition, since the chosen history feeds error
text). Do NOT switch — only measure.

Site: `typing/jkind.ml`, `combine_histories` → `choose_subjkind_history`
(the `Jkind_desc.sub ~type_equal ~sub_previously_ran_out_of_fuel:roofdn_a
~context env k_a k_b` call, ~jkind.ml:3724). It maps `Less`→history_a,
`Not_le`→history_b, `Equal`→higher-scored.

Hook (mirror the existing `set_floor_from_ikind`/`set_render_from_ikind`
pattern): in `jkind.ml` add
`let sub_verdict_from_ikind : (Env.t -> _ jkind -> _ jkind -> Misc.Le_result.t)
option ref = ref None` + a setter. In `ikind.ml`'s init (where the other
setters are called) install a function that computes the ikind verdict for two
jkinds: build both sub polynomials and use `Ldd.leq_with_reason` both directions
→ `Less` (a≤b ∧ ¬b≤a), `Equal` (both), `Not_le` (¬a≤b). Reuse
`compute_subcheck_polys` / the `sub_jkind_l` leq machinery; run in an isolated
scratch ctx (`create_scratch_ctx` + `Ldd.with_isolated_pending`, the 5a helpers)
so the differential cannot perturb an outer solve — combine_histories runs
mid-check.

Differential: in `choose_subjkind_history`, gate on
`!Clflags.ikinds_validate || <measure-env>` (cheap-hatch, same
`OXCAML_IK5D_MEASURE` convention as S1-S3): call the hook, compare its
`Le_result.t` to the legacy `Jkind_desc.sub` result, and count
checks / agreements / mismatches (report which-transition on mismatch:
Less/Equal/Not_le a-vs-b). Add an `[ikind-combine-history]` at_exit summary. The
default path is UNCHANGED (still uses the legacy result to choose).

Acceptance: boot-green; run `OXCAML_IK5D_MEASURE` over the seam corpus →
checks>0, mismatches=0 (byte-identity of selection); behaviour unchanged so
suites are byte-identical by construction. If mismatches>0, that is the "history
selection diverges" case: DO NOT switch in the M5 project without reconciling —
record the divergent transitions here. Beware: combine_histories is HOT (~267k
calls, STAGE5D-MIGRATION.md), so the hook must be no-op-cheap when the gate is
off (guard the call itself, not just the counting).

## Deletability map (as amended)

| §5d target | deletable after |
|---|---|
| `Jkind_desc.sub` + `Mod_bounds`/`With_bounds` subsumption ops | S9 (delete `-no-ikinds` fallbacks) + M5 (sub_or_error + classification reason error-text + combine_histories history ordering) — S3 proves its seam switch-ready; **S4 is DIVERGENT (~57%)** so the history ordering must be reconciled in M5 before the switch |
| `Base_and_axes.normalize` + `Jkind.normalize` + `get_mode_crossing` | M5 (error-text + decl-norm + round_up migrated off normalize); S1 already moved crossing off it on the default path |
| `mod_bounds : Mode.Crossing.t` field | after normalize is gone |
| `-no-ikinds` surface + dual-engine differential | S9 (40 twin tests) |

## Acceptance summary

- Per-commit boot-green; ocamlformat clean on every touched enforced file.
- S1: crossing-reroute 234/0 (validate); suites 73/45/54.
- S2: overturn 0 corpus-wide (0 natural rejects — accept-path seam);
  seeded-fault non-vacuous (8/8); suites 73/45/54.
- S3: classification differential green — checks=88831 disjoint=1466 may=87365
  mismatches=0; zero behaviour change.
- S4: DONE but **DIVERGENT** — differential green + zero behaviour change, but
  ikind-vs-legacy history selection diverges ~57% corpus-wide (ikind collapses to
  `Equal`; robust across two implementations). M4 switch routed into M5 (not
  zero-churn); PAYOFF 1 gated on that reconciliation. Suites 73/45/54; boot-green.
- Wave A frozen for adversarial review; NOT pushed. Physical legacy-engine
  deletion (PAYOFF-1..4) queued for the user (S9 + M5).
