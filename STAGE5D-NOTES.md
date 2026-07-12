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
behind `-no-ikinds`, the M5 error-text call, and validate tripwires — and
machine-checked proofs that the remaining two seams are switch-ready.

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

**STATUS: NOT DONE — frozen for a fresh finisher (handoff spec below).** S1-S3
are banked and verified; S4 was deliberately deferred at this point because it
is the highest-error-surface slice (a new cross-module hook + a hot path) and
this implementation session had run very long — per the team-lead's
freeze-when-degrading guidance, banking S1-S3 and handing S4 off loses nothing
(it is a self-contained differential).

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
| `Jkind_desc.sub` + `Mod_bounds`/`With_bounds` subsumption ops | S9 (delete `-no-ikinds` fallbacks) + M5 (sub_or_error + classification reason error-text) — the S3/S4 differentials prove the seams switch-ready |
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
- S4: NOT DONE — frozen for a fresh finisher (handoff spec above); self-contained
  differential, no loss from the S1-S3 commits.
- Wave A frozen for adversarial review; NOT pushed. Physical legacy-engine
  deletion (PAYOFF-1..4) queued for the user (S9 + M5).
