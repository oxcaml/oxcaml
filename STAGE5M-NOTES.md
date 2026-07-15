# Stage 5m notes: physically delete the legacy jkind engine (as-built)

Campaign: ikind-unification. Branch `ik/legacy-deletion` off `3f936718a`
(wave-A head). AS-BUILT record for the legacy-deletion finale. Plan of record:
`STAGE5M-PLAN.md`. Discipline: error-text churn promoted only when reviewed
correct-or-better (no garbage errors) + tallied here; non-error tests ZERO churn.

## M4 — combine_histories selects on the ikind verdict (DONE)

`combine_histories`/`choose_subjkind_history` (`typing/jkind.ml`) chose the
displayed history from the legacy `Jkind_desc.sub` verdict (`Less`→a, `Not_le`→b,
`Equal`→higher-scored). Switched the SELECTOR to the ikind sub verdict (the S4
hook `sub_verdict_from_ikind`, now always-computed — the validate/measure gate on
the selection path dropped; the wave-A `[ikind-combine-history]` differential
counters + at_exit removed). Legacy `Jkind_desc.sub` remains only as the `None`
fallback (hook unset / derivation raises); that fallback dies at FINAL.

### Sub-fix: `score_reason` de-prioritizes "notify-JS" placeholder histories

First cut surfaced GARBAGE reasons in ~5 errors: the ikind verdict collapses to
`Equal` ~57% (S4), routing to `choose_higher_scored_history`, whose `score_reason`
ranked internal/dummy histories (`it's a fresh unification variable`, `it's
assigned a dummy kind that should have been overwritten` — both print "Please
notify the Jane Street compilers group if you see this output") at par with real
reasons. That violates the "no garbage errors" discipline. Fix: `score_reason`
(used ONLY in the `Equal` tiebreaker, `typing/jkind.ml`) now scores the full
notify-JS set at `-2` (below `Concrete_creation`'s `-1`), so a real reason always
wins: `Any_creation {Initial_typedecl_env, Wildcard, Unification_var, Dummy_jkind,
Type_expression_call}`, `Scannable_creation Dummy_jkind`, `Value_or_null_creation
Probe`, `Value_creation {Row_variable, Tfield, Tnil, Debug_printer_argument,
Unknown _}`. This is a genuine improvement (these histories carry an
in-code "improve or remove" CR) and eliminated all 5 garbage regressions.

### Churn tally (promoted; all reviewed correct-or-better)

Suite churn after M4 + the score_reason fix, over the seam corpus (default
ikinds-on path). **Every changed line is a `because <reason>` history line inside
an `[%%expect]` Error block — zero non-error churn, zero garbage reasons
(grep-verified).**

| dir | tests changed |
|---|---:|
| typing-jkind-bounds | 1 |
| typing-layouts | 8 |
| typing-layouts-or-null | 6 |
| typing-layouts-products | 3 |
| typing-abstract-kinds | 1 |
| typing-layouts-err-msg | 1 |
| typing-modules | 0 |
| **total** | **20** (25 before the score_reason fix; 5 were garbage, now clean) |

20 files, 51 insertions / 59 deletions. Representative before → after:
- `because we must know concretely how to {pass a function argument, return a
  function result}.` → `because argument or result of a function type.` (the
  `Inside_of_Tarrow` history; the ikind selector prefers it — more concise, same
  meaning).
- `because it instantiates an unannotated type parameter of t, chosen to have
  layout value.` → `because it's the type of {the field of a polymorphic variant,
  a tuple element}.` (names the concrete source instead of the derived
  instantiation).
- `because the type argument of list has kind value_or_null.` → `because of the
  annotation on the universal variable 'a.` (points at the annotation).
- `because of the definition of t at line 4 ...` → `... at line 2 ...` (a
  different but valid definition-site provenance for the same layout requirement).

All name the right axes/types. No `[%%expect]` block that was previously a
non-error output changed; the diff is confined to error-message history lines.

Broad regression sweep (typing-modal-kinds, typing-modes, typing-gadts,
typing-recmod, typing-misc, typing-abstract-kinds-missing-cmi): 0 churn — the
only additional churn was typing-layouts-err-msg (1, same `Inside_of_Tarrow`
reason). Boot-green (`make boot-compiler`). Files: `typing/jkind.ml` + 20
testsuite `[%%expect]` promotions.

### Perf note (measure at exit-gate)

`combine_histories` is hot (~267k calls corpus-wide, STAGE5D §3). M4 makes the
ikind verdict (scratch ctx + two `ckind_of_jkind` derivations + `solve_pending` +
two `leq_with_reason`) run on every combine where it previously ran only under
the validate/measure gate. This is forced by the deletion (legacy `Jkind_desc.sub`
is going away), so there is no cheaper option; flagged for the STAGE2-PERF exit
row, not a per-slice blocker.

## M5 — error-text off legacy (sub_or_error + sub_or_intersect) (DONE)

Dropped the ikinds-path legacy error-text calls at both seams (`typing/ikind.ml`);
the ikind engine now owns the verdict AND the message. Only the two `-no-ikinds`
fallback calls to `Jkind.sub_or_error`/`sub_or_intersect` remain (removed in S9).

- **sub_or_error reject**: was `match Jkind.sub_or_error ... with Error e -> e |
  Ok () -> <overturn synth>` (legacy produced the message; the "overturn" branch
  already synthesized ikind-natively). Now mirrors `sub_jkind_l`: layout check
  first (on failure return that layout `Violation`), else `Ldd.leq_with_reason`
  violating axes → `axis_disagreement_reasons` → `Not_a_subjkind`. Legacy call,
  overturn detector, accept-side debug dual-run, and seeded fault all removed.
- **sub_or_intersect layout-fail**: was `let legacy = Jkind.sub_or_intersect
  ...` returning the legacy Disjoint/May class + reason list. Now classifies via
  `Jkind.may_have_intersection` (a `Base` op — KEPT) and builds the reason list
  ikind-natively (`[Layout_disagreement]`, the honest reason for a
  layout-incompatible pair). Legacy call + the S3 differential removed.
- Retired the now-dead wave-A S2/S3 telemetry: `sub_or_error_{rejects,overturns,
  fault,measure}`, `soi_class_*`, and their two `at_exit` summaries. (The
  §D-permanent validate/recompute/seeded-fault infra is untouched.)

**Churn: ZERO** across the 10-dir corpus (jkind-bounds, layouts, modules,
layouts-or-null, layouts-products, abstract-kinds, layouts-err-msg, gadts, modes,
modal-kinds — all 0 failed / 0 unexpected errors). The ikind-native errors are
byte-identical to the pre-M5 (post-M4) text: `sub_jkind_l` was already the
primary ikind-native error path, so routing these two seams through the same
`Violation.of_ (Not_a_subjkind ...)` / `may_have_intersection` machinery changed
no observable output. Only `typing/ikind.ml` modified; boot-green.

Note: `Violation.of_`'s `normalize ~mode:Require_best`-for-display is UNCHANGED
(it is coupled to the byte-identical decl-normalize path); its migration is
deferred to S5-S7 per the plan.

### Perf observation (M4, to quantify at exit-gate)

The M5 corpus run was markedly slower than baseline (typing-layouts alone ran
several minutes vs ~1 min). Consistent with M4 making the ikind sub-verdict
derivation run on every `combine_histories` call (~267k corpus-wide). Flagged for
the STAGE2-PERF exit row; the deletion forces it (no cheaper option), so it is
not a per-slice blocker, but it should be measured and reported.

## S9 — delete the -no-ikinds flag surface + 40 twin tests (DONE)

The ikind engine is now the sole kind checker; there is no legacy fallback path.

Flag surface removed: `Clflags.ikinds` (utils/clflags.ml + clflags.mli), `-no-ikinds`
(driver/main_args.ml mk+val+4 wirings+impl, main_args.mli, compenv.ml). KEPT
`-ikinds-debug`, `-print-from-ikinds`, `OXCAML_IKINDS_VALIDATE`.

Flag-branch collapses (`if not !Clflags.ikinds then <legacy> else <ikind>` →
`<ikind>`; `when !Clflags.ikinds && X` → `when X`):
- ikind.ml: `with_ikinds_enabled` (was the "ikinds disabled" sentinel),
  `type_declaration_ikind` 3 stored-ikind match guards, `mod_bounds_floor_for_printing`,
  `render_jkind_from_ikind` (now gated on `print_from_ikinds` alone),
  `sub_verdict_for_history`, `crossing_of_jkind`, `sub_jkind_l`, `sub_or_intersect`,
  `sub_or_error`.
- ctype.ml `crossing_of_ty`; jkind.ml `Violation.report_general` (dropped the
  `if not !Clflags.ikinds then report_fuel` call — dead on the ikinds path).
- Consequences: `type_equal` is now unused in `sub_jkind_l`/`sub_or_intersect`/
  `sub_or_error` (the legacy fallback that used it is gone) — kept the labeled
  param (interface-fixed) as `~type_equal:(_ : ...)`; the vestigial param can be
  dropped in a later cleanup. Deleted the now-unused `Violation.report_fuel` +
  `report_fuel_for_type` (legacy fuel-note machinery, only the flag-off path
  called it).

Twin tests: all 40 `-no-ikinds` files DELETED (each has a `_ikinds` twin covering
the same source under the ikind engine — verified 40/40 have the sibling).
Dirs: typing-jkind-bounds 18 top-level + subsumption 8 + inclusion/poly-variant-limit/
unsafe-across-files 1 each + regression-tests 3; typing-abstract-kinds 4 +
missing-cmi 1 + tool-ocamldep 1; typing-layouts-or-null 2.

The `-no-ikinds`-vs-default dual-engine differential is thereby retired (no
second engine to compare). The §D-permanent OXCAML_IKINDS_VALIDATE
recompute-from-`with_bounds` harness + seeded-fault infra are UNTOUCHED (they do
not depend on `-no-ikinds`).

Verification: boot-green. All affected dirs 0 failed / 0 unexpected errors:
typing-abstract-kinds 4, missing-cmi 1, tool-ocamldep 1, jkind-bounds 41
(+ subsumption 8, inclusion 1, poly-variant-limit 1, unsafe-across-files 1,
regression-tests 4), layouts-or-null 33, layouts 47, modules 54. Deleted twins
cleanly gone (no unknown-flag errors); zero churn in surviving tests.

Note: `external/merlin/...` keeps its own vendored `-no-ikinds` recognition
(out of scope — vendored copy, updated separately); harmless (merlin accepts it
as a no-op in its own arg parser).

Still standing after S9 (removed at FINAL): the `Jkind.sub_or_error`/
`sub_or_intersect`/`sub_jkind_l`/`get_mode_crossing` legacy functions (now
uncalled except the S1 crossing validate tripwire), `Jkind_desc.sub` (M4 None
fallback), the legacy subsumption ops.

## Stage-5m PERF FIX — defer combine_histories ordering to display (DONE)

The M4/M5 perf observations above are resolved. M4 made the ikind sub-verdict
(scratch ctx + two `ckind_of_jkind_desc` derivations + `solve_pending` + two
`leq_with_reason`) the history-ordering SELECTOR, run on EVERY `combine_histories`
call (~267k corpus-wide) — where the baseline flattened path had used the cheap
legacy `Jkind_desc.sub`. That is the regression (typing-layouts ~1min → several
minutes).

**Fix (option 1, deferral).** `combine_histories` (`typing/jkind.ml`) now only
records both histories in an `Interact` node (canonical `k1,k2` order) and does
nothing else — zero ikind work on the hot path. `resolve_flattened_history`
reproduces the eager selection LAZILY, only when a history is actually formatted
into an error (cold path): it re-derives the orientation (`try_allow_l/r` on the
stored descs), runs the ikind sub-verdict for the oriented pair, and applies the
`score_reason` tiebreak — matching the eager code's choice exactly (incl. the
tie-preference of `choose_higher_scored`'s first arg, and the score over already
-resolved sub-histories). A `history1 == history2` fast path skips the verdict.
The three history consumers resolve first: `format_flattened_history` (via
`is_informative` on the resolved history), and the `Violation.report_general`
`Missing_cmi` hint. The `Ikind` verdict hook (`set_sub_verdict_from_ikind`) now
takes jkind **descs** (all the derivation uses — `ckind_of_jkind` was just
`ckind_of_jkind_desc jkind.jkind`), which is what the `Interact` node carries;
`~context` was already ignored and is dropped.

The first cut deferred only orientations 1/2 and resolved the orientation-3
(both operands carry `with_bounds` → neither `try_allow` pairing holds) score
tiebreak EAGERLY in `combine`. That still ran the verdict on the hot path for the
with-bounds-heavy typing-layouts corpus (measured +31%). Moving orientation-3
into the resolver too (defer everything) closed it.

This also pre-implements FINAL's None-fallback re-route: no per-combine
`Jkind_desc.sub` call remains.

**A/B wall time** (`make -s test-one DIR=…`, 3 runs, median; same box + method;
install overhead included in both), fix-head (51f2eacb1) vs base 3f936718a:

| dir | base | fix | Δ |
|---|---:|---:|---:|
| typing-layouts | 77.4 / 77.2 / 77.3 → 77.3s | 77.8 / 77.7 / 77.6 → 77.7s | +0.5% |
| typing-jkind-bounds | 52.1 / 52.6 / 52.3 → 52.3s | 35.3 / 35.5 / 35.5 → 35.4s | −32% (*) |

(*) Review amendment A1: the −32% is substantially a test-count artifact — S9
deleted 32 jkind-bounds twins (73→41 tests), so wall-time drops from both fewer
tests and the deferral. The clean same-test-set cell (typing-layouts) shows the
deferral itself is perf-neutral (+0.5%, within noise); the ~1% perf constraint
is satisfied either way.

typing-layouts is within noise of baseline; typing-jkind-bounds is markedly
FASTER than base (deferral skips the per-combine legacy sub the baseline ran).
(The rejected orientation-1/2-only first cut measured typing-layouts 100.4 /
101.8 / 101.0 → 101.0s, +31% — recorded for the perf story.)

**Churn: ZERO.** 10-dir corpus (typing-layouts, -err-msg, jkind-bounds,
layouts-or-null, layouts-products, abstract-kinds, modules, modal-kinds, modes,
gadts): 0 failed / 0 unexpected. The deferred resolution selects the identical
histories as the eager M4 code, so M4's 20-test churn set is UNCHANGED and no new
churn appears. Non-error output byte-identical (history is error-only display).
boot-green + install-green. Files: `typing/jkind.ml`, `typing/jkind.mli`,
`typing/ikind.ml`.

## FINAL — sub-chain deletion (DONE)

With the perf fix already dropping the last per-combine `Jkind_desc.sub` caller
(the None-fallback re-route), the whole legacy subsumption chain is uncalled and
deleted. Verified dead by census (no live caller; grep-clean residue after) —
every remaining reference to a deleted name is a comment, and the one misleading
comment (`ctype.ml`: "falls back to `Jkind.sub_or_error` when ikinds are
disabled") was corrected.

**Deleted (`typing/jkind.ml`):** `Jkind_desc.sub`, `Jkind_desc.sub_expanded`,
`Base.sub_expanded` (both `sub_expanded`s were used only by the dying chain),
`check_sub`, `sub_with_reason`, `sub`, the `sub_or_intersect` FUNCTION,
`sub_or_error`, `sub_jkind_l`, `get_mode_crossing`, and
`With_bounds.to_best_eff_map` (its sole caller was `sub_jkind_l`).
**Deleted (`typing/jkind.mli`):** the matching `val`s (`sub`, `sub_or_intersect`,
`sub_or_error`, `sub_jkind_l`, `get_mode_crossing`).
**Deleted (`typing/ikind.ml`):** the S1 legacy-crossing validate tripwire inside
`crossing_of_jkind` (the `Jkind.get_mode_crossing` re-assertion) + the
`crossing_reroute_{checks,mismatches}` counters and their `at_exit` summary;
`crossing_of_jkind`'s now-unused `~context` is ignored.

**KEPT (confirmed still live, contra the original STAGE5D "delete the ops"):**
`type sub_or_intersect` (`Jkind` + the `Ikind` alias); `sub_layout_or_error`
(`ikind.ml` ×3 + `typedecl.ml`); the lattice ops `Mod_bounds.{meet,
less_or_equal}` and `With_bounds.{meet,join}` (live intersection/normalize infra
— `Jkind.intersection` ← `ctype.ml`, and `normalize`); `may_have_intersection`;
and `set_sub_verdict_from_ikind`/the verdict hook (the perf-fix resolver calls
it — do NOT delete, contra the old FINAL-a note written under the eager design).

Net ~294 lines deleted (jkind.ml −219, jkind.mli −46, ikind.ml −30). Pure
dead-code removal: no live logic changed. **Churn: ZERO** across the 10-dir
corpus (0 failed / 0 unexpected each). boot-green + install-green.

## S5 round_up — VERDICT: stays with normalize, rides with S7 (no code change)

`Jkind.round_up` (`jkind.ml`) is `normalize ~mode:Ignore_best` then strip
with-bounds, returning a `jkind option`. Census of its 3 `ctype.ml` callers:

- **`nondep_type_decl` fallback (ctype.ml:8433).** When `nondep_type_rec` cannot
  erase a type out of the jkind's with-bounds (covariant case), it rounds up to
  drop the un-erasable with-bound, and the result becomes the reconstructed
  declaration's **`type_jkind = jkind`** (ctype.ml:8450). That stored jkind is
  what `-i`/`.mli`/decl printing renders → **byte-identical constraint (2)
  applies, NOT waived**. Its product is a printable decl jkind, not a
  verdict/floor, so it is NOT migratable to a bare `Axis_lattice.t` ikind floor.
  **Decl-print-coupled → rides with S7.**
- **`intersect_type_jkind` (ctype.ml:3589, 3590).** Rounds both operands up
  (strip with-bounds) then feeds `Jkind.intersection` — a semantic
  (verdict/bound-shaped) use, not printed. These *could* take a floor, but they
  call the **same** `Jkind.round_up`, which must stay for the 8433 decl-print
  site regardless. Forking round_up to migrate only 2 of 3 sites yields no
  deletion benefit (normalize keeps its round_up caller either way) and would be
  pure churn.

Therefore S5 makes NO code change: `round_up` (and thus its `normalize` caller)
stays. It is one more `normalize` consumer that the **S7 decl-normalize
differential must cover** (alongside `normalize_decl_jkinds` and `Violation.of_`).
If S7's ikind-derived normalized decl jkind is proven byte-identical corpus-wide,
`round_up` migrates to the ikind path with normalize; if S7 diverges, `round_up`
keeps `normalize` as part of the surviving decl-print sidecar. Either way S5 does
not gate anything on its own.

## S7 decl-normalize differential — VERDICT: DIVERGENT (USER DECISION)

**Result: DIVERGENT. `normalize` STAYS for the decl path. PAYOFF 2 + PAYOFF 3
NOT taken. S8 skipped. This is a USER-DECISION item — no waiver is assumed.**

**Differential.** The ikind-derived decl-jkind rendering already exists behind
`-print-from-ikinds` (env `OXCAML_PRINT_FROM_IKINDS`, runtime-toggleable, no
rebuild): `render_jkind_from_ikind` + `mod_bounds_floor_for_printing` install
into `Jkind.Const.{render,floor}_from_ikind`, which `Jkind.Const.convert` calls
FIRST (falling back to legacy when they return `None`). So running the corpus
flag-ON vs flag-OFF is a faithful, corpus-wide, print-path differential of the
ikind-derived normalized decl jkind against legacy `Jkind.normalize
~mode:Require_best` — exactly what `-i`/`.mli`/decl/error printing renders.
Non-vacuous by construction (it detected many real divergences below; the
floor path also has the `print_floor_fault` seed).

**Measured divergence (tests whose printed kind changes flag-ON):**

| dir | diverged / total |
|---|---:|
| typing-jkind-bounds | 27 / 41 |
| typing-layouts | 1 / 45 |
| typing-layouts-products | 1 / 35 |
| typing-layouts-or-null | 2 / 30 |
| typing-modes | 2 / 36 |

Divergence is concentrated exactly in **with-bounds decls** (jkind-bounds, which
is dense with them, diverges 66%; the with-bounds-sparse dirs barely move).

**Divergence class (structural, not cosmetic).** The ikind engine represents
with-bounds as an **LDD polynomial over rigid names + raw coefficient vectors**,
having discarded the `type_expr` with-bounds surface form that legacy `normalize`
keeps. Representative (verbatim, `annots_ikinds.ml` / seam):

- legacy `The kind of type "t" is immutable_data with int`
  → ikind `... is value non_float` (a **different normal form** — the with-bound
  is folded into a different base abbreviation)
- legacy `... immutable_data with int`
  → ikind `... immutable_data with t_value/676.0 @ [0,0,1,3,3,1,1,3,3,0,0]`
  (raw LDD algebra: rigid-name identity `t_value/676`, coefficient index `.0`,
  and a raw `Axis_lattice` coefficient vector).

This is not a rendering-syntax nit fixable by a prettier printer: (a) the raw
rigid-name/coefficient form has no `type_expr` to reconstruct the legacy `with
<type>` surface from, and (b) the two engines choose **different normal forms**
(`value non_float` vs `immutable_data with int`). Byte-identity would require the
ikind path to replicate legacy `normalize`'s exact canonicalization — i.e.,
re-implement `normalize` — which defeats the deletion. The
`render_jkind_from_ikind` code comment already states it "intentionally diverges
from legacy surface syntax."

**Consequence (per the plan's DIVERGENT branch).** `Base_and_axes.normalize`
fixpoint + `Jkind.normalize` + `Violation.of_`'s `normalize ~mode:Require_best`
for-display STAY: they are the authoritative decl-print path (constraint (2),
byte-identical, NOT waived). `round_up` (S5) stays with them. **S8 (PAYOFF 3,
field relabel) is SKIPPED** — it was gated on S7 green. The surviving
`normalize` and its lattice-op users (`Mod_bounds.{meet,less_or_equal}`,
`With_bounds.{meet,join}`, `Jkind_desc.intersection`) are the permanent
print/intersection sidecar.

## PAYOFF ledger (final, honest)

- **PAYOFF 1 (delete the legacy sub chain): DONE** — perf slice + FINAL slice.
- **PAYOFF 4 (delete `-no-ikinds` surface + 40 twins + dual-engine
  differential): DONE** — S9 (predecessor).
- **PAYOFF 2 (delete `Base_and_axes.normalize` fixpoint + `Jkind.normalize`):
  NOT TAKEN** — S7 differential DIVERGENT (structural; decl-print byte-identity
  unachievable without re-implementing normalize). USER DECISION.
- **PAYOFF 3 (`mod_bounds : Mode.Crossing.t` → `ikind_floor : Axis_lattice.t`):
  NOT TAKEN** — S8 gated on S7; skipped.
- **PERF: within baseline** — deferral (perf slice); typing-layouts +0.5%,
  jkind-bounds −32% vs base 3f936718a (see A1 note above: substantially a
  test-count artifact from the S9 twin deletion; deferral itself perf-neutral).

## CI round — promote M4 history churn in 13 typing-layouts subdirs (DONE)

First CI run of the pushed branch (PR #6460) was RED: the build workflow's
"Run upstream testsuite" step failed (every build variant fails identically — a
config-independent expect diff). Fetched the failing-test list from the CI job
log (`gh api .../jobs/<id>/logs`): 20 tests across 13 `typing-layouts-*` subdirs
OUTSIDE the predecessor's M4 seam corpus and my 10-dir sweep. (The CI log also
*appeared* to implicate `win-unicode` and `typing-abstract-kinds`, but that was
parallel-log interleaving — both 0-fail locally; the real
`abstract_kinds.ml`/`non_pointer.ml` failures are in `typing-layouts-scannable`.)

Reproduced locally at HEAD (after a clean rebuild — the box's `_install` had been
left at base by the 5e agent); all 20 confirmed. **Every diff is the M4
history-selection class** — the ikind verdict picks a different but valid
`because <provenance>` history line inside an `[%%expect]` Error block; **zero
non-error changes, zero notify-JS garbage** (grep-verified). Promoted via `make
promote-one`; re-ran all 13 dirs → 0 failed / 0 unexpected.

Failing files (20): typing-layouts-{arrays[basics,basics_alpha], bits8[basics],
bits16[basics], bits32[basics,basics_alpha], bits64[basics,basics_alpha],
block-indices[basics_block_indices], float32[basics], float64[basics],
iarrays[basics], scannable[abstract_kinds,non_pointer],
untagged-immediate[basics,basics_alpha], vec128[basics,basics_alpha],
word[basics,basics_alpha]}. Diff stat: 20 files, +34 / −38.

Churn classes (added history line ×count; all correct-or-better, same classes as
the M4 tally, just in the wider testsuite):

| new `because …` | ×  | replaced (legacy) |
|---|--:|---|
| it's the type of the recursive variable x. | 15 | it's the type of an array element. |
| argument or result of a function type. | 14 | it's the type of a variable captured in an object. (Inside_of_Tarrow) |
| of the annotation on the universal variable 'a. | 2 | of the definition of a at line 3, chars 2-28/2-30. |
| it's the type argument to the array type. | 2 | it's the layout polymorphic type in an external declaration ([@layout_poly] …). |
| of the definition of t at line 2, chars 2-29. | 1 | of the definition of t at line 4, chars 2-39. |

Commit `ci: promote M4 history churn …`. Compiler unchanged (only testsuite
`[%%expect]` references updated); boot-green. NOT pushed.

## Freeze status (this PR, frozen for adversarial review)

Commits on `ik/legacy-deletion` this finale added on top of the predecessor's
M4/M5/S9 (52ac66c16):
1. `perf: defer combine_histories ordering to display` (jkind.ml/.mli, ikind.ml)
2. `FINAL: delete the legacy sub chain` (~294 lines, jkind.ml/.mli, ikind.ml,
   ctype.ml comment)
3. `S5 round_up verdict` (notes)
4. this S7-verdict + freeze notes update

Per-commit boot-green + install-green; the 10-dir corpus is 0 failed / 0
unexpected at HEAD (pure-deletion + deferral, no live-logic change → zero churn;
M4's 20-test error-text churn set unchanged). `OXCAML_IKINDS_VALIDATE`
recompute-from-`with_bounds` harness + seeded-fault infra UNTOUCHED (only the
dead S1 crossing-reroute tripwire/counters were removed). Validate spot-check
(typing-jkind-bounds, `OXCAML_IKINDS_VALIDATE=1`): **`mismatches=0`** over 135
recompute checks, and no kind-change diffs — the 2 test "failures" under validate
are the pre-existing `[ikind-validate] summary:` stderr line, not a regression.
NEVER pushed.
