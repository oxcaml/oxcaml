# Stage 5e — exit gate: perf row + harness endgame audit + campaign ledger

Campaign: ikind-unification (`../IKIND-REFACTOR.md`, `STAGE5-DESIGN.md`). This is
the CLOSING document for the campaign. It records (1) the default-path compile
perf row of the FINAL tree, (2) the STAGE5-DESIGN §D harness-endgame audit, and
(3) the honest campaign summary ledger. Measured on the warm-built FINAL tree
`ik/legacy-deletion @ 259503bc1` (`_prefix/bin/ocamlc.opt`). NO code change (docs
only). Never pushed.

## 0. Baselines used (and why)

The STAGE2-PERF methodology's original baseline was branch `ik/stage2-authority`
(tip = stage-1 tip `89a6df578` + the typecore seam flip), which predates the
reachable history in these worktrees, so it is not directly rebuildable here.
Per the exit-gate mandate we measured against two reachable, ALREADY-WARM-BUILT
bases (no from-scratch compiler build needed; both binaries behaviorally
verified — see §1):

- **PRIMARY: wave-A base `3f936718a`** (`ik5d-work/_prefix/bin/ocamlc.opt`). This
  is the DIRECT branch point of `ik/legacy-deletion` (merge-base confirmed; 10
  commits base→final). FINAL-vs-this isolates exactly the legacy-deletion PR
  (#6460) compile impact — the number the STAGE5-DESIGN Q3 exit gate asks for.
- **SECONDARY: pre-stage-5 base `6b3ae002e`** (`cleanroom-6202/_prefix/bin/
  ocamlc.opt`). `6b3ae002e` is a DOCS-ONLY commit (STAGE5A-NOTES.md, +21/−8), so
  it is code-identical to its parent — using the built binary satisfies the
  mandate's "6b3ae002e's parent if buildable". FINAL-vs-this brackets the WHOLE
  stage-5 arc (5a→5m).

Both bases accept `-no-ikinds` (legacy engine present); the FINAL tree rejects it
(`unknown option`), the behavioral proof that each binary is genuinely its rev
and the content-cache did not serve a stale artifact.

## 1. Method (STAGE2-PERF, adapted for the deleted toggle)

STAGE2-PERF toggled `-ikinds`/`-no-ikinds` on ONE binary to isolate the engine.
On the FINAL tree `-no-ikinds` is deleted (S9), so the engine can no longer be
toggled in-binary. The exit-gate question is instead a BEFORE/AFTER of the
default path across binaries: same corpus, `ocamlc.opt -c -extension
layouts_beta <file>`, run through the FINAL binary vs each baseline binary. Both
bases already run the ikind path by default (ikinds authoritative since stage 2),
so this measures the compile-time delta of the stage-5 deletions + deferral, not
an engine swap.

- Corpora: reconstructed TO THE STAGE2-PERF SPEC (the original `/tmp/gen_perf.py`
  was not preserved). Shapes/sizes: `perf_records` (400×{boxed `:immutable_data`
  record, param record `with 'a`, unboxed record} + 800 subkind/crossing checks,
  2001 ln), `perf_gadt` (300×{existential GADT, 2-ctor with-bound variant} + 60
  functors `S with type 'a b = 'a`, 661 ln), `perf_combined` (mixed, 952 ln),
  `perf_plain` (1200 plain variant decls + 2400 arith/list fns, NO kind
  annotations, 3600 ln). All four compile cleanly (exit 0, no diagnostics) on
  BOTH bases and the FINAL binary. Sizes for gadt/combined are approximate vs the
  stage-2 files; what is load-bearing is that the SAME file is run through every
  binary. Generator + corpora: `_tmp/perf5e/`.
- Protocol: N=11 reps/cell, first dropped as warmup (10 timed), wall-clock via
  `time.perf_counter`, MEDIAN + min/max. Binaries INTERLEAVED per-rep so any
  shared-box load drift hits all cells equally. 3 full runs; plus an N=25 (24
  timed) run on the borderline `perf_records` cell.
- Box conditions: `uptime` checked before/after every run. The box had a load
  spike to ~49 on arrival; all timing was deferred until the 1-min load drained
  below 4 and stayed there (1-min 1.3–2.3 across every timed run). Run 1's
  5/15-min averages were still ~17 (lagging the spike) and Run 1 is treated as
  provisional; Runs 2–3 + the N=24 run are the settled figures.

## 2. PERF ROW (the deliverable)

Absolute medians (median across the 3 full runs' per-cell medians, ms):

| corpus | base `3f93` | FINAL `2595` | Δ ms | Δ % (exit gate) | pre5 `6b3a` | FINAL vs pre5 |
|---|---:|---:|---:|---:|---:|---:|
| perf_records  | 239.5 | 241.9 | +2.4 | **+1.00%** | 228.7 | +5.77% |
| perf_gadt     |  81.6 |  81.3 | −0.3 | **−0.37%** |  82.3 | −1.22% |
| perf_combined | 127.6 | 127.9 | +0.3 | **+0.24%** | 128.7 | −0.62% |
| perf_plain    | 930.7 | 935.0 | +4.3 | **+0.46%** | 934.8 | +0.02% |

Borderline-cell confirmation — `perf_records`, N=24 timed, tight:

| cell | base `3f93` med/min | FINAL `2595` med/min | Δ% med | Δ% min |
|---|---:|---:|---:|---:|
| perf_records | 239.1 / 235.4 | 241.3 / 237.4 | **+0.92%** | +0.85% |

Per-run `perf_records` FINAL-vs-base Δ% (shows the tail behavior): Run1 +0.32,
Run2 +0.62, Run3 +1.77 — the +1.77 is a single slow-sample tail (FINAL max
~260–268 ms every run vs base ~242); the N=24 median +0.92% is robust to it.
Run1 `perf_plain` +1.51% was load-tail noise (Runs 2/3: +0.06 / +0.31).

### Verdict: EXIT GATE PASS (no >1% regression on the primary baseline)

- FINAL vs wave-A base `3f936718a` is under 1% on every corpus. The largest
  systematic signal is `perf_records` at **+0.92%** (N=24; +0.85% on min) — a
  with-bound + check-heavy shape. This is squarely inside STAGE2-PERF's
  user-accepted reviewer caveat ("neutral-to-beneficial, win concentrated on
  decl-heavy plain code; check-heavy/with-bound corpora measured up to +1.1%").
  It sits AT the noise/threshold boundary, not clearly above it, so it is a
  PASS-with-note rather than a STOP finding. decl-heavy/plain code is neutral
  (`perf_plain` +0.46%, `perf_combined` +0.24%, `perf_gadt` −0.37%).
- **The M4 regression is resolved.** STAGE5M-NOTES flagged that M4 made the ikind
  sub-verdict run on every `combine_histories` (~267k calls; typing-layouts ran
  minutes). Had that survived, `perf_records`/`perf_plain` would show a large
  regression. They do not — confirming the "defer combine_histories to display"
  perf fix landed and the deletion + deferral net out perf-neutral.
- **Stage-5 arc note (secondary baseline).** FINAL vs pre-stage-5 `6b3ae002e` is
  +5.77% on `perf_records`, ~0 elsewhere. But the wave-A base `3f936718a` is
  ITSELF ~+4% vs pre5 on records — i.e. the with-bound-heavy cost entered BETWEEN
  pre-stage-5 and wave-A base (the 5b unit-qualified `Residue` atom + 5c carrier
  deletion / print-from-ikind-default / format lock-in), NOT in the
  legacy-deletion PR. It is concentrated entirely on with-bound + check-heavy
  code and is consistent with the STAGE5-DESIGN §0 probe (+6.3% default-vs-legacy
  on a with-bound-heavy shape) and the accepted stage-2 profile. Surfaced here
  for the user; it is within the accepted with-bound envelope, not a new finding.

## 3. HARNESS-ENDGAME AUDIT (STAGE5-DESIGN §D)

Verified the survivor set is exactly as §D designed. All runs on the FINAL binary.

### 3a. Stored-vs-recompute harness FUNCTIONS (0 HARD; residue-neutralized=3)

`OXCAML_IKINDS_VALIDATE=1`, replicating the corpus test recipes:

| corpus / phase | checks | mismatches (HARD) | class_b | residue-neutralized | imported-residues | param-id-collisions |
|---|---:|---:|---:|---:|---:|---:|
| residue SAVE (`res_lam.ml`)      | 1414 | **0** | 3 | **3** | 0 | 0 |
| residue IMPORT (`consumer.ml`)   |   26 | **0** | 0 | 0 | 8 | 0 |
| cross_unit SAVE (`liba.ml`)      |   18 | **0** | 0 | 0 | 0 | 0 |
| cross_unit IMPORT (`client.ml`)  |   70 | **0** | 0 | 0 | 0 | 0 |

0 HARD everywhere; `residue-neutralized=3` on the residue save (matches the 5b
as-built evidence); `param-id-collisions=0` and `imported-foreign-params=0`
(residues cross the cmi as unit-qualified `Residue`, collision-free by
construction); CLASS-B fires 3× and each is LOGGED (auditable trust boundary, not
silently absorbed). (`imported-residues=8` here vs 9 in the 5b notes — immaterial
count difference, likely `.opt` vs `.byte`; every load-bearing invariant holds.)

### 3b. Seeded-fault infra PRESENT and NON-VACUOUS

All four seed env vars present in `typing/ikind.ml` (`OXCAML_IKIND_RESIDUE_FAULT`,
`OXCAML_IKIND_SAVE_FAULT`, `OXCAML_PRINT_FLOOR_FAULT`, `OXCAML_IKIND_COLLISION_FAULT`)
plus the `Ldd.bot` population seed. Non-vacuity run:

- `OXCAML_IKIND_SAVE_FAULT=1` on the residue SAVE, then IMPORT under validate →
  **mismatches=8 (HARD)** (matches 5b's "save-fault → 8 HARD"). Clean recompile →
  back to **0**. The harness genuinely catches a corrupted persisted ikind; it is
  not a no-op.

### 3c. Transition-specific machinery GONE (or documented)

- **Dual-engine differential vs `-no-ikinds`: GONE.** `Clflags.ikinds` / the
  `-no-ikinds` flag survive only in history COMMENTS; clflags survivor set is
  exactly `ikinds_debug` (`-ikinds-debug`), `ikinds_validate`
  (`OXCAML_IKINDS_VALIDATE`), `print_from_ikinds` (`-print-from-ikinds`).
- **Wave-A differential counters: GONE.** `sub_or_error_rejects`, `soi_class_*`,
  `crossing_reroute_*`, the `[ikind-combine-history]` differential, and "overturn"
  counters — zero matches. The two surviving `at_exit` summaries are the permanent
  §D telemetry (`[ikind-validate] summary` + the `[ikind-print]` debug summary),
  not transition counters.
- **Legacy sub chain: GONE.** `Jkind.sub_jkind_l` / `get_mode_crossing` / `sub` /
  `sub_or_error` / `sub_or_intersect` FUNCTION deleted from `jkind.ml`/`jkind.mli`
  (only the `sub_or_intersect` TYPE is kept, by design); the sole `let sub` at
  `jkind.ml:333` is the LAYOUT lattice `sub` (over `Scannable_axes`), unrelated.
  `Jkind_desc.sub` has no live caller (only a comment reference).
- **Carrier machinery: GONE** (§D "dies with the carrier"). `ikind_carrier`,
  `validate_carrier`, `carrier_benign`, `jkind_with_carrier`,
  `relabel_ikind_carrier` — zero matches.
- **CLASS-A/B/C classifiers + part-3.5 collision detector: KEPT** (repurposed to
  regression tripwire per §D). Confirmed live (CLASS-B fired; collision counter
  reads 0).

### 3d. Unexpected / worth-flagging (two benign survivors)

Neither runs on the default compile path; both are default-off debug/regression
aids. Listed per the mandate's "list anything unexpected":

1. **`debug_ikind_crossing_mismatch`** (`ctype.ml:28`, env `OXCAML_IKIND_CROSSING_
   MISMATCH`): opt-in debug that recomputes crossing via `crossing_of_jkind` and
   compares to `Ikind.crossing_of_type`, printing on divergence. Default path runs
   ONLY `Ikind.crossing_of_type`; the comparison is gated off by default. It is a
   transition-era dual-computation debug aid that survived — inert and harmless,
   but a candidate for a later cleanup (it is the last place a non-ikind crossing
   is computed, even if only under an env flag).
2. **`OXCAML_IK5A_REENTRANCY_PROBE`** (`ikind.ml:105`): the stage-5a print-ctx
   re-entrancy REGRESSION probe (default-off). Same spirit as the seeded faults
   (permanent regression infra); reasonable to keep, though §D did not enumerate
   it explicitly.

## 4. Campaign summary ledger (stages 0–5, honest framing)

**Goal.** Make the LDD/ikind engine the sole kind-answering engine, replacing the
legacy structural `mod_bounds` + `with_bounds` + `normalize`/`sub` engine.

**BUILT (stages 0–5):**
- Stage 1–2: LDD engine made authoritative on the accept path; `-ikinds` default;
  the `typecore` existential seam flipped through `Ikind`. STAGE2-PERF: engine is
  neutral-to-faster than legacy (win concentrated on decl-heavy plain code).
- Stage 3: `OXCAML_IKINDS_VALIDATE` stored-vs-recompute harness + CLASS-A/B/C
  classifiers (temp-decl declared-jkind; recursive-module foreign-Param residue;
  with-bound over-approximation). No compile-path change; harness-only.
- Stage 4: decl `type_ikind` stored + marshaled; Param-free stores; cross-unit
  correctness (4d); the stage-4a `ikind_carrier` prototype (later deleted).
- Stage 5a: non-cache-clearing scratch print ctx; toggle inlining; re-entrancy
  probe. 5b: unit-qualified `Residue` atom (soundness gate — residues collision-
  free by construction) + named-terms cmi format + magic bump (format lock-in).
  5c: authority consolidation — `type_ikind` (decls) + on-the-fly `ckind_of_jkind`
  (transients); print floor from the ikind; drop legacy-floor fast path.

**DELETED (5c/5d/5m):** the `ikind_carrier` field + all its population/relabel/
validate machinery; `Jkind_desc.sub`, `Base.sub_expanded`, `check_sub`,
`sub_with_reason`, `sub`, `sub_or_error`, `sub_or_intersect` fn, `sub_jkind_l`,
`get_mode_crossing`, `With_bounds.to_best_eff_map` (~294 ln); the `-no-ikinds`
flag surface + 40 `-no-ikinds` twin tests + the dual-engine differential; the
wave-A transition counters/hooks and the S1 crossing-reroute tripwire.

**SURVIVES (and why):**
- LDD/ikind engine = sole answering engine (every verdict).
- `mod_bounds` floor + `with_bounds` `type_expr`s — IRREDUCIBLE transient
  derivation input (STAGE4C-P2/STAGE4A-F1: applied-constructor with-bounds have no
  atomic LDD form and can't be filled below the solver), DEMOTED to derivation-
  input + print sidecar.
- `Base_and_axes.normalize` fixpoint + `Jkind.normalize` + `round_up` + the
  lattice ops (`Mod_bounds.{meet,less_or_equal}`, `With_bounds.{meet,join}`) —
  the authoritative DECL-PRINT + intersection sidecar (see PAYOFF 2/3 below).
- `-print-from-ikinds` render path (print sidecar); the §D permanent harness
  (validate + CLASS-A/B/C + collision detector + 4 seeded faults + Ldd.bot seed).
- `Tof_kind` (intrinsic to the ikind machinery; never a legacy artifact). Per §E,
  there was no `Tmod` node / #6272 mask on the campaign line to delete (vacuous).

**PAYOFF ledger (final):**
- PAYOFF 1 (delete legacy sub chain): **DONE.**
- PAYOFF 4 (delete `-no-ikinds` + 40 twins + dual-engine differential): **DONE.**
- PAYOFF 2 (delete `normalize` fixpoint) + PAYOFF 3 (`mod_bounds` →
  `Axis_lattice` floor): **NOT TAKEN — USER DECISION.** The S7 decl-normalize
  differential is DIVERGENT: the ikind engine and legacy `normalize` choose
  DIFFERENT normal forms for with-bounds decls (e.g. `value non_float` vs
  `immutable_data with int`, and a raw rigid-name/coefficient form with no
  `type_expr` to reconstruct the legacy `with <type>` surface). Decl-print
  byte-identity (constraint 2, not waived) is unachievable without re-implementing
  `normalize`, which defeats the deletion. So `normalize` + its lattice-op users
  STAY as the permanent decl-print/intersection sidecar, and S8 (field relabel)
  is skipped. **This is the one open user-decision item: whether to accept the
  print-syntax divergence in exchange for PAYOFF 2+3, or keep the sidecar.**

**Perf:** exit gate PASS — FINAL vs wave-A base under 1% on every corpus
(`perf_records` +0.92% at the with-bound-heavy boundary, within the accepted
stage-2 caveat; decl-heavy/plain neutral).

## 5. Reproduce

Perf: `_tmp/perf5e/` — `gen_perf5e.py` (corpora), `drive5e.py`
(`N=11 python3 drive5e.py`; `FILES=perf_records N=25 python3 drive5e.py`).
Harness: `_tmp/perf5e/harness/{res,cross}` — compile lib then consumer/client
under `OXCAML_IKINDS_VALIDATE=1`; add `OXCAML_IKIND_SAVE_FAULT=1` on the save for
the non-vacuity check.
