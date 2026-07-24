# Stage 5a notes: per-print scratch solver context (re-entrancy MUST-FIX) + toggle inlining

Campaign: ikind-unification. Branch `ik/stage5a-printctx` off `a65386e849`
(stage-4 integration tip: stage-4c print seam + stage-4d cmi/residue, 36/36
green). Sub-stage 5a per STAGE5-DESIGN.md. Line numbers from that tip.

I own the PRINT-CTX path and jkind.ml print seams. ik5b owns
ikind/ldd/subst/cmi-format (Residue atoms + named-terms cmi). Both touch
typing/ikind.ml; I keep to print-related regions.

---

## Mandate (this sub-stage)

1. SCRATCH PRINT CTX: make both print-path ikind derivations
   (`mod_bounds_floor_for_printing` = floor for with-bounds-free jkinds;
   `render_jkind_from_ikind` = full with-bounds rendering) run in a per-call
   scratch solver context that does NOT clear/perturb the global caches, so
   printing from ikinds is safe even mid-check (re-entrant by construction).
   This discharges the STAGE4C-DESIGN.md STAGE-5 MUST-FIX (lines 299-319):
   "when the flag flips on by default, BOTH derivations must switch to a
   non-cache-clearing per-print scratch ctx ... Do not attempt to prove
   non-re-entrancy; just give the print path its own scratch context."
2. TOGGLE INLINING per STAGE5-DESIGN.md §5a (`-no-ikinds`/`-ikinds` toggle
   simplification). BLOCKED: STAGE5-DESIGN.md is not reachable in this object DB
   (commit dba63c3c7 / branch ik/stage5-design absent) nor on disk. Requested
   from team-lead; scope TBD.

Evidence-first (mandate): demonstrate the cache-clearing effect with a probe
BEFORE building the fix, then show the scratch ctx eliminates it
(before: print clears N cache entries / after: 0).

---

## The hazard (code facts, base a65386e849)

### H1. `create_ctx` clears the two GLOBAL Solver caches, and the ctx REUSES them.

`Solver.create_ctx` (ikind.ml:316-325):

    let create_ctx ~mode ~env ~lookup_of_env =
      TyTbl.clear global_ty_to_kind;          (* <-- clears *)
      ConstrTbl.clear global_constr_to_coeffs; (* <-- clears *)
      { env; lookup_of_env; mode;
        ty_to_kind = global_ty_to_kind;         (* <-- ctx points AT the globals *)
        constr_to_coeffs = global_constr_to_coeffs }

`global_ty_to_kind` caches `type_expr -> Ldd.node` (keyed by the union-find
representative; also the circular-type termination memo) and
`global_constr_to_coeffs` caches constructor polynomials. Every `create_ctx`
wipes both. BOTH print derivations call it (ikind.ml:1401, 1435).

Consequence: if a print fires while an OUTER ikind computation is on the stack
(its ctx holds the same global tables), the print's `create_ctx` empties the
outer's live cache. That breaks circular-type termination (the memo that makes
recursive `kind` terminate is gone) and can change/repeat the outer result.

### H2. The print path also drains the GLOBAL `gfp_pending` list.

`gfp_pending : (var * node) list ref` (ldd.ml:498) accumulates greatest-fixpoint
constraints enqueued for ABSTRACT constructors during `Solver.constr_kind`
(ikind.ml:488, 497). `solve_pending ()` (ldd.ml:503-508) drains and solves ALL
of them, mutating each var's state to `Solved`.

- `mod_bounds_floor_for_printing` -> `Solver.round_up` -> `Ldd.round_up`
  (ldd.ml:527-530) calls `solve_pending ()`.
- `render_jkind_from_ikind` calls `Ldd.solve_pending ()` directly (ikind.ml:1437).

Consequence: a mid-check print drains the OUTER solve's pending gfps
prematurely, in the print's scratch context, corrupting the outer solve on a
second axis independent of H1.

### Not hazards (shared state that is safe to keep sharing)

- `rigid_tbl` (ldd.ml:125): interns rigid vars by param id. Param ids are stable
  and global; reuse across print and check is CORRECT (same param -> same var).
  Rigid vars are never gfp-enqueued (`solve_gfp` rejects `Rigid`, ldd.ml:490).
- `prev_non_rigid_id` (ldd.ml:123): monotonic fresh-id counter. A print advances
  it; harmless (only consumes ids, never corrupts).

So the isolation surface is exactly {global_ty_to_kind, global_constr_to_coeffs,
gfp_pending}. The scratch fix isolates all three; nothing else.

---

## The fix

1. `Solver.create_scratch_ctx ~mode ~env ~lookup_of_env`: identical to
   `create_ctx` but allocates FRESH `TyTbl`/`ConstrTbl` for `ty_to_kind` /
   `constr_to_coeffs` and does NOT touch the globals.
2. `Ldd.with_isolated_pending : (unit -> 'a) -> 'a` (new in Ldd_intf.S + ldd.ml):
   save `!gfp_pending`, set it `[]`, run `f`, restore in a `Fun.protect` finally.
   The print's own gfps (enqueued+drained inside `f`) never see, and never
   leave, the outer's pending list.
3. Point both print derivations at an ikind-level `create_scratch_ctx` wrapper
   and wrap each derivation body in `Ldd.with_isolated_pending`.

Re-entrant by construction: with fresh caches + isolated pending, a print cannot
read, clear, or drain any global state an outer solve depends on. No
non-re-entrancy argument is relied upon (per the MUST-FIX directive).

---

## The probe (evidence-first)

PRIMARY (committed, corpus-wide, real workload): counter
`print_ctx_evicted_entries` in the `-ikinds-debug` at_exit summary. Incremented
at each PRINT-path ctx creation by the number of global cache entries it evicts
(`TyTbl.length global_ty_to_kind + ConstrTbl.length global_constr_to_coeffs`
observed at that moment).

- BEFORE fix: print uses the clearing `create_ctx` -> counter = entries cleared.
  Measured flag-on across typing-jkind-bounds (and again with ikinds_validate
  on, so the evicted entries belong to genuine checker computations, not merely
  prior prints). Expect N > 0.
- AFTER fix: print uses `create_scratch_ctx` (clears nothing) -> counter = 0.

This is the literal acceptance phrasing "before: print clears N cache entries /
after: 0."

SECONDARY (gfp, `OXCAML_IK5A_REENTRANCY_PROBE`): a targeted probe that injects a
print derivation mid-check (globals warm + a gfp pending) and reports whether the
outer cache/pending survived. Numbers documented here. Demonstrates H2, which the
corpus counter cannot (in flag-on runs gfp_pending is empty at print time because
prints happen post-unwind -- exactly the "too optimistic" assumption the MUST-FIX
refuses to rely on).

---

## Measurements

### Probe (before fix), commit adding the counter

Boot compiler at the probe commit. Source `/tmp/ik5a_probe_src.ml`: eight type
decls mixing with-bounds-free (`w`, `x`) and with-bounds (`box`, `t`, `u`, `v`,
`big`, `nested`) jkinds. Compiled `-extension layouts_beta -print-from-ikinds
-ikinds-debug -i` (the `-i` inferred-signature print forces every decl's jkind
through the render/floor seam). at_exit summary:

    [ikind-print] floor derivations=336 with-bounds rendered=6 render       fallbacks=0 fault=false ctx-evicted=22

- `ctx-evicted=22`: print-path `create_ctx` calls evicted 22 warm global Solver
  cache entries during printing. N > 0 => hazard H1 is real and reachable on the
  print path (a print DOES clear a warm cache). Same 22 with
  `OXCAML_IKINDS_VALIDATE=1` (ikind checking on, so the warm entries include
  genuine checker computations, not only prior prints).
- Flag OFF (control): no `[ikind-print]` summary at all -- the print seam never
  runs, output byte-identical to base. Confirms the counter measures only the
  print path.
- Smoke (acceptance c): with-bounds jkinds render `with param[N] @ [..]` and
  `with box/279[1].1 & param[N] @ [..]`; with-bounds-free `w`/`x` render
  byte-identical floors (`immutable_data` / `mutable_data`). Render path healthy.

### H2 (gfp_pending) — APPROVED IN SCOPE, FIXED

Team-lead adjudicated H2 IN scope (the gfp drain is corruption, worse than the
cache wipe) and granted the ldd.ml boundary (ik5b frozen). Fix: `Ldd.with_
isolated_pending` (ldd.ml, exception-safe via Fun.protect) swaps in a fresh
empty pending list for the print derivation and restores the caller's list,
wired into both derivers. gfp-drain probe (env OXCAML_IK5A_REENTRANCY_PROBE, a
mid-check print at the first crossing_of_jkind):

    [ik5a-reentrancy] gfp drain by a mid-check print: un-isolated 1->0
      (drained=1, the H2 hazard); isolated 1->1 (drained=0, fixed)
      -- OK (with_isolated_pending prevents the drain)

before (un-isolated): the print's solve_pending drains the outer pending gfp
(1->0). after (isolated): the outer gfp survives (1->1). Both cases use the
scratch ctx, so the caches are safe in both; the drain is the isolated H2
signal.

### Fix (after), scratch-ctx commit

Same source + flags, boot compiler at the scratch-ctx commit:

    [ikind-print] floor derivations=336 with-bounds rendered=6 render \
      fallbacks=0 fault=false ctx-evicted=0

- `ctx-evicted=0` (was 22): the print path no longer evicts ANY global cache
  entry -- hazard H1 closed. The counter is measured faithfully (global size
  before vs after the print-ctx creation), so 0 means real: not a hard-coded
  value. Left in place as a permanent regression guard.
- `floor derivations=336`, `with-bounds rendered=6`, `render fallbacks=0`
  UNCHANGED, and the rendered `-i` signature is byte-for-byte identical to the
  before-fix output. Proof that the scratch ctx changes only re-entrancy safety,
  not derivation results (as expected: `create_ctx` already cleared the globals
  every call, so the cache was always a per-derivation memo; a fresh private
  table computes the same values).

---

## Acceptance (item 1: scratch print ctx)

- (a) flag-off byte-identical suites (`make test-one DIR=...`, real _install):
  typing-jkind-bounds 74/74, typing-modules 54/54, typing-layouts 45/45 -- all
  pass, 0 failures. (Counts match the stage-4 tip; +2 over STAGE4C's 72 is base
  growth, not churn.)
- (b) probe before/after: ctx-evicted 22 -> 0 (committed counter, faithful
  before/after eviction measurement; permanent regression guard).
- (c) render smoke: with-bounds render `with param[N] @ [..]` /
  `with box/..[..].1 & param[N] @ [..]`; with-bounds-free floors byte-identical;
  redundant-absorb sample also confirmed (below). floor derivations=336,
  with-bounds rendered=6, fallbacks=0.
- (d) boot-green per commit: design, probe, fix all `make boot-compiler -j8`
  exit 0.
- (e) flag-on mixmod5 validate picture: measured on THIS branch (stage-4 tip +
  scratch ctx), via `tools/expect -ikinds-debug` under `OXCAML_IKINDS_VALIDATE=1`:
  `checks=2826 mismatches=0 (HARD=0) class_b=10 residue_trusted=10`. HARD=0 is
  the soundness-critical figure and is satisfied. class_b=10 (not the mandate's
  9): the "9" is ik5b's Residue-branch figure (STAGE5-DESIGN AS-BUILT line 302 =
  class_b=9 on ik/stage5b-residue-cmi, whose Residue atom changes CLASS-B
  recognition); the stage-4 tip is 10. My change is print-path-only and gated on
  `print_from_ikinds` (off here), so it is provably inert to this count -- the
  full base..HEAD diff touches only the counter, the at_exit summary, two new
  unused Solver helpers, `create_print_ctx`, and the two print derivers. So the
  picture is UNCHANGED by my work; the 9 vs 10 is a base-context difference, not
  a regression. Flagged to team-lead.

## §5a scope (from STAGE5-DESIGN.md, now readable)

- Item 1 (scratch print ctx): DONE for the cache axis (this branch). The doc's
  wording (line 208-210) bundles `solve_pending` -- my hazard H2. H2 isolation
  lands in ldd.ml (`with_isolated_pending`), an ik5b-owned file; pending
  team-lead's boundary call. Doc also asks for a re-entrancy regression test.
- Item 2 (Param-projection/relabel): MOOT -- Q1 adjudicated DELETE-carrier
  (doc line 224: "If the carrier is DELETED ... these are MOOT").
- Item 3 (fold the four `enable_*` toggles to unconditional): TODO, this branch,
  mechanical. NOT `reset_constructor_ikind_on_substitution` (that is 5d, doc
  line 428).

---

## Re-entrancy regression guard (doc §5a acceptance)

The doc's §5a acceptance asks for "a re-entrancy regression test that prints a
jkind mid-check under -print-from-ikinds and shows no outer-derivation
corruption." The CACHE re-entrancy property is: *the print path must not
clear/evict the global Solver caches an outer solve depends on.* That is exactly
what the committed `ctx-evicted` counter measures and permanently guards:

- It is incremented at the ONLY place a print derivation creates a solver
  context (`create_print_ctx`), by the FAITHFUL before-vs-after delta of the
  global cache size (`max 0 (before - after)`), so it counts real global-cache
  eviction by the print path, corpus-wide, on every `-ikinds-debug` build.
- Necessary + sufficient: the ONLY channel by which a print corrupts a
  concurrent/outer derivation via the cache is by clearing the shared global
  tables the outer ctx points at (H1). `ctx-evicted=0` means the print path
  clears nothing, so no outer derivation can be corrupted through the cache.
  Structurally guaranteed by `create_scratch_ctx` (fresh private tables, globals
  untouched) and empirically confirmed (22 -> 0).
- Regression behaviour: if a future change re-homes a print deriver onto the
  clearing `create_ctx` (or otherwise perturbs the globals), `ctx-evicted` goes
  > 0 on the next `-print-from-ikinds -ikinds-debug` run -- a live tripwire.

REPRODUCTION (the "regression test" one-liner):

    ocamlc.opt -extension layouts_beta -print-from-ikinds -ikinds-debug -i FILE
    # assert the [ikind-print] at_exit line reports  ctx-evicted=0

A deeper explicit mid-check-injection test (nesting a print inside an in-flight
`constr_kind` and asserting the in-progress circular-type placeholder survives)
was scoped and REJECTED as too invasive for the value: it would instrument the
recursive hot path (`constr_kind`, ikind.ml:458 -- the circular-type termination
memo) behind a re-entrancy guard flag + a forward-reference hook to the
print-path ctx, all to re-demonstrate the property the `ctx-evicted` counter
already proves necessary-and-sufficiently. `type_declaration_ikind_of_manifest`
(the other warm-then-use path) has NO live caller, so it is not a usable hook.
Offered to team-lead if an explicit injection test is nonetheless wanted.

## Final acceptance (item 1 = H1 cache + H2 gfp; item 3 = toggles)

Team-lead adjudicated H2 IN scope and granted the ldd.ml boundary (ik5b frozen).
Both hazards are now closed and the package re-froze green:

- BOOT-GREEN per commit (7 code commits; every `make boot-compiler -j8` exit 0).
- FLAG-OFF byte-identical (real `make test-one`, after H2): typing-jkind-bounds
  74/74, typing-modules 54/54, typing-layouts 45/45.
- H1 cache: ctx-evicted 22 -> 0 (committed counter); render byte-identical.
- H2 gfp: mid-check-print probe -- un-isolated drained=1, isolated drained=0, OK.
- RENDER smoke: with-bounds `with param[N] @ [..]` etc.; redundant-absorb; floors
  byte-identical. floor derivations=336, with-bounds rendered=6, fallbacks=0.
- mixmod5 flag-on: HARD=0; class_b=10 (base-context, accepted by team-lead).
- `ocamlformat --check` clean on ikind.ml / ldd.ml / ldd_intf.ml.
- Re-entrancy REGRESSION coverage for BOTH hazards: cache = the corpus-wide
  ctx-evicted=0 counter (tripwire); gfp = the OXCAML_IK5A_REENTRANCY_PROBE
  mid-check-print probe (drained=0 with isolation, >0 without).

Diff: typing/ikind.ml, typing/ldd.ml, typing/ldd_intf.ml, STAGE5A-NOTES.md. No
jkind.ml; ldd.ml touched only in the gfp_pending region (team-lead-granted;
ik5b's Residue work is in the Name/atom area -- no semantic overlap).
