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

### H2 (gfp_pending) disposition

H2 requires isolating the LDD-global `gfp_pending` (ldd.ml), which is ik5b's
file per the ownership boundary. Flagged to team-lead as a scope finding beyond
STAGE4C's Solver-cache-only MUST-FIX; deferred pending adjudication. The
mandated fix (Solver-cache scratch ctx) is entirely in ikind.ml and drives
`ctx-evicted` to 0 on its own; that is the acceptance-(b) evidence
("before N / after 0"). H2 closure (with_isolated_pending) lands only if
team-lead approves the ldd touch / coordinates with ik5b.

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
