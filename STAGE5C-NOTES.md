# Stage 5c notes: authority consolidation (acceptance results)

Campaign: ikind-unification. Branch `ik/stage5c-authority` off `6b3ae002e`
(integrated stage-5a+5b head, 36/36 CI green). Implements STAGE5-DESIGN.md §5c
(three work items) + §C.2 transition validation for the floor flip.

## Commits (linear, off 6b3ae002e)

| SHA | item | summary |
|---|---|---|
| `18ed1ab27` | §5c-1 | delete the stage-4a `ikind_carrier` field (coherent slice) |
| `22cfd15df` | §5c-2 | print the mod-bounds floor from the ikind on the default path |
| `1b49f48dd` | §5c-3a | §C.2 transition-validation differential for the floor fast path |
| `cb4a13265` | §5c-3b | drop the legacy `mod_bounds`-floor fast path (LDD sole floor engine) |

Boot-green (`make -j8` exit 0) verified at every commit; `make fmt` clean incl.
the 80-column check on every touched file.

## Item 1 — DELETE the `ikind_carrier` field (Q1 adjudicated = delete)

Coherent single slice removing, together: the field on `base_and_axes`
(types.mli/types.ml); all 59 `ikind_carrier = None` construction literals;
`Base_and_axes.map_type_expr` carrier arm + `relabel_ikind_carrier` +
non-injective drop-on-collision (btype.ml); the `Subst` drop-on-save
(subst.ml); the bounds-change-resets-carrier updates in `apply_modality_l/r`
(jkind.ml); `jkind_with_carrier` + its typedecl call sites; `validate_carrier`
+ `is_constructor_atom` + the `carrier_checks/benign/mismatches` counters + the
at_exit summary field (ikind.ml); the ikind.mli val; and the now-moot test
`rename_relabel_ikinds.ml`.

WHY (recorded in the commit body so it is not archaeologically resurrected):
the stage-4a carrier was the exploratory prototype of "store the ikind on the
jkind." Stage 5 establishes it CANNOT be the sole datum (STAGE4A-F1: cannot be
filled at construction, below the solver in the DAG; STAGE4C-P2:
applied-constructor with-bounds have no atomic LDD form) and NEED NOT be a
stored memo (STAGE1: no transient physical reuse; STAGE2-PERF: on-the-fly
derivation is net-faster). Authority = `type_ikind` for decls + on-the-fly
`ckind_of_jkind` for transients. Do not re-add a jkind-level ikind field
without first refuting H2.

SURVIVING stage-4a contributions (kept, untouched): `Ldd.to_terms`/`of_terms`,
`Ldd.filter_out_pure_terms`, the injectivity analysis, and the seeded-fault /
term-precise-validation methodology.

## Item 2 — print the floor from the ikind on the default path

`mod_bounds_floor_for_printing` previously fired only under `-print-from-ikinds`;
dropped that guard (`if not (!print_from_ikinds && !ikinds)` -> `if not
!ikinds`) so the floor is derived from the ikind on the DEFAULT print path
whenever ikinds are on. For with-bounds-free const jkinds `round_up` of the
derived LDD returns exactly `to_axis_lattice mod_bounds` and `of_axis_lattice`
round-trips it (STAGE4C P1, byte-identical), so default printing is unchanged.
With-bounds jkinds return `None` and fall back to the legacy `mod_bounds` floor
(the LDD cannot reconstruct the `with`-clause surface, STAGE4C-P2); the
`with`-clause list itself is printed from the `with_bounds` sidecar
(jkind.ml `printable_with_bounds`, unchanged). Net: the `Mode.Crossing.t`
mod_bounds is no longer read to print the floor of a with-bounds-free jkind.
Re-entrancy-safe via the 5a scratch ctx + isolated pending.

## Item 3 — drop the legacy `mod_bounds`-floor fast path (with §C.2 first)

### 3a — transition-validation differential (§C.2), FIRST

A validate-gated differential in `compute_subcheck_polys`: at every seam where
the floor fast path fired, it computed the full ikind-derived sub polynomial
(exactly as the `None` arm does: Round_up, super constant) inside
`Ldd.with_isolated_pending`, and compared the fast-path floor's
`leq_with_reason` VIOLATING-AXIS SET against the full polynomial's. Counters
`floor_fastpath_checks` / `floor_fastpath_mismatches`.

### 3b — drop the fast path

Removed: the floor fast path in `compute_subcheck_polys`; the now-dead
`mod_bounds_floor_of_jkind` / `mod_bounds_floor_of_jkind_desc` legacy floor
reads; the `Lhs_mod_bounds_floor_fast_path` variant + its debug arm; and (with
the fast path) the transitional 3a differential + its counters. When `super` is
constant the sub polynomial is derived in Round_up mode exactly as the old
`None` arm did — the LDD is now the sole answering engine for the floor.
`Rhs_top_fast_path` (super rounds up to top) is retained.

## Acceptance evidence

1. **Boot-green per commit:** `make -j8` exit 0 at 18ed1ab27, 22cfd15df,
   1b49f48dd, cb4a13265 (and on the fmt-promoted trees).

2. **§C.2 floor differential green BEFORE the drop** (on 1b49f48dd, corpus =
   typing-jkind-bounds + all typing-layouts* compiled `-c` under
   `OXCAML_IKINDS_VALIDATE=1`):
   - **444 fast-path checks across 19 files, 0 mismatches** — the fast-path
     floor verdict is axis-set-identical to the full ikind sub polynomial
     everywhere the fast path fired.
   - **Seeded-fault liveness:** forcing the slow polynomial to `Ldd.bot` makes
     the differential flag `FLOOR-FASTPATH-MISMATCH fast=[contention] slow=[]`
     (checks=1, mismatches=1) — the harness is non-vacuous. (A `top` seed does
     NOT flag: `top` and the real poly give the same violating-axis set on every
     firing, which is itself corroboration that the fast path was a sound
     floor.)
   - **Independent corroboration (duplicate agent, clean 3a binary, pre-seed):**
     an independent item-3a validate sweep over the typing-jkind-bounds ikinds
     corpus reported floor-fastpath checks=130, mismatches=0; validate
     checks=38334, HARD=0; zero `FLOOR-FASTPATH-MISMATCH` lines. Two independent
     sweeps double-license the 3b drop.

3. **`OXCAML_IKINDS_VALIDATE` corpus at HEAD (cb4a13265) → 0 HARD:** 352 files
   (typing-jkind-bounds + all typing-layouts*), total validate-mismatches=0,
   total class_b=0, 0 CARRIER/FLOOR-FASTPATH/validate mismatch lines.

4. **Suites — item 3 adds ZERO churn** (same-environment baseline comparison,
   item-2 `22cfd15df` vs HEAD `cb4a13265`, both built + run here):
   - typing-jkind-bounds: IDENTICAL 4-test failure set at item-2 and HEAD.
   - typing-layouts: IDENTICAL 9-test failure set at item-2 and HEAD.
   - typing-modules: **54/0** clean at HEAD.
   Because the fast-path drop only affects the `super_is_constant` path and the
   §C.2 differential proved it verdict-equivalent, dropping it changes no
   verdict — confirmed empirically by the identical failure sets.

### On the residual pre-existing failures in this worktree

The 4 jkind-bounds + 9 layouts failures are NOT stage-5c regressions — they are
IDENTICAL with and without item 3 (§ acceptance 4), i.e. they pre-date 3a/3b.
Their causes are environmental / stale-expectation, unrelated to the
carrier/print/floor work:
- `annots_ikinds.ml`, `basics.ml`, and most layouts failures: `Error: Unbound
  module "Stdlib_upstream_compatible"` — that library is absent from this
  worktree's `_runtest` install; every test using it fails identically.
- `rectypes_ikinds.ml`: `-rectypes` occurs-check ("type constraints are not
  consistent") — flag/environment, not ikinds.
- `row_ikinds.ml`, `variants_ikinds.ml`: stale `[%%expect]` pinning an older
  REJECT where the current compiler (already at item-2, fast path present)
  ACCEPTS (the tests carry `(* should be accepted *)` notes); a promotion owed
  by an earlier ikind stage, not by 5c.

The duplicate agent's independent item-2 corroboration in a COMPLETE
environment (with the library present) was **typing-jkind-bounds 73/0,
typing-layouts 45/0, typing-modules 54/0** — i.e. items 1+2 are byte-identical
green when the library is available. My same-environment item-2-vs-HEAD
comparison then carries that green forward across item 3.

## Process note — duplicate-agent race (resolved)

During implementation a box restart made the original `ik5c` briefly appear
dead; a successor `ik5c-2` was spawned and both edited `typing/ikind.ml`
concurrently for a window. No work was lost (the history stayed linear:
items 1/2 committed by the original, 3a by the successor). The team-lead
arbitrated: the successor stood down permanently and the original was
confirmed sole owner to finish item 3. The committed 3a differential is the
successor's axis-set version (more precise than the original's accept/reject
draft); it was rebuilt, re-verified green (444/0), and then removed with the
fast path in 3b. Standing down on the race rather than clobbering is per
`AGENTS.md` house rule (verify sole ownership before writing).

## Status: FROZEN for adversarial review. Not pushed.
