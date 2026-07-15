# SLICE3-HANDOFF: normalize-engine deletion (Phase-2, fresh agent)

Handoff from the slice-2 finisher (ik5f3). Slice 2 (renderer promotion + codex
C1/F2 fixes + full corpus promotion) is DONE, accepted, and banked.

## Slice-2 endstate
- Branch `ik/pr2-print-deletions` @ **614a0d842** (tree clean; only untracked
  `_prefix/`, `_tmp/`). Boot-green. Warm build in `_build/_bootinstall`.
- Base of the PR range = **aa4e74b14** (PR #6451 tip); slice-2 work is
  92035033e..614a0d842.
- 13 commits: §4.1 resolver, gate flip (renderer is now the DEFAULT decl-print
  path), F2 fix, C1 fix, C2 doc, residual doc, and 4 promotion commits
  (or-null; core typing dirs; 12 CI layout subdirs; products+scannable).

## FIRST ACTION (before ANY slice-3 edit): full-suite gate
Run on the PRISTINE tree so there is no source-edit/`install_for_test`
contamination race:
`TMPDIR=$PWD/_tmp make -s test` (+ `make -s install`) and the validate corpus
`OXCAML_IKINDS_VALIDATE=1` must be **0 HARD**. The promoted expects match the
compiler, so it should be green; this is the slice-2 boundary/freeze gate.

## C1 residual (documented; DO NOT try to "fix" without a new design)
The C1 layout-filter (07b21181d) restores pre-M4 provenance corpus-wide EXCEPT
the representable-sort-var class. Residual sites (keyed to codex's C1 list),
left at the M4 text:
- `testsuite/tests/typing-layouts-products/basics.ml:2200`
- `testsuite/tests/typing-layouts-scannable/non_pointer.ml:523`
- `testsuite/tests/typing-layouts-scannable/abstract_kinds.ml:110,301`
Mechanism: the layout filter can't discriminate an `any` branch against an
unconstrained sort-var target without unifying; needs snapshot-at-violation
(C2 territory, de-scoped) or a non-unifying representability sub. Ledger for the
freeze review: codex C1 ~22-24/25 fixed + extra restorations beyond codex's list;
residual = representable-sort-var class only. Two timeboxed refinement forms both
regressed (see STAGE5F-NOTES "C1 residual"); team-lead ruled ship-(B) + document,
NO 3rd form. Also propagate this residual note to PRINT-DESIGN.md (ik5p-work) —
not done from ik5m-work (cross-worktree); a doc pass should add it.

## Slice-3 plan (design of record: ik5prep-work/SLICE34-PREP.md)
At 614a0d842 the legacy SUB engine is already gone; only `Base_and_axes.normalize`
fixpoint (+ `Jkind.normalize` wrapper + `round_up`) remains. Order:
1. **get_mod_bounds re-route FIRST** (jkind.ml:2586→2588 → get_externality_upper_bound
   → typedecl_separability.ml:506 + typeopt.ml:497). This is a TYPING-SEMANTICS
   path (not print/error): re-route to an ikind-native externality read with a
   ZERO-behavior-change validate-gated differential (5c-floor-flip standard),
   its own bisectable commit, BEFORE the fixpoint deletion.
2. Delete `Base_and_axes.normalize` fixpoint — GATED on the S7 decl-normalize
   differential (typedecl.ml:3540 stored type_jkind must stay byte-identical;
   DIVERGENT ⇒ KEEP normalize as the print sidecar + report).
3. **Mod_bounds.less_or_equal STAYS** — it is now PRINT machinery (sole caller
   jkind.ml:1661 To_out_jkind_const.diff), superseding the old plan's Finding-B.
4. Slice 4 (S8): `mod_bounds : Mode.Crossing.t` → `ikind_floor : Axis_lattice.t`,
   thin-shim `Mod_bounds` over `Axis_lattice` (keep the ~87 call sites), 47
   writes / 36 reads per the prep map; subst.ml floor-meets → Axis_lattice.meet.
5. Perf A/B: ik5prep `_tmp/perf_time.py --bin base=... --bin final=...` (ONE
   session, interleaved; box load makes absolutes unreliable). >1% regression =
   STOP.

## GOTCHAS (cost real time — heed them)
- **NEVER edit `typing/{jkind,ikind}.{ml,mli}` with the Edit tool** — the
  formatter hook reflows the whole file to a wrong profile. Use python/sed, then
  `ocamlformat --inplace <file>`; verify `git diff | grep -c ';;'` == 0.
- **Boot-only iteration** (skips the ~25-min `make install`): after editing,
  `make -s boot-compiler -j8`, then run `_build/_bootinstall/bin/ocamlc.opt`
  DIRECTLY for probes. Only `test-one`/`promote-one` need the full install.
- **Don't edit typing/ source while a `promote-one`/`test-one` batch runs** —
  `install_for_test` rebuilds from the working tree and contaminates results
  (learned the hard way; had to revert + re-run a whole sweep).
- **C1 validation shortcut**: `git diff aa4e74b14 -- <dir> | grep because` == 0
  proves provenance byte-identical to pre-M4 (catches reason-text AND location
  deltas) with zero collateral.
- **GAP-6 round-trip**: paired single-decl `.mli` method (`rtgate.sh` lives at
  `ik5m-work/_tmp/rtgate.sh`, gitignored — NOT ik5prep-work): write two files, one
  with the OLD spelling and one with the NEW spelling of the same decl, compile
  both with `-ikinds-debug -i`, extract the decl's LDD (`sub_poly`/`super_poly`)
  and compare modulo param-id (`param[N]`→`param[]`). PASS = identical = kind-
  equivalent. If the decl only appears in a REJECTED (error) context there is no
  `origin=ctype:decl` witness — wrap the same kind in a minimal ACCEPTED abstract
  decl (`type tgt : <kind>`) and compare its `origin=typedecl:normalize` block
  instead (the void promotion used this; see `_tmp/verify/void_h*.mli`). ik5prep's
  `roundtrip.py` has a parse bug on kind-annotated-param decls (its naive `" : "`
  split cuts at the param annotation) — use the paired-mli method, or fix the split.
- `TMPDIR=$PWD/_tmp`, `-j8`. Commit prefix `ikind: pr2 -- `. NEVER push.

## Q&A
Slice-2 finisher (ik5f3) is standing down but holding context as the freeze-review
Q&A resource.
