# SLICE2-HANDOFF: promote the ikind renderer to the default print path

Campaign: ikind-unification, PR #6460 Phase-2 full deletion. Branch
`ik/pr2-print-deletions`, currently at the **slice-1 tip f63f7c1d3** (the
human-readable renderer is on the branch but still gated behind
`-print-from-ikinds`). This document hands off slice 2 (promotion) and the
remaining slices to a fresh agent after the diagnostic phase.

Slice 1 (renderer port) is DONE and accepted (commit f63f7c1d3). Slice 2 was
started: the gate-flip was implemented + built + the first corpus dir swept,
which uncovered two renderer gaps the sign-off gallery missed. Per team-lead
ruling, the gate-flip is REVERTED (not committed — it would print wrong variable
letters at its bisect point) and slice 2 restarts with a renderer FIX first.

## 0. Do this in order (team-lead directive)

1. **Implement the §4.1 param_name_resolver** (GAP-2 fix — §3 below). Approved;
   no user re-sign-off (it completes the signed-off design and moves output
   toward legacy-correct).
2. **Extend the gallery** with the two blind-spot shapes (§6) and verify vs
   legacy BEFORE flipping the gate.
3. **Flip the gate** (§2, verbatim diff below).
4. **Build the round-trip gate into promotion** (GAP-6 condition — §4) and do a
   SINGLE class-by-class promotion pass over the corpus (§5). No promote-then-
   regenerate: the resolver changes every with-bound letter, so it must land
   first.
5. Then slices 3-4 (delete normalize machinery; S8 mod_bounds→ikind_floor) per
   STAGE5M-PLAN.md / task #51.

## 1. Current tree state

- Branch `ik/pr2-print-deletions` @ f63f7c1d3, working tree CLEAN (only untracked
  `_tmp/`, `_prefix/`). Warm build; `_prefix/bin/ocamlc.opt` is currently a
  PROMOTED build (gate-flipped) from the diagnostic phase — rebuild after you
  land the resolver.
- Reference compiler with the readable renderer behind the flag:
  `/usr/local/home/jujacobs/oxcamls/prwork/ik5p-work/_prefix/bin/ocamlc.opt`
  (its DEFAULT path is legacy; `-print-from-ikinds` selects the readable
  renderer). Legacy-correct output = ik5p default; signed-off-renderer output =
  ik5p `-print-from-ikinds`.
- Gallery sources + captures: `prwork/ik5p-work/_tmp/gal_*.ml{,i}`,
  `gal_*.new.txt` (readable), `gal_*.legacy.txt`.

## 2. The gate flip (verbatim)

In `typing/ikind.ml`, `render_jkind_from_ikind`, delete the outer guard so ikind
rendering is unconditional (the floor path `mod_bounds_floor_for_printing` is
already ungated + byte-identical since STAGE4C, so only this one changes):

```
 fun env jkind ->
-  if not !Clflags.print_from_ikinds
-  then None
-  else
-    match jkind.Types.with_bounds with
-    | Types.No_with_bounds -> None
+  match jkind.Types.with_bounds with
+  | Types.No_with_bounds -> None
   ... (rest unchanged; run `ocamlformat -i typing/ikind.ml` to reindent) ...
```

`convert` (jkind.ml) already calls `!render_from_ikind.render env jkind` FIRST,
falling back to legacy on `None`, so this flip makes ikind the default renderer.
Leave the now-inert `-print-from-ikinds` flag + env setter + the line-~260
at_exit summary for the slice-3 flag-surface cleanup (harmless meanwhile).

## 3. GAP-2 fix — the §4.1 param_name_resolver (DO FIRST)

**Defect:** `type ('a, 'b) u : value mod portable with 'b` renders as
`... with 'a` (bound on the 2nd param printed as the 1st). The decl HEADER is
printed by the normal type printer (real letters) but the with-clause is printed
by `render_terms_readable` via `synthetic_param_naming`, which numbers 'a,'b by
ascending `get_id` over ONLY the params appearing in the LDD terms — so a bound
omitting the leading param gets the wrong letter. This is genuinely incorrect
(not kind-equivalent: `with 'a` ≠ `with 'b`).

**Confirmed inherent to the signed-off renderer** (NOT a port bug): ik5p
`-print-from-ikinds` produces the identical wrong `with 'a`; legacy is correct.
`grep param_name_resolver typing/ikind.ml` = 0 — PRINT-DESIGN §4.1 specifies it
but the prototype never built it.

**Fix (PRINT-DESIGN §4.1):** thread the live type-printer variable-name table
into the renderer. Install a resolver ref (e.g. `param_name_resolver : (int ->
string option) ref`) populated from Printtyp's `Variable_names.names` (keyed by
`Types.get_id`) during type printing; `render_terms_readable`'s `name_of` looks
up `!param_name_resolver id` FIRST and falls back to `synthetic_param_naming`
ONLY when unresolved (annotation contexts with no live table). Then decl/error
with-bound letters match the header + legacy. Acceptance: the two repros in §3a
render the correct letter; gallery (§6) unchanged.

### 3a. GAP-2 repro cases (verify the fix on these)
- `testsuite/tests/typing-jkind-bounds/inclusion/inclusion.mli:1`:
  `type ('a, 'b) u : value mod portable with 'b` — must render `with 'b`.
- `testsuite/tests/typing-jkind-bounds/subsumption/modalities_ikinds.ml:~145`:
  `type ('a, 'b) t : value mod portable with 'b` — must render `with 'b`.
Minimal standalone: put the first line in an `.mli` and run
`ocamlc.opt -extension-universe alpha -i file.mli`.

## 4. GAP-6 — approved as promotable, WITH the round-trip gate

**Observation:** on non-value payloads, legacy `@@ forkable unyielding many`
renders as ikind `@@ unyielding many [stateless]`. `modalities_of_coeff` computes
the ignored-axis set against `reference = value co_sub floor` (assumes a value
payload); for a non-value payload it spells a different (but equivalent) modality
that drops axes already implied by the base floor.

**Verified kind-equivalent** (raw LDD via `-ikinds-debug`): both spellings give
byte-identical coefficient vectors (e.g. `sync_data with 'a`: base
`[2,1,0,0,0,0,0,0,3,1,2]`, coeff `[[2,1,0,3,0,0,0,0,3,1,2]]` either way). So it's
a "redundant modality-axis elision (kind-equivalent)" — the modality analogue of
the user's signed-off redundant-bound elision, strictly more precise.

**Ruling (team-lead): promotable ONLY behind a scripted round-trip gate.** Two
anecdotes don't license a ~30-site class. Build into the promotion: for EVERY
churned site in this class, parse the NEW printed spelling back and compare its
raw LDD vectors to the original decl's; **100% identical → promote; ANY site that
fails equivalence is a real GAP-6 mis-detection → freeze that site and report it
(site-scoped)**. Record the per-dir round-trip tally (N sites, N identical) in the
notes; PRINT-DESIGN.md's GAP-6 entry must state the verification method
("kind-equivalence verified by scripted LDD round-trip on every promoted site"),
not just the two examples. Also correct GAP-6's "rare" → "pervasive on
mutable_data/non-value payloads; fixed-or-gated".

NOTE: the round-trip gate ALSO catches any residual GAP-2 (a wrong letter
re-parses to a different LDD) — belt-and-suspenders after the resolver fix.

## 5. Promotion state + approved class list

**Swept so far:** `typing-jkind-bounds` only (21/41 tests churn). Promotion was
run then REVERTED (`git checkout -- testsuite/tests/typing-jkind-bounds/`); tree
is clean. Remaining dirs to sweep (with-bounds/jkind + the CI layouts subdirs):
typing-layouts, typing-layouts-or-null, typing-layouts-products,
typing-abstract-kinds{,-missing-cmi}, typing-layouts-err-msg, typing-modules,
typing-modes, typing-modal-kinds, typing-gadts, and the 13 CI
`typing-layouts-{arrays,bits8,bits16,bits32,bits64,block-indices,float32,float64,
iarrays,scannable,untagged-immediate,vec128,word}`.

**Approved divergence classes (promote; per-class tally + representative
before/after in the notes):**
1. Redundant with-bound elision (with-bound implied by base dropped): e.g.
   `('a : immutable_data) t : immutable_data with 'a` → `... : immutable_data`.
   (User-signed-off.)
2. Redundant modality-axis elision (kind-equivalent) — GAP-6, §4. **Round-trip
   gated, 100% per-site.**
3. Unknown/Residue → `_`: poly-variant, first-class module, anon-type arg, open
   row with-bounds render `with _`.
4. Abstract-ctor recombination: `with 'a t`, `with ('a,'b) abs2`, `with t @@
   contended`, `with _ abstract @@ portable`.
5. Concrete-ctor collapse (GAP-1): `with int` folds into the base (legacy folds
   it too — usually no divergence).
6. Canonical clause order (vs source order) + secondary module-signature Format
   reflow driven by shorter kind text.

**Discipline (team-lead):** non-print output (verdicts, exit codes, artifacts)
MUST stay byte-stable — only jkind-rendering text changes. Verified on
jkind-bounds: 0 `[%%expect]` markers changed, Error lines 33/33 paired (no
verdict flips). Any changed expect NOT mapping to an approved class is a FINDING:
extend the list if defensibly correct-or-different, STOP-AND-SCOPE if it looks
wrong. Errors embed jkinds too, so error-corpus churn is broader than decl
printing — same discipline. Commit promotions in reviewable per-class/per-dir
chunks, not one mega-commit. Write the running tally in `STAGE5F-NOTES.md`.

## 6. REQUIRED gallery extension (before flipping the gate)

The 37-decl sign-off gallery missed BOTH gaps because every row used with-bounds
on LEADING params in order, with VALUE payloads. Add regression rows for the two
blind-spot shapes and verify each vs legacy (ik5p default) before the gate flips:
- **Non-leading / subset param bounds:** `type ('a, 'b) u : ... with 'b`;
  `type ('a, 'b, 'c) t : ... with 'c with 'a`; bounds naming a strict subset in
  non-source order. (Exercises the §4.1 resolver.)
- **Non-value payloads:** `('a : mutable_data)`, `('a : sync_data)`,
  `('a : immediate)` with-bound payloads, with and without explicit modalities.
  (Exercises GAP-6.)
Expected post-fix: variable letters match the header; modalities either match
legacy or are round-trip-proven equivalent. Record in PRINT-GALLERY.md.

## 7. House rules (load-bearing — cost me time)

- **NEVER edit `typing/{jkind,ikind}.{ml,mli}` with the Edit tool** — its
  formatter hook reflows the WHOLE file to a wrong (margin-90, janestreet
  `;;`/leading-`;`) profile, producing a ~3000-line spurious diff. Edit via
  python/sed, then `ocamlformat -i <file>` (repo-local ocamlformat 0.29.0 honors
  `typing/.ocamlformat-enable`; these files ARE ocamlformat-governed and are
  EXEMPT from the 80ch check — `ocamlformat --print-config` reports
  disable=false). Verify with `git diff --stat` + grep for `;;` contamination.
- The print-design branch's own commit 9d58dc668 carries that wrong-profile
  reflow; slice 1 was ported as the semantic delta only (do not cherry-pick it).
- `TMPDIR=$PWD/_tmp`, `make -s boot-compiler -j8`, `make -s install -j8`. Long
  builds: run in background (long Bash timeouts are blocked here).
- Corpus probe invocation: `ocamlc.opt -extension-universe alpha
  -print-from-ikinds -i file.ml` (drop `-print-from-ikinds` once the gate is
  flipped); `-ikinds-debug` adds the raw LDD form. `make -s promote-one
  DIR=<dir>` promotes a dir; `git diff` the test files to review, `git checkout`
  to revert.
- Validate corpora must stay 0 HARD (`OXCAML_IKINDS_VALIDATE=1`); the recompute
  harness reads mod_bounds+with_bounds — untouched by the renderer work.
- NEVER push. Commit prefix `ikind: pr2 -- `. Defensive reflog/status before
  commits. Report to the team-lead at each slice; FREEZE at the wave end for
  adversarial review.

## 8. Scope boundary (H2 — do NOT cross without evidence)

`with_bounds` has two roles: DECL-PRINT SIDECAR (dies as printing moves to the
renderer) and TRANSIENT-DERIVATION INPUT (irreducible per STAGE5-DESIGN §H2/F1 —
`ckind_of_jkind` derives transient ikinds from mod_bounds+with_bounds on the
fly). The field SURVIVES this wave for transients. `mod_bounds : Mode.Crossing.t`
DOES die in S8 (→ `ikind_floor : Axis_lattice.t`; re-route subst.ml floor meets
via `Axis_lattice.meet` first). If a census suggests the transient derivation can
go ikind-native cheaply, STOP-AND-SCOPE — do not re-architect unilaterally.
