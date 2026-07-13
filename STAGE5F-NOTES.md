# STAGE5F-NOTES: renderer promotion + legacy-print deletion (Phase-2 finale)

Campaign: ikind-unification, PR #6460 Phase-2. Branch `ik/pr2-print-deletions`.
This is the as-built log for the finisher wave: the §4.1 param resolver, the
gate flip, the round-trip-gated promotion, and slices 3-4 (normalize deletion +
S8 field relabel). Design of record: SLICE2-HANDOFF.md, STAGE5M-PLAN.md,
PRINT-DESIGN.md/PRINT-GALLERY.md (ik5p-work).

## Slice 2a — §4.1 param_name_resolver (GAP-2 fix)

**Defect (GAP-2):** `type ('a, 'b) u : value mod portable with 'b` rendered as
`... with 'a` — the with-clause param letter was assigned by `synthetic_param_
naming` (ascending get_id over ONLY the params appearing in LDD terms), so a
bound that omits the leading param got the wrong letter. Confirmed inherent to
the signed-off renderer (ik5p `-print-from-ikinds` reproduced the wrong `with
'a`); legacy is correct.

**Fix (PRINT-DESIGN §4.1):** thread the live type-printer variable-name table
into the renderer.
- `typing/jkind.ml{,i}` (`Jkind.Const`): a `param_name_resolver : (int -> string
  option) ref` + `set_param_name_resolver` / `resolve_param_name`. Default
  resolver returns `None`.
- `typing/out_type.ml` (`Variable_names`): `resolve_param_id id` scans the live
  `!names` table (`(transient_expr * string) list`) for a var whose
  `Types.get_id` matches, returning its printed letter; installed once via
  `let () = Jkind.Const.set_param_name_resolver Variable_names.resolve_param_id`.
  The `names` table is populated during the decl header print (params named in
  `type_defined` before the kind is rendered in `tree_of_type_decl`), so the
  with-clause sees the decl's own letters. Unresolved ids (annotation contexts,
  no live table) fall through to synthetic naming.
- `typing/ikind.ml` (`render_terms_readable`): `name_of id` consults
  `Jkind.Const.resolve_param_name id` first, falls back to `synthetic id`.

**Acceptance (verified, `-print-from-ikinds` vs ik5p legacy default):**
- `testsuite/tests/typing-jkind-bounds/inclusion/inclusion.mli:1`
  `type ('a, 'b) u : value mod portable with 'b` — renders `with 'b`. PASS.
- subsumption `type ('a, 'b) t : value mod portable with 'b` (modalities_ikinds
  line 147 shape) — renders `with 'b`. PASS.
- Original 37-decl gallery: byte-identical between the pre-resolver renderer and
  the resolver build (resolver is purely additive; it only changes output where a
  param was previously mis-named). SAME on all 6 gallery source files.

## Gallery extension (SLICE2-HANDOFF §6, before the gate flip)

Two blind-spot shapes the 37-decl sign-off gallery missed. Verified vs ik5p
legacy default.

### Non-leading / subset / non-source-order param bounds (exercises §4.1)
`_tmp/gal_ext_resolver.mli`, all 6 rows byte-identical to legacy:

| decl | legacy = new+resolver |
|---|---|
| `('a, 'b) u : value mod portable with 'b` | `with 'b` ✅ |
| `('a, 'b, 'c) t : immutable_data with 'c with 'a` | `with 'a with 'c` ✅ |
| `('a, 'b, 'c) t2 : immutable_data with 'b` | `with 'b` ✅ |
| `('a, 'b) t3 : immutable_data with 'b with 'a` | `with 'a with 'b` ✅ |
| `('a, 'b, 'c) t4 : value mod portable with 'c` | `with 'c` ✅ |
| `('a, 'b) v : immutable_data with 'b @@ portable` | `with 'b @@ portable` ✅ |

Before the resolver, u/t2/t4/v would all have mis-rendered the leading `'a`.

### Non-value payloads (exercises GAP-6)
`_tmp/gal_ext_nonvalue.mli`. Divergences are GAP-6 (modality-axis elision,
round-trip-proven kind-equivalent) + 1 redundant-bound elision:

| decl | legacy | new | class |
|---|---|---|---|
| `('a : mutable_data) t_mut : immutable_data with 'a` | `with 'a` | `with 'a @@ unyielding many stateless` | GAP-6 |
| `('a : sync_data) t_sync : immutable_data with 'a` | `with 'a` | `with 'a @@ unyielding many stateless contended` | GAP-6 |
| `('a : mutable_data) t_mutp : ... with 'a @@ portable` | `with 'a @@ portable` | `with 'a @@ unyielding many stateless` | GAP-6 |
| `('a : sync_data) t_syncmany : ... with 'a @@ many` | `with 'a @@ many` | `with 'a @@ unyielding many stateless contended` | GAP-6 |
| `('a : mutable_data) t_mutc : ... with 'a @@ contended` | `with 'a @@ contended` | `with 'a @@ unyielding many stateless contended` | GAP-6 |
| `('a : immediate) t_immbnd : mutable_data with 'a` | `with 'a` | `mutable_data` (elided) | redundant-bound elision |

**GAP-6 round-trip proof (the gate the promotion uses).** For `t_mut`, both the
original source (`with 'a`) and the reprinted spelling (`with 'a @@ unyielding
many stateless`) compile to the IDENTICAL decl LDD:
`sub_poly=([0,0,0,3,0,0,0,0,3,0,0] ⊓ param[202]) ⊔ [2,1,0,0,0,0,0,0,0,1,2]`
(via `-print-from-ikinds -ikinds-debug -i`, the `origin=ctype:decl` line). So the
extra modality is a redundant-axis elision, kind-equivalent, strictly-more-
precise — the modality analogue of the signed-off redundant-bound elision.

## Slice 2b — gate flip (commit e945dfad8)

Deleted the `if not !Clflags.print_from_ikinds then None else` guard in
`render_jkind_from_ikind` (typing/ikind.ml), making the ikind renderer the
default decl-print path. Committed as a bisect-safe follow-on to the §4.1
resolver (46bb33226), not an amend: resolver-then-flip has no wrong-letter
bisect point. Verbatim SLICE2-HANDOFF §2 diff + ocamlformat reindent. Boot-green.

## Renderer bugs found by the promotion gate (corpus-wide sweep)

The 37-decl gallery + the two extension shapes did not exercise every corpus
shape. The first promotion dir (typing-jkind-bounds) surfaced two renderer
issues + confirmed one user-approved class. All three were present already in the
signed-off renderer (verified NEW-flipped == ik5p `-print-from-ikinds`), i.e. not
introduced by the flip/resolver.

### F1 (approved class): concrete-fold floor display

**User ruling 2026-07-12:** "printing the normalized form is fine if t is
concrete." An APPLIED type whose with-bound payload constructor is CONCRETE folds
at LDD construction (GAP-1), so the with-bound information lives only in the
`with_bounds` print sidecar at display time; the ikind renderer shows the folded
floor kind. APPROVED as a new promotion class:

> **concrete-fold floor display (sound; user-approved 2026-07-12)** — an applied
> type's kind in an error/decl renders as the folded floor (e.g. `int t`,
> `int list`, `int option`, `int array`, `(unit -> unit) t` → `value non_float`
> [`mod ...`]) instead of legacy's `immutable_data with int`. Sound
> over-approximation; verdict unchanged.

ABSTRACT payloads are the exception: their atom REMAINS in the LDD and MUST still
render `with t` (no fold). A collapse of an abstract payload to the floor is a
BUG, not this class — freeze it (see abstract-payload verification, below).

### F2 (fixed): dangling/empty `with ` on transitively-expanded abstract payloads

**Bug:** `clause_of_standalone` (typing/ikind.ml) rendered `Otyp_stuff ""` for a
standalone with-term made entirely of constructor atoms — it used
`payloads_of names`, which strips `Atom` names, leaving an empty product. Output:
`type v : value mod portable with ` (dangling `with`, invalid kind syntax).
Triggered when a with-bound payload is an abstract type carrying its OWN
with-bound, whose LDD flattens the term to a multi-atom product (`u & t`).

**Fix (display-side, no term-construction / LDD change — validate corpora
unaffected):** render ALL of a standalone term's names as the honest `&`-product.

**Result = new divergence class: transitive with-bound expansion (honest LDD
closure).** The abstract atom stays (no collapse), but because `t : ... with u`
brings `u`, the honest flattened form shows both:

| decl (source) | legacy | new |
|---|---|---|
| `type v : value mod portable with t` (`t : value mod portable with u`) | `with t` | `with u & t` |
| `type 'a x : immutable_data with 'a with t2` (`t2 : immutable_data with u`) | `with 'a with t2` | `with 'a with u & t2` |

Legacy shows the user-written concise form; the ikind renders the LDD closure.
Suppressing the transitive atom would require env-dependent implication reasoning
at render time (the same reason "no pattern-recovery" was ruled). ACCEPTED by
precedent (maps onto the standing "keep honest `k & 'a`, no pattern-recovery"
decision; byte-identity waived). Gallery rows: `_tmp/gal_ext_abstract.mli`. If
the user prefers the concise form, ANNOTATION-ECHO (below) restores it.

## C1 (fixed): Equal-tiebreak false provenance

**Codex HIGH / verified real M4 regression (4/4 spot-checked sites vs base
aa4e74b14).** `resolve_flattened_history` (typing/jkind.ml), on an ikind verdict
of `Equal` or `None`, fell back to a pure `score_reason` specificity tiebreak
over the two combined histories — which could pick the history explaining the
ACTUAL (weaker) kind and misattribute it as the reason the REQUIRED (stronger)
kind is imposed. Signature: the "must be <stronger>" line cited the SAME history
as the "is <weaker>" line (e.g. an `any` annotation cited as why a var "must be
representable"; both `any` and required `value` citing the same declaration).

**Root cause (proven by debug, NOT C2).** The stored Interact descs are accurate
at format time (not converged): d1.base=`value_or_null`, d2.base=`value`,
tgt.base=`value`. The bug is that the ikind sub-verdict is **layout-blind** — it
compares only the modal axes, so it returns `Equal` for `value_or_null` vs
`value` and `any` vs `value` (which differ only in LAYOUT). With the verdict
unable to tell the branches apart, selection fell to the score tiebreak, which
prefers the (more specific) annotation reason.

**Fix (lead's layout-level filter; filter-first).** Thread the kind being
formatted (`target = t.jkind`, available at the `format_flattened_history` call
site) into `resolve_flattened_history`. A branch's history may explain the
requirement only if the branch's kind IMPLIES `target` in BOTH axes:
`Sub_result.is_le (Jkind_desc.sub_layout env d tgt)` (the layout discriminator
the verdict misses) AND the modal verdict `∈ {Less, Equal}`. This filter is the
PRIMARY discriminator — applied BEFORE the verdict orientation (which otherwise
picks a sub/super side directly and bypasses the filter). When it cannot decide
(both or neither imply, e.g. no target / verdict unavailable / pure-modal
violation where layouts are equal) it falls back to the prior oriented-verdict +
score tiebreak, so pre-existing correct selections are unchanged. Cold path only
(format time); the ~267k-call combine path stays untouched. Only the
user-visible `format_flattened_history` call passes `target`; the Missing_cmi
lookup at the other caller is unchanged. Both §3a repros restored to the exact
pre-M4 provenance.

**Re-churn:** the M4-promoted expects carrying false provenance are re-churned to
the corrected (pre-M4-equivalent) text. Validation per dir: `git diff aa4e74b14
-- <dir> | grep because` == 0 confirms provenance byte-identical to pre-M4. The
fix restores provenance across the WHOLE corpus (broader than codex's enumerated
~25 — M4 regressed more sites) EXCEPT the residual class below.

**Residual class (documented, backlog #60): representable-sort-var target.**
When the violated requirement is "must be representable" (the target layout has
an unconstrained sort variable, e.g. `_representable_layout_N separable & value`)
the layout filter cannot discriminate: every layout is `<=` an unconstrained sort
var (and `Layout.sub` would unify it), so the non-implying `any`-annotation
branch is not dropped and the score tiebreak still wins. Affected sites:
`typing-layouts-products/basics.ml:2200` + `typing-layouts-scannable`
(non_pointer.ml:523, abstract_kinds.ml:110/301) — they stay at the M4 text.
A timeboxed (one boot iteration) non-unifying representability refinement was
attempted (branch-layout-is-`any` ⇒ can't explain representability) in two forms:
REPLACE (repr_required ? not-`any` : sub_layout) fixed products:2200 but
regressed the annots case (its function-arg branch's own desc is `any`); ADDITIVE
(sub_layout && not-`any`) fixed annots but regressed products (sub_layout of the
func-arg branch vs the product sort-var target is Not_le). No single non-unifying
predicate cleaned both, so per the timebox we shipped (B) (the sub_layout-only
fix, 07b21181d) + this documented residual. Honest ledger: codex C1 ~22-24/25
fixed + broad extra restorations; residual = the representable-sort-var sites.
Precision path (backlog #60): snapshot the branch layouts at combine time (C2
territory) or a non-unifying representability sub in the layout engine.

## ANNOTATION-ECHO — design note (NOT built this wave; user "(maybe)")

**User direction 2026-07-12:** user-written with-bounds "we should consider
retaining that in that exact form, but not on jkind, on the original construct
(maybe)". A SOURCE-FORM ECHO: keep the user's literal kind annotation attached to
the syntactic construct and prefer it for display over reconstructing from the
ikind.

**Feasibility — the infrastructure largely EXISTS.**
- `Parsetree.type_declaration.ptype_jkind_annotation : jkind_annotation option`
  (parsing/parsetree.mli) — the source annotation, verbatim.
- `Typedtree.type_declaration.typ_jkind_annotation : Parsetree.jkind_annotation
  option` (typing/typedtree.mli:1326) — retained into the Typedtree.
- **`'d jkind.annotation : Parsetree.jkind_annotation option`** (typing/types.mli:455)
  — already carried on EVERY jkind. This is the natural echo slot; it is populated
  when the user wrote an explicit annotation.

**Design.** At decl/error print time, when `jkind.annotation` is `Some a` (user
wrote the kind), print `a` verbatim (re-substituting for applied types) instead
of running the ikind renderer / `with_bounds` sidecar. Inferred kinds (annotation
`None`) still reconstruct from the ikind as today.

**What it lets us delete later.** For the ANNOTATED case it removes the reliance
on the `with_bounds` print sidecar and on byte-identical `normalize` output —
i.e. it is a path to retiring the print sidecar and dissolving the S7
decl-normalize crux (STAGE5M-PLAN §1a Finding A) for user-annotated decls. It
does NOT cover inferred kinds, so the sidecar/normalize cannot be deleted
wholesale on this alone.

**Beneficiary — the F2 transitive-with-bound-expansion class.** For a
user-written `type v : ... with t`, the echo prints `with t` (the exact source),
eliminating the honest-but-verbose `with u & t` LDD-closure form WITHOUT any
render-time implication reasoning. Same for concise abstract-payload display
generally.

**Size estimate.** Small-to-medium, printer-local: (1) a display-time branch in
`Out_type.tree_of_type_decl` / the jkind print path that prefers
`jkind.annotation` when present; (2) re-substitution of the source annotation for
applied types (parametric decls); (3) a gallery/differential pass proving the
echoed form is byte-identical to legacy on annotated decls. No engine/LDD change.
Risk concentrated in (2) (applied-type substitution of the source AST) and in
deciding precedence when the annotation is looser than the inferred/best kind
(the echo would then UNDER-report — needs a "annotation is at least best" guard
or a fall-through to reconstruction).

## Slice 2-tail — typing-layouts-void promotion (commit 2ce315ac9)

Missed by the slice-2 sweep (absent from SLICE2-HANDOFF §5). The gate-flip churned
its error-embedded kind text; expects were never re-promoted → sole red on the
full-suite gate at ba1591d0f. All 4 kind-text changes round-trip-verified
kind-equivalent (paired-mli LDD, `origin=typedecl:normalize` block, param-id
normalized; negative control `immediate` vs `immediate with key` discriminates):

| legacy spelling | new spelling | verdict |
|---|---|---|
| `immediate with key` | `immediate with key @@ external_` | PASS |
| `immediate with key with unit_u` | `immediate with key @@ external_` | PASS |
| `immediate with unit_u with v1 with v2` | `immediate with v1 @@ external_ with v2 @@ external_` | PASS |
| `immediate with v1` | `immediate with v1 @@ external_` | PASS |

Mechanism: `unit_u : void mod everything` crosses every axis ⇒ redundant with-bound
elided (approved class 1); plain-void `key`/`v1`/`v2` carry externality, rendered
as `@@ external_` modality (approved GAP-6). Verdict-preserving. Post: dir 13/13.
**New GAP-6 exercised shape:** void-as-with-bound (externality-carrying void
payload), round-trip count 4/4.

## Slice 3 — normalize-engine deletion (as-built)

### 3a — externality re-route (commit 82a0cf4ef)
`get_externality_upper_bound` (the SEMANTIC read used by
`typedecl_separability.msig_of_external_type` + `typeopt`) now reads the
externality axis of the ikind `round_up` floor (`Solver.round_up ∘
ckind_of_jkind → Axis_lattice.externality`, modeled on `crossing_of_jkind`)
instead of the legacy `Ignore_best normalize` fixpoint. Bridged via a new
`Jkind.externality_from_ikind` hook (mirrors `sub_verdict_from_ikind`).
Zero-behavior differential (`OXCAML_IKINDS_VALIDATE`, both computed, fatal on
`Externality` mismatch): positive control (forced `External64`) fires the fatal
(non-vacuity proven); synthetic corpus 182 checks / 0 mismatch; testsuite under
validate over typing-layouts-void/-layouts/-products/-or-null/typing-abstract-kinds
= 787 summaries, all `mismatches=0`, 0 disagreements.

### 3b(i) — get_mod_bounds differential kept PERMANENT (commits a58aba4c0, then reverted)
First landed as a deletion of `get_mod_bounds` + the differential (a58aba4c0,
full-suite gate 2330/0). Reverted per team-lead ruling: since the
`Base_and_axes.normalize` fixpoint SURVIVES for the decl-normalize path (S7
residual, below), the externality differential costs nothing to keep and is
retained as a PERMANENT validate-only detector (defense-in-depth, S2
overturn-detector precedent): `get_externality_upper_bound` ANSWERS from the
ikind floor, and under `OXCAML_IKINDS_VALIDATE` also computes the legacy
`get_mod_bounds`/normalize value and `Misc.fatal_errorf`s on any `Externality`
mismatch. Keeps the seeded-fault positive control reproducible and catches
future ikind/legacy drift. Net code delta over 3a: none (3a state preserved).

### S7 / S5 residual — the Base_and_axes.normalize fixpoint STAYS (evidence-backed)

**Skip-normalize experiment (2026-07-13).** normalize_decl_jkinds (typedecl.ml:3540)
temporarily patched to store `decl.type_jkind` UN-normalized; boot-built; compared
against the normalized `_install` compiler. Classification:
- **PRINT churn: ZERO.** `-i` decl output byte-identical (the slice-2 renderer
  renders from the ikind LDD, normalize-independent).
- **SEMANTIC churn: PRESENT and broad.** The stored decl `type_jkind` .cmi is
  BYTE-DIFFERENT for every decl whose kind normalize would fold — NOT just
  explicit-`with`-bound decls: `type r : immutable_data = { a : int }` (folds to a
  with-bounds-free floor) ALSO diverges, because normalize is what performs the
  fold. `-ikinds-debug` stored forms differ. ⇒ dropping normalize changes .cmi
  hashes codebase-wide (cross-unit-visible), and the direct-`with_bounds` reader
  `to_unsafe_mode_crossing` would diverge. This is exactly team-lead's
  ".cmi-shape divergence" trigger → KEEP.
- **Bounding finding (cross-unit .cmi probe):** unit B reading A's .cmi and doing
  subkind/crossing derives the SAME ikind and gets IDENTICAL verdicts regardless
  of A.cmi shape (ikind is normalize-independent). So the ikind-based TYPING
  behavior is stable — the risk of deletion is confined to the stored
  representation, .cmi hashes, and direct-`with_bounds` reads, not verdict flips.
  (This is why criterion (b) partially holds but NOT cleanly — the .cmi surface is
  not ikind-mediated.)

**Verdict: KEEP normalize for the decl path. Semantic churn present (broad
.cmi-shape divergence) ⇒ team-lead decision rule = KEEP. PAYOFF 2 stays
not-taken** (a recorded USER decision; re-opening it is the user's call). The
fixpoint's remaining callers all feed the STORED decl `type_jkind` (⇒ `.cmi` +
cross-unit + semantic queries), not just print — NOT promotable print churn, and
the ikind cannot reconstruct their stored output:

- **normalize_decl_jkinds (typedecl.ml:3540, S7):** `decl.type_jkind =
  normalized_jkind` is the authoritative stored decl kind (annotation-subkind
  check `Ikind.sub_jkind_l` at typedecl.ml:3568, `to_unsafe_mode_crossing`
  (jkind.ml:2641) on the `allow_any_crossing` path (typedecl.ml:3588-3591),
  cross-decl recursive lookup, `.cmi`). PRINT half
  is resolved (slice-2 renderer renders decl kinds from the ikind LDD terms, not
  from `jkind.with_bounds`). SEMANTIC half is NOT: the stored jkind's
  `with_bounds` cannot be reconstructed from the ikind LDD (which carries
  rigid-name/param atoms + coeff vectors and has discarded the with-bound
  `type_expr` surface form). Neither team-lead criterion holds — (a) stored jkind
  is not unchanged if normalize is dropped (`with_bounds` differs, `.cmi`
  diverges), (b) downstream is not all-ikind (`to_unsafe_mode_crossing` reads
  `mod_bounds`+`with_bounds` directly). ⇒ STAYS. Matches the prior finale
  finding (byte-identity needs re-implementing normalize; PAYOFF 2/3 not taken).
- **round_up (jkind.ml:3873, S5):** its result feeds ctype `intersect_type_jkind`
  and — decl-coupled — `nondep_type_decl`'s covariant fallback (ctype.ml:8433),
  which becomes the stored `type_jkind` ⇒ `.cmi`. The result is `with_bounds`-free
  so the with-bounds blocker does not apply, BUT the result BASE is the
  post-`Ignore_best`-normalize expanded base, which the ikind lattice does not
  carry; reconstructing it risks `.cmi` divergence. Re-routing buys nothing (the
  fixpoint survives via normalize_decl_jkinds) at real `.cmi` risk ⇒ STAYS.
- **Violation.of_ (jkind.ml:3372/3375/3380):** error-display normalize. Could be
  re-routed under approved error-text churn, but the fixpoint survives regardless,
  so it would be cosmetic caller-count reduction at churn cost; `report_reason`
  also reads `mod_bounds`/`with_bounds` structurally. ⇒ STAYS this slice (can be
  isolated later if the full deletion is pursued).

**Net:** slice 3 deleted the type-op externality path (3a) + `get_mod_bounds`
(3b-i). The `Base_and_axes.normalize` fixpoint SURVIVES, pinned by the
decl-`type_jkind` consumers. The gating item for the full "delete normalize"
payoff is **ikind→jkind with_bounds/base reconstruction** (re-implement normalize
on the ikind), which re-opens the prior PAYOFF-2 USER DECISION and was NOT taken
here. `Mod_bounds.less_or_equal` STAYS (const printer, per plan).

### Perf A/B (freeze regression gate) — HEAD vs 92035033e (whole PR2: slices 2+3+4)
Interleaved `_tmp/perf_time.py` (records+plain, prep corpus, 15 reps), base =
92035033e (PR #6460 re-cut base; fresh full-install build in ik5f4-perfbase),
final = ik5m `_install` @ HEAD (c45cac38a). Box HEAVILY loaded this run (load
13-26); the interleaved harness cancels per-rep drift; MIN is the robust figure.

| cell | base median / min | final median / min | Δmedian | Δmin |
|---|---|---|---|---|
| perf_records | 232.76 / 226.69 | 231.73 / 224.15 | −0.44% | −1.1% (faster) |
| perf_plain | 1072.66 / 1057.25 | 1067.52 / 1054.92 | −0.48% | −0.2% (faster) |

WASH / slightly faster — final ≤ base on BOTH cells (median and min). NO
regression; **>1% gate PASSED, no STOP-AND-SCOPE**. Consistent with the change
profile: slice 3 touches a cold per-decl type-op (externality read), slice 2 is
print-path, slice 4 deferred.

Secondary (slices 3-4 isolation, base = c3bcfdb2f = slice-2 endstate): also a
WASH on robust min (records final ≈ base, plain <1%; median ±1% = box-noise,
records/plain opposite directions run-to-run). Confirms slice 3-4 added no
regression on top of slice 2.
Note: HEAD-binary perf is unaffected by the validate-only differential-restore
(f8f769fbb) — that path is inert without OXCAML_IKINDS_VALIDATE.

## FREEZE-LEDGER ITEM — the only route to actually delete normalize (Q3, user-gated)

Deleting the `Base_and_axes.normalize` fixpoint requires the stored decl
`type_jkind` to be produced WITHOUT normalize — i.e. an **ikind→jkind
reconstruction** that rebuilds a semantically-and-.cmi-equivalent normalized
jkind from the ikind LDD. This re-opens the recorded PAYOFF-2 USER DECISION and
is NOT ours to take; logged here for the freeze report so the user can rule.

Why it is the ONLY route: the H2 boundary is definitional — the ikind LDD stores
the floor as rigid-name/param atoms + coeff vectors and, by design, discards the
with-bound `type_expr` surface form. normalize is what folds `with`-bounds into
the floor and produces the stored jkind. So either normalize survives (today), or
something reconstructs its output from the LDD.

Rough shape / cost of the reconstruction effort (separate design, not a slice):
1. **with_bounds reconstruction** — map LDD abstract atoms back to `type_expr`
   with-bounds. The LDD→type_expr direction is lossy today (atoms are rigid
   names/paths, not the original `type_expr`); needs either a side-table from
   derivation time (atom → source `type_expr`) or a canonical reconstruction that
   downstream + `.cmi` accept as equivalent.
2. **base reconstruction** — round_up's result base is the post-`Ignore_best`
   expanded base, not carried by the lattice; needs the expanded layout recovered
   from the ikind/env.
3. **equivalence bar** — must be `.cmi`-byte-stable (or a deliberate,
   cmi-magic-bumped format change accepted fleet-wide) AND preserve every semantic
   reader (`to_unsafe_mode_crossing`, `sub_jkind_l`-vs-annotation, cross-unit).
   The skip-normalize experiment shows a naive drop is broadly .cmi-divergent, so
   the bar is real work, not a re-route.
4. **payoff** — deletes normalize (PAYOFF 2) and unblocks the S8 field retype
   (PAYOFF 3), since the stored floor would then be ikind-native.
Estimate: medium-to-large, concentrated in (1). Risk: `.cmi` equivalence across
the whole fleet. Recommend a dedicated design pass, not folded into this PR.

## S8 status (census in "Slice 3 boundary" report; see message log)

S8 (mod_bounds field → `ikind_floor : Axis_lattice.t`) is mechanically feasible
via the thin-shim even with normalize surviving (normalize writes mod_bounds
through the `Mod_bounds` API: create/meet/max/min/set_max_in_set at
jkind.ml:1238/1452/1493/1322/…, absorbed by the shim), BUT it is a
`.cmi`-serialized representation change ⇒ needs a 2nd cmi magic bump
(build-aux/ocaml_version.m4 + autoconf27 reconfigure) + 87 `Mod_bounds.*` / 26
write / 36 read sites, for low value while normalize survives (cosmetic floor-
representation unification; does not delete normalize). Disposition pending
team-lead ruling (memory records DEFER; task #62 records PROCEED — flagged).
