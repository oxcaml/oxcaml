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
branch is not dropped and the score tiebreak still wins. Affected sites (3, per
adversarial-review A4 recount): `typing-layouts-products/basics.ml:2200` +
`typing-layouts-scannable/abstract_kinds.ml:110` and `:301` — they stay at the M4
text. (`typing-layouts-scannable/non_pointer.ml:523` was earlier listed here as a
4th residual but the reviewer verified it is FULLY RESTORED by the sub_layout
filter — the fix restored more than originally claimed.)
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

## A1 known limitation — `-i` signature output of some with-bounds is not recompilable (USER-RULED ship-with-doc)

**USER RULING 2026-07-13:** `-i` round-trippability of jkind with-bounds is NOT
required. We ship the current renderer output and DOCUMENT the limitation; no
renderer behavior change was made for A1.

Three with-bound atom classes render, in `-i` signature output, a kind spelling
that does not parse back as a valid kind annotation (copy-pasting the printed
signature into source will not recompile the with-clause):

1. **transitive-abstract product `u & t`** — when an abstract with-bound `t`
   itself carries a with-bound `u`, the honest LDD closure prints `with u & t`
   (the `&`-product of the flattened atoms). `&` is not with-clause surface
   syntax (see the F2 transitive-with-bound-expansion class above).
2. **residue `_`** — atom classes the renderer cannot name (poly-variant,
   first-class module, anonymous type argument, open row) print as `with _`.
   `_` is not a legal with-bound.
3. **bare `KAtom`** — a constructor-atom path is rendered as a bare type
   constructor; where the source kind was written differently (or the path
   needs arguments/qualification the LDD did not retain), the printed form is
   not a faithful re-parseable annotation.

These are display-only: the LDD (the authoritative kind) and all verdicts are
unaffected — only the human-readable `-i` text is non-round-trippable. **Designed
future fix: ANNOTATION-ECHO** (see the design note above): retain the user's
literal source kind annotation on the syntactic construct and prefer it for
display, restoring source-faithful, recompilable bounds for annotated decls
(inferred kinds still reconstruct from the LDD). ANNOTATION-ECHO is the same
mechanism that would let the with_bounds print sidecar retire.

## Codex round-2 fix-forward (Z1-Z5, on top of ff83f52da)

Post-push codex round-2 (verified by rev-ik5fmt) on the Phase-2 range.

- **Z1 [HIGH, wrong output] — param letters reversed in the two-sided Violation
  render.** `format_base_or_kind k1/k2` (jkind.ml) renders "The kind of the first
  is X / must be a subkind of Y"; for a module-inclusion check the compared decls
  are COPIES, so k1/k2's LDD `Param` ids (= param `type_expr` `get_id`, ikind.ml)
  are absent from the live printer `!names` → `resolve_param_name` fails →
  `synthetic` (id-sort) assigns letters, and the copy's source-'a can have a
  higher id than source-'b ⇒ inversion (subsumption/modalities_ikinds.ml:344
  renders both sides as each other's kind; :158 single-param). NOT localized:
  `synthetic` first-appearance was tried and STILL inverts (LDD terms are not
  source-ordered); correct letters need the decls' `type_params`, which the
  `Violation.t` does not carry. FIXED (team-lead-ruled (A), minimized; (B)
  copy-semantics change rejected): `report_with_name`/`report_general` gained an
  optional per-side `param_name_hints` (id→letter map for the offending kind and
  the required kind); `report_general` renders k1 under hint1 and k2 under hint2
  via a new `Jkind.Const.with_param_name_resolver`; includecore's `Jkind` error
  variant carries the hints, built at the `check_decl_jkind` site from
  `decl1.type_params`/`decl2.type_params`. No hint ⇒ resolver unchanged (every
  other `Violation.of_` site). Sweep: the ONLY churn is modalities_ikinds
  (:158 `with 'a`→`with 'b`; :344 each side now matches its own decl),
  param-letter restorations, 0 verdict flips; GAP-2 `-i` repros unchanged.
- **Z2 [fixed] — `&`-product string fallback used raw `Path.name`.** A2 fixed
  only the single-atom out_type path; `string_of_atom` (ikind.ml, the multi-atom
  `&`-product + ctor-arg string fallback) still emitted raw `Path.name`. Routed
  through `oide_of_path` + a `string_of_oide` mirroring `Oprint.print_ident`.
  Non-shadowed output byte-identical; shadowed paths gain `/2`.
- **Z3 [fixed] — semantic externality read not solver-reentrant.**
  `externality_read` (ikind.ml) ran on a shared `create_ctx` + `Solver.round_up`
  (drains global pending-GFP) with no isolation. Wrapped in `create_scratch_ctx`
  + `Ldd.with_isolated_pending`, mirroring the print path. Differential
  unchanged (182 checks / 0 mismatch; positive control still fires).
- **Z4 [fixed] — C1 `implies` mutated layouts at format time.**
  `implies`→`sub_layout`→`Layout.sub`→`Sort.equate` instantiates sort vars while
  formatting. Wrapped each check in `Btype.snapshot`/`backtrack` (sort change log
  wired via types.ml:1213/1704) ⇒ pure, independent, no eval-order-dependent
  provenance, no toplevel-recovery leakage.
- **Z5 [REFUTED] — no action.** Codex lead: a `ctype.ml:4705` fatal + several
  promoted outputs flagged as lossy/contradictory. Refuted by rev-ik5fmt: the
  `ctype.ml:4705` fatal is PRE-EXISTING (not introduced by this range), and the
  lossy `-i` forms (`k1 ... with k1`, `_` residue, `'b & 'a t` grouping) are the
  already-documented + USER-ruled A1 known-limitation classes, not new defects.

## Codex round-3 fix-forward (W1-W3, on top of dfdec85e6)

Round-3 on the Z-fixes (Z3 came back clean). W1 verified as a real regression;
all three fixed, full suite 2330/0 (zero churn — no existing test exercised
these shapes).

- **W1 [HIGH regression, fixed] — Z1 discarded explicit param names.** The Z1
  hint builder forced `letter_of_int` by position, so `type ('left,'right) t`
  rendered its violation kinds with `'a`/`'b` (VERIFIED vs the 92035033e legacy
  binary, which prints `'left`/`'right`). Fix: hint uses the param `Tvar` name
  when present, `letter_of_int` fallback for anonymous. Named repro now == legacy;
  alphabetic case unchanged.
  **V1 clarification (round-4, verified byte-identical vs legacy):** the hints
  are built AFTER `Ctype.unify` (includecore), so with DIFFERENT names per side
  the impl params are aliased to the signature's; "The kind of the first is"
  then renders the impl's kind with the SIG names — and LEGACY does exactly the
  same (it renders the same post-unify `type_expr`s). So this is NOT a further
  regression and needs no pre-unify capture: W1 is correct "up to unification
  aliasing", matching legacy. The earlier phrasing "each kind reads in its own
  decl's namespace" was imprecise — accurate is "reproduces legacy's post-unify
  rendering" (which fixes the Z1 synthetic-letter swap and keeps the aliased
  names legacy shows). Pinned by the different-names regression test.
- **W2 [MED, fixed] — Z2 didn't escape keyword idents.** `string_of_oide`
  concatenated `printed_name` raw, so a type named `and` printed `with and & t`
  not `with \#and & t`. Fix: render via `Format_doc.asprintf "%a"
  !Oprint.out_ident` (authoritative Oprint escaping). Byte-identical for ordinary
  idents. **Residual caveat (codex's subtler point):** `out_ident` names are
  mutable so later path discoveries retroactively disambiguate earlier names;
  stringifying in the `&`-product fallback freezes the name at render time, so a
  disambiguation FIRST discovered after this fallback ran would be missed. This
  is inherent to rendering the honest non-parseable `&`-product as a STRING (vs
  a live `out_type`); it only affects that already-non-round-trippable A1 fallback
  form, not the single-atom paths. Left as-is (display-only, A1-class).
- **W3 [LOW, fixed] — Z4 backtrack not exception-safe.** `pure_implies` skipped
  `Btype.backtrack` if `implies` raised. Fix: `Fun.protect ~finally`.
- **W4 [fixed] — `&`-product string fallback's Param branch didn't escape
  tyvars.** After W1, a named param reaching `string_of_atom`'s Param branch was
  emitted raw (`"'" ^ name`); a keyword-named param would render `'and` not
  `'\#and`, unlike the single-atom `Otyp_var`/Oprint route. Fix: route through
  `Pprintast.tyvar_of_name` (Oprint's `ty_var` escaper). Byte-identical for
  ordinary/synthetic names. Guard: `type ('\#and, 'b) u : value mod portable
  with '\#and` in modalities_ikinds.ml (single-atom keyword-param — the accepted
  fallback: W4's multi-atom branch uses the same escaper but is only reachable
  via a finicky module-mismatch+`&`-product, since single-decl `&`-products
  render synthetic non-keyword letters).

---

## Stage C round_up re-route: ENGINE-DIVERGENCE finding (case (b), NOT shipped)

**Outcome:** the Stage C(ii) attempt to re-route `Jkind.round_up` off legacy
`Base_and_axes.normalize` onto the ikind engine was REVERTED (rebase-dropped;
was commit 036fcf989). `round_up` keeps calling legacy `normalize ~mode:Ignore_best`.
The `Base_and_axes.normalize` fixpoint SURVIVES. (NOTE: this entry was written when
only C(ii) had been reverted; C(i) fold-stop and C(iii) Violation-display were
subsequently reverted too, so the fixpoint is now live on ALL its original callers
-- normalize_decl_jkinds, round_up, Violation.of_, and get_mod_bounds. See the
FINAL LEDGER below.)

**The divergence.** `round_up`'s legacy `Some` floor folds the with-bounds UP into
a bounds-free floor. On the GADT-equation input class it disagrees with every ikind
derivation tried:
- Input class: the jkind produced by `Ctype.add_jkind_equation` <-
  `add_gadt_equation` (`jkind_of_abstract_type_declaration` on the local abstract
  type) over a RECURSIVE with-bound type. Concrete site: `ast-invariants/test.ml`,
  GADT `type _ kind = Implem : Parsetree.structure kind | Interf : Parsetree.signature kind`,
  matched by `let parse : type a. a kind -> ...`. Reached via
  `round_up <- intersect_type_jkind (ctype.ml:3589) <- add_jkind_equation (4654)
  <- add_gadt_equation (4710) <- unify3`. 18 occurrences, 1 shape, corpus-wide
  (whole testsuite); minimal `int list` / recursive-record GADTs do NOT reproduce.
- Floors (Axis_lattice, 11 axes): legacy `[2,1,1,3,3,1,1,3,3,1,2]` (with-bounds
  folded up on the 7 modal axes) vs ikind `[2,1,0,0,0,0,0,0,0,1,2]` (bare
  immutable_data base = SLICE34-PREP §4's with-bounds-EXCLUDED base poly, modal
  axes at bot). ikind <= legacy on every axis, strictly below on the 7 modal ones
  -> UNSOUND for `round_up` (violates `t <= round_up(t)`).
- BOTH ikind modes give the SAME `[2,1,0,...]`: NORMAL-mode `to_terms` name-free
  join AND ROUND_UP-mode `Solver.round_up` (the `crossing_of_jkind` primitive).
  So it is not a floor-primitive choice: `ckind_of_jkind` on the equation-jkind
  does not surface the recursive with-bound contributions that legacy
  `Ignore_best normalize` expands/folds. Reproducing legacy's fold = rebuilding
  normalize inside the ikind engine = the expensive branch, declined and deferred
  to the user as a follow-up (pinned by the regression test below).

**Error-taxonomy lesson.** The first fix attempt was itself instructive: when I
noticed `Solver.round_up` (Round_up mode) collapses abstract with-bounds to top
and thus loses `round_up`'s load-bearing `None` (irreducible) path, I OVER-
CORRECTED — abandoning `Solver.round_up` for the floor ENTIRELY and using the
name-free const-base idiom (correct only for crossing/print, where with-bounds are
carried separately). That produced the bounds-EXCLUDED base, byte-identical to the
documented base poly — a mechanism, not a coincidence. Lesson: rescue the ONE part
that needs it (None-detection), don't discard the primitive that was right for the
FLOOR. (In the end even the correct primitive under-computed — genuine engine
divergence, case (b).)

**Detector coverage.** The armed round_up validate-differential CAUGHT this on real
code (ast-invariants) that two synthetic corpora + a fired seeded-fault had all
passed — the value of gating the re-route with the differential + running the real
suite. With the re-route reverted, that differential is gone; `round_up` fidelity
is now just "it still calls legacy," and the surviving Stage-B crossing differential
+ externality differential + OXCAML_IKINDS_VALIDATE recompute harness cover the rest.

## Gotchas for successors

- **`ocamlformat --check` exit code via a pipe is a LIE.** `ocamlformat ... --check
  f | head` (or `| tail`) makes `$?` reflect `head`, not ocamlformat, so a DIRTY
  file reads as clean. Verify the exit code DIRECTLY: `ocamlformat ... --check f &&
  echo CLEAN || echo DIRTY`. (This masked Stage A/B leaving jkind/ikind dirty;
  fixed in the fmt-hygiene commit. ocamlformat version is pinned `0.29.0` in
  `typing/.ocamlformat`.)
- **`/tmp` is a SEPARATE 4G filesystem** (`/dev/mapper/vg01-tmp`), distinct from
  the 500G+ `/usr/local/home`. A full `make compiler`/`make test` default-TMPDIRs
  to `/tmp` and fills it -> `Error: I/O error: No space left on device` even though
  `df .` shows hundreds of GB free. Route builds/tests to the big fs:
  `TMPDIR=<worktree>/_tmp/ttmp make ...`. Stray `_tmp/bundled_*.s` (262M each) and
  `_runtest/` accumulate; both are regenerable and safe to delete from your own
  worktree.

## Stage C fold-stop (C(i)): SECOND engine-divergence finding (-principal), REVERTED

The Stage C(i) fold-stop -- `normalize_decl_jkinds` storing the RAW decl jkind
instead of the `Require_best`-normalized one -- was REVERTED (rebase-dropped, was
commit 7a3708c6b) per USER RULING, on evidence of a -principal verdict flip.

- Symptom: the full non-validate suite showed 0 NORMAL-mode flips (run-expect green
  corpus-wide) but 29 tests failing check-program-output under -PRINCIPAL, all in
  jkind/kind dirs.
- Attribution chain: Stage A (e224e71e8) proven clean by the wave's own full-suite
  gate (2330/0, includes check-program-output); Stage B (0d1f58d3f) built and
  targeted-run CLEAN on gadt_ikinds incl. -principal; HEAD-with-C(i) FAILS the same
  -principal check. C(iii) is display-only and cannot cause success->error. => C(i).
- Mechanism: C(i) runs the annotation sub-check `Ikind.sub_jkind_l` on the RAW decl
  jkind (typedecl.ml:3609 raise site). The sub-check was ALREADY ikind-native, so
  this is NOT a missed-consumer re-route; what changed is raw-vs-`Require_best`-folded
  INPUT. Under -principal the raw inferred kind `value non_float mod portable` FAILS
  the annotation `value mod portable with (type : value mod portable) abstract`, which
  the folded jkind passed. Class: existential + `with (type:...) abstract`
  (gadt_ikinds `existential_abstract`). Regression-pinned (see below).

## UNIFIED PATTERN (the real deliverable of what Stage C did NOT ship)

Two independent ikind-engine divergences, same root, found on two different stricter
paths -- the ikind engine ANSWERING FROM RAW with-bound forms is WEAKER than the
legacy `Base_and_axes.normalize` FOLD on complex with-bound shapes:

  1. round_up (C(ii)): on the GADT-equation jkind (add_gadt_equation ->
     intersect_type_jkind), the ikind derivation yields the with-bounds-EXCLUDED
     const base [2,1,0,0,0,0,0,0,0,1,2] where legacy folds them up to
     [2,1,1,3,3,1,1,3,3,1,2]. Both NORMAL and ROUND_UP ikind modes give the base.
  2. sub-check (C(i)): under -principal, sub_jkind_l on the RAW decl jkind rejects
     `value non_float mod portable` vs `... with (type:...) abstract` where the
     folded jkind is accepted.

Turning "delete the normalize fold" from a plausible-sounding task into a
precisely-scoped ENGINE workstream: to delete the fold, the ikind engine must
reproduce, on raw with-bound forms, what `Ignore_best`/`Require_best normalize`
computes by folding -- for the round_up floor AND the -principal sub-check. Until
then the fixpoint must stay. Both shapes are pinned as regression tests
(round_up_gadt_equation_ikinds.ml; the gadt_ikinds existential_abstract case).

## Probe/harness preservation

The fold-stop cross-unit probe sources (unit A foldable/param/allow_any decls +
unit B crossing/subkind/representation exercises) and the round_up floor-dump
instrumentation are preserved under `_tmp/probe/`, `_tmp/flip/`, `_tmp/synth*/` in
the ik5m-work worktree -- the harness for whoever retries fold-stop after the engine
work. (They die with the code revert but the SOURCES are kept.)

## STAGE5F FINAL LEDGER (PAYOFF-2+3 deletion wave)

SHIPS (on branch ik/pr2-print-deletions, FINAL chain: Stage A -> Stage B -> fmt ->
knowledge):
  - Stage A: base_and_axes floor field retype mod_bounds -> ikind_floor :
    Axis_lattice.t. MAGIC: the retype changes the MARSHALLED Types shape, which is
    embedded in the cmi AND in every typed-tree/signature-bearing format -- crucially
    the bin-annot .cmt/.cmti/.cms (they marshal base_and_axes), plus cmo/cmx/exec.
    So the SHARED MAGIC_NUMBER__VERSION is bumped 581 -> 582 (invalidates cmt/cms/
    cmo/cmx/exec; over-inclusive but safe -- artifacts rebuilt every roll), and the
    CMI keeps its own override at I583 (one ahead, for the earlier named-terms cmi
    wire change). REJECTION-TESTED BOTH DIRECTIONS: (a) a stale I582 cmi is rejected
    by the new compiler ("not a compiled interface for this version"); (b) a stale
    T581 bin-annot .cmt (T-magic-headed, codex's garbage-decode path) is now rejected
    by the cmt reader ("compiled typedtree file (cmt) for an older version") instead
    of decoding the old boxed mod_bounds record as a garbage Axis_lattice int. [The
    F1 review must-fix: the initial bump was cmi-only (I583, shared 581), which left
    the T-headed .cmt at T581 = cross-version-matching and garbage-decoding; the
    shared bump fixes it.]
  - Stage B: to_unsafe_mode_crossing re-routed to the ikind engine (BASE-ONLY
    floor -- with-bounds STRIPPED before lowering, carried separately in
    unsafe_with_bounds) + permanent validate differential; seeded-fault-proven,
    blast-radius clean. [The initial re-route lowered the WHOLE jkind and
    double-counted CONCRETE bounds -- the W-1 verdict flip; fixed fix-forward,
    see the W-1 section.]
  - fmt hygiene (ocamlformat 0.29.0; formats the Stage A retype + Stage B hook
    jkind/ikind lines).
  - knowledge: this ledger + the two engine-divergence entries + three regression
    pins (round_up_gadt_equation_ikinds.ml, foldstop_principal_subcheck_ikinds.ml,
    reexport_unsafe_crossing_ikinds.ml [W-1]).

REVERTED (all of Stage C's semantic deletions; documented + regression-pinned where
applicable; engine-workstream follow-up):
  - C(i) fold-stop (write-path): -principal sub_jkind_l divergence on raw vs folded
    decl jkind (existential + with-(type:...)-abstract). Engine evidence.
  - C(ii) round_up re-route: GADT-equation floor divergence (ikind base vs legacy
    fold). Engine evidence.
  - C(iii) Violation.of_ display off normalize: NOT an engine divergence -- it is
    display-quality. Under -principal the un-normalized annotation rendered
    self-contradictory diagnostics (`mod uncontended <= mod uncontended` etc.),
    garbage-class under the promotion doctrine (deliberate/justified, NEVER wrong-on-
    its-face) and outside the user's error-text waiver. Its justification was the
    fold deletion, which died on engine evidence; per the uniformity-breakage
    doctrine C(iii) dies with it rather than being re-accommodated (a normalize-for-
    display refinement would be the pre-C(iii) state with extra steps).

RESIDUAL: the `Base_and_axes.normalize` fixpoint SURVIVES, fully live on BOTH the
write path (normalize_decl_jkinds fold, restored by the C(i) revert) and the read
path (round_up via legacy; get_mod_bounds feeding the externality validate
differential; Violation.of_ display, restored by the C(iii) revert). `Require_best`
mode is live again.

LOC NET: POSITIVE (the wave ADDS code net) -- the honest outcome. The headline
"delete the normalize fixpoint" did NOT happen: ALL THREE of Stage C's changes that
touched the fold (C(i)/C(ii)/C(iii)) were reverted -- the first two on engine-
divergence evidence, the third on a display-quality regression whose justification
died with them. What ships is the S8 retype (field + boundary conversions) and the
crossing re-route + its differential -- both ADD/move code; nothing structural was
deleted. The deliverable is (a) the retype/crossing-re-route groundwork and (b) the
two precisely-characterized, regression-pinned engine divergences that turn "delete
normalize" into a scoped engine workstream: the ikind engine must reproduce, on RAW
with-bound forms, what the legacy fold computes -- for the round_up floor AND the
-principal sub-check -- before the fold can be removed from any path.

## PERF A/B (endgame) -- recorded honestly, box was loaded

Interleaved (perf_time.py, ik5prep-work/_tmp/perf) base = c97be81e9 (built+installed
in prwork/ik5f6-legacybase) vs final = reduced chain (A+B) @ 87fdb2967. ocamlc.opt
boot binaries, each using its own _prefix stdlib (base I582, final I583).

  RUN 1 (reps 11): perf_records median 326.44 -> 333.45 = +2.15% (min 311.88 -> 312.50 = +0.20%);
                   perf_plain   median 1495.72 -> 1491.52 = -0.28% (min 1446.20 -> 1448.23 = +0.12%)
  RUN 2 (reps 17): perf_records median 331.41 -> 327.72 = -1.11% (min 305.70 -> 314.11 = +2.75%);
                   perf_plain   median 1489.36 -> 1518.56 = +1.96% (min 1421.82 -> 1438.38 = +1.16%)
  Box load (uptime 1/5/15-min) ~ 31-51 throughout (prep baseline was taken at 7-14).

CONCLUSION: the Δ SIGN FLIPS between runs on BOTH cells (records +2.15% -> -1.11%;
plain -0.28% -> +1.96%), so there is no consistent-sign regression -- the wave's
impact is BELOW the measurement noise floor at the available box load (~45).
A consistent-sign >1% regression is ruled out. Sub-1% precision was NOT achievable
this window (load too high); this is NOT a claim of "<1%", it is "below noise floor,
no consistent regression." Consistent with the shipped surface being cold-path
(Stage A field retype: hot ikind reads drop a to_axis_lattice + a couple boundary
conversions; Stage B crossing re-route only on the rare allow_any_crossing decl path
+ a validate-only differential). A quiet-box re-measure (17+ reps, load <10) is
queued as a pre-push backstop; a consistent-sign >1% there = STOP-before-push.

## W-1 fix-forward (crossing_read concrete-bound double-count) -- CONFIRMED verdict flip on the pushed head

DEFECT. Stage B's crossing_read (ikind.ml) lowered the WHOLE jkind through the
solver and took the join of its name-free const terms as the crossing floor.
That was correct ONLY for name-CARRYING with-bounds (which stay as named terms
and are skipped by the [names = []] join). But a CONCRETE with-bound -- [int],
[int ref], [int list] -- resolves through the solver to a NAME-FREE constant, so
it landed in the join and was folded INTO the floor. [to_unsafe_mode_crossing]
ALSO carries the with-bounds separately in [unsafe_with_bounds] (jkind.ml). Net:
concrete bounds were DOUBLE-COUNTED -- once in the inflated floor, once in
unsafe_with_bounds. (The original Stage-B no-double-count argument held only for
name-carrying bounds.)

TWO CONFIRMED CONSEQUENCES (codex W-1; rev-ik5fmt reproduced both):
  1. Under OXCAML_IKINDS_VALIDATE, a VALID decl -- [type t : value mod
     everything with int ref = A of string [@@unsafe_allow_any_mode_crossing]]
     -- goes FATAL ("ikind-derived crossing disagrees with legacy stored-floor
     crossing"): the buggy read inflates the floor, the legacy stored floor
     (bounds-excluded) does not, so the differential trips on a decl that is fine.
  2. VERDICT FLIP: the buggy pushed head ACCEPTS a re-export
     (equal_unsafe_mode_crossing, types.ml) that the pre-ikind compiler
     c97be81e9 REJECTS -- the inflated M1.t floor swamps a real crossing
     difference so the two crossings compare equal, WEAKENING the unsafe-crossing
     guardrail. (CI does not run VALIDATE, so consequence 2 is what actually
     reached the pushed head; the pin below catches it without VALIDATE.)

FIX (one commit on a0c9ea09e). crossing_read now STRIPS with-bounds
([with_bounds = No_with_bounds]) before lowering, so the floor is BASE-ONLY --
exactly what the legacy stored [ikind_floor] held. Contract, now recorded in the
code (ikind.ml block comment + jkind.mli doc): umc floor = bounds-EXCLUDED
(bounds travel in unsafe_with_bounds) vs round_up floor = bounds-FOLDED. OPPOSITE
contracts, so neither may be "fixed" into the other later.

WHY THE STAGE-B GATES MISSED IT (the real lesson). Stage B's differential and
seeded-fault were exercised on name-CARRYING with-bound shapes (the round_up
divergence family), where the name-free join genuinely excludes the bounds -- so
the buggy read AGREED with legacy there and the differential stayed clean. The
bug lives on a DIFFERENT shape (concrete, name-free bounds) that the Stage-B
corpus never fed in. A differential is only as strong as the shapes fed to it --
the same lesson as this campaign's earlier false-GEM (a "verified" result that
held only on the shapes probed, not on the shape that mattered). The fix's gate
is therefore SHAPE-TARGETED at the missing class.

GATE (non-vacuous this time; proven BOTH directions; corpus in _tmp/w1gate/:
value/immutable_data/mutable_data mod ... with int / int ref / int list,
allow_any, single-decl AND cross-module re-export):
  - FIXED binary, single_decls under VALIDATE: checks=38 mismatches=0, exit 0
    (CLEAN). reexport: REJECTS (exit 2) with the guardrail error BYTE-IDENTICAL
    to base c97be81e9; validate clean (checks=18 mismatches=0).
  - BUGGY binary (crossing_read body reverted to the whole-jkind lowering, boot
    rebuilt): single_decls under VALIDATE goes FATAL (FIRES); reexport ACCEPTS
    (exit 0 = the flip).
  So the corpus DISTINGUISHES buggy from fixed -- the positive control fires on
  the unfixed binary and is clean on the fixed one. (rev's repro (a),
  _tmp/rev5/w1_validate.ml, is the same positive control, independently proven
  firing.)

REGRESSION PIN (CI-visible, no VALIDATE needed):
testsuite/tests/typing-jkind-bounds/reexport_unsafe_crossing_ikinds.ml -- the
re-export is REJECTED by the equal_unsafe_mode_crossing guardrail (M1.t and M2.t
have different unsafe mode crossing). A future crossing_read that folds concrete
bounds back in would make it compile, re-flipping the verdict; the pin catches
that in ordinary CI (this is the guard the pushed head lacked).

W-2 (no code action; note only). The Stage-B validate differential (kept
permanent) is only an ACTIVE guard for shapes actually compiled under
OXCAML_IKINDS_VALIDATE, which CI does not run. Wiring the concrete-bound corpus
(_tmp/w1gate/) into a VALIDATE-armed CI lane would turn it from a manual gate
into an active guard; recorded as a follow-up.

W-3 (folded into this commit). The m4 magic comment now names the LOAD-BEARING
formats (cmi + cmt/cmti bin-annot, which actually marshal base_and_axes) vs the
over-inclusive-but-safe cmo/cmx/exec bump. No magic-value change (shared 582,
cmi I583 unchanged).
