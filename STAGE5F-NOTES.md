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

**Re-churn:** the ~25 M4-promoted expects carrying the false provenance are
re-churned to the corrected (pre-M4-equivalent) text. Tally: _pending build +
re-churn_. Each corrected expect reads like the BEFORE column of the verification
table (correct provenance restored).

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
