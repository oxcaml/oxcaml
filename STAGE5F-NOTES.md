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
