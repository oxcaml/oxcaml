# Stage 5 format lock-in notes: named-terms cmi payload + magic bump

Campaign: ikind-unification. Branch `ik/stage5-format-lockin` off `6b3ae002e`
(integrated stage-5a + stage-5b head, CI 36/36 green). This slice is the 5b
items 2-3 that were split out of the soundness-gate package (STAGE5-DESIGN.md
§5b AS-BUILT), plus two review amendments. The Residue soundness gate
(`Rigid_name.Residue`) was already shipped and integrated at HEAD; it is NOT
redone here.

I own ikind cmi persistence (`file_formats/cmi_format.ml` boundary, the
`type_ikind` representation in `typing/types.ml{,i}`, the exhaustiveness arms in
`typing/ikind.ml`) and the cmi magic number (`build-aux/ocaml_version.m4`).

---

## Work items (STAGE5-DESIGN.md §5b items 2-3 + amendments)

1. **Switch cmi ikind persistence to the explicit named-terms format**
   (`Ldd.to_terms`/`of_terms`, the STAGE4C decomposition) instead of the raw
   `Ldd` node-DAG marshal. Locks the cmi representation in before stage 5d
   deletes the recompute-from-legacy safety net.
2. **CMI magic bump** `Caml1999I581 -> Caml1999I582`.
3. **Review amendment 1:** add `Residue` atoms to the `to_terms`/`of_terms`
   round-trip test corpus (load-bearing once named-terms marshals via
   `to_terms`).
4. **Review amendment 2:** the bump lands NO LATER than this slice.

---

## As-built design

### Representation (typing/types.ml{,i})

    type ikind_term = Axis_lattice.t * Rigid_name.t list
    type saved_constructor_ikind =
      { saved_base : ikind_term list; saved_coeffs : ikind_term list array }
    type constructor_ikind_entry =
      | Constructor_ikind of constructor_ikind   (* live: Ldd nodes *)
      | No_constructor_ikind of string
      | Saved_ikind of saved_constructor_ikind   (* on-disk only *)

`Saved_ikind` is the named-terms wire form. `constructor_ikind_to_saved`
(`Ldd.to_terms` per base/coeff) and `constructor_ikind_of_saved`
(`Ldd.of_terms`) are the encode/decode.

### Boundary (file_formats/cmi_format.ml)

`serialize` maps `Constructor_ikind -> Saved_ikind`; `deserialize` maps back via
`of_terms` (recursing into `type_unboxed_version` and, through the mapper's
`map_signature`, into nested module signatures). The conversion is confined to
the cmi serialize/deserialize boundary: **live code only ever observes
`Constructor_ikind`** (rehydrated at the deserialize boundary, before any
consumer or the import subst), so the existing node-based atom remap in
`substitute_decl_ikind_with_lookup` (Atom/KAtom path rewriting; Residue/Unknown
identity) is entirely unchanged. This is a pure wire-format change; the semantic
remap logic is untouched. `ikind.ml` gains `Misc.fatal_error` arms at the two
`type_ikind` match sites (lookup, subst) enforcing the invariant that a
`Saved_ikind` never reaches live code.

Why this shape: `to_terms`/`of_terms` decouple the cmi from the `Ldd` node-block
/ `Axis_lattice` internal layout (the STAGE4D D2 robustness concern), and give an
explicit place to reason about the payload. Residue atoms (the stage-5b
soundness gate) ride through the terms as ordinary `Rigid_name.Residue` names,
collision-free by construction.

### Coexistence window then bisectable delete (STAGE5-DESIGN.md §C.1)

- **Window commit (`2aa6b7b8f`):** `saved_constructor_ikind` additionally carried
  `saved_legacy : constructor_ikind option = Some ci` -- the raw-DAG node form
  ALONGSIDE the named-terms payload, so both load paths rode the SAME cmi. Under
  `OXCAML_IKINDS_VALIDATE` the deserialize boundary cross-checked the LDD
  rehydrated from the on-disk named-terms payload against that raw-DAG legacy
  node (`Ldd.leq_with_reason` both directions), fatal on mismatch. The magic
  bump landed in this commit.
- **Delete commit (`c13422892`):** dropped `saved_legacy` and the cross-check;
  the named-terms payload is the sole wire. The cmi no longer marshals any raw
  `Ldd` node DAG.

### Magic bump (build-aux/ocaml_version.m4)

CMI magic `Caml1999I581 -> Caml1999I582`. Two DEVIATIONS from the doc's
"config.common.ml two-line change", both documented in the C2 commit message:

1. This tree derives the cmi magic from an autoconf substitution
   (`@CMI_MAGIC_NUMBER@` in `utils/config.common.ml.in`), NOT a literal in a
   checked-in `config.common.ml`. So the bump lives in the m4 source and is
   propagated by `autoconf27` + reconfigure. (`configure` is generated and
   gitignored; not committed.)
2. **CMI-only** bump: only the `.cmi` format changed (the `.cmt`/`.cmo`/`.cmx`
   node marshaling is untouched, since the change is confined to the cmi
   serialize/deserialize boundary), so their magic versions stay at 581. This is
   tighter and more correct than a shared-version bump, which would needlessly
   invalidate unchanged artifacts. Achieved by overriding `CMI__MAGIC_NUMBER` in
   the m4 while leaving `MAGIC_NUMBER__VERSION` at 581.

Cost, as planned: a full stdlib/world rebuild (done this slice).

Intra-stage-5 note: the window (`2aa6b7b8f`) and delete (`c13422892`) commits both
carry magic I582 but have incompatible marshal layouts (saved_legacy present vs
absent). This is safe on the campaign branch because the build graph rebuilds
every cmi consumer when the compiler changes (STAGE5-DESIGN.md
MAGIC-BUMP-DECISION); no mixed-I582 world arises within a build or a fresh
bisect. The magic gates stage-5 cmis against pre-stage-5 (I581) compilers, which
is the property that matters for 5d.

---

## Acceptance evidence (final tree, `c13422892`, `_prefix/bin/ocamlc.opt`)

All runs: `_prefix/bin/ocamlc.opt`, `-extension layouts_beta`, `TMPDIR` local.

### Magic bump verification

    $ ocamlc.opt -config | grep cmi_magic
    cmi_magic_number: Caml1999I582

Magic gate (new I582 compiler reading version-flipped copies of a real cmi):

    reading I581 (older) cmi -> "It seems to be for an older version of OCaml."
    reading I583 (newer) cmi -> "It seems to be for a newer version of OCaml."

Both are the `Cmi_format.read_cmi_lazy` rejection path. This proves the magic
gate is LIVE and discriminates on exactly the 581/582/583 boundary (not a no-op).
`read_cmi_lazy` compares the file magic against `Config.cmi_magic_number` and
classifies `<` as "older", `>` as "newer" -- shared code, `Config`-relative. So a
PRE-BUMP compiler (`Config = Caml1999I581`) reading a new I582 cmi takes the
`I582 > I581` branch -> "newer version" -> rejected, by the identical mechanism
demonstrated above. A pre-bump compiler therefore rejects the new cmis.

### §C.1 transition validation -- named-terms load path == from-legacy recompute

`OXCAML_IKINDS_VALIDATE=1 -ikinds-debug`, `[ikind-validate]` summaries:

cross_unit_ikinds (liba defines; client consumes A's persisted decl ikinds):

    liba:   checks=18 mismatches=0 ... decl-ikind stored=4  recomputed=4  ... param-id-collisions=0
    client: checks=70 mismatches=0 ... decl-ikind stored=18 recomputed=5  ... param-id-collisions=0

  -> 0 HARD. The client consumes 18 decl ikinds rehydrated from the named-terms
     payload and every one agrees with the from-legacy recompute.

residue_import_ikinds (res_lam = mixmod5-family recursive-module fixpoint;
consumer imports its Residue-bearing decls):

    res_lam:  checks=1414 mismatches=0 class_b=3 residue_trusted=3 ... residue-neutralized=3 imported-residues=0 param-id-collisions=0
    consumer: checks=26   mismatches=0 ... residue-neutralized=0 imported-residues=9 param-id-collisions=0

  -> 0 HARD. `residue-neutralized=3` (save) and `imported-residues=9` (import),
     `param-id-collisions=0` -- MATCHES the shipped 5b evidence
     (STAGE5-DESIGN.md AS-BUILT / the residue_import consumer). Residue atoms
     cross the cmi through the named-terms payload unchanged.

Per-decl round-trip `of_terms (to_terms n) ~= n`: `ldd_terms_roundtrip_test`
(oxcaml/tests/typing), now including three unit-qualified `Residue` atoms and a
Residue-distinctness assertion:

    ldd_terms_roundtrip_test: OK

On-disk differential (window commit `2aa6b7b8f`, since removed): the deserialize
boundary cross-checked rehydrated-from-payload vs raw-DAG legacy over the same
corpus under validate -> 0 fatal (0 HARD on the on-disk payload).

### Seeded faults remain non-vacuous after the format switch

    OXCAML_IKIND_SAVE_FAULT=1 (res_lam saved corrupted; consumer validated):
      consumer: mismatches=8  (8 HARD) -- matches STAGE4D.
    OXCAML_IKIND_RESIDUE_FAULT=1 (res_lam, the CLASS-B unit):
      res_lam: mismatches=3 residue_trusted=3  (3 HARD) -- the residue-trust
      reference is perturbed and caught; the trust boundary is not a blind spot.

Both faults still produce HARD through the named-terms format -> the harness is
non-vacuous.

### Suites (final tree `c13422892`)

    make test-one DIR=typing-jkind-bounds : 74 passed, 0 failed
      (incl. cross_unit_ikinds, residue_import_ikinds)
    make test-one DIR=typing-modules      : 54 passed, 0 failed (incl. mixmod5)
    dune @oxcaml/tests/typing/runtest     : ldd_terms_roundtrip_test OK
    -no-ikinds acceptance                 : liba/client/res_lam/consumer all accept
    boot-compiler + full make + install   : exit 0 (world rebuilt after the bump)

Flag-off default (validate off) is the default suite configuration above;
`-no-ikinds` accepts the cross-unit + residue corpus exactly as before.

---

## Deviations from the design doc (summary)

1. Magic bump lives in `build-aux/ocaml_version.m4` (autoconf substitution), not
   a checked-in `config.common.ml`; propagated via `autoconf27` + reconfigure.
2. CMI-ONLY bump (581->582 for `.cmi` alone; `.cmt`/`.cmo`/`.cmx` stay 581),
   tighter than a shared-version bump because only the cmi format changed.
3. The named-terms conversion is done at the cmi serialize/deserialize boundary
   (so live code only ever holds `Constructor_ikind` and the existing node-based
   atom remap is reused unchanged), rather than remapping Names in term-space.
   Functionally equivalent (of_terms/map_rigid commute over the LDD algebra) and
   lower-risk (reuses the STAGE4D-proven node remap path).

## Not done / out of scope (belongs to later slices)

- Deleting the legacy `mod_bounds`/`with_bounds` engine and the
  recompute-from-legacy reference (stage 5d). The named-terms cmi is the
  prerequisite this slice locks in; the recompute reference still exists and the
  §C.1 differential above depends on it.
