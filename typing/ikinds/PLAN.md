# ikinds plan

This file tracks the current implementation plan derived from
`typing/ikinds/TODO.md`.

Benchmark results are append-only in `typing/ikinds/benchresults.txt`.

## Principles

- Keep changes incremental and bisectable.
- Prefer algorithmic wins before invasive refactors.
- Use `make boot-compiler` as the fast check.
- Before declaring success, run targeted promote tests and check for diffs:

```sh
make promote-one DIR=typing-jkind-bounds
make promote-one DIR=typing-layouts-products
make promote-one DIR=typing-layouts
make promote-one DIR=typing-modes
make promote-one DIR=typing-modules
make promote-one DIR=typing-layouts-or-null
git diff
```

Any resulting diffs (especially under `testsuite/`) are behavioral changes that
need review.

## Work streams

### Solver hot path (ikinds)

**Phase 1: Quick wins, low risk (DONE)**

-[done] Enforce subtract-normal form for `Types.constructor_ikind` via a smart
  constructor:
  - Add `Types.constructor_ikind_create ~base ~coeffs` that stores
    `coeffs[i] := Ldd.sub_subsets coeffs[i] base`.
  - Use it everywhere we construct a `constructor_ikind`:
    `Ikinds.type_declaration_ikind`, `Ikinds.type_declaration_ikind_of_jkind`,
    and any rewriting/substitution that might disturb the invariant.
-[done] Remove obvious allocation hotspots:
  - Avoid `List.of_seq` in `ckind_of_jkind`.
  - Avoid repeated `List.nth_opt` in hot loops (use arrays where helpful).
  - Pre-size hash tables where we can estimate sizes.

**Phase 2: Context closure optimization (DONE)**

-[done] Remove the `ctx -> ...` closure indirection (`JK.ckind`) in `JK.env`
  and `kind_of`.
  - Change `env.kind_of` to `ctx -> ty -> kind`.
  - Update `kind_of` to take `ctx` as an explicit argument, avoiding closure
    allocation for every type node traversal.
  - This is a high-impact optimization for the hot path.

**Phase 3: Main algorithm improvements (DONE)**

For mode crossing we are going to round up at the end so it doesn't make much
sense to first compute a complex formula with many variables and params in it,
and then round it up. So we want to implement a different ikinds mode, which is
either normal or round up mode. In round up mode, we map all variables to top
eagerly, instead of doing it at the end. This should keep formulas small and
efficient.

Can you implement this mode? Don't cause code duplication, just pass around
which mode we are in.

**Phase 4: Reduce memoization pressure (DONE)**

-[done] Limit `ty_to_kind` memoization to types that are potentially circular
  (polymorphic variants); keep `constr_to_coeffs` memoization.
- Re-check whether the remaining hotspots justify larger refactors.
- For types that don't need the memoization, we can just recurse, without
  creating ldd variables for recursing that are later solved.

**Phase 5: Preserve ikinds more broadly**

- Reduce `Types.ikind_reset` cases where substitution/renaming can preserve the
  cached constructor ikind (use `substitute_decl_ikind_with_lookup`).
- Consider keeping ikinds for unboxed-version aliases where safe.
- Inventory: current `Types.ikind_reset` sites (and preservation strategy)
  - `typing/typedecl.ml:313` `transl_declaration initial unboxed`
    - Jkind: initial `type_jkind` chosen for placeholder decl.
    - Preserve: no prior ikind; compute/store ikind once decl is final.
  - `typing/typedecl.ml:334` `transl_declaration initial`
    - Jkind: initial `type_jkind` chosen for placeholder decl.
    - Preserve: no prior ikind; compute/store ikind once decl is final.
  - `typing/typedecl.ml:1090` `update_decl_jkind initial`
    - Jkind: seeds `type_jkind` before later adjustments.
    - Preserve: if an ikind existed, recompute after the final adjustment.
  - `typing/typedecl.ml:1269` `derive_unboxed_versions`
    - Jkind: derives an unboxed-version decl's `type_jkind`.
    - Preserve: either recompute, or map boxed/unboxed ikinds when aliasing.
  - `typing/typedecl.ml:2154` `update_decl_jkind open`
    - Jkind: rewrites `type_jkind` for open types during normalization.
    - Preserve: recompute ikind from the updated decl (no simple rename).
  - `typing/typedecl.ml:2165` `update_decl_jkind record`
    - Jkind: rewrites record layout/jkind (may change rep/labels).
    - Preserve: recompute ikind from the updated decl.
  - `typing/typedecl.ml:2194` `update_decl_jkind unboxed record`
    - Jkind: recomputes unboxed-record product jkind/sorts.
    - Preserve: recompute ikind from the updated decl.
  - `typing/typedecl.ml:2211` `update_decl_jkind variant`
    - Jkind: rewrites variant rep/jkind (may change constructor args).
    - Preserve: recompute ikind from the updated decl.
  - `typing/typedecl.ml:2793` `name_recursion`
    - Jkind: unchanged; rewires manifests so recursion goes via the path.
    - Preserve: keep existing ikind, or substitute via a path-only lookup.
  - `typing/typedecl.ml:4130` `transl_with_constraint unboxed`
    - Jkind: copies the signature's unboxed `type_jkind`.
    - Preserve: prefer copying/substituting the signature's unboxed ikind.
  - `typing/typedecl.ml:4170` `transl_with_constraint`
    - Jkind: copies signature `type_jkind`, installs a manifest.
    - Preserve: copy/substitute signature ikind; fall back to recompute.
  - `typing/typedecl.ml:4275` `transl_package_constraint`
    - Jkind: dummy/temporary decl for package constraint checking.
    - Preserve: not worth preserving (temporary decl thrown away).
  - `typing/typedecl.ml:4303` `abstract_type_decl`
    - Jkind: approximate abstract decl for typing/inclusion.
    - Preserve: not worth preserving (approximation).
  - `typing/typedecl.ml:4322` `abstract_type_decl unboxed`
    - Jkind: approximate unboxed decl for typing/inclusion.
    - Preserve: not worth preserving (approximation).
  - `typing/typemod.ml:900` `merge constraint temporary`
    - Jkind: temporary decl used only for checking constraints.
    - Preserve: not worth preserving (temporary).
  - `typing/typemod.ml:1147` `package constraint removal`
    - Jkind: temporary package-constraint decl; drops temporary manifest.
    - Preserve: not worth preserving (temporary).
  - `typing/typemod.ml:2759` `package_constraints_sig`
    - Jkind: unchanged; installs a manifest for unpacked packages.
    - Preserve: keep existing ikind, or substitute via the same path mapping.
  - `typing/env.ml:1511` `env unboxed alias`
    - Jkind: uses the aliased unboxed-version `type_jkind`.
    - Preserve: reuse aliased unboxed ikind with a path renaming if needed.
  - `typing/ctype.ml:1417` `new_local_type`
    - Jkind: synthetic decl; used for local constraint solving.
    - Preserve: not worth preserving (fresh type).
  - `typing/ctype.ml:7381` `nondep_type_decl`
    - Jkind: maps `type_jkind` through `nondep_type_rec`/round-up fallback.
    - Preserve: apply the same mapping to the ikind via lookup substitution.
  - `typing/mtype.ml:109` `strengthen abstract`
    - Jkind: unchanged; adds a manifest `Tconstr(path, ...)`.
    - Preserve: keep existing ikind; no recomputation needed.
  - `typing/mtype.ml:117` `strengthen manifest`
    - Jkind: unchanged; adds a manifest `Tconstr(path, ...)`.
    - Preserve: keep existing ikind; no recomputation needed.
  - `typing/mtype.ml:595` `enrich unboxed`
    - Jkind: unchanged; installs an unboxed manifest for compatibility.
    - Preserve: keep/substitute existing ikind with the same path mapping.
  - `typing/mtype.ml:605` `enrich manifest`
    - Jkind: unchanged; installs a manifest for compatibility.
    - Preserve: keep/substitute existing ikind with the same path mapping.
  - `typing/predef.ml:375` `predef unboxed ...`
    - Jkind: built-in types; jkind is chosen by `Predef`.
    - Preserve: can compute once (or leave reset; not hot).
  - `typing/predef.ml:396` `predef ...` (arity 0)
    - Jkind: built-in types; jkind is chosen by `Predef`.
    - Preserve: can compute once (or leave reset; not hot).
  - `typing/predef.ml:436` `predef ...` (arity 1)
    - Jkind: built-in types; jkind is chosen by `Predef`.
    - Preserve: can compute once (or leave reset; not hot).
  - `typing/predef.ml:463` `predef ...` (arity 2)
    - Jkind: built-in types; jkind is chosen by `Predef`.
    - Preserve: can compute once (or leave reset; not hot).
  - `typing/datarepr.ml:86` `datarepr boxed record`
    - Jkind: fresh decl created during datarepr computation.
    - Preserve: not worth preserving (helper decl).
  - `typing/typeclass.ml:1624` `typeclass temp_abbrev`
    - Jkind: synthetic abbrev for class/object typing.
    - Preserve: not worth preserving (temporary).
  - `typing/typeclass.ml:1856` `typeclass temp_abbrev`
    - Jkind: synthetic abbrev for class/object typing.
    - Preserve: not worth preserving (temporary).
  - `typing/printtyp.ml:2541` `print dummy`
    - Jkind: dummy decl used only for printing.
    - Preserve: not worth preserving.
  - `typing/ikinds/ikinds.ml:651` `ikinds disabled`
    - Jkind: not relevant; ikinds are globally gated off.
    - Preserve: N/A.
  - `typing/ikinds/ikinds.ml:675` `ikinds disabled`
    - Jkind: not relevant; ikinds are globally gated off.
    - Preserve: N/A.
  - `typing/ikinds/ikinds.ml:943` `ikind substitution reset`
    - Jkind: not relevant; this is a feature toggle for substitution.
    - Preserve: keep disabled; prefer `substitute_decl_ikind_with_lookup`.

**Phase 6: Implement sub_or_intersect using ikinds**

We want to implement sub_or_intersect using ikinds, because ikinds are now
faster than jkinds.

# Future Work

## ikinds

- `Ldd` fast paths (in `typing/ikinds/ldd_cached_down0.ml`):
  - Add physical equality checks (`==`) in `join`, `meet`, `canonicalize`.
  - Handle `bot` and `top` explicitly.
- Memoize `Ldd.map_rigid` (in `typing/ikinds/ldd_cached_down0.ml`):
  - Add a `Hashtbl` to memoize visited nodes during traversal to handle DAGs
    efficiently.
- Balanced join in `ikinds.ml`:
  - Use a balanced merge (divide and conquer) for `Ttuple` and `Tvariant` joins.
- Substitute LDD atoms only when "now concrete" (has manifest):
  - Gate `Ldd.Name.Atom` expansion in `JK.constr_kind` so abstract/no-manifest
    constructors stay rigid (no deep expansion).
  - Use `context.is_abstract` to check if a constructor is abstract before
    recursing in `instantiate`.

## Mode solver representation

- Evaluate a packed/bitfield representation in the mode solver
  (`typing/mode.ml`, `typing/solver.ml`). Treat as a standalone effort.

## History/provenance overhead

- Add a "no history/provenance" mode that skips building jkind/mode histories
  when they will not be reported (separate track; impacts error reporting).
