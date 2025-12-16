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

### A. Solver hot path (ikinds)

**Phase 1: Quick wins, low risk**

-[done] Enforce subtract-normal form for `Types.constructor_ikind` via a smart
  constructor:
  - Add `Types.constructor_ikind_create ~base ~coeffs` that stores
    `coeffs[i] := Ldd.sub_subsets coeffs[i] base`.
  - Use it everywhere we construct a `constructor_ikind`:
    `Ikinds.type_declaration_ikind`, `Ikinds.type_declaration_ikind_of_jkind`,
    and any rewriting/substitution that might disturb the invariant.
- Remove obvious allocation hotspots:
  - Avoid `List.of_seq` in `ckind_of_jkind`.
  - Avoid repeated `List.nth_opt` in hot loops (use arrays where helpful).
  - Pre-size hash tables where we can estimate sizes.

**Phase 2: Context closure optimization (DONE)**

- [x] Remove the `ctx -> ...` closure indirection (`JK.ckind`) in `JK.env` and `kind_of`.
  - Change `env.kind_of` to `ctx -> ty -> kind`.
  - Update `kind_of` to take `ctx` as an explicit argument, avoiding closure allocation for every type node traversal.
  - This is a high-impact optimization for the hot path.

**Phase 3: Main algorithm improvements**

- Substitute LDD atoms only when "now concrete" (has manifest):
  - Gate `Ldd.Name.Atom` expansion in `JK.constr_kind` so abstract/no-manifest
    constructors stay rigid (no deep expansion).
  - Use `context.is_abstract` to check if a constructor is abstract before recursing in `instantiate`.
- LHS normalization in sub checks:
  - Before `Ldd.leq_with_reason`, compute RHS atom support and map LHS-only
    abstract atoms to `⊤`.
  - Preserve non-variable bounds by only erasing the abstract variable itself,
    not its bound contributions (relies on current abstract encoding).
- Mode-crossing special case:
  - Since RHS is effectively plain mod-bounds, treat all abstract atoms as `⊤`
    before rounding up; still meet in abstract bounds for non-variables.
- LHS normalization in sub checks:
  - Before `Ldd.leq_with_reason`, compute RHS atom support and map LHS-only
    abstract atoms to `⊤`.
  - Preserve non-variable bounds by only erasing the abstract variable itself,
    not its bound contributions (relies on current abstract encoding).
- Mode-crossing special case:
  - Since RHS is effectively plain mod-bounds, treat all abstract atoms as `⊤`
    before rounding up; still meet in abstract bounds for non-variables.

**Phase 4: Reduce memoization pressure**

- Limit `ty_to_kind` memoization to types that are potentially circular
  (polymorphic variants); keep `constr_to_coeffs` memoization.
- Re-check whether the remaining hotspots justify larger refactors.

**Phase 5: Preserve ikinds more broadly**

- Reduce `Types.ikind_reset` cases where substitution/renaming can preserve the
  cached constructor ikind (use `substitute_decl_ikind_with_lookup`).
- Consider keeping ikinds for unboxed-version aliases where safe.

### B. Mode solver representation

- Evaluate a packed/bitfield representation in the mode solver
  (`typing/mode.ml`, `typing/solver.ml`). Treat as a standalone effort.

### C. History/provenance overhead

- Add a "no history/provenance" mode that skips building jkind/mode histories
  when they will not be reported (separate track; impacts error reporting).

## Correctness tasks

- Investigate lookup failures and add internal assertions where appropriate.
- Add targeted tests for new normalization/substitution behavior.

## Measurement tasks

- Measure on a large tree and on the compiler itself.
- Record results in `typing/ikinds/benchresults.txt` (append-only).

## Ordering notes

- The big refactor makes later refines easier, but makes small incremental work
  harder (wider surface area, more churn).

## Future Work

- `Ldd` fast paths (in `typing/ikinds/ldd_cached_down0.ml`):
  - Add physical equality checks (`==`) in `join`, `meet`, `canonicalize`.
  - Handle `bot` and `top` explicitly.
- Memoize `Ldd.map_rigid` (in `typing/ikinds/ldd_cached_down0.ml`):
  - Add a `Hashtbl` to memoize visited nodes during traversal to handle DAGs efficiently.
- Balanced join in `ikinds.ml`:
  - Use a balanced merge (divide and conquer) for `Ttuple` and `Tvariant` joins.
