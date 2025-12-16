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

**Phase 2: Main algorithm improvements**

- Substitute LDD atoms only when "now concrete" (has manifest):
  - Gate `Ldd.Name.Atom` expansion in `JK.constr_kind` so abstract/no-manifest
    constructors stay rigid (no deep expansion).
- LHS normalization in sub checks:
  - Before `Ldd.leq_with_reason`, compute RHS atom support and map LHS-only
    abstract atoms to `⊤`.
  - Preserve non-variable bounds by only erasing the abstract variable itself,
    not its bound contributions (relies on current abstract encoding).
- Mode-crossing special case:
  - Since RHS is effectively plain mod-bounds, treat all abstract atoms as `⊤`
    before rounding up; still meet in abstract bounds for non-variables.

**Phase 3: Reduce memoization pressure**

- Limit `ty_to_kind` memoization to types that are potentially circular
  (polymorphic variants); keep `constr_to_coeffs` memoization.
- Re-check whether the remaining hotspots justify larger refactors.

**Phase 4: Preserve ikinds more broadly**

- Reduce `Types.ikind_reset` cases where substitution/renaming can preserve the
  cached constructor ikind (use `substitute_decl_ikind_with_lookup`).
- Consider keeping ikinds for unboxed-version aliases where safe.

**Phase 5: Big refactor (do last)**

- Remove the `ctx -> ...` closure indirection (`JK.ckind`) by switching to
  direct `ctx`-passing APIs. This is invasive and should follow Phase 2/3.

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

- Do Phase 2 before the big refactor: it lands the main wins with small deltas.
- The big refactor makes later refines easier, but makes small incremental work
  harder (wider surface area, more churn).
