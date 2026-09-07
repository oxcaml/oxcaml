# cse_immutable_array_load

Source: `testsuite/tests/flambda2/cse_immutable_array_load.ml`
Reference: `cse_immutable_array_load.simplify.reference`
Flags: `[@@@ocaml.flambda_o3]`

## Input

```ocaml
open Stdlib_stable
let f (arr : int Iarray.t) =
  let x = Iarray.unsafe_get arr 0 in
  let y = Iarray.unsafe_get arr 0 in
  x = y
```

## Prediction (written before reading the reference)

Both `unsafe_get` calls lower to an immutable `Array_load (arr, 0)`. The final
result is `x = y`, and I predict it collapses to constant `true` (`1`): once the
second load is shown equal to the first, `x = y` becomes `x = x`, which the
integer-equality prover resolves reflexively to `1` (`S.Rewrite.Prim.ConstFold`
on `Int_comp` over a variable and itself). So `f` becomes `cont k (1)`.

Mechanism (per the formalism as written): `S.Rewrite.CSE.Eligible` states
"projections/loads excluded" from CSE, with load information propagated through
types via the `S.Rewrite.Prim.Projection` family. So I predict the dedup here is
**type-based projection propagation** (the first load binds `x` with a type
recording it as element 0 of `arr`; the second load reifies to an alias of `x`),
**not** the CSE table.

## Actual

```
... f_0_1 (arr : imm array) ... size(1) ... = cont k (1)
```

The whole body collapses to `cont k (1)` — returns `true`. The final result
matches. But the code path that performs the dedup is CSE, not projection
propagation: `binary_primitive_eligible_for_cse` returns `true` for
`Array_load (_, _, (Immutable | Immutable_unique))`
(`middle_end/flambda2/terms/flambda_primitive.ml`), and immutable array reads
have `(No_effects, No_coeffects)`, so they pass the eligibility gate and are
CSE'd via `S.Rewrite.CSE.Replace`.

## Verdict

PARTIAL. The observable outcome (`f` returns constant `1`) is predicted
correctly, but the mechanism attribution is wrong: the dedup is genuine CSE
(`S.Rewrite.CSE.Replace`), because immutable array loads *are* CSE-eligible —
contradicting `S.Rewrite.CSE.Eligible`. See Diagnosis; the companion test
`cse_immutable_array_load_var_index` is the sharp witness (MISMATCH there).

## Diagnosis (formalism error)

`S.Rewrite.CSE.Eligible` (chapter 10) is inaccurate. Its rule body says the
per-primitive predicate excludes "projections/loads", and the surrounding prose
says "projections are deliberately excluded — the information they carry is
propagated through types (the `S.Rewrite.Prim.Projection` family)". But the
per-primitive predicate in the code (`*_primitive_eligible_for_cse` in
`middle_end/flambda2/terms/flambda_primitive.ml`) is finer:

- Excluded (predicate `false`): `Block_load`, `Project_function_slot`,
  `Project_value_slot`, **mutable** `Array_load`, `String_or_bigstring_load`,
  `Bigarray_load`, `Duplicate_*`, `Unbox_number`, `Untag_immediate`.
- **Eligible** (predicate `true`): **immutable** `Array_load`
  (`Immutable | Immutable_unique`), `Array_length`, `String_length`, `Is_int`,
  `Get_tag`, `Get_header`, `Bigarray_get_alignment`, and the arithmetic/compare
  families.

So the blanket "loads excluded" is false: immutable array loads are the explicit
counterexample and are exactly the subject of this test. The fix is to narrow
the rule's parenthetical to "block loads, closure/value-slot projections, and
mutable/string/bigarray loads excluded; immutable array loads and immutable
header reads (length/tag/is_int) eligible."
