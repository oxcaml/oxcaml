# cse_immutable_array_load_var_index

Source: `testsuite/tests/flambda2/cse_immutable_array_load_var_index.ml`
Reference: `cse_immutable_array_load_var_index.simplify.reference`
Flags: `[@@@ocaml.flambda_o3]`

## Input

```ocaml
open Stdlib_stable
let f (arr : int Iarray.t) i =
  let x = Iarray.unsafe_get arr i in
  let y = Iarray.unsafe_get arr i in
  x = y
```

## Prediction (written before reading the reference)

Same shape as `cse_immutable_array_load`, but with a **variable** index `i`.

Reading the formalism literally: `S.Rewrite.CSE.Eligible` says loads are excluded
from CSE, and load dedup instead flows through the type-based
`S.Rewrite.Prim.Projection` family. But the projection/type machinery keys on a
*statically known* field/index: an immutable array's element type is tracked per
static index, not for a symbolic variable index `i`. So under the formalism as
written, there is **no** way to dedup the two loads at a variable index, and I
predict the second load is *not* eliminated — `f` should retain both loads and a
real `x = y` comparison, i.e. it does **not** collapse to a constant.

(If instead immutable array loads were CSE-eligible, the two syntactically-equal
`Array_load (arr, i)` applications would be CSE'd on canonicalized arguments and
`f` would collapse to `true`.)

## Actual

```
... f_0_1 (arr : imm array, i : imm tagged) ... size(1) ... = cont k (1)
```

`f` collapses to `cont k (1)` — returns `true`. The second load **is**
eliminated, even though the index is a variable.

## Verdict

MISMATCH. The formalism-literal prediction (no dedup, both loads retained) is
wrong; the loads are deduplicated and `f` returns a constant. This is the sharp
witness that the dedup is genuine **CSE** (`S.Rewrite.CSE.Replace` keying on the
syntactic `Array_load (arr, i)` with canonicalized args), not type-based
projection propagation — because type propagation demonstrably cannot handle a
variable index, yet the dedup still fires.

## Diagnosis (formalism error)

Same root cause as `cse_immutable_array_load`: `S.Rewrite.CSE.Eligible` wrongly
states loads are excluded from CSE. `binary_primitive_eligible_for_cse` returns
`true` for `Array_load (_, _, (Immutable | Immutable_unique))`
(`middle_end/flambda2/terms/flambda_primitive.ml`), and immutable reads are
`(No_effects, No_coeffects)`, so the two `Array_load (arr, i)` applications are
CSE-eligible and the second is replaced by an alias to `x`. Then `x = y` becomes
`x = x`, folding to `1`. This test exists precisely to distinguish CSE from
type-propagation, and it confirms CSE is the mechanism. The rule's exclusion
parenthetical must be narrowed (see `cse_immutable_array_load.md` Diagnosis).
