# code_size_of_single_arg_switch

Source: `testsuite/tests/flambda2/code_size_of_single_arg_switch.ml`
Reference: `code_size_of_single_arg_switch.simplify.reference`
Flags: `-flambda2-inline-small-function-size 0 -flambda2-inline-large-function-size 0`

## Input

```ocaml
type t = A | B | C
let switch_converted_to_lookup_table  x = match x with A -> 1 | B -> 2 | C -> 4
let switch_converted_to_lookup_table' x = switch_converted_to_lookup_table x
let switch_converted_to_affine  x = match x with A -> 7 | B -> 8 | C -> 9
let switch_converted_to_affine' x = switch_converted_to_affine x
```

## Prediction (written before reading the reference)

- `A|B|C` are constant constructors, represented as immediates `0,1,2`; each
  `match` is a 3-arm switch feeding one destination.
- `lookup_table`: results `1,2,4` are **not** affinely related, so the ≥3-arm
  single-arg-to-same-destination rewrite emits an immutable value array and an
  `Array_load` indexed by the scrutinee. Chapter 10 (lines 528-532) marks this
  rewrite **descriptive with no rule ID**, so there is no normative rule to cite;
  I predict the phenomenon only.
- `affine`: results `7,8,9 = x + 7` are affinely related, so the affine rewrite
  emits `Int_arith` (multiply by stride 1, add base 7). Also descriptive / no
  rule ID.
- Both primed callers: `small_function_size = 0`, so the callee's size (> 0)
  never qualifies as must-inline and inlining is declined
  (`S.Inline.DeclDecision` → `S.Inline.Decision` keep). They retain the direct
  call.

## Actual

```
let ...switch_block40 = Value_array [|1; 2; 4|] in
... switch_converted_to_lookup_table_0_1 (x) ... size(2) =
  let arg = %array_load (...switch_block40, x) in cont k (arg)
... switch_converted_to_affine_2_1 (x) ... size(7) =
  let scaled_arg = %int_barith.mul (x, 1) in
  let final_arg  = %int_barith.add (scaled_arg, 7) in
  cont k (final_arg)
```

Both primed functions retain `apply direct(...)`.

## Verdict

MATCH (descriptive). Both the lookup-table (immutable `Value_array` +
`Array_load`) and affine (`mul x 1` then `add 7`) rewrites fire exactly as
predicted, and neither caller inlines.

## Diagnosis

Not a mismatch, but worth recording: the two rewrites exercised here
(`rebuild_switch_with_single_arg_to_same_destination`,
`rebuild_affine_switch_to_same_destination`) are deliberately left descriptive
in chapter 10 with no rule ID. This test would be a good candidate to promote to
a cited rule if the formalism ever chooses to formalize the large-switch size
optimizations. The affine form multiplying by an explicit stride of 1 confirms
the general `k*x + b` shape rather than a special-cased consecutive-values path.
