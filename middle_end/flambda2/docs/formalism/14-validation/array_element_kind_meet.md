# array_element_kind_meet

Source: `testsuite/tests/flambda2/array_element_kind_meet.ml`
References: `array_element_kind_meet.simplify.reference` (flat-float-array) and
`array_element_kind_meet.simplify.no-flat-float-array.reference`
Flags: `-O3`

## Input

```ocaml
type foo = A | B of string
type t = { a : int; b : int; c : foo array }
let sum (t : t) =
  Array.fold_left (fun acc x -> match x with A -> acc | B _ -> acc + 1) 0 t.c
```

The test asserts: when a record field is known to be a value array,
`Is_flat_float_array` should simplify to `false` and be removed.

## Prediction (written before reading the reference)

- The field `c` has type `foo array` where `foo = A | B of string` is a value
  type, so the array's element kind is `Value` (not naked float). Field access
  `t.c` yields an array whose Flambda type records element kind `Value`.
- `Array.fold_left` at `-O3` is inlined (`S.Inline.Substitute`) and its body,
  which contains an `Is_flat_float_array` test to pick between the flat-float and
  generic access paths, is now exposed to the caller's type for `t.c`. Because
  the element kind is known `Value`, meeting the array type against the
  float-array requirement is Bottom, so the prover resolves
  `Is_flat_float_array` to `false` (`0`); the guarding switch then prunes to the
  generic path (`S.Rewrite.Switch.ArmPrune`/`Merge`), and the now-dead
  `Is_flat_float_array` binding is deleted (`S.Rewrite.Let.DeadBinding`).
- Predicted output: no `%is_flat_float_array` primitive anywhere; a generic
  value `Array_load` in the fold body, with the `foo` element discriminated by
  `%is_int` (A constant vs B block). This should hold in both the flat- and
  no-flat-float-array variants (the element kind is `Value` regardless).

## Actual

Neither reference contains `%is_flat_float_array` (grep count 0 in both). The
`-O3` fold is fully inlined and loopified into a `rec` continuation; the array
access is `%array_load.mut (Pfield, i)` and the element is discriminated with
`let prim_3 = %is_int (ifnot_result) in switch prim_3 …`. The parameter type
confirms element kind Value: `t : [ 0 of imm tagged * imm tagged * val array ]`.
The only variant difference is cosmetic array-length naming
(`%array_length.generic` vs `%array_length`).

## Verdict

MATCH. The `Is_flat_float_array` test is folded away and removed in both
variants, driven by the array field's known `Value` element kind; the fold is
inlined and the element accessed generically, exactly as predicted.
