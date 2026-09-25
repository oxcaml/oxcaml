# gadt_simplified_switch (raw dump)

Source: `testsuite/tests/flambda2/gadt_simplified_switch.ml`
Reference: `gadt_simplified_switch.raw.reference` (raw fexpr, i.e. after CPS /
closure conversion, **before** Simplify)

## Input

```ocaml
type _ t = A : string t | B : string t | C : int -> int t | D : int -> int t
let extract_int (x : int t)    = match x with C s -> s | D s -> s + 1
let extract_string (x : string t) = match x with A -> "A" | B -> "B"
```

## Prediction (written before reading the reference)

This is a raw-dump test, so the property to predict is a property of the CPS /
closure-conversion output, not of a Simplify rewrite (`close_switch` in
`from_lambda/closure_conversion.ml`, cf. `WF.Syntax.SwitchMinArms`). The test
asserts that no spurious `%is_int` switch is introduced.

- `extract_int` matches only `C | D`, both **non-constant** (boxed, int-carrying)
  constructors. With no constant constructor in play there is nothing to
  distinguish with `Is_int`; the discrimination is purely by block tag. Predicted
  raw shape: `Get_tag x`, then a `switch` on the tag `{0 → …, 1 → …}`, each arm
  doing a `Block_load` of field 0 (and `D`'s arm adding 1). No `%is_int`.
- `extract_string` matches only `A | B`, both **constant** constructors,
  represented as immediates `0,1`. Predicted raw shape: `switch` on the untagged
  immediate `{0 → "A", 1 → "B"}`. No `%is_int`.

## Actual

`extract_int`:
```
let prim = %get_tag (x) in
let scrutinee_tag = %tag_imm (prim) in
let untagged = %untag_imm (scrutinee_tag) in
switch untagged | 0 -> k3 | 1 -> k4
  where k4 = let Pfield = %block_load.[`0`] (x) in
             let int_add = %int_barith.add (Pfield, 1) in cont k1 (int_add)
  where k3 = let Pfield = %block_load.[`0`] (x) in cont k1 (Pfield)
```

`extract_string`:
```
let untagged = %untag_imm (x) in
switch untagged | 0 -> k1 ("A") | 1 -> k1 ("B")
```

No `%is_int` occurs anywhere in the dump.

## Verdict

MATCH. Neither function contains an `Is_int` primitive; `extract_int`
discriminates on `Get_tag`, `extract_string` on the raw immediate — exactly as
predicted.

## Diagnosis

Not a mismatch. One side detail not in the prediction: in the raw (pre-Simplify)
dump `extract_int` performs a redundant `Get_tag → Tag_immediate →
Untag_immediate` round trip before the switch. This is expected of the raw IR
(the tag/untag pair is cleaned up later by `S.Rewrite.Prim.UntagTag`), and does
not affect the targeted property. Both functions are also annotated with the full
four-constructor variant type `[ 0 | 1 | 0 of imm tagged | 1 of imm tagged ]`;
the elimination of the irrelevant constructors happens in the switch structure,
not by trimming the type, which is consistent with the test's intent.
