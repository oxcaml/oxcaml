# float32-double-round: int→float32 constant-folding double-rounds (SOUNDNESS BUG)

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/float32_double_round.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/float32_double_round.ml) (reference: `formalism/float32_double_round.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/float32_double_round.ml`.

Unlike every other case study here, this one is a **MISMATCH witnessing a
compiler soundness bug**, not a MATCH. It exercises coupling point (i) of
[`../13-soundness.md` §2](../13-soundness.md) (`INV.Rewrite.Local`): constant
folding must equal the primitive's denotation, which must in turn equal what the
generated code computes. It does not, for `Num_conv` into `Naked_float32`.

Targets `P.Unary.NumConv` (ch. 05, STATUS normative), `S.Rewrite.Prim.ConstFold`
(ch. 10), and the denotational-agreement obligation of `INV.Rewrite.Local`
(ch. 13).

## The claim under test

An integer→`float32` conversion is fully defined for every input (no undefined
behaviour). So folding `float32_of_int n` at compile time must produce the exact
same `float32` the running program would produce for the same `n`. The backend
lowers this conversion to a single hardware instruction, `cvtsi2ss`
(`backend/cmm_helpers.ml#Scalar_type.static_cast`, `Integral→Float` →
`Cstatic_cast (Float_of_int Float32)`; `backend/amd64/emit.ml`), which rounds the
64-bit integer to single precision **once**.

## Source

```ocaml
external float32_of_int : int -> float32 = "%float32ofint"
let f () = float32_of_int 9007199791611905
```

`9007199791611905 = 2^53 + 2^29 + 1`. It fits in a 63-bit OCaml `int`, and it is
*not* exactly representable in `float64`, which is what makes it a witness.

- Correct (single) rounding, `cvtsi2ss`: `2^53 + 2^30`, float32 bits `0x5a000001`
  (printed `0x1.000002p+53s`).
- Double rounding int→`float64`→`float32`: `2^53`, float32 bits `0x5a000000`
  (printed `0x1p+53s`). Rounding to `float64` first lands exactly on `2^53`
  (dropping the `+2^29+1`), after which the `float64`→`float32` step has nothing
  left to round up.

## Prediction (before running)

`float32_of_int` is the primitive `%float32ofint`, lowered to
`Num_conv {src = Tagged_immediate; dst = Naked_float32}`
(`from_lambda/lambda_to_flambda_primitives.ml#static_cast0`). The argument is a
proven constant, so `S.Rewrite.Prim.ConstFold` fires (the fold is *not* gated on
`-flambda2-float-const-prop`; `simplify/simplify_unary_primitive.ml#Make_simplify_int_conv`
folds on any `Known_result`). If the folded value equals the denotation `⟦Num_conv⟧`
and the denotation equals the backend, the body should bind the single-rounded
constant `0x1.000002p+53s` (= `0x5a000001`).

## Actual IR (final, `-dfexpr` / dump-simplify)

```
let $camlFloat32_double_round__float32_of_int22 = 0x1p+53s in
...
  cont k ($camlFloat32_double_round__float32_of_int22)
```

The fold produces `0x1p+53s` = `2^53` = bits `0x5a000000` — the **double-rounded**
value, not the backend's `0x5a000001`.

## Observable miscompilation (runtime A/B)

Compiling and running (installed flambda2 `ocamlopt.opt`, `Stdlib_stable.Float32`):

```ocaml
let n = 9007199791611905 in
let folded  = F.to_bits (F.of_int n) in                       (* Simplify folds *)
let runtime = F.to_bits (F.of_int (Sys.opaque_identity n)) in (* cvtsi2ss at run time *)
```

```
folded   = 0x5a000000  (Simplify const-fold of of_int)
runtime  = 0x5a000001  (cvtsi2ss at run time)
differ   = true
```

The *same* conversion `Float32.of_int 9007199791611905` yields two different
`float32` values depending only on whether the argument is visible to Simplify as
a constant. That is a soundness violation, not covered by the "modulo undefined
behaviour" clause of `INV.Simplify.Preserves` (the conversion has no undefined
behaviour).

## Verdict

MISMATCH — a genuine compiler soundness bug. Per the strict protocol
([`README.md`](README.md), [`../13-soundness.md` §5](../13-soundness.md)) a
MISMATCH is resolved by fixing the thing that is wrong. Here the *code* is wrong,
not the prediction: the fold disagrees with the primitive's real (backend)
denotation, so `P.Unary.NumConv` as implemented violates the
denotational-agreement obligation of `INV.Rewrite.Local`.

## Diagnosis

Root cause: `middle_end/flambda2/simplify/number_adjuncts.ml#to_naked_float32`.
Each of the four numeric variants (`For_int64s`, `For_nativeints`,
`For_tagged_immediates`, `For_naked_immediates`) computes
`Float32_by_bit_pattern.create (X.to_float t)` = `Int32.bits_of_float
(Int64.to_float t)`, i.e. int → `float64` (`Int64.to_float`, one rounding) →
`float32` (`Int32.bits_of_float`, a second rounding). Two roundings; the backend
does one.

Scope: only the `Naked_float32` destination with a source wider than `float64`'s
exact range (`int64`/`nativeint`, or a tagged/naked `int` `> 2^53`).
`Naked_float` (float64) destinations single-round (`Int64.to_float` = `cvtsi2sd`),
and `int32`→`float32` is exact — which is why the bug is narrow and went
unnoticed.

Fix (compiler, not formalism): make `to_naked_float32` round int→float32 **once**
(e.g. via a genuine single-rounding conversion such as `caml_float32_of_int64`
rather than `Float32.create (X.to_float t)`). The fold should stay; it must just
compute `0x5a000001`. Recorded as a known discrepancy in
[`../13-soundness.md` §4](../13-soundness.md); it is the live form of Open
Question #3 in [`../05-primitives-scalar.md`](../05-primitives-scalar.md).

The checked-in `.simplify.reference` captures the **buggy** folded constant
(`0x1p+53s`); when the fix lands it flips to `0x1.000002p+53s`, so the test doubles
as a regression signal.
