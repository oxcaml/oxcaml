# new-04: CSE of a repeated pure primitive

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/cse.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/cse.ml) (reference: `formalism/cse.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/cse.ml`.

Targets `S.Rewrite.CSE.Replace` (+ `.Extend`, `.Eligible`) (ch. 10): two
identical `String.length` reads collapse to one.

## Source

`t04.ml`:

```ocaml
let f s = String.length s + String.length s
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 04.raw.fl -dfexpr-to 04.final.fl t04.ml`.

## Raw IR (body of `f`)

```
let prim/34 = %string_length (s/32) in
let Pstringlength/35 = %tag_imm (prim/34) in
let prim/36 = %string_length (s/32) in
let Pstringlength/37 = %tag_imm (prim/36) in
let int_add/38 = %int_barith.add (Pstringlength/37, Pstringlength/35) in
cont k/11 (int_add/38)
```

Two syntactically identical `%string_length (s)` applications, each followed by
`%tag_imm`.

## Prediction (before running)

`%string_length` reads an immutable string: it is pure (no coeffects) and has a
variable argument, so it is CSE-eligible (`S.Rewrite.CSE.Eligible`). The first
application records `string_length(s) ↦ prim/34` in the CSE table
(`S.Rewrite.CSE.Extend`); the second, with canonicalized argument `s`, hits that
entry and is replaced by an alias to `prim/34` (`S.Rewrite.CSE.Replace`).
`%tag_imm` of the same value is likewise CSE'd.

Expected: exactly one `%string_length` and one `%tag_imm`; the add uses the same
variable for both operands.

## Actual IR (final)

```
let prim/64 = %string_length (s/63) in
let Pstringlength/65 = %tag_imm (prim/64) in
let int_add/66 = %int_barith.add (Pstringlength/65, Pstringlength/65) in
cont k/23 (int_add/66)
```

## Verdict

MATCH. One `%string_length`, one `%tag_imm`, and the addition reads
`Pstringlength/65` twice — the second load and tag were replaced by aliases.

## Diagnosis

No discrepancy. This confirms `S.Rewrite.CSE.Eligible`'s classification of
`String_length` as pure/coeffect-free (strings are immutable) and the
Extend-then-Replace pairing. Note the addition's operand order is swapped
relative to source (`add (Pstringlength/37, Pstringlength/35)` in the raw): the
argument that survives is the first-computed one, consistent with alias-set
intersection in canonicalization.
