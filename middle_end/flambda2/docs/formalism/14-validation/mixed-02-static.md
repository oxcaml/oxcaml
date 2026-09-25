# mixed-02: statically-allocated mixed record

**Testsuite:** [`testsuite/tests/flambda2/examples/formalism/mixed_static.ml`](../../../../../testsuite/tests/flambda2/examples/formalism/mixed_static.ml) (reference: `formalism/mixed_static.simplify.reference`) — run with `make -s test-one-no-rebuild TEST=flambda2/examples/formalism/mixed_static.ml`.

Targets `P.Static.MixedBlock` (ch. 06): a toplevel immutable mixed record
constant is lifted to a static `Block` whose fields include raw naked scalars,
which only a `Mixed_record` scannable shape can hold.

## Source

`m2.ml`:

```ocaml
type t = { x1 : string; x2 : float#; x3 : int32# }

let c = { x1 = "a"; x2 = #1.0; x3 = #2l }
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to m2.raw.fl -dfexpr-to m2.final.fl m2.ml`.

## Raw IR

```
let $camlM2__immstring4 = "a" in
let $camlM2__const_mixed_block6 = Block 0 ($camlM2__immstring4, 0x1p+0, 2l) in
let $camlM2__Pmakeblock10 = Block 0 ($camlM2__const_mixed_block6) in
...
```

The record is already a `Static_const.Block` at CPS time (a compile-time
constant), not a `Make_block` primitive.

## Prediction (before running)

`c` is a compile-time-constant immutable record of mixed shape σ = ⟨1,
[Naked_float, Naked_int32]⟩, so it lifts to `Static_const.Block(tag 0, Immutable,
Mixed_record σ, ["a"; #1.0; #2l])` (`P.Static.MixedBlock`). Each static field's
kind must be `field_kinds(σ)(i)` (`block_field_kind =
Scannable_block_shape.element_kind`): a value symbol for `x1`, a naked float for
`x2`, a naked int32 for `x3`. `to_cmm_static` would emit a mixed-block header of
`size_in_words(σ) = 3` physical words with `scannable_prefix_len = 1`. Because
the fields include raw naked scalars, the block is necessarily a `Mixed_record`
static block (a `Value_only` block cannot hold naked scalars) and necessarily
immutable (a mutable static mixed block is rejected at `to_cmm_static`).

Expected final IR: a static `Block 0 (<string sym>, 1.0, 2l)`.

## Actual IR (final)

```
let $camlM2__immstring4 = "a" in
let $camlM2__const_mixed_block6 = Block 0 ($camlM2__immstring4, 0x1p+0, 2l) in
let $camlM2 = Block 0 ($camlM2__const_mixed_block6) in
cont done ($camlM2)
```

## Verdict

MATCH. `c` is a static `Block 0` whose fields are the string symbol
`$camlM2__immstring4`, the naked float `0x1p+0` (= 1.0), and the naked int32
`2l`. A `Value_only` static block cannot store `0x1p+0`/`2l` as fields, so this
is a `Mixed_record σ = ⟨1, [Naked_float, Naked_int32]⟩` static block, immutable,
exactly as `P.Static.MixedBlock` describes. No `Make_block` primitive is emitted
(the constant is installed once as a static const, per `OS.Let.Static`).

## Diagnosis

None. The fexpr printer renders both `Value_only` and `Mixed_record` static
blocks with the same `Block <tag> (fields)` surface syntax, so the shape is not
printed explicitly; the presence of unboxed field values is the decisive
evidence that the shape is `Mixed_record`. `to_cmm_static.ml`'s
`black_mixed_block_header ... ~scannable_prefix_len` path and its mutable-mixed
rejection are not exercised at fexpr level but are the anchors cited in the rule.
