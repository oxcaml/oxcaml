# mixed-01: build and read a mixed record

Targets `P.Variadic.MakeBlock.Mixed`, `P.Unary.BlockLoad.Mixed`,
`P.MixedShape.FieldKinds` (ch. 06) and `WF.Prim.MakeBlockMixed` (ch. 03): a
record with one value field and two unboxed flat-suffix fields builds a mixed
block and reads each field at its correct field kind, and a load from a
locally-built immutable mixed block folds through the type.

## Source

`m1.ml`:

```ocaml
type t = { x1 : string; x2 : float#; x3 : int32# }

let build (s : string) (f : float#) (i : int32#) = { x1 = s; x2 = f; x3 = i }

let get_x1 (t : t) = t.x1
let get_x2 (t : t) = t.x2
let get_x3 (t : t) = t.x3

let roundtrip (s : string) (f : float#) (i : int32#) =
  let t = { x1 = s; x2 = f; x3 = i } in
  get_x2 t
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to m1.raw.fl -dfexpr-to m1.final.fl m1.ml`.

## Raw IR

```
build_0 (s/94 : val, f/95 : float, i/96 : int32) ... =
  let Pmakeblock/98 = %block.mixed.`0`.`1`.[`float`, `int32`] (s/94, f/95, i/96) in
  cont k/27 (Pmakeblock/98)

get_x1_1 (t/101 : [ 0 of val * float * int32 ]) ... : val =
  let Pmixedfield/103 = %block_load.mixed.`1`.[`float`, `int32`].[`0`] (t/101) in ...

get_x2_2 (t/106 : ...) ... : float =
  let Pmixedfield/108 = %block_load.mixed.`float`.`1`.[`float`, `int32`].[`1`] (t/106) in ...

get_x3_3 (t/111 : ...) ... : int32 =
  let Pmixedfield/113 = %block_load.mixed.`int32`.`1`.[`float`, `int32`].[`2`] (t/111) in ...

roundtrip_4 (s/116, f/117, i/118) ... : float =
  ... let t/121 = %block.mixed.`0`.`1`.[`float`, `int32`] (s/116, f/117, i/118) in
  apply direct(get_x2_2) ... (t/121) ...
```

## Prediction (before running)

The record type `t` has mixed shape σ = ⟨value_prefix_size = 1, flat_suffix =
[Naked_float, Naked_int32]⟩, so `field_kinds(σ) = [Value, Naked_number
Naked_float, Naked_number Naked_int32]` (`P.MixedShape.FieldKinds`).

- `build`: `Make_block(Mixed(tag 0, σ), Immutable, Heap)` on `(s, f, i)`, kinds
  checked against `field_kinds(σ)` (`WF.Prim.MakeBlockMixed`,
  `P.Variadic.MakeBlock.Mixed`).
- `get_x1`: `Block_load Mixed`, field 0, `i < p` ⟹ `Value_prefix Any_value`,
  result kind `Value` (`P.Unary.BlockLoad.Mixed`, `element_kind_for_load`).
- `get_x2`: field 1, `i ≥ p` ⟹ `Flat_suffix e₀ = Naked_float`, result naked
  float.
- `get_x3`: field 2, `Flat_suffix e₁ = Naked_int32`, result naked int32.
- `roundtrip`: `get_x2` inlines; the `Block_load Mixed` field 1 reads the
  locally-built *immutable* block, so it folds to the constructor argument `f`
  (projection through the type), and the `Make_block` is deleted as unused
  (`Only_generative_effects Immutable`). Expected result: `cont k (f)`.

## Actual IR (final)

```
build_0_1 (...) : [ 0 of val * float * int32 ] =
  %block.mixed.`0`.`1`.[`float`, `int32`] (s, f, i)          (* Make_block kept *)

get_x1_1_1 ... : val   = %block_load.mixed.`1`.[`float`, `int32`].[`0`] (t)
get_x2_2_1 ... : float = %block_load.mixed.`float`.`1`.[`float`, `int32`].[`1`] (t)
get_x3_3_1 ... : int32 = %block_load.mixed.`int32`.`1`.[`float`, `int32`].[`2`] (t)

roundtrip_4_1 (s/225, f/226, i/227) ... : float =
  cont k/81 (f/226)                                          (* load folded, block gone *)
```

## Verdict

MATCH. The `%block.mixed.`0`.`1`.[float, int32]` printing gives exactly the
predicted shape (tag 0, value_prefix_size 1, flat suffix `[float, int32]`). Each
`get_x*` produces a `Block_load Mixed` at the predicted field index and element
kind: field 0 is a value-prefix load (no element-kind annotation, result `val`),
fields 1 and 2 carry the `float`/`int32` element-kind annotation and produce
naked results. `roundtrip` folds `get_x2` down to `cont k/81 (f/226)` — the load
against the local immutable mixed block was projected to the constructor
argument and the `Make_block` deleted (size 13 → 1).

## Diagnosis

None. The `element_kind_for_load` = `field_kinds(σ)(i)` mapping and the
`i < p` / `i ≥ p` value-prefix/flat-suffix split show up verbatim in the field
annotations, and immutable-mixed-load folding is the ordinary projection
behaviour shared with `Values` blocks.
