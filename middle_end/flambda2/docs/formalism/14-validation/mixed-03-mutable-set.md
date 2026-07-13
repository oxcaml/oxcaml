# mixed-03: mutable mixed field, write then read

Targets `P.Binary.BlockSet.Mixed` and `P.Unary.BlockLoad.Mixed` +
`P.Effects.ReadingFromBlock` (ch. 06): a write to a mutable flat-suffix field
followed by a read of the same field does not fold the read to the just-written
value, because a mutable-block load has coeffects.

## Source

`m3.ml`:

```ocaml
type t = { x1 : string; mutable x2 : float#; x3 : int32# }

let write_then_read (t : t) (v : float#) =
  t.x2 <- v;
  t.x2
```

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to m3.raw.fl -dfexpr-to m3.final.fl m3.ml`.

## Prediction (before running)

Shape σ = ⟨1, [Naked_float, Naked_int32]⟩; `x2` is logical field 1 (`i ≥ p`, so
`Flat_suffix Naked_float`).

- `t.x2 <- v` is `Block_set Mixed { field_kind = Flat_suffix Naked_float; field
  = 1 }` (`P.Binary.BlockSet.Mixed`), classified `writing_to_a_block`
  (`Arbitrary_effects, No_coeffects`).
- `t.x2` is `Block_load Mixed { field_kind = Flat_suffix Naked_float; mut =
  Mutable; field = 1 }`. Because the field is mutable, `mut = Mutable` drives
  `Coeffects = Has_coeffects` (`P.Effects.ReadingFromBlock`), so the load is
  **not** CSE-able and **not** foldable to `v`: it cannot be moved across / over
  the preceding write.

Expected: both `Block_set Mixed` and `Block_load Mixed .mut` survive; the result
is the loaded value, not `v`.

## Actual IR (final)

```
write_then_read_0_1 (t/55 : val, v/56 : float) ... : float =
  let Psetmixedfield/57 =
    %block_set.mixed.`float`.`1`.[`float`, `int32`].[`1`] (t/55, v/56) in
  let Pmixedfield/58 =
    %block_load.mixed.`float`.`1`.[`float`, `int32`].mut.[`1`] (t/55) in
  cont k/23 (Pmixedfield/58)
```

## Verdict

MATCH. The set is `%block_set.mixed.`float`.`1`.[float, int32].[`1`]` — a
`Flat_suffix Naked_float` write to logical field 1 of the shape-⟨1,[float,
int32]⟩ block. The load is `%block_load.mixed.`float`.`1`.[float,
int32].mut.[`1`]` — note the `.mut` annotation marking it a mutable load — and
the continuation returns `Pmixedfield/58` (the loaded value), **not** `v/56`.
The read was not folded to the written value, exactly as the coeffect discipline
of `P.Effects.ReadingFromBlock` (Mutable ⟹ Has_coeffects) requires.

## Diagnosis

None. The `.mut` marker on the load is the fexpr rendering of `mut = Mutable`,
the source of the load's `Has_coeffects`; suffix fields (unboxed scalars) need
no GC write barrier, consistent with the `P.Binary.BlockSet.Mixed` note.
