# Mode Axis Inventory (Stage 0)

This document records the current per-axis record surface that the bitfield
migration must preserve. It summarises lattice orientation, record fields,
representative call-sites, and the authoritative bit layout in
`typing/ikinds/axis_lattice.ml`.

## Axis Overview

| Index | Axis (levels in bit layout order) | Lattice orientation | Primary record field(s) | Key consumers |
|-------|-----------------------------------|----------------------|-------------------------|----------------|
| 0 | Areality — `Global < Regional < Local` | Heyting (`Locality`/`Regionality`) | `Mode.Value.Comonadic.areality` (`typing/mode.ml:725`) | `typing/mode.ml`, `typing/typemode.ml`, `typing/typecore.ml`, `typing/ikinds/axis_lattice.ml` |
| 1 | Linearity — `Many < Once` | Heyting | `Mode.Value.Comonadic.linearity` (`typing/mode.ml:725`) | `typing/mode.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 2 | Uniqueness — **bit order** `Aliased < Unique` (Co-Heyting) | Reversed (Co-Heyting) | `Mode.Value.Monadic.uniqueness` (`typing/mode.ml:641`) | `typing/mode.ml`, `typing/typecore.ml`, `typing/typemode.ml`, `typing/uniqueness_analysis.ml`, `typing/ikinds/axis_lattice.ml` |
| 3 | Portability — `Portable < Nonportable` | Heyting | `Mode.Value.Comonadic.portability` (`typing/mode.ml:725`) | `typing/mode.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 4 | Contention — **bit order** `Contended < Shared < Uncontended` | Reversed (Co-Heyting) | `Mode.Value.Monadic.contention` (`typing/mode.ml:641`) | `typing/mode.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 5 | Yielding — `Unyielding < Yielding` | Heyting | `Mode.Value.Comonadic.yielding` (`typing/mode.ml:725`) | `typing/mode.ml`, `typing/includecore.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 6 | Statefulness — `Stateless < Observing < Stateful` | Heyting | `Mode.Value.Comonadic.statefulness` (`typing/mode.ml:725`) | `typing/mode.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 7 | Visibility — **bit order** `Immutable < Read < Read_write` | Reversed (Co-Heyting) | `Mode.Value.Monadic.visibility` (`typing/mode.ml:641`) | `typing/mode.ml`, `typing/includemod_errorprinter.ml`, `typing/typemode.ml`, `typing/ikinds/axis_lattice.ml` |
| 8 | Externality — `External < External64 < Internal` | Heyting-style (`Jkind_axis.Externality`) | Non-modal masks via `Types.Jkind_mod_bounds` | `typing/jkind_axis.ml`, `typing/types.ml`, `typing/ikinds/axis_lattice.ml` |
| 9 | Nullability — `Non_null < Maybe_null` | Heyting-style | Shallow mask helpers in `Types.Jkind_mod_bounds` | `typing/jkind_axis.ml`, `typing/types.ml`, `typing/ikinds/axis_lattice.ml` |
| 10 | Separability — `Non_float < Separable < Maybe_separable` | Heyting-style | Shallow mask helpers in `Types.Jkind_mod_bounds` | `typing/jkind_axis.ml`, `typing/types.ml`, `typing/ikinds/axis_lattice.ml` |

**Orientation note:** Monadic axes (`Uniqueness`, `Contention`, `Visibility`) use
`Total.CoHeyting` lattices (`typing/mode.ml:321`, `typing/mode.ml:450`,
`typing/mode.ml:592`), so their logical minima are encoded as higher bit levels.
`Axis_lattice` mirrors this by mapping `Unique → level 1`,
`Uncontended → level 2`, and `Read_write → level 2`
(see `typing/ikinds/axis_lattice.ml:262`, `typing/ikinds/axis_lattice.ml:288`,
`typing/ikinds/axis_lattice.ml:323`).

### Axis-specific notes

- **Areality (index 0):**
  - Record access throughout `typing/mode.ml:725` and `typing/mode.ml:3198`.
  - Annotated in type modes via `typing/typemode.ml:135` and
    `typing/typemode.ml:258`.
  - Type checker enforces submode constraints at `typing/typecore.ml:10124`.
  - Pretty-print support in `typing/printtyp.ml:1448`.

- **Linearity (index 1):**
  - Lattice operations in `typing/mode.ml:374` and `typing/mode.ml:725`.
  - Consumed by annot/printing paths in `typing/typemode.ml:135` and
    diagnostics `typing/printtyp.ml:1448`.

- **Uniqueness (index 2, monadic):**
  - Defined with `Total.CoHeyting` in `typing/mode.ml:321`.
  - Monadic record usage `typing/mode.ml:641` and residual computations
    `typing/mode.ml:3198`.
  - Type core relies on `typing/typecore.ml:10124`.
  - Uniqueness analysis depends on lattice semantics at
    `typing/uniqueness_analysis.ml:149`.

- **Portability (index 3):**
  - Lattice defined at `typing/mode.ml:409`; referenced by crossings in
    `typing/ikinds/axis_lattice.ml:360` and typemode operations
    (`typing/typemode.ml:135`).

- **Contention (index 4, monadic):**
  - Co-Heyting lattice `typing/mode.ml:450` and monadic record usage
    `typing/mode.ml:641`.
  - Diagnostics in `typing/printtyp.ml:1448` and annotations in
    `typing/typemode.ml:135` and `typing/typemode.ml:347`.

- **Yielding (index 5):**
  - Heyting lattice `typing/mode.ml:500` and comonadic record usage
    `typing/mode.ml:725`.
  - Test for annotation conflicts at `typing/typemode.ml:292`.
  - Inclusion checks in `typing/includecore.ml:176` (yielding guard).

- **Statefulness (index 6):**
  - Heyting lattice `typing/mode.ml:542` and comonadic operations
    `typing/mode.ml:725`.
  - Annotation handling `typing/typemode.ml:135`.

- **Visibility (index 7, monadic):**
  - Co-Heyting lattice `typing/mode.ml:592` and monadic record usage
    `typing/mode.ml:641`.
  - Present in module inclusion diagnostics at
    `typing/includemod_errorprinter.ml:272` and type annotations
    `typing/typemode.ml:113` and `typing/typemode.ml:347`.

- **Externality (index 8):**
  - Axis defined in `typing/jkind_axis.ml:23`; consumed by
    `typing/types.ml:67` and conversions in `typing/ikinds/axis_lattice.ml:328`.

- **Nullability (index 9):**
  - Axis definition `typing/jkind_axis.ml:76`; conversions in
    `typing/ikinds/axis_lattice.ml:340`.

- **Separability (index 10):**
  - Axis definition `typing/jkind_axis.ml:117`; conversions in
    `typing/ikinds/axis_lattice.ml:348`.

## Record Definitions & Helpers

### `Mode.Value.Monadic` (`typing/mode.ml:641`)
- Record fields: `uniqueness`, `contention`, `visibility`.
- Lattice operations (`le`, `join`, `meet`, `subtract`) inline the axis-level
  operations (`typing/mode.ml:668`).
- Pretty-printer emits comma-separated axis values (`typing/mode.ml:720`).

### `Mode.Value.Comonadic` (`typing/mode.ml:725`)
- Parameterised by an `Areality` module; instantiated with `Regionality` for
  compiler-facing types (`typing/mode.ml:882`).
- Fields: `areality`, `linearity`, `portability`, `yielding`, `statefulness`.
- Provides `imply` in addition to lattice ops (`typing/mode.ml:806`).
- Pretty-print method (`typing/mode.ml:830`) used by diagnostics.

### `Mode.Value.t`
- Combines monadic/comonadic fragments into `{ monadic; comonadic }` records,
  with projection helpers (`typing/mode.ml:3032`).
- `diff`/`co_sub` style helpers compute per-axis residuals (`typing/mode.ml:3198`).

### `Mode.Crossing`
- `Crossing.Monadic` wraps `Mode.Modality.Monadic` join-constants (`typing/mode.ml:4094`).
- `Crossing.Comonadic` mirrors meet-constant structure (`typing/mode.ml:4173`).
- Axis metadata functor surfaces per-axis operations used by
  `Axis_lattice` (`typing/mode.ml:4240`).

### `Mode.Modality`
- Encapsulates modality constants for both fragments; used by
  `Axis_lattice` when building crossings from bitfields
  (`typing/ikinds/axis_lattice.ml:360`).

## Key Consumers Per Axis

- **`typing/mode.ml:321`** — owns lattice definitions, record construction,
  adjunction helpers, and per-axis diff/zap utilities.
- **`typing/typemode.ml:103`** — serialises/deserialises per-axis
  annotations and enforces modifier defaults.
- **`typing/typecore.ml:10118`** — tightens `areality` during submode
  checks for array allocations.
- **`typing/printtyp.ml:1443`** — derives diagnostic payloads with
  implied defaults for yielding/contention/portability.
- **`typing/includecore.ml:176`** — reconciles yielding defaults during module inclusion.
- **`typing/includemod_errorprinter.ml:272`** — prints visibility-related
  inclusion errors using monadic projections.
- **`typing/uniqueness_analysis.ml:149`** — performs post-typechecking
  uniqueness analysis based on the monadic lattice structure.
- **`typing/ikinds/axis_lattice.ml:1`** and
  **`typing/ikinds/axis_lattice_array.ml:1`** — maintain the canonical bit
  layout, conversions, and ikind lattice integration.
- **`typing/jkind_axis.ml:23`** and **`typing/types.ml:67`** — define
  non-modal axes and expose `Types.Jkind_mod_bounds` accessors.

## Bit Layout Canonical Source

`typing/ikinds/axis_lattice.ml` owns the authoritative packing order, offsets,
mask generators, and conversion routines for all axes (`typing/ikinds/axis_lattice.ml:1`).
Any future backend must delegate to this module (or its `Axis_bitfield`
replacement) for:

- `axis_sizes`, `offsets`, `axis_mask` tables (`typing/ikinds/axis_lattice.ml:28`,
  `typing/ikinds/axis_lattice.ml:55`, `typing/ikinds/axis_lattice.ml:65`).
- `encode`/`decode` round-trip (`typing/ikinds/axis_lattice.ml:98`,
  `typing/ikinds/axis_lattice.ml:112`).
- Per-axis level converters (`typing/ikinds/axis_lattice.ml:242`,
  `typing/ikinds/axis_lattice.ml:355`).
- Crossings reconstruction (`typing/ikinds/axis_lattice.ml:360`).

## Known Invariants to Preserve

- Monadic axes behave contravariantly; `Mode.Value.Monadic` relies on
  `Total.CoHeyting` laws, so bit-level comparisons must flip order to maintain
  API semantics.
- `Mode.Value` printers and diff helpers assume record field order matches
  diagnostic output; equivalence tests compare record fields structurally.
- `Axis_lattice` iterates axes in a fixed order (`all_axes_correct_order` at
  `typing/ikinds/axis_lattice.ml:34`); all consumers (e.g.
  `Axis_lattice_array`) depend on that enumeration.
- Downstream masks (e.g. `Mode.Modality.mask_of_modality`) expect shallow axes
  (`Nullability`, `Separability`) to be skippable in certain operations.

## Next Steps

- Flesh out per-axis property tests using the enumerated call-sites as the
  regression oracle.
- Decide whether the Areality parameterisation should live in `Axis_bitfield`
  (single layout) or remain abstracted for alternative layouts.
