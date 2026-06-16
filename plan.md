# Middle modalities plan

## Goal

Support modalities that are middle elements on diamond axes, such as
`shared`, `corrupted`, `shareable`, `corruptible`, `read`, `write`,
`reading`, and `writing`, when computing modal kinds.

Today these modalities work as field modalities, but modal-kind computation
mostly reduces them to an `Axis_set`. That loses the difference between a
middle modality and a full-axis modality.

## Current problem

`With_bounds_type_info.t` stores only:

```ocaml
{ relevant_axes : Jkind_axis.Axis_set.t }
```

This can say that a type parameter is relevant on the portability axis, but it
cannot say that it is relevant only up to `shareable`.

The main lossy paths are:

- `Btype.Jkind0.Mod_bounds.relevant_axes_of_modality`
- `Axis_lattice.mask_of_modality`
- `Jkind.apply_modality_l`
- `Jkind.apply_modality_r`
- `Jkind.Base_and_axes.normalize`, where a with-bound either contributes on a
  whole axis or does not contribute on that axis
- `Jkind.sub_jkind_l`, where `MB_WITH` currently subtracts matching
  with-bounds with `Axis_set.diff`
- `Btype.Jkind0.Jkind_desc.add_with_bounds`, where arrow types have an eager
  fast path that bypasses `With_bounds.add_modality`
- `Ikind`, where field and constructor modalities use
  `Axis_lattice.mask_of_modality`

`Axis_lattice.t` itself can already represent middle elements, including the
two middle elements of diamond axes. The issue is that we often convert a
modality to an `Axis_set` before using it.

## Proposed representation

Change with-bound type info from an axis set to a mask that can represent
middle elements.

Possible shape:

```ocaml
module With_bounds_type_info : sig
  module Mask : sig
    type t

    val join : t -> t -> t
    val meet : t -> t -> t
    val residual : t -> t -> t
    val le : t -> t -> bool
  end

  type t = { relevant_bounds : Mask.t } [@@unboxed]

  val join : t -> t -> t
end
```

The implementation can use `Axis_lattice.t` internally, but avoid exposing the
bit-packed representation as the conceptual API. This keeps callers from
mixing payload bounds, modality masks, and loop-control masks accidentally.

Use the mask lattice join for `With_bounds_type_info.join`.

Naming should avoid `relevant_axes` once the value is not just a set. Good
names:

- `relevant_bounds`
- `modality_mask`
- `payload_mask`

I would start with `relevant_bounds`, because it fits the existing
`With_bounds_type_info` role.

## Modality masks

Replace `Axis_lattice.mask_of_modality` with a mask helper, backed by
`Axis_lattice`, that directly returns the per-axis mask represented by the
modality.

Expected behavior:

- Identity modality on an axis should contribute the top mask for that axis,
  because the payload remains fully relevant there.
- Constant modality on an axis should contribute the bottom mask for that axis,
  because the payload is ignored there.
- Middle modality should contribute its actual middle level.

Examples:

- `@@ portable` masks portability to `portable`.
- `@@ shareable` masks portability to `shareable`.
- `@@ shared` masks contention to `shared`.
- `@@ read` masks visibility to `read`.
- `@@ reading` masks statefulness to `reading`.

For nonmodal externality, do not leave the value implicit. A modality-only mask
should keep externality at top, matching the current behavior where modality
relevance always includes externality.

Code paths that also parse an explicit externality modifier, such as
`Pjk_with`, should use an externality-aware mask constructor with this
payload-relevance table:

| Explicit externality | Payload externality mask |
| --- | --- |
| none | top / `internal` |
| `internal` | top / `internal` |
| `external64` | bottom / no payload contribution |
| `external_` | bottom / no payload contribution |

This preserves the current behavior where a non-top explicit externality
modifier removes the payload type from the externality computation. If we want
`external64` to become a precise middle mask later, that should be a separate
semantic change with tests.

## Normalization algorithm

In `Jkind.Base_and_axes.normalize`, replace the current whole-axis logic:

```ocaml
if Axis_set.mem relevant_axes axis then
  join current_bound payload_bound
else
  current_bound
```

with mask-based logic:

```ocaml
let payload_contribution =
  Mod_bounds.cap_by_mask_l payload_bound relevant_bounds
in
Mod_bounds.join current_bound payload_contribution
```

At the `Mod_bounds.t` level, add named helpers rather than spreading ad hoc
conversions around call sites:

- `Mod_bounds.cap_by_mask_l`
- `Mod_bounds.relax_by_mask_r`
- `Mod_bounds.residual_mask`
- `Mod_bounds.saturated_mask`

`cap_by_mask_l` is the direct-bound operation corresponding to left-side
modality application. Its lattice law is:

```ocaml
to_mask (cap_by_mask_l bounds mask) =
  Mask.meet (to_mask bounds) mask
```

`relax_by_mask_r` is the right adjoint of `cap_by_mask_l`. Its required law is:

```ocaml
Mod_bounds.le (cap_by_mask_l actual mask) expected
iff
Mod_bounds.le actual (relax_by_mask_r expected mask)
```

Do not implement `relax_by_mask_r` by guessing from bit complement tricks. Test
the adjunction directly for chain and diamond axes, including sibling middle
elements.

## Subkinding residuals

`Jkind.sub_jkind_l` currently implements `MB_WITH` by subtracting right-side
relevant axes from left-side relevant axes with `Axis_set.diff`. This must
become a lattice residual.

Use `Mask.residual` or a `Mod_bounds` helper with the same meaning:

```ocaml
let remaining =
  Mask.residual left_relevant_bounds
    (Mask.join right_relevant_bounds saturated_by_right_direct_bounds)
```

This matters for sibling middle elements. For example, subtracting `read` from
`read_write` should leave `write`, not drop the whole visibility axis.
Likewise, `shareable` must not cancel `corruptible`, and `shared` must not
cancel `corrupted`.

The `saturated_by_right_direct_bounds` mask generalizes the current
`axes_max_on_right` optimization. It represents the parts of the left mask that
the right-side direct bounds already accept, so they do not stay spuriously
relevant after `MB_WITH`.

## Loop-control masks

Do not use `Axis_set` for normalize loop-control or seen-state once the
distinction between sibling middle elements matters.

`Loop_control` currently records `relevant_axes_when_seen` and uses
`Axis_set.is_subset`. With masks, this must become mask-aware state:

- record the relevant mask seen for a type, not only the axes seen
- use `Mask.le` for subset checks
- use `Mask.join` when merging seen requirements
- derive a plain `Axis_set` only for diagnostics or for APIs that truly need a
  list of axis names

This avoids treating `read` and `write` as the same cache key just because both
are on the visibility axis.

The early-exit optimization also needs a mask-aware saturation check. A bound
that has reached `shareable` is saturated for a `shareable` mask even though it
is not max on the whole portability axis.

## Where `Axis_set` should remain

Do not replace all `Axis_set` uses mechanically.

Some code still needs a plain set of axes, for example:

- diagnostics that list the axes involved in an error
- genuinely whole-axis operations that are independent of modality masks

For those cases, derive an axis set from the mask only when needed. An internal
helper backed by `Axis_lattice.non_bot_axes` plus
`axis_number_to_axis_packed` can support this.

## Refactor steps

1. Add mask helpers backed by `Axis_lattice`.
   - Add a direct modality-to-mask helper.
   - Add helper functions to detect whether a mask is bottom/top on a given
     axis if needed.
   - Add a helper to convert non-bottom axes in a mask to `Axis_set.t` if
     needed by existing control-flow code.

2. Change `With_bounds_type_info.t`.
   - Replace `relevant_axes` with `relevant_bounds`.
   - Update `Types.With_bounds_types` users.
   - Update printers and debug output.

3. Update construction sites.
   - `With_bounds.add_modality`
   - `Jkind.Pjk_with` translation
   - `Jkind.for_abbreviation`
   - `Btype.Jkind0.Jkind_desc.add_with_bounds`, including its arrow fast path
   - any other direct `{ relevant_axes = ... }` construction

4. Update `Jkind.apply_modality_l` and `Jkind.apply_modality_r`.
   - For `apply_modality_l`, apply the modality mask to existing with-bounds
     instead of intersecting axis sets.
   - Preserve the existing `apply_modality_r` contract that with-bounds are left
     unchanged, unless a separate proof and tests justify changing it.
   - For the direct `mod_bounds` part, use `Mod_bounds.cap_by_mask_l` for the
     left-side operation and `Mod_bounds.relax_by_mask_r` for the right-side
     operation.
   - Require tests for the adjunction law of `cap_by_mask_l` and
     `relax_by_mask_r`.

5. Update `Jkind.Base_and_axes.normalize`.
   - Replace recursion/fuel seen-state with mask-aware state.
   - Use `Mod_bounds.cap_by_mask_l payload_bound relevant_bounds` for the
     actual payload contribution.
   - Convert between `Mod_bounds.t` and the mask representation through helper
     APIs, not ad hoc conversions at every call site.
   - Add a mask-aware saturation check so bounded masks do not force needless
     expansion until fuel runs out.

6. Update `Jkind.sub_jkind_l`.
   - Replace `Axis_set.diff` in the `MB_WITH` path with `Mod_bounds.residual_mask`
     or an equivalent mask residual.
   - Include the generalized `axes_max_on_right` / direct-bound saturation mask
     in the residual.
   - Add examples/tests for partial cancellation on diamond axes.

7. Update `Ikind`.
   - Remove the `Axis_lattice.of_axis_set` conversion for with-bounds.
   - Use `bound_info.relevant_bounds` directly.
   - Use the shared modality-to-mask helper for field and constructor payloads,
     preserving middle values.

8. Update diagnostics and output.
   - Any diagnostic currently checking `Axis_set.mem relevant_axes axis` should
     check whether the axis has a non-bottom contribution in
     `relevant_bounds`.
   - Replace whole-axis "ignored by modalities" formatting with a mask-aware
     path.
   - Add expected-output tests for normal jkind printing and subkind error
     details involving middle masks.
   - Preserve existing wording where possible.

9. Update unsafe-mode-crossing equality.
   - `Types.equal_unsafe_mode_crossing` must compare `With_bounds_type_info.t`
     values, or normalize them to an explicitly equivalent form before
     comparing.
   - Add mismatch tests where the same bound type appears under sibling masks,
     such as `read` vs `write` and `shareable` vs `corruptible`.

10. Add or update tests.
   - Promote the existing "Doesn't work yet" tests for `shareable`.
   - Add matching tests for `shared`, `corrupted`, `corruptible`, `read`,
     `write`, `reading`, and `writing`.
   - Include both record fields and constructor arguments if both paths are
     expected to work.
   - Add with-bound subsumption tests that exercise partial cancellation.
   - Add direct mask algebra tests for modality-to-mask conversion, residuals,
     saturation, and the `cap_by_mask_l` / `relax_by_mask_r` adjunction.
   - Add tests that force `Loop_control` to see the same type head under sibling
     masks in both orders.
   - Add tests for `apply_modality_l` and `apply_modality_r` on jkinds with
     with-bounds.
   - Add unsafe-mode-crossing mismatch tests for sibling masks.
   - Add externality regression tests for `Pjk_with` with no explicit
     externality, `internal`, `external64`, and `external_`.

## Suggested test cases

Start from existing tests:

- `testsuite/tests/typing-modes/shareable.ml`
- `testsuite/tests/typing-modes/crossing.ml`
- `testsuite/tests/typing-modes/statefulness-visibility.ml`
- `testsuite/tests/typing-modal-kinds/basics.ml`
- `testsuite/tests/typing-jkind-bounds/`, including paired `_ikinds` tests
- `oxcaml/tests/typing/axis_lattice_roundtrip_test.ml` or a nearby direct
  mask-algebra test

Important expected successes:

```ocaml
type t
type s : value mod shareable = { v : t @@ shareable } [@@unboxed]
type u : value mod corruptible = { v : t @@ corruptible } [@@unboxed]
```

```ocaml
type t
type s : value mod shared = { v : t @@ shared } [@@unboxed]
type u : value mod corrupted = { v : t @@ corrupted } [@@unboxed]
```

```ocaml
type t
type s : value mod read = { v : t @@ read } [@@unboxed]
type u : value mod write = { v : t @@ write } [@@unboxed]
```

```ocaml
type t
type s : value mod reading = { v : t @@ reading } [@@unboxed]
type u : value mod writing = { v : t @@ writing } [@@unboxed]
```

Also keep negative tests that prove the middle modality is not rounded all the
way to the bottom/top of the axis or to the wrong sibling:

- `shareable` should not imply `portable`.
- `shareable` should not imply `corruptible`, and conversely.
- `shared` should not imply `uncontended`.
- `shared` should not imply `corrupted`, and conversely.
- `read` should not imply `write`, and conversely.
- `reading` should not imply `writing`, and conversely.
- Subkinding should cancel only the overlapping part of a mask. Cover both
  subtraction orders for each sibling pair: `shareable` / `corruptible`,
  `shared` / `corrupted`, `read` / `write`, and `reading` / `writing`.
- Repeated-type-head normalization should not depend on visit order. Test both
  `read` then `write` and `write` then `read`, and similarly for the other
  sibling pairs.
- Right-side modality application should satisfy the adjunction test:
  `cap_by_mask_l actual mask <= expected` iff
  `actual <= relax_by_mask_r expected mask`.

## Risks

- `Axis_lattice` orders monadic axes in the modal-kind direction, which differs
  from the user-facing mode order. Keep using existing conversion helpers
  instead of reinterpreting constructors by hand.
- `Axis_set` is still useful for traversal and diagnostics. Replacing it
  everywhere at once would make the refactor larger and easier to get wrong.
- Existing comments and variable names say "axes" in many places. Rename only
  where the value is now a bound mask; do not churn unrelated code.
- The nonmodal externality axis is packed into the internal lattice
  representation. Be explicit about whether modality masks should affect it.
- The `apply_modality_r` contract is asymmetric with `apply_modality_l`.
  Changing right-side with-bounds would be a separate semantic change.
- Converting masks back to axis sets for cache keys or residuals reintroduces
  the original bug for sibling middle elements.

## Validation

After the refactor compiles, run targeted tests first:

```sh
make -s test-one DIR=typing-modes
make -s test-one DIR=typing-modal-kinds
make -s test-one DIR=typing-jkind-bounds
```

If those pass and the change is ready for wider validation, run:

```sh
make -s boot-compiler
```

Do not run the full test suite until the local behavior is stable.
