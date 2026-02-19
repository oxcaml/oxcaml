---
layout: documentation-page
collectionName: Kinds
title: Non-modal bounds
---

# Non-modal bounds

## Externality

The externality axis records whether all a type's values may safely be ignored
by the GC.  This may be because they are OCaml "immediates" (values represented
by a tagged integer), because they are unboxed types like `float#` or `int32#`,
or because they are allocated elsewhere.

The axis has three possible values, with `external_ < external64 < internal`.
* `external_` means that all values of the type are safely ignored by the
  GC.
* `external64` means that all values of the type are safely ignored by the GC
  _on 64-bit systems_. The only 32-bit target currently supported by the OxCaml
  compiler is bytecode. Note that, although JavaScript and WASM are 32-bit
  platforms and the compiler goes through bytecode to reach them, they still
  count as 64-bit systems for the purpose of this axis because of their unique
  data models.
* `internal` means values of the type may need to be scanned.

The compiler uses the externality axis for certain runtime optimizations. In
particular, updating a mutable reference to a type that is `external_` can skip
the write barrier (i.e., it does not need a call to `caml_modify`).

Writes of types with non-`value` (i.e. unboxed) base layouts never require a
write barrier, as values of such types must never be seen by the garbage
collector. However, since externality is tracked as a non-modal property of
kinds, as opposed to part of its layout, it is possible to express types with
layout e.g. `float64` which are not external. This is very rarely desirable; it
is typically far more convenient to preserve externality information than it is
to enforce some semantic property of an unboxed type by hiding it. For this
reason, the kinds `bits8`, `bits16`, `bits32`, `bits64`, `float32`, `float64`,
`untagged_immediate`, `vec128`, `vec256`, `vec512`, `void`, and `word` all imply
`mod external_` by default.

To refer to the kinds of these base layouts _without_ externality crossing, one
may append the suffix `_internal`, e.g. `bits8_internal`. In other words,
`bits8` is synonymous with `bits8_internal mod external_`, and likewise for the
other non-`value` base layouts listed above.

In the future, we plan to make externality a mode, rather than just a property
of types.