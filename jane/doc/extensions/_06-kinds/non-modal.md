---
layout: documentation-page
collectionName: Kinds
title: Non-modal bounds
---

# Non-modal bounds

# Nullability

The nullability axis records whether `NULL` (the machine word 0) is a possible
value of a type, and is used to support the non-allocating option `'a or_null`
type. The axis has two possible values, with `non_null < maybe_null`. A type may
be `non_null` if none of its values are `NULL`.

The kind of values with `NULL` added as a possibility is written
`value_or_null`. The more common `value` kind is an abbreviation for
`value_or_null mod non_null separable`.

Types that don't have `NULL` as a possible value are
compatible with `or_null`, a non-allocating option type that is built into
OxCaml.  Its definition is:
```ocaml
type ('a : value) or_null : value_or_null =
  | Null
  | This of 'a
```

## Separability

The separability axis records whether a type can have float or non-float values, where a float value is a pointer to an allocated block tagged with `Double_tag` (which is what `float` values look like).
This axis has three possible values, with `non_float < separable < maybe_separable`. A type is `non_float` if none of its elements are floats, and a type is `separable` if either all or none of its elements are floats. Separability is used to track types for which it is safe
to apply the float array optimization.

`value_or_null` is considered `maybe_separable`, since `float or_null` has both float
and non-float elements. However, all types in vanilla OCaml are `separable`.
