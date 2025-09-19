---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Small Numbers
---

# Small Numbers

The small numbers extension adds `float32`, `int16`, and `int8` types to OxCaml.

## Float32

When small numbers are enabled, the following float32 types are available:

```
float32
float32#
float32 array
float32# array
```

Literals use the `s` suffix:

```
1.0s  : float32
#1.0s : float32#
```

Pattern matching on `float32`s is not supported.

### Operations

Operations on 32-bit floats are available via the `Stdlib_stable.Float32` and
`Stdlib_stable.Float32_u` libraries, which provide `Base`-like APIs.

### Representation

The boxed `float32` type is encoded as a custom block with similar semantics to
`int32`.  Similarly, `float32 array` is a typical OxCaml array containing boxed
elements.

The `float32#` type is unboxed:

- Function arguments and returns of type `float32#` are passed using
  floating-point registers.

- Record fields of type `float32#` are not boxed, but each take up one word of
  space.  Using float32 records requires the mixed blocks extension, which is
  also enabled by default.

- Arrays of type `float32# array` contain tightly packed unboxed float32
  elements.  The array itself is a custom block with similar semantics to
  `int32# array`.

Like floats, compiler optimizations allow boxed float32s to remain unboxed while
being manipulated within the scope of a function.

### C ABI

Both boxed and unboxed float32s may be passed to C stubs.  The OxCaml runtime
provides helper functions for working with float32s.

```ocaml
external float32_stub : (float32[@unboxed]) -> (float32[@unboxed]) =
  "boxed_float32_stub" "unboxed_float32_stub"

external float32_hash_stub : float32# -> float32# =
  "boxed_float32_stub" "unboxed_float32_stub"

(* ... *)
```
```c
#include <caml/float32.h>

float unboxed_float32_stub(float v) {
  return v;
}

value boxed_float32_stub(value v) {
  return caml_copy_float32(unboxed_float32_stub(Float32_val(v)));
}
```

## Int8 / Int16

When small numbers are enabled, the following types are available:
```
int8
int8#
int16
int16#
int8 array
int8# array
int16 array
int16# array
```

Pattern matching is supported for all of these types.

Literals use `s` for `int8` and `S` for `int16`:
```
42s  : int8
#42s : int8#
42S  : int16
#42S : int16#
```

### Operations

Operations on small integers are available via the `Stdlib_stable.Int8`,
`Stdlib_stable.Int8_u`, `Stdlib_stable.Int16`, and `Stdlib_stable.Int16_u`
libraries.

### Representation

The boxed `int8` and `int16` types are encoded as tagged immediates, similar to
regular OCaml `int`s. Similarly, `int8 array` and `int16 array` are not packed.

The unboxed `int8#` and `int16#` types are passed around using general purpose
registers, but do not have a tag bit, unlike `int8` and `int16`. The ints in
`int8# array`s and `int16# array`s are packed, but they are not packed in any
other context. For example, an `int8# array` of length 30 takes up 4 words of
space, plus the header word, but a `#(int8# * int8#)` takes up 2 words of space
and requires 2 registers to pass around.

## Untagged Char

When small numbers are enabled, the types `char#` and `char# array`
are available.

Literals are prefixed with `#`:
```
#'a'     : char#
#'\123'  : char#
#'\o123' : char#
#'\xff'  : char#
```

Like regular char literals, untagged char literals can be used in patterns and
in ranges:
```
match x with
| #'a' -> f ()
| #'a'..#'z' -> g ()
```

### Operations

Operations on untagged chars are available via the `Stdlib_stable.Char_u`
library.

### Representation

Untagged chars have the same layout as `int8#`, and `char# array`s are packed
like `int8# array`s.
