---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Small Numbers
---

# Small Numbers

The small numbers extension adds the types `float32`, `int16`, and `int8` to
OxCaml.

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

The range of these literals is `-128s` to `128s` for `int8` and `-32768S` to
`32768S` for `int16`. Note that `128s` overflows to `-128s` since the max
value of an `int8` is 127. Similarly, `32768S` overflows to `-32768S`. This
behavior is consistent with the literals for larger integer types.

### Operations

Operations on small integers are available via the `Stdlib_stable.Int8`,
`Stdlib_stable.Int8_u`, `Stdlib_stable.Int16`, and `Stdlib_stable.Int16_u`
libraries.

### Representation

The types `int8` and `int16` are encoded as tagged immediates, similar to
regular OCaml `int`s. They are sign-extended to the full width of a tagged
immediate, so polymorphic hash and compare work as expected. `int8 array` and
`int16 array` are not packed.

The types `int8#` and `int16#` are passed around using general purpose
registers, but do not have a tag bit. They are sign-extended to the width of a
`nativeint#`. The ints in `int8# array`s and `int16# array`s are packed, but
they are not packed in any other context. For example, an `int8# array` of
length 30 takes up 4 words of memory, plus the header word, but a
`#(int8# * int8#)` takes up 2 words of memory and requires 2 registers to pass
around.

### Codegen

In general, the compiler only emits 64-bit instructions, even for 32/16/8-bit
operations, and results are sign-extended after every operation. The peephole
optimizer can remove some unnecessary sign-extensions, but there is still a
noticeable performance decrease compared to C.

### C ABI {#small-int-c-abi}

Both tagged and untagged `int8`s and `int16`s may be passed to C stubs, though
parameters of layout `bits8` and `bits16` (such as `int8#`) are banned by
default. Small int parameters should be sign/zero-extended to 32 bits
before being passed to C stubs.

```ocaml
external sign_extend_int16 : int16# -> int32# = "%int32#_of_int16#"
external sign_extend_int8 : int8# -> int32# = "%int32#_of_int8#"

external int16_stub : (int16[@unboxed]) -> (int16[@unboxed]) =
  "tagged_int16_stub" "untagged_int16_stub"

external int16_hash_stub : int32# -> int16# =
  "tagged_int16_stub" "untagged_int16_stub"

let int16_hash_stub (x : int16#) : int16# =
  int16_hash_stub (sign_extend_int16 x)

external int8_stub : (int8[@unboxed]) -> (int8[@unboxed]) =
  "tagged_int8_stub" "untagged_int8_stub"

external int8_hash_stub : int32# -> int8# =
  "tagged_int8_stub" "untagged_int8_stub"

let int8_hash_stub (x : int8#) : int8# =
  int8_hash_stub (sign_extend_int8 x)
```

Parameters of layout `bits8` and `bits16` are banned since Clang expects such
parameters to be sign/zero-extended to 32 bits. OxCaml's calling convention will
soon match that of System V and GCC, which treats all the upper bits as garbage.
If you are absolutely sure your C function does not use the upper bits, you may
use the `[@unsafe_unextended]` attribute to pass `bits8` and `bits16`
parameters.

```ocaml
external int16_hash_stub : (int16#[@unsafe_unextended]) -> int16# =
  "tagged_int16_stub" "untagged_int16_stub"

external int8_hash_stub : (int8#[@unsafe_unextended]) -> int8# =
  "tagged_int8_stub" "untagged_int8_stub"
```

The following is also valid, but discouraged:
```ocaml
external int16_stub_untagged : (int16[@untagged]) -> (int16[@untagged]) =
  "tagged_int16_stub" "untagged_int16_stub"
```
`[@untagged]` can be applied to any `immediate` type, and it doesn't sign
extend the stub return value, which is usually zero-extended. On the other
hand, the behavior of `[@unboxed]` depends on the particular type,
so the proper sign extensions are applied to the return value.

For example, the following implementation makes `int16_stub` and
`int16_hash_stub` valid, but `int16_stub_untagged` invalid:
```c
signed short untagged_int16_stub (short x) { return -2; }
```

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

### C ABI

Untagged chars may be passed to C stubs:
```ocaml
external char_hash_stub : (char[@untagged]) -> char# =
  "tagged_char_stub" "untagged_char_stub"
```

For reasons discussed in the [Small Int C ABI](#small-int-c-abi) section,
`char#` parameters are not recommended, but are possible with
`[@unsafe_unextended]`:
```ocaml
external char_hash_stub : (char#[@unsafe_unextended]) -> char# =
  "tagged_char_stub" "untagged_char_stub"
```

`char[@unboxed]` is not allowed in external declarations. As a reminder, you may
use upstream OCaml's `char[@untagged]` in correspondence with `intnat` in C.
