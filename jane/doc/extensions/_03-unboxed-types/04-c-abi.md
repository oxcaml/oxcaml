---
layout: documentation-page
collectionName: Unboxed types
title: C ABI
---

# C ABI

This page describes how unboxed types are passed between OCaml and C stubs.
See also the [OCaml manual chapter on interfacing with
C](https://ocaml.org/manual/5.4/intfc.html), which documents the upstream
`[@unboxed]` and `[@untagged]` attributes.

Bytecode and native code represent unboxed types differently, so separate
versions of external bindings (or `caml_no_bytecode_impl`) are required.

## Unboxed numbers

To bind an unboxed number in a C stub, bind it to the natural C representation
on native, and treat it like a boxed/tagged number on bytecode, as enumerated by
the below table.

| OCaml layout | Native: bind to C type                  | Bytecode: bind to `value` and treat as this OCaml type |
|--------------|-----------------------------------------|--------------------------------------------------------|
| `float64`    | `double`                                | `float`                                                |
| `float32`    | `float`                                 | `float32`                                              |
| `bits64`     | `int64_t`                               | `int64`                                                |
| `bits32`     | `int32_t`                               | `int32`                                                |
| `bits16`     | `int16_t`                               | `int16`                                                |
| `bits8`      | `int8_t`                                | `int8`                                                 |
| `word`       | `intnat`                                | `nativeint`                                            |
| `void`       | `void` as a return; omit as an argument | `unit`                                                 |

[Small numbers](../../miscellaneous-extensions/small-numbers) and [SIMD vector
types](../../simd/intro) may also be passed to C stubs; see their respective
pages.

As an example, see the following.

```ocaml
(* foo.ml *)
external add_floats :
  float# -> float# -> float# =
  "add_floats_bytecode" "add_floats_native"
```
```c
// stubs.c
double add_floats_native(double a, double b) {
  return a + b;
}

value add_floats_bytecode(value a, value b) {
  CAMLparam2(a, b);
  CAMLreturn(caml_copy_double(Double_val(a) + Double_val(b)));
}
```

## Unboxed products

On native, an unboxed product is passed to and returned from a C stub as the
C struct whose members are the product's components, in order. A nested
product is a nested struct, and a component of layout `void` is left out. On
bytecode, the product is a single boxed block (as unboxed products are boxed in
bytecode).

```ocaml
external sum_and_diff :
  #(int32# * int32#) -> #(int32# * int32#) =
  "sum_and_diff_bytecode" "sum_and_diff_native"
```
```c
typedef struct { int32_t a; int32_t b; } pair;

pair sum_and_diff_native(pair p) {
  pair res = { p.a + p.b, p.a - p.b };
  return res;
}

value sum_and_diff_bytecode(value p) {
  CAMLparam1(p);
  CAMLlocal3(res, sum, diff);
  int32_t a = Int32_val(Field(p, 0));
  int32_t b = Int32_val(Field(p, 1));
  sum = caml_copy_int32(a + b);
  diff = caml_copy_int32(a - b);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = sum;
  Field(res, 1) = diff;
  CAMLreturn(res);
}
```

The C calling convention pads nested structs, making nesting in the unboxed
product type significant. For example, `#(#(int32# * int16#) * int16#)` is the
12-byte struct

```c
typedef struct { struct { int32_t a; int16_t b; } inner; int16_t c; } t;
```

whereas the flat `#(int32# * int16# * int16#)` is an 8-byte struct.

Components of layout `bits8` and `bits16` are allowed inside products. Unlike
scalar arguments (see [Small numbers](../../miscellaneous-extensions/small-numbers)),
they do not need `[@unsafe_unextended]`, because C does not assume that struct
members are extended.

### Supported products {#supported-products}

Native code supports x86-64 (Linux and macOS) and arm64 (Linux and macOS).
Windows is not supported. Products that C passes by reference or returns in
memory are not supported. Specifically:

- Arguments: on x86-64, any product; on arm64, products of at most 16
  bytes, or of up to four components that are all `float64`, all `float32`, or
  all 128-bit vectors (C's "homogeneous aggregates"), such as
  `#(float# * float# * float# * float#)`;
- Returns: products of at most 16 bytes, and on arm64 homogeneous
  aggregates.

As in C, a product argument that does not fit in the remaining argument
registers (or, on x86-64, is over 16 bytes) is passed on the stack.

Unsupported declarations are rejected when compiling to native code, e.g. on
arm64:

```
Error: The unboxed product argument of the external call to f_native cannot be
       compiled: the corresponding C struct (24 bytes) would be passed by
       reference, since it is larger than 16 bytes and not a homogeneous
       floating-point or vector aggregate of at most four members. Only
       unboxed product arguments that C passes by value are currently
       supported.
```

To pass such a product, split it into smaller products, or use
[`[@unpacked]`](#unpacked).

A product with a single non-void component, such as `#(int * unit#)`, is passed
like that component in most cases (the exception being macOS arm64 with
`bits{8,16,32}`).

## Unboxed product arguments: `[@unpacked]` {#unpacked}

The `[@unpacked]` attribute on a product argument passes each component of the
product as a separate argument to the native stub, instead of as a struct.

```ocaml
external add_i64_int :
  (#(int64_u * int) [@unpacked]) -> int64_u =
  "add_i64_int_bytecode" "add_i64_int_native"
```
```c
int64_t add_i64_int_native(int64_t a, value b) {
  return a + Long_val(b);
}

value add_i64_int_bytecode(value prod) {
  CAMLparam1(prod);
  int64_t a = Int64_val(Field(prod, 0));
  long b = Long_val(Field(prod, 1));
  CAMLreturn(caml_copy_int64(a + b));
}
```

On native, nested products are also flattened, and components of layout `void`
are erased. On bytecode, nested products are nested blocks, and a component of
layout `void` is represented like `unit`. For example,
`#(int * #(unit# * int))` is treated like two `int` arguments on native, and
like one `int * (unit * int)` argument on bytecode.
```ocaml
external add_with_void :
  (#(int * #(unit# * int)) [@unpacked]) -> int =
  "add_with_void_bytecode" "add_with_void_native"
```
```c
value add_with_void_native(value a, value b) {
  return Val_long(Long_val(a) + Long_val(b));
}

value add_with_void_bytecode(value prod) {
  value inner = Field(prod, 1); /* fields: unit# (0) and int (1) */
  return Val_long(Long_val(Field(prod, 0)) + Long_val(Field(inner, 1)));
}
```

`[@unpacked]` may only be attached to arguments, and cannot be combined with
`[@unboxed]` or `[@untagged]`. Since the components become separate arguments,
components of layout `bits8` or `bits16` follow the rules for scalar
arguments.

### `[@unpacked]` products are not C structs

Unpacked components are separate arguments, whereas C structs have their own
calling convention (e.g. a struct that does not fit in the remaining argument
registers is passed entirely on the stack). Consider the following:

```ocaml
external f :
  int -> int -> int -> int -> int ->
  (#(int * int) [@unpacked]) -> int =
  "f_bytecode" "f_native"
```

On x86-64, the five `int`s occupy the first five argument registers, the first
component of the unpacked product uses the sixth register, and the
second component is passed on the stack. The correct native binding is the
following:
```c
value f_native(value a, value b, value c, value d, value e,
               value x, value y);
```

Without `[@unpacked]`, the product would be a two-word struct that does not fit
in the one remaining register, so the whole struct would be passed on the
stack.
