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

## Unboxed product arguments: `[@unpacked]` {#unpacked}

C stubs can only take an unboxed product (e.g. `#(int64_u * int)`) as an
argument with the `[@unpacked]` attribute. This passes each component of the
product as a separate argument to the native stub, while the bytecode stub
receives the product as a single boxed block (as unboxed products are boxed in
bytecode).

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
`[@unboxed]` or `[@untagged]`.

### `[@unpacked]` products are not C aggregates

Unpacked components are treated as separate arguments, whereas C aggregates
have a distinguished calling convention (e.g. an aggregate that does not fit
in the remaining argument registers is passed entirely on the stack). Consider
the following:

```ocaml
external f :
  int -> int -> int -> int -> int ->
  (#(int * int) [@unpacked]) -> int =
  "f_bytecode" "f_native"
```

On x86-64 System V, the five `int`s occupy the first five argument registers,
the first component of the unpacked product uses the sixth register, and the
second component is passed on the stack. The correct native binding is the
following:
```c
value f_native(value a, value b, value c, value d, value e,
               value x, value y);
```

A C function that combined `x` and `y` into a two-word struct parameter would
instead expect the whole struct on the stack.

## Unboxed product returns

C stubs may return an unboxed product without any attribute, provided the
product is a pair whose components are not themselves products. The native
stub returns a two-field struct by value, and as usual, the bytecode stub
returns the product as a single boxed block.

The components of the returned pair are subject to the following restrictions:
- Each must be of layout `bits64`, `word`, `float64`, or `value`.
- `float32` and 128-bit vectors are accepted only on arm64.
- On arm64, both components must also have the same layout, so mixed pairs such
  as `#(int64_u * float#)` are supported only on x86-64.

```ocaml
external f64_f64_make : unit -> #(float# * float#) =
  "f64_f64_make_bytecode" "f64_f64_make"
```
```c
typedef struct {
  double a;
  double b;
} f64_f64_struct;

f64_f64_struct f64_f64_make(void) {
  f64_f64_struct res = { 123, 456 };
  return res;
}

value f64_f64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(123);
  b = caml_copy_double(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}
```
