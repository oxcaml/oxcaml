#include <stdint.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

typedef struct {
  uint64_t a;
  uint64_t b;
} ui64_ui64_struct;

ui64_ui64_struct ui64_ui64_make(void) {
  uint64_t a = 123;
  uint64_t b = 456;
  ui64_ui64_struct res = { a, b };
  return res;
}

value ui64_ui64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int64(123);
  b = caml_copy_int64(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  uint64_t a;
  double b;
} ui64_f64_struct;

ui64_f64_struct ui64_f64_make(void) {
  uint64_t a = 123;
  double b = 456;
  ui64_f64_struct res = { a, b };
  return res;
}

value ui64_f64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int64(123);
  b = caml_copy_double(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  double a;
  uint64_t b;
} f64_ui64_struct;

f64_ui64_struct f64_ui64_make(void) {
  double a = 123;
  uint64_t b = 456;
  f64_ui64_struct res = { a, b };
  return res;
}

value f64_ui64_make_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(123);
  b = caml_copy_int64(456);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn (res);
}

typedef struct {
  double a;
  double b;
} f64_f64_struct;

f64_f64_struct f64_f64_make(void) {
  double a = 123;
  double b = 456;
  f64_f64_struct res = { a, b };
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
  CAMLreturn (res);
}

/* Stubs for unboxed_return3.ml. The native versions return C structs by
   value, so that the calling convention used by the OCaml compiler for
   unboxed product returns is checked against the C compiler's implementation
   of the underlying ABI. */

#include "caml/float32.h"

typedef struct { int32_t a; int32_t b; } i32_i32_struct;

i32_i32_struct ret_i32_i32_native(value unit) {
  i32_i32_struct res = { -123456789, 987654321 };
  return res;
}

value ret_i32_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int32(-123456789);
  b = caml_copy_int32(987654321);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { int16_t a; int16_t b; } i16_i16_struct;

i16_i16_struct ret_i16_i16_native(value unit) {
  i16_i16_struct res = { -12345, 6789 };
  return res;
}

value ret_i16_i16_bytecode(value unit) {
  value res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int16(-12345);
  Field(res, 1) = Val_int16(6789);
  return res;
}

typedef struct { int8_t a; int8_t b; } i8_i8_struct;

i8_i8_struct ret_i8_i8_native(value unit) {
  i8_i8_struct res = { -100, 27 };
  return res;
}

value ret_i8_i8_bytecode(value unit) {
  value res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int8(-100);
  Field(res, 1) = Val_int8(27);
  return res;
}

typedef struct { int8_t a; int16_t b; } i8_i16_struct;

i8_i16_struct ret_i8_i16_native(value unit) {
  i8_i16_struct res = { -5, -1234 };
  return res;
}

value ret_i8_i16_bytecode(value unit) {
  value res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int8(-5);
  Field(res, 1) = Val_int16(-1234);
  return res;
}

typedef struct { int16_t a; int32_t b; } i16_i32_struct;

i16_i32_struct ret_i16_i32_native(value unit) {
  i16_i32_struct res = { -321, 100000 };
  return res;
}

value ret_i16_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, b);
  b = caml_copy_int32(100000);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int16(-321);
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { int8_t a; int32_t b; } i8_i32_struct;

i8_i32_struct ret_i8_i32_native(value unit) {
  i8_i32_struct res = { 77, -100000 };
  return res;
}

value ret_i8_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, b);
  b = caml_copy_int32(-100000);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int8(77);
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { float a; int32_t b; } f32_i32_struct;

f32_i32_struct ret_f32_i32_native(value unit) {
  f32_i32_struct res = { -1.25f, -7 };
  return res;
}

value ret_f32_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_float32(-1.25f);
  b = caml_copy_int32(-7);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { int32_t a; float b; } i32_f32_struct;

i32_f32_struct ret_i32_f32_native(value unit) {
  i32_f32_struct res = { 42, 2.5f };
  return res;
}

value ret_i32_f32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int32(42);
  b = caml_copy_float32(2.5f);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { int16_t a; float b; } i16_f32_struct;

i16_f32_struct ret_i16_f32_native(value unit) {
  i16_f32_struct res = { -2, 8.125f };
  return res;
}

value ret_i16_f32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, b);
  b = caml_copy_float32(8.125f);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int16(-2);
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { float a; float b; } f32_f32_struct;

f32_f32_struct ret_f32_f32_native(value unit) {
  f32_f32_struct res = { 1.25f, -3.5f };
  return res;
}

value ret_f32_f32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_float32(1.25f);
  b = caml_copy_float32(-3.5f);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { float a; double b; } f32_f64_struct;

f32_f64_struct ret_f32_f64_native(value unit) {
  f32_f64_struct res = { 1.5f, -0.0625 };
  return res;
}

value ret_f32_f64_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_float32(1.5f);
  b = caml_copy_double(-0.0625);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { double a; float b; } f64_f32_struct;

f64_f32_struct ret_f64_f32_native(value unit) {
  f64_f32_struct res = { -4.5, 100.25f };
  return res;
}

value ret_f64_f32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(-4.5);
  b = caml_copy_float32(100.25f);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { int32_t a; double b; } i32_f64_struct;

i32_f64_struct ret_i32_f64_native(value unit) {
  i32_f64_struct res = { -9, 3.5 };
  return res;
}

value ret_i32_f64_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_int32(-9);
  b = caml_copy_double(3.5);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { double a; int32_t b; } f64_i32_struct;

f64_i32_struct ret_f64_i32_native(value unit) {
  f64_i32_struct res = { 0.125, -77 };
  return res;
}

value ret_f64_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_double(0.125);
  b = caml_copy_int32(-77);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { value a; double b; } int_f64_struct;

int_f64_struct ret_int_f64_native(value unit) {
  int_f64_struct res = { Val_long(12), -2.5 };
  return res;
}

value ret_int_f64_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, b);
  b = caml_copy_double(-2.5);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_long(12);
  Field(res, 1) = b;
  CAMLreturn(res);
}

typedef struct { double a; value b; } f64_int_struct;

f64_int_struct ret_f64_int_native(value unit) {
  f64_int_struct res = { 6.75, Val_long(-3) };
  return res;
}

value ret_f64_int_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, a);
  a = caml_copy_double(6.75);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = Val_long(-3);
  CAMLreturn(res);
}

typedef struct { value a; value b; } int_int_struct;

int_int_struct ret_int_int_native(value unit) {
  int_int_struct res = { Val_long(17), Val_long(-29) };
  return res;
}

value ret_int_int_bytecode(value unit) {
  value res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_long(17);
  Field(res, 1) = Val_long(-29);
  return res;
}

typedef struct { intnat a; int32_t b; } n_i32_struct;

n_i32_struct ret_n_i32_native(value unit) {
  n_i32_struct res = { 4660, -1 };
  return res;
}

value ret_n_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, a, b);
  a = caml_copy_nativeint(4660);
  b = caml_copy_int32(-1);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

/* #(int64# * unit#): the void component is erased in native code, so the
   native stub returns a plain int64_t. In bytecode the void component
   occupies a field of the returned tuple. */

int64_t ret_i64_void_native(value unit) {
  return 999;
}

value ret_i64_void_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, a);
  a = caml_copy_int64(999);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = Val_unit;
  CAMLreturn(res);
}

/* #(unit# * float#): likewise the native stub returns a plain double. */

double ret_void_f64_native(value unit) {
  return 2.25;
}

value ret_void_f64_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, b);
  b = caml_copy_double(2.25);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_unit;
  Field(res, 1) = b;
  CAMLreturn(res);
}

/* Ten tagged-integer arguments (some of which are passed on the stack on
   both x86-64 and arm64) with an unboxed product return. */

typedef struct { int32_t a; float b; } combine10_struct;

combine10_struct combine10_native(value x1, value x2, value x3, value x4,
                                  value x5, value x6, value x7, value x8,
                                  value x9, value x10) {
  long sum = Long_val(x1) + Long_val(x2) + Long_val(x3) + Long_val(x4)
    + Long_val(x5) + Long_val(x6) + Long_val(x7) + Long_val(x8)
    + Long_val(x9) + Long_val(x10);
  combine10_struct res = { (int32_t) sum, (float) (2 * sum) };
  return res;
}

value combine10_bytecode(value *argv, int argn) {
  CAMLparam0();
  CAMLlocal3(res, a, b);
  long sum = 0;
  int i;
  for (i = 0; i < argn; i++) sum += Long_val(argv[i]);
  a = caml_copy_int32((int32_t) sum);
  b = caml_copy_float32((float) (2 * sum));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}

/* An [@unpacked] product argument (components passed as separate C
   arguments) with an unboxed product return (following the C struct
   convention). */

typedef struct { int64_t a; double b; } i64_f64_struct;

i64_f64_struct swap_f64_i64_native(double a, int64_t b) {
  i64_f64_struct res = { b, a };
  return res;
}

value swap_f64_i64_bytecode(value prod) {
  CAMLparam1(prod);
  CAMLlocal3(res, a, b);
  a = caml_copy_int64(Int64_val(Field(prod, 1)));
  b = caml_copy_double(Double_val(Field(prod, 0)));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}
