/* Stubs for unboxed_args.ml. The native versions take and return C structs
   by value, so that the calling conventions used by the OCaml compiler for
   unboxed products are checked against the C compiler's implementation of
   the underlying ABIs. The bytecode versions receive and return products as
   (possibly nested) tuples. */

#include <stdint.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/float32.h"

typedef struct { int64_t a; int64_t b; } args_i64_i64;
typedef struct { int32_t a; int32_t b; } args_i32_i32;
typedef struct { int16_t a; int32_t b; } args_i16_i32;
typedef struct { double a; double b; } args_f64_f64;
typedef struct { float a; float b; float c; float d; } args_f32x4;
typedef struct { float a; int32_t b; } args_f32_i32;
typedef struct { value a; double b; } args_int_f64;
typedef struct { double a; int64_t b; } args_f64_i64;
typedef struct { args_i32_i32 p; int64_t c; } args_nested;
typedef struct { int32_t a; int32_t b; int64_t c; } args_i32_i32_i64;
typedef struct { int32_t a; float b; } args_i32_f32;
typedef struct { float a; int32_t b; } args_f32_i32_ret;

int64_t sum_i64_pair_native(args_i64_i64 p) {
  return p.a + p.b;
}

value sum_i64_pair_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_int64(Int64_val(Field(p, 0)) + Int64_val(Field(p, 1))));
}

int64_t sum_i32_pair_native(args_i32_i32 p) {
  return (int64_t) p.a + (int64_t) p.b;
}

value sum_i32_pair_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int32_val(Field(p, 0))
    + (int64_t) Int32_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_i16_i32_native(args_i16_i32 p) {
  return (int64_t) p.a + (int64_t) p.b;
}

value sum_i16_i32_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int16_val(Field(p, 0))
    + (int64_t) Int32_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

double sum_f64_pair_native(args_f64_f64 p) {
  return p.a + p.b;
}

value sum_f64_pair_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_double(Double_val(Field(p, 0))
                              + Double_val(Field(p, 1))));
}

double sum_f32_quad_native(args_f32x4 p) {
  return (double) p.a + (double) p.b + (double) p.c + (double) p.d;
}

value sum_f32_quad_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + (double) Float32_val(Field(p, 1))
    + (double) Float32_val(Field(p, 2))
    + (double) Float32_val(Field(p, 3));
  CAMLreturn(caml_copy_double(res));
}

double sum_f32_i32_native(args_f32_i32 p) {
  return (double) p.a + (double) p.b;
}

value sum_f32_i32_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + (double) Int32_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

double sum_int_f64_native(args_int_f64 p) {
  return (double) Long_val(p.a) + p.b;
}

value sum_int_f64_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Long_val(Field(p, 0)) + Double_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

double sum_f64_i64_native(args_f64_i64 p) {
  return p.a + (double) p.b;
}

value sum_f64_i64_bytecode(value p) {
  CAMLparam1(p);
  double res = Double_val(Field(p, 0)) + (double) Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

int64_t sum_nested_native(args_nested p) {
  return (int64_t) p.p.a + (int64_t) p.p.b + p.c;
}

value sum_nested_bytecode(value p) {
  CAMLparam1(p);
  value inner = Field(p, 0);
  int64_t res = (int64_t) Int32_val(Field(inner, 0))
    + (int64_t) Int32_val(Field(inner, 1)) + Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_two_pairs_native(args_i64_i64 p, args_i64_i64 q) {
  return p.a + p.b + q.a + q.b;
}

value sum_two_pairs_bytecode(value p, value q) {
  CAMLparam2(p, q);
  int64_t res = Int64_val(Field(p, 0)) + Int64_val(Field(p, 1))
    + Int64_val(Field(q, 0)) + Int64_val(Field(q, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_three_pairs_native(args_i64_i64 p, args_i64_i64 q,
                               args_i64_i64 r) {
  return p.a + p.b + q.a + q.b + r.a + r.b;
}

value sum_three_pairs_bytecode(value p, value q, value r) {
  CAMLparam3(p, q, r);
  int64_t res = Int64_val(Field(p, 0)) + Int64_val(Field(p, 1))
    + Int64_val(Field(q, 0)) + Int64_val(Field(q, 1))
    + Int64_val(Field(r, 0)) + Int64_val(Field(r, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t mixed_args_native(int64_t x, args_f64_f64 fp, value n,
                          args_i32_i32 ip, double d) {
  return x + Long_val(n) + (int64_t) ip.a + (int64_t) ip.b
    + (int64_t) (fp.a + fp.b + d);
}

value mixed_args_bytecode(value x, value fp, value n, value ip, value d) {
  CAMLparam5(x, fp, n, ip, d);
  int64_t res = Int64_val(x) + Long_val(n)
    + (int64_t) Int32_val(Field(ip, 0)) + (int64_t) Int32_val(Field(ip, 1))
    + (int64_t) (Double_val(Field(fp, 0)) + Double_val(Field(fp, 1))
                 + Double_val(d));
  CAMLreturn(caml_copy_int64(res));
}

/* #(int64# * unit#): the void component is erased in native code. */

int64_t sum_i64_void_native(int64_t x) {
  return x;
}

value sum_i64_void_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_int64(Int64_val(Field(p, 0))));
}

args_i32_i32_i64 ret_i32i32_i64_native(value unit) {
  args_i32_i32_i64 res = { -11, 22, -3333 };
  return res;
}

value ret_i32i32_i64_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(res, a, b, c);
  a = caml_copy_int32(-11);
  b = caml_copy_int32(22);
  c = caml_copy_int64(-3333);
  res = caml_alloc_small(3, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  Field(res, 2) = c;
  CAMLreturn(res);
}

args_f32x4 ret_f32_quad_native(value unit) {
  args_f32x4 res = { 0.5f, -1.5f, 2.25f, -4.0f };
  return res;
}

value ret_f32_quad_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal5(res, a, b, c, d);
  a = caml_copy_float32(0.5f);
  b = caml_copy_float32(-1.5f);
  c = caml_copy_float32(2.25f);
  d = caml_copy_float32(-4.0f);
  res = caml_alloc_small(4, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  Field(res, 2) = c;
  Field(res, 3) = d;
  CAMLreturn(res);
}

args_nested ret_nested_native(value unit) {
  args_nested res = { { -77, 88 }, 9999 };
  return res;
}

value ret_nested_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(res, inner, a, b);
  a = caml_copy_int32(-77);
  b = caml_copy_int32(88);
  inner = caml_alloc_small(2, 0);
  Field(inner, 0) = a;
  Field(inner, 1) = b;
  a = caml_copy_int64(9999);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = inner;
  Field(res, 1) = a;
  CAMLreturn(res);
}

args_f32_i32_ret swap_mixed_native(args_i32_f32 p) {
  args_f32_i32_ret res = { p.b, p.a };
  return res;
}

value swap_mixed_bytecode(value p) {
  CAMLparam1(p);
  CAMLlocal3(res, a, b);
  a = caml_copy_float32(Float32_val(Field(p, 1)));
  b = caml_copy_int32(Int32_val(Field(p, 0)));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  CAMLreturn(res);
}
