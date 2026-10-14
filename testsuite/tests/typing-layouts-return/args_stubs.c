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
typedef struct { int32_t a; int16_t b; } args_i32_i16;
typedef struct { args_i32_i16 p; int16_t c; } args_nested_padded;
typedef struct { int32_t a; int16_t b; int16_t c; } args_flat_i32_i16_i16;
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

/* The layouts of [args_nested_padded] and [args_flat_i32_i16_i16] differ
   (12 bytes versus 8), so these stubs check that the nesting of unboxed
   products is respected by the OCaml side. */
_Static_assert(sizeof(args_nested_padded) == 12,
               "nested struct with trailing padding should be 12 bytes");
_Static_assert(sizeof(args_flat_i32_i16_i16) == 8,
               "flattened struct should be 8 bytes");

int64_t sum_nested_padded_native(args_nested_padded p) {
  return (int64_t) p.p.a + (int64_t) p.p.b + (int64_t) p.c;
}

value sum_nested_padded_bytecode(value p) {
  CAMLparam1(p);
  value inner = Field(p, 0);
  int64_t res = (int64_t) Int32_val(Field(inner, 0))
    + (int64_t) Int16_val(Field(inner, 1)) + (int64_t) Int16_val(Field(p, 1));
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

/* A one-member struct passed on the stack (after the integer argument
   registers have been exhausted), between two int32_t arguments. On Apple
   arm64 the struct takes an 8-byte stack slot aligned to 8 bytes, unlike a
   bare int16_t, which takes 2 bytes. */

typedef struct { int16_t a; } args_i16;

int64_t stack_single_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                            int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                            int32_t x, args_i16 s, int32_t y) {
  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + 10 * (int64_t) x
    + 100 * (int64_t) s.a + 1000 * (int64_t) y;
}

value stack_single_bytecode(value *argv, int argn) {
  CAMLparam0();
  int64_t sum = 0;
  int i;
  for (i = 0; i < 8; i++) sum += Int64_val(argv[i]);
  sum += 10 * (int64_t) Int32_val(argv[8]);
  sum += 100 * (int64_t) Int16_val(Field(argv[9], 0));
  sum += 1000 * (int64_t) Int32_val(argv[10]);
  CAMLreturn(caml_copy_int64(sum));
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

args_nested_padded ret_nested_padded_native(value unit) {
  args_nested_padded res = { { -77777, 1234 }, -5678 };
  return res;
}

value ret_nested_padded_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(res, inner, a);
  a = caml_copy_int32(-77777);
  inner = caml_alloc_small(2, 0);
  Field(inner, 0) = a;
  Field(inner, 1) = Val_int16(1234);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = inner;
  Field(res, 1) = Val_int16(-5678);
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

/* More than two components per register. */

typedef struct { int16_t a; int16_t b; int16_t c; int16_t d; } args_i16x4;
typedef struct { int8_t a; int8_t b; int16_t c; int32_t d; } args_i8_i8_i16_i32;

_Static_assert(sizeof(args_i16x4) == 8, "four int16s should be 8 bytes");
_Static_assert(sizeof(args_i8_i8_i16_i32) == 8,
               "int8, int8, int16, int32 should be 8 bytes");

int64_t sum_i16_quad_native(args_i16x4 p) {
  return (int64_t) p.a + (int64_t) p.b + (int64_t) p.c + (int64_t) p.d;
}

value sum_i16_quad_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int16_val(Field(p, 0))
    + (int64_t) Int16_val(Field(p, 1)) + (int64_t) Int16_val(Field(p, 2))
    + (int64_t) Int16_val(Field(p, 3));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_i8_i8_i16_i32_native(args_i8_i8_i16_i32 p) {
  return (int64_t) p.a + (int64_t) p.b + (int64_t) p.c + (int64_t) p.d;
}

value sum_i8_i8_i16_i32_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int8_val(Field(p, 0))
    + (int64_t) Int8_val(Field(p, 1)) + (int64_t) Int16_val(Field(p, 2))
    + (int64_t) Int32_val(Field(p, 3));
  CAMLreturn(caml_copy_int64(res));
}

args_i16x4 ret_i16_quad_native(value unit) {
  args_i16x4 res = { -1, 2, -3000, 4000 };
  return res;
}

value ret_i16_quad_bytecode(value unit) {
  value res = caml_alloc_small(4, 0);
  Field(res, 0) = Val_int16(-1);
  Field(res, 1) = Val_int16(2);
  Field(res, 2) = Val_int16(-3000);
  Field(res, 3) = Val_int16(4000);
  return res;
}

args_i8_i8_i16_i32 ret_i8_i8_i16_i32_native(value unit) {
  args_i8_i8_i16_i32 res = { -100, 50, -20000, 1000000 };
  return res;
}

value ret_i8_i8_i16_i32_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, d);
  d = caml_copy_int32(1000000);
  res = caml_alloc_small(4, 0);
  Field(res, 0) = Val_int8(-100);
  Field(res, 1) = Val_int8(50);
  Field(res, 2) = Val_int16(-20000);
  Field(res, 3) = d;
  CAMLreturn(res);
}

/* Floating-point components in less common positions. */

typedef struct { float a; int64_t b; } args_f32_i64;
typedef struct { float a; float b; float c; } args_f32x3;

double sum_f32_i64_native(args_f32_i64 p) {
  return (double) p.a + (double) p.b;
}

value sum_f32_i64_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + (double) Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

double sum_f32_triple_native(args_f32x3 p) {
  return (double) p.a + (double) p.b + (double) p.c;
}

value sum_f32_triple_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + (double) Float32_val(Field(p, 1)) + (double) Float32_val(Field(p, 2));
  CAMLreturn(caml_copy_double(res));
}

args_f32x3 ret_f32_triple_native(value unit) {
  args_f32x3 res = { 1.5f, -0.25f, 4.0f };
  return res;
}

value ret_f32_triple_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(res, a, b, c);
  a = caml_copy_float32(1.5f);
  b = caml_copy_float32(-0.25f);
  c = caml_copy_float32(4.0f);
  res = caml_alloc_small(3, 0);
  Field(res, 0) = a;
  Field(res, 1) = b;
  Field(res, 2) = c;
  CAMLreturn(res);
}

/* Heap pointers as components. */

typedef struct { value s; int64_t n; } args_string_i64;

int64_t sum_string_i64_native(args_string_i64 p) {
  return (int64_t) caml_string_length(p.s) + p.n;
}

value sum_string_i64_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) caml_string_length(Field(p, 0))
    + Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

args_string_i64 ret_string_i64_native(value n) {
  args_string_i64 res;
  res.s = caml_copy_string("hello");
  res.n = 2 * Long_val(n);
  return res;
}

value ret_string_i64_bytecode(value n) {
  CAMLparam1(n);
  CAMLlocal3(res, s, m);
  s = caml_copy_string("hello");
  m = caml_copy_int64(2 * Long_val(n));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = s;
  Field(res, 1) = m;
  CAMLreturn(res);
}

/* Stack arguments. */

int64_t sum_three_pairs_then_int_native(args_i64_i64 p, args_i64_i64 q,
                                        args_i64_i64 r, value n) {
  return p.a + p.b + q.a + q.b + r.a + r.b + Long_val(n);
}

value sum_three_pairs_then_int_bytecode(value p, value q, value r, value n) {
  CAMLparam4(p, q, r, n);
  int64_t res = Int64_val(Field(p, 0)) + Int64_val(Field(p, 1))
    + Int64_val(Field(q, 0)) + Int64_val(Field(q, 1))
    + Int64_val(Field(r, 0)) + Int64_val(Field(r, 1)) + Long_val(n);
  CAMLreturn(caml_copy_int64(res));
}

/* A one-member struct of a float passed on the stack; on Apple arm64 this
   takes a 4-byte slot, unlike the 8-byte slot of [args_i16] above. */

typedef struct { float a; } args_f32;

double stack_single_f32_native(double a0, double a1, double a2, double a3,
                               double a4, double a5, double a6, double a7,
                               args_f32 s, double y) {
  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + 10 * (double) s.a + 100 * y;
}

value stack_single_f32_bytecode(value *argv, int argn) {
  CAMLparam0();
  double sum = 0;
  int i;
  for (i = 0; i < 8; i++) sum += Double_val(argv[i]);
  sum += 10 * (double) Float32_val(Field(argv[8], 0));
  sum += 100 * Double_val(argv[9]);
  CAMLreturn(caml_copy_double(sum));
}
