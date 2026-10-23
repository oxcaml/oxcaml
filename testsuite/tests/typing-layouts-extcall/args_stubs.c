/* Stubs for unboxed_args.ml. The native versions take C structs by value;
   the bytecode versions use (possibly nested) tuples.

   Sums weight successive components by powers of two, so that swapped
   components or arguments are detected. */

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
typedef struct { int32_t a; float b; } args_i32_f32;
typedef struct { float a; int32_t b; } args_f32_i32_ret;

int64_t sum_i64_pair_native(args_i64_i64 p) {
  return p.a + 2 * p.b;
}

value sum_i64_pair_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_int64(Int64_val(Field(p, 0))
                             + 2 * Int64_val(Field(p, 1))));
}

int64_t sum_i32_pair_native(args_i32_i32 p) {
  return (int64_t) p.a + 2 * (int64_t) p.b;
}

value sum_i32_pair_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int32_val(Field(p, 0))
    + 2 * (int64_t) Int32_val(Field(p, 1));
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
  return p.a + 2 * p.b;
}

value sum_f64_pair_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_double(Double_val(Field(p, 0))
                              + 2 * Double_val(Field(p, 1))));
}

double sum_f32_quad_native(args_f32x4 p) {
  return (double) p.a + 2 * (double) p.b + 4 * (double) p.c
    + 8 * (double) p.d;
}

value sum_f32_quad_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + 2 * (double) Float32_val(Field(p, 1))
    + 4 * (double) Float32_val(Field(p, 2))
    + 8 * (double) Float32_val(Field(p, 3));
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
  return (int64_t) p.p.a + 2 * (int64_t) p.p.b + 4 * p.c;
}

value sum_nested_bytecode(value p) {
  CAMLparam1(p);
  value inner = Field(p, 0);
  int64_t res = (int64_t) Int32_val(Field(inner, 0))
    + 2 * (int64_t) Int32_val(Field(inner, 1)) + 4 * Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

/* See [sum_nested_padded] in unboxed_args.ml. */
_Static_assert(sizeof(args_nested_padded) == 12,
               "nested struct with trailing padding should be 12 bytes");
_Static_assert(sizeof(args_flat_i32_i16_i16) == 8,
               "flattened struct should be 8 bytes");

int64_t sum_nested_padded_native(args_nested_padded p) {
  return (int64_t) p.p.a + 2 * (int64_t) p.p.b + 4 * (int64_t) p.c;
}

value sum_nested_padded_bytecode(value p) {
  CAMLparam1(p);
  value inner = Field(p, 0);
  int64_t res = (int64_t) Int32_val(Field(inner, 0))
    + 2 * (int64_t) Int16_val(Field(inner, 1))
    + 4 * (int64_t) Int16_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_two_pairs_native(args_i64_i64 p, args_i64_i64 q) {
  return p.a + 2 * p.b + 4 * q.a + 8 * q.b;
}

value sum_two_pairs_bytecode(value p, value q) {
  CAMLparam2(p, q);
  int64_t res = Int64_val(Field(p, 0)) + 2 * Int64_val(Field(p, 1))
    + 4 * Int64_val(Field(q, 0)) + 8 * Int64_val(Field(q, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_three_pairs_native(args_i64_i64 p, args_i64_i64 q,
                               args_i64_i64 r) {
  return p.a + 2 * p.b + 4 * q.a + 8 * q.b + 16 * r.a + 32 * r.b;
}

value sum_three_pairs_bytecode(value p, value q, value r) {
  CAMLparam3(p, q, r);
  int64_t res = Int64_val(Field(p, 0)) + 2 * Int64_val(Field(p, 1))
    + 4 * Int64_val(Field(q, 0)) + 8 * Int64_val(Field(q, 1))
    + 16 * Int64_val(Field(r, 0)) + 32 * Int64_val(Field(r, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t mixed_args_native(int64_t x, args_f64_f64 fp, value n,
                          args_i32_i32 ip, double d) {
  return x + 2 * Long_val(n) + 4 * (int64_t) ip.a + 8 * (int64_t) ip.b
    + (int64_t) (fp.a + 2 * fp.b + 4 * d);
}

value mixed_args_bytecode(value x, value fp, value n, value ip, value d) {
  CAMLparam5(x, fp, n, ip, d);
  int64_t res = Int64_val(x) + 2 * Long_val(n)
    + 4 * (int64_t) Int32_val(Field(ip, 0))
    + 8 * (int64_t) Int32_val(Field(ip, 1))
    + (int64_t) (Double_val(Field(fp, 0)) + 2 * Double_val(Field(fp, 1))
                 + 4 * Double_val(d));
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

/* See [stack_single] in unboxed_args.ml. */

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
  return (int64_t) p.a + 2 * (int64_t) p.b + 4 * (int64_t) p.c
    + 8 * (int64_t) p.d;
}

value sum_i16_quad_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int16_val(Field(p, 0))
    + 2 * (int64_t) Int16_val(Field(p, 1))
    + 4 * (int64_t) Int16_val(Field(p, 2))
    + 8 * (int64_t) Int16_val(Field(p, 3));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_i8_i8_i16_i32_native(args_i8_i8_i16_i32 p) {
  return (int64_t) p.a + 2 * (int64_t) p.b + 4 * (int64_t) p.c
    + 8 * (int64_t) p.d;
}

value sum_i8_i8_i16_i32_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int8_val(Field(p, 0))
    + 2 * (int64_t) Int8_val(Field(p, 1))
    + 4 * (int64_t) Int16_val(Field(p, 2))
    + 8 * (int64_t) Int32_val(Field(p, 3));
  CAMLreturn(caml_copy_int64(res));
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
  return (double) p.a + 2 * (double) p.b + 4 * (double) p.c;
}

value sum_f32_triple_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + 2 * (double) Float32_val(Field(p, 1))
    + 4 * (double) Float32_val(Field(p, 2));
  CAMLreturn(caml_copy_double(res));
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

/* Stack arguments. */

int64_t sum_three_pairs_then_int_native(args_i64_i64 p, args_i64_i64 q,
                                        args_i64_i64 r, value n) {
  return p.a + 2 * p.b + 4 * q.a + 8 * q.b + 16 * r.a + 32 * r.b
    + 64 * Long_val(n);
}

value sum_three_pairs_then_int_bytecode(value p, value q, value r, value n) {
  CAMLparam4(p, q, r, n);
  int64_t res = Int64_val(Field(p, 0)) + 2 * Int64_val(Field(p, 1))
    + 4 * Int64_val(Field(q, 0)) + 8 * Int64_val(Field(q, 1))
    + 16 * Int64_val(Field(r, 0)) + 32 * Int64_val(Field(r, 1))
    + 64 * Long_val(n);
  CAMLreturn(caml_copy_int64(res));
}

/* Seven integer arguments exhaust the six x86-64 integer registers (the
   seventh goes on the stack) but leave the SSE registers for the pair. */

double seven_ints_f64_pair_native(value i0, value i1, value i2, value i3,
                                  value i4, value i5, value i6,
                                  args_f64_f64 p) {
  return (double) (Long_val(i0) + 2 * Long_val(i1) + 4 * Long_val(i2)
                   + 8 * Long_val(i3) + 16 * Long_val(i4) + 32 * Long_val(i5)
                   + 64 * Long_val(i6))
    + p.a + 2 * p.b;
}

value seven_ints_f64_pair_bytecode(value *argv, int argn) {
  CAMLparam0();
  double sum = 0;
  int i;
  for (i = 0; i < 7; i++) sum += (double) ((1 << i) * Long_val(argv[i]));
  sum += Double_val(Field(argv[7], 0)) + 2 * Double_val(Field(argv[7], 1));
  CAMLreturn(caml_copy_double(sum));
}

/* A one-member struct of a value on the stack, followed by a scalar. */

typedef struct { value a; } args_val;

int64_t stack_single_value_native(int64_t a0, int64_t a1, int64_t a2,
                                  int64_t a3, int64_t a4, int64_t a5,
                                  int64_t a6, int64_t a7, args_val s,
                                  value y) {
  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + 10 * Long_val(s.a)
    + 100 * Long_val(y);
}

value stack_single_value_bytecode(value *argv, int argn) {
  CAMLparam0();
  int64_t sum = 0;
  int i;
  for (i = 0; i < 8; i++) sum += Int64_val(argv[i]);
  sum += 10 * Long_val(Field(argv[8], 0));
  sum += 100 * Long_val(argv[9]);
  CAMLreturn(caml_copy_int64(sum));
}

/* A one-member struct of a float passed on the stack, followed by a bare
   float. On Apple arm64 the struct takes the 4-byte slot of its member (unlike
   the 8-byte slot of [args_i16] above), so [y] is at offset 4; an 8-byte slot
   would put [y] at offset 8. */

typedef struct { float a; } args_f32;

double stack_single_f32_native(double a0, double a1, double a2, double a3,
                               double a4, double a5, double a6, double a7,
                               args_f32 s, float y) {
  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + 10 * (double) s.a
    + 100 * (double) y;
}

value stack_single_f32_bytecode(value *argv, int argn) {
  CAMLparam0();
  double sum = 0;
  int i;
  for (i = 0; i < 8; i++) sum += Double_val(argv[i]);
  sum += 10 * (double) Float32_val(Field(argv[8], 0));
  sum += 100 * (double) Float32_val(argv[9]);
  CAMLreturn(caml_copy_double(sum));
}

/* A product all of whose components are void corresponds to no C
   arguments at all. */

int64_t all_void_arg_native(void) {
  return 4242;
}

value all_void_arg_bytecode(value p) {
  CAMLparam1(p);
  CAMLreturn(caml_copy_int64(4242));
}

/* A plain void argument likewise corresponds to no C arguments. */

int64_t void_arg_native(void) {
  return 2424;
}

value void_arg_bytecode(value u) {
  CAMLparam1(u);
  CAMLreturn(caml_copy_int64(2424));
}

/* Void arguments and components. Void corresponds to nothing in the native
   stubs; in bytecode it is [Val_unit], and void components remain tuple
   fields. */

int64_t void_between_args_native(int64_t x, args_i32_i32 p, int64_t y) {
  return x + 2 * (int64_t) p.a + 4 * (int64_t) p.b + 8 * y;
}

value void_between_args_bytecode(value x, value u1, value p, value u2,
                                 value y) {
  CAMLparam5(x, u1, p, u2, y);
  int64_t res = Int64_val(x) + 2 * (int64_t) Int32_val(Field(p, 0))
    + 4 * (int64_t) Int32_val(Field(p, 1)) + 8 * Int64_val(y);
  CAMLreturn(caml_copy_int64(res));
}

typedef struct { float a; float b; } args_f32_f32;

double void_inside_f32_native(args_f32_f32 p) {
  return (double) p.a + 2 * (double) p.b;
}

value void_inside_f32_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 1))
    + 2 * (double) Float32_val(Field(p, 3));
  CAMLreturn(caml_copy_double(res));
}

/* The all-void middle product has no counterpart. */
typedef struct { int32_t b; } args_i32;
typedef struct { args_i16 p; args_i32 r; } args_void_nested;

_Static_assert(sizeof(args_void_nested) == 8,
               "void_nested struct should be 8 bytes");

int64_t void_nested_native(args_void_nested p) {
  return (int64_t) p.p.a + 2 * (int64_t) p.r.b;
}

value void_nested_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int16_val(Field(Field(p, 0), 1))
    + 2 * (int64_t) Int32_val(Field(Field(p, 2), 0));
  CAMLreturn(caml_copy_int64(res));
}

int64_t void_stack_single_nested_native(int64_t a0, int64_t a1, int64_t a2,
                                        int64_t a3, int64_t a4, int64_t a5,
                                        int64_t a6, int64_t a7, args_i16 s,
                                        int32_t y) {
  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + 100 * (int64_t) s.a
    + 1000 * (int64_t) y;
}

value void_stack_single_nested_bytecode(value *argv, int argn) {
  CAMLparam0();
  int64_t sum = 0;
  int i;
  for (i = 0; i < 8; i++) sum += Int64_val(argv[i]);
  sum += 100 * (int64_t) Int16_val(Field(Field(argv[8], 1), 1));
  sum += 1000 * (int64_t) Int32_val(argv[9]);
  CAMLreturn(caml_copy_int64(sum));
}

int64_t void_regs_int_native(int64_t a, int64_t b, int64_t c, int64_t d,
                             args_i64_i64 p) {
  return a + 2 * b + 4 * c + 8 * d + 16 * p.a + 32 * p.b;
}

value void_regs_int_bytecode(value *argv, int argn) {
  CAMLparam0();
  value p = argv[6];
  int64_t res = Int64_val(argv[0]) + 2 * Int64_val(argv[1])
    + 4 * Int64_val(argv[2]) + 8 * Int64_val(argv[3])
    + 16 * Int64_val(Field(p, 0)) + 32 * Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

double void_regs_float_native(double a0, double a1, double a2, double a3,
                              double a4, double a5, args_f64_f64 p) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * p.a
    + 128 * p.b;
}

value void_regs_float_bytecode(value *argv, int argn) {
  CAMLparam0();
  double res = 0;
  double weight = 1;
  int i;
  for (i = 0; i < 6; i++, weight *= 2) res += weight * Double_val(argv[i]);
  res += 64 * Double_val(Field(argv[8], 0));
  res += 128 * Double_val(Field(argv[8], 1));
  CAMLreturn(caml_copy_double(res));
}

typedef struct { int32_t x; int64_t y; } args_i32_i64;

int64_t void_record_native(args_i32_i64 p) {
  return (int64_t) p.x + 2 * p.y;
}

value void_record_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int32_val(Field(p, 0)) + 2 * Int64_val(Field(p, 2));
  CAMLreturn(caml_copy_int64(res));
}

args_f32_i32_ret void_swap_native(args_i32_f32 p) {
  args_f32_i32_ret res = { p.b, p.a };
  return res;
}

value void_swap_bytecode(value u1, value p, value u2) {
  CAMLparam3(u1, p, u2);
  CAMLlocal3(res, a, b);
  a = caml_copy_float32(Float32_val(Field(p, 2)));
  b = caml_copy_int32(Int32_val(Field(p, 0)));
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_unit;
  Field(res, 1) = a;
  Field(res, 2) = b;
  CAMLreturn(res);
}

typedef struct { int32_t a; int32_t b; int32_t c; } args_i32x3;
typedef struct { double a; float b; float c; } args_f64_f32_f32;
typedef struct { int8_t a; double b; } args_i8_f64;
typedef struct { float a; double b; } args_f32_f64;
typedef struct { int64_t a; int8_t b; } args_i64_i8;
typedef struct { int16_t a; float b; } args_i16_f32;
typedef struct { int32_t a; int32_t b; double c; } args_i32_i32_f64;
typedef struct { value a; value b; } args_int_int;
typedef struct { intnat a; int64_t b; } args_n_i64;
typedef struct { int8_t a; int8_t b; } args_i8_i8;
typedef struct { args_i8_i8 p; int16_t c; } args_nested2;
typedef struct { args_nested2 p; int32_t d; } args_nested3;
typedef struct { int32_t a; int8_t b; } args_i32_i8;
typedef struct { int8_t a; args_i32_i8 p; } args_nested_inner_padded;

_Static_assert(sizeof(args_i32x3) == 12, "three int32s should be 12 bytes");
_Static_assert(sizeof(args_i8_f64) == 16, "int8, double should be 16 bytes");
_Static_assert(sizeof(args_f32_f64) == 16, "float, double should be 16 bytes");
_Static_assert(sizeof(args_i64_i8) == 16, "int64, int8 should be 16 bytes");
_Static_assert(sizeof(args_nested3) == 8, "nested3 should be 8 bytes");
_Static_assert(sizeof(args_nested_inner_padded) == 12,
               "int8, { int32, int8 } should be 12 bytes");

double sum_f32_pair_native(args_f32_f32 p) {
  return (double) p.a + 2 * (double) p.b;
}

value sum_f32_pair_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0))
    + 2 * (double) Float32_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

int64_t sum_i32_triple_native(args_i32x3 p) {
  return (int64_t) p.a + 2 * (int64_t) p.b + 4 * (int64_t) p.c;
}

value sum_i32_triple_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Int32_val(Field(p, 0))
    + 2 * (int64_t) Int32_val(Field(p, 1))
    + 4 * (int64_t) Int32_val(Field(p, 2));
  CAMLreturn(caml_copy_int64(res));
}

double sum_f64_f32_f32_native(args_f64_f32_f32 p) {
  return p.a + 2 * (double) p.b + 4 * (double) p.c;
}

value sum_f64_f32_f32_bytecode(value p) {
  CAMLparam1(p);
  double res = Double_val(Field(p, 0)) + 2 * (double) Float32_val(Field(p, 1))
    + 4 * (double) Float32_val(Field(p, 2));
  CAMLreturn(caml_copy_double(res));
}

double sum_i8_f64_native(args_i8_f64 p) {
  return (double) p.a + 2 * p.b;
}

value sum_i8_f64_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Int8_val(Field(p, 0)) + 2 * Double_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

double sum_f32_f64_native(args_f32_f64 p) {
  return (double) p.a + 2 * p.b;
}

value sum_f32_f64_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Float32_val(Field(p, 0)) + 2 * Double_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

int64_t sum_i64_i8_native(args_i64_i8 p) {
  return p.a + 2 * (int64_t) p.b;
}

value sum_i64_i8_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = Int64_val(Field(p, 0)) + 2 * (int64_t) Int8_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

double sum_i16_f32_native(args_i16_f32 p) {
  return (double) p.a + 2 * (double) p.b;
}

value sum_i16_f32_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Int16_val(Field(p, 0))
    + 2 * (double) Float32_val(Field(p, 1));
  CAMLreturn(caml_copy_double(res));
}

double sum_i32_i32_f64_native(args_i32_i32_f64 p) {
  return (double) p.a + 2 * (double) p.b + 4 * p.c;
}

value sum_i32_i32_f64_bytecode(value p) {
  CAMLparam1(p);
  double res = (double) Int32_val(Field(p, 0))
    + 2 * (double) Int32_val(Field(p, 1)) + 4 * Double_val(Field(p, 2));
  CAMLreturn(caml_copy_double(res));
}

value sum_int_int_native(args_int_int p) {
  return Val_long(Long_val(p.a) + 2 * Long_val(p.b));
}

value sum_int_int_bytecode(value p) {
  return Val_long(Long_val(Field(p, 0)) + 2 * Long_val(Field(p, 1)));
}

int64_t sum_n_i64_native(args_n_i64 p) {
  return (int64_t) p.a + 2 * p.b;
}

value sum_n_i64_bytecode(value p) {
  CAMLparam1(p);
  int64_t res = (int64_t) Nativeint_val(Field(p, 0))
    + 2 * Int64_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_nested3_native(args_nested3 p) {
  return (int64_t) p.p.p.a + 2 * (int64_t) p.p.p.b + 4 * (int64_t) p.p.c
    + 8 * (int64_t) p.d;
}

value sum_nested3_bytecode(value p) {
  CAMLparam1(p);
  value mid = Field(p, 0);
  value inner = Field(mid, 0);
  int64_t res = (int64_t) Int8_val(Field(inner, 0))
    + 2 * (int64_t) Int8_val(Field(inner, 1))
    + 4 * (int64_t) Int16_val(Field(mid, 1))
    + 8 * (int64_t) Int32_val(Field(p, 1));
  CAMLreturn(caml_copy_int64(res));
}

int64_t sum_nested_inner_padded_native(args_nested_inner_padded p) {
  return (int64_t) p.a + 2 * (int64_t) p.p.a + 4 * (int64_t) p.p.b;
}

value sum_nested_inner_padded_bytecode(value p) {
  CAMLparam1(p);
  value inner = Field(p, 1);
  int64_t res = (int64_t) Int8_val(Field(p, 0))
    + 2 * (int64_t) Int32_val(Field(inner, 0))
    + 4 * (int64_t) Int8_val(Field(inner, 1));
  CAMLreturn(caml_copy_int64(res));
}

double attr_scalars_native(intnat n, args_i32_i32 p, double d) {
  return (double) n + 2 * (double) p.a + 4 * (double) p.b + 8 * d;
}

value attr_scalars_bytecode(value n, value p, value d) {
  CAMLparam3(n, p, d);
  double res = (double) Long_val(n) + 2 * (double) Int32_val(Field(p, 0))
    + 4 * (double) Int32_val(Field(p, 1)) + 8 * Double_val(d);
  CAMLreturn(caml_copy_double(res));
}
