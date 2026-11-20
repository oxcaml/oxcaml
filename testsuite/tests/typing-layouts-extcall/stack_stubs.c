/* Stubs for unboxed_args_stack.ml. Sums weight components by powers of two,
   so that swaps are detected. */

#include <stdint.h>
#include "caml/mlvalues.h"

typedef struct { int64_t a; int64_t b; } args_i64_i64;
typedef struct { double a; double b; } args_f64_f64;
typedef struct { float a; float b; float c; } args_f32x3;
typedef struct { int64_t a; double b; } args_i64_f64;
typedef struct { int16_t a; int32_t b; } args_i16_i32;
typedef struct { value a; int64_t b; } args_val_i64;

static int64_t sum8(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                    int64_t a4, int64_t a5, int64_t a6, int64_t a7) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * a7;
}

static double fsum8(double a0, double a1, double a2, double a3, double a4,
                    double a5, double a6, double a7) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * a7;
}

int64_t i64x8_pair_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                          int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                          args_i64_i64 p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * p.a + 512 * p.b;
}

int64_t i64x5_pair_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                              int64_t a4, args_i64_i64 p, int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * p.a + 64 * p.b
    + 128 * y;
}

int64_t i64x7_pair_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                              int64_t a4, int64_t a5, int64_t a6,
                              args_i64_i64 p, int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * p.a + 256 * p.b + 512 * y;
}

double f64x7_pair_f64_native(double a0, double a1, double a2, double a3,
                             double a4, double a5, double a6, args_f64_f64 p,
                             double y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * p.a + 256 * p.b + 512 * y;
}

double f64x8_pair_f64_native(double a0, double a1, double a2, double a3,
                             double a4, double a5, double a6, double a7,
                             args_f64_f64 p, double y) {
  return fsum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * p.a + 512 * p.b
    + 1024 * y;
}

double f64x7_f32x3_f32_native(double a0, double a1, double a2, double a3,
                              double a4, double a5, double a6, args_f32x3 p,
                              float y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * (double) p.a + 256 * (double) p.b + 512 * (double) p.c
    + 1024 * (double) y;
}

double i64x6_mixed_f64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                              int64_t a4, int64_t a5, args_i64_f64 p,
                              double y) {
  return (double) (a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5)
    + 64 * (double) p.a + 128 * p.b + 256 * y;
}

double i64x8_mixed_f64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                              int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                              args_i64_f64 p, double y) {
  return (double) sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (double) p.a
    + 512 * p.b + 1024 * y;
}

double i64x8_pairs_i64_f64_native(int64_t a0, int64_t a1, int64_t a2,
                                  int64_t a3, int64_t a4, int64_t a5,
                                  int64_t a6, int64_t a7, args_i64_i64 p,
                                  args_i16_i32 q, int64_t y, double z) {
  return (double) (sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * p.a
                   + 512 * p.b + 1024 * (int64_t) q.a
                   + 2048 * (int64_t) q.b + 4096 * y)
    + 8192 * z;
}

int64_t i64x8_i16_i32_i32_native(int64_t a0, int64_t a1, int64_t a2,
                                 int64_t a3, int64_t a4, int64_t a5,
                                 int64_t a6, int64_t a7, args_i16_i32 p,
                                 int32_t y) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (int64_t) p.a
    + 512 * (int64_t) p.b + 1024 * (int64_t) y;
}

int64_t i64x8_val_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                             int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                             args_val_i64 p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * Long_val(p.a)
    + 512 * p.b;
}

typedef struct { int32_t a; int32_t b; } args_i32_i32;
typedef struct { args_i32_i32 p; int64_t c; } args_nested;
typedef struct { int16_t a; int8_t b; } args_i16_i8;
typedef struct { args_i16_i8 p; int64_t c; } args_nested_padded;
typedef struct { args_f64_f64 p; double c; } args_nested_hfa;

int64_t i64x8_nested_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                            int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                            args_nested p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (int64_t) p.p.a
    + 512 * (int64_t) p.p.b + 1024 * p.c;
}

int64_t i64x8_nested_padded_native(int64_t a0, int64_t a1, int64_t a2,
                                   int64_t a3, int64_t a4, int64_t a5,
                                   int64_t a6, int64_t a7,
                                   args_nested_padded p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (int64_t) p.p.a
    + 512 * (int64_t) p.p.b + 1024 * p.c;
}

double f64x8_nested_hfa_native(double a0, double a1, double a2, double a3,
                               double a4, double a5, double a6, double a7,
                               args_nested_hfa p) {
  return fsum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * p.p.a + 512 * p.p.b
    + 1024 * p.c;
}

typedef struct { double a; double b; double c; double d; } args_f64x4;
typedef struct { float a; float b; float c; float d; } args_f32x4;
typedef struct { int32_t a; int32_t b; } args_i32_i32_pair;

double f64x5_quad_f64_native(double a0, double a1, double a2, double a3,
                             double a4, args_f64x4 p, double y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * p.a + 64 * p.b
    + 128 * p.c + 256 * p.d + 512 * y;
}

double f64x7_f32x4_f32_native(double a0, double a1, double a2, double a3,
                              double a4, double a5, double a6, args_f32x4 p,
                              float y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * (double) p.a + 256 * (double) p.b + 512 * (double) p.c
    + 1024 * (double) p.d + 2048 * (double) y;
}

int64_t i64x5_pair_i32pair_i64_native(int64_t a0, int64_t a1, int64_t a2,
                                      int64_t a3, int64_t a4, args_i64_i64 p,
                                      args_i32_i32_pair q, int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * p.a + 64 * p.b
    + 128 * (int64_t) q.a + 256 * (int64_t) q.b + 512 * y;
}

double i64x6_f64x8_mixed_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                                int64_t a4, int64_t a5, double f0, double f1,
                                double f2, double f3, double f4, double f5,
                                double f6, double f7, args_i64_i64 p,
                                args_f64_f64 q, int64_t y, double z) {
  return (double) (a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5)
    + 64 * fsum8(f0, f1, f2, f3, f4, f5, f6, f7)
    + 16384 * (double) p.a + 32768 * (double) p.b + 65536 * q.a
    + 131072 * q.b + 262144 * (double) y + 524288 * z;
}

typedef struct { int32_t a; int32_t b; int32_t c; } args_i32x3;
typedef struct { int8_t a; int64_t b; } args_i8_i64;
typedef struct { float a; float b; } args_f32_f32;

_Static_assert(sizeof(args_i32x3) == 12, "three int32s should be 12 bytes");
_Static_assert(sizeof(args_i8_i64) == 16, "int8, int64 should be 16 bytes");

int64_t i64x8_i32_triple_native(int64_t a0, int64_t a1, int64_t a2,
                                int64_t a3, int64_t a4, int64_t a5,
                                int64_t a6, int64_t a7, args_i32x3 p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (int64_t) p.a
    + 512 * (int64_t) p.b + 1024 * (int64_t) p.c;
}

int64_t i64x8_i8_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                            int64_t a4, int64_t a5, int64_t a6, int64_t a7,
                            args_i8_i64 p) {
  return sum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (int64_t) p.a
    + 512 * p.b;
}

double f64x8_f32_pair_native(double a0, double a1, double a2, double a3,
                             double a4, double a5, double a6, double a7,
                             args_f32_f32 p) {
  return fsum8(a0, a1, a2, a3, a4, a5, a6, a7) + 256 * (double) p.a
    + 512 * (double) p.b;
}
