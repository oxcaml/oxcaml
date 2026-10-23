/* Stubs for unboxed_args_memory.ml. Sums weight components by powers of two,
   so that swaps are detected. */

#include <stdint.h>
#include "caml/mlvalues.h"

typedef struct { int64_t a; int64_t b; int64_t c; } args_i64x3;
typedef struct { double a; double b; double c; double d; double e; } args_f64x5;
typedef struct { int32_t a; float b; int64_t c; float d; } args_mixed;
typedef struct { int16_t a; int64_t b; int8_t c; } args_padded;

int64_t i64x3_native(args_i64x3 p) {
  return p.a + 2 * p.b + 4 * p.c;
}

double f64x5_native(args_f64x5 p) {
  return p.a + 2 * p.b + 4 * p.c + 8 * p.d + 16 * p.e;
}

double i64x3_i64_f64_native(args_i64x3 p, int64_t y, double z) {
  return (double) (p.a + 2 * p.b + 4 * p.c + 8 * y) + 16 * z;
}

double mixed_native(args_mixed p) {
  return (double) p.a + 2 * (double) p.b + 4 * (double) p.c
    + 8 * (double) p.d;
}

int64_t padded_native(args_padded p) {
  return (int64_t) p.a + 2 * p.b + 4 * (int64_t) p.c;
}

int64_t i64x7_i64x3_i64x3_i64_native(int64_t a0, int64_t a1, int64_t a2,
                                     int64_t a3, int64_t a4, int64_t a5,
                                     int64_t a6, args_i64x3 p, args_i64x3 q,
                                     int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * p.a + 256 * p.b + 512 * p.c + 1024 * q.a + 2048 * q.b
    + 4096 * q.c + 8192 * y;
}

typedef struct { int64_t a; int64_t b; } args_i64_i64;
typedef struct { args_i64_i64 p; int64_t c; } args_nested_i64x3;
typedef struct { int64_t a; int8_t b; } args_i64_i8;
typedef struct { args_i64_i8 p; int8_t c; } args_nested_tail_padded;

int64_t nested_i64x3_native(args_nested_i64x3 p) {
  return p.p.a + 2 * p.p.b + 4 * p.c;
}

int64_t nested_tail_padded_native(args_nested_tail_padded p) {
  return p.p.a + 2 * (int64_t) p.p.b + 4 * (int64_t) p.c;
}

typedef struct { float a; float b; float c; float d; float e; } args_f32x5;

_Static_assert(sizeof(args_f32x5) == 20, "five floats should be 20 bytes");

double f32x5_native(args_f32x5 p) {
  return (double) p.a + 2 * (double) p.b + 4 * (double) p.c
    + 8 * (double) p.d + 16 * (double) p.e;
}

typedef struct { int64_t a; int64_t b; int64_t c; int64_t d; } args_i64x4;
typedef struct { double a; double b; double c; double d; } args_f64x4;
typedef struct { int32_t a; float b; int16_t c; int8_t d; } args_i32_f32_i16_i8;
typedef struct {
  args_i64x4 i;
  args_f64x4 f;
  args_i32_f32_i16_i8 s;
  int64_t z;
} args_big;

_Static_assert(sizeof(args_big) == 88, "big should be 88 bytes");

static double big_sum(args_big p) {
  return p.i.a + 2 * p.i.b + 4 * p.i.c + 8 * p.i.d + 16 * p.f.a + 32 * p.f.b
    + 64 * p.f.c + 128 * p.f.d + 256 * (double) p.s.a + 512 * (double) p.s.b
    + 1024 * (double) p.s.c + 2048 * (double) p.s.d + 4096 * (double) p.z;
}

double big_pair_native(int64_t x, args_big p, double y, args_big q) {
  return x + 2 * y + 4 * big_sum(p) + 32768 * big_sum(q);
}
