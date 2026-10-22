/* Stubs for unboxed_args_vec_amd64.ml: C structs over 16 bytes containing
   SSE vectors, which the x86-64 psABI classifies as MEMORY and passes by
   value on the stack.

   Sums weight successive components (and vector lanes) by powers of two, so
   that swapped components or arguments are detected. */

#include <stdint.h>
#include <emmintrin.h>
#include "caml/mlvalues.h"

__m128i make_i64x2_native(int64_t low, int64_t high) {
  return _mm_set_epi64x(high, low);
}

/* lane 0 + 2 * lane 1 */
static int64_t lanes(__m128i v) {
  int64_t elts[2];
  _mm_storeu_si128((__m128i *) elts, v);
  return elts[0] + 2 * elts[1];
}

typedef struct { __m128i a; __m128i b; } args_v_v;
typedef struct { int64_t a; __m128i b; } args_i64_v;
typedef struct { __m128i a; int64_t b; } args_v_i64;
typedef struct { double a; __m128i b; } args_f64_v;

/* 32 bytes, no padding. */
int64_t v_v_native(args_v_v p) {
  return lanes(p.a) + 4 * lanes(p.b);
}

/* Padding between the int64 and the vector; the struct is 16-aligned. */
int64_t i64_v_native(args_i64_v p) {
  return p.a + 2 * lanes(p.b);
}

/* Six int64s use up the integer registers, so y goes on the stack after the
   struct: at +32, after the struct's tail padding. */
int64_t i64x6_v_i64_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                               int64_t a4, int64_t a5, args_v_i64 p,
                               int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * lanes(p.a)
    + 256 * p.b + 512 * y;
}

/* The seventh int64 is on the stack at +0, so the 16-aligned struct starts at
   +16 (its first slot is only 8 bytes), and y follows at +48. */
int64_t i64x7_i64_v_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                               int64_t a4, int64_t a5, int64_t a6,
                               args_i64_v p, int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * p.a + 256 * lanes(p.b) + 1024 * y;
}

/* As above with a struct whose first slot is the vector. */
int64_t i64x7_v_v_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                             int64_t a4, int64_t a5, int64_t a6, args_v_v p,
                             int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * lanes(p.a) + 512 * lanes(p.b) + 2048 * y;
}

/* A floating-point member next to a vector; z still takes an SSE register. */
double f64_v_f64_native(args_f64_v p, double z) {
  return p.a + 2 * (double) lanes(p.b) + 8 * z;
}
