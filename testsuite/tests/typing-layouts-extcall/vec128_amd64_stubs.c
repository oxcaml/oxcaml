/* Stubs for unboxed_args_vec_amd64.ml. Sums weight components and lanes by
   powers of two, so that swaps are detected. */

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

int64_t v_v_native(args_v_v p) {
  return lanes(p.a) + 4 * lanes(p.b);
}

int64_t i64_v_native(args_i64_v p) {
  return p.a + 2 * lanes(p.b);
}

int64_t i64x6_v_i64_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                               int64_t a4, int64_t a5, args_v_i64 p,
                               int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * lanes(p.a)
    + 256 * p.b + 512 * y;
}

int64_t i64x7_i64_v_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                               int64_t a4, int64_t a5, int64_t a6,
                               args_i64_v p, int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * p.a + 256 * lanes(p.b) + 1024 * y;
}

int64_t i64x7_v_v_i64_native(int64_t a0, int64_t a1, int64_t a2, int64_t a3,
                             int64_t a4, int64_t a5, int64_t a6, args_v_v p,
                             int64_t y) {
  return a0 + 2 * a1 + 4 * a2 + 8 * a3 + 16 * a4 + 32 * a5 + 64 * a6
    + 128 * lanes(p.a) + 512 * lanes(p.b) + 2048 * y;
}

double f64_v_f64_native(args_f64_v p, double z) {
  return p.a + 2 * (double) lanes(p.b) + 8 * z;
}
