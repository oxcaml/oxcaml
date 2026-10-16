/* Helpers for the generated AVX512 intrinsics tests (the OCaml side is in the
   preamble printed by tools/simdgen/simdgen_intrins.ml). */

#include <caml/simd.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define BUILTIN(name) void name() { assert(0); }

/* Builtins, only called if not selected. */
BUILTIN(caml_vec512_unreachable)
BUILTIN(caml_mask_of_int64)
BUILTIN(caml_int64_of_mask)

/* test_buf(i) points 128 bytes into 512-byte buffer i. Buffer 0 is read by
   loads and gathers; stores and scatters write buffers 1 and 2. */
static unsigned char *bufs[3];

static void fill(int i)
{
  for (int j = 0; j < 512; j++)
    bufs[i][j] = (unsigned char)(j * 37 + (i == 0 ? 11 : 90));
}

intnat test_buf(intnat i)
{
  if (!bufs[i]) {
    bufs[i] = aligned_alloc(64, 512);
    fill(i);
  }
  return (intnat)(bufs[i] + 128);
}

intnat test_buf_reset(intnat unused)
{
  (void)unused;
  test_buf(1);
  test_buf(2);
  fill(1);
  fill(2);
  return 0;
}

intnat test_buf_eq(intnat unused)
{
  (void)unused;
  return memcmp(bufs[1], bufs[2], 512) == 0;
}

/* Without AVX512 (when compiled by a C compiler lacking the newer intrinsic
   names, in which case the tests do not run), these are aborting stubs so that
   the test executables still link. */
#ifdef ARCH_AVX512
#include <immintrin.h>

int64_t vec512_wi(__m512i v, int i)
{
  int64_t t[8];
  _mm512_storeu_si512((void *)t, v);
  return t[i];
}

int64_t vec256_wi(__m256i v, int i)
{
  int64_t t[4];
  _mm256_storeu_si256((void *)t, v);
  return t[i];
}

int64_t vec128_wi(__m128i v, int i)
{
  int64_t t[2];
  _mm_storeu_si128((void *)t, v);
  return t[i];
}

__m512i vec512_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d,
                         int64_t e, int64_t f, int64_t g, int64_t h)
{
  return _mm512_set_epi64(h, g, f, e, d, c, b, a);
}

__m256i vec256_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d)
{
  return _mm256_set_epi64x(d, c, b, a);
}

__m128i vec128_of_int64s(int64_t a, int64_t b)
{
  return _mm_set_epi64x(b, a);
}

#else

BUILTIN(vec512_wi)
BUILTIN(vec256_wi)
BUILTIN(vec128_wi)
BUILTIN(vec512_of_int64s)
BUILTIN(vec256_of_int64s)
BUILTIN(vec128_of_int64s)

#endif
