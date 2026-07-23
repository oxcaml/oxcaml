/* C oracle for mem.ml, compiled with clang (gcc 8.5 lacks some names). */
#include <caml/simd.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_vec512_unreachable);
BUILTIN(caml_mask_of_int64);
BUILTIN(caml_mm512_loadu_epi32);
BUILTIN(caml_mm512_load_epi32);
BUILTIN(caml_mm512_mask_loadu_epi32);
BUILTIN(caml_mm512_maskz_loadu_epi32);
BUILTIN(caml_mm512_storeu_epi32);
BUILTIN(caml_mm512_mask_storeu_epi32);
BUILTIN(caml_mm512_mask_compressstoreu_epi32);
BUILTIN(caml_mm512_mask_expandloadu_epi32);
BUILTIN(caml_mm512_i32gather_epi32);
BUILTIN(caml_mm512_mask_i32gather_epi32);
BUILTIN(caml_mm512_i32scatter_epi32);
BUILTIN(caml_mm512_mask_i32scatter_epi32);

void *vec_aligned_alloc(intnat align, intnat size) { return aligned_alloc(align, size); }
intnat buf_eq64(void *a, void *b)
{
    const int64_t *x = a, *y = b;
    for (int i = 0; i < 8; i++)
        if (x[i] != y[i]) return 0;
    return 1;
}

#ifdef ARCH_AVX512
#include <immintrin.h>
static int64_t ex(__m512i v, int i) { int64_t t[8]; _mm512_storeu_si512((void *)t, v); return t[i]; }
int64_t vec512_wi(__m512i v, int i) { return ex(v, i); }
__m512i vec512_of_int64s(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
                         int64_t f, int64_t g, int64_t h)
{ return _mm512_set_epi64(h, g, f, e, d, c, b, a); }

__m512i ctest_loadu_epi32(void *p) { return _mm512_loadu_epi32(p); }
__m512i ctest_load_epi32(void *p) { return _mm512_load_epi32(p); }
__m512i ctest_mask_loadu_epi32(__m512i s, __mmask16 k, void *p) { return _mm512_mask_loadu_epi32(s, k, p); }
__m512i ctest_maskz_loadu_epi32(__mmask16 k, void *p) { return _mm512_maskz_loadu_epi32(k, p); }
void ctest_storeu_epi32(void *p, __m512i a) { _mm512_storeu_epi32(p, a); }
void ctest_mask_storeu_epi32(void *p, __mmask16 k, __m512i a) { _mm512_mask_storeu_epi32(p, k, a); }
void ctest_compressstoreu_epi32(void *p, __mmask16 k, __m512i a) { _mm512_mask_compressstoreu_epi32(p, k, a); }
__m512i ctest_expandloadu_epi32(__m512i s, __mmask16 k, void *p) { return _mm512_mask_expandloadu_epi32(s, k, p); }
/* The C gather/scatter intrinsics require a literal scale, so each tested
   scale gets its own oracle; the scale parameter mirrors the OCaml external
   and must agree with the baked-in literal. */
#define GATHER_ORACLE(S) \
  __m512i ctest_i32gather_epi32_s##S(int scale, __m512i vindex, void *base) \
  { assert(scale == S); return _mm512_i32gather_epi32(vindex, base, S); }
GATHER_ORACLE(1)
GATHER_ORACLE(2)
GATHER_ORACLE(4)
GATHER_ORACLE(8)
#define SCATTER_ORACLE(S) \
  void ctest_i32scatter_epi32_s##S(int scale, void *base, __m512i vindex, __m512i a) \
  { assert(scale == S); _mm512_i32scatter_epi32(base, vindex, a, S); }
SCATTER_ORACLE(1)
SCATTER_ORACLE(2)
SCATTER_ORACLE(4)
SCATTER_ORACLE(8)
__m512i ctest_mask_i32gather_epi32(int scale, __m512i src, __mmask16 k, __m512i vindex, void *base)
{ assert(scale == 4); return _mm512_mask_i32gather_epi32(src, k, vindex, base, 4); }
void ctest_mask_i32scatter_epi32(int scale, void *base, __mmask16 k, __m512i vindex, __m512i a)
{ assert(scale == 4); _mm512_mask_i32scatter_epi32(base, k, vindex, a, 4); }
#endif
