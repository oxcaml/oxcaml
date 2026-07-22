#include <caml/simd.h>
#include <assert.h>
#include <stdint.h>

#define BUILTIN(name) \
    void name() { assert(0); }

/* Fallback closure symbols for the [@@builtin] externals: these lower to inline
   code and are never called, but the closures the compiler emits must link. */
BUILTIN(caml_vec512_unreachable);
BUILTIN(caml_mask_of_int64);
BUILTIN(caml_int64_of_mask);
BUILTIN(caml_mm512_add_ps);
BUILTIN(caml_mm512_add_epi32);
BUILTIN(caml_mm512_sub_epi32);
BUILTIN(caml_mm512_abs_epi32);
BUILTIN(caml_mm512_abs_epi64);
BUILTIN(caml_mm512_max_epi64);
BUILTIN(caml_mm512_mask_add_ps);
BUILTIN(caml_mm512_maskz_add_ps);
BUILTIN(caml_mm512_cmp_ps_mask);
BUILTIN(caml_mm512_mask_cmp_ps_mask);
BUILTIN(caml_mm512_fmadd_ps);
BUILTIN(caml_mm512_mask_fmadd_ps);
BUILTIN(caml_mm512_mask3_fmadd_ps);
BUILTIN(caml_mm512_maskz_fmadd_ps);
BUILTIN(caml_mm512_mask_blend_ps);
BUILTIN(caml_mm512_permutex2var_epi32);
BUILTIN(caml_mm512_mask2_permutex2var_epi32);
BUILTIN(caml_mm512_slli_epi32);
BUILTIN(caml_mm512_shuffle_epi32);
BUILTIN(caml_mm512_ternarylogic_epi32);
BUILTIN(caml_mm512_cmplt_epi32_mask);
BUILTIN(caml_mm512_add_round_ps);
BUILTIN(caml_kortestz_mask16_u8);
BUILTIN(caml_kortestc_mask16_u8);
BUILTIN(caml_ktestz_mask16_u8);
BUILTIN(caml_mm512_movm_epi8);
BUILTIN(caml_mm512_getmant_ps);
BUILTIN(caml_mm512_roundscale_ps);

#ifdef ARCH_AVX512
#include <immintrin.h>

static int64_t extract(__m512i v, int i)
{
    int64_t tmp[8];
    _mm512_storeu_si512((void *)tmp, v);
    return tmp[i];
}
int64_t vec512_w0(__m512i v) { return extract(v, 0); }
int64_t vec512_w1(__m512i v) { return extract(v, 1); }
int64_t vec512_w2(__m512i v) { return extract(v, 2); }
int64_t vec512_w3(__m512i v) { return extract(v, 3); }
int64_t vec512_w4(__m512i v) { return extract(v, 4); }
int64_t vec512_w5(__m512i v) { return extract(v, 5); }
int64_t vec512_w6(__m512i v) { return extract(v, 6); }
int64_t vec512_w7(__m512i v) { return extract(v, 7); }
__m512i vec512_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3,
                         int64_t w4, int64_t w5, int64_t w6, int64_t w7)
{
    return _mm512_set_epi64(w7, w6, w5, w4, w3, w2, w1, w0);
}

__m512 ctest_mm512_add_ps(__m512 a, __m512 b) { return _mm512_add_ps(a, b); }
__m512i ctest_mm512_add_epi32(__m512i a, __m512i b) { return _mm512_add_epi32(a, b); }
__m512i ctest_mm512_sub_epi32(__m512i a, __m512i b) { return _mm512_sub_epi32(a, b); }
__m512i ctest_mm512_abs_epi32(__m512i a) { return _mm512_abs_epi32(a); }
__m512i ctest_mm512_abs_epi64(__m512i a) { return _mm512_abs_epi64(a); }
__m512i ctest_mm512_max_epi64(__m512i a, __m512i b) { return _mm512_max_epi64(a, b); }
__m512 ctest_mm512_mask_add_ps(__m512 s, __mmask16 k, __m512 a, __m512 b)
{ return _mm512_mask_add_ps(s, k, a, b); }
__m512 ctest_mm512_maskz_add_ps(__mmask16 k, __m512 a, __m512 b)
{ return _mm512_maskz_add_ps(k, a, b); }
__mmask16 ctest_mm512_cmp_ps_mask(int imm, __m512 a, __m512 b)
{ return _mm512_cmp_ps_mask(a, b, 1); }
__mmask16 ctest_mm512_mask_cmp_ps_mask(int imm, __mmask16 k, __m512 a, __m512 b)
{ return _mm512_mask_cmp_ps_mask(k, a, b, 1); }
__m512 ctest_mm512_fmadd_ps(__m512 a, __m512 b, __m512 c)
{ return _mm512_fmadd_ps(a, b, c); }
__m512 ctest_mm512_mask_fmadd_ps(__m512 a, __mmask16 k, __m512 b, __m512 c)
{ return _mm512_mask_fmadd_ps(a, k, b, c); }
__m512 ctest_mm512_mask3_fmadd_ps(__m512 a, __m512 b, __m512 c, __mmask16 k)
{ return _mm512_mask3_fmadd_ps(a, b, c, k); }
__m512 ctest_mm512_maskz_fmadd_ps(__mmask16 k, __m512 a, __m512 b, __m512 c)
{ return _mm512_maskz_fmadd_ps(k, a, b, c); }
__m512 ctest_mm512_mask_blend_ps(__mmask16 k, __m512 a, __m512 b)
{ return _mm512_mask_blend_ps(k, a, b); }
__m512i ctest_mm512_permutex2var_epi32(__m512i a, __m512i idx, __m512i b)
{ return _mm512_permutex2var_epi32(a, idx, b); }
__m512i ctest_mm512_mask2_permutex2var_epi32(__m512i a, __m512i idx, __mmask16 k, __m512i b)
{ return _mm512_mask2_permutex2var_epi32(a, idx, k, b); }
__m512i ctest_mm512_slli_epi32(int imm, __m512i a) { return _mm512_slli_epi32(a, 3); }
__m512i ctest_mm512_shuffle_epi32(int imm, __m512i a)
{ return _mm512_shuffle_epi32(a, 0x1b); }
__m512i ctest_mm512_ternarylogic_epi32(int imm, __m512i a, __m512i b, __m512i c)
{ return _mm512_ternarylogic_epi32(a, b, c, 0xca); }

__mmask16 ctest_mm512_cmplt_epi32_mask(__m512i a, __m512i b)
{ return _mm512_cmplt_epi32_mask(a, b); }
__m512 ctest_mm512_add_round_ps(int imm, __m512 a, __m512 b)
{ return _mm512_add_round_ps(a, b, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); }

intnat ctest_kortestz_mask16_u8(__mmask16 a, __mmask16 b)
{ return _kortestz_mask16_u8(a, b); }
intnat ctest_kortestc_mask16_u8(__mmask16 a, __mmask16 b)
{ return _kortestc_mask16_u8(a, b); }
intnat ctest_ktestz_mask16_u8(__mmask16 a, __mmask16 b)
{ return _ktestz_mask16_u8(a, b); }

__m512i ctest_mm512_movm_epi8(__mmask64 k) { return _mm512_movm_epi8(k); }
__m512 ctest_mm512_getmant_ps(int interval, int sign, __m512 a)
{ return _mm512_getmant_ps(a, _MM_MANT_NORM_1_2, _MM_MANT_SIGN_src); }
__m512 ctest_mm512_roundscale_ps(int imm, __m512 a)
{ return _mm512_roundscale_ps(a, 0x13); }

#endif
