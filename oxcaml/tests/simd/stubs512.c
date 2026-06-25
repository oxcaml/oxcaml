
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/simd.h>
#include <caml/callback.h>
#include <assert.h>
#include <stdint.h>

#define BUILTIN(name) \
    void name() { assert(0); }

BUILTIN(caml_vec512_unreachable);

// Masks are passed to C as integers, so [mask_and] just takes two integers.
int64_t mask_and(int64_t l, int64_t r)
{
    return l & r;
}

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

value boxed_combine512(value v0, value v1)
{
    CAMLparam2(v0, v1);

    __m512i l = Vec512_vali(v0);
    __m512i r = Vec512_vali(v1);
    __m512i result = _mm512_add_epi64(l, r);

    CAMLreturn(caml_copy_vec512i(result));
}

__m512i lots_of_vectors512(
    __m512i v0, __m512i v1, __m512i v2, __m512i v3,
    __m512i v4, __m512i v5, __m512i v6, __m512i v7,
    __m512i v8, __m512i v9, __m512i v10, __m512i v11,
    __m512i v12, __m512i v13, __m512i v14, __m512i v15)
{
    __m512i x0 = _mm512_add_epi64(v0, v1);
    __m512i x1 = _mm512_add_epi64(v2, v3);
    __m512i x2 = _mm512_add_epi64(v4, v5);
    __m512i x3 = _mm512_add_epi64(v6, v7);
    __m512i x4 = _mm512_add_epi64(v8, v9);
    __m512i x5 = _mm512_add_epi64(v10, v11);
    __m512i x6 = _mm512_add_epi64(v12, v13);
    __m512i x7 = _mm512_add_epi64(v14, v15);
    __m512i y0 = _mm512_add_epi64(x0, x1);
    __m512i y1 = _mm512_add_epi64(x2, x3);
    __m512i y2 = _mm512_add_epi64(x4, x5);
    __m512i y3 = _mm512_add_epi64(x6, x7);
    __m512i z0 = _mm512_add_epi64(y0, y1);
    __m512i z1 = _mm512_add_epi64(y2, y3);
    return _mm512_add_epi64(z0, z1);
}

#endif /* ARCH_AVX512 */
