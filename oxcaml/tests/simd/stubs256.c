
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/simd.h>
#include <caml/callback.h>
#include <assert.h>

#define BUILTIN(name) \
    void name() { assert(0); }

BUILTIN(caml_vec256_unreachable);
BUILTIN(caml_vec256_cast);
BUILTIN(caml_vec256_low_of_vec128);
BUILTIN(caml_vec256_low_to_vec128);
BUILTIN(caml_float32x8_low_of_float32);
BUILTIN(caml_float32x8_low_to_float32);
BUILTIN(caml_float64x4_low_of_float);
BUILTIN(caml_float64x4_low_to_float);
BUILTIN(caml_int32x8_low_of_int32);
BUILTIN(caml_int32x8_low_to_int32);
BUILTIN(caml_int64x4_low_of_int64);
BUILTIN(caml_int64x4_low_to_int64);
BUILTIN(caml_int16x16_low_of_int16);
BUILTIN(caml_int16x16_low_to_int16);
BUILTIN(caml_int8x32_low_of_int8);
BUILTIN(caml_int8x32_low_to_int8);

// 256-bit vector constants
BUILTIN(caml_float32x8_const1);
BUILTIN(caml_float32x8_const8);
BUILTIN(caml_float64x4_const1);
BUILTIN(caml_float64x4_const4);
BUILTIN(caml_int64x4_const1);
BUILTIN(caml_int64x4_const4);
BUILTIN(caml_int32x8_const1);
BUILTIN(caml_int32x8_const8);
BUILTIN(caml_int16x16_const1);
BUILTIN(caml_int16x16_const16);
BUILTIN(caml_int8x32_const1);
BUILTIN(caml_int8x32_const32);

#ifdef ARCH_AVX
#include <immintrin.h>

int64_t vec256_first_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 0);
}

int64_t vec256_second_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 1);
}

int64_t vec256_third_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 2);
}

int64_t vec256_fourth_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 3);
}

__m256i vec256_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3)
{
    return _mm256_set_epi64x(w3, w2, w1, w0);
}

__m256i vec256_of_vec128s(__m128i low, __m128i high)
{
    return _mm256_set_m128i(high, low);
}

value boxed_combine256(value v0, value v1)
{
    CAMLparam2(v0, v1);

    __m256i l = Vec256_vali(v0);
    __m256i r = Vec256_vali(v1);
    __m256i result = _mm256_add_epi64(l, r);

    CAMLreturn(caml_copy_vec256i(result));
}

__m256i lots_of_vectors256(
    __m256i v0, __m256i v1, __m256i v2, __m256i v3,
    __m256i v4, __m256i v5, __m256i v6, __m256i v7,
    __m256i v8, __m256i v9, __m256i v10, __m256i v11,
    __m256i v12, __m256i v13, __m256i v14, __m256i v15)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i x5 = _mm256_add_epi64(v10, v11);
    __m256i x6 = _mm256_add_epi64(v12, v13);
    __m256i x7 = _mm256_add_epi64(v14, v15);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(x4, x5);
    __m256i y3 = _mm256_add_epi64(x6, x7);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z1 = _mm256_add_epi64(y2, y3);
    return _mm256_add_epi64(z0, z1);
}

__m256i vectors_and_floats256(
    __m256i v0, double f0, __m256i v1, double f1,
    __m256i v2, double f2, __m256i v3, double f3,
    double f4, __m256i v4, __m256i v5, double f5,
    double f6, __m256i v6, __m256i v7, double f7,
    double f8, double f9, __m256i v8, __m256i v9,
    __m256i v10, double f10, double f11, double f12)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(v10, x4);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z = _mm256_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
    return vec256_of_int64s((int64_t)f, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

__m256i vectors_and_floats_and_ints256(
    __m256i v0, double f0, __m256i v1, int64_t i0,
    __m256i v2, double f1, __m256i v3, int64_t i1,
    int64_t i2, __m256i v4, __m256i v5, double f2,
    double f3, __m256i v6, __m256i v7, int64_t i3,
    int64_t i4, double f4, __m256i v8, __m256i v9,
    __m256i v10, int64_t i5, int64_t i6, double f5)
{
    __m256i x0 = _mm256_add_epi64(v0, v1);
    __m256i x1 = _mm256_add_epi64(v2, v3);
    __m256i x2 = _mm256_add_epi64(v4, v5);
    __m256i x3 = _mm256_add_epi64(v6, v7);
    __m256i x4 = _mm256_add_epi64(v8, v9);
    __m256i y0 = _mm256_add_epi64(x0, x1);
    __m256i y1 = _mm256_add_epi64(x2, x3);
    __m256i y2 = _mm256_add_epi64(v10, x4);
    __m256i z0 = _mm256_add_epi64(y0, y1);
    __m256i z = _mm256_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5;
    int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
    return vec256_of_int64s((int64_t)f + i, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

double vector_and_then_stack_floats(
    __attribute__((unused)) __m256i v0,
    double f0, double f1, double f2, double f3,
    double f4, double f5, double f6, double f7)
{
    return f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7;
}

#elif defined(__ARM_NEON)
#include <arm_neon.h>

typedef struct
{
    int64x2_t low;
    int64x2_t high;
} caml_int64x4_t;

caml_int64x4_t unbox_int64x4(value v)
{
    return *(caml_int64x4_t *)v;
}

value box_int64x4(caml_int64x4_t v)
{
    value res = caml_alloc_mixed_shr_check_gc(4, 0, 0);
    *(caml_int64x4_t *)res = v;
    return res;
}

caml_int64x4_t add_int64x4(caml_int64x4_t l, caml_int64x4_t r)
{
    return (caml_int64x4_t){vaddq_s64(l.low, r.low), vaddq_s64(l.high, r.high)};
}

int64_t vec256_first_int64(caml_int64x4_t v)
{
    return vgetq_lane_s64(v.low, 0);
}

int64_t vec256_second_int64(caml_int64x4_t v)
{
    return vgetq_lane_s64(v.low, 1);
}

int64_t vec256_third_int64(caml_int64x4_t v)
{
    return vgetq_lane_s64(v.high, 0);
}

int64_t vec256_fourth_int64(caml_int64x4_t v)
{
    return vgetq_lane_s64(v.high, 1);
}

caml_int64x4_t vec256_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3)
{
    return (caml_int64x4_t){vcombine_s64(vcreate_s64(w0), vcreate_s64(w1)),
                            vcombine_s64(vcreate_s64(w2), vcreate_s64(w3))};
}

caml_int64x4_t vec256_of_vec128s(caml_int64x4_t v)
{
    return v;
}

value boxed_combine256(value v0, value v1)
{
    CAMLparam2(v0, v1);

    caml_int64x4_t l = unbox_int64x4(v0);
    caml_int64x4_t r = unbox_int64x4(v1);
    caml_int64x4_t result = add_int64x4(l, r);

    CAMLreturn(box_int64x4(result));
}

caml_int64x4_t lots_of_vectors256(
    caml_int64x4_t v0, caml_int64x4_t v1, caml_int64x4_t v2, caml_int64x4_t v3,
    caml_int64x4_t v4, caml_int64x4_t v5, caml_int64x4_t v6, caml_int64x4_t v7,
    caml_int64x4_t v8, caml_int64x4_t v9, caml_int64x4_t v10, caml_int64x4_t v11,
    caml_int64x4_t v12, caml_int64x4_t v13, caml_int64x4_t v14, caml_int64x4_t v15)
{
    caml_int64x4_t x0 = add_int64x4(v0, v1);
    caml_int64x4_t x1 = add_int64x4(v2, v3);
    caml_int64x4_t x2 = add_int64x4(v4, v5);
    caml_int64x4_t x3 = add_int64x4(v6, v7);
    caml_int64x4_t x4 = add_int64x4(v8, v9);
    caml_int64x4_t x5 = add_int64x4(v10, v11);
    caml_int64x4_t x6 = add_int64x4(v12, v13);
    caml_int64x4_t x7 = add_int64x4(v14, v15);
    caml_int64x4_t y0 = add_int64x4(x0, x1);
    caml_int64x4_t y1 = add_int64x4(x2, x3);
    caml_int64x4_t y2 = add_int64x4(x4, x5);
    caml_int64x4_t y3 = add_int64x4(x6, x7);
    caml_int64x4_t z0 = add_int64x4(y0, y1);
    caml_int64x4_t z1 = add_int64x4(y2, y3);
    return add_int64x4(z0, z1);
}

caml_int64x4_t vectors_and_floats256(
    caml_int64x4_t v0, double f0, caml_int64x4_t v1, double f1,
    caml_int64x4_t v2, double f2, caml_int64x4_t v3, double f3,
    double f4, caml_int64x4_t v4, caml_int64x4_t v5, double f5,
    double f6, caml_int64x4_t v6, caml_int64x4_t v7, double f7,
    double f8, double f9, caml_int64x4_t v8, caml_int64x4_t v9,
    caml_int64x4_t v10, double f10, double f11, double f12)
{
    caml_int64x4_t x0 = add_int64x4(v0, v1);
    caml_int64x4_t x1 = add_int64x4(v2, v3);
    caml_int64x4_t x2 = add_int64x4(v4, v5);
    caml_int64x4_t x3 = add_int64x4(v6, v7);
    caml_int64x4_t x4 = add_int64x4(v8, v9);
    caml_int64x4_t y0 = add_int64x4(x0, x1);
    caml_int64x4_t y1 = add_int64x4(x2, x3);
    caml_int64x4_t y2 = add_int64x4(v10, x4);
    caml_int64x4_t z0 = add_int64x4(y0, y1);
    caml_int64x4_t z = add_int64x4(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
    return vec256_of_int64s((int64_t)f, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

caml_int64x4_t vectors_and_floats_and_ints256(
    caml_int64x4_t v0, double f0, caml_int64x4_t v1, int64_t i0,
    caml_int64x4_t v2, double f1, caml_int64x4_t v3, int64_t i1,
    int64_t i2, caml_int64x4_t v4, caml_int64x4_t v5, double f2,
    double f3, caml_int64x4_t v6, caml_int64x4_t v7, int64_t i3,
    int64_t i4, double f4, caml_int64x4_t v8, caml_int64x4_t v9,
    caml_int64x4_t v10, int64_t i5, int64_t i6, double f5)
{
    caml_int64x4_t x0 = add_int64x4(v0, v1);
    caml_int64x4_t x1 = add_int64x4(v2, v3);
    caml_int64x4_t x2 = add_int64x4(v4, v5);
    caml_int64x4_t x3 = add_int64x4(v6, v7);
    caml_int64x4_t x4 = add_int64x4(v8, v9);
    caml_int64x4_t y0 = add_int64x4(x0, x1);
    caml_int64x4_t y1 = add_int64x4(x2, x3);
    caml_int64x4_t y2 = add_int64x4(v10, x4);
    caml_int64x4_t z0 = add_int64x4(y0, y1);
    caml_int64x4_t z = add_int64x4(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5;
    int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
    return vec256_of_int64s((int64_t)f + i, vec256_first_int64(z) + vec256_second_int64(z),
                            vec256_third_int64(z), vec256_fourth_int64(z));
}

double vector_and_then_stack_floats(
    __attribute__((unused)) caml_int64x4_t v0,
    double f0, double f1, double f2, double f3,
    double f4, double f5, double f6, double f7)
{
    return f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7;
}

#endif
