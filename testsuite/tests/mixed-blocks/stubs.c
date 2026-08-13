
#include <assert.h>
#include <stdint.h>

#define BUILTIN(name) void name(void) { assert(0); }

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_simd_vec128_interleave_high_64);
BUILTIN(caml_simd_vec128_interleave_low_64);

#ifdef __AVX__

#include <immintrin.h>

int64_t vec256_first_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 3);
}

int64_t vec256_second_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 2);
}

int64_t vec256_third_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 1);
}

int64_t vec256_fourth_int64(__m256i v)
{
    return _mm256_extract_epi64(v, 0);
}

__m256i vec256_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3)
{
    return _mm256_set_epi64x(w0, w1, w2, w3);
}

#elif defined(__aarch64__)

#include <arm_neon.h>

typedef struct
{
    int64x2_t low;
    int64x2_t high;
} caml_int64x4_t;

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

#else
#error "Target not supported"
#endif
