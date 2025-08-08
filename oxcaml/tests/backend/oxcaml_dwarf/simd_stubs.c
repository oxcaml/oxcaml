#include <smmintrin.h>
#include <emmintrin.h>
#include <immintrin.h>

// 128-bit SIMD functions
__m128i vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

__m128i vec128_of_int32s(int i0, int i1, int i2, int i3)
{
  return _mm_set_epi32(i3, i2, i1, i0);
}

__m128 vec128_of_floats(float f0, float f1, float f2, float f3)
{
  return _mm_set_ps(f3, f2, f1, f0);
}

__m128d vec128_of_doubles(double d0, double d1)
{
  return _mm_set_pd(d1, d0);
}

__m128i vec128_cast (__m128i v)
{
  return v;
}

// 256-bit SIMD functions
__m256i vec256_of_int64s(int64_t w0, int64_t w1, int64_t w2, int64_t w3)
{
  return _mm256_set_epi64x(w3, w2, w1, w0);
}

__m256i vec256_of_int32s(int32_t i0, int32_t i1, int32_t i2, int32_t i3, int32_t i4, int32_t i5, int32_t i6, int32_t i7)
{
  return _mm256_set_epi32(i7, i6, i5, i4, i3, i2, i1, i0);
}

__m256 vec256_of_floats(float f0, float f1, float f2, float f3, float f4, float f5, float f6, float f7)
{
  return _mm256_set_ps(f7, f6, f5, f4, f3, f2, f1, f0);
}

__m256d vec256_of_doubles(double d0, double d1, double d2, double d3)
{
  return _mm256_set_pd(d3, d2, d1, d0);
}

__m256i vec256_cast(__m256i v)
{
  return v;
}