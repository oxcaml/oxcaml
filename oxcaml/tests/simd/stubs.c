#include <caml/memory.h>
#include <caml/simd.h>
#include <caml/callback.h>
#include <assert.h>
#include <stdbool.h>

value vec128_run_callback(value f)
{
  CAMLparam1(f);
  CAMLreturn(caml_callback(f, Val_unit));
}

value vec128_run_callback_stack_args(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value f)
{
  CAMLparam1(f); // Others are ints
  value args[] = {i0, i1, i2, i3, i4, i5, i6, i7};
  CAMLreturn(caml_callbackN(f, 8, args));
}

#ifdef __ARM_NEON
#include <arm_neon.h>

typedef poly128_t simd_poly128_t;
typedef float32x4_t simd_float32x4_t;
typedef float32x2_t simd_float32x2_t;
typedef float64x2_t simd_float64x2_t;
typedef poly128_t simd_int128_t;
typedef int64x2_t simd_int64x2_t;
typedef int32x4_t simd_int32x4_t;

#define simd_add_int64x2 vaddq_s64
#define simd_extract_int64x2 vgetq_lane_s64
#define simd_extract_int32x4 vgetq_lane_s32
#define simd_dup_float32x4  vdupq_n_f32
#define simd_dup_float64x2 vdupq_n_f64
#define simd_float64x2_sqrt vsqrtq_f64
#define simd_float64x2_min vminq_f64
#define simd_float64x2_max vmaxq_f64

#define simd_float32x4_add vaddq_f32
#define simd_float32x4_sub vsubq_f32
#define simd_float32x4_mul vmulq_f32
#define simd_float32x4_div vdivq_f32
#define simd_float32x4_min vminq_f32
#define simd_float32x4_max vmaxq_f32
#define simd_float32x4_sqrt vsqrtq_f32
#define simd_float32x4_rcp vrecpeq_f32
#define simd_float32x4_rsqrt vrsqrteq_f32
#define simd_float32x4_to_int32x4 vcvtnq_s32_f32
#define simd_int32x4_to_float32x4 vcvtq_f32_s32

/* [caml_float32_to_bits] is defined in runtime/float32.c */
int32_t caml_float32_to_bits(float f);

static inline int32_t simd_extract_float32x4(simd_float32x4_t a, intnat i) {
  float32_t e = 0;
  switch (i % 4) {
  case 0: e = vgetq_lane_f32(a, 0); break;
  case 1: e = vgetq_lane_f32(a, 1); break;
  case 2: e = vgetq_lane_f32(a, 2); break;
  case 3: e = vgetq_lane_f32(a, 3); break;
  default: assert(0);
  }
  int32_t res = caml_float32_to_bits(e);
  return res;
}

int32_t test_simd_vec128_extract_ps(simd_float32x4_t a, intnat i) {
  return (simd_extract_float32x4(a, i));
}

static inline simd_int128_t vec128i_of_int64x2(simd_int64x2_t v)
{
  return vreinterpretq_p128_s64(v);
}

static inline simd_int64x2_t Int64x2_vali(value v)
{
  simd_int128_t t = Vec128_vali(v);
  return vreinterpretq_s64_p128(t);
}
static inline float64_t simd_low_float64x2(simd_float64x2_t v)
{
  return vgetq_lane_f64(v, 0);
}

static inline simd_float64x2_t simd_float64x2_round_near(simd_float64x2_t v)
{
  return vrndnq_f64(v);
}

static inline simd_float32x4_t simd_float32x4_round_near(simd_float32x4_t v)
{
  return vrndnq_f32(v);
}

int64x2_t vec128_of_int64s(int64_t low, int64_t high)
{
  return vcombine_s64(vcreate_s64(low), vcreate_s64(high));
}


/* The meaning of PACKUSDW on amd64 is not exactly the same as UQXTN
   on arm64: amd64 treats the input as signed int32, whereas arm64
   treats the input as unsigned int32, therefore negative input
   results in 0H on amd64 and FFFFH on arm64. */
int64_t int32_su16(int64_t i) {
  uint32_t x = (uint32_t)i;
  return x > UINT16_MAX ? UINT16_MAX : (x < 0 ? 0 : x);
}

int64_t int16_su8(int64_t i) {
  uint16_t x = (uint16_t)i;
  return x > UINT8_MAX ? UINT8_MAX : (x < 0 ? 0 : x);
}

#else /* __ARM_NEON */
#if defined(__SSE4_2__)
#include <smmintrin.h>

typedef __m128 simd_poly128_t;
typedef __m128 simd_float32x4_t;
typedef __m128d simd_float64x2_t;
typedef __m128i simd_int128_t;
typedef __m128i simd_int64x2_t;
typedef __m128i simd_int32x4_t;

#define simd_add_int64x2 _mm_add_epi64
#define simd_extract_float32x4 _mm_extract_ps
#define simd_extract_int64x2 _mm_extract_epi64
#define simd_extract_int32x4 _mm_extract_epi32
#define simd_dup_float32x4 _mm_set1_ps
#define simd_dup_float64x2 _mm_set1_pd
#define simd_low_float64x2 _mm_cvtsd_f64
#define simd_float64x2_sqrt _mm_sqrt_pd
#define simd_float64x2_min _mm_min_pd
#define simd_float64x2_max _mm_max_pd

#define simd_float32x4_add _mm_add_ps
#define simd_float32x4_sub _mm_sub_ps
#define simd_float32x4_mul _mm_mul_ps
#define simd_float32x4_div _mm_div_ps
#define simd_float32x4_min _mm_min_ps
#define simd_float32x4_max _mm_max_ps
#define simd_float32x4_sqrt _mm_sqrt_ps
#define simd_float32x4_rcp _mm_rcp_ps
#define simd_float32x4_rsqrt _mm_rsqrt_ps
#define simd_float32x4_to_int32x4 _mm_cvtps_epi32

#define Int64x2_vali Vec128_vali

static inline simd_int128_t vec128i_of_int64x2(simd_int64x2_t v)
{
  return v;
}

static inline simd_float64x2_t simd_float64x2_round_near(simd_float64x2_t v)

{
  return _mm_round_pd(v, 0x8);
}

static inline simd_float32x4_t simd_float32x4_round_near(simd_float32x4_t v)
{
  return _mm_round_ps(v, 0x8);
}

simd_int64x2_t vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

int32_t test_simd_vec128_extract_ps(simd_float32x4_t a, intnat i) {
  switch (i % 4) {
    case 0: return (simd_extract_float32x4(a, 0));
    case 1: return (simd_extract_float32x4(a, 1));
    case 2: return (simd_extract_float32x4(a, 2));
    case 3: return (simd_extract_float32x4(a, 3));
    default: assert(0);
  }
}

int64_t int32_su16(int64_t i) {
  int32_t x = (int32_t)i;
  return x > UINT16_MAX ? UINT16_MAX : (x < 0 ? 0 : x);
}

int64_t int16_su8(int64_t i) {
  int16_t x = (int16_t)i;
  return x > UINT8_MAX ? UINT8_MAX : (x < 0 ? 0 : x);
}

#else /* __SSE4_2__ */
#error "Target not supported"
#endif /* __SSE4_2__ */
#endif /* __ARM_NEON */

int64_t vec128_low_int64(simd_int64x2_t v)
{
  return simd_extract_int64x2(v, 0);
}

int64_t vec128_high_int64(simd_int64x2_t v)
{
  return simd_extract_int64x2(v, 1);
}

value boxed_combine(value v0, value v1)
{
  CAMLparam2(v0, v1);

  simd_int64x2_t l = Int64x2_vali(v0);
  simd_int64x2_t r = Int64x2_vali(v1);
  simd_int64x2_t result = simd_add_int64x2(l, r);

  CAMLreturn(caml_copy_vec128i(vec128i_of_int64x2(result)));
}

simd_int64x2_t lots_of_vectors(
  simd_int64x2_t v0, simd_int64x2_t v1, simd_int64x2_t v2, simd_int64x2_t v3,
  simd_int64x2_t v4, simd_int64x2_t v5, simd_int64x2_t v6, simd_int64x2_t v7,
  simd_int64x2_t v8, simd_int64x2_t v9, simd_int64x2_t v10, simd_int64x2_t v11,
  simd_int64x2_t v12, simd_int64x2_t v13, simd_int64x2_t v14, simd_int64x2_t v15)
{
  simd_int64x2_t x0 = simd_add_int64x2(v0, v1);
  simd_int64x2_t x1 = simd_add_int64x2(v2, v3);
  simd_int64x2_t x2 = simd_add_int64x2(v4, v5);
  simd_int64x2_t x3 = simd_add_int64x2(v6, v7);
  simd_int64x2_t x4 = simd_add_int64x2(v8, v9);
  simd_int64x2_t x5 = simd_add_int64x2(v10, v11);
  simd_int64x2_t x6 = simd_add_int64x2(v12, v13);
  simd_int64x2_t x7 = simd_add_int64x2(v14, v15);
  simd_int64x2_t y0 = simd_add_int64x2(x0, x1);
  simd_int64x2_t y1 = simd_add_int64x2(x2, x3);
  simd_int64x2_t y2 = simd_add_int64x2(x4, x5);
  simd_int64x2_t y3 = simd_add_int64x2(x6, x7);
  simd_int64x2_t z0 = simd_add_int64x2(y0, y1);
  simd_int64x2_t z1 = simd_add_int64x2(y2, y3);
  return simd_add_int64x2(z0, z1);
}

simd_int64x2_t vectors_and_floats(
  simd_int64x2_t v0, double f0, simd_int64x2_t v1, double f1,
  simd_int64x2_t v2, double f2, simd_int64x2_t v3, double f3,
  double f4, simd_int64x2_t v4, simd_int64x2_t v5, double f5,
  double f6, simd_int64x2_t v6, simd_int64x2_t v7, double f7,
  double f8, double f9, simd_int64x2_t v8, simd_int64x2_t v9,
  simd_int64x2_t v10, double f10, double f11, double f12)
{
  simd_int64x2_t x0 = simd_add_int64x2(v0, v1);
  simd_int64x2_t x1 = simd_add_int64x2(v2, v3);
  simd_int64x2_t x2 = simd_add_int64x2(v4, v5);
  simd_int64x2_t x3 = simd_add_int64x2(v6, v7);
  simd_int64x2_t x4 = simd_add_int64x2(v8, v9);
  simd_int64x2_t y0 = simd_add_int64x2(x0, x1);
  simd_int64x2_t y1 = simd_add_int64x2(x2, x3);
  simd_int64x2_t y2 = simd_add_int64x2(v10, x4);
  simd_int64x2_t z0 = simd_add_int64x2(y0, y1);
  simd_int64x2_t z = simd_add_int64x2(z0, y2);
  double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
  return vec128_of_int64s((int64_t)f, vec128_low_int64(z) + vec128_high_int64(z));
}

simd_int64x2_t vectors_and_floats_and_ints(
  simd_int64x2_t v0, double f0, simd_int64x2_t v1, int64_t i0,
  simd_int64x2_t v2, double f1, simd_int64x2_t v3, int64_t i1,
  int64_t i2, simd_int64x2_t v4, simd_int64x2_t v5, double f2,
  double f3, simd_int64x2_t v6, simd_int64x2_t v7, int64_t i3,
  int64_t i4, double f4, simd_int64x2_t v8, simd_int64x2_t v9,
  simd_int64x2_t v10, int64_t i5, int64_t i6, double f5)
{
  simd_int64x2_t x0 = simd_add_int64x2(v0, v1);
  simd_int64x2_t x1 = simd_add_int64x2(v2, v3);
  simd_int64x2_t x2 = simd_add_int64x2(v4, v5);
  simd_int64x2_t x3 = simd_add_int64x2(v6, v7);
  simd_int64x2_t x4 = simd_add_int64x2(v8, v9);
  simd_int64x2_t y0 = simd_add_int64x2(x0, x1);
  simd_int64x2_t y1 = simd_add_int64x2(x2, x3);
  simd_int64x2_t y2 = simd_add_int64x2(v10, x4);
  simd_int64x2_t z0 = simd_add_int64x2(y0, y1);
  simd_int64x2_t z = simd_add_int64x2(z0, y2);
  double f = f0 + f1 + f2 + f3 + f4 + f5;
  int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
  return vec128_of_int64s((int64_t)f + i, vec128_low_int64(z) + vec128_high_int64(z));
}

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_vec128_unreachable);
BUILTIN(caml_vec128_cast);
BUILTIN(caml_float32x4_low_of_float32);
BUILTIN(caml_float32x4_low_to_float32);
BUILTIN(caml_float32x4_const1);
BUILTIN(caml_float32x4_const4);

BUILTIN(caml_float64x2_low_of_float);
BUILTIN(caml_float64x2_low_to_float);
BUILTIN(caml_float64x2_const1);
BUILTIN(caml_float64x2_const2);

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_int64x2_const1);
BUILTIN(caml_int64x2_const2);

BUILTIN(caml_int32x4_low_of_int32);
BUILTIN(caml_int32x4_low_to_int32);
BUILTIN(caml_int32x4_const1);
BUILTIN(caml_int32x4_const4);

BUILTIN(caml_int16x8_low_of_int);
BUILTIN(caml_int16x8_low_to_int);
BUILTIN(caml_int16x8_const1);
BUILTIN(caml_int16x8_const8);

BUILTIN(caml_int8x16_low_of_int);
BUILTIN(caml_int8x16_low_to_int);
BUILTIN(caml_int8x16_const1);
BUILTIN(caml_int8x16_const16);

BUILTIN(caml_simd_float32x4_add);

BUILTIN(caml_int64_ctz_nonzero_unboxed_to_untagged);
BUILTIN(caml_int64_ctz_unboxed_to_untagged);
BUILTIN(caml_int64_clz_nonzero_unboxed_to_untagged);
BUILTIN(caml_int64_clz_unboxed_to_untagged);
BUILTIN(caml_int64_popcnt_unboxed_to_untagged);
BUILTIN(caml_int32_popcnt_unboxed_to_untagged);
BUILTIN(caml_int32_ctz_nonzero_unboxed_to_untagged)
BUILTIN(caml_int32_ctz_unboxed_to_untagged)
BUILTIN(caml_int32_clz_nonzero_unboxed_to_untagged)
BUILTIN(caml_int32_clz_unboxed_to_untagged)
BUILTIN(caml_int_ctz_untagged_to_untagged);
BUILTIN(caml_int_popcnt_untagged_to_untagged);
BUILTIN(caml_int_popcnt_tagged_to_untagged);
BUILTIN(caml_int_clz_untagged_to_untagged);
BUILTIN(caml_int_clz_tagged_to_untagged);

BUILTIN(caml_simd_vec128_interleave_low_32)
BUILTIN(caml_simd_vec128_interleave_low_64)
BUILTIN(caml_simd_float64_max);
BUILTIN(caml_simd_float64_min);


#include <float.h>
#include <math.h>

// Int32

int32_t uint32_max(int32_t l, int32_t r) {
  uint32_t ul = (uint32_t)l;
  uint32_t ur = (uint32_t)r;
  return ul > ur ? l : r;
}
int32_t uint32_min(int32_t l, int32_t r) {
  uint32_t ul = (uint32_t)l;
  uint32_t ur = (uint32_t)r;
  return ul < ur ? l : r;
}
int64_t int32_si16(int64_t i) {
  int32_t x = (int32_t)i;
  return x > INT16_MAX ? INT16_MAX : (x < INT16_MIN ? INT16_MIN : x);
}
int32_t int32_mul_low(int32_t l, int32_t r) {
  return (int32_t)(((int64_t)l * (int64_t)r) & 0xffffffff);
}

// Int16

int64_t int16_max(int64_t l, int64_t r) {
  int16_t ul = (int16_t)l;
  int16_t ur = (int16_t)r;
  return ul > ur ? l : r;
}
int64_t int16_min(int64_t l, int64_t r) {
  int16_t ul = (int16_t)l;
  int16_t ur = (int16_t)r;
  return ul < ur ? l : r;
}
int64_t int16_maxu(int64_t l, int64_t r) {
  uint16_t ul = (uint16_t)l;
  uint16_t ur = (uint16_t)r;
  return ul > ur ? l : r;
}
int64_t int16_minu(int64_t l, int64_t r) {
  uint16_t ul = (uint16_t)l;
  uint16_t ur = (uint16_t)r;
  return ul < ur ? l : r;
}
int64_t int16_add(int64_t l, int64_t r) {
  return (int16_t)l + (int16_t)r;
}
int64_t int16_sub(int64_t l, int64_t r) {
  return (int16_t)l - (int16_t)r;
}
int64_t int16_abs(int64_t i) {
  int16_t x = i;
  return x < 0 ? -x : x;
}
int64_t int16_adds(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > INT16_MAX) return INT16_MAX;
  if(sum < INT16_MIN) return INT16_MIN;
  return sum;
}
int64_t int16_subs(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  int32_t diff = x - y;
  if(diff > INT16_MAX) return INT16_MAX;
  if(diff < INT16_MIN) return INT16_MIN;
  return diff;
}
int64_t int16_addsu(int64_t l, int64_t r) {
  uint16_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > UINT16_MAX) return UINT16_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int16_subsu(int64_t l, int64_t r) {
  uint16_t x = l, y = r;
  int32_t sum = (int32_t)x - (int32_t)y;
  if(sum > UINT16_MAX) return UINT16_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int16_mulsign(int64_t l, int64_t r) {
  int16_t x = l, y = r;
  return y == 0 ? 0 : y > 0 ? x : -x;
}
int64_t int16_cmpeq(int64_t l, int64_t r) {
  if((int16_t)l == (int16_t)r) return 0xffff;
  return 0;
}
int64_t int16_cmpgt(int64_t l, int64_t r) {
  if((int16_t)l > (int16_t)r) return 0xffff;
  return 0;
}
int32_t int16_sxi32(int64_t x) {
  return (int32_t)(int16_t)x;
}
int32_t int16_zxi32(int64_t x) {
  return (uint32_t)(uint16_t)x;
}
int64_t int16_sxi64(int64_t x) {
  return (int64_t)(int16_t)x;
}
int64_t int16_zxi64(int64_t x) {
  return (uint64_t)(uint16_t)x;
}
int64_t int16_logand(int64_t l, int64_t r) {
  return (int16_t)l & (int16_t)r;
}
int64_t int16_shift_left(int64_t x, int64_t shift) {
  return (int16_t)x << shift;
}
int64_t int16_shift_right(int64_t x, int64_t shift) {
  return (int16_t)x >> shift;
}
int64_t int16_shift_right_logical(int64_t x, int64_t shift) {
  return (uint16_t)(int16_t)x >> shift;
}
int64_t int16_avgu(int64_t l, int64_t r) {
  uint16_t x = (uint16_t)(int16_t)l;
  uint16_t y = (uint16_t)(int16_t)r;
  return (x + y + 1) >> 1;
}
int64_t int16_si8(int64_t i) {
  int16_t x = (int16_t)i;
  return x > INT8_MAX ? INT8_MAX : (x < INT8_MIN ? INT8_MIN : x);
}
int32_t int16_mul_i32(int64_t l_, int64_t r_) {
  int32_t l = (int32_t)(int16_t)l_;
  int32_t r = (int32_t)(int16_t)r_;
  return l * r;
}
int64_t int16_mul_low(int64_t l_, int64_t r_) {
  int64_t l = (int64_t)(int16_t)l_;
  int64_t r = (int64_t)(int16_t)r_;
  return (l * r) & 0xffff;
}
int64_t int16_mul_high(int64_t l_, int64_t r_) {
  int64_t l = (int64_t)(int16_t)l_;
  int64_t r = (int64_t)(int16_t)r_;
  return ((l * r) >> 16) & 0xffff;
}
int64_t int16_mul_high_unsigned(int64_t l_, int64_t r_) {
  uint64_t l = (uint64_t)(uint16_t)(int16_t)l_;
  uint64_t r = (uint64_t)(uint16_t)(int16_t)r_;
  return ((l * r) >> 16) & 0xffff;
}

// Int8

int64_t int8_max(int64_t l, int64_t r) {
  int8_t ul = (int8_t)l;
  int8_t ur = (int8_t)r;
  return ul > ur ? l : r;
}
int64_t int8_min(int64_t l, int64_t r) {
  int8_t ul = (int8_t)l;
  int8_t ur = (int8_t)r;
  return ul < ur ? l : r;
}
int64_t int8_maxu(int64_t l, int64_t r) {
  uint8_t ul = (uint8_t)l;
  uint8_t ur = (uint8_t)r;
  return ul > ur ? l : r;
}
int64_t int8_minu(int64_t l, int64_t r) {
  uint8_t ul = (uint8_t)l;
  uint8_t ur = (uint8_t)r;
  return ul < ur ? l : r;
}
int64_t int8_add(int64_t l, int64_t r) {
  return (int8_t)l + (int8_t)r;
}
int64_t int8_sub(int64_t l, int64_t r) {
  return (int8_t)l - (int8_t)r;
}
int64_t int8_abs(int64_t i) {
  int8_t x = i;
  return x < 0 ? -x : x;
}
int64_t int8_adds(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > INT8_MAX) return INT8_MAX;
  if(sum < INT8_MIN) return INT8_MIN;
  return sum;
}
int64_t int8_subs(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  int32_t diff = x - y;
  if(diff > INT8_MAX) return INT8_MAX;
  if(diff < INT8_MIN) return INT8_MIN;
  return diff;
}
int64_t int8_addsu(int64_t l, int64_t r) {
  uint8_t x = l, y = r;
  int32_t sum = (int32_t)x + (int32_t)y;
  if(sum > UINT8_MAX) return UINT8_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int8_subsu(int64_t l, int64_t r) {
  uint8_t x = l, y = r;
  int32_t sum = (int32_t)x - (int32_t)y;
  if(sum > UINT8_MAX) return UINT8_MAX;
  if(sum < 0) return 0;
  return sum;
}
int64_t int8_mulsign(int64_t l, int64_t r) {
  int8_t x = l, y = r;
  return y == 0 ? 0 : y > 0 ? x : -x;
}
int64_t int8_cmpeq(int64_t l, int64_t r) {
  if((int8_t)l == (int8_t)r) return 0xff;
  return 0;
}
int64_t int8_cmpgt(int64_t l, int64_t r) {
  if((int8_t)l > (int8_t)r) return 0xff;
  return 0;
}
int64_t int8_sxi16(int64_t x) {
  return (int64_t)(int16_t)(int8_t)x;
}
int64_t int8_zxi16(int64_t x) {
  return (uint64_t)(uint16_t)(uint8_t)x;
}
int32_t int8_sxi32(int64_t x) {
  return (int32_t)(int8_t)x;
}
int32_t int8_zxi32(int64_t x) {
  return (uint32_t)(uint8_t)x;
}
int64_t int8_sxi64(int64_t x) {
  return (int64_t)(int8_t)x;
}
int64_t int8_zxi64(int64_t x) {
  return (uint64_t)(uint8_t)x;
}
int64_t int8_avgu(int64_t l, int64_t r) {
  uint8_t x = (uint8_t)(int8_t)l;
  uint8_t y = (uint8_t)(int8_t)r;
  return (x + y + 1) >> 1;
}
int64_t int8_diffu(int64_t l, int64_t r) {
  uint8_t x = (uint8_t)(int8_t)l;
  uint8_t y = (uint8_t)(int8_t)r;
  return x > y ? x - y : y - x;
}
int64_t int8_mulu_i16(int64_t l, int64_t r) {
  int16_t x = (int16_t)(uint16_t)(uint8_t)(int8_t)l;
  int16_t y = (int16_t)(int8_t)r;
  return (int64_t)(x * y);
}

// Float64

double float64_round(double f) {
  simd_float64x2_t v = simd_dup_float64x2(f);
  return simd_low_float64x2(simd_float64x2_round_near(v));
}
double float64_sqrt(double f) {
  simd_float64x2_t v = simd_dup_float64x2(f);
  return simd_low_float64x2(simd_float64x2_sqrt(v));
}

static inline bool iszero_f64(double a) {
  return ((a == 0.0) || (a == -0.0));
}

static inline bool iszero_f32(float a) {
  return ((a == 0.0f) || (a == -0.0f));
}

static inline bool isnan_f32(float a) {
  return (a != a);
}

double float64_min_match_sse(double l, double r) {
  if (iszero_f64(l) && iszero_f64(r)) return r;
  if (isnan(l) || isnan(r)) return r;
  if (l < r) return l;
  return r;
}

double float64_max_match_sse(double l, double r) {
  if (iszero_f64(l) && iszero_f64(r)) return r;
  if (isnan(l) || isnan(r)) return r;
  if (l > r) return l;
  return r;
}

float float32_min_match_sse(float l, float r) {
  if (iszero_f32(l) && iszero_f32(r)) return r;
  if (isnan_f32(l) || isnan_f32(r)) return r;
  if (l < r) return l;
  return r;
}

float float32_max_match_sse(float l, float r) {
  if (iszero_f32(l) && iszero_f32(r)) return r;
  if (isnan_f32(l) || isnan_f32(r)) return r;
  if (l > r) return l;
  return r;
}

double float64_min(double l, double r) {
  simd_float64x2_t lv = simd_dup_float64x2(l);
  simd_float64x2_t rv = simd_dup_float64x2(r);
  return simd_low_float64x2(simd_float64x2_min(lv, rv));
}
double float64_max(double l, double r) {
  simd_float64x2_t lv = simd_dup_float64x2(l);
  simd_float64x2_t rv = simd_dup_float64x2(r);
  return simd_low_float64x2(simd_float64x2_max(lv, rv));
}

// Float32

int32_t int32_of_float(float f) {
  return *(int32_t*)&f;
}
float float_of_int32(int32_t i) {
  return *(float*)&i;
}

int32_t float32_zero(value unit) { return int32_of_float(0.0f); }
int32_t float32_neg_zero(value unit) { return int32_of_float(-0.0f); }
int32_t float32_one(value unit) { return int32_of_float(1.0f); }
int32_t float32_neg_one(value unit) { return int32_of_float(-1.0f); }
int32_t float32_nan(value unit) { return int32_of_float(NAN); }
int32_t float32_neg_infinity(value unit) { return int32_of_float(-INFINITY); }
int32_t float32_infinity(value unit) { return int32_of_float(INFINITY); }
int32_t float32_maxv(value unit) { return int32_of_float(FLT_MAX); }
int32_t float32_minv(value unit) { return int32_of_float(FLT_MIN); }
value float32_eq(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) == float_of_int32(r)); }
value float32_lt(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) < float_of_int32(r)); }
value float32_le(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) <= float_of_int32(r)); }
value float32_ne(int32_t l, int32_t r) { return Val_bool(float_of_int32(l) != float_of_int32(r)); }
value float32_nle(int32_t l, int32_t r) { return Val_bool(!(float_of_int32(l) <= float_of_int32(r))); }
value float32_nlt(int32_t l, int32_t r) { return Val_bool(!(float_of_int32(l) < float_of_int32(r))); }
value float32_ord(int32_t l, int32_t r) { return Val_bool(!(isnan(float_of_int32(l)) || isnan(float_of_int32(r)))); }
value float32_uord(int32_t l, int32_t r) { return Val_bool(isnan(float_of_int32(l)) || isnan(float_of_int32(r))); }

#define FLOAT32_BINOP(name, intrin)              \
  int32_t float32_##name(int32_t l, int32_t r) { \
    simd_float32x4_t vl = simd_dup_float32x4(float_of_int32(l));  \
    simd_float32x4_t vr = simd_dup_float32x4(float_of_int32(r));  \
    return simd_extract_float32x4(intrin(vl, vr), 0);    \
  }

#define FLOAT32_UNOP(name, intrin)             \
  int32_t float32_##name(int32_t f) {          \
    simd_float32x4_t v = simd_dup_float32x4(float_of_int32(f)); \
    return simd_extract_float32x4(intrin(v), 0);       \
  }

FLOAT32_BINOP(add, simd_float32x4_add);
FLOAT32_BINOP(sub, simd_float32x4_sub);
FLOAT32_BINOP(mul, simd_float32x4_mul);
FLOAT32_BINOP(div, simd_float32x4_div);
FLOAT32_BINOP(min, simd_float32x4_min);
FLOAT32_BINOP(max, simd_float32x4_max);

FLOAT32_UNOP(sqrt, simd_float32x4_sqrt);
FLOAT32_UNOP(rcp, simd_float32x4_rcp);
FLOAT32_UNOP(rsqrt, simd_float32x4_rsqrt);

int32_t float32_cvt_i32(int32_t i) {
  float f = float_of_int32(i);
  simd_float32x4_t v = simd_dup_float32x4(f);
  simd_int32x4_t res = simd_float32x4_to_int32x4(v);
  return simd_extract_int32x4(res, 0);
}

int32_t float32_round(int32_t f) {
  simd_float32x4_t v = simd_dup_float32x4(float_of_int32(f));
  return simd_extract_float32x4(simd_float32x4_round_near(v), 0);
}


value caml_test_abort(value unused) {
  abort ();
}
