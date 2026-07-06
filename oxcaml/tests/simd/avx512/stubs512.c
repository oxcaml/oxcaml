
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/simd.h>
#include <caml/callback.h>
#include <assert.h>
#include <stdint.h>

#define BUILTIN(name) \
    void name() { assert(0); }

BUILTIN(caml_vec512_unreachable);
BUILTIN(caml_mask_of_int64);
BUILTIN(caml_int64_of_mask);

// Builtin fallback symbols for the 512-bit cast and constant intrinsics. Like
// the mask builtins above, these lower to inline code and are never called;
// they only exist so the closures the compiler emits for the externals link.
BUILTIN(caml_vec512_cast);
BUILTIN(caml_vec512_low_of_vec128);
BUILTIN(caml_vec512_low_to_vec128);
BUILTIN(caml_vec512_low_of_vec256);
BUILTIN(caml_vec512_low_to_vec256);
BUILTIN(caml_int64x8_low_of_int64);
BUILTIN(caml_int64x8_low_to_int64);
BUILTIN(caml_int64x8_const1);
BUILTIN(caml_int64x8_const8);
BUILTIN(caml_float64x8_low_of_float);
BUILTIN(caml_float64x8_low_to_float);
BUILTIN(caml_float64x8_const1);
BUILTIN(caml_float64x8_const8);
BUILTIN(caml_int32x16_low_of_int32);
BUILTIN(caml_int32x16_low_to_int32);
BUILTIN(caml_int32x16_const1);
BUILTIN(caml_int16x32_low_of_int16);
BUILTIN(caml_int16x32_low_to_int16);
BUILTIN(caml_int16x32_const1);
BUILTIN(caml_int8x64_low_to_int8);
BUILTIN(caml_int8x64_const1);
BUILTIN(caml_float32x16_low_to_float32);
BUILTIN(caml_float32x16_const1);

// Call an OCaml callback from C. Compiled with AVX512 enabled, so the C side
// may use the vector/mask registers, exercising their preservation across the
// OCaml<->C boundary.
value vec512_run_callback(value f)
{
    CAMLparam1(f);
    CAMLreturn(caml_callback(f, Val_unit));
}

value vec512_run_callback_stack_args(value i0, value i1, value i2, value i3,
                                     value i4, value i5, value i6, value i7,
                                     value f)
{
    CAMLparam1(f); // Others are ints
    value args[] = {i0, i1, i2, i3, i4, i5, i6, i7};
    CAMLreturn(caml_callbackN(f, 8, args));
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

int64_t mask_and(__mmask8 l, __mmask8 r)
{
    __m512i v = _mm512_maskz_mov_epi64(l, _mm512_set1_epi64(-1));
    return (int64_t)_mm512_mask_test_epi64_mask(r, v, v);
}

// A mask argument beyond the six integer argument registers, passed on the
// stack per the C ABI.
int64_t mask_stack_arg(int64_t i0, int64_t i1, int64_t i2, int64_t i3,
                       int64_t i4, int64_t i5, __mmask64 m)
{
    return i0 + i1 + i2 + i3 + i4 + i5 + (int64_t)m;
}

// Masks are returned as integers per the C ABI.
__mmask64 mask_ret(int64_t x)
{
    return (__mmask64)x;
}

// Overwrites every mask register, checking that the compiler treats them as
// destroyed at C calls.
int64_t clobber_masks(int64_t x)
{
    __asm__ volatile(
        "kxnorq %%k0, %%k0, %%k0\n\t"
        "kxnorq %%k1, %%k1, %%k1\n\t"
        "kxnorq %%k2, %%k2, %%k2\n\t"
        "kxnorq %%k3, %%k3, %%k3\n\t"
        "kxnorq %%k4, %%k4, %%k4\n\t"
        "kxnorq %%k5, %%k5, %%k5\n\t"
        "kxnorq %%k6, %%k6, %%k6\n\t"
        "kxnorq %%k7, %%k7, %%k7"
        ::: "k0", "k1", "k2", "k3", "k4", "k5", "k6", "k7");
    return x;
}

// Mixed vector/float arguments, exercising the C calling convention.
__m512i vectors_and_floats512(
    __m512i v0, double f0, __m512i v1, double f1,
    __m512i v2, double f2, __m512i v3, double f3,
    double f4, __m512i v4, __m512i v5, double f5,
    double f6, __m512i v6, __m512i v7, double f7,
    double f8, double f9, __m512i v8, __m512i v9,
    __m512i v10, double f10, double f11, double f12)
{
    __m512i x0 = _mm512_add_epi64(v0, v1);
    __m512i x1 = _mm512_add_epi64(v2, v3);
    __m512i x2 = _mm512_add_epi64(v4, v5);
    __m512i x3 = _mm512_add_epi64(v6, v7);
    __m512i x4 = _mm512_add_epi64(v8, v9);
    __m512i y0 = _mm512_add_epi64(x0, x1);
    __m512i y1 = _mm512_add_epi64(x2, x3);
    __m512i y2 = _mm512_add_epi64(v10, x4);
    __m512i z0 = _mm512_add_epi64(y0, y1);
    __m512i z = _mm512_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10 + f11 + f12;
    return vec512_of_int64s((int64_t)f, extract(z, 0) + extract(z, 1),
                            extract(z, 2), extract(z, 3), extract(z, 4),
                            extract(z, 5), extract(z, 6), extract(z, 7));
}

// Mixed vector/float/int arguments.
__m512i vectors_and_floats_and_ints512(
    __m512i v0, double f0, __m512i v1, int64_t i0,
    __m512i v2, double f1, __m512i v3, int64_t i1,
    int64_t i2, __m512i v4, __m512i v5, double f2,
    double f3, __m512i v6, __m512i v7, int64_t i3,
    int64_t i4, double f4, __m512i v8, __m512i v9,
    __m512i v10, int64_t i5, int64_t i6, double f5)
{
    __m512i x0 = _mm512_add_epi64(v0, v1);
    __m512i x1 = _mm512_add_epi64(v2, v3);
    __m512i x2 = _mm512_add_epi64(v4, v5);
    __m512i x3 = _mm512_add_epi64(v6, v7);
    __m512i x4 = _mm512_add_epi64(v8, v9);
    __m512i y0 = _mm512_add_epi64(x0, x1);
    __m512i y1 = _mm512_add_epi64(x2, x3);
    __m512i y2 = _mm512_add_epi64(v10, x4);
    __m512i z0 = _mm512_add_epi64(y0, y1);
    __m512i z = _mm512_add_epi64(z0, y2);
    double f = f0 + f1 + f2 + f3 + f4 + f5;
    int64_t i = i0 + i1 + i2 + i3 + i4 + i5 + i6;
    return vec512_of_int64s((int64_t)f + i, extract(z, 0) + extract(z, 1),
                            extract(z, 2), extract(z, 3), extract(z, 4),
                            extract(z, 5), extract(z, 6), extract(z, 7));
}

// A vector register argument followed by stack-passed floats.
double vector_and_then_stack_floats512(
    __attribute__((unused)) __m512i v0,
    double f0, double f1, double f2, double f3,
    double f4, double f5, double f6, double f7)
{
    return f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7;
}

#endif /* ARCH_AVX512 */
