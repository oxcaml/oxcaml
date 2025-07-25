
#include <assert.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_int64x2_const1);
BUILTIN(caml_int64x4_const1);
BUILTIN(caml_simd_int64x2_add);
BUILTIN(caml_simd_int64x2_sub);
BUILTIN(caml_simd_vec128_interleave_high_64);
BUILTIN(caml_simd_vec128_interleave_low_64);
BUILTIN(caml_avx2_int64x4_sub);
BUILTIN(caml_avx2_int64x4_add);
BUILTIN(caml_avx_vec256_extract_128);
BUILTIN(caml_avx_vec256_insert_128);