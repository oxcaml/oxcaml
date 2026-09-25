#include <assert.h>

/* Fallback symbols for compiler builtins. The builtins lower to inline code
   and are never called; these exist only so that the closures the compiler
   emits for the [external] declarations link. */
#define BUILTIN(name) void name(void) { assert(0); }

BUILTIN(caml_int64x2_low_of_int64);
BUILTIN(caml_int64x2_low_to_int64);
BUILTIN(caml_simd_vec128_interleave_low_64);
BUILTIN(caml_simd_vec128_interleave_high_64);
