#include <assert.h>
#include <stdint.h>
#include "caml/mlvalues.h"

/* Never called: [@@builtin] externals compile to instructions in native
   code; the symbol is only needed to satisfy the linker. */
CAMLprim void caml_int64x4_const1(void) { assert(0); }

/* Received by value on the stack, where the caller placed the vec256.  The
   alignment term in the checksum checks that the caller aligned the stack
   slot to 32 bytes as the ABI requires for __m256: a __m256-typed parameter
   would instead fault in vmovapd, but only when this file is compiled with
   AVX support, and declaring the struct 32-aligned would provoke gcc's
   -Wpsabi note about the GCC 4.6 ABI change, which ocamltest treats as a
   compilation failure. */
struct v256 { int64_t w[4]; };

intnat test_fat_simd_native(unsigned char *p, unsigned char *q,
                            int64_t a, int64_t b, int64_t c, int64_t d,
                            double f0, double f1, double f2, double f3,
                            double f4, double f5, double f6, double f7,
                            struct v256 v)
{
  return (intnat)*p + (intnat)*q + (intnat)(a + b + c + d)
         + (intnat)(f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7)
         + (intnat)(v.w[0] + v.w[1] + v.w[2] + v.w[3])
         + (intnat)((uintptr_t)&v & 31);
}
