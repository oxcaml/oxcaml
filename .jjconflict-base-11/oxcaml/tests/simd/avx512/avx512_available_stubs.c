/* Detects whether the host CPU supports the AVX512 features the tests in
   this directory use. Built without AVX512 codegen flags so that it can run
   on any amd64 host. */

#include <caml/mlvalues.h>

#ifdef __x86_64__

#include <cpuid.h>

#define AVX512F_BIT (1u << 16)
#define AVX512DQ_BIT (1u << 17)
#define AVX512BW_BIT (1u << 30)
#define AVX512VL_BIT (1u << 31)

static int avx512_available(void) {
  unsigned int eax, ebx, ecx, edx;
  unsigned int features =
      AVX512F_BIT | AVX512DQ_BIT | AVX512BW_BIT | AVX512VL_BIT;
  return __get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx) &&
         (ebx & features) == features;
}

#else

static int avx512_available(void) { return 0; }

#endif

value test_avx512_available(value unit) {
  (void)unit;
  return Val_bool(avx512_available());
}
