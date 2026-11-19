/* Detects whether the host CPU and OS support the AVX512 features the tests
   in this directory (and intrins/) use. Built without AVX512 codegen flags so
   that it can run on any amd64 host. */

#include <caml/mlvalues.h>

#ifdef __x86_64__

#include <cpuid.h>

/* CPUID.(EAX=7,ECX=0):EBX */
#define AVX512F_BIT (1u << 16)
#define AVX512DQ_BIT (1u << 17)
#define AVX512CD_BIT (1u << 28)
#define AVX512BW_BIT (1u << 30)
#define AVX512VL_BIT (1u << 31)

/* CPUID.1:ECX */
#define OSXSAVE_BIT (1u << 27)

/* XCR0 state the OS must save: SSE, AVX, opmask, ZMM_Hi256, and Hi16_ZMM. */
#define XCR0_AVX512 0xe6u

static int os_saves_avx512_state(void) {
  unsigned int eax, ebx, ecx, edx, xcr0_lo, xcr0_hi;
  if (!__get_cpuid(1, &eax, &ebx, &ecx, &edx) || !(ecx & OSXSAVE_BIT))
    return 0;
  __asm__ volatile("xgetbv" : "=a"(xcr0_lo), "=d"(xcr0_hi) : "c"(0));
  (void)xcr0_hi;
  return (xcr0_lo & XCR0_AVX512) == XCR0_AVX512;
}

static int avx512_available(void) {
  unsigned int eax, ebx, ecx, edx;
  unsigned int features = AVX512F_BIT | AVX512DQ_BIT | AVX512CD_BIT |
                          AVX512BW_BIT | AVX512VL_BIT;
  return __get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx) &&
         (ebx & features) == features && os_saves_avx512_state();
}

#else

static int avx512_available(void) { return 0; }

#endif

value test_avx512_available(value unit) {
  (void)unit;
  return Val_bool(avx512_available());
}
