/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                      Max Slater, Jane Street                           */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#ifdef __x86_64__
#include <cpuid.h>
#endif

#if defined(__aarch64__) && defined(__linux__)
#include <sys/auxv.h>
#ifndef HWCAP_ATOMICS
#define HWCAP_ATOMICS (1 << 8)
#endif
#elif defined(__aarch64__) && defined(__APPLE__)
#include <sys/sysctl.h>
#endif

#include "caml/cpu.h"

#define PTWRITE_LEAF 0x14
#define PTWRITE_SUBLEAF 0x0
#define PTWRITE_BIT (1 << 4)

CAMLexport bool caml_cpu_ptwrite __attribute__((visibility("hidden")));

#ifdef __aarch64__
CAMLexport bool caml_cpu_lse;

static bool caml_arm64_detect_lse(void) {
#if defined(HAS_LSE)
  return true;
#elif defined(__linux__)
  return (getauxval(AT_HWCAP) & HWCAP_ATOMICS) != 0;
#elif defined(__APPLE__)
  int value = 0;
  size_t size = sizeof(value);
  return sysctlbyname("hw.optional.arm.FEAT_LSE", &value, &size, NULL, 0) == 0
         && value != 0;
#else
  return false;
#endif
}
#endif

static void caml_cpuid(unsigned int leaf, unsigned int subleaf,
                       unsigned int *eax, unsigned int *ebx, unsigned int *ecx,
                       unsigned int *edx) {
#ifdef __x86_64__
  if (!__get_cpuid_count(leaf, subleaf, eax, ebx, ecx, edx)) {
    *eax = 0;
    *ebx = 0;
    *ecx = 0;
    *edx = 0;
  }
#else
  *eax = 0;
  *ebx = 0;
  *ecx = 0;
  *edx = 0;
#endif
}

CAMLexport void caml_detect_cpu_features(void) {

  unsigned int eax, ebx, ecx, edx;

  caml_cpuid(PTWRITE_LEAF, PTWRITE_SUBLEAF, &eax, &ebx, &ecx, &edx);
  caml_cpu_ptwrite = (ebx & PTWRITE_BIT) != 0;

#ifdef __aarch64__
  caml_cpu_lse = caml_arm64_detect_lse();
#endif
}
