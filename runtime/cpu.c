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

#include <cpuid.h>

#include "caml/cpu.h"

#define PTWRITE_LEAF 0x14
#define PTWRITE_SUBLEAF 0x0
#define PTWRITE_BIT (1 << 4)

CAMLexport bool caml_cpu_ptwrite __attribute__((visibility("hidden")));

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
}
