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

#include <string.h>

#include "caml/cpu.h"

#define PTWRITE_BIT (1 << 4)

CAMLexport bool caml_cpu_ptwrite __attribute__((visibility("hidden")));

static void caml_cpuid(int info[4], int eax, int ecx) {
#ifdef __x86_64__
#ifdef _MSC_VER
  __cpuidex((int *)info, eax, ecx);
#else
  asm volatile("cpuid"
               : "=a"(info[0]), "=b"(info[1]), "=c"(info[2]), "=d"(info[3])
               : "a"(eax), "c"(ecx));
#endif
#else
  memset(info, 0, 4 * sizeof(int));
#endif
}

CAMLexport void caml_detect_cpu_features(void) {

  int info[4];
  caml_cpuid(info, 0x14, 0);

  caml_cpu_ptwrite = (info[1] & PTWRITE_BIT) != 0;
}
