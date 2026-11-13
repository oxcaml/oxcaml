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

#ifndef CAML_CPU_H
#define CAML_CPU_H

#include "misc.h"

#ifdef CAML_INTERNALS
CAMLextern void caml_detect_cpu_features(void);

#ifdef __aarch64__
/* Whether the CPU supports FEAT_LSE. Set by [caml_detect_cpu_features];
   false before that has run. */
CAMLextern bool caml_cpu_lse;

Caml_inline bool caml_cpu_has_lse(void)
{
#ifdef HAS_LSE
  return true;
#else
  return caml_cpu_lse;
#endif
}
#endif /* __aarch64__ */
#endif /* CAML_INTERNALS */

#endif /* CAML_CPU_H */
