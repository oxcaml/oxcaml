/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
<<<<<<<< HEAD:runtime4/caml/hooks.h
/*                    Fabrice Le Fessant, INRIA de Paris                  */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
========
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2024 Tarides                                               */
>>>>>>>> upstream/5.4:runtime/caml/compatibility.h
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

<<<<<<<< HEAD:runtime4/caml/hooks.h
#ifndef CAML_HOOKS_H
#define CAML_HOOKS_H

#include "misc.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif
========
/* Definitions for compatibility with old identifiers. */
>>>>>>>> upstream/5.4:runtime/caml/compatibility.h

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

<<<<<<<< HEAD:runtime4/caml/hooks.h
#ifdef NATIVE_CODE

/* executed just before calling the entry point of a dynamically
   loaded native code module. */
CAMLextern void (*caml_natdynlink_hook)(void* handle, const char* unit);

#endif /* NATIVE_CODE */

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_HOOKS_H */
========
#define HAS_STDINT_H 1 /* Deprecated since OCaml 5.3 */

/* HAS_NANOSECOND_STAT is deprecated since OCaml 5.3 */
#if defined(HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC)
#  define HAS_NANOSECOND_STAT 1
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC)
#  define HAS_NANOSECOND_STAT 2
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
#  define HAS_NANOSECOND_STAT 3
#endif

#ifndef _WIN32
/* unistd.h is assumed to be available */
#define HAS_UNISTD 1
#endif

#endif  /* CAML_COMPATIBILITY_H */
>>>>>>>> upstream/5.4:runtime/caml/compatibility.h
