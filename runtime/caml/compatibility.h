/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
<<<<<<<< oxcaml:runtime4/caml/hooks.h
/*                    Fabrice Le Fessant, INRIA de Paris                  */
|||||||| upstream-base:runtime/caml/atomic_refcount.h
/*      Florian Angeletti, projet Cambium, Inria                          */
========
/*                         Antonin Decimo, Tarides                        */
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h
/*                                                                        */
<<<<<<<< oxcaml:runtime4/caml/hooks.h
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
|||||||| upstream-base:runtime/caml/atomic_refcount.h
/*   Copyright 2022 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
========
/*   Copyright 2024 Tarides                                               */
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

<<<<<<<< oxcaml:runtime4/caml/hooks.h
#ifndef CAML_HOOKS_H
#define CAML_HOOKS_H

#include "misc.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif
|||||||| upstream-base:runtime/caml/atomic_refcount.h
#ifndef CAML_ATOMIC_REFCOUNT_H
#define CAML_ATOMIC_REFCOUNT_H
========
/* Definitions for compatibility with old identifiers. */
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

<<<<<<<< oxcaml:runtime4/caml/hooks.h
#ifdef NATIVE_CODE
|||||||| upstream-base:runtime/caml/atomic_refcount.h
#include "camlatomic.h"
========
#define HAS_STDINT_H 1 /* Deprecated since OCaml 5.3 */
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h

<<<<<<<< oxcaml:runtime4/caml/hooks.h
/* executed just before calling the entry point of a dynamically
   loaded native code module. */
CAMLextern void (*caml_natdynlink_hook)(void* handle, const char* unit);
|||||||| upstream-base:runtime/caml/atomic_refcount.h
Caml_inline void caml_atomic_refcount_init(atomic_uintnat* refc, uintnat n){
  atomic_store_release(refc, n);
}
========
/* HAS_NANOSECOND_STAT is deprecated since OCaml 5.3 */
#if defined(HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC)
#  define HAS_NANOSECOND_STAT 1
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC)
#  define HAS_NANOSECOND_STAT 2
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
#  define HAS_NANOSECOND_STAT 3
#endif
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h

<<<<<<<< oxcaml:runtime4/caml/hooks.h
#endif /* NATIVE_CODE */

#endif /* CAML_INTERNALS */

#ifdef __cplusplus
}
#endif

#endif /* CAML_HOOKS_H */
|||||||| upstream-base:runtime/caml/atomic_refcount.h
Caml_inline uintnat caml_atomic_refcount_decr(atomic_uintnat* refcount){
  return atomic_fetch_add (refcount, -1);
}

Caml_inline uintnat caml_atomic_refcount_incr(atomic_uintnat* refcount){
  return atomic_fetch_add (refcount, 1);
}

#endif /* CAML_INTERNALS */

#endif // CAML_ATOMIC_REFCOUNT_H
========
#ifndef _WIN32
/* unistd.h is assumed to be available */
#define HAS_UNISTD 1
#endif

#endif  /* CAML_COMPATIBILITY_H */
>>>>>>>> upstream-incoming:runtime/caml/compatibility.h
