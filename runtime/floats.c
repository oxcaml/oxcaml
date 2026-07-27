/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Runtime support for floats: the locale used for float formatting
   and parsing, and libm shims.  The float primitives are in
   stdlib/prims/floats_prims.c. */

/* Needed for uselocale */
#define _XOPEN_SOURCE 700

#include <math.h>
#include "caml/misc.h"
#include "caml/mlvalues.h"

#if defined(HAS_LOCALE) || defined(__MINGW32__)

#if defined(HAS_LOCALE_H) || defined(__MINGW32__)
#include <locale.h>
#endif

#if defined(HAS_XLOCALE_H)
#include <xlocale.h>
#endif

#if defined(_MSC_VER)
#ifndef locale_t
#define locale_t _locale_t
#endif
#ifndef freelocale
#define freelocale _free_locale
#endif
#endif

#endif /* defined(HAS_LOCALE) */

/*
 OCaml runtime itself doesn't call setlocale, i.e. it is using
 standard "C" locale by default, but it is possible that
 third-party code loaded into process does.
*/
#ifdef HAS_LOCALE
locale_t caml_locale = (locale_t)0;
#endif


void caml_init_locale(void)
{
#if defined(_MSC_VER) || defined(__MINGW32__)
  _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
#endif
#ifdef HAS_LOCALE
  if ((locale_t)0 == caml_locale)
  {
#if defined(_MSC_VER)
    caml_locale = _create_locale(LC_NUMERIC, "C");
#else
    caml_locale = newlocale(LC_NUMERIC_MASK,"C",(locale_t)0);
#endif
  }
#endif
}

void caml_free_locale(void)
{
#ifdef HAS_LOCALE
  if ((locale_t)0 != caml_locale) freelocale(caml_locale);
  caml_locale = (locale_t)0;
#endif
}

/* This emulation of log1p() is due to William Kahan.
   See http://www.plunk.org/~hatch/rightway.php */
CAMLexport double caml_log1p(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return log1p(x);
#else
  double u = 1. + x;
  if (u == 1.)
    return x;
  else
    return log(u) * x / (u - 1.);
#endif
}
