/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                      Max Slater, Jane Street                           */
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

/* Needed for uselocale */
#define _XOPEN_SOURCE 700

/* Needed for strtof_l */
#define _GNU_SOURCE

#include <math.h>
#include <float.h>
#include <limits.h>
#include <string.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/intext.h"
#include "caml/mlvalues.h"

#ifndef CAML_FLOAT32_H
#define CAML_FLOAT32_H

#define Float32_val(v) (*((float *)Data_custom_val(v)))

#endif /* CAML_FLOAT32_H */

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
#ifndef strtof_l
#define strtof_l _strtof_l
#endif
#endif

#endif /* defined(HAS_LOCALE) */

/*
 OCaml runtime itself doesn't call setlocale, i.e. it is using
 standard "C" locale by default, but it is possible that
 third-party code loaded into process does.
*/
#ifdef HAS_LOCALE
extern locale_t caml_locale;
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
/* there is no analogue to uselocale in MSVC so just set locale for thread */
#define USE_LOCALE setlocale(LC_NUMERIC,"C")
#define RESTORE_LOCALE do {} while(0)
#elif defined(HAS_LOCALE)
#define USE_LOCALE locale_t saved_locale = uselocale(caml_locale)
#define RESTORE_LOCALE uselocale(saved_locale)
#else
#define USE_LOCALE do {} while(0)
#define RESTORE_LOCALE do {} while(0)
#endif

CAMLprim value caml_format_float32(value fmt, value arg)
{
  /* See caml_format_float */
  value res;
  float f = Float32_val(arg);

#ifdef HAS_BROKEN_PRINTF
  if (isfinite(f)) {
#endif
    USE_LOCALE;
    res = caml_alloc_sprintf(String_val(fmt), f);
    RESTORE_LOCALE;
#ifdef HAS_BROKEN_PRINTF
  } else {
    if (isnan(f)) {
      res = caml_copy_string("nan");
    } else {
      if (f > 0)
        res = caml_copy_string("inf");
      else
        res = caml_copy_string("-inf");
    }
  }
#endif
  return res;
}
