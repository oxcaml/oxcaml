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

/* Runtime services on strings.  The String and Bytes primitives are
   in stdlib/prims/str_prims.c. */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* returns a number of bytes (chars) */
CAMLexport mlsize_t caml_string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  CAMLassert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

CAMLexport int caml_string_is_c_safe (value s)
{
  return strlen(String_val(s)) == caml_string_length(s);
}

CAMLexport value caml_alloc_sprintf(const char * format, ...)
{
  va_list args;
  char buf[128];
  int n;
  value res;

#if !defined(_WIN32) || defined(_UCRT)
  /* C99-compliant implementation */
  va_start(args, format);
  /* "vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest", including the terminating '\0'.
     It returns the number of characters of the formatted string,
     excluding the terminating '\0'. */
  n = vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  if (n < 0) {
    caml_raise_out_of_memory();
  } else if (n < sizeof(buf)) {
    /* All output characters were written to buf, including the
       terminating '\0'.  Allocate a Caml string with length "n"
       as computed by vsnprintf, and copy the output of vsnprintf into it. */
    res = caml_alloc_initialized_string(n, buf);
  } else {
    /* PR#7568: if the format is in the Caml heap, the following
       caml_alloc_string could move or free the format.  To prevent
       this, take a copy of the format outside the Caml heap. */
    char * saved_format = caml_stat_strdup(format);
    /* Allocate a Caml string with length "n" as computed by vsnprintf. */
    res = caml_alloc_string(n);
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to vsnprintf is n+1. */
    va_start(args, format);
    vsnprintf((char *)String_val(res), n + 1, saved_format, args);
    va_end(args);
    caml_stat_free(saved_format);
  }
  return res;
#else
  /* Implementation specific to the Microsoft CRT library */
  va_start(args, format);
  /* "_vsnprintf(dest, sz, format, args)" writes at most "sz" characters
     into "dest".  Let "len" be the number of characters of the formatted
     string.
     If "len" < "sz", a null terminator was appended, and "len" is returned.
     If "len" == "sz", no null termination, and "len" is returned.
     If "len" > "sz", a negative value is returned. */
  n = _vsnprintf(buf, sizeof(buf), format, args);
  va_end(args);
  if (n >= 0 && n <= sizeof(buf)) {
    /* All output characters were written to buf.
       "n" is the actual length of the output.
       Allocate a Caml string of length "n" and copy the characters into it. */
    res = caml_alloc_string(n);
    memcpy((char *)String_val(res), buf, n);
  } else {
    /* PR#7568: if the format is in the Caml heap, the following
       caml_alloc_string could move or free the format.  To prevent
       this, take a copy of the format outside the Caml heap. */
    char * saved_format = caml_stat_strdup(format);
    /* Determine actual length of output, excluding final '\0' */
    va_start(args, format);
    n = _vscprintf(format, args);
    va_end(args);
    res = caml_alloc_string(n);
    /* Re-do the formatting, outputting directly in the Caml string.
       Note that caml_alloc_string left room for a '\0' at position n,
       so the size passed to _vsnprintf is n+1. */
    va_start(args, format);
    _vsnprintf((char *)String_val(res), n + 1, saved_format, args);
    va_end(args);
    caml_stat_free(saved_format);
  }
  return res;
#endif
}
