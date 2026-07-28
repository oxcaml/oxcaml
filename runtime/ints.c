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

/* Custom operations for the boxed integer types int32, int64 and
   nativeint, used by the marshaller and the generic comparison and
   hashing functions.  The arithmetic primitives are in
   stdlib/prims/ints_prims.c. */

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* 32-bit integers */

static int int32_cmp(value v1, value v2)
{
  int32_t i1 = Int32_val(v1);
  int32_t i2 = Int32_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat int32_hash(value v)
{
  return Int32_val(v);
}

static void int32_serialize(value v, uintnat * bsize_32,
                            uintnat * bsize_64)
{
  caml_serialize_int_4(Int32_val(v));
  *bsize_32 = *bsize_64 = 4;
}

static uintnat int32_deserialize(void * dst)
{
  *((int32_t *) dst) = caml_deserialize_sint_4();
  return 4;
}

static const struct custom_fixed_length int32_length = { 4, 4 };

CAMLexport const struct custom_operations caml_int32_ops = {
  "_i",
  custom_finalize_default,
  int32_cmp,
  int32_hash,
  int32_serialize,
  int32_deserialize,
  custom_compare_ext_default,
  &int32_length
};


/* 64-bit integers */

static int int64_cmp(value v1, value v2)
{
  int64_t i1 = Int64_val(v1);
  int64_t i2 = Int64_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat int64_hash(value v)
{
  int64_t x = Int64_val(v);
  uint32_t lo = (uint32_t) x, hi = (uint32_t) (x >> 32);
  return hi ^ lo;
}

static void int64_serialize(value v, uintnat * bsize_32,
                            uintnat * bsize_64)
{
  caml_serialize_int_8(Int64_val(v));
  *bsize_32 = *bsize_64 = 8;
}

static uintnat int64_deserialize(void * dst)
{
#ifndef ARCH_ALIGN_INT64
  *((int64_t *) dst) = caml_deserialize_sint_8();
#else
  union { int32_t i[2]; int64_t j; } buffer;
  buffer.j = caml_deserialize_sint_8();
  ((int32_t *) dst)[0] = buffer.i[0];
  ((int32_t *) dst)[1] = buffer.i[1];
#endif
  return 8;
}

static const struct custom_fixed_length int64_length = { 8, 8 };

CAMLexport const struct custom_operations caml_int64_ops = {
  "_j",
  custom_finalize_default,
  int64_cmp,
  int64_hash,
  int64_serialize,
  int64_deserialize,
  custom_compare_ext_default,
  &int64_length
};


/* Native integers */

static int nativeint_cmp(value v1, value v2)
{
  intnat i1 = Nativeint_val(v1);
  intnat i2 = Nativeint_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intnat nativeint_hash(value v)
{
  intnat n = Nativeint_val(v);
#ifdef ARCH_SIXTYFOUR
  /* 32/64 bits compatibility trick.  See explanations in file "hash.c",
     function caml_hash_mix_intnat. */
  return (n >> 32) ^ (n >> 63) ^ n;
#else
  return n;
#endif
}

static void nativeint_serialize(value v, uintnat * bsize_32,
                                uintnat * bsize_64)
{
  intnat l = Nativeint_val(v);
#ifdef ARCH_SIXTYFOUR
  if ((intnat)INT32_MIN <= l && l <= (intnat)INT32_MAX) {
    caml_serialize_int_1(1);
    caml_serialize_int_4((int32_t) l);
  } else {
    caml_serialize_int_1(2);
    caml_serialize_int_8(l);
  }
#else
  caml_serialize_int_1(1);
  caml_serialize_int_4(l);
#endif
  *bsize_32 = 4;
  *bsize_64 = 8;
}

static uintnat nativeint_deserialize(void * dst)
{
  switch (caml_deserialize_uint_1()) {
  case 1:
    *((intnat *) dst) = caml_deserialize_sint_4();
    break;
  case 2:
#ifdef ARCH_SIXTYFOUR
    *((intnat *) dst) = caml_deserialize_sint_8();
#else
    caml_deserialize_error("input_value: native integer value too large");
#endif
    break;
  default:
    caml_deserialize_error("input_value: ill-formed native integer");
  }
  return sizeof(intnat);
}

static const struct custom_fixed_length nativeint_length = { 4, 8 };
CAMLexport const struct custom_operations caml_nativeint_ops = {
  "_n",
  custom_finalize_default,
  nativeint_cmp,
  nativeint_hash,
  nativeint_serialize,
  nativeint_deserialize,
  custom_compare_ext_default,
  &nativeint_length
};
