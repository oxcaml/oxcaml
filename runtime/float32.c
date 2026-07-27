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

/* Custom operations and comparison for boxed 32-bit floats, used by
   the marshaller and the generic comparison and hashing functions.
   The float32 arithmetic primitives are in
   stdlib/prims/float32_prims.c. */

#include <assert.h>
#include <stdint.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/float32.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

static_assert(sizeof(float) == sizeof(int32_t), "");

intnat caml_float32_compare_unboxed(float f, float g)
{
  /* If one or both of f and g is NaN, order according to the convention
     NaN = NaN and NaN < x for all other floats x. */
  /* This branchless implementation is from GPR#164.
     Note that [f == f] if and only if f is not NaN.
     We expand each subresult of the expression to
     avoid sign-extension on 64bit. GPR#2250.  */
  intnat res =
      (intnat)(f > g) - (intnat)(f < g) + (intnat)(f == f) - (intnat)(g == g);
  return res;
}

static int float32_cmp(value v1, value v2)
{
  return caml_float32_compare_unboxed(Float32_val(v1), Float32_val(v2));
}

static intnat float32_hash(value v)
{
  union {
    float f;
    uint32_t i;
  } u;
  uint32_t n;
  u.f = Float32_val(v);  n = u.i;
  /* Normalize NaNs */
  if ((n & 0x7F800000) == 0x7F800000 && (n & 0x007FFFFF) != 0) {
    n = 0x7F800001;
  }
  /* Normalize -0 into +0 */
  else if (n == 0x80000000) {
    n = 0;
  }
  return n;
}

static uintnat float32_deserialize(void *dst)
{
  *((float *)dst) = caml_deserialize_float_4();
  return 4;
}

static void float32_serialize(value v, uintnat *bsize_32,
                              uintnat *bsize_64)
{
  caml_serialize_float_4(Float32_val(v));
  *bsize_32 = *bsize_64 = 4;
}

static const struct custom_fixed_length float32_length = {4, 4};

CAMLexport const struct custom_operations caml_float32_ops = {
  "_f32",
  custom_finalize_default,
  float32_cmp,
  float32_hash,
  float32_serialize,
  float32_deserialize,
  custom_compare_ext_default,
  &float32_length
};
