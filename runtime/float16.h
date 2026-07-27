/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FLOAT16_H
#define CAML_FLOAT16_H

#ifdef CAML_INTERNALS

/* Conversions between half-precision and single-precision floating
   point numbers, shared between the bigarray machinery
   (runtime/bigarray.c) and the Bigarray primitives
   (stdlib/prims/bigarray_prims.c). */

#include <stdint.h>
#include "caml/bigarray.h"
#include "caml/misc.h"

#if defined(__GNUC__) && defined(__aarch64__)

union float16_bits { uint16_t i; _Float16 f; };

Caml_inline float caml_float16_to_float(caml_ba_uint16 d)
{
  union float16_bits u;
  u.i = d; return u.f;
}

Caml_inline caml_ba_uint16 caml_float_to_float16(float d)
{
  union float16_bits u;
  u.f = d; return u.i;
}

#elif defined(__GNUC__) && defined(__F16C__)

#include <immintrin.h>

Caml_inline float caml_float16_to_float(caml_ba_uint16 d)
{ return _cvtsh_ss(d); }

Caml_inline caml_ba_uint16 caml_float_to_float16(float d)
{ return _cvtss_sh(d, (_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC)); }

#else

union float_bits {
  uint32_t i;
  float f;
};

/*
 * half_to_float_fast5
 * https://gist.github.com/rygorous/2144712
 */
Caml_inline float caml_float16_to_float(caml_ba_uint16 d)
{
  static const union float_bits magic = { (254 - 15) << 23 };
  static const union float_bits was_infnan = { (127 + 16) << 23 };

  union float_bits o;

  o.i = (d & 0x7fff) << 13;     /* exponent/mantissa bits */
  o.f *= magic.f;               /* exponent adjust */
  if (o.f >= was_infnan.f)      /* make sure Inf/NaN survive */
    o.i |= 255 << 23;
  o.i |= (d & 0x8000) << 16;    /* sign bit */
  return o.f;
}

/*
 * float_to_half_fast3_rtne
 * https://gist.github.com/rygorous/2156668
 */
Caml_inline caml_ba_uint16 caml_float_to_float16(float d)
{
  static const union float_bits f32infty = { 255 << 23 };
  static const union float_bits f16max = { (127 + 16) << 23 };
  static const union float_bits denorm_magic =
    { ((127 - 15) + (23 - 10) + 1) << 23 };
  static const uint32_t sign_mask = 0x80000000u;

  union float_bits f;
  caml_ba_uint16 o = 0;
  uint32_t sign;

  f.f = d;
  sign = f.i & sign_mask;
  f.i ^= sign;

  // NOTE all the integer compares in this function can be safely
  // compiled into signed compares since all operands are below
  // 0x80000000. Important if you want fast straight SSE2 code
  // (since there's no unsigned PCMPGTD).

  if (f.i >= f16max.i) // result is Inf or NaN (all exponent bits set)
    o = (f.i > f32infty.i) ? 0x7e00 : 0x7c00; // NaN->qNaN and Inf->Inf
  else // (De)normalized number or zero
  {
    if (f.i < (113 << 23)) // resulting FP16 is subnormal or zero
    {
      // use a magic value to align our 10 mantissa bits at the bottom of
      // the float. as long as FP addition is round-to-nearest-even this
      // just works.
      f.f += denorm_magic.f;

      // and one integer subtract of the bias later, we have our final float!
      o = f.i - denorm_magic.i;
    }
    else
    {
      uint32_t mant_odd = (f.i >> 13) & 1; // resulting mantissa is odd

      // update exponent, rounding bias part 1
      f.i += ((uint32_t)(15 - 127) << 23) + 0xfff;
      // rounding bias part 2
      f.i += mant_odd;
      // take the bits!
      o = f.i >> 13;
    }
  }

  o |= sign >> 16;
  return o;
}

#endif  /* defined(__GNUC__) && defined(__F16C__) */


#endif /* CAML_INTERNALS */

#endif /* CAML_FLOAT16_H */
