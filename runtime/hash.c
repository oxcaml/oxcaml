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

/* The caml_hash_mix_* functions: mixing steps used by the hash
   functions of custom blocks (and by the generic hashing primitives
   in stdlib/prims/hash_prims.c).  Based on MurmurHash 3,
   https://github.com/aappleby/smhasher/ -- the mixing macros are in
   caml/hash.h. */

#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/hash.h"

CAMLexport uint32_t caml_hash_mix_uint32(uint32_t h, uint32_t d)
{
  MIX(h, d);
  return h;
}

/* Mix a platform-native integer. */

CAMLexport uint32_t caml_hash_mix_intnat(uint32_t h, intnat d)
{
  uint32_t n;
#ifdef ARCH_SIXTYFOUR
  /* Mix the low 32 bits and the high 32 bits, in a way that preserves
     32/64 compatibility: we want n = (uint32_t) d
     if d is in the range [-2^31, 2^31-1]. */
  n = (d >> 32) ^ (d >> 63) ^ d;
  /* If 0 <= d < 2^31:   d >> 32 = 0     d >> 63 = 0
     If -2^31 <= d < 0:  d >> 32 = -1    d >> 63 = -1
     In both cases, n = (uint32_t) d.  */
#else
  n = d;
#endif
  MIX(h, n);
  return h;
}

/* Mix a 64-bit integer. */

CAMLexport uint32_t caml_hash_mix_int64(uint32_t h, int64_t d)
{
  uint32_t hi = (uint32_t) (d >> 32), lo = (uint32_t) d;
  MIX(h, lo);
  MIX(h, hi);
  return h;
}

/* Mix a double-precision float.
   Treats +0.0 and -0.0 identically.
   Treats all NaNs identically.
*/

CAMLexport uint32_t caml_hash_mix_double(uint32_t hash, double d)
{
  union {
    double d;
#if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
    struct { uint32_t h; uint32_t l; } i;
#else
    struct { uint32_t l; uint32_t h; } i;
#endif
  } u;
  uint32_t h, l;
  /* Convert to two 32-bit halves */
  u.d = d;
  h = u.i.h; l = u.i.l;
  /* Normalize NaNs */
  if ((h & 0x7FF00000) == 0x7FF00000 && (l | (h & 0xFFFFF)) != 0) {
    h = 0x7FF00000;
    l = 0x00000001;
  }
  /* Normalize -0 into +0 */
  else if (h == 0x80000000 && l == 0) {
    h = 0;
  }
  MIX(hash, l);
  MIX(hash, h);
  return hash;
}

/* Mix a single-precision float.
   Treats +0.0 and -0.0 identically.
   Treats all NaNs identically.
*/

CAMLexport uint32_t caml_hash_mix_float(uint32_t hash, float d)
{
  union {
    float f;
    uint32_t i;
  } u;
  uint32_t n;
  /* Convert to int32_t */
  u.f = d;  n = u.i;
  /* Normalize NaNs */
  if ((n & 0x7F800000) == 0x7F800000 && (n & 0x007FFFFF) != 0) {
    n = 0x7F800001;
  }
  /* Normalize -0 into +0 */
  else if (n == 0x80000000) {
    n = 0;
  }
  MIX(hash, n);
  return hash;
}

/* Mix an OCaml string */

CAMLexport uint32_t caml_hash_mix_string(uint32_t h, value s)
{
  mlsize_t len = caml_string_length(s);
  mlsize_t i;
  uint32_t w;

  /* Mix by 32-bit blocks (little-endian) */
  for (i = 0; i + 4 <= len; i += 4) {
#ifdef ARCH_BIG_ENDIAN
    w = Byte_u(s, i)
        | (Byte_u(s, i+1) << 8)
        | (Byte_u(s, i+2) << 16)
        | (Byte_u(s, i+3) << 24);
#else
    w = *((uint32_t *) &Byte_u(s, i));
#endif
    MIX(h, w);
  }
  /* Finish with up to 3 bytes */
  w = 0;
  switch (len & 3) {
  case 3: w  = Byte_u(s, i+2) << 16; fallthrough;
  case 2: w |= Byte_u(s, i+1) << 8;  fallthrough;
  case 1: w |= Byte_u(s, i);
          MIX(h, w);                 fallthrough;
  default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
  }
  /* Finally, mix in the length.  Ignore the upper 32 bits, generally 0. */
  h ^= (uint32_t) len;
  return h;
}
