/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2011 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Auxiliary functions for custom hash functions */

#ifndef CAML_HASH_H
#define CAML_HASH_H

#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLextern uint32_t caml_hash_mix_uint32(uint32_t h, uint32_t d);
CAMLextern uint32_t caml_hash_mix_intnat(uint32_t h, intnat d);
CAMLextern uint32_t caml_hash_mix_int64(uint32_t h, int64_t d);
CAMLextern uint32_t caml_hash_mix_double(uint32_t h, double d);
CAMLextern uint32_t caml_hash_mix_float(uint32_t h, float d);
CAMLextern uint32_t caml_hash_mix_string(uint32_t h, value s);

#ifdef __cplusplus
}
#endif


#ifdef CAML_INTERNALS

/* The MurmurHash 3 mixing steps, used by the caml_hash_mix_*
   functions in runtime/hash.c and by the generic hashing primitives
   in stdlib/prims/hash_prims.c. */

#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d) \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

#define FINAL_MIX(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

#endif /* CAML_INTERNALS */

#endif /* CAML_HASH_H */
