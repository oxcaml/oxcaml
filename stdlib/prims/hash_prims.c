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

/* The generic hashing primitive */

/* The interface of this file is in "mlvalues.h" (for [caml_hash_variant])
   and in "hash.h" (for the other exported functions). */

#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/hash.h"
#include "caml/fail.h"

/* The implementation based on MurmurHash 3,
   https://github.com/aappleby/smhasher/ */

/* Maximal size of the queue used for breadth-first traversal.  */
#define HASH_QUEUE_SIZE 256
/* Maximal number of Forward_tag links followed in one step */
#define MAX_FORWARD_DEREFERENCE 1000

/* The generic hash function */

/* Internally to Jane Street, we have renamed [caml_hash] to [caml_hash_exn]
   to suggest that calling it could raise. (E.g. it raises on mixed blocks.)
   As such, we've removed [@@noalloc] from the OCaml [external] that references
   this C binding, and would likewise need to remove [@@noalloc] from any
   other [external] formerly bound to [caml_hash].
*/

CAMLprim value caml_hash_exn(value count, value limit, value seed, value obj)
{
  value queue[HASH_QUEUE_SIZE]; /* Queue of values to examine */
  intnat rd;                    /* Position of first value in queue */
  intnat wr;                    /* One past position of last value in queue */
  intnat sz;                    /* Max number of values to put in queue */
  intnat num;                   /* Max number of meaningful values to see */
  uint32_t h;                     /* Rolling hash */
  value v;
  mlsize_t len;

  sz = Long_val(limit);
  if (sz < 0 || sz > HASH_QUEUE_SIZE) sz = HASH_QUEUE_SIZE;
  num = Long_val(count);
  h = Int_val(seed);
  queue[0] = obj; rd = 0; wr = 1;

  while (rd < wr && num > 0) {
    v = queue[rd++];
  again:
    if (Is_long(v)) {
      h = caml_hash_mix_intnat(h, v);
      num--;
    } else {
      switch (Tag_val(v)) {
      case String_tag:
        h = caml_hash_mix_string(h, v);
        num--;
        break;
      case Double_tag:
        h = caml_hash_mix_double(h, Double_val(v));
        num--;
        break;
      case Double_array_tag:
        for (mlsize_t i = 0, len = Wosize_val(v) / Double_wosize;
             i < len;
             i++) {
          h = caml_hash_mix_double(h, Double_flat_field(v, i));
          num--;
          if (num <= 0) break;
        }
        break;
      case Abstract_tag:
        /* Block contents unknown.  Do nothing. */
        break;
      case Infix_tag:
        /* Mix in the offset to distinguish different functions from
           the same mutually-recursive definition */
        h = caml_hash_mix_uint32(h, Infix_offset_val(v));
        v = v - Infix_offset_val(v);
        goto again;
      case Forward_tag:
        /* PR#6361: we can have a loop here, so limit the number of
           Forward_tag links being followed */
        for (mlsize_t i = MAX_FORWARD_DEREFERENCE; i > 0; i--) {
          v = Forward_val(v);
          if (Is_long(v) || Tag_val(v) != Forward_tag)
            goto again;
        }
        /* Give up on this object and move to the next */
        break;
      case Object_tag:
        h = caml_hash_mix_intnat(h, Oid_val(v));
        num--;
        break;
      case Custom_tag:
        /* If no hashing function provided, do nothing. */
        /* Only use low 32 bits of custom hash, for 32/64 compatibility */
        if (Custom_ops_val(v)->hash != NULL) {
          uint32_t n = (uint32_t) Custom_ops_val(v)->hash(v);
          h = caml_hash_mix_uint32(h, n);
          num--;
        }
        break;
      case Closure_tag: {
        mlsize_t i, startenv;
        len = Wosize_val(v);
        startenv = Start_env_closinfo(Closinfo_val(v));
        CAMLassert (startenv <= len);
        /* Mix in the tag and size, but do not count this towards [num] */
        h = caml_hash_mix_uint32(h, Cleanhd_hd(Hd_val(v)));
        /* Mix the code pointers, closure info fields, and infix headers */
        for (i = 0; i < startenv; i++) {
          h = caml_hash_mix_intnat(h, Field(v, i));
          num--;
        }
        /* Copy environment fields into queue,
           not exceeding the total size [sz] */
        for (/*nothing*/; i < len; i++) {
          if (wr >= sz) break;
          queue[wr++] = Field(v, i);
        }
        break;
      }
      case Cont_tag:
        /* All continuations hash to the same value,
           since we have no idea how to distinguish them. */
        break;

      default:
	if (Is_mixed_block_reserved(Reserved_val(v))) {
	  caml_invalid_argument("hash: mixed block value");
	}
        /* Mix in the tag and size, but do not count this towards [num] */
        h = caml_hash_mix_uint32(h, Cleanhd_hd(Hd_val(v)));
        /* Copy fields into queue, not exceeding the total size [sz] */
        for (mlsize_t i = 0, len = Wosize_val(v); i < len; i++) {
          if (wr >= sz) break;
          queue[wr++] = Field(v, i);
        }
        break;
      }
    }
  }
  /* Final mixing of bits */
  FINAL_MIX(h);
  /* Fold result to the range [0, 2^30-1] so that it is a nonnegative
     OCaml integer both on 32 and 64-bit platforms. */
  return Val_int(h & 0x3FFFFFFFU);
}

CAMLprim value caml_string_hash(value seed, value string)
{
  uint32_t h;
  h = Int_val(seed);
  h = caml_hash_mix_string (h, string);
  FINAL_MIX(h);
  return Val_int(h & 0x3FFFFFFFU);
}

/* Hashing variant tags */

CAMLexport value caml_hash_variant(char const * tag)
{
  value accu;
  /* Same hashing algorithm as in ../typing/btype.ml, function hash_variant */
  for (accu = Val_int(0); *tag != 0; tag++)
    accu = Val_int(223 * Int_val(accu) + *((unsigned char *) tag));
#ifdef ARCH_SIXTYFOUR
  accu = accu & Val_long(0x7FFFFFFFL);
#endif
  /* Force sign extension of bit 31 for compatibility between 32 and 64-bit
     platforms */
  return (int32_t) accu;
}
