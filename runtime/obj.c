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

/* Operations on objects */

#include <assert.h>
#include <string.h>
#include "caml/camlatomic.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"
#include "caml/prims.h"
#include "caml/signals.h"

static int obj_tag (value arg)
{
  header_t hd;

  if (Is_null(arg)) {
    return 1010;   /* null_tag */
  } else if (Is_long (arg)) {
    return 1000;   /* int_tag */
  } else if ((long) arg & (sizeof (value) - 1)) {
    return 1002;   /* unaligned_tag */
  } else {
    /* The acquire load ensures that reading the field of a Forward_tag
       block in stdlib/camlinternalLazy.ml:force_gen has the necessary
       synchronization. */
    hd = (header_t)atomic_load_acquire(Hp_atomic_val(arg));
    return Tag_hd(hd);
  }
}

CAMLprim value caml_obj_tag(value arg)
{
  return Val_int (obj_tag(arg));
}

CAMLprim value caml_obj_make_forward(value blk, value fwd)
{
  caml_modify(&Field(blk, 0), fwd);
  Tag_val (blk) = Forward_tag;
  return Val_unit;
}

/* The following functions are used to support lazy values. They are not
 * written in OCaml in order to ensure atomicity guarantees with respect to the
 * GC. */
CAMLprim value caml_lazy_make_forward (value v)
{
  CAMLparam1 (v);
  CAMLlocal1 (res);

  res = caml_alloc_small (1, Forward_tag);
  Field (res, 0) = v;
  CAMLreturn (res);
}

static int obj_update_tag (value blk, int old_tag, int new_tag)
{
  header_t hd;
  tag_t tag;

  SPIN_WAIT {
    hd = Hd_val(blk);
    tag = Tag_hd(hd);

    if (tag != old_tag) return 0;
    if (caml_domain_alone()) {
      Unsafe_store_tag_val(blk, new_tag);
      return 1;
    }

    if (atomic_compare_exchange_strong(Hp_atomic_val(blk), &hd,
                                       Hd_with_tag(hd, new_tag)))
      return 1;
  }
}

CAMLprim value caml_lazy_reset_to_lazy (value v)
{
  CAMLassert (Tag_val(v) == Forcing_tag);

  obj_update_tag (v, Forcing_tag, Lazy_tag);
  return Val_unit;
}

CAMLprim value caml_lazy_update_to_forward (value v)
{
  CAMLassert (Tag_val(v) == Forcing_tag);

  obj_update_tag (v, Forcing_tag, Forward_tag);
  return Val_unit;
}

CAMLprim value caml_lazy_read_result (value v)
{
  if (obj_tag(v) == Forward_tag)
    return Field(v,0);
  return v;
}

CAMLprim value caml_lazy_update_to_forcing (value v)
{
  if (Is_block(v) && /* Needed to ensure that we don't attempt to update the
                        header of a integer value */
      obj_update_tag (v, Lazy_tag, Forcing_tag)) {
    return Val_int(0);
  } else {
    return Val_int(1);
  }
}

/* Compute how many words in the heap are occupied by blocks accessible
   from a given value */

#define ENTRIES_PER_QUEUE_CHUNK 4096
struct queue_chunk {
  struct queue_chunk *next;
  value entries[ENTRIES_PER_QUEUE_CHUNK];
};

/* For compiling let rec over values */

/* [size] is a [value] representing number of words (fields) */
CAMLprim value caml_alloc_dummy(value size)
{
  mlsize_t wosize = Long_val(size);
  return caml_alloc (wosize, 0);
}

/* [size] is a [value] representing number of floats. */
CAMLprim value caml_alloc_dummy_float (value size)
{
  mlsize_t wosize = Long_val(size) * Double_wosize;
  return caml_alloc (wosize, 0);
}

/* [size] is a [value] representing the number of fields.
   [scannable_size] is a [value] representing the length of the prefix of
   fields that contains pointer values.
*/
CAMLprim value caml_alloc_dummy_mixed (value size, value scannable_size)
{
  mlsize_t wosize = Long_val(size);
#ifdef NATIVE_CODE
  mlsize_t scannable_wosize = Long_val(scannable_size);
  /* The below code runs for bytecode and native code, and critically assumes
     that a double record field can be stored in one word. That's true both for
     32-bit and 64-bit bytecode (as a double record field in a mixed record is
     always boxed), and for 64-bit native code (as the double record field is
     stored flat, taking up 1 word).
  */
  static_assert(Double_wosize == 1, "");
  reserved_t reserved =
    Reserved_mixed_block_scannable_wosize_native(scannable_wosize);
#else
  /* [scannable_size] can't be used meaningfully in bytecode */
  (void)scannable_size;
  reserved_t reserved = Faux_mixed_block_sentinel;
#endif // NATIVE_CODE
  return caml_alloc_with_reserved (wosize, 0, reserved);
}

/* This is a specialized primitive despite being expressible in terms
   of [caml_alloc_dummy], because lambda/Value_rec_compiler recognizes
   calls to this function specifically -- the distinction lets us
   reconstruct type information that is useful for compilation. */
CAMLprim value caml_alloc_dummy_lazy (value unit)
{
  return caml_alloc(1, 0);
}

CAMLprim value caml_update_dummy(value dummy, value newval)
{
  mlsize_t size;
  tag_t tag;

  tag = Tag_val (newval);
  CAMLassert (tag != Infix_tag);
  CAMLassert(tag != Closure_tag);

  if (Wosize_val(dummy) == 0) {
      /* Size-0 blocks are statically-allocated atoms. We cannot
         mutate them, but there is no need:
         - All atoms used in the runtime to represent OCaml values
           have tag 0 --- including empty flat float arrays, or other
           types that use a non-0 tag for non-atom blocks.
         - The dummy was already created with tag 0.
         So doing nothing suffices. */
      CAMLassert(Wosize_val(newval) == 0);
      CAMLassert(Tag_val(dummy) == Tag_val(newval));
  } else if (tag == Double_array_tag){
    CAMLassert (Wosize_val(newval) == Wosize_val(dummy));
    CAMLassert (Tag_val(dummy) != Infix_tag);
    Unsafe_store_tag_val(dummy, Double_array_tag);
    size = Wosize_val (newval) / Double_wosize;
    for (mlsize_t i = 0; i < size; i++) {
      Store_double_flat_field (dummy, i, Double_flat_field (newval, i));
    }
  } else {
    CAMLassert (Scannable_tag(tag));
    CAMLassert (Tag_val(dummy) != Infix_tag);
    CAMLassert (Reserved_val(dummy) == Reserved_val(newval));
    Unsafe_store_tag_val(dummy, tag);
    size = Wosize_val(newval);
    CAMLassert (size == Wosize_val(dummy));
    mlsize_t scannable_size = Scannable_wosize_val(newval);
    CAMLassert (scannable_size == Scannable_wosize_val(dummy));
    /* See comment above why this is safe even if [tag == Closure_tag]
       and some of the "values" being copied are actually code pointers.

       This reasoning does not apply to arbitrary flat fields, which might have
       the same shape as pointers into the minor heap, so we need to handle the
       non-scannable suffix of mixed blocks specially.
    */
    for (mlsize_t i = 0; i < scannable_size; i++){
      caml_modify (&Field(dummy, i), Field(newval, i));
    }
    for (mlsize_t i = scannable_size; i < size; i++) {
      Field(dummy, i) = Field(newval, i);
    }
  }
  return Val_unit;
}

CAMLprim value caml_update_dummy_lazy(value dummy, value newval)
{
  // Note: [obj_tag] works on immediates as well
  int tag = obj_tag (newval);
  switch (tag) {
  case Lazy_tag:
  case Forcing_tag:
  case Forward_tag:
    caml_update_dummy(dummy, newval);
    break;
  // If the tag of [newval] is not a lazy tag,
  // it comes from a Forward block that was shortcut.
  default:
    CAMLassert (Wosize_val(dummy) == 1);
    caml_modify(&Field(dummy, 0), newval);
    Unsafe_store_tag_val(dummy, Forward_tag);
    break;
  }
  return Val_unit;
}
