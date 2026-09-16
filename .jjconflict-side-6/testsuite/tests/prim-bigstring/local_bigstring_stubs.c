#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"

static const struct custom_operations caml_bigstring_local_ops = {
  /* Same identifier as [caml_ba_ops] --- stack-allocated bigstrings demarshal
   * into heap-allocated bigstrings that own and free their backing memory */
  "_bigarr02",
  /* [caml_alloc_custom_local] does not support finalizers */
  custom_finalize_default,
  /* Remaining fields are the same as [caml_ba_ops] */
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  caml_ba_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Like [caml_ba_alloc]. The allocated block always has the flags corresponding
 * to a plain bigstring. When stack allocation is enabled, the block is
 * allocated on the stack, and the [CAML_BA_STACK] flag is set.
 *
 * When [data = NULL], [caml_bigstring_alloc_local] does _not_ malloc new
 * backing memory owned by the allocated block. Instead, the block's data
 * pointer simply remains [NULL]. */

static value caml_bigstring_alloc_local(void * data, intnat len)
{
  struct caml_ba_array * b;
  value res;

  CAMLassert(len >= 0);
  res = caml_alloc_custom_local(&caml_bigstring_local_ops,
                               SIZEOF_BA_ARRAY + sizeof(intnat), 0, 1);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = 1;
  b->flags = CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  /* Test stubs can be linked with either the native or bytecode runtime. */
  if (caml_is_stack(res)) b->flags |= CAML_BA_STACK;
  b->proxy = NULL;
  b->dim[0] = len;
  return res;
}

/* Stack-allocate a bigstring view into a subrange. It is the caller's
 * responsibility to ensure [vb] remains alive for the duration of the lifetime
 * of the returned bigstring. */

CAMLprim value local_bigstring_sub_local(value vb, value vofs, value vlen)
{
  CAMLparam1(vb);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  intnat ofs = Long_val(vofs);
  intnat len = Long_val(vlen);
  void * data;

  if (b->num_dims != 1 ||
      (b->flags & (CAML_BA_KIND_MASK | CAML_BA_LAYOUT_MASK)) !=
        (CAML_BA_CHAR | CAML_BA_C_LAYOUT))
    caml_invalid_argument("with_sub_local: not a bigstring");
  if (ofs < 0 || len < 0 || ofs > b->dim[0] || len > b->dim[0] - ofs)
    caml_invalid_argument("with_sub_local: bad subrange");
  data = b->data;
  /* Avoid pointer arithmetic on NULL data pointer. */
  data = data ? (char *) data + ofs : data;
  CAMLreturn(caml_bigstring_alloc_local(data, len));
}

CAMLprim value local_bigstring_owns_data(value v)
{
  struct caml_ba_array *b = Caml_ba_array_val(v);
  return Val_bool((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_MANAGED
                  && b->proxy == NULL);
}

CAMLprim value local_bigstring_has_finalizer(value v)
{
  return Val_bool(Custom_ops_val(v)->finalize != NULL);
}
