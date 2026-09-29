#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"

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
