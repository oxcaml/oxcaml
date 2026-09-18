#include "caml/mlvalues.h"
#include "caml/bigarray.h"
#include "caml/custom.h"

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
