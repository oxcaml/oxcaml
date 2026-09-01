#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value dynamic_callback_call(value f)
{
  CAMLparam1(f);
  CAMLlocal1(res);
  res = caml_callback(f, Val_unit);
  CAMLreturn(res);
}
