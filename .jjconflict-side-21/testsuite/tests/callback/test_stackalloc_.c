#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>


value apply10(value cb)
{
  CAMLparam1(cb);
  CAMLlocal1(ref);
  for (int i = 0; i < 10; i++) {
    caml_region_t reg = caml_region_begin();
    ref = caml_alloc_local(1, 0);
    Field(ref, 0) = Val_int(i);
    caml_callback_exn(cb, ref);
    ref = Val_unit;
    caml_region_end(reg);
  }
  CAMLreturn(Val_unit);
}
