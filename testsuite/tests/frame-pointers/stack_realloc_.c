#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

value c_fun(void)
{
  /* A local root keeps this frame non-trivial, so the C compiler need
     not save x29 just below the c_stack_link. */
  CAMLparam0();
  CAMLlocal1(v);
  const value *callback = caml_named_value("callback");

  v = caml_callback(*callback, Val_unit);

  if (Is_exception_result(v))
    CAMLreturn(Val_long(-1));

  CAMLreturn(Val_long(Long_val(v) + 1));
}
