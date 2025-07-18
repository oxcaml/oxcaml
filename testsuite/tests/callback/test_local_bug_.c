#include <caml/memory.h>
#include <caml/callback.h>

value local_args_repro_call(value fn) {
  CAMLparam1(fn);

  int nargs = 7;
  value argv[7];
  for (int i = 0; i < nargs; i++) {
    argv[i] = Val_unit;
  }

  caml_callbackN(fn, nargs, argv);

  CAMLreturn(Val_unit);
}
