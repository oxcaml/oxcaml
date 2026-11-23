#include <caml/mlvalues.h>

value toplevel_load_stub(value unit)
{
  return Val_int(42);
}
