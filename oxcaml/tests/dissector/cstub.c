/* C stub for testing passthrough behavior */
#include <caml/mlvalues.h>

CAMLprim value caml_cstub_test_get_magic_number(value unit)
{
  (void)unit;
  return Val_int(42);
}
