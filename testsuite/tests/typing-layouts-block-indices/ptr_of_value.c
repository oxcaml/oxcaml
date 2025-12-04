#include "caml/mlvalues.h"

intnat caml_native_pointer_of_value (value v)
{
  return (intnat) v;
}
