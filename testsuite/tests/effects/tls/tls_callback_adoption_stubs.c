#include "caml/mlvalues.h"
#include "caml/callback.h"

value call_me_back(value clos)
{
  return caml_callback(clos, Val_unit);
}
