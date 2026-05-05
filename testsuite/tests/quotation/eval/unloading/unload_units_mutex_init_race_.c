#define CAML_INTERNALS

#include <stdlib.h>
#include <string.h>

#include "caml/fail.h"
#include "caml/mlvalues.h"
#include "caml/unloadable.h"

CAMLprim value caml_test_register_unregister_dummy_unloadable_unit(value unit)
{
  (void)unit;

  struct caml_unloadable_unit *u =
      (struct caml_unloadable_unit *)malloc(sizeof(*u));
  if (u == NULL) caml_raise_out_of_memory();
  memset(u, 0, sizeof(*u));

  caml_register_unloadable_unit(u);
  caml_unregister_unloadable_unit(u);

  free(u);

  return Val_unit;
}

