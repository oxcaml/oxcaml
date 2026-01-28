#define CAML_INTERNALS

<<<<<<< HEAD
#include "caml/domain.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/startup_aux.h"
=======
#include <caml/domain.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/startup_aux.h>
>>>>>>> upstream/5.4

CAMLprim value
caml_get_max_domains(value nada)
{
  CAMLparam0();

  CAMLreturn(Val_long(caml_params->max_domains));
}
