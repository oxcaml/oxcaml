#define CAML_INTERNALS

<<<<<<< oxcaml
#include "caml/domain.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/startup_aux.h"
||||||| upstream-base
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/domain.h"
=======
#include <caml/domain.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/startup_aux.h>
>>>>>>> upstream-incoming

CAMLprim value
caml_get_max_domains(value nada)
{
  CAMLparam0();

  CAMLreturn(Val_long(caml_params->max_domains));
}
