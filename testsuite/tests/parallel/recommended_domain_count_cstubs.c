#define CAML_INTERNALS

<<<<<<< HEAD
#include "caml/domain.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/startup_aux.h"
||||||| 23e84b8c4d
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/domain.h"
=======
#include <caml/domain.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/startup_aux.h>
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

CAMLprim value
caml_get_max_domains(value nada)
{
  CAMLparam0();

  CAMLreturn(Val_long(caml_params->max_domains));
}
