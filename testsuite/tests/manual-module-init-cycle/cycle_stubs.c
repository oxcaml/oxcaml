#define CAML_INTERNALS
#define NATIVE_CODE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/startup.h>

/* This stub tries to initialize Cycle_module while we're already initializing it.
   This should be detected as a cycle. */
CAMLprim value try_init_self_stub(value unit)
{
  CAMLparam1(unit);

  printf("C stub: try_init_self_stub called, will try to init Cycle_module\n");
  fflush(stdout);

  /* This will trigger cycle detection, raise, and be handled. */
  caml_init_module("Cycle_module");

  /* Should not reach here */
  printf("C stub: ERROR - should have detected cycle!\n");
  fflush(stdout);

  CAMLreturn(Val_int(-1));
}
