#define CAML_INTERNALS
#define NATIVE_CODE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/startup.h>
#include <caml/fail.h>

/* This stub is called during Reentrant_b initialization.
   It re-enters caml_init_module to initialize Reentrant_c,
   which is NOT a dependency of Reentrant_b. */
CAMLprim value init_reentrant_c_stub(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  const value *get_magic_fn;

  printf("C stub: init_reentrant_c_stub called, will init Reentrant_c\n");
  fflush(stdout);

  /* Re-enter caml_init_module to initialize a module we don't depend on */
  caml_init_module("Reentrant_c");

  printf("C stub: Reentrant_c initialized, getting magic value via callback\n");
  fflush(stdout);

  /* Get the magic value from Reentrant_c via callback */
  get_magic_fn = caml_named_value("get_reentrant_c_magic");
  if (get_magic_fn == NULL) {
    printf("C stub: ERROR - callback not found!\n");
    fflush(stdout);
    CAMLreturn(Val_int(-1));
  }

  result = caml_callback(*get_magic_fn, Val_unit);
  printf("C stub: got magic_value = %ld\n", (long)Long_val(result));
  fflush(stdout);

  CAMLreturn(result);
}

/* This stub is called during Reentrant_b initialization.
   It re-enters caml_init_module to initialize Reentrant_a,
   which IS a dependency of Reentrant_b (already initialized). */
CAMLprim value init_reentrant_a_stub(value unit)
{
  CAMLparam1(unit);

  printf("C stub: init_reentrant_a_stub called, will init Reentrant_a\n");
  fflush(stdout);

  /* Re-enter caml_init_module - should be no-op since already initialized */
  caml_init_module("Reentrant_a");

  printf("C stub: Reentrant_a init returned (was already done)\n");
  fflush(stdout);

  CAMLreturn(Val_int(42));
}

/* Get the magic value from Reentrant_c via callback.
   Used by Ocaml_init to verify Reentrant_c was initialized. */
CAMLprim value get_reentrant_c_magic_stub(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  const value *get_magic_fn;

  get_magic_fn = caml_named_value("get_reentrant_c_magic");
  if (get_magic_fn == NULL) {
    printf("C stub: ERROR - get_reentrant_c_magic callback not found!\n");
    fflush(stdout);
    CAMLreturn(Val_int(-1));
  }

  result = caml_callback(*get_magic_fn, Val_unit);
  CAMLreturn(result);
}
