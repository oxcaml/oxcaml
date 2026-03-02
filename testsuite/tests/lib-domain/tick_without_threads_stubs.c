#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/domain.h>
#include <caml/memory.h>

static value tick_hook_callback = Val_unit;

static void custom_tick_hook() {
  if (tick_hook_callback != Val_unit) {
    caml_callback(tick_hook_callback, Val_unit);
  }
}

CAMLprim value set_tick_hook(value v_hook) {
  CAMLparam1(v_hook);
  tick_hook_callback = v_hook;
  caml_register_generational_global_root(&tick_hook_callback);
  caml_domain_tick_hook = &custom_tick_hook;
  CAMLreturn(Val_unit);
}
