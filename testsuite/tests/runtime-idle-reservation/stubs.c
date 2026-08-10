#define CAML_INTERNALS

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/major_gc.h"

static atomic_uintnat completed;
static atomic_uintnat incurred;

CAMLprim value caml_test_idle_reservation_reset(value completed_value,
                                                 value incurred_value)
{
  atomic_store(&completed, Long_val(completed_value));
  atomic_store(&incurred, Long_val(incurred_value));
  return Val_unit;
}

CAMLprim value caml_test_idle_reservation_reserve(value limit_value,
                                                   value observed_value)
{
  CAMLparam1(observed_value);
  CAMLlocal1(result);
  uintnat observed = Long_val(observed_value);
  intnat reserved = caml_reserve_major_idle_work(
    &completed, &incurred, Long_val(limit_value), &observed);

  result = caml_alloc_small(2, 0);
  Field(result, 0) = Val_long(reserved);
  Field(result, 1) = Val_long(observed);
  CAMLreturn(result);
}

CAMLprim value caml_test_idle_reservation_completed(value unit)
{
  return Val_long(atomic_load(&completed));
}
