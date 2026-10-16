/* Support for tick_stack_realloc.ml: request a tick for the current
   domain and process it synchronously, as a poll point would. */

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/camlatomic.h>
#include <caml/domain_state.h>
#include <caml/domain.h>
#include <caml/fail.h>

CAMLprim value test_request_and_process_tick(value unit)
{
  atomic_store_release(&Caml_state->requested_tick, 1);
  caml_get_value_or_raise(caml_process_tick_res());
  return Val_unit;
}
