#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>


void marker_trace_start() { }
void marker_trace_end() { }

static volatile void (*trace_start)(void) = &marker_trace_start;
static volatile void (*trace_end)(void) = &marker_trace_end;


value trace_steps(value f) {
  marker_trace_start();
  value v = caml_callback_exn(f, Val_unit);
  marker_trace_end();
  if (Is_exception_result(v))
    caml_raise(Extract_exception(v));
  else
    return v;
}
