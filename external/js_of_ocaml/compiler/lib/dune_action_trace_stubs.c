#include <caml/mlvalues.h>
#include <time.h>

#define NANOS_PER_SECOND 1000000000

CAMLprim value oxcaml_dune_action_trace_now_in_nanoseconds() {
  struct timespec ts;

  if (timespec_get(&ts, TIME_UTC) != TIME_UTC) {
    return Val_long(0);
  } else {
    return Val_long(NANOS_PER_SECOND * (uintnat)ts.tv_sec + (uintnat)ts.tv_nsec);
  }
}
