/* A stand-in for the single-stepping LBR emulator that the test programs
   call: it runs the closure without tracing it. */

#define CAML_NAME_SPACE
#include <caml/callback.h>
#include <caml/mlvalues.h>

CAMLprim value caml_singlestep_trace(value append, value path, value closure)
{
  (void)append;
  (void)path;
  return caml_callback(closure, Val_unit);
}
