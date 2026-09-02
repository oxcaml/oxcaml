#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/callback.h"

/* C trampoline: invokes an OCaml worker (which is an eval'd closure) with
 * two arguments. The point of the test is that during the call, a host
 * callback runs Gc.compact while the eval'd unit's frame is on the OCaml
 * stack above this C frame. F.2 must scan the eval'd frame and keep the
 * unit alive across the C-OCaml-C-OCaml boundary. */
value c_trampoline(value worker, value cb, value n)
{
  return caml_callback3(worker, cb, n, Val_unit);
}
