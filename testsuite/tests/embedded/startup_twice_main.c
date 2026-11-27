#define CAML_INTERNALS
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

int main_os(int argc, char_os ** argv)
{
  caml_main((char_os const * const *) argv);
  caml_callback(*caml_named_value("change"), Val_unit);
  /* The runtime is already started: this call must leave its settings
     alone. */
  caml_main((char_os const * const *) argv);
  caml_callback(*caml_named_value("show"), Val_unit);
  caml_shutdown();
  caml_shutdown();
  return 0;
}
