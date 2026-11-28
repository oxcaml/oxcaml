#define CAML_INTERNALS
#include <caml/callback.h>
#include <caml/misc.h>

void start_ocaml(char **argv)
{
  caml_startup((char_os const * const *) argv);
}
