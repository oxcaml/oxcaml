#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/startup.h>

/* Initializes [Mixed], and before it the units that it depends on, each of
   which registers its frametable as it is initialized. */
int main(int argc, char **argv)
{
  caml_startup((char_os const * const *) argv);
  caml_init_module("Mixed");
  caml_shutdown();
  return 0;
}
