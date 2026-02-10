#define CAML_INTERNALS
#define NATIVE_CODE
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/startup.h>

int main(int argc, char **argv)
{
  caml_startup(argv);
  caml_init_module("Sticky_failure_test");
  return 0;
}
