#define CAML_INTERNALS
#include <caml/callback.h>
#include <caml/misc.h>

int main_os(int argc, char_os **argv)
{
  caml_startup((char_os const * const *) argv);
  return 0;
}
