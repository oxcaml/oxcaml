#define CAML_INTERNALS
#include <dlfcn.h>
#include <stdio.h>
#include <caml/callback.h>
#include <caml/misc.h>

/* Starts the runtime, says what the dynamic linker then has to report,
   loads the shared library that the first argument names, and starts the
   runtime a second time. */
int main_os(int argc, char_os **argv)
{
  const char * error;
  (void) dlerror();
  caml_startup((char_os const * const *) argv);
  error = dlerror();
  if (error != NULL) printf("after the runtime started: %s\n", error);
  fflush(stdout);
  if (argc > 1) {
    if (dlopen(argv[1], RTLD_NOW | RTLD_GLOBAL) == NULL) {
      printf("%s\n", dlerror());
      return 2;
    }
    caml_startup((char_os const * const *) argv);
    printf("started again\n");
  }
  return 0;
}
