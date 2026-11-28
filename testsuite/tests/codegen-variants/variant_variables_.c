#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* The macros that this compilation sees, among the one that the test adds
   to cflags and mkexe and the codegen-variant axis macros. */
static const char *macros(void)
{
  return ""
#ifdef FROM_TEST
    " FROM_TEST"
#endif
#ifdef WITH_FRAME_POINTERS
    " WITH_FRAME_POINTERS"
#endif
#ifdef STACK_CHECKS_ENABLED
    " STACK_CHECKS_ENABLED"
#endif
#ifdef WITH_ADDRESS_SANITIZER
    " WITH_ADDRESS_SANITIZER"
#endif
    ;
}

#ifdef STANDALONE
int main(void)
{
  printf("mkexe:%s\n", macros());
  return 0;
}
#else
value variant_macros(value unit)
{
  (void)unit;
  return caml_copy_string(macros());
}
#endif
