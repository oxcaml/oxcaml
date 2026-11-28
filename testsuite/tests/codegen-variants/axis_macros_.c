#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* The names of the codegen-variant axis macros this compilation sees,
   tested with #ifdef as existing stubs do. */
value axis_macros(value unit)
{
  (void)unit;
  return caml_copy_string(""
#ifdef WITH_FRAME_POINTERS
    " WITH_FRAME_POINTERS"
#endif
#ifdef STACK_CHECKS_ENABLED
    " STACK_CHECKS_ENABLED"
#endif
#ifdef WITH_ADDRESS_SANITIZER
    " WITH_ADDRESS_SANITIZER"
#endif
    );
}
