/* Found only through the -I directory that header_path.ml passes. */
#include <m.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>

value header_macros(value unit)
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
