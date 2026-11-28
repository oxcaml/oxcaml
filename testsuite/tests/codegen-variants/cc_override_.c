#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

/* ocamlopt defines the macros of the axes of the variant that it compiles
   this file for, and cc_override.sh those of the flags that the C compiler
   is given. */

value cc_override_frame_pointers(value unit)
{
  (void)unit;
#if defined(WITH_FRAME_POINTERS) == defined(CC_KEEPS_FRAME_POINTERS)
  return Val_true;
#else
  return Val_false;
#endif
}

value cc_override_sanitizer(value unit)
{
  (void)unit;
#if defined(WITH_ADDRESS_SANITIZER) == defined(CC_SANITIZES_ADDRESSES)
  return Val_true;
#else
  return Val_false;
#endif
}
