#include <stdlib.h>

/* These symbols are referenced by the object file, but the calls must be
   replaced by instructions. */
void caml_bigstring_compare_and_swap_int32_unboxed(void) { abort(); }
void caml_bigstring_compare_and_swap_int64_unboxed(void) { abort(); }
