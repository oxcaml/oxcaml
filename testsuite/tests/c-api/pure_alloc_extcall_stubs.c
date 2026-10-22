#include <caml/mlvalues.h>
#include <caml/alloc.h>

extern value caml_gc_full_major(value);

value pure_alloc_extcall_gc_then_box(value x)
{
  (void) x;
  caml_gc_full_major(Val_unit);
  return caml_copy_double(1.5);
}
