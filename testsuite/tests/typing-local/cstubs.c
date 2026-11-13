#include "caml/mlvalues.h"
#include "caml/gc.h"
#include "caml/memory.h"

value make_dumb_external_block(value unit)
{
    value *p = caml_stat_alloc(sizeof(value));
    *p = Caml_out_of_heap_header(0, 0);
    return Val_int(((intnat)p) + 1);
}

intnat caml_native_pointer_of_value(value v)
{
    return (intnat)v;
}
