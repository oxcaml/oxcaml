#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/callback.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"

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

value caml_native_pointer_to_value(intnat p)
{
    return (value)p;
}

value remembered_set_size(value unit)
{
    struct caml_ref_table *tbl = &Caml_state->minor_tables->major_ref;
    return Val_long(tbl->ptr - tbl->base);
}

value test_callback2(value f, value a, value b)
{
    return caml_callback2(f, a, b);
}

value test_callback3(value f, value a, value b, value c)
{
    return caml_callback3(f, a, b, c);
}

value test_callbackN(value f, value a, value b, value c, value d)
{
    value args[] = {a, b, c, d};
    return caml_callbackN(f, 4, args);
}
