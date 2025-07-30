#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"
#include <stdio.h>
#include <malloc.h>

CAMLprim value is_young(uint64_t v){
    return Val_bool(Is_young(v));
}

CAMLprim value get_malloc_bytes(value unit) {
    // The github actions complain about this occasionally, just silence this particular warning.
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    struct mallinfo info = mallinfo();
    #pragma GCC diagnostic pop
    return Val_long(info.uordblks);
}

CAMLprim value is_static_alloc(uint64_t v) {
    // This is extremly hacky: detect runtime version by checking for shared_heap
    #if defined(CAML_INTERNALS) && __has_include("caml/shared_heap.h")
        return Val_bool(Is_in_static_data(v));
    #else
        return Val_bool(Classify_addr(v) & In_static_data);
    #endif
}

CAMLprim value print_block(uint64_t v, uint64_t fields, value name){
    CAMLparam1(name);
    printf("Test %s:\n", String_val(name));
    for(int i = 0; i < fields; i++){
        printf("  Field %d: %lu\n",i,(uint64_t) Field(v,i));
    }
    printf("\n");
    fflush(stdout);
    return Val_unit;
}
