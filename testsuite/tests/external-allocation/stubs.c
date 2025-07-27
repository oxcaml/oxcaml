#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"
#include <stdio.h>

CAMLprim value is_young(uint64_t v){
    return Val_bool(Is_young(v));
}

CAMLprim value print_block(uint64_t v, uint64_t fields, value name){
    CAMLparam1(name);
    printf("Test %s:\n", String_val(name));
    for(int i = 0; i < fields; i++){
        printf("  Field %d: %llu\n",i,(uint64_t) Field(v,i));
    }
    printf("\n");
    fflush(stdout);
    return Val_unit;
}