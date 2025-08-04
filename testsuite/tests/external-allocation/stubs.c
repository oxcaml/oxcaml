#include "caml/mlvalues.h"
#include "caml/address_class.h"
#include "caml/memory.h"
#include <stdio.h>
#include <malloc.h>
#include <stdbool.h>

CAMLprim value is_young(uint64_t v){
    return Val_bool(Is_young(v));
}

CAMLprim value is_static_alloc(uint64_t v) {
    // This is extremly hacky: detect runtime version by checking for shared_heap
    #if defined(CAML_INTERNALS) && __has_include("caml/shared_heap.h")
        return Val_bool(Is_in_static_data(v));
    #elif defined(NO_NAKED_POINTERS)
        // In no-naked-pointers mode, we can't distinguish static data from heap
        // so we conservatively return false for static allocation check
        return Val_bool(0);
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

static bool called_malloc = false;

CAMLprim value malloc_was_called()
{
  return Val_bool(called_malloc);
}

CAMLprim value called_malloc_reset()
{
  called_malloc = false;
  return Val_unit;
}

CAMLextern intnat __real_caml_alloc_malloc (mlsize_t wosize, tag_t);

CAMLprim void __wrap_caml_alloc_malloc(mlsize_t wosize, tag_t tag)
{
  called_malloc = true;
  __real_caml_alloc_malloc(wosize, tag);
}

CAMLextern intnat __real_caml_alloc_mixed_malloc(mlsize_t, tag_t, mlsize_t);

CAMLprim void __wrap_caml_alloc_mixed_malloc(mlsize_t wosize, tag_t tag, mlsize_t prefix)
{
  called_malloc = true;
  __real_caml_alloc_mixed_malloc(wosize, tag, prefix);
}
