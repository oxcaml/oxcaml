#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"

CAMLprim int8_t not(int8_t b) {
  return !b;
}

CAMLprim value box(int8_t b) {
  return Val_int8(b);
}

CAMLprim int8_t unbox(value b) {
  return Int8_val(b);
}

CAMLprim value not_bytecode(value b) {
  CAMLparam1(b);
  CAMLlocal1(result);
  result = box(not(unbox(b)));
  CAMLreturn(result);
}

CAMLprim value box_bytecode(value b) {
  CAMLparam1(b);
  CAMLlocal1(result);
  result = box(unbox(b));
  CAMLreturn(result);
}

CAMLprim value unbox_bytecode(value b) {
  CAMLparam1(b);
  CAMLlocal1(result);
  result = box(unbox(b));
  CAMLreturn(result);
}

