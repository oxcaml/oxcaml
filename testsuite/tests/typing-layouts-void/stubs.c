#include <caml/mlvalues.h>
#include <assert.h>

void void_to_void() {
}

value void_to_seven() {
  return Val_long(7);
}

void seven_to_void(value seven) {
  assert(Long_val(seven) == 7);
}

value void_to_void_bytecode(value unit) {
  assert(Long_val(unit) == 0);
  return Val_unit;
}

value void_to_seven_bytecode(value unit) {
  assert(Long_val(unit) == 0);
  return Val_long(7);
}

value seven_to_void_bytecode(value seven) {
  assert(Long_val(seven) == 7);
  return Val_unit;
}

value six_to_seven(value six) {
  assert(Long_val(six) == 6);
  return Val_long(7);
}

value six_to_seven_to_eight(value six, value seven) {
  assert(Long_val(six) == 6);
  assert(Long_val(seven) == 7);
  return Val_long(8);
}

value void_to_six_to_void_to_seven_bytecode(value unit1, value six, value unit2) {
  assert(Long_val(unit1) == 0);
  assert(Long_val(unit2) == 0);
  assert(Long_val(six) == 6);
  return Val_long(7);
}

value six_to_void_to_seven_to_eight_bytecode(value six, value unit, value seven) {
  assert(Long_val(unit) == 0);
  assert(Long_val(six) == 6);
  assert(Long_val(seven) == 6);
  return Val_long(8);
}

value six_to_void_to_seven_bytecode(value six, value unit) {
  assert(Long_val(unit) == 0);
  assert(Long_val(six) == 6);
  return Val_long(7);
}

value void_to_six_to_seven_bytecode(value unit, value six) {
  assert(Long_val(unit) == 0);
  assert(Long_val(six) == 6);
  return Val_long(7);
}

value void_to_void_seven_bytecode(value unit1, value unit2) {
  assert(Long_val(unit1) == 0);
  assert(Long_val(unit2) == 0);
  return Val_long(7);
}

value void_to_seven_void_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_long(7);
  Field(res, 1) = Val_unit;
  CAMLreturn (res);
}
