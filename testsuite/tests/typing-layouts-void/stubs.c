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
