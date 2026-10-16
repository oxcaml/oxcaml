#include <assert.h>
#include <stdint.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

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
  assert(Long_val(seven) == 7);
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

value void_to_void_void_to_seven_bytecode(value unit1, value unit2) {
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

value void_to_void_seven_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_unit;
  Field(res, 1) = Val_long(7);
  CAMLreturn (res);
}

value void_to_void_void_bytecode(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_unit;
  Field(res, 1) = Val_unit;
  CAMLreturn (res);
}

/* Stubs for [elide_void_runtime.ml]. Arguments are small ints and the
   result concatenates them as decimal digits, so a void argument that is
   passed (as unit, i.e. 0) or shifts the others changes the result. The
   [argv] stub also records [argn]. */

value elide_digits1(value a) {
  return Val_long(Long_val(a));
}

value elide_digits2(value a, value b) {
  return Val_long(Long_val(a) * 10 + Long_val(b));
}

value elide_digits3(value a, value b, value c) {
  return Val_long((Long_val(a) * 10 + Long_val(b)) * 10 + Long_val(c));
}

value elide_digits6(value a, value b, value c, value d, value e, value f) {
  long r = 0;
  value args[6] = { a, b, c, d, e, f };
  for (int i = 0; i < 6; i++) r = r * 10 + Long_val(args[i]);
  return Val_long(60000000 + r);
}

value elide_digits_argv(value *argv, int argn) {
  long r = 0;
  for (int i = 0; i < argn; i++) r = r * 10 + Long_val(argv[i]);
  return Val_long(argn * 10000000 + r);
}

/* Ordinary void arguments are passed as unit in bytecode. The native
   bindings use the same stubs as their [@elide_void] counterparts. */

value elide_first_normal(value unit, value a, value b) {
  assert(unit == Val_unit);
  return elide_digits2(a, b);
}

value elide_mid_normal(value a, value unit, value b) {
  assert(unit == Val_unit);
  return elide_digits2(a, b);
}

value elide_last_normal(value a, value b, value unit) {
  assert(unit == Val_unit);
  return elide_digits2(a, b);
}

value elide_mixed_normal(value unit1, value unit2, value a) {
  assert(unit1 == Val_unit);
  assert(unit2 == Val_unit);
  return elide_digits1(a);
}

value elide_seven_last_normal(value *argv, int argn) {
  assert(argn == 7);
  assert(argv[6] == Val_unit);
  return elide_digits6(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value elide_six_source_normal(value *argv, int argn) {
  assert(argn == 6);
  assert(argv[1] == Val_unit);
  assert(argv[3] == Val_unit);
  assert(argv[5] == Val_unit);
  return elide_digits3(argv[0], argv[2], argv[4]);
}

CAMLextern value caml_hypot_float(value, value);

value elide_hypot_normal(value a, value unit, value b) {
  assert(unit == Val_unit);
  return caml_hypot_float(a, b);
}
