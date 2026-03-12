#include <stdint.h>
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

/* int * int */

value add_int_int_native(value a, value b) {
  return Val_long(Long_val(a) + Long_val(b));
}

value add_int_int_bytecode(value prod) {
  return Val_long(Long_val(Field(prod, 0)) + Long_val(Field(prod, 1)));
}

/* int64# * int64# */

int64_t add_i64_i64_native(int64_t a, int64_t b) {
  return a + b;
}

value add_i64_i64_bytecode(value prod) {
  CAMLparam1(prod);
  int64_t a = Int64_val(Field(prod, 0));
  int64_t b = Int64_val(Field(prod, 1));
  CAMLreturn(caml_copy_int64(a + b));
}

/* float# * float# */

double add_f64_f64_native(double a, double b) {
  return a + b;
}

value add_f64_f64_bytecode(value prod) {
  CAMLparam1(prod);
  double a = Double_val(Field(prod, 0));
  double b = Double_val(Field(prod, 1));
  CAMLreturn(caml_copy_double(a + b));
}

/* int64# * int */

int64_t add_i64_int_native(int64_t a, value b) {
  return a + Long_val(b);
}

value add_i64_int_bytecode(value prod) {
  CAMLparam1(prod);
  int64_t a = Int64_val(Field(prod, 0));
  long b = Long_val(Field(prod, 1));
  CAMLreturn(caml_copy_int64(a + b));
}

/* int * int64# */

int64_t add_int_i64_native(value a, int64_t b) {
  return Long_val(a) + b;
}

value add_int_i64_bytecode(value prod) {
  CAMLparam1(prod);
  long a = Long_val(Field(prod, 0));
  int64_t b = Int64_val(Field(prod, 1));
  CAMLreturn(caml_copy_int64(a + b));
}

/* float# * int */

double add_f64_int_native(double a, value b) {
  return a + (double)Long_val(b);
}

value add_f64_int_bytecode(value prod) {
  CAMLparam1(prod);
  double a = Double_val(Field(prod, 0));
  long b = Long_val(Field(prod, 1));
  CAMLreturn(caml_copy_double(a + (double)b));
}

/* int * float# */

double add_int_f64_native(value a, double b) {
  return (double)Long_val(a) + b;
}

value add_int_f64_bytecode(value prod) {
  CAMLparam1(prod);
  long a = Long_val(Field(prod, 0));
  double b = Double_val(Field(prod, 1));
  CAMLreturn(caml_copy_double((double)a + b));
}

/* int64# * float# */

double add_i64_f64_native(int64_t a, double b) {
  return (double)a + b;
}

value add_i64_f64_bytecode(value prod) {
  CAMLparam1(prod);
  int64_t a = Int64_val(Field(prod, 0));
  double b = Double_val(Field(prod, 1));
  CAMLreturn(caml_copy_double((double)a + b));
}

/* float# * int64# */

double add_f64_i64_native(double a, int64_t b) {
  return a + (double)b;
}

value add_f64_i64_bytecode(value prod) {
  CAMLparam1(prod);
  double a = Double_val(Field(prod, 0));
  int64_t b = Int64_val(Field(prod, 1));
  CAMLreturn(caml_copy_double(a + (double)b));
}

/* int * int * int (3-element product) */

value add_three_ints_native(value a, value b, value c) {
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c));
}

value add_three_ints_bytecode(value prod) {
  return Val_long(Long_val(Field(prod, 0))
                  + Long_val(Field(prod, 1))
                  + Long_val(Field(prod, 2)));
}

/* Multiple unpacked args: #(int * int) -> #(int * int) -> int */

value add_two_pairs_native(value a, value b, value c, value d) {
  return Val_long(Long_val(a) + Long_val(b)
                  + Long_val(c) + Long_val(d));
}

value add_two_pairs_bytecode(value prod1, value prod2) {
  return Val_long(Long_val(Field(prod1, 0)) + Long_val(Field(prod1, 1))
                  + Long_val(Field(prod2, 0)) + Long_val(Field(prod2, 1)));
}

/* Mixed unpacked and regular args: int -> #(int * int) -> int -> int */

value add_mixed_native(value a, value b, value c, value d) {
  return Val_long(Long_val(a) + Long_val(b)
                  + Long_val(c) + Long_val(d));
}

value add_mixed_bytecode(value a, value prod, value d) {
  return Val_long(Long_val(a)
                  + Long_val(Field(prod, 0)) + Long_val(Field(prod, 1))
                  + Long_val(d));
}

/* Nested product: #(int * #(int * int)) -> int
   Flattened to 3 native args: int, int, int */

value add_nested_native(value a, value b, value c) {
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c));
}

value add_nested_bytecode(value prod) {
  value inner = Field(prod, 1);
  return Val_long(Long_val(Field(prod, 0))
                  + Long_val(Field(inner, 0))
                  + Long_val(Field(inner, 1)));
}

/* Product with void: #(int * #(unit# * int)) -> int
   Void is erased on native, so flattened to 2 native args: int, int */

value add_with_void_native(value a, value b) {
  return Val_long(Long_val(a) + Long_val(b));
}

value add_with_void_bytecode(value prod) {
  value inner = Field(prod, 1);
  /* inner has fields: unit (void, index 0) and int (index 1) */
  return Val_long(Long_val(Field(prod, 0))
                  + Long_val(Field(inner, 1)));
}

/* Product of all voids: #(unit# * unit#) -> int
   All void, so 0 native args for the product */

value all_voids_native(void) {
  return Val_long(42);
}

value all_voids_bytecode(value prod) {
  (void)prod;
  return Val_long(42);
}

/* Unpacked first, regular second: #(int * int) -> int -> int */

value unpacked_first_native(value a, value b, value c) {
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c));
}

value unpacked_first_bytecode(value prod, value c) {
  return Val_long(Long_val(Field(prod, 0)) + Long_val(Field(prod, 1))
                  + Long_val(c));
}

/* Regular first, unpacked second: int -> #(int * int) -> int */

value unpacked_second_native(value a, value b, value c) {
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c));
}

value unpacked_second_bytecode(value a, value prod) {
  return Val_long(Long_val(a)
                  + Long_val(Field(prod, 0)) + Long_val(Field(prod, 1)));
}
