#include <caml/mlvalues.h>
#include <assert.h>


intnat lognot_UtoU(intnat u) {
  return ~u;
}

intnat lognot_BtoU(value v) {
  return ~Long_val(v);
}

value lognot_UtoB(intnat u) {
  return Val_long(~u);
}

value lognot_bytecode(value v) {
  return Val_long(~Long_val(v));
}

intnat sum_7_UBUBUBUtoU(intnat u1, value b2, intnat u3, value b4,
                        intnat u5, value b6, intnat u7) {
  return Long_val(Val_long(u1 + Long_val(b2) + u3 +
                           Long_val(b4)
                           + u5 + Long_val(b6) + u7
                           ));
}

value sum_7_bytecode(value* argv, int argn) {
  assert(argn == 7);
  intnat u1 = Long_val(argv[0]);
  intnat u2 = Long_val(argv[1]);
  intnat u3 = Long_val(argv[2]);
  intnat u4 = Long_val(argv[3]);
  intnat u5 = Long_val(argv[4]);
  intnat u6 = Long_val(argv[5]);
  intnat u7 = Long_val(argv[6]);
  return Val_long(u1 + u2 + u3 + u4 + u5 + u6 + u7);
}


value is_sign_extended(intnat u) {
  if (u != (u << 1) >> 1) {
    return Val_long(1);
  } else {
    return Val_long(0);
  }
}

intnat invalid_sign_bit(value unit) {
  (void)unit;
  return -1UL >> 1;
}

value negative_one(value unit) {
  return Val_long(-1);
}

value return_true(value ignored) {
  (void)ignored;
  return Val_true;
}
